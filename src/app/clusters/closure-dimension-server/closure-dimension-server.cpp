/*
 *    Copyright (c) 2023 Project CHIP Authors
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

#include "closure-dimension-server.h"
#include "closure-dimension-cluster-objects.h"

#include <app/AttributeAccessInterface.h>
#include <app/AttributeAccessInterfaceRegistry.h>
#include <app/CommandHandlerInterfaceRegistry.h>
#include <app/ConcreteAttributePath.h>
#include <app/InteractionModelEngine.h>
#include <app/util/attribute-storage.h>

using namespace chip;
using namespace chip::app;
using namespace chip::app::DataModel;
using namespace chip::app::Clusters;
using namespace chip::app::Clusters::ClosureDimension;
using namespace chip::app::Clusters::ClosureDimension::Attributes;
using chip::Protocols::InteractionModel::Status;

namespace chip {
namespace app {
namespace Clusters {
namespace ClosureDimension {

Instance::Instance(Delegate & aDelegate, EndpointId aEndpointId, ClusterId aClusterId, //(((((((here - reference based delegate)))))))
        RotationAxisEnum aRotationAxis,
        OverflowEnum aOverflow,
        ModulationTypeEnum aModulation,
        LatchingAxisEnum aLatchingAxis,
        TranslationDirectionEnum aTranslationDirection) :
        AttributeAccessInterface(Optional<EndpointId>(aEndpointId), aClusterId),
        CommandHandlerInterface(MakeOptional(aEndpointId), aClusterId),
        mEndpointId(aEndpointId), mClusterId(aClusterId)
{
	if constexpr (FeatureRotationEnabled)
	{
		this->mRotationAxis = aRotationAxis;
		this->mOverflow = aOverflow;
	}
	if constexpr (FeatureModulationEnabled)
	{
		this->mModulationType = aModulation;
	}
	if constexpr (FeatureTranslationEnabled)
	{
		this->mTranslationDirection = aTranslationDirection;
	}
	if constexpr (FeatureLatchingEnabled && !FeaturePositioningEnabled)
	{
		this->mLatchingAxis = aLatchingAxis;
	}

	ChipLogDetail(Zcl, "%s ClDim Instance.Constructor() w Features: ID=0x%02lX EP=%u", GetClusterName(), long(mClusterId), mEndpointId);
	LogFeatureMap(GenerateFeatureMap());
	
	/* set the delegates endpointId */
	//mDelegate.SetEndpointId(aEndpointId); //(((((((hand endpoint to delegate)))))))
	
	/* set the delegates ClusterId */
	//mDelegate.SetClusterId(mClusterId); //(((((((hand cluster id to delegate))))
}

Instance::~Instance() override
{
	ChipLogDetail(Zcl, "%s ClDim Instance.Destructor(): ID=0x%02lX EP=%u", GetClusterName(), long(mClusterId), mEndpointId);
	CommandHandlerInterfaceRegistry::Instance().UnregisterCommandHandler(this);
	AttributeAccessInterfaceRegistry::Instance().Unregister(this);
}

CHIP_ERROR Instance::Init()
{
	ChipLogDetail(NotSpecified, "%s ClDim Init Registration", GetClusterName());

	// Conformances for Features preChecked
	static_assert( FeaturePositioningEnabled ||  FeatureLatchingEnabled   , "Feature: At least one of Positioning and/or Latching should be enabled");
	static_assert( FeaturePositioningEnabled || !FeatureUnitEnabled       , "Feature: Unit        requires Positioning to be true");
	static_assert( FeaturePositioningEnabled || !FeatureSpeedEnabled      , "Feature: Speed       requires Positioning to be true");
	static_assert( FeaturePositioningEnabled || !FeatureLimitationEnabled , "Feature: Limitation  requires Positioning to be true");
	static_assert( FeaturePositioningEnabled || !FeatureRotationEnabled   , "Feature: Rotation    requires Positioning to be true");
	static_assert( FeaturePositioningEnabled || !FeatureTranslationEnabled, "Feature: Translation requires Positioning to be true");
	static_assert( FeaturePositioningEnabled || !FeatureModulationEnabled , "Feature: Modulation  requires Positioning to be true");
	static_assert( FeatureModulationEnabled  || !FeatureRotationEnabled   || !FeatureTranslationEnabled, "Features: RO vs TR, exclusivity unmet");
	static_assert( FeatureTranslationEnabled || !FeatureRotationEnabled   || !FeatureModulationEnabled , "Features: RO vs MD, exclusivity unmet");
	static_assert( FeatureRotationEnabled    || !FeatureModulationEnabled || !FeatureTranslationEnabled, "Features: MD vs TR, exclusivity unmet");
	static_assert(!FeatureTranslationEnabled ||  FeatureRotationEnabled   ||  FeatureModulationEnabled || FeatureTranslationEnabled, "Features: at least one among MD/RO/TR");
	static_assert(!FeatureRotationEnabled    || !FeatureModulationEnabled || !FeatureTranslationEnabled, "Features: MD vs TR vs RO, exclusivity unmet");

	VerifyOrReturnError(IsValidAliasCluster(), CHIP_ERROR_INCORRECT_STATE);

	// Check if the cluster has been selected in zap
	VerifyOrReturnError(emberAfContainsServer(mEndpointId, mClusterId), CHIP_ERROR_INCORRECT_STATE);

	// Register the object as attribute provider
	VerifyOrReturnError(AttributeAccessInterfaceRegistry::Instance().Register(this), CHIP_ERROR_INCORRECT_STATE);
	ReturnErrorOnFailure(CommandHandlerInterfaceRegistry::Instance().RegisterCommandHandler(this));

	mFeatureMap = GenerateFeatureMap();
	LogFeatureMap(mFeatureMap);

	ChipLogDetail(NotSpecified, "%s ClDim Registered as Ep[%u] Id=0x%04X", GetClusterName(), mEndpointId, mClusterId);

	return CHIP_NO_ERROR;
}

template <bool Enabled = FeaturePositioningEnabled || FeatureLatchingEnabled, typename = std::enable_if_t<Enabled, CHIP_ERROR>>
CHIP_ERROR Instance::SetCurrent(Optional<Percent100ths> aPositioning, Optional<LatchingEnum> aLatch, Optional<Globals::ThreeLevelAutoEnum> aSpeed)
{
	NCurrentStruct nullableCurrent = this->mCurrent;
	CurrentStruct current = { .kPosition = 0, .kLatching = 1, .kSpeed = 2 }; // ToDo is this needed?
	
	if (aPositioning.HasValue())
	{
		if (aPositioning.Value() > kMaxPercent100ths)
		{
			return CHIP_ERROR_INVALID_ARGUMENT;
		}

		current.kPosition = aPositioning.Value();
		nullableCurrent.SetNonNull(current);
	}
	if (aSpeed.HasValue())
	{
		if (aSpeed >= Globals::ThreeLevelAutoEnum::kUnknownEnumValue)
		{
			return CHIP_ERROR_INVALID_ARGUMENT;
		}

		current.kSpeed = aSpeed.Value();
		nullableCurrent.SetNonNull(current);
	}	
	if (aLatch.HasValue())
	{
		if (aLatch >= LatchingEnum::kUnknownEnumValue)
		{
			return CHIP_ERROR_INVALID_ARGUMENT;
		}

		current.kLatching = aLatch.Value();
		nullableCurrent.SetNonNull(current);
	}		

	if (!aSpeed.HasValue() && !aPositioning.HasValue() && !aLatch.HasValue())
	{
		nullableCurrent.SetNull();
		ChipLogDetail(NotSpecified, "set Nullable");
	}

	// Check to see if a change has ocurred
	VerifyOrReturnError(this->mCurrent != nullableCurrent, CHIP_NO_ERROR); 
	
	this->mCurrent = nullableCurrent;
	MatterReportingAttributeChangeCallback(mEndpointId, mClusterId, Attributes::Current::Id);

	return CHIP_NO_ERROR;
}

template <bool Enabled = FeaturePositioningEnabled || FeatureLatchingEnabled, typename = std::enable_if_t<Enabled, CHIP_ERROR>>
CHIP_ERROR Instance::CHIP_ERROR SetTarget(Optional<Percent100ths> aPositioning, Optional<TagLatchEnum> aTagLatch, Optional<Globals::ThreeLevelAutoEnum> aSpeed)
{
	NTargetStruct nullableTarget = this->mTarget;
	TargetStruct target = { .kPosition = 0, .kTagLatch = 1, .kSpeed = 2 };

	if (aPositioning.HasValue())
	{
		if (aPositioning.Value() > kMaxPercent100ths)
		{
			return CHIP_ERROR_INVALID_ARGUMENT;
		}

		newTarget.kPosition = aPositioning.Value();
		nullableTarget.SetNonNull(newTarget);
	}
	if (aSpeed.HasValue())
	{
		if (aSpeed >= Globals::ThreeLevelAutoEnum::kUnknownEnumValue)
		{
			return CHIP_ERROR_INVALID_ARGUMENT;
		}

		newTarget.kSpeed = aSpeed.Value();
		nullableTarget.SetNonNull(newTarget);
	}
	if (aLatch.HasValue())
	{
		if (aLatch >= TagLatchEnum::kUnknownEnumValue)
		{
			return CHIP_ERROR_INVALID_ARGUMENT;
		}

		newTarget.kLatching = kTagLatch.Value();
		nullableTarget.SetNonNull(newTarget);
	}

	if (!aPositioning.HasValue() && !aTagLatch.HasValue() && !aSpeed.HasValue())
	{
		nullableTarget.SetNull();
		ChipLogDetail(NotSpecified, "set Nullable");
	}

	// Check to see if a change has ocurred
	VerifyOrReturnError(this->mTarget != nullableTarget, CHIP_NO_ERROR); 
	
	this->mTarget = nullableTarget;
	MatterReportingAttributeChangeCallback(mEndpointId, mClusterId, Attributes::Target::Id);
}

template <bool Enabled = FeaturePositioningEnabled, typename = std::enable_if_t<Enabled, CHIP_ERROR>>
CHIP_ERROR Instance::CHIP_ERROR SetResolutionAndStepValue(Percent100ths aResolutionPercent100ths, uint8_t aStepCoef)
{
	//
	if (0 == aResolutionPercent100ths)
	{
		aResolutionPercent100ths = 1;
	}

	if (aResolutionPercent100ths > kMaxPercent100ths)
	{
		this->mResolution = kMaxPercent100ths;
	}
	else
	{
		this->mResolution = aResolutionPercent100ths;
	}

	if (0 == aStepCoef)
	{
		this->mStepValue = this->mResolution;
	}
	else
	{
		this->mStepValue = this->mResolution * aStepCoef;
	}

	if (this->mStepValue > kMaxPercent100ths)
	{
		this->mStepValue = kMaxPercent100ths;
	}

	MatterReportingAttributeChangeCallback(mEndpointId, mClusterId, Attributes::Resolution::Id);
	MatterReportingAttributeChangeCallback(mEndpointId, mClusterId, Attributes::StepValue::Id);

	ChipLogDetail(NotSpecified, "Resolution=%u, StepValue=%u", this->mResolution, this->mStepValue);        

	return CHIP_NO_ERROR;
}

template <bool Enabled = FeatureUnitEnabled, typename = std::enable_if_t<Enabled, CHIP_ERROR>>
CHIP_ERROR Instance::CHIP_ERROR SetUnitAndRange(ClosureUnitEnum aUnit, int16_t aMin, int16_t aMax)
{
	VerifyOrReturnError(aMax > aMin, CHIP_ERROR_INVALID_ARGUMENT);
	VerifyOrReturnError(EnsureKnownEnumValue(aUnit) != ClosureUnitEnum::kUnknownEnumValue, CHIP_ERROR_INVALID_ARGUMENT);

	if (this->mUnit != aUnit)
	{
		this->mUnit = aUnit;
		MatterReportingAttributeChangeCallback(mEndpointId, mClusterId, Attributes::Unit::Id);
		ChipLogDetail(NotSpecified, "Unit=%u", to_underlying(this->mUnit));
	}

	// Check to see if a change has ocurred
	VerifyOrReturnError(this->mUnitRange.min != aMin || this->mUnitRange.max != aMax , CHIP_NO_ERROR);
	this->mUnitRange.min = aMin;
	this->mUnitRange.max = aMax;
	MatterReportingAttributeChangeCallback(mEndpointId, mClusterId, Attributes::UnitRange::Id);

	ChipLogDetail(NotSpecified, "Range.min=%i Range.max=%i", this->mUnitRange.min, this->mUnitRange.max);

	return CHIP_NO_ERROR;
}

template <bool Enabled = FeatureLimitationEnabled, typename = std::enable_if_t<Enabled, CHIP_ERROR>>
CHIP_ERROR Instance::CHIP_ERROR SetLimitRange(Optional<Percent100ths> aMin, Optional<Percent100ths> aMax)
{
	//DataModel::Nullable<LatchingEnum> oldLatching = nullableLatching;
	NRangePercent100thsStruct limitRange = this->mLimitRange;
	RangePercent100thsStruct limit = { .min = kMinPercent100ths, .max = kMaxPercent100ths };

	if (!aMin.HasValue() && !aMax.HasValue())
	{
		limitRange.SetNull();
		ChipLogDetail(NotSpecified, "set limit range Nullable");
	}

	if (aMin.HasValue() && aMax.HasValue())
	{
		VerifyOrReturnError(aMax.Value() > aMin.Value(), CHIP_ERROR_INVALID_ARGUMENT);
	}

	if (aMin.HasValue())
	{
		limit.min = aMin.Value();
		limitRange.SetNonNull(limit);
		ChipLogDetail(NotSpecified, "set limit range min value");
	}

	if (aMax.HasValue())
	{
		limit.max = aMax.Value();
		limitRange.SetNonNull(limit);
		ChipLogDetail(NotSpecified, "set limit range max value");
	}

	this->mLimitRange = limitRange;

	if (this->mLimitRange.IsNull())
	{
		ChipLogDetail(NotSpecified, "Limit Null")
	}
	else
	{
		ChipLogDetail(NotSpecified, "Limit.min=%i Limit.max=%i", this->mLimitRange.Value().min, this->mLimitRange.Value().max);
	}

	MatterReportingAttributeChangeCallback(mEndpointId, mClusterId, Attributes::LimitRange::Id);

	return CHIP_NO_ERROR;
}

template <bool Enabled = FeatureTranslationEnabled, typename = std::enable_if_t<Enabled, CHIP_ERROR>>
CHIP_ERROR Instance::CHIP_ERROR SetTranslationDirection(TranslationDirectionEnum aTranslationDirection)
{
	VerifyOrReturnError(EnsureKnownEnumValue(aTranslationDirection) != TranslationDirectionEnum::kUnknownEnumValue, CHIP_ERROR_INVALID_ARGUMENT);
	// Check to see if a change has ocurred
	VerifyOrReturnError(this->mTranslationDirection != aTranslationDirection, CHIP_NO_ERROR);
	this->mTranslationDirection = aTranslationDirection;
	MatterReportingAttributeChangeCallback(mEndpointId, mClusterId, Attributes::TranslationDirection::Id);

	return CHIP_NO_ERROR;
}

template <bool Enabled = FeatureRotationEnabled, typename = std::enable_if_t<Enabled, CHIP_ERROR>>
CHIP_ERROR Instance::CHIP_ERROR SetRotationAxis(RotationAxisEnum aRotationAxis)
{
	VerifyOrReturnError(EnsureKnownEnumValue(aRotationAxis) != RotationAxisEnum::kUnknownEnumValue, CHIP_ERROR_INVALID_ARGUMENT);
	// Check to see if a change has ocurred
	VerifyOrReturnError(this->mRotationAxis != aRotationAxis, CHIP_NO_ERROR);
	this->mRotationAxis = aRotationAxis;
	MatterReportingAttributeChangeCallback(mEndpointId, mClusterId, Attributes::RotationAxis::Id);

	return CHIP_NO_ERROR;
}

template <bool Enabled = FeatureRotationEnabled || FeatureLatchingEnabled, typename = std::enable_if_t<Enabled, CHIP_ERROR>>
CHIP_ERROR Instance::CHIP_ERROR SetOverflow(OverflowEnum aOverflow)
{
	VerifyOrReturnError(EnsureKnownEnumValue(aOverflow) != OverflowEnum::kUnknownEnumValue, CHIP_ERROR_INVALID_ARGUMENT);
	// Check to see if a change has ocurred
	VerifyOrReturnError(this->mOverflow != aOverflow, CHIP_NO_ERROR);
	this->mOverflow = aOverflow;
	MatterReportingAttributeChangeCallback(mEndpointId, mClusterId, Attributes::Overflow::Id);

	return CHIP_NO_ERROR;
}

template <bool Enabled = FeatureModulationEnabled, typename = std::enable_if_t<Enabled, CHIP_ERROR>>
CHIP_ERROR Instance::CHIP_ERROR SetModulationType(ModulationTypeEnum aModulationType)
{
	VerifyOrReturnError(EnsureKnownEnumValue(aModulationType) != ModulationTypeEnum::kUnknownEnumValue, CHIP_ERROR_INVALID_ARGUMENT);
	// Check to see if a change has ocurred
	VerifyOrReturnError(this->mModulationType != aModulationType, CHIP_NO_ERROR);
	this->mModulationType = aModulationType;
	MatterReportingAttributeChangeCallback(mEndpointId, mClusterId, Attributes::ModulationType::Id);

	return CHIP_NO_ERROR;
	
}

template <bool Enabled = FeatureLatchingEnabled && !FeaturePositioningEnabled, typename = std::enable_if_t<Enabled, CHIP_ERROR>>
CHIP_ERROR Instance::CHIP_ERROR SetLatchingAxis(LatchingAxisEnum aLatchingAxis)
{
	VerifyOrReturnError(EnsureKnownEnumValue(aLatchingAxis) != LatchingAxisEnum::kUnknownEnumValue, CHIP_ERROR_INVALID_ARGUMENT);
	// Check to see if a change has ocurred
	VerifyOrReturnError(this->mLatchingAxis != aLatchingAxis, CHIP_NO_ERROR);
	this->mLatchingAxis = aLatchingAxis;
	MatterReportingAttributeChangeCallback(mEndpointId, mClusterId, Attributes::LatchingAxis::Id);

	return CHIP_NO_ERROR;
}

void Instance::InvokeCommand(HandlerContext & handlerContext)
{
	ChipLogDetail(Zcl, "%s ClDim: InvokeCommand", GetClusterName());
	if (handlerContext.mCommandHandled || !IsValidAliasCluster(handlerContext.mRequestPath.mClusterId))
	{
		// TODO check separately the mCommandHandled
		handlerContext.mCommandHandler.AddStatus(handlerContext.mRequestPath, Protocols::InteractionModel::Status::UnsupportedCluster);
		return;
	}

	switch (handlerContext.mRequestPath.mCommandId)
	{
		case Commands::Step::Id:
				ChipLogDetail(Zcl, "%s ClDim::Cmd::Step()", GetClusterName());
				Commands::Step::DecodableType requestPayload;
				handlerContext.SetCommandHandled();

				if (DataModel::Decode(handlerContext.mPayload, requestPayload) != CHIP_NO_ERROR)
				{
					handlerContext.mCommandHandler.AddStatus(handlerContext.mRequestPath, Protocols::InteractionModel::Status::InvalidCommand);
					return;
				}
				// Payload ready to be used
				HandleStepCommand(handlerContext, requestPayload);
			break;

		case Commands::SetTarget::Id:
				ChipLogDetail(Zcl, "%s ClDim::Cmd::Latch()", GetClusterName());
				Commands::Latch::DecodableType requestPayload;
				handlerContext.SetCommandHandled();

				if (DataModel::Decode(handlerContext.mPayload, requestPayload) != CHIP_NO_ERROR)
				{
					handlerContext.mCommandHandler.AddStatus(handlerContext.mRequestPath, Protocols::InteractionModel::Status::InvalidCommand);
					return;
				}
				
				// Payload ready to be used
				HandleSetTargetCommand(handlerContext, requestPayload);
			break;

		default:
			ChipLogError(Zcl, "%s ClDim: InvokeCommand unknown", GetClusterName());
			handlerContext.mCommandHandler.AddStatus(handlerContext.mRequestPath, Protocols::InteractionModel::Status::UnsupportedCommand);
			return;
			break;
	}

	/* NOTE: TODO better error handing this is for demo purpose */
	handlerContext.mCommandHandler.AddStatus(handlerContext.mRequestPath, Protocols::InteractionModel::Status::Success);	
}

// AttributeAccessInterface
CHIP_ERROR Instance::Read(const ConcreteReadAttributePath & aPath, AttributeValueEncoder & aEncoder)
{
	bool isAttributeSupported = false;
	switch (aPath.mAttributeId)
	{
	case Attributes::TagList::Id:
	    ChipLogDetail(Zcl, "%s ClDim::Read::TagList", GetClusterName());
		// ToDo: To be Completed	
		break;

	/* Positioning feature Attributes */
	case Attributes::Current::Id:
		ChipLogDetail(Zcl, "%s ClDim::Read::Current", GetClusterName());
		if constexpr (FeaturePositioningEnabled)
		{
			ReturnErrorOnFailure(aEncoder.Encode(this->mCurrent));
			isAttributeSupported = true;
		}
		break;

	case Attributes::Target::Id:
		ChipLogDetail(Zcl, "%s ClDim::Read::Target", GetClusterName());
		if constexpr (FeaturePositioningEnabled)
		{
			ReturnErrorOnFailure(aEncoder.Encode(this->mTarget));
			isAttributeSupported = true;
		}
		break;

	case Attributes::Resolution::Id:
		ChipLogDetail(Zcl, "%s ClDim::Read::Resolution", GetClusterName());
		if constexpr (FeaturePositioningEnabled)
		{
			ReturnErrorOnFailure(aEncoder.Encode(this->mResolution));
			isAttributeSupported = true;
		}
		break;

	case Attributes::StepValue::Id:
		ChipLogDetail(Zcl, "%s ClDim::Read::StepValue", GetClusterName());
		if constexpr (FeaturePositioningEnabled)
		{
			ReturnErrorOnFailure(aEncoder.Encode(this->mStepValue));
			isAttributeSupported = true;
		}
		break;

	/* Unit feature Attributes */
	case Attributes::Unit::Id:
		ChipLogDetail(Zcl, "%s ClDim::Read::Unit", GetClusterName());
		if constexpr (FeatureUnitEnabled)
		{
			ReturnErrorOnFailure(aEncoder.Encode(this->mUnit));
			isAttributeSupported = true;
		}
		break;

	case Attributes::UnitRange::Id:
		ChipLogDetail(Zcl, "%s ClDim::Read::UnitRange", GetClusterName());
		if constexpr (FeatureUnitEnabled)
		{
			ReturnErrorOnFailure(aEncoder.Encode(this->mUnitRange));
			isAttributeSupported = true;
		}
		break;

	/* Limitation feature Attributes */
	case Attributes::LimitRange::Id:
		ChipLogDetail(Zcl, "%s ClDim::Read::LimitRange", GetClusterName());
		if constexpr (FeatureLimitationEnabled)
		{
			ReturnErrorOnFailure(aEncoder.Encode(this->mLimitRange));
			isAttributeSupported = true;
		}
		break;

	case Attributes::TranslationDirection::Id:
		ChipLogDetail(Zcl, "%s ClDim::Read::TranslationDirection", GetClusterName());
		if constexpr (FeatureTranslationEnabled)
		{
			ReturnErrorOnFailure(aEncoder.Encode(this->mTranslationDirection));
			isAttributeSupported = true;
		}
		break;

	case Attributes::RotationAxis::Id:
		ChipLogDetail(Zcl, "%s ClDim::Read::RotationAxis", GetClusterName());
		if constexpr (FeatureRotationEnabled)
		{
			ReturnErrorOnFailure(aEncoder.Encode(this->mRotationAxis));
			isAttributeSupported = true;
		}
		break;

	case Attributes::Overflow::Id:
		ChipLogDetail(Zcl, "%s ClDim::Read::Overflow", GetClusterName());
		if constexpr (FeatureRotationEnabled || FeatureLatchingEnabled)
		{
			ReturnErrorOnFailure(aEncoder.Encode(this->mOverflow));
			isAttributeSupported = true;
		}
		break;

	case Attributes::ModulationType::Id:
		ChipLogDetail(Zcl, "%s ClDim::Read::ModulationType", GetClusterName());
		if constexpr (FeatureModulationEnabled)
		{
			ReturnErrorOnFailure(aEncoder.Encode(this->mModulationType));
			isAttributeSupported = true;
		}
		break;

	case Attributes::LatchingAxis::Id:
		ChipLogDetail(Zcl, "%s ClDim::Read::LatchingAxis", GetClusterName());
		if constexpr (FeatureLatchingEnabled && !FeaturePositioningEnabled)
		{
			ReturnErrorOnFailure(aEncoder.Encode(this->mLatchingAxis));
			isAttributeSupported = true;
		}
		break;

	case Attributes::FeatureMap::Id:
		ChipLogDetail(Zcl, "%s ClDim::Read::FeatureMap", GetClusterName());
		LogFeatureMap(mFeatureMap);
		ReturnErrorOnFailure(aEncoder.Encode(mFeatureMap));
		isAttributeSupported = true;
		break;
	}

	return isAttributeSupported ? CHIP_NO_ERROR : CHIP_IM_GLOBAL_STATUS(UnsupportedAttribute);
}

void Instance::HandleStepCommand(HandlerContext & ctx, const Commands::Step::DecodableType & req)
{
	ChipLogDetail(Zcl, "%s ClDim: HandleStepCommand", GetClusterName());
	ChipLogDetail(Zcl, "%s ClDim: dir=%u #Step=%u cId=0x%04X/0x%04X", GetClusterName(), to_underlying(req.direction), req.numberOfSteps, req.GetClusterId(), mClusterId);
	LogStepsRequest(req);

	// Delegate forwarding
	mDelegate.SetStepCallback();
	
	// Error handling
	ctx.mCommandHandler.AddStatus(ctx.mRequestPath, Protocols::InteractionModel::Status::Success);
}

void Instance::HandleSetTargetCommand(HandlerContext & ctx, const Commands::SetTarget::DecodableType & req)
{
	 Status status = Status::Success;

	 auto & position = req.position;
	 auto & tagLatch = reg.tagLatch;
	 auto & speed    = reg.speed;

	ChipLogDetail(Zcl, "%s ClDim: HandleLatchCommand", GetClusterName());


	// Error handling
	if constexpr (FeaturePositioningEnabled)
	{
		if(position > kMaxPercent100ths)
		{
			ctx.mCommandHandler.AddStatus(ctx.mRequestPath, Status::ConstraintError);
			return;
		}
	}
	if constexpr (FeatureLatchingEnabled)
	{
		if(tagLatch >= TagLatchEnum::kUnknownEnumValue)
		{
			ctx.mCommandHandler.AddStatus(ctx.mRequestPath, Status::ConstraintError);
			return;
		}
	}
	if constexpr (FeatureSpeedEnabled)
	{
		if(speed >= Globals::ThreeLevelAutoEnum::kUnknownEnumValue)
		{
			ctx.mCommandHandler.AddStatus(ctx.mRequestPath, Status::ConstraintError);
			return;
		}
	}

	// Delegate forwarding
	status = mDelegate.SetSetTargetCallback(position, tagLatch, speed);
	
	ctx.mCommandHandler.AddStatus(ctx.mRequestPath, status);
}

uint32_t Instance::GenerateFeatureMap() const
{
	//
	BitMask<Feature, uint32_t> featureMap(0);

	if constexpr (FeaturePositioningEnabled)
	{
		featureMap.Set(Feature::kPositioning);
	}

	if constexpr (FeatureLatchingEnabled)
	{
		featureMap.Set(Feature::kLatching);
	}

	if constexpr (FeatureRotationEnabled)
	{
		featureMap.Set(Feature::kRotation);
	}

	if constexpr (FeatureTranslationEnabled)
	{
		featureMap.Set(Feature::kTranslation);
	}

	if constexpr (FeatureUnitEnabled)
	{
		featureMap.Set(Feature::kUnit);
	}

	if constexpr (FeatureSpeedEnabled)
	{
		featureMap.Set(Feature::kSpeed);
	}

	if constexpr (FeatureModulationEnabled)
	{
		featureMap.Set(Feature::kModulation);
	}

	if constexpr (FeatureLimitationEnabled)
	{
		featureMap.Set(Feature::kLimitation);
	}

	return featureMap.Raw();
}

// For test purpose only
const char * Instance::GetClusterName() const
{
	for (Info AliasedCluster : AliasedClusters)
	{
		if (mClusterId == AliasedCluster.cId)
		{
			return AliasedCluster.name;
		}
	}
	ChipLogDetail(Zcl, "INVALID CLUSTER");
	return nullptr;
};

} // namespace ClosureDimension
} // namespace Clusters
} // namespace app
} // namespace chip