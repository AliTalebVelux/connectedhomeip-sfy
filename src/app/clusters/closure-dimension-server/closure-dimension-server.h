/*
 *
 *    Copyright (c) 2024 Project CHIP Authors
 *    All rights reserved.
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
 
 #pragma once

#include "closure-dimension-cluster-objects.h"
#include <app/AttributeAccessInterface.h>
#include <app/AttributeAccessInterfaceRegistry.h>
#include <app/CommandHandlerInterfaceRegistry.h>
#include <app/data-model/Nullable.h>
#include <app/reporting/reporting.h>
#include <app/util/attribute-storage.h>
#include <lib/support/IntrusiveList.h>
#include <type_traits>

#include <app/clusters/closure-dimension-server/closure-dimension-common.h> // To be removed

using chip::Protocols::InteractionModel::Status;

namespace chip {
namespace app {
namespace Clusters {
namespace ClosureDimension {
	
namespace Detail {

struct DummyPositioningMembers
{
};

struct DummyUnitMembers
{
};

struct DummySpeedMembers
{
};

struct DummyLatchingMembers
{
};

struct DummyLatchingOnlyMembers
{
};

struct DummyLimitationMembers
{
};

struct DummyRotationMembers
{
};

struct DummyTranslationMembers
{
};

struct DummyModulationMembers
{
};

struct DummyRotationOrLatchingMembers
{
};

class PositioningMembers
{
protected:
	NCurrentStruct mCurrent;
	NTargetStruct mTarget;

    Percent100ths mResolution;
    Percent100ths mStepValue;
};

class LatchingMembers
{
protected:
};

class LatchingOnlyMembers
{
protected:
    LatchingAxisEnum mLatchingAxis;
};

class UnitMembers
{
protected:
	ClosureUnitEnum mUnit;

	UnitRangeStruct mUnitRange;
};

class SpeedMembers
{
protected:
};

class LimitationMembers
{
protected:
    NRangePercent100thsStruct mLimitRange;
};

class RotationMembers
{
protected:
    RotationAxisEnum mRotationAxis;
};

class TranslationMembers
{
protected:
    TranslationDirectionEnum mTranslationDirection;
};

class ModulationMembers
{
protected:
    ModulationTypeEnum mModulationType;
};

class RotationOrLatchingMembers
{
protected:
	OverflowEnum mOverflow;
};

} // namespace Detail


template <class U> // Temporary for debug reason 
void ChipLogOptionalValue(const chip::Optional<U> & item, const char * message, const char * name) //const Optional<U> & other)
{
    if (item.HasValue())
    {
        ChipLogDetail(Zcl, "%s %s 0x%02u", message, name, static_cast<uint16_t>(item.Value()));
    }
    else
    {
        //ChipLogDetail(Zcl, "%s %s" CL_YELLOW "NotPresent" CL_CLEAR, message, name);
    }
}

/**
 * Defines methods for implementing application-specific logic for the closure dimension aliased cluster.
 * The delegate API assumes there will be separate delegate objects for each cluster instance.
 */
class Delegate
{
public:
    Delegate() = default;
	virtual ~Delegate() = default;
	
    //void SetEndpointId(EndpointId aEndpoint) { mEndpointId = aEndpoint; }
	//EndpointId GetEndpointId() { return mEndpointId; }		
	//void mDelegate.SetClusterId(ClusterId aClusterId) { mClusterId = aClusterId; }
	//ClusterId GetClusterId() { return aClusterId; }    	

	// Command callback Delegates

     /**
     * Handle Command Callback in application: Step
      * It should report Status::Success if successful and may
      * return other Status codes if it fails
     */
	virtual Protocols::InteractionModel::Status SetStepCallback(const StepDirectionEnum direction, const uint16_t numberOfSteps, const Globals::ThreeLevelAutoEnum speed) = 0;

     /**
     * Handle Command Callback in application: SetTarget
      * It should report Status::Success if successful and may
      * return other Status codes if it fails
     */
	virtual Protocols::InteractionModel::Status SetSetTargetCallback(const Percent100ths positioning, const TagLatchEnum tagLatch, const Globals::ThreeLevelAutoEnum speed) = 0;
	
protected:
    //EndpointId mEndpointId = 0;
	//ClusterId mClusterId   = 0;
};

/**
 * This class provides the base implementation for the server side of the ClosureDimension cluster(s) as well as an API for
 * setting the values of the attributes + delegation for commands
 * It implements CommandHandlerInterface so it can generically handle commands for any derivation cluster id.
 *
 * @tparam FeaturePositioningEnabled whether the cluster supports Positioning
 * @tparam FeatureLatchingEnabled whether the cluster supports Latching
 * @tparam FeatureUnitEnabled whether the cluster supports Unit
 * @tparam FeatureSpeedEnabled whether the cluster supports Speed
 * @tparam FeatureLimitationEnabled whether the cluster supports Limitation
 * @tparam FeatureRotationEnabled whether the cluster supports Rotation
 * @tparam FeatureTranslationEnabled whether the cluster supports Translation
 * @tparam FeatureModulationEnabled whether the cluster supports Modulation
 */
template <bool FeaturePositioningEnabled,
          bool FeatureLatchingEnabled,
          bool FeatureUnitEnabled,
          bool FeatureSpeedEnabled,
          bool FeatureLimitationEnabled,
          bool FeatureRotationEnabled,
          bool FeatureTranslationEnabled,
          bool FeatureModulationEnabled>
class Instance
    : public AttributeAccessInterface, public CommandHandlerInterface,
      protected std::conditional_t<FeaturePositioningEnabled, Detail::PositioningMembers, Detail::DummyPositioningMembers>,
      protected std::conditional_t<FeatureLatchingEnabled   , Detail::LatchingMembers   , Detail::DummyLatchingMembers>,
      protected std::conditional_t<FeatureUnitEnabled       , Detail::UnitMembers       , Detail::DummyUnitMembers>,
      protected std::conditional_t<FeatureSpeedEnabled      , Detail::SpeedMembers      , Detail::DummySpeedMembers>,
      protected std::conditional_t<FeatureLimitationEnabled , Detail::LimitationMembers , Detail::DummyLimitationMembers>,
      protected std::conditional_t<FeatureRotationEnabled   , Detail::RotationMembers   , Detail::DummyRotationMembers>,
      protected std::conditional_t<FeatureTranslationEnabled, Detail::TranslationMembers, Detail::DummyTranslationMembers>,
      protected std::conditional_t<FeatureModulationEnabled , Detail::ModulationMembers , Detail::DummyModulationMembers>,
      protected std::conditional_t<FeatureLatchingEnabled && !FeaturePositioningEnabled,
          Detail::LatchingOnlyMembers      , Detail::DummyLatchingOnlyMembers>,
      protected std::conditional_t<FeatureRotationEnabled || FeatureLatchingEnabled,
          Detail::RotationOrLatchingMembers, Detail::DummyRotationOrLatchingMembers>
{
private:
	Delegate & mDelegate;
	
    const EndpointId mEndpointId;
    const ClusterId mClusterId;
	
	static const Percent100ths kMaxPercent100ths = 10000;
    static const Percent100ths kMinPercent100ths = 0;
    uint32_t mFeatureMap = 0;

	/**
	 * Handle Command: Step
	 */
    void HandleStepCommand(HandlerContext & ctx, const Commands::Step::DecodableType & req)
    {
        Status status = Status::Success;

        auto & direction     = req.direction;
        auto & numberOfSteps = req.numberOfSteps;
        auto & speed         = req.speed;

        ChipLogDetail(Zcl, "%s ClDim: HandleStepCommand", GetClusterName());
        ChipLogDetail(Zcl, "%s ClDim: dir=%u #Step=%u cId=0x%04X/0x%04X", GetClusterName(), to_underlying(req.direction), req.numberOfSteps, req.GetClusterId(), mClusterId);
        
        // Error handling
        if(numberOfSteps == 0)
        {
            ctx.mCommandHandler.AddStatus(ctx.mRequestPath, Protocols::InteractionModel::Status::Success);
            return;
        }
        if(direction >= StepDirectionEnum::kUnknownEnumValue)
        {
            ctx.mCommandHandler.AddStatus(ctx.mRequestPath, Status::ConstraintError);
            return;
        }
        if(speed.HasValue())
        {
            if(speed.Value() >= Globals::ThreeLevelAutoEnum::kUnknownEnumValue)
            {
                ctx.mCommandHandler.AddStatus(ctx.mRequestPath, Status::ConstraintError);
                return;
            }
        }	
        
        // Delegate forwarding
        status = mDelegate.SetStepCallback(direction, numberOfSteps, speed.Value());

        ctx.mCommandHandler.AddStatus(ctx.mRequestPath, status);
    };
	
	/**
	 * Handle Command: SetTarget
	 */
    void HandleSetTargetCommand(HandlerContext & ctx, const Commands::SetTarget::DecodableType & req)
    {
        Status status = Status::Success;

        auto & position = req.position;
        auto & tagLatch = req.tagLatch;
        auto & speed    = req.speed;

        ChipLogDetail(Zcl, "%s ClDim: HandleLatchCommand", GetClusterName());


        // Error handling	
        if(position.HasValue())
        {
            if(position.Value() > kMaxPercent100ths)
            {
                ctx.mCommandHandler.AddStatus(ctx.mRequestPath, Status::ConstraintError);
                return;
            }
        }
        if(tagLatch.HasValue())
        {
            if(tagLatch.Value() >= TagLatchEnum::kUnknownEnumValue)
            {
                ctx.mCommandHandler.AddStatus(ctx.mRequestPath, Status::ConstraintError);
                return;
            }
        }
        if(speed.HasValue())
        {
            if(speed.Value() >= Globals::ThreeLevelAutoEnum::kUnknownEnumValue)
            {
                ctx.mCommandHandler.AddStatus(ctx.mRequestPath, Status::ConstraintError);
                return;
            }
        }	

        // Delegate forwarding
        status = mDelegate.SetSetTargetCallback(position.Value(), tagLatch.Value(), speed.Value());	
        ctx.mCommandHandler.AddStatus(ctx.mRequestPath, status);
    };

	/**
	 * This function is called by the interaction model engine when a command destined for this instance is received.
	 * Inherited from CommandHandlerInterface
	 */
	void InvokeCommand(HandlerContext & handlerContext)
    {
        ChipLogDetail(Zcl, "%s ClDim: InvokeCommand", GetClusterName());
    /*     if (handlerContext.mCommandHandled || !IsValidAliasCluster(handlerContext.mRequestPath.mClusterId))
        {
            // TODO check separately the mCommandHandled
            handlerContext.mCommandHandler.AddStatus(handlerContext.mRequestPath, Protocols::InteractionModel::Status::UnsupportedCluster);
            return;
        } */

        switch (handlerContext.mRequestPath.mCommandId)
        {
            case Commands::Step::Id: {
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
                }	
                break;
            case Commands::SetTarget::Id: {
                    ChipLogDetail(Zcl, "%s ClDim::Cmd::SetTarget()", GetClusterName());
                    Commands::SetTarget::DecodableType requestPayload;
                    handlerContext.SetCommandHandled();

                    if (DataModel::Decode(handlerContext.mPayload, requestPayload) != CHIP_NO_ERROR)
                    {
                        handlerContext.mCommandHandler.AddStatus(handlerContext.mRequestPath, Protocols::InteractionModel::Status::InvalidCommand);
                        return;
                    }
                    
                    // Payload ready to be used
                    HandleSetTargetCommand(handlerContext, requestPayload);
                }
                break;
            default:
                ChipLogError(Zcl, "%s ClDim: InvokeCommand unknown", GetClusterName());
                handlerContext.mCommandHandler.AddStatus(handlerContext.mRequestPath, Protocols::InteractionModel::Status::UnsupportedCommand);
                return;
                break;
        }

        // NOTE: TODO better error handing this is for demo purpose 
        handlerContext.mCommandHandler.AddStatus(handlerContext.mRequestPath, Protocols::InteractionModel::Status::Success);	
    };
	
	/**
	 * Inherited from AttributeAccessInterface.
	 * @return appropriately mapped CHIP_ERROR if applicable (may return CHIP_IM_GLOBAL_STATUS errors)
	 */
	CHIP_ERROR Read(const ConcreteReadAttributePath & aPath, AttributeValueEncoder & aEncoder) override
    {
        bool isAttributeSupported = false;
        switch (aPath.mAttributeId)
        {
        case Attributes::TagList::Id:
            ChipLogDetail(Zcl, "%s ClDim::Read::TagList", GetClusterName());
            // ToDo: To be Completed	
            break;

        // Positioning feature Attributes
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

        // Unit feature Attributes
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

        // Limitation feature Attributes
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
            //LogFeatureMap(mFeatureMap);
            ReturnErrorOnFailure(aEncoder.Encode(mFeatureMap));
            isAttributeSupported = true;
            break;
        }

        return isAttributeSupported ? CHIP_NO_ERROR : CHIP_IM_GLOBAL_STATUS(UnsupportedAttribute);
    };

	/**
     * This checks if the clusters instance is a valid ClosureDimension cluster based on the AliasedClusters list.
     * @return true if the cluster is a valid ClosureDimension cluster.
     */
	bool IsValidAliasCluster(ClusterId cId) const;
	inline bool IsValidAliasCluster() const { return IsValidAliasCluster(mClusterId); }

    uint32_t GenerateFeatureMap() const
    {
        BitMask<Feature, uint32_t> featureMap(0);

        if constexpr (FeaturePositioningEnabled)
        {
            featureMap.Set(Feature::kPositioning);
        }

        if constexpr (FeatureLatchingEnabled)
        {
            featureMap.Set(Feature::kMotionLatching);
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
    };

public:
    /**
     * Creates a Dimensions cluster instance for a given cluster ID. The Init() function needs to be called for this instance to be registered and
     * called by the interaction model at the appropriate times.
     * This constructor should be used for all features.
     * @param aEndpointId The endpoint on which this cluster exists. This must match the zap configuration.
     * @param aClusterId The ID of the ModeBase aliased cluster to be instantiated.
	 * @param aDelegate A reference to the delegate to be used by this server.
     * @param aRotationAxis needed along the Rotation feature.
     * @param aOverflow needed along the Rotation feature.
     * @param aModulation needed along the Modulation feature.
     * @param aLatchingAxis needed along the Latching feature only.
     * @param aTranslationDirection, needed along the Translation feature.
     */   
    Instance(Delegate & aDelegate, EndpointId aEndpointId, ClusterId aClusterId,
        RotationAxisEnum aRotationAxis,
        OverflowEnum aOverflow,
        ModulationTypeEnum aModulation,
        LatchingAxisEnum aLatchingAxis,
        TranslationDirectionEnum aTranslationDirection) :
        AttributeAccessInterface(Optional<EndpointId>(aEndpointId), aClusterId),
        CommandHandlerInterface(MakeOptional(aEndpointId), aClusterId),
        mDelegate(aDelegate), mEndpointId(aEndpointId), mClusterId(aClusterId)
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
        //LogFeatureMap(GenerateFeatureMap());
 
    }

    ~Instance() override
    {
        // ++++ ChipLogDetail(Zcl, "%s ClDim Instance.Destructor(): ID=0x%02lX EP=%u", GetClusterName(), long(mClusterId), mEndpointId);
        CommandHandlerInterfaceRegistry::Instance().UnregisterCommandHandler(this);
        AttributeAccessInterfaceRegistry::Instance().Unregister(this);
    };

    /* ******** For test purpose ******** */
    uint32_t GetFeatureMap() { return mFeatureMap; }
    EndpointId GetEndpoint() { return mEndpointId; }

    const char * GetClusterName() const
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
    /* void LogFeatureMap(const uint32_t & featureMap)
    {
        const BitMask<Feature> value = featureMap;

        ChipLogDetail(NotSpecified, "%s ClDim::FeatureMap=0x%08X (%u)", GetClusterName(), value.Raw(), value.Raw());

        LogIsFeatureSupported(featureMap, Feature::kPositioning);
        LogIsFeatureSupported(featureMap, Feature::kMotionLatching);
        LogIsFeatureSupported(featureMap, Feature::kUnit);
        LogIsFeatureSupported(featureMap, Feature::kLimitation);
        LogIsFeatureSupported(featureMap, Feature::kSpeed);
        LogIsFeatureSupported(featureMap, Feature::kTranslation);
        LogIsFeatureSupported(featureMap, Feature::kRotation);
        LogIsFeatureSupported(featureMap, Feature::kModulation);
    }; */
	
	/**
     * Initialise the closure dimension server instance.
     * This function must be called after defining an Instance class object.
     * @return Returns an error if the given endpoint and cluster ID have not been enabled in zap or if the
     * CommandHandler or AttributeHandler registration fails, else returns CHIP_NO_ERROR.
     */
    CHIP_ERROR Init()
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

        //VerifyOrReturnError(IsValidAliasCluster(), CHIP_ERROR_INCORRECT_STATE);

        // Check if the cluster has been selected in zap
        VerifyOrReturnError(emberAfContainsServer(mEndpointId, mClusterId), CHIP_ERROR_INCORRECT_STATE);

        // Register the object as attribute provider
        VerifyOrReturnError(AttributeAccessInterfaceRegistry::Instance().Register(this), CHIP_ERROR_INCORRECT_STATE);
        ReturnErrorOnFailure(CommandHandlerInterfaceRegistry::Instance().RegisterCommandHandler(this));

        mFeatureMap = GenerateFeatureMap();
        //LogFeatureMap(mFeatureMap);

        ChipLogDetail(NotSpecified, "%s ClDim Registered as Ep[%u] Id=0x%04X", GetClusterName(), mEndpointId, mClusterId);

        return CHIP_NO_ERROR;
    };
	
	/* Attrbutes that will be set during setup or via InvokeCommand. */

	// ToDO: SetTaglist()

    template <bool Enabled = FeaturePositioningEnabled || FeatureLatchingEnabled, typename = std::enable_if_t<Enabled, CHIP_ERROR>>
    CHIP_ERROR SetCurrent(Optional<Percent100ths> aPositioning, Optional<LatchingEnum> aLatch, Optional<Globals::ThreeLevelAutoEnum> aSpeed)
    {
        NCurrentStruct nullableCurrent = this->mCurrent;
        CurrentStruct current = { .position = 0, .latching = 1, .speed = 2 };
        
        if (aPositioning.HasValue())
        {
            if (aPositioning.Value() > kMaxPercent100ths)
            {
                return CHIP_ERROR_INVALID_ARGUMENT;
            }

            current.position = aPositioning.Value();
            nullableCurrent.SetNonNull(current);
        }
        if (aSpeed.HasValue())
        {
            if (aSpeed.Value() >= Globals::ThreeLevelAutoEnum::kUnknownEnumValue)
            {
                return CHIP_ERROR_INVALID_ARGUMENT;
            }

            current.speed = aSpeed.Value();
            nullableCurrent.SetNonNull(current);
        }	
        if (aLatch.HasValue())
        {
            if (aLatch.Value() >= LatchingEnum::kUnknownEnumValue)
            {
                return CHIP_ERROR_INVALID_ARGUMENT;
            }

            current.latching = aLatch.Value();
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
    };

	template <bool Enabled = FeaturePositioningEnabled || FeatureLatchingEnabled, typename = std::enable_if_t<Enabled, CHIP_ERROR>>
    CHIP_ERROR SetTarget(Optional<Percent100ths> aPositioning, Optional<TagLatchEnum> aTagLatch, Optional<Globals::ThreeLevelAutoEnum> aSpeed)
    {
        NTargetStruct nullableTarget = this->mTarget;
        TargetStruct target = { .position = 0, .tagLatch = 1, .speed = 2 };

        if (aPositioning.HasValue())
        {
            if (aPositioning.Value() > kMaxPercent100ths)
            {
                return CHIP_ERROR_INVALID_ARGUMENT;
            }

            target.position = aPositioning.Value();
            nullableTarget.SetNonNull(target);
        }
        if (aSpeed.HasValue())
        {
            if (aSpeed.Value() >= Globals::ThreeLevelAutoEnum::kUnknownEnumValue)
            {
                return CHIP_ERROR_INVALID_ARGUMENT;
            }

            target.speed = aSpeed.Value();
            nullableTarget.SetNonNull(target);
        }
        if (aTagLatch.HasValue())
        {
            if (aTagLatch.Value() >= TagLatchEnum::kUnknownEnumValue)
            {
                return CHIP_ERROR_INVALID_ARGUMENT;
            }

            target.tagLatch = aTagLatch.Value();
            nullableTarget.SetNonNull(target);
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

        return CHIP_NO_ERROR;
    };
	
	template <bool Enabled = FeaturePositioningEnabled, typename = std::enable_if_t<Enabled, CHIP_ERROR>>
    CHIP_ERROR SetResolutionAndStepValue(Percent100ths aResolutionPercent100ths, uint8_t aStepCoef)
    {
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
    };
	
	template <bool Enabled = FeatureUnitEnabled, typename = std::enable_if_t<Enabled, CHIP_ERROR>>
    CHIP_ERROR SetUnitAndRange(ClosureUnitEnum aUnit, int16_t aMin, int16_t aMax)
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
    };
	
	template <bool Enabled = FeatureLimitationEnabled, typename = std::enable_if_t<Enabled, CHIP_ERROR>>
    CHIP_ERROR SetLimitRange(Percent100ths aMin, Percent100ths aMax)
    {
        VerifyOrReturnError(aMax > aMin, CHIP_ERROR_INVALID_ARGUMENT);

        // Check to see if a change has ocurred
        VerifyOrReturnError(this->mLimitRange.min != aMin || this->mLimitRange.max != aMax, CHIP_NO_ERROR);
        this->mLimitRange.min = aMin;
        this->mLimitRange.max = aMax;
        MatterReportingAttributeChangeCallback(mEndpointId, mClusterId, Attributes::LimitRange::Id);

        return CHIP_NO_ERROR;
    };
        
	template <bool Enabled = FeatureTranslationEnabled, typename = std::enable_if_t<Enabled, CHIP_ERROR>>
    CHIP_ERROR SetTranslationDirection(TranslationDirectionEnum aTranslationDirection)
    {
        VerifyOrReturnError(EnsureKnownEnumValue(aTranslationDirection) != TranslationDirectionEnum::kUnknownEnumValue, CHIP_ERROR_INVALID_ARGUMENT);
        // Check to see if a change has ocurred
        VerifyOrReturnError(this->mTranslationDirection != aTranslationDirection, CHIP_NO_ERROR);
        this->mTranslationDirection = aTranslationDirection;
        MatterReportingAttributeChangeCallback(mEndpointId, mClusterId, Attributes::TranslationDirection::Id);

        return CHIP_NO_ERROR;
    };
	
	template <bool Enabled = FeatureRotationEnabled, typename = std::enable_if_t<Enabled, CHIP_ERROR>>
    CHIP_ERROR SetRotationAxis(RotationAxisEnum aRotationAxis)
    {
        VerifyOrReturnError(EnsureKnownEnumValue(aRotationAxis) != RotationAxisEnum::kUnknownEnumValue, CHIP_ERROR_INVALID_ARGUMENT);
        // Check to see if a change has ocurred
        VerifyOrReturnError(this->mRotationAxis != aRotationAxis, CHIP_NO_ERROR);
        this->mRotationAxis = aRotationAxis;
        MatterReportingAttributeChangeCallback(mEndpointId, mClusterId, Attributes::RotationAxis::Id);

        return CHIP_NO_ERROR;
    };
	
	template <bool Enabled = FeatureRotationEnabled || FeatureLatchingEnabled, typename = std::enable_if_t<Enabled, CHIP_ERROR>>
    CHIP_ERROR SetOverflow(OverflowEnum aOverflow)
    {
        VerifyOrReturnError(EnsureKnownEnumValue(aOverflow) != OverflowEnum::kUnknownEnumValue, CHIP_ERROR_INVALID_ARGUMENT);
        // Check to see if a change has ocurred
        VerifyOrReturnError(this->mOverflow != aOverflow, CHIP_NO_ERROR);
        this->mOverflow = aOverflow;
        MatterReportingAttributeChangeCallback(mEndpointId, mClusterId, Attributes::Overflow::Id);

        return CHIP_NO_ERROR;
    };
	
	template <bool Enabled = FeatureModulationEnabled, typename = std::enable_if_t<Enabled, CHIP_ERROR>>
    CHIP_ERROR SetModulationType(ModulationTypeEnum aModulationType)
    {
        VerifyOrReturnError(EnsureKnownEnumValue(aModulationType) != ModulationTypeEnum::kUnknownEnumValue, CHIP_ERROR_INVALID_ARGUMENT);
        // Check to see if a change has ocurred
        VerifyOrReturnError(this->mModulationType != aModulationType, CHIP_NO_ERROR);
        this->mModulationType = aModulationType;
        MatterReportingAttributeChangeCallback(mEndpointId, mClusterId, Attributes::ModulationType::Id);

        return CHIP_NO_ERROR;	
    };
	
	template <bool Enabled = FeatureLatchingEnabled && !FeaturePositioningEnabled, typename = std::enable_if_t<Enabled, CHIP_ERROR>>
    CHIP_ERROR SetLatchingAxis(LatchingAxisEnum aLatchingAxis)
    {
        VerifyOrReturnError(EnsureKnownEnumValue(aLatchingAxis) != LatchingAxisEnum::kUnknownEnumValue, CHIP_ERROR_INVALID_ARGUMENT);
        // Check to see if a change has ocurred
        VerifyOrReturnError(this->mLatchingAxis != aLatchingAxis, CHIP_NO_ERROR);
        this->mLatchingAxis = aLatchingAxis;
        MatterReportingAttributeChangeCallback(mEndpointId, mClusterId, Attributes::LatchingAxis::Id);

        return CHIP_NO_ERROR;
    };

	EndpointId GetEndpoint() const { return mEndpointId; };
};


} // namespace ClosureDimension
} // namespace Clusters
} // namespace app
} // namespace chip