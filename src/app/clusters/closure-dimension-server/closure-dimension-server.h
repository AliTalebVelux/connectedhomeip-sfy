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

class Delegate;

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
    Instance(Delegate & aDelegate, EndpointId aEndpointId, ClusterId aClusterId, //(((((reference based delegate )))))
        RotationAxisEnum aRotationAxis,
        OverflowEnum aOverflow,
        ModulationTypeEnum aModulation,
        LatchingAxisEnum aLatchingAxis,
        TranslationDirectionEnum aTranslationDirection);

    ~Instance() override;
	
	/**
     * Initialise the closure dimension server instance.
     * This function must be called after defining an Instance class object.
     * @return Returns an error if the given endpoint and cluster ID have not been enabled in zap or if the
     * CommandHandler or AttributeHandler registration fails, else returns CHIP_NO_ERROR.
     */
    CHIP_ERROR Init();
	
	/* Attrbutes that will be set during setup or via InvokeCommand. */

	// ToDO: SetTaglist()

    template <bool Enabled = FeaturePositioningEnabled || FeatureLatchingEnabled, typename std::enable_if_t<Enabled, CHIP_ERROR>> 
	CHIP_ERROR SetCurrent(Optional<Percent100ths> aPositioning, Optional<LatchingEnum> aLatch, Optional<Globals::ThreeLevelAutoEnum> aSpeed);

	template <bool Enabled = FeaturePositioningEnabled || FeatureLatchingEnabled, typename std::enable_if_t<Enabled, CHIP_ERROR>>	
	CHIP_ERROR SetTarget(Optional<Percent100ths> aPositioning, Optional<TagLatchEnum> aTagLatch, Optional<Globals::ThreeLevelAutoEnum> aSpeed);
	
	// template <bool Enabled = FeaturePositioningEnabled, typename = std::enable_if_t<Enabled, CHIP_ERROR>>
    template <bool Enabled = FeaturePositioningEnabled, typename std::enable_if_t<Enabled, CHIP_ERROR>>
	CHIP_ERROR SetResolutionAndStepValue(Percent100ths aResolutionPercent100ths, uint8_t aStepCoef);
	
	// ++template <bool Enabled = FeatureUnitEnabled, typename = std::enable_if_t<Enabled, CHIP_ERROR>>
    template <bool Enabled = FeatureUnitEnabled, typename std::enable_if_t<Enabled, CHIP_ERROR>>
	CHIP_ERROR SetUnitAndRange(ClosureUnitEnum aUnit, int16_t aMin, int16_t aMax);
	
	// ++template <bool Enabled = FeatureLimitationEnabled, typename = std::enable_if_t<Enabled, CHIP_ERROR>>
    template <bool Enabled = FeatureLimitationEnabled, typename std::enable_if_t<Enabled, CHIP_ERROR>>
	CHIP_ERROR SetLimitRange(Percent100ths aMin, Percent100ths aMax);
	
	// ++template <bool Enabled = FeatureTranslationEnabled, typename = std::enable_if_t<Enabled, CHIP_ERROR>>
    template <bool Enabled = FeatureTranslationEnabled, typename std::enable_if_t<Enabled, CHIP_ERROR>>
	CHIP_ERROR SetTranslationDirection(TranslationDirectionEnum aTranslationDirection);
	
	// ++template <bool Enabled = FeatureRotationEnabled, typename = std::enable_if_t<Enabled, CHIP_ERROR>>
    template <bool Enabled = FeatureRotationEnabled, typename std::enable_if_t<Enabled, CHIP_ERROR>>
	CHIP_ERROR SetRotationAxis(RotationAxisEnum aRotationAxis);
	
	// ++template <bool Enabled = FeatureRotationEnabled || FeatureLatchingEnabled, typename = std::enable_if_t<Enabled, CHIP_ERROR>>
    template <bool Enabled = FeatureRotationEnabled || FeatureLatchingEnabled, typename std::enable_if_t<Enabled, CHIP_ERROR>>
	CHIP_ERROR SetOverflow(OverflowEnum aOverflow);
	
	// ++template <bool Enabled = FeatureModulationEnabled, typename = std::enable_if_t<Enabled, CHIP_ERROR>>
    template <bool Enabled = FeatureModulationEnabled, typename std::enable_if_t<Enabled, CHIP_ERROR>>
	CHIP_ERROR SetModulationType(ModulationTypeEnum aModulationType);
	
	// ++template <bool Enabled = FeatureLatchingEnabled && !FeaturePositioningEnabled, typename = std::enable_if_t<Enabled, CHIP_ERROR>>
    template <bool Enabled = FeatureLatchingEnabled, typename std::enable_if_t<Enabled, CHIP_ERROR>>
	CHIP_ERROR SetLatchingAxis(LatchingAxisEnum aLatchingAxis);

	EndpointId GetEndpoint() const { return mEndpointId; }
	
private:
	Delegate & mDelegate;
	
    const EndpointId mEndpointId;
    const ClusterId mClusterId;
	
	static const Percent100ths kMaxPercent100ths = 10000; //?????
    static const Percent100ths kMinPercent100ths = 0;	  //?????
    uint32_t mFeatureMap = 0;
	
	/**
	 * This function is called by the interaction model engine when a command destined for this instance is received.
	 * Inherited from CommandHandlerInterface
	 */
	void InvokeCommand(HandlerContext & handlerContext);
	
	/**
	 * Inherited from AttributeAccessInterface.
	 * @return appropriately mapped CHIP_ERROR if applicable (may return CHIP_IM_GLOBAL_STATUS errors)
	 */
	CHIP_ERROR Read(const ConcreteReadAttributePath & aPath, AttributeValueEncoder & aEncoder) override;
	
	/**
	 * Handle Command: Step
	 */
	void HandleStepCommand(HandlerContext & ctx, const Commands::Step::DecodableType & req);
	
	/**
	 * Handle Command: Latch
	 */
	void HandleSetTargetCommand(HandlerContext & ctx, const Commands::SetTarget::DecodableType & req);

	/**
     * This generates a feature bitmap from the enabled features and then returns its raw value.
     * @return The raw feature bitmap.
     */
    uint32_t GenerateFeatureMap() const;
	
	/**
     * This checks if the clusters instance is a valid ClosureDimension cluster based on the AliasedClusters list.
     * @return true if the cluster is a valid ClosureDimension cluster.
     */
	bool IsValidAliasCluster(ClusterId cId) const;
	inline bool IsValidAliasCluster() const { return IsValidAliasCluster(mClusterId); }
	
	// For test purpose only
	const char * GetClusterName() const;
};

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

} // namespace ClosureDimension
} // namespace Clusters
} // namespace app
} // namespace chip