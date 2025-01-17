#pragma once

#include "closures-operational-state-delegate.h"
#include <app/clusters/mode-base-server/mode-base-server.h>
#include <app/clusters/operational-state-server/operational-state-server.h>
#include <app/clusters/closure-dimension-server/closure-dimension-server.h>
#include "../../linux/MotionSimulator.h"

#include <string>


#if defined(CHIP_IMGUI_ENABLED) && CHIP_IMGUI_ENABLED
#include <imgui_ui/ui.h>

typedef void (example::Ui::ImguiUi::*AddWindow)(std::unique_ptr<example::Ui::Window> window);
#endif

using chip::Protocols::InteractionModel::Status;

namespace chip {
namespace app {
namespace Clusters {


class DimensionDelegate : public ClosureDimension::Delegate
{
public:
    DimensionDelegate() : ClosureDimension::Delegate() {ChipLogDetail(NotSpecified, "Delegate INIT");}
    ~DimensionDelegate() = default;

    Status SetStepCallback(ClosureDimension::StepDirectionEnum direction, uint16_t numberOfSteps, Globals::ThreeLevelAutoEnum speed) override
    {
        ChipLogDetail(NotSpecified, "SetStepCallback");
        return Status::Success;

    };

    Status SetSetTargetCallback(Percent100ths positioning, ClosureDimension::TagLatchEnum tagLatch, Globals::ThreeLevelAutoEnum speed) override
    {
        ChipLogDetail(NotSpecified, "SetSetTargetCallback");
        return Status::Success;
    }

};

class ClosuresDevice : public OperationalState::Observer 
{
private:

    ClosureOperationalState::ClosuresOperationalStateDelegate mOperationalStateDelegate;
    ClosureOperationalState::Instance mOperationalStateInstance;
    MotionSimulator mMotionSimulator;

    bool mCalibrating   = false;
    bool mProtected = false;
    bool mDisengaged = false;
    bool mNotOperational = false;
    bool mStopped = false;
    bool mRunning = false;
    bool mPaused = false;
    bool mError = true;

    bool mReadyToRun = false;
    bool mActionNeeded = false;
    bool mSetupRequired = false;
    bool mFallbackNeeded = false;

    ClosureOperationalState::Structs::OverallStateStruct::Type mOverallState;
    ClosureOperationalState::PositioningEnum mPositioning;
    ClosureOperationalState::LatchingEnum mLatching;
    Globals::ThreeLevelAutoEnum mSpeed;
    ClosureOperationalState::RestingProcedureEnum mRestingProcedure;
    ClosureOperationalState::TriggerConditionEnum mTriggerCondition;
    ClosureOperationalState::TriggerPositionEnum mTriggerPosition;
    uint16_t mWaitingDelay;
    uint16_t mKickoffTimer = 0;
    uint8_t mStateBeforePause = 0;

    void SetPositioning(ClosureOperationalState::PositioningEnum aPositioning);
    void SetLatching(ClosureOperationalState::LatchingEnum aLatching);
    void SetSpeed(Globals::ThreeLevelAutoEnum aSpeed);
    const char* GetStateString(ClosureOperationalState::OperationalStateEnum aOpState) const;
    ClosureOperationalState::PositioningEnum ConvertTagToPositioning(ClosureOperationalState::TagEnum aTag);

    // Todo rework
    void ClosureDimensionsSetup(chip::EndpointId endpoint);

    // ImGui addition
#if defined(CHIP_IMGUI_ENABLED) && CHIP_IMGUI_ENABLED
    example::Ui::ImguiUi * mImguiInstance;
    AddWindow mImguiCallback;

    void AddImGuiClosureDimensionInstance(chip::EndpointId aEp, const char * aName, uint32_t aFeature);
    void AddImGuiClosureOpStateInstance(chip::EndpointId aEp, const char * aName, uint32_t aFeature);
#endif

    // Add gRotationInstance and gDimensionDelegate as a class member
    DimensionDelegate *gDimensionDelegate = nullptr;
    ClosureDimension::Instance<true, false, false, true, true, true, false, false> gRotationInstance;

public:
    /**
     * This class is responsible for initialising all the Closures clusters and manging the interactions between them as required by
     * the specific "business logic". See the state machine diagram.
     * @param aClosuresClustersEndpoint The endpoint ID where all the Closures clusters exist.
     */
    using Feature = ClosureOperationalState::Feature;
    
    explicit ClosuresDevice(EndpointId aClosuresClustersEndpoint);

    ~ClosuresDevice();

    // Add a public method to allow adding observers
    void AddOperationalStateObserver(OperationalState::Observer * observer);

    // ImGui addition
#if defined(CHIP_IMGUI_ENABLED) && CHIP_IMGUI_ENABLED
    void AddImGuiInstance(AddWindow aCallback, example::Ui::ImguiUi * aGuiInstance);
#endif

    /**
     * Init all the clusters used by this device.
     */
    void Init();

    ClosureOperationalState::PositioningEnum GetPositioning();
    ClosureOperationalState::LatchingEnum GetLatching();
    Globals::ThreeLevelAutoEnum GetSpeed();
    ClosureOperationalState::Structs::OverallStateStruct::Type GetOverallState() const;

    void CheckReadiness(ReadinessCheckType aType, bool & aReady);

    // Call to observer to be notified of a state change
    void OnStateChanged(ClosureOperationalState::OperationalStateEnum state);

    /**
     * Handles the ClosureOperationalState pause command.
     */
    void HandleOpStatePauseCallback(Clusters::OperationalState::GenericOperationalError & err);

    /**
     * Handles the ClosureOperationalState resume command.
     */
    void HandleOpStateResumeCallback(Clusters::OperationalState::GenericOperationalError & err);

    /**
     * Handles the ClosureOperationalState stop command.
     */
    void HandleOpStateStopCallback(Clusters::OperationalState::GenericOperationalError & err);

    /**
     * Handles the ClosureOperationalState MoveTo command.
     */
    void HandleOpStateMoveToCallback(OperationalState::GenericOperationalError & err, const Optional<ClosureOperationalState::TagEnum> tag, 
                                                            const Optional<Globals::ThreeLevelAutoEnum> speed, 
                                                            const Optional<ClosureOperationalState::LatchingEnum> latch);

    /**
     * Handles the ClosureOperationalState ConfigureFallback command.
     */
    void HandleOpStateConfigureFallbackCallback(OperationalState::GenericOperationalError & err, const Optional<ClosureOperationalState::RestingProcedureEnum> restingProcedure,
                                                            const Optional<ClosureOperationalState::TriggerConditionEnum> triggerCondition,
                                                            const Optional<ClosureOperationalState::TriggerPositionEnum> triggerPosition, 
                                                            const Optional<uint16_t> waitingDelay);

    /**
     * Handles the ClosureOperationalState Cancel Fallback command.
     */
    void HandleOpStateCancelFallbackCallback(OperationalState::GenericOperationalError & err);

    /**
     * Handles the ClosureOperationalState Calibrate command.
     */
    void HandleOpStateCalibrateCallback(OperationalState::GenericOperationalError & err);

    /**
     * Handles the MoveTo command stimuli from app.
     */
    void ClosuresMoveToStimuli(std::optional<uint8_t> tag, std::optional<uint8_t> speed, std::optional<uint8_t> latch);

    /**
     * Handles the Stop command stimuli from app.
     */
    void ClosuresStopStimuli();

    /**
     * Handles the Calibrate command stimuli from app.
     */
    void ClosuresCalibrateStimuli();

    /**
     * Handles the ConfigureFallback command stimuli from app.
     */
    void ClosuresConfigureFallbackStimuli(std::optional<uint8_t> restingProcedure, std::optional<uint8_t> triggerCondition, 
                                        std::optional<uint8_t> triggerPosition, std::optional<uint16_t> waitingDelay);

    /**
     * Handles the Protected stimuli from app.
     */
    void ClosuresProtectedStimuli();

    /**
     * Handles the Unprotected stimuli from app.
     */
    void ClosuresUnprotectedStimuli();

    /**
     * Handles the Ready to run stimuli from app.
     */
    void ClosuresReadyToRunStimuli(bool aReady);

    /**
     * Handles the Action needed stimuli from app.
     */
    void ClosuresActionNeededStimuli(bool aActionNeeded);

    /**
     * Handles the Fallback needed stimuli from app.
     */
    void ClosuresFallbackNeededStimuli(bool aFallbackNeeded);

    /**
     * Handles the Setup Required stimuli from app.
     */
    void ClosuresSetupRequiredStimuli(bool aSetupRequired);

    /**
     * Sets the device to an error state with the error state ID matching the error name given.
     * @param error The error name. 
     */
    void HandleErrorEvent(const std::string & error);

    void HandleClearErrorMessage();

    void HandleResetStimuli();

};


class MockCommandHandler : public chip::app::CommandHandler {
public:
    MockCommandHandler() : chip::app::CommandHandler() {}

    // Implement any pure virtual methods here
    void AddStatus(const ConcreteCommandPath & aRequestCommandPath,
                           const Protocols::InteractionModel::ClusterStatusCode & aStatus, const char * context = nullptr) override 
    {
    }

    FabricIndex GetAccessingFabricIndex() const override
    {
        FabricIndex test = 1;
        return test;
    }

    bool IsTimedInvoke() const override 
    {
        bool test = true;
        return test;
    }

    virtual void FlushAcksRightAwayOnSlowCommand() override {}
    Access::SubjectDescriptor GetSubjectDescriptor() const override 
    {
        Access::SubjectDescriptor test;
        return test;
    }

    virtual CHIP_ERROR FallibleAddStatus(const ConcreteCommandPath & aRequestCommandPath,
                                         const Protocols::InteractionModel::ClusterStatusCode & aStatus,
                                         const char * context = nullptr) override 
                                         {
                                            return CHIP_NO_ERROR;
                                         }

    CHIP_ERROR AddResponseData(const ConcreteCommandPath & aRequestCommandPath, CommandId aResponseCommandId,
                                       const DataModel::EncodableToTLV & aEncodable) override
                                       {
                                        return CHIP_NO_ERROR;
                                       }
    void AddResponse(const ConcreteCommandPath & aRequestCommandPath, CommandId aResponseCommandId,
                             const DataModel::EncodableToTLV & aEncodable) override {}

    Messaging::ExchangeContext * GetExchangeContext() const override
    {
        return nullptr;
    }
};

} // namespace Clusters
} // namespace app
} // namespace chip
