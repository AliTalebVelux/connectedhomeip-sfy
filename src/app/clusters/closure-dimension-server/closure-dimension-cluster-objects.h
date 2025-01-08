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

#include <app-common/zap-generated/cluster-objects.h>
#include <app/CommandHandlerInterface.h>
#include <app/util/util.h>
#include <utility>

namespace chip {
namespace app {
namespace Clusters {
namespace ClosureDimension {

struct Info {
    ClusterId cId;
    const char * name;
};

static constexpr std::array<Info, 5> const AliasedClusters = {{ 
                                                            { Closure1stDimension::Id, "1st" },
                                                            { Closure2ndDimension::Id, "2nd" },
                                                            { Closure3rdDimension::Id, "3rd" },
                                                            { Closure4thDimension::Id, "4th" },
                                                            { Closure5thDimension::Id, "5th" },
                                                       }};

// Redeclare alias toward the first Cluster dimension for common DataType
/* Enum, BitMap, Structure */

using ClosureUnitEnum = Closure1stDimension::ClosureUnitEnum; //--> Clusters::detail::UnitEnum;

using LatchingEnum = Closure1stDimension::LatchingEnum;
using TagLatchEnum = Closure1stDimension::TagLatchEnum;

using TranslationDirectionEnum = Closure1stDimension::TranslationDirectionEnum;
using RotationAxisEnum = Closure1stDimension::RotationAxisEnum;
using ModulationTypeEnum = Closure1stDimension::ModulationTypeEnum;
using LatchingAxisEnum = Closure1stDimension::LatchingAxisEnum;
using OverflowEnum = Closure1stDimension::OverflowEnum;

using UnitRangeStruct = Closure1stDimension::Structs::UnitRangeStruct::Type;

using RangePercent100thsStruct = Closure1stDimension::Structs::RangePercent100thsStruct::Type;
using NRangePercent100thsStruct = Closure1stDimension::Attributes::LimitRange::TypeInfo::Type; // Nullable

using CurrentStruct = Closure1stDimension::Structs::CurrentStruct::Type;
using NCurrentStruct = Closure1stDimension::Attributes::Current::TypeInfo::Type; // Nullable
using TargetStruct = Closure1stDimension::Structs::TargetStruct::Type;
using NTargetStruct = Closure1stDimension::Attributes::Target::TypeInfo::Type; // Nullable

//using SemanticTagStruct = Closure1stDimension::Structs::SemanticTagStruct::Type; 

using Feature = Closure1stDimension::Feature;
//using TXXT = chip::app::DataModel::Nullable<chip::BitMask<chip::app::Clusters::Closure1stDimension::PositioningBitmap>>; ///using PositioningBitmap = Clusters::detail::PositioningBitmap;
//chip::app::DataModel::Nullable<chip::BitMask<chip::app::Clusters::Closure4thDimension::PositioningBitmap>>;
//using SignedValuesRangeStruct = Closure1stDimension::Structs::SignedValuesRangeStruct;
// enum class Feature : uint32_t
// {
//     kNumericMeasurement = 0x1,
//     kLevelIndication    = 0x2,
//     kMediumLevel        = 0x4,
//     kCriticalLevel      = 0x8,
//     kPeakMeasurement    = 0x10,
//     kAverageMeasurement = 0x20,
// };

/* Attributes::Id */
namespace Attributes {

namespace TagList {
static constexpr AttributeId Id = Closure1stDimension::Attributes::TagList::Id;
} // namespace TagList

namespace Current {
static constexpr AttributeId Id = Closure1stDimension::Attributes::Current::Id;
} // namespace Current

namespace Target {
static constexpr AttributeId Id = Closure1stDimension::Attributes::Target::Id;
} // namespace Target

namespace Resolution {
static constexpr AttributeId Id = Closure1stDimension::Attributes::Resolution::Id;
} // namespace Resolution

namespace StepValue {
static constexpr AttributeId Id = Closure1stDimension::Attributes::StepValue::Id;
} // namespace StepValue

namespace Unit {
static constexpr AttributeId Id = Closure1stDimension::Attributes::Unit::Id;
} // namespace Unit

namespace UnitRange {
static constexpr AttributeId Id = Closure1stDimension::Attributes::UnitRange::Id;
} // namespace UnitRange

namespace LimitRange {
static constexpr AttributeId Id = Closure1stDimension::Attributes::LimitRange::Id;
} // namespace LimitRange

namespace TranslationDirection {
static constexpr AttributeId Id = Closure1stDimension::Attributes::TranslationDirection::Id;
} // namespace TranslationDirection

namespace RotationAxis {
static constexpr AttributeId Id = Closure1stDimension::Attributes::RotationAxis::Id;
} // namespace RotationAxis

namespace Overflow {
static constexpr AttributeId Id = Closure1stDimension::Attributes::Overflow::Id;
} // namespace OverFlow

namespace ModulationType {
static constexpr AttributeId Id = Closure1stDimension::Attributes::ModulationType::Id;
} // namespace ModulationType

namespace LatchingAxis {
static constexpr AttributeId Id = Closure1stDimension::Attributes::LatchingAxis::Id;
} // namespace LatchingAxis

namespace GeneratedCommandList {
static constexpr AttributeId Id = Globals::Attributes::GeneratedCommandList::Id;
} // namespace GeneratedCommandList

namespace AcceptedCommandList {
static constexpr AttributeId Id = Globals::Attributes::AcceptedCommandList::Id;
} // namespace AcceptedCommandList

namespace AttributeList {
static constexpr AttributeId Id = Globals::Attributes::AttributeList::Id;
} // namespace AttributeList

namespace FeatureMap {
static constexpr AttributeId Id = Globals::Attributes::FeatureMap::Id;
} // namespace FeatureMap

namespace ClusterRevision {
static constexpr AttributeId Id = Globals::Attributes::ClusterRevision::Id;
} // namespace ClusterRevision

} // namespace Attributes

/* Commands::Id */
namespace Commands {

namespace SetTarget  {
static constexpr CommandId Id = Closure1stDimension::Commands::SetTarget::Id;
using DecodableType = Closure1stDimension::Commands::SetTarget::DecodableType;
} // namespace SetTarget 
namespace Step {
static constexpr CommandId Id = Closure1stDimension::Commands::Step::Id;
using DecodableType = Closure1stDimension::Commands::Step::DecodableType;
} // namespace Steps

} // namespace Commands

} // namespace ClosureDimension
} // namespace Clusters
} // namespace app
} // namespace chip
