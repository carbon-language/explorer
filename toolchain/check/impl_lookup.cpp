// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#include "toolchain/check/impl_lookup.h"

#include <algorithm>

#include "toolchain/base/kind_switch.h"
#include "toolchain/check/deduce.h"
#include "toolchain/check/diagnostic_helpers.h"
#include "toolchain/check/generic.h"
#include "toolchain/check/import_ref.h"
#include "toolchain/check/inst.h"
#include "toolchain/check/type.h"
#include "toolchain/check/type_completion.h"
#include "toolchain/check/type_structure.h"
#include "toolchain/sem_ir/ids.h"
#include "toolchain/sem_ir/impl.h"
#include "toolchain/sem_ir/inst.h"
#include "toolchain/sem_ir/typed_insts.h"

namespace Carbon::Check {

static auto FindAssociatedImportIRs(Context& context,
                                    SemIR::ConstantId query_self_const_id,
                                    SemIR::ConstantId query_facet_type_const_id)
    -> llvm::SmallVector<SemIR::ImportIRId> {
  llvm::SmallVector<SemIR::ImportIRId> result;

  // Add an entity to our result.
  auto add_entity = [&](const SemIR::EntityWithParamsBase& entity) {
    // We will look for impls in the import IR associated with the first owning
    // declaration.
    auto decl_id = entity.first_owning_decl_id;
    if (!decl_id.has_value()) {
      return;
    }
    if (auto ir_id = GetCanonicalImportIRInst(context, decl_id).ir_id;
        ir_id.has_value()) {
      result.push_back(ir_id);
    }
  };

  llvm::SmallVector<SemIR::InstId> worklist;
  worklist.push_back(context.constant_values().GetInstId(query_self_const_id));
  worklist.push_back(
      context.constant_values().GetInstId(query_facet_type_const_id));

  // Push the contents of an instruction block onto our worklist.
  auto push_block = [&](SemIR::InstBlockId block_id) {
    if (block_id.has_value()) {
      llvm::append_range(worklist, context.inst_blocks().Get(block_id));
    }
  };

  // Add the arguments of a specific to the worklist.
  auto push_args = [&](SemIR::SpecificId specific_id) {
    if (specific_id.has_value()) {
      push_block(context.specifics().Get(specific_id).args_id);
    }
  };

  while (!worklist.empty()) {
    auto inst_id = worklist.pop_back_val();

    // Visit the operands of the constant.
    auto inst = context.insts().Get(inst_id);
    auto [arg0_kind, arg1_kind] = inst.ArgKinds();
    for (auto [arg, kind] :
         {std::pair{inst.arg0(), arg0_kind}, {inst.arg1(), arg1_kind}}) {
      switch (kind) {
        case SemIR::IdKind::For<SemIR::InstId>: {
          if (auto id = SemIR::InstId(arg); id.has_value()) {
            worklist.push_back(id);
          }
          break;
        }
        case SemIR::IdKind::For<SemIR::InstBlockId>: {
          push_block(SemIR::InstBlockId(arg));
          break;
        }
        case SemIR::IdKind::For<SemIR::ClassId>: {
          add_entity(context.classes().Get(SemIR::ClassId(arg)));
          break;
        }
        case SemIR::IdKind::For<SemIR::InterfaceId>: {
          add_entity(context.interfaces().Get(SemIR::InterfaceId(arg)));
          break;
        }
        case SemIR::IdKind::For<SemIR::FacetTypeId>: {
          const auto& facet_type_info =
              context.facet_types().Get(SemIR::FacetTypeId(arg));
          for (const auto& impl : facet_type_info.impls_constraints) {
            add_entity(context.interfaces().Get(impl.interface_id));
            push_args(impl.specific_id);
          }
          break;
        }
        case SemIR::IdKind::For<SemIR::FunctionId>: {
          add_entity(context.functions().Get(SemIR::FunctionId(arg)));
          break;
        }
        case SemIR::IdKind::For<SemIR::SpecificId>: {
          push_args(SemIR::SpecificId(arg));
          break;
        }
        default: {
          break;
        }
      }
    }
  }

  // Deduplicate.
  llvm::sort(result, [](SemIR::ImportIRId a, SemIR::ImportIRId b) {
    return a.index < b.index;
  });
  result.erase(llvm::unique(result), result.end());

  return result;
}

// Returns true if a cycle was found and diagnosed.
static auto FindAndDiagnoseImplLookupCycle(
    Context& context,
    const llvm::SmallVector<Context::ImplLookupStackEntry>& stack,
    SemIR::LocId loc_id, SemIR::ConstantId query_self_const_id,
    SemIR::ConstantId query_facet_type_const_id) -> bool {
  // Deduction of the interface parameters can do further impl lookups, and we
  // need to ensure we terminate.
  //
  // https://docs.carbon-lang.dev/docs/design/generics/details.html#acyclic-rule
  // - We look for violations of the acyclic rule by seeing if a previous lookup
  //   had all the same type inputs.
  // - The `query_facet_type_const_id` encodes the entire facet type being
  //   looked up, including any specific parameters for a generic interface.
  //
  // TODO: Implement the termination rule, which requires looking at the
  // complexity of the types on the top of (or throughout?) the stack:
  // https://docs.carbon-lang.dev/docs/design/generics/details.html#termination-rule
  for (auto [i, entry] : llvm::enumerate(stack)) {
    if (entry.query_self_const_id == query_self_const_id &&
        entry.query_facet_type_const_id == query_facet_type_const_id) {
      auto facet_type_type_id =
          context.types().GetTypeIdForTypeConstantId(query_facet_type_const_id);
      CARBON_DIAGNOSTIC(ImplLookupCycle, Error,
                        "cycle found in search for impl of {0} for type {1}",
                        SemIR::TypeId, SemIR::TypeId);
      auto builder = context.emitter().Build(
          loc_id, ImplLookupCycle, facet_type_type_id,
          context.types().GetTypeIdForTypeConstantId(query_self_const_id));
      for (const auto& active_entry : llvm::drop_begin(stack, i)) {
        if (active_entry.impl_loc.has_value()) {
          CARBON_DIAGNOSTIC(ImplLookupCycleNote, Note,
                            "determining if this impl clause matches", );
          builder.Note(active_entry.impl_loc, ImplLookupCycleNote);
        }
      }
      builder.Emit();
      return true;
    }
  }
  return false;
}

// Gets the set of `SpecificInterface`s that are required by a facet type
// (as a constant value).
static auto GetInterfacesFromConstantId(
    Context& context, SemIR::ConstantId query_facet_type_const_id,
    bool& has_other_requirements)
    -> llvm::SmallVector<SemIR::SpecificInterface> {
  auto facet_type_inst_id =
      context.constant_values().GetInstId(query_facet_type_const_id);
  auto facet_type_inst =
      context.insts().GetAs<SemIR::FacetType>(facet_type_inst_id);
  const auto& facet_type_info =
      context.facet_types().Get(facet_type_inst.facet_type_id);
  has_other_requirements = facet_type_info.other_requirements;
  // TODO: This needs to match the order of witnesses for the facet type, which
  // will need to be maintained once we add support for named constraints.
  return facet_type_info.impls_constraints;
}

static auto GetWitnessIdForImpl(Context& context, SemIR::LocId loc_id,
                                SemIR::ConstantId query_self_const_id,
                                const SemIR::SpecificInterface& interface,
                                SemIR::ImplId impl_id) -> SemIR::InstId {
  // The impl may have generic arguments, in which case we need to deduce them
  // to find what they are given the specific type and interface query. We use
  // that specific to map values in the impl to the deduced values.
  auto specific_id = SemIR::SpecificId::None;
  {
    // DeduceImplArguments can import new impls which can invalidate any
    // pointers into `context.impls()`.
    const SemIR::Impl& impl = context.impls().Get(impl_id);
    if (impl.generic_id.has_value()) {
      specific_id =
          DeduceImplArguments(context, loc_id,
                              {.self_id = impl.self_id,
                               .generic_id = impl.generic_id,
                               .specific_id = impl.interface.specific_id},
                              query_self_const_id, interface.specific_id);
      if (!specific_id.has_value()) {
        return SemIR::InstId::None;
      }
    }
  }

  // Get a pointer again after DeduceImplArguments() is complete.
  const SemIR::Impl& impl = context.impls().Get(impl_id);

  // The self type of the impl must match the type in the query, or this is an
  // `impl T as ...` for some other type `T` and should not be considered.
  auto deduced_self_const_id = SemIR::GetConstantValueInSpecific(
      context.sem_ir(), specific_id, impl.self_id);
  // In a generic `impl forall` the self type can be a FacetAccessType, which
  // will not be the same constant value as a query facet value. We move through
  // to the facet value here, and if the query was a FacetAccessType we did the
  // same there so they still match.
  if (auto access = context.insts().TryGetAs<SemIR::FacetAccessType>(
          context.constant_values().GetInstId(deduced_self_const_id))) {
    deduced_self_const_id =
        context.constant_values().Get(access->facet_value_inst_id);
  }
  if (query_self_const_id != deduced_self_const_id) {
    return SemIR::InstId::None;
  }

  // The impl's constraint is a facet type which it is implementing for the self
  // type: the `I` in `impl ... as I`. The deduction step may be unable to be
  // fully applied to the types in the constraint and result in an error here,
  // in which case it does not match the query.
  auto deduced_constraint_id =
      context.constant_values().GetInstId(SemIR::GetConstantValueInSpecific(
          context.sem_ir(), specific_id, impl.constraint_id));
  if (deduced_constraint_id == SemIR::ErrorInst::SingletonInstId) {
    return SemIR::InstId::None;
  }

  auto deduced_constraint_facet_type_id =
      context.insts()
          .GetAs<SemIR::FacetType>(deduced_constraint_id)
          .facet_type_id;
  const auto& deduced_constraint_facet_type_info =
      context.facet_types().Get(deduced_constraint_facet_type_id);
  CARBON_CHECK(deduced_constraint_facet_type_info.impls_constraints.size() ==
               1);

  if (deduced_constraint_facet_type_info.other_requirements) {
    // TODO: Remove this when other requirements goes away.
    return SemIR::InstId::None;
  }

  // The specifics in the queried interface must match the deduced specifics in
  // the impl's constraint facet type.
  auto impl_interface_specific_id =
      deduced_constraint_facet_type_info.impls_constraints[0].specific_id;
  auto query_interface_specific_id = interface.specific_id;
  if (impl_interface_specific_id != query_interface_specific_id) {
    return SemIR::InstId::None;
  }

  LoadImportRef(context, impl.witness_id);
  if (specific_id.has_value()) {
    // We need a definition of the specific `impl` so we can access its
    // witness.
    ResolveSpecificDefinition(context, loc_id, specific_id);
  }
  return context.constant_values().GetInstId(SemIR::GetConstantValueInSpecific(
      context.sem_ir(), specific_id, impl.witness_id));
}

// In the case where `facet_const_id` is a facet, see if its facet type requires
// that `specific_interface` is implemented. If so, return the witness from the
// facet.
static auto FindWitnessInFacet(
    Context& context, SemIR::LocId loc_id, SemIR::ConstantId facet_const_id,
    const SemIR::SpecificInterface& specific_interface) -> SemIR::InstId {
  SemIR::InstId facet_inst_id =
      context.constant_values().GetInstId(facet_const_id);
  SemIR::TypeId facet_type_id = context.insts().Get(facet_inst_id).type_id();
  if (auto facet_type_inst =
          context.types().TryGetAs<SemIR::FacetType>(facet_type_id)) {
    const auto& facet_type_info =
        context.facet_types().Get(facet_type_inst->facet_type_id);
    // TODO: This depends on the index into `impls_constraints` matching
    // the index into the facet type witness. This will have to be maintained
    // even for facet types that include named constraints, once that is
    // supported.
    for (auto [index, interface] :
         llvm::enumerate(facet_type_info.impls_constraints)) {
      if (interface == specific_interface) {
        return GetOrAddInst(
            context, loc_id,
            SemIR::FacetAccessWitness{
                .type_id = GetSingletonType(
                    context, SemIR::WitnessType::SingletonInstId),
                .facet_value_inst_id = facet_inst_id,
                .index = SemIR::ElementIndex(index)});
      }
    }
  }
  return SemIR::InstId::None;
}

// Finds the best impl among all available impls that provides the
// `specific_interface` for the type in `type_const_id`, and returns a witness
// for that impl. Returns `None` if no match was found.
static auto FindWitnessInImpls(
    Context& context, SemIR::LocId loc_id,
    SemIR::ConstantId query_self_const_id,
    const SemIR::SpecificInterface& specific_interface) -> SemIR::InstId {
  auto& stack = context.impl_lookup_stack();

  struct CandidateImpl {
    SemIR::ImplId impl_id;
    SemIR::InstId loc_inst_id;
    TypeStructure type_structure;
  };

  auto query_type_structure = BuildTypeStructure(
      context, context.constant_values().GetInstId(query_self_const_id),
      specific_interface);

  llvm::SmallVector<CandidateImpl> candidate_impl_ids;
  for (auto [id, impl] : context.impls().enumerate()) {
    // If the impl's interface_id differs from the query, then this impl can not
    // possibly provide the queried interface.
    if (impl.interface.interface_id != specific_interface.interface_id) {
      continue;
    }

    // When the impl's interface_id matches, but the interface is generic, the
    // impl may or may not match based on restrictions in the generic parameters
    // of the impl.
    //
    // As a shortcut, if the impl's constraint is not symbolic (does not depend
    // on any generic parameters), then we can determine that we match if the
    // specific ids match exactly.
    auto impl_interface_const_id =
        context.constant_values().Get(impl.constraint_id);
    if (!impl_interface_const_id.is_symbolic()) {
      if (impl.interface.specific_id != specific_interface.specific_id) {
        continue;
      }
    }

    // This check comes first to avoid deduction with an invalid impl. We use an
    // error value to indicate an error during creation of the impl, such as a
    // recursive impl which will cause deduction to recurse infinitely.
    if (impl.witness_id == SemIR::ErrorInst::SingletonInstId) {
      continue;
    }
    CARBON_CHECK(impl.witness_id.has_value());

    auto type_structure =
        BuildTypeStructure(context, impl.self_id, impl.interface);
    if (!query_type_structure.IsCompatibleWith(type_structure)) {
      continue;
    }

    candidate_impl_ids.push_back(
        {id, impl.definition_id, std::move(type_structure)});
  }

  auto compare = [](auto& lhs, auto& rhs) -> bool {
    // TODO: Allow Carbon code to provide a priority ordering explicitly. For
    // now they have all the same priority, so the priority is the order in
    // which they are found in code.

    // Sort by their type structures. Higher value in type structure comes
    // first, so we use `>` comparison.
    return lhs.type_structure > rhs.type_structure;
  };
  // Stable sort is used so that impls that are seen first are preferred when
  // they have an equal priority ordering.
  std::ranges::stable_sort(candidate_impl_ids, compare);

  for (const auto& candidate : candidate_impl_ids) {
    stack.back().impl_loc = candidate.loc_inst_id;
    // NOTE: GetWitnessIdForImpl() does deduction, which can cause new impls to
    // be imported, invalidating any pointer into `context.impls()`.
    auto witness_id =
        GetWitnessIdForImpl(context, loc_id, query_self_const_id,
                            specific_interface, candidate.impl_id);
    if (witness_id.has_value()) {
      return witness_id;
    }
  }
  return SemIR::InstId::None;
}

auto LookupImplWitness(Context& context, SemIR::LocId loc_id,
                       SemIR::ConstantId query_self_const_id,
                       SemIR::ConstantId query_facet_type_const_id)
    -> SemIR::InstBlockIdOrError {
  if (query_self_const_id == SemIR::ErrorInst::SingletonConstantId ||
      query_facet_type_const_id == SemIR::ErrorInst::SingletonConstantId) {
    return SemIR::InstBlockIdOrError::MakeError();
  }

  {
    // The query self value is a type value or a facet value.
    auto query_self_type_id =
        context.insts()
            .Get(context.constant_values().GetInstId(query_self_const_id))
            .type_id();
    CARBON_CHECK(context.types().Is<SemIR::TypeType>(query_self_type_id) ||
                 context.types().Is<SemIR::FacetType>(query_self_type_id));
    // The query facet type value is indeed a facet type.
    CARBON_CHECK(context.insts().Is<SemIR::FacetType>(
        context.constant_values().GetInstId(query_facet_type_const_id)));
  }

  // If the self type is a FacetAccessType, work with the facet value directly,
  // which gives us the potential witnesses to avoid looking for impl
  // declarations. We will do the same for the impl declarations we try to match
  // so that we can compare the self constant values.
  if (auto access = context.insts().TryGetAs<SemIR::FacetAccessType>(
          context.constant_values().GetInstId(query_self_const_id))) {
    query_self_const_id =
        context.constant_values().Get(access->facet_value_inst_id);
  }

  auto import_irs = FindAssociatedImportIRs(context, query_self_const_id,
                                            query_facet_type_const_id);
  for (auto import_ir : import_irs) {
    // TODO: Instead of importing all impls, only import ones that are in some
    // way connected to this query.
    for (auto impl_index : llvm::seq(
             context.import_irs().Get(import_ir).sem_ir->impls().size())) {
      // TODO: Track the relevant impls and only consider those ones and any
      // local impls, rather than looping over all impls below.
      ImportImpl(context, import_ir, SemIR::ImplId(impl_index));
    }
  }

  if (FindAndDiagnoseImplLookupCycle(context, context.impl_lookup_stack(),
                                     loc_id, query_self_const_id,
                                     query_facet_type_const_id)) {
    return SemIR::InstBlockIdOrError::MakeError();
  }

  bool has_other_requirements = false;
  auto interfaces = GetInterfacesFromConstantId(
      context, query_facet_type_const_id, has_other_requirements);
  if (has_other_requirements) {
    // TODO: Remove this when other requirements go away.
    return SemIR::InstBlockId::None;
  }
  if (interfaces.empty()) {
    return SemIR::InstBlockId::Empty;
  }

  llvm::SmallVector<SemIR::InstId> result_witness_ids;

  auto& stack = context.impl_lookup_stack();
  stack.push_back({
      .query_self_const_id = query_self_const_id,
      .query_facet_type_const_id = query_facet_type_const_id,
  });

  // We look for a witness in two different places if the query self type is a
  // facet value. First we try to find a witness on the facet value itself. This
  // avoids the need to do impl lookups which are more expensive. If that fails,
  // then we go look for an impl declaration as we would for other self types.
  auto facet_value_self_const_id = query_self_const_id;

  // When the query is a concrete FacetValue, we want to look through it at the
  // underlying type to find all interfaces it implements. This supports
  // conversion from a FacetValue to any other possible FacetValue, since
  // conversion depends on impl lookup to verify it is a valid type change. See
  // https://github.com/carbon-language/carbon-lang/issues/5137. We can't do
  // this step earlier than inside impl lookup since we want the converted facet
  // value in `facet_value_self_const_id` to avoid looking for impl
  // declarations.
  if (auto facet_value = context.insts().TryGetAs<SemIR::FacetValue>(
          context.constant_values().GetInstId(query_self_const_id))) {
    query_self_const_id =
        context.constant_values().Get(facet_value->type_inst_id);
  }

  // We need to find a witness for each interface in `interfaces`. Every
  // consumer of a facet type needs to agree on the order of interfaces used for
  // its witnesses.
  for (const auto& interface : interfaces) {
    // TODO: Since both `interfaces` and `query_self_const_id` are sorted lists,
    // do an O(N+M) merge instead of O(N*M) nested loops.
    auto result_witness_id = FindWitnessInFacet(
        context, loc_id, facet_value_self_const_id, interface);
    if (!result_witness_id.has_value()) {
      result_witness_id =
          FindWitnessInImpls(context, loc_id, query_self_const_id, interface);
    }
    if (result_witness_id.has_value()) {
      result_witness_ids.push_back(result_witness_id);
    } else {
      // At least one queried interface in the facet type has no witness for the
      // given type, we can stop looking for more.
      break;
    }
  }
  stack.pop_back();
  // TODO: Validate that the witness satisfies the other requirements in
  // `interface_const_id`.

  // All interfaces in the query facet type must have been found to be available
  // through some impl, or directly on the value's facet type if
  // `query_self_const_id` is a facet value.
  if (result_witness_ids.size() != interfaces.size()) {
    return SemIR::InstBlockId::None;
  }

  return context.inst_blocks().AddCanonical(result_witness_ids);
}

}  // namespace Carbon::Check
