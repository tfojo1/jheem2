# Baseline Metrics Summary

**Date:** 2024-11-20
**Branch:** dev (before refactoring)
**Commit:** $(git rev-parse --short HEAD)

---

## Overall Package Size

- **Total R code:** 79,977 lines
- **Total modules:** 11 (SPECIFICATION, JHEEM, LIKELIHOODS, etc.)
- **Total R files:** ~40 files

---

## Largest Files (Top 10)

| File | Lines | Target Package |
|------|-------|----------------|
| SPECIFICATION_model_specification.R | 10,967 | jheem.specification |
| JHEEM_engine.R | 7,231 | jheem.engine |
| ONTOLOGY_ontology_mappings.R | 3,759 | **jheem.core** |
| LIKELIHOODS_nested_proportion_likelihood.R | 3,372 | jheem.models |
| DATA_MANAGER_data_manager.R | 3,252 | jheem.engine |
| SPECIFICATION_compiled_specification.R | 3,199 | jheem.specification |
| SPECIFICATION_functional_forms.R | 3,099 | jheem.specification |
| JHEEM_simulation.R | 2,541 | jheem.engine |
| CALIBRATION_main.R | 1,963 | jheem.analysis |
| LIKELIHOODS_basic_likelihood.R | 1,675 | jheem.models |

---

## Files for jheem.core (8 files)

**Total lines:** ~8,732

| File | Lines | Size |
|------|-------|------|
| ONTOLOGY_ontology_mappings.R | 3,759 | 175K |
| HELPERS_dim_names_helpers.R | 1,141 | 44K |
| HELPERS_age_year_helpers.R | ~1,000 | 39K |
| HELPERS_array_helpers.R | ~950 | 37K |
| VERSIONS_version_manager.R | ~900 | 35K |
| ONTOLOGY_ontology.R | ~620 | 24K |
| HELPERS_misc_helpers.R | ~360 | 14K |
| HELPERS_bundle_function.R | ~180 | 7.0K |

**C++ files to include:**
- src/ontology_mappings.cpp
- src/array_helpers.cpp

---

## Module Distribution

| Module | Files | Approx Lines | Target Package |
|--------|-------|--------------|----------------|
| SPECIFICATION | 6 | 19,906 | jheem.specification |
| JHEEM | 5 | 13,847 | jheem.engine |
| ONTOLOGY | 2 | 4,455 | **jheem.core** |
| HELPERS | 5 | 3,489 | **jheem.core** |
| LIKELIHOODS | 7 | 6,862 | jheem.models |
| INTERVENTIONS | 4 | 4,460 | jheem.models |
| DATA_MANAGER | 1 | 3,252 | jheem.engine |
| CALIBRATION | 2 | 2,931 | jheem.analysis |
| PLOTS | 3 | 2,109 | jheem.analysis |
| VERSIONS | 1 | 788 | **jheem.core** |
| FILE_MANAGER | 1 | 576 | jheem.analysis |

---

## Known Issues

- **Circular dependencies:** 11 (per ARCHITECTURE_ANALYSIS.md)
- **Mega-files:** 2 files > 7,000 lines
- **Test framework:** Manual scripts, no testthat

---

## Phase 1 Targets

### Week 1 Goal: Extract jheem.core
- [ ] 8 R files extracted
- [ ] 2 C++ files extracted
- [ ] Builds independently
- [ ] Has basic tests
- [ ] jheem2 uses it successfully

### Phase 1 Complete (Week 8)
- [ ] All 5 packages extracted
- [ ] Each builds independently
- [ ] All original functionality preserved
- [ ] Performance < 5% regression
