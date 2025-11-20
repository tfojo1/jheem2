# jheem2 Refactoring Execution Plan

**Created:** 2024-11-20
**Status:** Ready to Execute
**Approach:** Phased mono-repo → multi-repo transition

---

## Table of Contents

- [Executive Summary](#executive-summary)
- [Why We're Doing This](#why-were-doing-this)
- [The Strategy: Phased Approach](#the-strategy-phased-approach)
- [Phase 1: Mono-Repo Extraction (Weeks 1-8)](#phase-1-mono-repo-extraction-weeks-1-8)
- [Phase 2: Stabilization & Cycle Breaking (Weeks 9-12)](#phase-2-stabilization--cycle-breaking-weeks-9-12)
- [Phase 3: Multi-Repo Split (Month 4+)](#phase-3-multi-repo-split-month-4)
- [Success Criteria](#success-criteria)
- [Risk Management](#risk-management)
- [Session-to-Session Continuity](#session-to-session-continuity)

---

## Executive Summary

**Goal:** Transform jheem2 from a monolithic 62,942-line package with 11 circular dependencies into a clean 5-package architecture.

**Approach:** Start with mono-repo for agile refactoring, then split to multi-repo when stable.

**Timeline:**
- Phase 1 (Weeks 1-8): Extract packages within single repo
- Phase 2 (Weeks 9-12): Break circular dependencies, stabilize
- Phase 3 (Month 4+): Split to separate repos

**Key Insight:** Multi-repo is the right end state, but mono-repo during refactoring avoids coordination hell.

---

## Why We're Doing This

### Current Pain Points

From [ARCHITECTURE_ANALYSIS.md](ARCHITECTURE_ANALYSIS.md):

1. **11 Circular Dependencies** - Can't test components in isolation, changes ripple unpredictably
2. **2 Mega-Files** - SPECIFICATION (19,906 lines), JHEEM (13,847 lines) are hard to navigate
3. **No Formal Testing** - Manual test scripts instead of automated testthat framework
4. **Tight Coupling** - Everything depends on everything

### What Success Looks Like

**After Phase 3:**

```
jheem.core/              (separate repo) - 8,732 lines
  ├── Ontology system
  ├── Helpers
  └── Version management

jheem.specification/     (separate repo) - 19,906 lines
  ├── Model definitions
  └── Parameters

jheem.engine/            (separate repo) - 17,099 lines
  ├── Simulation execution
  └── Data management

jheem.models/            (separate repo) - 11,322 lines
  ├── Interventions
  └── Likelihoods

jheem.analysis/          (separate repo) - 5,616 lines
  ├── Calibration
  ├── Plotting
  └── File I/O

jheem2/                  (umbrella repo)
  └── Re-exports everything for backward compatibility
```

**Benefits:**
- ✅ Zero circular dependencies
- ✅ Each package testable in isolation
- ✅ New developers can understand one package at a time
- ✅ Users can install only what they need
- ✅ Easier to maintain and evolve

---

## The Strategy: Phased Approach

### Why Phased?

**Option A: Extract to multi-repo immediately**
- ❌ Breaking cycles requires coordinated changes across repos (PAINFUL)
- ❌ Can't do atomic commits when refactoring touches multiple packages
- ❌ Need to coordinate 5 PRs for one logical change
- ❌ Testing integration is harder

**Option B: Reorganize in place, then extract**
- ❌ Large-scale file moves in one go (risky)
- ❌ Merge conflicts with ongoing dev work
- ❌ Hard to demonstrate progress

**Option C: Mono-repo transition, then split (CHOSEN)**
- ✅ Extract packages one at a time (incremental progress)
- ✅ Atomic commits across packages while refactoring
- ✅ Easy testing (all in one repo)
- ✅ Split to multi-repo when stable (get standard approach benefits)
- ✅ Show progress every week

### The Key Insight

**Your git history shows:**
- JHEEM (engine): 17 changes in 6 months
- ONTOLOGY/HELPERS (core): 1 change in 6 months

**Implication:** Foundation packages are stable. Once extracted, they won't need frequent cross-package changes. Multi-repo will work well - but NOT during the active refactoring phase.

---

## Phase 1: Mono-Repo Extraction (Weeks 1-8)

### Goal
Extract all 5 sub-packages into `packages/` directory while keeping jheem2 repo as single package.

### Repository Structure

```
jheem2/  (still ONE repo, ONE git)
├── packages/
│   ├── jheem.core/
│   │   ├── DESCRIPTION
│   │   ├── NAMESPACE
│   │   ├── R/
│   │   │   ├── ONTOLOGY_*.R
│   │   │   ├── HELPERS_*.R
│   │   │   └── VERSIONS_*.R
│   │   ├── src/
│   │   │   ├── ontology_mappings.cpp
│   │   │   └── array_helpers.cpp
│   │   └── tests/testthat/
│   │       └── test-ontology.R
│   ├── jheem.specification/
│   │   ├── DESCRIPTION (Imports: jheem.core)
│   │   └── R/
│   ├── jheem.engine/
│   ├── jheem.models/
│   └── jheem.analysis/
├── R/                  (legacy - gradually empty)
├── src/                (legacy - gradually empty)
├── tests/              (legacy)
├── DESCRIPTION         (jheem2 - depends on sub-packages)
└── .Rbuildignore       (exclude packages/ for now)
```

### Week-by-Week Plan

#### Week 1-2: Extract jheem.core

**Why jheem.core first?**
- ✅ Zero circular dependencies (HELPERS depends on nothing)
- ✅ Self-contained utilities
- ✅ High value (used by everything)
- ✅ Low risk (pure functions, easy to test)
- ✅ Fast win to prove the approach

**Steps:**

1. **Create branch**
   ```bash
   git checkout dev
   git checkout -b refactor/extract-packages
   ```

2. **Create package structure**
   ```bash
   mkdir -p packages/jheem.core/{R,src,tests/testthat}
   ```

3. **Copy files**
   ```bash
   cp R/ONTOLOGY_*.R packages/jheem.core/R/
   cp R/HELPERS_*.R packages/jheem.core/R/
   cp R/VERSIONS_*.R packages/jheem.core/R/
   cp src/ontology_mappings.cpp packages/jheem.core/src/
   cp src/array_helpers.cpp packages/jheem.core/src/
   ```

4. **Create DESCRIPTION** for jheem.core
   - List dependencies: Rcpp, reshape2, locations
   - No dependencies on other jheem packages

5. **Build and test jheem.core in isolation**
   ```bash
   cd packages/jheem.core
   Rscript -e "Rcpp::compileAttributes()"
   Rscript -e "devtools::document()"
   Rscript -e "devtools::test()"
   Rscript -e "devtools::install()"
   ```

6. **Update main jheem2 package**
   - Add to DESCRIPTION: `Imports: jheem.core`
   - Option A: Keep old files, exclude from build (safer)
   - Option B: Delete old files, rely on installed jheem.core (cleaner)

7. **Test integration**
   ```bash
   cd ../..  # Back to jheem2 root
   devtools::load_all()
   # Run existing tests - should still pass
   ```

**Success Criteria:**
- ✅ jheem.core builds independently
- ✅ jheem.core has 5+ tests passing
- ✅ jheem2 still works with jheem.core as dependency
- ✅ All existing jheem2 tests pass

**Deliverable:** Working jheem.core package + updated jheem2

---

#### Week 3-4: Extract jheem.specification

**Why specification second?**
- ✅ Only depends on jheem.core (no other dependencies yet)
- ✅ Large module (19,906 lines) - high impact
- ✅ Tests dependency injection pattern

**Challenge:** SPECIFICATION is massive (one 10,967-line file)

**Approach:** Extract first, split later
- Week 3-4: Extract SPECIFICATION_*.R files as-is
- Phase 2: Split the mega-file into smaller pieces

**Steps:**

1. **Create package structure**
   ```bash
   mkdir -p packages/jheem.specification/{R,tests/testthat}
   ```

2. **Copy files**
   ```bash
   cp R/SPECIFICATION_*.R packages/jheem.specification/R/
   ```

3. **Create DESCRIPTION**
   - Imports: jheem.core

4. **Build and test**
   ```bash
   cd packages/jheem.specification
   devtools::document()
   devtools::test()
   devtools::install()
   ```

5. **Update jheem2**
   - Add to DESCRIPTION: `Imports: jheem.specification`
   - Update or remove old SPECIFICATION files

6. **Test integration**

**Success Criteria:**
- ✅ jheem.specification builds independently
- ✅ Depends only on jheem.core (clean dependency)
- ✅ jheem2 still works
- ✅ Can create specifications and register them

---

#### Week 5-6: Extract jheem.engine

**Why engine third?**
- ✅ Depends on core + specification (logical progression)
- ✅ Contains critical simulation logic
- ✅ Includes DATA_MANAGER

**Components:**
- JHEEM_*.R files (13,847 lines)
- DATA_MANAGER_*.R files (3,252 lines)
- C++ code: diffeq.cpp, engine_*.cpp

**Steps:**

1. **Create package structure**
   ```bash
   mkdir -p packages/jheem.engine/{R,src,tests/testthat}
   ```

2. **Copy files**
   ```bash
   cp R/JHEEM_*.R packages/jheem.engine/R/
   cp R/DATA_MANAGER_*.R packages/jheem.engine/R/
   cp src/diffeq.cpp packages/jheem.engine/src/
   cp src/engine_*.cpp packages/jheem.engine/src/
   cp src/functional_forms.cpp packages/jheem.engine/src/
   ```

3. **Create DESCRIPTION**
   - Imports: jheem.core, jheem.specification, deSolve, Rcpp

4. **Handle C++ compilation**
   ```bash
   cd packages/jheem.engine
   Rcpp::compileAttributes()
   ```

5. **Build and test**

6. **Test full simulation workflow**
   ```r
   spec <- create.jheem.specification(...)
   engine <- create.jheem.engine(...)
   sim <- engine$run(...)
   ```

**Success Criteria:**
- ✅ jheem.engine builds with C++ code
- ✅ Full simulation workflow works
- ✅ ODE solver performance unchanged

---

#### Week 7: Extract jheem.models

**Components:**
- INTERVENTIONS_*.R (4,460 lines)
- LIKELIHOODS_*.R (6,862 lines)
- C++ code: nested_proportion_likelihood.cpp

**Note:** This package has circular dependencies with engine and specification. For now, we extract as-is. We'll break cycles in Phase 2.

**Steps:**

1. Create package structure
2. Copy INTERVENTIONS and LIKELIHOODS files
3. Copy relevant C++ code
4. Create DESCRIPTION (Imports: core, specification, engine)
5. Build and test

**Success Criteria:**
- ✅ Can create interventions
- ✅ Can create likelihoods
- ✅ (Circular dependencies still exist - we'll fix in Phase 2)

---

#### Week 8: Extract jheem.analysis

**Components:**
- CALIBRATION_*.R (2,931 lines)
- PLOTS_*.R (2,109 lines)
- FILE_MANAGER_*.R (576 lines)

**Why last?**
- Top of the dependency stack
- Depends on everything else

**Steps:**

1. Create package structure
2. Copy files
3. Create DESCRIPTION (Imports: all other packages, ggplot2, plotly)
4. Build and test

**Success Criteria:**
- ✅ Can run calibrations
- ✅ Can create plots
- ✅ Full workflow end-to-end works

---

### Phase 1 Deliverables

**At end of Week 8:**

```
jheem2/ (one repo, one git)
├── packages/
│   ├── jheem.core/        ✅ (builds independently)
│   ├── jheem.specification/ ✅ (builds independently)
│   ├── jheem.engine/      ✅ (builds independently)
│   ├── jheem.models/      ✅ (builds independently)
│   └── jheem.analysis/    ✅ (builds independently)
├── R/                     (empty or legacy stubs)
├── DESCRIPTION            (imports all sub-packages)
└── README.md              (updated with new structure)
```

**Status:**
- ✅ All 5 packages extracted
- ✅ Each builds independently
- ✅ All original functionality works
- ⚠️ Circular dependencies still exist (addressed in Phase 2)
- ⚠️ Some mega-files still exist (split in Phase 2)

---

## Phase 2: Stabilization & Cycle Breaking (Weeks 9-12)

### Goal
Break circular dependencies and split mega-files while still in mono-repo.

### Why in mono-repo?
Breaking cycles requires coordinated changes across packages. In mono-repo:
- ✅ Atomic commits across packages
- ✅ Test everything together
- ✅ Can refactor aggressively
- ✅ Rollback is easy

In multi-repo, this would be coordination hell.

### Week 9: Break SPECIFICATION ↔ INTERVENTIONS Cycle

**The Problem:**
```
SPECIFICATION → INTERVENTIONS → SPECIFICATION
```
- SPECIFICATION needs intervention types
- INTERVENTIONS needs specification structure

**The Solution:** Registry pattern (from ARCHITECTURE_ANALYSIS.md)

Move intervention slot *definitions* to SPECIFICATION, keep *implementations* in INTERVENTIONS.

```r
# In jheem.specification
JHEEM.SPECIFICATION$set("public", "register_intervention_slot",
  function(slot_name, affected_compartments) {
    # Just records that interventions CAN be applied
  }
)

# In jheem.models (interventions)
# Implementations reference the spec, but spec doesn't reference back
TestingIntervention$validate <- function(spec) {
  # Check if slot exists in spec
}
```

**Steps:**

1. Identify all places where SPECIFICATION imports from INTERVENTIONS
2. Move type definitions to SPECIFICATION
3. Keep implementations in INTERVENTIONS
4. Test that cycle is broken

**Success Criteria:**
- ✅ SPECIFICATION → INTERVENTIONS (one-way)
- ✅ Cycle eliminated
- ✅ All tests still pass

---

### Week 10: Break JHEEM ↔ INTERVENTIONS Cycle

**The Problem:**
```
JHEEM.ENGINE → INTERVENTIONS → JHEEM.ENGINE
```

**The Solution:** Dependency inversion (interface pattern)

```r
# In jheem.engine/interfaces.R
IInterventionEffect <- R6Class("IInterventionEffect",
  public = list(
    apply_to_state = function(state, time, params) {
      stop("Must implement")
    }
  )
)

# JHEEM.ENGINE depends on interface only
apply_interventions <- function(state, interventions) {
  for (int in interventions) {
    if (!inherits(int, "IInterventionEffect")) stop("...")
    state <- int$apply_to_state(state, ...)
  }
}

# In jheem.models/INTERVENTIONS
TestingEffect <- R6Class("TestingEffect",
  inherit = IInterventionEffect,  # Implements interface
  public = list(
    apply_to_state = function(state, time, params) {
      # Implementation
    }
  )
)
```

**Success Criteria:**
- ✅ ENGINE → IInterventionEffect (abstract)
- ✅ INTERVENTIONS → ENGINE (for interface only)
- ✅ Cycle broken

---

### Week 11: Break Remaining Cycles

Apply same patterns to:
- ONTOLOGY ↔ SPECIFICATION
- JHEEM ↔ FILE_MANAGER
- Any others discovered

**Target:** Get from 11 cycles to 0 cycles.

---

### Week 12: Split Mega-Files

**SPECIFICATION_model_specification.R (10,967 lines)**

Split into:
```
specification/
  core.R              (R6 skeleton)
  compartments.R      (compartment methods)
  parameters.R        (parameter methods)
  quantities.R        (quantity methods)
  functional_forms.R  (already split)
  inheritance.R       (version inheritance)
  validation.R        (input checking)
```

Use facade pattern during transition:
```r
# SPECIFICATION_model_specification.R (old file)
# Now just sources the split files
source('specification/core.R')
source('specification/compartments.R')
# ... etc
```

**Success Criteria:**
- ✅ No file > 3,000 lines
- ✅ All tests pass
- ✅ No performance regression

---

### Phase 2 Deliverables

**At end of Week 12:**

- ✅ Zero circular dependencies
- ✅ All files < 3,000 lines
- ✅ Clean dependency graph
- ✅ All tests passing
- ✅ Ready for multi-repo split

---

## Phase 3: Multi-Repo Split (Month 4+)

### Goal
Extract each package to its own repository.

### Why Now?
- ✅ Packages are stable
- ✅ Cycles broken - clean boundaries
- ✅ Testing infrastructure in place
- ✅ Rare cross-package changes expected

### Process

For each package:

1. **Create new repo**
   ```bash
   cd packages/jheem.core
   git init
   git add .
   git commit -m "Initial commit: extracted from jheem2"
   gh repo create your-org/jheem.core --public
   git remote add origin https://github.com/your-org/jheem.core
   git push -u origin main
   ```

2. **Set up CI**
   ```yaml
   # .github/workflows/R-CMD-check.yml
   name: R-CMD-check
   on: [push, pull_request]
   jobs:
     check:
       runs-on: ubuntu-latest
       steps:
         - uses: actions/checkout@v3
         - uses: r-lib/actions/setup-r@v2
         - uses: r-lib/actions/setup-r-dependencies@v2
         - run: rcmdcheck::rcmdcheck()
   ```

3. **Update jheem2 to reference external packages**
   ```r
   # jheem2/DESCRIPTION
   Remotes:
     your-org/jheem.core,
     your-org/jheem.specification,
     your-org/jheem.engine,
     your-org/jheem.models,
     your-org/jheem.analysis
   ```

4. **Transform jheem2 into umbrella package**
   ```r
   # jheem2/R/jheem2.R
   #' @importFrom jheem.core ontology
   #' @export
   jheem.core::ontology

   #' @importFrom jheem.specification create.jheem.specification
   #' @export
   jheem.specification::create.jheem.specification

   # ... re-export all major functions
   ```

### Order of Extraction

1. **Week 1:** jheem.core (no dependencies)
2. **Week 2:** jheem.specification (depends on core)
3. **Week 3:** jheem.engine (depends on core + spec)
4. **Week 4:** jheem.models + jheem.analysis

### User Migration

**For users who install from repo:**

**Before (single repo):**
```r
devtools::install_github("your-org/jheem2")
```

**After (umbrella still works):**
```r
devtools::install_github("your-org/jheem2")
# Behind the scenes: installs all sub-packages via Remotes
```

**Advanced users (selective install):**
```r
# Install only what you need
devtools::install_github("your-org/jheem.core")
devtools::install_github("your-org/jheem.specification")
devtools::install_github("your-org/jheem.engine")
```

### Phase 3 Deliverables

**At completion:**

```
GitHub org: your-org/
├── jheem.core          (independent repo)
├── jheem.specification (independent repo)
├── jheem.engine        (independent repo)
├── jheem.models        (independent repo)
├── jheem.analysis      (independent repo)
└── jheem2              (umbrella repo)
```

**User experience:**
- ✅ Existing users: no change (install jheem2 umbrella)
- ✅ New users: can install selectively
- ✅ CRAN submission: each package independent
- ✅ Standard R ecosystem approach

---

## Success Criteria

### Phase 1 Success Metrics

| Metric | Target |
|--------|--------|
| Packages extracted | 5/5 |
| Each builds independently | 100% |
| Circular dependencies | Still 11 (will fix in Phase 2) |
| Original functionality works | 100% |
| Performance regression | < 5% |
| Test coverage | ≥ current baseline |

### Phase 2 Success Metrics

| Metric | Target |
|--------|--------|
| Circular dependencies | 0 |
| Max file size | < 3,000 lines |
| Dependency graph depth | 5 clean layers |
| Tests passing | 100% |
| Performance regression | 0% |

### Phase 3 Success Metrics

| Metric | Target |
|--------|--------|
| Independent repos | 5 |
| Each has CI | 100% |
| User migration issues | < 5 reported |
| Backward compatibility | 100% |

---

## Risk Management

### High-Priority Risks

#### Risk 1: Breaking R-C++ Interface
**Probability:** Medium
**Impact:** High (breaks core functionality)

**Mitigation:**
- Don't move C++ code until tested in mono-repo
- Extensive tests for ODE solver before extraction
- Keep integration tests for engine performance
- Benchmark before and after

**Rollback:** Revert git branch, < 1 hour recovery

---

#### Risk 2: Performance Regression in ODE Solver
**Probability:** Low
**Impact:** High (makes simulations slow)

**Mitigation:**
- Benchmark before Phase 1 starts
- Profile after each major change
- Track simulation time in CI
- Keep performance tests in test suite

**Rollback:** Revert changes, investigate offline

---

#### Risk 3: Breaking Cycles Creates New Issues
**Probability:** Medium
**Impact:** Medium (might need redesign)

**Mitigation:**
- Break ONE cycle at a time
- Test extensively after each cycle break
- Have patterns documented (from ARCHITECTURE_ANALYSIS.md)
- Team review before major changes

**Rollback:** Revert to end of Phase 1 (packages extracted but cycles intact)

---

#### Risk 4: Users Update Mid-Refactoring
**Probability:** Low (refactor branch separate from dev)
**Impact:** Medium (confused users)

**Mitigation:**
- All work on `refactor/extract-packages` branch
- Don't merge to dev until phase complete
- Clear communication about branches
- Tag releases: v1.9.x (old), v2.0.x (refactored)

**Rollback:** N/A (users won't see until merged)

---

#### Risk 5: Losing Momentum After Phase 1
**Probability:** Medium
**Impact:** High (incomplete refactoring)

**Mitigation:**
- Show progress weekly (demos to team)
- Document wins clearly
- Make each phase independently valuable
- If stuck, can stop after Phase 1 (still valuable)

**Rollback:** Phase 1 alone provides value (organized code)

---

## Session-to-Session Continuity

### Tracking Progress

**Location:** `.refactoring/` directory (tracked in git)

```
.refactoring/
├── STATUS.md              # Current phase, completed steps
├── DECISIONS.md           # Architecture decisions made
├── ISSUES.md              # Problems encountered and solutions
└── metrics/
    ├── baselines/         # Performance, file sizes, etc.
    └── current/           # Updated after each major change
```

### STATUS.md Format

```markdown
# Refactoring Status

**Current Phase:** 1 (Mono-Repo Extraction)
**Week:** 1 of 8
**Current Task:** Extracting jheem.core

## Completed
- [x] Week 1, Day 1: Created refactor branch
- [x] Week 1, Day 1: Created packages/ directory
- [ ] Week 1, Day 2: Extract jheem.core files

## Next Steps
1. Copy ONTOLOGY files to packages/jheem.core/R/
2. Create DESCRIPTION for jheem.core
3. Build and test jheem.core

## Blockers
None currently

## Notes
- Decided to use mono-repo approach in Phase 1
- Will split to multi-repo in Phase 3
```

### Starting a New Session

**At start of each session:**

1. Read `.refactoring/STATUS.md`
2. Review last session's ISSUES.md
3. Check current git branch
4. Verify tests still pass

**Command:**
```bash
cat .refactoring/STATUS.md
git status
git log -5 --oneline
devtools::test()  # If mid-extraction
```

### Ending a Session

**At end of each session:**

1. Update STATUS.md with progress
2. Commit all changes
3. Push to remote (backup)
4. Update session log

**Command:**
```bash
# Update status
vim .refactoring/STATUS.md

# Commit
git add .
git commit -m "refactor: [describe progress]"
git push origin refactor/extract-packages

# Log session
echo "Session $(date): [summary]" >> .claude-sessions/refactoring_log.txt
```

---

## Quick Reference

### Key Commands

**Building a sub-package:**
```bash
cd packages/jheem.core
Rscript -e "Rcpp::compileAttributes()"
Rscript -e "devtools::document()"
Rscript -e "devtools::test()"
Rscript -e "devtools::install()"
```

**Testing integration:**
```bash
cd ../../  # Back to jheem2 root
Rscript -e "devtools::load_all()"
Rscript -e "devtools::test()"
```

**Checking for circular dependencies:**
```r
# Create analysis script
source('.refactoring/scripts/check_cycles.R')
```

**Benchmarking performance:**
```r
source('.refactoring/scripts/benchmark.R')
```

### Key Files

| File | Purpose |
|------|---------|
| ARCHITECTURE_ANALYSIS.md | Original comprehensive analysis |
| REFACTORING_EXECUTION_PLAN.md | This document - the execution roadmap |
| .refactoring/STATUS.md | Current progress tracker |
| .refactoring/DECISIONS.md | Architecture decisions log |
| .refactoring/ISSUES.md | Problems and solutions |

### Decision Log Template

When making architectural decisions, log them:

```markdown
## Decision: [Title]
**Date:** 2024-11-20
**Context:** Why we needed to decide
**Options:**
1. Option A - pros/cons
2. Option B - pros/cons
**Decision:** Chose Option B
**Rationale:** Why we chose it
**Consequences:** What this means going forward
```

---

## Summary

**Phase 1 (Weeks 1-8):** Extract packages to mono-repo
- ✅ Fast progress, atomic commits
- ✅ Easy to test integration
- ✅ Show value every week

**Phase 2 (Weeks 9-12):** Break cycles, stabilize
- ✅ Mono-repo makes refactoring easy
- ✅ Can do atomic commits across packages
- ✅ Get to clean architecture

**Phase 3 (Month 4+):** Split to multi-repo
- ✅ Standard R ecosystem approach
- ✅ Each package evolves independently
- ✅ Ready for CRAN submission

**The key insight:** Use mono-repo as a refactoring tool, then split when stable. Best of both worlds.

---

**Ready to start?** → See Phase 1, Week 1 above for first steps.

**Questions?** → Review ARCHITECTURE_ANALYSIS.md for detailed context.

**Track progress?** → Update .refactoring/STATUS.md after each session.
