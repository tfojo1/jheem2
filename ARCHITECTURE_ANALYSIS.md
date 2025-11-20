# jheem2 Architecture Analysis & Refactoring Proposal

**Date:** 2025-10-23
**Author:** Claude Code Analysis
**Status:** Proposal

---

## Executive Summary

The jheem2 package has a solid conceptual architecture with clear separation of concerns reflected in file naming conventions. However, **11 circular dependencies** and **2 mega-files** (20k and 14k lines) prevent clean sub-package extraction. This document provides:

1. Detailed dependency analysis of current state
2. Proposed 5-package architecture with clean layers
3. Concrete patterns to break circular dependencies
4. 6-month incremental migration strategy
5. Success metrics and next steps

**Key Finding:** This codebase is highly splittable - the modules are logically separate, they just need dependency inversion and interface extraction.

---

## Table of Contents

- [Current State Analysis](#current-state-analysis)
- [Dependency Crisis](#dependency-crisis)
- [Proposed Architecture](#proposed-architecture)
- [Sub-Package Structure](#sub-package-structure)
- [Breaking the Cycles](#breaking-the-cycles)
- [Migration Strategy](#migration-strategy)
- [Practical Challenges](#practical-challenges)
- [Success Metrics](#success-metrics)
- [Recommended Next Steps](#recommended-next-steps)

---

## Current State Analysis

### Module Sizes

**Total:** 62,942 lines across 11 modules

```
SPECIFICATION    19,906 lines  (32%) - Model definition & parameters
JHEEM            13,847 lines  (22%) - Simulation engine
LIKELIHOODS       6,862 lines  (11%) - Statistical fitting
INTERVENTIONS     4,460 lines   (7%) - Policy modeling
ONTOLOGY          4,455 lines   (7%) - Dimension mapping
HELPERS           3,489 lines   (6%) - Utilities
DATA_MANAGER      3,252 lines   (5%) - Data storage
CALIBRATION       2,931 lines   (5%) - MCMC
PLOTS             2,109 lines   (3%) - Visualization
VERSIONS            788 lines   (1%) - Version management
FILE_MANAGER        576 lines   (1%) - I/O
```

### Dependency Matrix

*(rows depend on columns, numbers show # of cross-module references)*

```
              CALIBRATION DATA_MANAGER FILE_MANAGER HELPERS INTERVENTIONS JHEEM
CALIBRATION             0            1            4       1             0     5
DATA_MANAGER            0            0            0       3             0     0
FILE_MANAGER            0            0            0       0             0     1
HELPERS                 0            0            0       0             0     0
INTERVENTIONS           0            0            0       1             0     3
JHEEM                   0            1            5       9             3     0
LIKELIHOODS             0            6            0       5             0     3
ONTOLOGY                0            0            0       7             0     0
PLOTS                   0            2            0       0             0     0
SPECIFICATION           0            0            0       3             1     0
VERSIONS                1            0            0       0             1     0

              LIKELIHOODS ONTOLOGY PLOTS SPECIFICATION VERSIONS
CALIBRATION             3        0     0             0        0
DATA_MANAGER            0        9     0             1        0
FILE_MANAGER            0        0     0             0        0
HELPERS                 0        0     0             0        0
INTERVENTIONS           0        3     0             3        0
JHEEM                   0        6     0             8        1
LIKELIHOODS             0        6     0             6        0
ONTOLOGY                0        0     0             1        0
PLOTS                   0        3     0             0        0
SPECIFICATION           0       11     0             0        2
VERSIONS                0        1     0             0        0
```

### Key Observations

**Strengths:**
- ✅ Clear prefix-based file organization
- ✅ R6 for stateful objects (appropriate choice)
- ✅ C++ for performance-critical paths
- ✅ Ontology system (clever dimension management)
- ✅ HELPERS is properly foundational (only layer with no dependencies)

**Weaknesses:**
- ❌ No formal test framework despite 3,252+ line core classes
- ❌ Tests are manual scripts, not automated
- ❌ Two mega-files: SPECIFICATION (10,967 lines), JHEEM (7,225 lines)
- ❌ 11 circular dependency cycles
- ❌ Global state management (VERSION.MANAGER, default.data.manager.holder)

---

## Dependency Crisis

### Circular Dependencies Found: 11 Cycles

```
Cycle 1:  DATA_MANAGER → ONTOLOGY → SPECIFICATION → INTERVENTIONS → JHEEM → DATA_MANAGER

Cycle 2:  JHEEM → FILE_MANAGER → JHEEM

Cycle 3:  INTERVENTIONS → JHEEM → INTERVENTIONS

Cycle 4:  ONTOLOGY → SPECIFICATION → INTERVENTIONS → JHEEM → ONTOLOGY

Cycle 5:  SPECIFICATION → INTERVENTIONS → JHEEM → SPECIFICATION

Cycle 6:  CALIBRATION → DATA_MANAGER → ONTOLOGY → SPECIFICATION → INTERVENTIONS
          → JHEEM → VERSIONS → CALIBRATION

Cycle 7:  INTERVENTIONS → JHEEM → VERSIONS → INTERVENTIONS

Cycle 8:  ONTOLOGY → SPECIFICATION → INTERVENTIONS → JHEEM → VERSIONS → ONTOLOGY

Cycle 9:  ONTOLOGY → SPECIFICATION → INTERVENTIONS → ONTOLOGY

Cycle 10: SPECIFICATION → INTERVENTIONS → SPECIFICATION

Cycle 11: ONTOLOGY → SPECIFICATION → ONTOLOGY
```

### Critical Cycles Explained

#### 1. **SPECIFICATION ↔ INTERVENTIONS**
- **Problem:** SPECIFICATION needs intervention types; INTERVENTIONS needs specification structure
- **Impact:** Can't define models without intervention concepts, can't implement interventions without model structure

#### 2. **JHEEM ↔ INTERVENTIONS**
- **Problem:** JHEEM applies interventions during simulation; INTERVENTIONS reference JHEEM.ENGINE class
- **Impact:** Engine and domain logic are tightly coupled

#### 3. **JHEEM ↔ FILE_MANAGER**
- **Problem:** Both reference each other's classes
- **Impact:** I/O logic mixed with execution logic

#### 4. **ONTOLOGY ↔ SPECIFICATION**
- **Problem:** SPECIFICATION uses ontologies; ONTOLOGY validates against specifications
- **Impact:** Core data structure depends on model definition

#### 5. **The "Big Loop"**
```
CALIBRATION → DATA_MANAGER → ONTOLOGY → SPECIFICATION
→ INTERVENTIONS → JHEEM → VERSIONS → CALIBRATION
```
- **Problem:** Nearly all major components form one giant dependency cycle
- **Impact:** Impossible to test components in isolation or extract sub-packages

### Why Cycles Matter

When everything depends on everything, you can't:
- **Test components in isolation** - Need to load entire package for unit tests
- **Refactor safely** - Changes ripple unpredictably
- **Split into sub-packages** - No clean boundaries
- **Reason about the system** - Mental model requires understanding entire codebase
- **Onboard new developers** - No clear entry point
- **Parallelize development** - Teams step on each other

### Dependency Layer Analysis

**Attempted Layering:**
```
Layer 1 (Foundation): HELPERS only

WARNING: Circular dependencies prevent clean layering
Remaining nodes: CALIBRATION, DATA_MANAGER, FILE_MANAGER, INTERVENTIONS,
                 JHEEM, LIKELIHOODS, ONTOLOGY, PLOTS, SPECIFICATION, VERSIONS
```

Only HELPERS can be cleanly layered - everything else is tangled together.

---

## Proposed Architecture

### Conceptual Layered Architecture

This is the **natural architecture** hiding inside your codebase:

```
┌─────────────────────────────────────────────────────────────┐
│                    Layer 4: APPLICATIONS                     │
│  ┌─────────────────┐  ┌──────────────┐  ┌────────────────┐ │
│  │   CALIBRATION   │  │    PLOTS     │  │  FILE_MANAGER  │ │
│  │   (2931 lines)  │  │ (2109 lines) │  │   (576 lines)  │ │
│  └─────────────────┘  └──────────────┘  └────────────────┘ │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                  Layer 3: DOMAIN MODELS                      │
│  ┌──────────────────┐  ┌──────────────────────────────────┐ │
│  │   LIKELIHOODS    │  │       INTERVENTIONS              │ │
│  │   (6862 lines)   │  │       (4460 lines)               │ │
│  └──────────────────┘  └──────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                  Layer 2: SIMULATION ENGINE                  │
│  ┌──────────────────────────────────────────────────────┐   │
│  │                   JHEEM ENGINE                       │   │
│  │              (13,847 lines - SPLIT ME!)             │   │
│  └──────────────────────────────────────────────────────┘   │
│                                                              │
│  ┌──────────────────┐  ┌──────────────────────────────────┐ │
│  │  DATA_MANAGER    │  │      SPECIFICATION               │ │
│  │  (3252 lines)    │  │      (19,906 lines - SPLIT!)     │ │
│  └──────────────────┘  └──────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                  Layer 1: FOUNDATIONS                        │
│  ┌──────────────────┐  ┌──────────────────────────────────┐ │
│  │    ONTOLOGY      │  │          HELPERS                 │ │
│  │   (4455 lines)   │  │        (3489 lines)              │ │
│  └──────────────────┘  └──────────────────────────────────┘ │
│                                                              │
│  ┌────────────────────────────────────────────────────────┐ │
│  │              VERSIONS (788 lines)                      │ │
│  └────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────┘
```

### Design Principles

1. **Dependency Direction:** Lower layers never depend on higher layers
2. **Interface Segregation:** Components depend on abstractions, not concretions
3. **Single Responsibility:** Each layer has one reason to change
4. **Open/Closed:** Extend through composition, not modification

---

## Sub-Package Structure

### Package 1: `jheem.core` (Foundation)

**Purpose:** Core data structures and utilities used by everything else

**Size:** 8,732 lines

**Contents:**
- `ONTOLOGY` (4,455 lines)
- `HELPERS` (3,489 lines)
- `VERSIONS` (788 lines)

**Why Separate:**
These are pure utilities with no domain logic. They should be usable standalone for data manipulation, dimension mapping, and version management without requiring the full HIV modeling context.

**API Surface:**
```r
# Ontology operations
ont <- ontology(age = c("13-24", "25-34"), risk = c("MSM", "IDU"))
mapping <- create.ontology.mapping(from.ont, to.ont)
mapped_data <- apply.ontology.mapping(data, mapping)

# Helper utilities
expanded <- expand.with.anchor.year(arr, years, anchor)
smoothed <- restratify.age.counts(counts, method = 'pclm')
indexed <- get.dim.index(arr, age = "25-34", risk = "MSM")

# Version registry
register.version(spec, version = "2.0")
spec <- get.version("2.0")
versions <- list.available.versions()
```

**Dependencies:**
- Base R
- Rcpp (for performance)
- Minimal external packages (reshape2 for ontology operations)

**Exports:**
- Ontology creation and manipulation functions
- Array/dimension helpers
- Age/year utilities
- Version registry

---

### Package 2: `jheem.specification` (Model Definition)

**Purpose:** Define what a model *is* - compartments, parameters, dimensions

**Size:** 19,906 lines

**Contents:**
- `SPECIFICATION` (currently monolithic 19,906 lines)

**Proposed Split:**
```
specification/
  core.R                    (~3,000 lines) - R6 class skeleton + public API
  compartments.R            (~3,000 lines) - Compartment management logic
  parameters.R              (~4,000 lines) - Parameter registry & operations
  quantities.R              (~4,000 lines) - Quantity definitions & calculations
  functional_forms.R        (already split, 3,035 lines)
  inheritance.R             (~3,000 lines) - Parent/child version logic
  validation.R              (~2,000 lines) - Input validation & checking
  compiler.R                (already split, 3,199 lines as compiled_specification)
  evaluatable_value.R       (already split)
  links.R                   (already split)
  scales.R                  (already split)
  metadata.R                (already split)
```

**Why Separate:**
Model specification is a distinct concern from *running* models. Scientists may want to define/share model structures without the full simulation stack. Specifications can be validated, versioned, and documented independently.

**API Surface:**
```r
# Create specification
spec <- create.jheem.specification(
  version = "2.0",
  iteration = 1,
  description = "Updated transmission model",
  start.year = 2007,
  compartments.for.infected.only = list(
    risk = c("msm", "idu", "heterosexual")
  ),
  compartments.for.uninfected.only = list(
    continuum = c("unaware", "aware", "in.care")
  )
)

# Register model components
register.model.quantity(spec, "hiv.incidence", ...)
register.parameter(spec, "transmission.rate", ...)
register.model.outcome(spec, "hiv.prevalence", ...)

# Configure functional forms
set.element.functional.form.main.effect.alphas(
  spec,
  element.name = "transmission.rate",
  alpha.name = "scale",
  dimension = "risk",
  values = c(msm = 1.5, idu = 0.8)
)

# Register and retrieve
register.model.specification(spec, version = "2.0")
spec <- get.jheem.specification(version = "2.0")
```

**Dependencies:**
- `jheem.core` (for ontologies, helpers, version registry)

**Exports:**
- JHEEM.SPECIFICATION R6 class
- Specification creation and manipulation functions
- Parameter and quantity registration functions
- Functional form configuration
- Specification validation

**Breaking Change Mitigation:**
Keep original `SPECIFICATION_model_specification.R` as a facade that sources the split files during transition period.

---

### Package 3: `jheem.engine` (Simulation Execution)

**Purpose:** Run simulations, solve ODEs, compute outcomes

**Size:** 17,099 lines

**Contents:**
- `JHEEM` module files (currently 13,847 lines)
- `DATA_MANAGER` (3,252 lines)

**Proposed Split for JHEEM:**
```
engine/
  core.R                    (~3,000 lines) - JHEEM.ENGINE R6 class skeleton
  runner.R                  (~2,000 lines) - Simulation execution logic
  ode_interface.R           (~2,000 lines) - ODE solver interface (diffeq.R)
  simulation_results.R      (~2,500 lines) - SIMULATION.SET class
  kernel.R                  (~1,000 lines) - Pre-compiled specifications (JHEEM.KERNEL)
  transmutation.R           (~1,000 lines) - Parameter transformations
  outcome_mappings.R        (~1,000 lines) - Location/outcome mapping logic
  simset_collection.R       (~1,000 lines) - Collection management
  entity.R                  (already split, JHEEM.ENTITY)
  run_metadata.R            (already split)
```

**Why Separate:**
This is the computational engine. It consumes specifications and produces results. Users doing pure data analysis with existing simulation results don't need the engine. Separating allows focused optimization and testing of the ODE solver and computational components.

**API Surface:**
```r
# Create engine
engine <- create.jheem.engine(
  version = "2.0",
  location = "C.12580"  # Baltimore
)

# Run simulation
sim <- engine$run(
  parameters = params,
  years = 2007:2030,
  intervention = my.intervention
)

# Extract results
results <- get.simset.data(
  sim,
  outcomes = c("hiv.prevalence", "hiv.incidence"),
  dimensions = c("year", "age", "risk")
)

# Batch operations
sims <- run.jheem.simulations(
  version = "2.0",
  locations = c("C.12580", "C.13820"),
  parameters.list = param.samples
)

# Data management
dm <- create.data.manager()
dm$put.data(
  values = surveillance.data,
  outcome = "hiv.diagnoses",
  dimensions = list(year = 2015:2020, age = age.groups)
)

data <- dm$pull.data(
  outcome = "hiv.diagnoses",
  from.ontology = data.ont,
  to.ontology = model.ont
)
```

**Dependencies:**
- `jheem.core` (ontologies, helpers, versions)
- `jheem.specification` (JHEEM.SPECIFICATION class)
- `Rcpp` (for C++ ODE solver)
- `deSolve` (for ODE integration)

**Exports:**
- JHEEM.ENGINE R6 class
- Engine creation and execution functions
- SIMULATION.SET and result access functions
- DATA.MANAGER R6 class
- Data storage and retrieval functions
- JHEEM.KERNEL (compiled specification)

**Key Refactoring - Interface Extraction:**

To break circular dependency with INTERVENTIONS:

```r
# In jheem.engine/interfaces.R
IIntervention <- R6Class("IIntervention",
  public = list(
    apply = function(state, time, params) {
      stop("Not implemented: apply() must be overridden")
    },
    get_affected_compartments = function() {
      stop("Not implemented")
    }
  )
)

# JHEEM.ENGINE accepts any IIntervention implementation
JHEEM.ENGINE$set("public", "run", function(parameters, intervention = NULL) {
  if (!is.null(intervention)) {
    if (!inherits(intervention, "IIntervention")) {
      stop("intervention must inherit from IIntervention")
    }
  }
  # ... rest of implementation
})
```

Now INTERVENTIONS package implements the interface, but JHEEM.ENGINE only depends on the abstract interface.

---

### Package 4: `jheem.models` (Domain Models)

**Purpose:** Higher-level modeling constructs - interventions, likelihoods

**Size:** 11,322 lines

**Contents:**
- `INTERVENTIONS` (4,460 lines)
- `LIKELIHOODS` (6,862 lines)

**Why Separate:**
These are *applications* of the engine with specific domain semantics. Users doing pure simulation don't need likelihood machinery. Users doing scenario analysis without calibration don't need MCMC. Separating allows teams to work on statistical methods vs. intervention modeling independently.

**API Surface:**

#### Interventions
```r
# Define target population
target <- create.target.population(
  age = c("25-34", "35-44"),
  risk = "msm",
  continuum = "aware"
)

# Define intervention effect
effect <- testing.intervention.effect(
  increase.rate = 0.5,
  start.year = 2020
)

# Combine into intervention
intervention <- create.intervention(
  target.population = target,
  effect = effect
)

# Apply to model
sim <- engine$run(parameters = params, intervention = intervention)

# Complex interventions
multi <- create.sequential.interventions(
  prep.intervention,
  testing.intervention,
  linkage.intervention
)
```

#### Likelihoods
```r
# Basic likelihood
ll <- create.basic.likelihood(
  outcome = "hiv.diagnoses",
  data.manager = dm,
  ontology.mapping = mapping,
  years = 2010:2020
)

# Nested proportion likelihood (hierarchical)
ll_nested <- create.nested.proportion.likelihood(
  outcome = "proportion.virally.suppressed",
  denominator.outcome = "hiv.prevalence",
  data.manager = dm,
  ontology.mapping = mapping
)

# Joint likelihood
ll_joint <- create.joint.likelihood(
  ll_diagnoses,
  ll_prevalence,
  ll_suppression
)

# Compute likelihood
log.lik <- compute.likelihood(ll, simset = sim)
```

**Dependencies:**
- `jheem.core` (ontologies, helpers)
- `jheem.specification` (for intervention type definitions)
- `jheem.engine` (for IIntervention interface, DATA.MANAGER)

**Exports:**
- Intervention creation and application functions
- TARGET.POPULATION R6 class
- Various intervention effect classes
- Likelihood instruction and instantiation functions
- Various likelihood type classes (basic, nested, bernoulli, ratio, custom)

**Challenge - SPECIFICATION → INTERVENTIONS dependency:**

**Current Problem:** SPECIFICATION registers intervention types

**Solution:**
```r
# Move intervention TYPE definitions to jheem.specification
# Keep intervention IMPLEMENTATION in jheem.models

# In jheem.specification:
register.intervention.slot <- function(spec, slot.name, ...) {
  # Registers that interventions CAN be applied here
}

# In jheem.models:
# Concrete intervention implementations fill those slots
TestingIntervention <- R6Class("TestingIntervention",
  inherit = IIntervention,
  public = list(
    apply = function(state, time, params) {
      # Implementation
    }
  )
)
```

---

### Package 5: `jheem.analysis` (Applications)

**Purpose:** High-level workflows - calibration, plotting, I/O

**Size:** 5,616 lines

**Contents:**
- `CALIBRATION` (2,931 lines)
- `PLOTS` (2,109 lines)
- `FILE_MANAGER` (576 lines)

**Why Separate:**
These are end-user tools for specific workflows. Power users may want to build custom calibration strategies or visualization approaches without these dependencies. Plotting and I/O are orthogonal concerns to simulation.

**API Surface:**

#### Calibration
```r
# Set up calibration
calibration <- setup.calibration(
  specification.version = "2.0",
  location = "C.12580",
  likelihood = joint.likelihood,
  prior = prior.distributions,
  target.parameter.names = c("transmission.rate", "testing.rate")
)

# Run MCMC
results <- run.calibration(
  calibration,
  n.iter = 10000,
  n.chains = 4,
  thin = 10,
  save.frequency = 100
)

# Analyze results
summary <- get.calibration.summary(results)
trace.plots <- plot.calibration.traces(results)
posterior <- get.posterior.samples(results, n = 1000)
```

#### Plotting
```r
# Quick simulation plot
simplot(
  simset,
  outcomes = "hiv.prevalence",
  facet.by = "risk",
  split.by = "age"
)

# Multi-simulation comparison
plot.simulations(
  list(baseline = sim1, intervention = sim2),
  outcomes = c("hiv.prevalence", "hiv.incidence"),
  years = 2007:2030
)

# Calibration diagnostics
plot.calibration.fit(
  simset = calibrated.sim,
  data.manager = dm,
  outcomes = c("hiv.diagnoses", "hiv.prevalence")
)
```

#### File I/O
```r
# Save/load simulations
save.simset(sim, "results/baseline.rds")
sim <- load.simset("results/baseline.rds")

# Save/load calibrations
save.calibration(calibration, "calibrations/baltimore_v2.rds")
cal <- load.calibration("calibrations/baltimore_v2.rds")

# Batch operations
save.simset.collection(sims, "results/sensitivity_analysis/")
```

**Dependencies:**
- ALL other packages (top of stack)
- `ggplot2` (plotting)
- `plotly` (interactive plots)

**Exports:**
- Calibration setup and execution functions
- Posterior analysis functions
- Plotting functions (simplot, plot.simulations)
- File I/O functions
- Style management for plots

---

### Target Dependency Graph

```
                  jheem.analysis (5,616 lines)
                        │
         ┌──────────────┴──────────────┐
         │                              │
   jheem.models                   (CALIBRATION, PLOTS, I/O)
   (11,322 lines)                       │
         │                              │
         │       ┌──────────────────────┘
         └───────┤
                 │
           jheem.engine
          (17,099 lines)
                 │
                 │
        jheem.specification
          (19,906 lines)
                 │
                 │
            jheem.core
           (8,732 lines)
                 │
                 └─> base R + Rcpp + deSolve + locations
```

**Key Improvements:**
- ✅ No circular dependencies
- ✅ Clear separation of concerns
- ✅ Testable layers
- ✅ Progressive complexity (simple → advanced)
- ✅ Users can install only what they need
- ✅ Teams can work on different packages independently

---

## Breaking the Cycles

### Pattern 1: Dependency Inversion (Interfaces)

**Problem:** JHEEM → INTERVENTIONS → JHEEM (Cycle 3)

**Root Cause:**
- JHEEM.ENGINE needs to apply interventions during simulation
- INTERVENTIONS need to reference JHEEM.ENGINE for context

**Solution - Extract Interface:**

```r
# In jheem.engine/interfaces.R
# Define abstract base class for interventions
IInterventionEffect <- R6Class("IInterventionEffect",
  public = list(
    apply_to_state = function(state, time, params) {
      stop("Must implement apply_to_state()")
    },

    get_affected_compartments = function() {
      stop("Must implement get_affected_compartments()")
    },

    validate = function(specification) {
      # Default implementation - can be overridden
      TRUE
    }
  )
)

# In JHEEM.ENGINE - depend on interface, not concrete classes
apply_interventions <- function(state, time, params, interventions) {
  if (is.null(interventions)) return(state)

  for (int in interventions) {
    if (!inherits(int, "IInterventionEffect")) {
      stop("Interventions must implement IInterventionEffect interface")
    }
    state <- int$apply_to_state(state, time, params)
  }

  state
}

# In jheem.models/INTERVENTIONS - implement interface
TestingEffect <- R6Class("TestingEffect",
  inherit = IInterventionEffect,

  public = list(
    increase_rate = NULL,
    start_year = NULL,

    initialize = function(increase_rate, start_year) {
      self$increase_rate <- increase_rate
      self$start_year <- start_year
    },

    apply_to_state = function(state, time, params) {
      if (time < self$start_year) return(state)

      # Modify testing rates in state
      state$testing.rate <- state$testing.rate * (1 + self$increase_rate)
      state
    },

    get_affected_compartments = function() {
      c("unaware", "aware")
    }
  )
)
```

**Result:**
- JHEEM.ENGINE depends on `IInterventionEffect` (abstract)
- INTERVENTIONS depends on JHEEM.ENGINE for interface definition
- No cycle! Abstract dependency is one-way

---

### Pattern 2: Composition Over Inheritance

**Problem:** SPECIFICATION ↔ INTERVENTIONS (Cycle 10)

**Root Cause:**
- SPECIFICATION needs to know what intervention slots exist
- INTERVENTIONS need to know specification structure to validate

**Solution - Registry Pattern:**

```r
# In jheem.specification
JHEEM.SPECIFICATION$set("public", "register_intervention_slot",
  function(slot_name, affected_compartments, description) {
    # Just records that interventions CAN be applied
    private$intervention_slots[[slot_name]] <- list(
      compartments = affected_compartments,
      description = description
    )
  }
)

# SPECIFICATION doesn't need to know about intervention implementations
# It just provides "hooks" where interventions can be applied

# In jheem.models/INTERVENTIONS
# Concrete implementations reference the specification to:
# 1. Validate they're using valid slots
# 2. Get compartment structure
# But SPECIFICATION doesn't reference back to INTERVENTIONS

TestingIntervention <- R6Class("TestingIntervention",
  inherit = IIntervention,

  public = list(
    validate_against_specification = function(spec) {
      # Check if our target slot exists
      if (!"testing" %in% names(spec$intervention_slots)) {
        stop("Specification doesn't have 'testing' intervention slot")
      }
      TRUE
    }
  )
)
```

**Result:**
- SPECIFICATION provides extension points (slots)
- INTERVENTIONS consume those extension points
- Dependency is one-way: INTERVENTIONS → SPECIFICATION

---

### Pattern 3: Event-Driven / Callback Pattern

**Problem:** JHEEM ↔ FILE_MANAGER (Cycle 2)

**Root Cause:**
- FILE_MANAGER needs to know JHEEM.ENGINE structure to save/load
- JHEEM references FILE_MANAGER for I/O operations

**Solution - Separate Concerns:**

```r
# In jheem.engine - provide basic serialization
JHEEM.ENGINE$set("public", "to_list", function() {
  list(
    version = private$version,
    location = private$location,
    specification = private$specification,
    # ... all state
  )
})

JHEEM.ENGINE$set("public", "from_list", function(data) {
  private$version <- data$version
  private$location <- data$location
  # ... restore state
})

# Generic save/load (no FILE_MANAGER dependency)
save.jheem.engine <- function(engine, path) {
  saveRDS(engine$to_list(), path)
}

load.jheem.engine <- function(path, version) {
  data <- readRDS(path)
  engine <- create.jheem.engine(version, data$location)
  engine$from_list(data)
  engine
}

# In jheem.analysis/FILE_MANAGER - enhanced version with metadata
save.simset.with.metadata <- function(simset, path, metadata = NULL) {
  obj <- list(
    simset = simset,
    metadata = metadata,
    saved_at = Sys.time(),
    jheem_version = packageVersion("jheem2")
  )
  saveRDS(obj, path)
}
```

**Result:**
- ENGINE provides serialization interface
- FILE_MANAGER provides enhanced I/O on top of basic interface
- Dependency is one-way: FILE_MANAGER → ENGINE

---

### Pattern 4: Facade Pattern

**Problem:** ONTOLOGY ↔ SPECIFICATION (Cycle 11)

**Root Cause:**
- SPECIFICATION uses ontologies for dimension structure
- ONTOLOGY validates dimension names against specification standards

**Solution - Remove Back-Reference:**

```r
# In jheem.core/ONTOLOGY
# Remove any specification-specific validation
# Make ontology a pure data structure

ontology <- function(..., incomplete.dimensions = NULL) {
  rv <- list(...)

  # Generic validation only
  validate_dimension_names(names(rv))
  validate_dimension_values(rv)

  class(rv) <- c("ontology", "list")
  attr(rv, "incomplete.dimensions") <- incomplete.dimensions

  rv
}

# In jheem.specification
# Specification validates ontologies in ITS context
JHEEM.SPECIFICATION$set("public", "validate_ontology",
  function(ont) {
    # Specification-specific validation
    required_dims <- private$get_required_dimensions()

    if (!all(required_dims %in% names(ont))) {
      stop("Ontology missing required dimensions")
    }

    # Check compartment values match
    for (dim in names(private$compartments)) {
      if (dim %in% names(ont)) {
        validate_compartment_values(dim, ont[[dim]])
      }
    }
  }
)
```

**Result:**
- ONTOLOGY is a pure data structure (no specification knowledge)
- SPECIFICATION validates ontologies in its own context
- Dependency is one-way: SPECIFICATION → ONTOLOGY

---

### Pattern 5: Dependency Injection

**Problem:** Global state managers create hidden dependencies

**Current Code:**
```r
# Global environment (bad)
VERSION.MANAGER <- new.env()

# Functions magically reference global
get.jheem.specification <- function(version) {
  VERSION.MANAGER[[version]]
}

# Tests are hard - can't isolate
```

**Solution:**
```r
# Create explicit registry class
VersionRegistry <- R6Class("VersionRegistry",
  private = list(
    versions = NULL
  ),

  public = list(
    initialize = function() {
      private$versions <- list()
    },

    register = function(version, spec) {
      private$versions[[version]] <- spec
    },

    get = function(version) {
      private$versions[[version]]
    }
  )
)

# Provide default instance for convenience
DEFAULT_VERSION_REGISTRY <- VersionRegistry$new()

# But allow injection
get.jheem.specification <- function(version, registry = DEFAULT_VERSION_REGISTRY) {
  registry$get(version)
}

# Now testable!
test_that("version registry works", {
  test_registry <- VersionRegistry$new()
  test_registry$register("test", test_spec)

  spec <- get.jheem.specification("test", registry = test_registry)
  expect_equal(spec$version, "test")
})
```

**Result:**
- Explicit dependencies (can see what depends on what)
- Testable (can inject mock registry)
- Still convenient (default instance for normal use)

---

## Migration Strategy

### Overview: 3-Phase Approach (6 months)

**Philosophy:**
- **Incremental, not revolutionary** - Small, safe steps with tests at each stage
- **Backward compatible** - Users' existing scripts continue working
- **Measured, not guessed** - Establish baselines and track metrics
- **Fail-fast** - Each phase has clear success criteria before proceeding

---

### Phase 1: Prepare (Month 1) - No Breaking Changes

**Goal:** Make future changes safer without breaking anything

#### Week 1-2: Add Testing Infrastructure

**Tasks:**
1. Install testthat framework
   ```r
   usethis::use_testthat()
   ```

2. Convert 3-5 critical test scripts to testthat format
   - Start with `R/tests/ONTOLOGY_mapping_tests.R` (stable, foundational)
   - Convert `R/tests/DATA_MANAGER_test.R` (core functionality)
   - Convert one ENGINE test (integration coverage)

   ```r
   # Example conversion
   # Before: R/tests/ONTOLOGY_mapping_tests.R
   source('R/ONTOLOGY_ontology_mappings.R')
   ont1 <- ontology(age = c("13-24", "25-34"))
   ont2 <- ontology(age = c("13-44"))
   mapping <- create.ontology.mapping(ont1, ont2)
   # ... manual inspection

   # After: tests/testthat/test-ontology-mapping.R
   test_that("ontology mapping aggregates age groups correctly", {
     ont1 <- ontology(age = c("13-24", "25-34"))
     ont2 <- ontology(age = c("13-44"))
     mapping <- create.ontology.mapping(ont1, ont2)

     expect_s3_class(mapping, "ontology.mapping")
     expect_equal(mapping$aggregation$age, c("13-24" = "13-44", "25-34" = "13-44"))
   })
   ```

3. Add integration smoke test (end-to-end happy path)
   ```r
   test_that("full simulation workflow completes", {
     spec <- create.jheem.specification(...)
     register.model.specification(spec, version = "test.v1")
     engine <- create.jheem.engine(version = "test.v1", location = "C.12580")
     sim <- engine$run(parameters = default.params)

     expect_s3_class(sim, "jheem.simulation.set")
     expect_true(length(sim$simulations) > 0)
   })
   ```

4. Set up CI if not present (GitHub Actions)
   ```yaml
   # .github/workflows/R-CMD-check.yaml
   on: [push, pull_request]
   jobs:
     R-CMD-check:
       runs-on: ubuntu-latest
       steps:
         - uses: actions/checkout@v2
         - uses: r-lib/actions/setup-r@v2
         - run: Rscript -e "devtools::check()"
   ```

**Success Criteria:**
- ✅ testthat runs successfully with 10+ tests
- ✅ CI passes on main branch
- ✅ All existing functionality works unchanged

#### Week 2-3: Document Current State

**Tasks:**
1. Generate current API reference
   ```r
   # Get all exported functions
   exports <- getNamespaceExports("jheem2")

   # Document signatures
   api_reference <- lapply(exports, function(fn) {
     if (is.function(get(fn))) {
       list(
         name = fn,
         formals = formals(get(fn)),
         file = find_source_file(fn)
       )
     }
   })

   saveRDS(api_reference, "docs/api_baseline_v1.9.2.rds")
   ```

2. Create architecture documentation
   - Copy this ARCHITECTURE_ANALYSIS.md
   - Create sequence diagrams for key workflows
   - Document R-C++ boundary contracts

3. Profile typical workflows (establish performance baseline)
   ```r
   # Profile calibration workflow
   Rprof("profiles/calibration_baseline.out")
   # ... run typical calibration ...
   Rprof(NULL)
   summaryRprof("profiles/calibration_baseline.out")

   # Profile simulation workflow
   # ... same for other workflows
   ```

**Success Criteria:**
- ✅ API reference documented
- ✅ Performance baseline established
- ✅ Architecture diagrams created

#### Week 3-4: Extract Interfaces

**Tasks:**
1. Create `R/interfaces.R`
   ```r
   #' Base interface for intervention effects
   #' @export
   IInterventionEffect <- R6Class("IInterventionEffect",
     public = list(
       apply_to_state = function(state, time, params) {
         stop("Not implemented: apply_to_state() must be overridden")
       }
     )
   )

   #' Base interface for likelihoods
   #' @export
   ILikelihood <- R6Class("ILikelihood",
     public = list(
       compute = function(simset) {
         stop("Not implemented")
       }
     )
   )
   ```

2. Make existing classes inherit from interfaces (no behavior change yet)
   ```r
   # In INTERVENTIONS_intervention_effects.R
   TESTING.INTERVENTION.EFFECT <- R6Class("TESTING.INTERVENTION.EFFECT",
     inherit = IInterventionEffect,  # NEW
     # ... rest unchanged
   )
   ```

3. Add deprecation warning infrastructure
   ```r
   # R/deprecation.R
   .Deprecated_jheem <- function(old, new, version = "2.0.0") {
     warning(
       sprintf("%s is deprecated and will be removed in version %s. Use %s instead.",
               old, version, new),
       call. = FALSE
     )
   }
   ```

**Success Criteria:**
- ✅ Interface classes defined
- ✅ All concrete classes inherit properly
- ✅ Tests still pass
- ✅ No API changes for users

#### Week 4: Integration Testing

**Tasks:**
1. Add comprehensive integration tests
   ```r
   # tests/testthat/test-integration-calibration.R
   test_that("calibration workflow end-to-end", {
     # Full workflow from specification to calibrated results
   })

   # tests/testthat/test-integration-interventions.R
   test_that("intervention application workflow", {
     # Full workflow with interventions
   })
   ```

2. Run full test suite and fix any issues

3. Tag release: `v1.9.3-refactor-prep`

**Success Criteria:**
- ✅ 30+ tests passing (unit + integration)
- ✅ Full workflow tests cover main use cases
- ✅ Release tagged

---

### Phase 2: Internal Refactoring (Months 2-3) - Soft Breaking Changes

**Goal:** Reduce complexity without breaking public APIs

#### Week 1-2: Split SPECIFICATION

**Tasks:**
1. Create split files (keep original as facade initially)
   ```
   R/SPECIFICATION_core.R                    # R6 skeleton
   R/SPECIFICATION_compartments.R            # Compartment logic
   R/SPECIFICATION_parameters.R              # Parameter management
   R/SPECIFICATION_quantities.R              # Quantity calculations
   R/SPECIFICATION_inheritance.R             # Version inheritance
   R/SPECIFICATION_validation.R              # Input validation
   ```

2. Move code to split files, preserving exact behavior
   ```r
   # R/SPECIFICATION_core.R
   JHEEM.SPECIFICATION <- R6Class("JHEEM.SPECIFICATION",
     public = list(
       version = NULL,
       iteration = NULL,
       # ... fields only

       # Load methods from other files
       initialize = NULL,  # Defined in same file
       # Compartment methods loaded from compartments.R
       # Parameter methods loaded from parameters.R
     )
   )

   # Source split files
   source('R/SPECIFICATION_compartments.R')  # Adds compartment methods
   source('R/SPECIFICATION_parameters.R')    # Adds parameter methods
   # ...
   ```

3. Update SPECIFICATION_model_specification.R to source all splits
   ```r
   # R/SPECIFICATION_model_specification.R becomes:
   source('R/SPECIFICATION_core.R')
   source('R/SPECIFICATION_compartments.R')
   source('R/SPECIFICATION_parameters.R')
   source('R/SPECIFICATION_quantities.R')
   source('R/SPECIFICATION_inheritance.R')
   source('R/SPECIFICATION_validation.R')
   ```

4. Run tests after EACH file split (not all at once!)

**Success Criteria:**
- ✅ All tests pass after split
- ✅ No API changes
- ✅ Each split file < 3000 lines
- ✅ No performance regression

#### Week 2-3: Split JHEEM

**Tasks:**
1. Same pattern as SPECIFICATION
   ```
   R/JHEEM_core.R               # ENGINE class skeleton
   R/JHEEM_runner.R             # Simulation execution
   R/JHEEM_ode_interface.R      # ODE solver interface
   R/JHEEM_results.R            # SIMULATION.SET
   ```

2. Keep `R/JHEEM_engine.R` as facade sourcing splits

**Success Criteria:**
- ✅ All tests pass
- ✅ Each split file < 2500 lines
- ✅ No performance regression

#### Week 3-4: Break Circular Dependencies

**Focus on breaking ONE cycle completely as proof of concept**

**Target:** SPECIFICATION ↔ INTERVENTIONS (simplest cycle)

**Tasks:**
1. Move intervention slot definitions to SPECIFICATION
   ```r
   # In SPECIFICATION
   JHEEM.SPECIFICATION$set("public", "register_intervention_slot",
     function(slot_name, affected_compartments) {
       # Just records extension point
     }
   )
   ```

2. Make INTERVENTIONS consume slots (not define them)
   ```r
   # In INTERVENTIONS
   TestingIntervention$validate <- function(spec) {
     # Check slot exists in spec
   }
   ```

3. Verify cycle is broken
   ```r
   # Run dependency analysis script
   source('analysis/check_circular_deps.R')
   # Should show 10 cycles now (down from 11)
   ```

4. Document the pattern for other cycles

**Success Criteria:**
- ✅ SPECIFICATION ↔ INTERVENTIONS cycle eliminated
- ✅ Tests pass
- ✅ Pattern documented for replication

#### Week 4: Organize for Future Split

**Tasks:**
1. Create subdirectory structure
   ```
   R/
     core/                # Files destined for jheem.core
       ONTOLOGY_*.R
       HELPERS_*.R
       VERSIONS_*.R

     specification/       # Files destined for jheem.specification
       SPECIFICATION_*.R

     engine/              # Files destined for jheem.engine
       JHEEM_*.R
       DATA_MANAGER_*.R

     models/              # Files destined for jheem.models
       INTERVENTIONS_*.R
       LIKELIHOODS_*.R

     analysis/            # Files destined for jheem.analysis
       CALIBRATION_*.R
       PLOTS_*.R
       FILE_MANAGER_*.R
   ```

2. Update COLLATE in DESCRIPTION to source in correct order
   ```
   Collate:
       'core/HELPERS_array_helpers.R'
       'core/HELPERS_dim_names_helpers.R'
       'core/ONTOLOGY_ontology.R'
       # ... etc in dependency order
   ```

3. Still one package, just organized

**Success Criteria:**
- ✅ Clean directory structure
- ✅ COLLATE order respects dependencies
- ✅ Package builds successfully

---

### Phase 3: Actual Sub-Packages (Months 4-6)

**Goal:** Create separate packages with clean boundaries

#### Month 4: Create Foundation Packages

**Week 1: jheem.core**

**Tasks:**
1. Create new package structure
   ```bash
   mkdir -p ../jheem.core
   cd ../jheem.core
   Rscript -e "usethis::create_package('jheem.core')"
   ```

2. Copy core files
   ```bash
   cp ../jheem2/R/core/ONTOLOGY_*.R R/
   cp ../jheem2/R/core/HELPERS_*.R R/
   cp ../jheem2/R/core/VERSIONS_*.R R/
   cp ../jheem2/src/ontology_mappings.cpp src/
   cp ../jheem2/src/array_helpers.cpp src/
   ```

3. Set up DESCRIPTION
   ```
   Package: jheem.core
   Version: 0.1.0
   Imports: Rcpp (>= 1.0.10), reshape2
   LinkingTo: Rcpp
   ```

4. Port relevant tests
   ```bash
   cp ../jheem2/tests/testthat/test-ontology*.R tests/testthat/
   cp ../jheem2/tests/testthat/test-helpers*.R tests/testthat/
   ```

5. Build and test
   ```r
   devtools::document()
   devtools::test()
   devtools::check()
   ```

**Success Criteria:**
- ✅ Package builds cleanly
- ✅ All tests pass
- ✅ R CMD check gives 0 errors, 0 warnings

**Week 2: jheem.specification**

**Tasks:**
1. Create package, copy specification files
2. Update DESCRIPTION
   ```
   Package: jheem.specification
   Version: 0.1.0
   Imports: jheem.core (>= 0.1.0)
   ```

3. Port tests

4. Build and test

**Success Criteria:**
- ✅ Depends only on jheem.core
- ✅ All tests pass
- ✅ R CMD check clean

**Week 3: jheem.engine**

**Tasks:**
1. Create package, copy engine and data manager files
2. Update DESCRIPTION
   ```
   Package: jheem.engine
   Version: 0.1.0
   Imports:
       jheem.core (>= 0.1.0),
       jheem.specification (>= 0.1.0),
       deSolve
   LinkingTo: Rcpp
   ```

3. Copy C++ code (diffeq.cpp, etc.)

4. Port tests

**Success Criteria:**
- ✅ Depends only on core and specification
- ✅ All tests pass
- ✅ Full simulation workflow works

**Week 4: Testing & Integration**

**Tasks:**
1. Set up CI for all three packages
2. Test building in correct order:
   ```r
   devtools::install("jheem.core")
   devtools::install("jheem.specification")
   devtools::install("jheem.engine")
   ```

3. Integration test across packages
   ```r
   # In separate test file
   library(jheem.core)
   library(jheem.specification)
   library(jheem.engine)

   ont <- ontology(age = c("13-24", "25-34"))  # from core
   spec <- create.jheem.specification(...)      # from specification
   engine <- create.jheem.engine(...)           # from engine
   ```

**Success Criteria:**
- ✅ All packages install in order
- ✅ Cross-package integration works
- ✅ CI passes for all three

#### Month 5: Create Application Packages

**Week 1-2: jheem.models**

**Tasks:**
1. Create package with INTERVENTIONS and LIKELIHOODS
2. Update DESCRIPTION
   ```
   Package: jheem.models
   Version: 0.1.0
   Imports:
       jheem.core (>= 0.1.0),
       jheem.specification (>= 0.1.0),
       jheem.engine (>= 0.1.0)
   ```

3. Port tests

**Success Criteria:**
- ✅ All model creation functions work
- ✅ Tests pass

**Week 3: jheem.analysis**

**Tasks:**
1. Create package with CALIBRATION, PLOTS, FILE_MANAGER
2. Update DESCRIPTION
   ```
   Package: jheem.analysis
   Version: 0.1.0
   Imports:
       jheem.core (>= 0.1.0),
       jheem.specification (>= 0.1.0),
       jheem.engine (>= 0.1.0),
       jheem.models (>= 0.1.0),
       ggplot2,
       plotly
   ```

3. Port tests

**Success Criteria:**
- ✅ Full calibration workflow works
- ✅ Plotting functions work

**Week 4: Create Umbrella Package**

**Tasks:**
1. Create jheem2 umbrella package
   ```
   Package: jheem2
   Title: HIV Epidemic Modeling (Complete Suite)
   Version: 2.0.0
   Depends:
       jheem.core (>= 0.1.0),
       jheem.specification (>= 0.1.0),
       jheem.engine (>= 0.1.0),
       jheem.models (>= 0.1.0),
       jheem.analysis (>= 0.1.0)
   ```

2. Add backward compatibility layer
   ```r
   # R/compat.R

   #' @importFrom jheem.specification create.jheem.specification
   #' @export
   jheem.specification::create.jheem.specification

   #' @importFrom jheem.engine create.jheem.engine
   #' @export
   jheem.engine::create.jheem.engine

   # ... re-export all major functions
   ```

3. Test that old user scripts work
   ```r
   # User's old script
   library(jheem2)  # Loads all sub-packages

   spec <- create.jheem.specification(...)  # Works via re-export
   engine <- create.jheem.engine(...)       # Works via re-export
   ```

**Success Criteria:**
- ✅ Old user scripts work unchanged
- ✅ New users can install specific sub-packages
- ✅ Full integration test suite passes

#### Month 6: Polish & Release

**Week 1-2: Documentation**

**Tasks:**
1. Write vignettes for each sub-package
   ```
   vignettes/
     jheem.core_ontology_guide.Rmd
     jheem.specification_creating_models.Rmd
     jheem.engine_running_simulations.Rmd
     jheem.models_interventions.Rmd
     jheem.analysis_calibration.Rmd
     migration_guide.Rmd
   ```

2. Create migration guide
   ```markdown
   # Migrating to jheem2 v2.0

   ## For existing users
   Simply install jheem2 v2.0 - your scripts will work unchanged

   ## For new users
   Install only what you need:
   - Data manipulation: `install.packages("jheem.core")`
   - Model definition: `install.packages("jheem.specification")`
   - Simulation: `install.packages("jheem.engine")`
   - Full suite: `install.packages("jheem2")`
   ```

3. Update pkgdown site

**Week 3: Performance Testing**

**Tasks:**
1. Run performance benchmarks against baseline
2. Profile memory usage
3. Fix any regressions

**Week 4: Release**

**Tasks:**
1. Version all packages to 0.1.0 (sub-packages) and 2.0.0 (umbrella)
2. Tag releases
3. Submit to CRAN (if applicable) or internal repository
4. Announce with migration guide

**Success Criteria:**
- ✅ All packages released
- ✅ Documentation complete
- ✅ Users can migrate smoothly

---

## Practical Challenges

### Challenge 1: "Everything uses HELPERS"

**Reality Check:** This is fine! HELPERS is a foundation library - it SHOULD be widely used.

**Solution:**
- Move HELPERS to `jheem.core`
- Consider splitting into focused modules:
  - `helpers_array.R` - Array operations and indexing
  - `helpers_dimensions.R` - Dimension name handling
  - `helpers_age_year.R` - Temporal utilities (age/year conversions)
  - `helpers_misc.R` - True miscellaneous

- Keep it dependency-free (only base R + Rcpp)
- Version conservatively (breaking changes = major version bump)

**Non-Problem:** Foundation libraries are SUPPOSED to be dependencies!

---

### Challenge 2: "ONTOLOGY is used everywhere"

**This is good!** Ontology is your **lingua franca** - the shared vocabulary for multi-dimensional data.

**Why it's good:**
- Provides consistent interface across all components
- Single source of truth for dimension handling
- Enables interoperability

**Solution:**
- Move to `jheem.core` (foundation layer)
- Make it rock-solid with comprehensive tests
  - Test all edge cases
  - Test performance with large dimensions
  - Test mapping edge cases

- Document extensively
  - How ontologies work
  - When to use complete vs. incomplete dimensions
  - Mapping semantics

- Version it conservatively
  - Semver MAJOR bumps only for breaking changes
  - Extensive deprecation periods for API changes

**Pattern to follow:** Like data.frame or tibble - foundational data structure used everywhere.

---

### Challenge 3: "SPECIFICATION is 20k lines - how do we split it?"

**Problem:** Single R6 class with 10,967 lines + supporting functions = 20k total

**Strategy:** Split by **responsibility**, not by size

**Current Monolith Contains:**
1. Data structure (R6 class fields and initialization)
2. Compartment management (registering, validating compartments)
3. Parameter management (registering, setting parameters)
4. Quantity registration (model quantities and outcomes)
5. Functional form handling (time-varying parameters)
6. Inheritance logic (parent/child version relationships)
7. Validation (input checking, consistency checks)
8. Compilation/optimization (creating compiled specifications)

**Proposed Split:**

```r
specification/
  core.R                # 3,000 lines
    - JHEEM.SPECIFICATION R6 skeleton
    - Public API functions
    - Field definitions
    - Initialize method

  compartments.R        # 3,000 lines
    - Compartment registration
    - Compartment validation
    - Compartment queries
    - Methods: register.compartments(), get.compartments(), etc.

  parameters.R          # 4,000 lines
    - Parameter registration
    - Parameter setting/getting
    - Parameter validation
    - Methods: register.parameter(), set.parameter(), etc.

  quantities.R          # 4,000 lines
    - Model quantity definitions
    - Outcome definitions
    - Calculation specifications
    - Methods: register.model.quantity(), register.model.outcome()

  functional_forms.R    # 3,035 lines (already separate!)
    - Time-varying parameter specifications
    - Functional form types (linear, spline, logistic, etc.)

  inheritance.R         # 3,000 lines
    - Parent/child version relationships
    - Component inheritance logic
    - Override handling

  validation.R          # 2,000 lines
    - Input validation helpers
    - Consistency checking
    - Error message generation

  compiler.R            # 3,199 lines (already separate as compiled_specification!)
    - Optimization for simulation
    - Pre-calculation of static values
```

**Implementation Pattern:**

```r
# R/SPECIFICATION_core.R - Define skeleton
JHEEM.SPECIFICATION <- R6Class("JHEEM.SPECIFICATION",
  public = list(
    # Fields
    version = NULL,
    compartments = NULL,
    parameters = NULL,

    # Methods added from other files
    initialize = function(...) { ... },

    # Placeholder for methods defined elsewhere
    register_compartment = NULL,
    register_parameter = NULL,
    # ... etc
  ),

  private = list(
    # Private fields and helpers
  )
)

# Source other files to add methods
source('R/SPECIFICATION_compartments.R')
source('R/SPECIFICATION_parameters.R')
# ...

# R/SPECIFICATION_compartments.R - Add compartment methods
JHEEM.SPECIFICATION$set("public", "register_compartment",
  function(dimension, values, infected_only = FALSE) {
    # Implementation
  }
)

JHEEM.SPECIFICATION$set("public", "get_compartments",
  function(dimension = NULL) {
    # Implementation
  }
)

# ... etc
```

**Migration Path:**

1. **Week 1:** Create split files, move code (keep behavior identical)
2. **Week 2:** Update `SPECIFICATION_model_specification.R` to source splits
3. **Week 3:** Run full test suite, fix any issues
4. **Week 4:** Profile performance, ensure no regression

**Benefits:**
- ✅ Each file < 3000 lines (human-graspable)
- ✅ Clear separation of concerns
- ✅ Easier to find code ("where's compartment logic?" → compartments.R)
- ✅ Multiple developers can work on different aspects
- ✅ Easier testing (can test compartment logic independently)

**Risks:**
- ⚠️ R6 method injection is less common pattern (may confuse maintainers)
- ⚠️ Source order matters (must load core before extensions)

**Mitigation:**
- Document the pattern clearly in ARCHITECTURE.md
- Use COLLATE in DESCRIPTION to enforce source order
- Add checks to ensure files are sourced correctly

---

### Challenge 4: "Our users have existing scripts"

**Problem:** Breaking changes force users to rewrite code

**Solution:** Backward compatibility layer for 12 months

#### Approach 1: Umbrella Package (Recommended)

```r
# DESCRIPTION for jheem2 v2.0
Package: jheem2
Title: HIV Epidemic Modeling (Complete Suite)
Version: 2.0.0
Depends:
    jheem.core (>= 0.1.0),
    jheem.specification (>= 0.1.0),
    jheem.engine (>= 0.1.0),
    jheem.models (>= 0.1.0),
    jheem.analysis (>= 0.1.0)

# R/jheem2.R - Re-export all major functions
#' @importFrom jheem.specification create.jheem.specification
#' @export
jheem.specification::create.jheem.specification

#' @importFrom jheem.specification register.model.specification
#' @export
jheem.specification::register.model.specification

#' @importFrom jheem.engine create.jheem.engine
#' @export
jheem.engine::create.jheem.engine

# ... re-export all user-facing functions
```

**User Experience:**

```r
# Old script (still works!)
library(jheem2)

spec <- create.jheem.specification(...)  # Works via re-export
engine <- create.jheem.engine(...)       # Works via re-export
sim <- engine$run(...)                   # Works
```

**New users can be selective:**

```r
# Modern approach - install only what you need
library(jheem.specification)
library(jheem.engine)

spec <- create.jheem.specification(...)
engine <- create.jheem.engine(...)
```

#### Approach 2: Deprecation Warnings (Optional)

```r
# In umbrella package, add gentle nudges
#' @export
create.jheem.specification <- function(...) {
  message("Note: consider using library(jheem.specification) directly")
  jheem.specification::create.jheem.specification(...)
}
```

#### Timeline for Transition

```
2025 Q2: Release v2.0
  - Umbrella package works perfectly
  - No warnings, just message in docs
  - 0% breakage for users

2025 Q4: Release v2.1
  - Add gentle messages suggesting new approach
  - Still 0% breakage

2026 Q2: Release v2.2
  - Mark umbrella re-exports as "soft deprecated"
  - Still works, but message says "will be removed in v3.0"

2026 Q4: Release v3.0
  - Umbrella package is lightweight
  - Recommends explicit sub-package installation
  - Still provides backward compat for 1 more year

2027 Q4: Release v4.0
  - Umbrella package purely for convenience
  - Users expected to have transitioned
```

**12+ month runway** - plenty of time for users to migrate at their pace.

---

### Challenge 5: "C++ Code is Tightly Coupled"

**Problem:** C++ code in `src/` references R structures directly

**Current C++ Files:**
- `diffeq.cpp` - ODE computation (called 1000s of times per simulation)
- `ontology_mappings.cpp` - Dimension transformations
- `array_helpers.cpp` - Multi-dimensional indexing
- `nested_proportion_likelihood.cpp` - Hierarchical likelihood
- `functional_forms.cpp` - Parameter evaluation
- `engine_helpers.cpp`, `engine_optimizations.cpp` - Engine performance

**Challenge:** Which sub-package does C++ code go in?

**Solution:**

```
jheem.core/
  src/
    ontology_mappings.cpp      # Core data structure operations
    array_helpers.cpp          # Core utilities

jheem.engine/
  src/
    diffeq.cpp                 # Engine-specific ODE solver
    engine_helpers.cpp
    engine_optimizations.cpp
    functional_forms.cpp       # Actually used by engine

jheem.models/
  src/
    nested_proportion_likelihood.cpp  # Model-specific computation
```

**Key Insight:** C++ code goes with the R code that calls it.

**Example - ontology_mappings.cpp:**

```cpp
// src/ontology_mappings.cpp in jheem.core
// [[Rcpp::export]]
NumericVector apply_ontology_mapping_cpp(
    NumericVector data,
    IntegerMatrix mapping_indices,
    CharacterVector aggregation_type
) {
    // Fast C++ implementation
}
```

```r
# R/ONTOLOGY_ontology_mappings.R in jheem.core
apply.ontology.mapping <- function(data, mapping) {
    # Call C++ function in same package
    .Call("apply_ontology_mapping_cpp", data, mapping$indices, mapping$type)
}
```

**Benefits:**
- ✅ Clear ownership (C++ code lives with R code that uses it)
- ✅ Each sub-package is self-contained
- ✅ Can optimize C++ for specific use case

---

### Challenge 6: "Testing Across Package Boundaries"

**Problem:** Integration tests need multiple packages

**Solution:** Multiple test levels

#### Level 1: Unit Tests (within each package)

```r
# In jheem.core/tests/testthat/test-ontology.R
test_that("ontology creation works", {
    ont <- ontology(age = c("13-24", "25-34"))
    expect_s3_class(ont, "ontology")
})

# In jheem.specification/tests/testthat/test-specification.R
test_that("specification creation works", {
    spec <- create.jheem.specification(...)
    expect_s3_class(spec, "JHEEM.SPECIFICATION")
})
```

#### Level 2: Integration Tests (within dependent packages)

```r
# In jheem.engine/tests/testthat/test-engine-integration.R
test_that("engine uses specification correctly", {
    spec <- create.jheem.specification(...)
    register.model.specification(spec, version = "test")
    engine <- create.jheem.engine(version = "test", location = "test")

    expect_s3_class(engine, "JHEEM.ENGINE")
})
```

#### Level 3: System Tests (in umbrella package)

```r
# In jheem2/tests/testthat/test-full-workflow.R
test_that("full calibration workflow", {
    # Specification
    spec <- create.jheem.specification(...)

    # Engine
    engine <- create.jheem.engine(...)

    # Models
    likelihood <- create.basic.likelihood(...)

    # Analysis
    calibration <- run.calibration(...)

    # Everything works together
    expect_true(calibration$converged)
})
```

**CI Strategy:**

```yaml
# Each sub-package has its own CI
name: jheem.core CI
on: [push]
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - uses: r-lib/actions/setup-r@v2
      - run: devtools::test()

# Umbrella package tests integration
name: jheem2 Integration CI
on: [push]
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - name: Install sub-packages
        run: |
          devtools::install("../jheem.core")
          devtools::install("../jheem.specification")
          # ... etc
      - name: Run integration tests
        run: devtools::test()
```

---

### Challenge 7: "Version Management Across Sub-Packages"

**Problem:** Sub-packages have dependencies - how do versions work?

**Solution:** Semantic Versioning + Version Ranges

#### Version Strategy

```
jheem.core 0.1.0         (Foundation - stable)
  └─ jheem.specification 0.1.0  (Depends: jheem.core >= 0.1.0)
      └─ jheem.engine 0.1.0  (Depends: jheem.specification >= 0.1.0)
          ├─ jheem.models 0.1.0  (Depends: jheem.engine >= 0.1.0)
          └─ jheem.analysis 0.1.0  (Depends: jheem.engine >= 0.1.0)

jheem2 2.0.0  (Umbrella)
  Depends: all of the above
```

#### Versioning Rules

**MAJOR version (X.0.0):** Breaking API changes
- Change function signatures
- Remove functions
- Change return types
- Change class structures

**MINOR version (0.X.0):** New features, backward compatible
- Add new functions
- Add new parameters with defaults
- Add new fields to classes
- Extend existing functionality

**PATCH version (0.0.X):** Bug fixes, no API changes
- Fix bugs
- Performance improvements
- Documentation updates

#### Example Version Evolution

```
# Initial release (Month 4)
jheem.core 0.1.0
jheem.specification 0.1.0  (Depends: jheem.core >= 0.1.0, < 1.0.0)
jheem.engine 0.1.0         (Depends: jheem.specification >= 0.1.0, < 1.0.0)

# Bug fix in core (Month 5)
jheem.core 0.1.1
# Other packages don't need to change (0.1.0 range still works)

# New feature in specification (Month 6)
jheem.specification 0.2.0  (Depends: jheem.core >= 0.1.0, < 1.0.0)
jheem.engine 0.1.1         (Depends: jheem.specification >= 0.1.0, < 1.0.0)
# Engine works with either spec 0.1.0 or 0.2.0

# Breaking change in core (Year 2)
jheem.core 1.0.0           (Breaking change!)
jheem.specification 0.3.0  (Depends: jheem.core >= 1.0.0, < 2.0.0)  # Must update
jheem.engine 0.2.0         (Depends: jheem.specification >= 0.3.0)  # Must update
```

#### DESCRIPTION Version Ranges

```r
# In jheem.specification/DESCRIPTION
Package: jheem.specification
Version: 0.1.0
Imports:
    jheem.core (>= 0.1.0)
# Meaning: works with 0.1.0, 0.1.1, 0.2.0, ..., but not 1.0.0

# Alternatively, be more restrictive:
Imports:
    jheem.core (>= 0.1.0, < 1.0.0)
# Meaning: works with 0.x.x series only
```

**Recommendation:** Use wide ranges initially, tighten as needed.

---

## Success Metrics

Track these metrics to know if refactoring is working.

### Structural Health Metrics

**Target: Clean Architecture**

| Metric | Baseline (Current) | Phase 1 Target | Phase 2 Target | Phase 3 Target |
|--------|-------------------|----------------|----------------|----------------|
| Circular dependencies | 11 | 11 (unchanged) | 3-5 | 0 |
| Max file size (lines) | 10,967 | 10,967 | <3,000 | <2,000 |
| Files >1000 lines | 18 | 18 | <10 | <5 |
| Dependency graph depth | N/A (cycles) | N/A | 3-4 layers | 5 clean layers |
| Sub-packages | 1 | 1 | 1 (organized) | 5 independent |

**How to measure:**
```r
# Circular dependencies
source('analysis/check_circular_deps.R')
count_cycles()

# File sizes
files <- list.files("R", pattern = "*.R", full.names = TRUE)
sizes <- sapply(files, function(f) length(readLines(f)))
max(sizes)
sum(sizes > 1000)

# Dependency layers
source('analysis/compute_dependency_layers.R')
```

---

### Test Coverage Metrics

**Target: Comprehensive Testing**

| Metric | Baseline | Phase 1 Target | Phase 2 Target | Phase 3 Target |
|--------|----------|----------------|----------------|----------------|
| Test framework | Manual scripts | testthat | testthat | testthat |
| Total tests | ~30 (manual) | 30 automated | 75 | 150+ |
| Unit test coverage | Unknown | 40% | 60% | 70% |
| Integration tests | 0 | 5 | 10 | 20 |
| Performance tests | 0 | 1 baseline | 3 | 5 |
| CI/CD | None | Basic | Full | Per-package |

**How to measure:**
```r
# Test coverage
covr::package_coverage()

# Test count
length(list.files("tests/testthat", pattern = "^test-"))

# CI status - check GitHub Actions dashboard
```

---

### Code Quality Metrics

**Target: Maintainable Code**

| Metric | Baseline | Phase 1 Target | Phase 2 Target | Phase 3 Target |
|--------|----------|----------------|----------------|----------------|
| lintr issues | TBD | <200 | <100 | <50 |
| Cyclomatic complexity (max) | TBD | <30 | <20 | <15 |
| Function length (mean) | TBD | <100 lines | <75 lines | <50 lines |
| Undocumented exports | TBD | 0 | 0 | 0 |
| Global variables | 4+ (managers) | 4 | 2 | 0 (DI pattern) |

**How to measure:**
```r
# Linter issues
lint_results <- lintr::lint_package()
length(lint_results)

# Cyclomatic complexity
library(cyclocomp)
files <- list.files("R", pattern = "*.R", full.names = TRUE)
complexities <- sapply(files, cyclocomp_package)
max(complexities)

# Function length
# (Custom script to parse functions and count lines)

# Undocumented exports
exports <- getNamespaceExports("jheem2")
documented <- # ... parse Rd files
length(setdiff(exports, documented))
```

---

### Developer Experience Metrics

**Target: Easy Onboarding & Development**

| Metric | Baseline | Phase 1 Target | Phase 2 Target | Phase 3 Target |
|--------|----------|----------------|----------------|----------------|
| Time to understand one module | Unknown | 6 hours | 4 hours | 2 hours |
| Time to add new feature | Unknown | 3 days | 2 days | 1 day |
| Build time (full package) | TBD | Same | -10% | N/A (sub-packages) |
| Build time (one sub-package) | N/A | N/A | N/A | <1 min |
| "Where does X go?" questions | Frequent | Frequent | Occasional | Rare |

**How to measure:**
```r
# Build time
system.time(devtools::load_all())
system.time(devtools::document())

# Developer survey
# - Ask team to rate ease of understanding (1-10)
# - Track time for common tasks
# - Count "where does this code go?" questions in PRs
```

---

### User Experience Metrics

**Target: Smooth Migration**

| Metric | Baseline | Phase 1 Target | Phase 2 Target | Phase 3 Target |
|--------|----------|----------------|----------------|----------------|
| Breaking changes | N/A | 0 | 0 | 0 (compat layer) |
| User-reported issues | Baseline | No increase | No increase | <10% increase |
| Migration questions | N/A | N/A | N/A | <20 |
| Documentation pages | TBD | +10 pages | +20 pages | +50 pages |
| Vignettes | TBD | +2 | +3 | +7 (one per package + migration) |

**How to measure:**
```r
# Breaking changes
# - Run test suite from previous version
# - Check user scripts from collaborators

# User issues
# - Track GitHub issues
# - Track email questions
# - Track Slack/internal questions

# Documentation
pkgdown::build_site()
# Count pages in docs/
```

---

### Performance Metrics

**Target: No Regression**

| Metric | Baseline | Phase 1 Target | Phase 2 Target | Phase 3 Target |
|--------|----------|----------------|----------------|----------------|
| Simulation time (1 run) | TBD | ≤ baseline | ≤ baseline +5% | ≤ baseline |
| Calibration time (100 iter) | TBD | ≤ baseline | ≤ baseline +5% | ≤ baseline |
| Memory usage (peak) | TBD | ≤ baseline | ≤ baseline +10% | ≤ baseline |
| Package load time | TBD | ≤ baseline | ≤ baseline +20% | ≤ baseline +10% |

**How to measure:**
```r
# Simulation benchmark
library(microbenchmark)

baseline <- microbenchmark(
  engine$run(parameters = test.params),
  times = 10
)

# Memory profiling
profvis::profvis({
  engine$run(parameters = test.params)
})

# Load time
system.time(library(jheem2))
```

**Note:** Some increase acceptable during refactoring (Phase 2) if it's reversed by Phase 3.

---

### Summary Dashboard

Create a simple dashboard to track progress:

```r
# refactoring_metrics.R
library(ggplot2)

metrics <- data.frame(
  metric = c("Circular Deps", "Max File Size", "Test Count", "Lintr Issues"),
  baseline = c(11, 10967, 30, 250),
  current = c(11, 10967, 30, 250),  # Update as you go
  target = c(0, 2000, 150, 50)
)

metrics$progress <- (metrics$baseline - metrics$current) /
                    (metrics$baseline - metrics$target)

ggplot(metrics, aes(x = metric, y = progress)) +
  geom_col() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "green") +
  labs(title = "Refactoring Progress",
       y = "Progress to Target (1.0 = complete)",
       x = "Metric") +
  theme_minimal()
```

Run this weekly to track progress visually.

---

## Recommended Next Steps

### Immediate Actions (This Week)

#### Day 1-2: Setup & Baseline

```r
# 1. Set up testthat
usethis::use_testthat()

# 2. Run lintr and save results
lint_results <- lintr::lint_package()
saveRDS(lint_results, "metrics/baseline_lint.rds")

# 3. Get file size baseline
files <- list.files("R", pattern = "*.R", full.names = TRUE)
sizes <- data.frame(
  file = basename(files),
  lines = sapply(files, function(f) length(readLines(f, warn = FALSE)))
)
write.csv(sizes, "metrics/baseline_file_sizes.csv")

# 4. Profile a typical workflow
Rprof("metrics/baseline_simulation_profile.out")
# ... run typical simulation workflow ...
Rprof(NULL)
summaryRprof("metrics/baseline_simulation_profile.out")
```

#### Day 3-4: Convert First Tests

```r
# Pick 3 stable, foundational test files:
# 1. R/tests/ONTOLOGY_mapping_tests.R
# 2. R/tests/DATA_MANAGER_test.R
# 3. R/tests/ARRAY_HELPERS_tests.R

# Convert to testthat format
# Move to tests/testthat/test-ontology-mapping.R
# Run: devtools::test()
# Fix any failures
```

#### Day 5: Integration Test

```r
# Create tests/testthat/test-integration-simulation.R
test_that("full simulation workflow completes", {
  # Create specification
  spec <- create.jheem.specification(
    version = "test.baseline",
    iteration = 1,
    description = "Integration test spec",
    start.year = 2007,
    compartments.for.infected.only = list(risk = c("msm"))
  )
  register.model.specification(spec, version = "test.baseline")

  # Create engine
  engine <- create.jheem.engine(
    version = "test.baseline",
    location = "C.12580"
  )

  # Run simulation
  sim <- engine$run(parameters = default.parameters)

  # Verify results
  expect_s3_class(sim, "jheem.simulation.set")
  expect_true(length(sim$simulations) > 0)

  # Extract outcomes
  results <- get.simset.data(sim, outcomes = "hiv.prevalence")
  expect_true(nrow(results) > 0)
})

devtools::test()
```

**Success Criteria:**
- ✅ testthat set up and running
- ✅ 10+ tests passing
- ✅ Baseline metrics saved
- ✅ Integration test works

---

### Week 2: Document & Extract Interfaces

#### Task 1: Generate API Reference

```r
# Get all exported functions
exports <- getNamespaceExports("jheem2")

# Document current API
api_doc <- lapply(exports, function(fn) {
  obj <- get(fn, envir = asNamespace("jheem2"))

  if (is.function(obj)) {
    list(
      name = fn,
      type = "function",
      formals = names(formals(obj)),
      source_file = find_source_file(fn)  # Custom helper
    )
  } else if (R6::is.R6Class(obj)) {
    list(
      name = fn,
      type = "R6Class",
      methods = names(obj$public_methods),
      source_file = find_source_file(fn)
    )
  }
})

saveRDS(api_doc, "docs/api_baseline_v1.9.2.rds")
write.csv(do.call(rbind, api_doc), "docs/api_baseline_v1.9.2.csv")
```

#### Task 2: Create Interface Definitions

```r
# Create R/interfaces.R

#' Base class for intervention effects
#'
#' All intervention effects must implement this interface
#' @export
IInterventionEffect <- R6::R6Class("IInterventionEffect",
  public = list(
    #' Apply intervention to model state
    #' @param state Current model state
    #' @param time Current simulation time
    #' @param params Model parameters
    #' @return Modified state
    apply_to_state = function(state, time, params) {
      stop("Not implemented: subclasses must override apply_to_state()")
    },

    #' Get compartments affected by this intervention
    #' @return Character vector of compartment names
    get_affected_compartments = function() {
      stop("Not implemented: subclasses must override get_affected_compartments()")
    },

    #' Validate intervention against specification
    #' @param specification A JHEEM.SPECIFICATION object
    #' @return TRUE if valid, error otherwise
    validate = function(specification) {
      # Default implementation
      TRUE
    }
  )
)

#' Base class for likelihood calculations
#' @export
ILikelihood <- R6::R6Class("ILikelihood",
  public = list(
    #' Compute log-likelihood
    #' @param simset A JHEEM.SIMULATION.SET object
    #' @return Numeric log-likelihood value
    compute = function(simset) {
      stop("Not implemented: subclasses must override compute()")
    }
  )
)

# Add more interfaces as needed
```

#### Task 3: Update Existing Classes

```r
# In R/INTERVENTIONS_intervention_effects.R
# Add inherit to existing classes

TESTING.INTERVENTION.EFFECT <- R6::R6Class("TESTING.INTERVENTION.EFFECT",
  inherit = IInterventionEffect,  # ADD THIS LINE

  public = list(
    # ... existing implementation ...

    # Make sure methods match interface
    apply_to_state = function(state, time, params) {
      # ... existing code ...
    },

    get_affected_compartments = function() {
      c("unaware", "aware")
    }
  )
)

# Repeat for all intervention and likelihood classes
```

#### Task 4: Document Architecture

Create `docs/ARCHITECTURE_OVERVIEW.md`:

```markdown
# jheem2 Architecture Overview

## System Components

[Insert conceptual layer diagram]

## Data Flow

[Insert sequence diagram for simulation workflow]

## Key Abstractions

### Ontology
Multi-dimensional data structure for epidemiological data...

### Specification
Defines model structure, compartments, parameters...

### Engine
Executes simulations by solving ODEs...

[... etc]
```

**Success Criteria:**
- ✅ API reference saved
- ✅ Interface classes defined
- ✅ Existing classes inherit from interfaces
- ✅ Tests still pass
- ✅ Architecture documented

---

### Week 3-4: Plan Detailed Refactoring

#### Create Detailed Work Items

```markdown
# refactoring_tasks.md

## Phase 1 Tasks (Month 1) - COMPLETED ABOVE

## Phase 2 Tasks (Month 2-3)

### Task 2.1: Split SPECIFICATION_model_specification.R
- [ ] Create R/SPECIFICATION_core.R
  - [ ] Move R6 class skeleton (lines 1-500)
  - [ ] Move initialize method
  - [ ] Test: run devtools::load_all()

- [ ] Create R/SPECIFICATION_compartments.R
  - [ ] Move register.compartment methods
  - [ ] Move get.compartments methods
  - [ ] Move validate.compartments methods
  - [ ] Test: run compartment tests

- [ ] Create R/SPECIFICATION_parameters.R
  - [ ] Move register.parameter methods
  - [ ] Move set.parameter methods
  - [ ] Move get.parameter methods
  - [ ] Test: run parameter tests

... [detailed breakdown of every step]

### Task 2.2: Split JHEEM_engine.R
... [detailed breakdown]

### Task 2.3: Break SPECIFICATION ↔ INTERVENTIONS cycle
... [detailed steps with code examples]

## Phase 3 Tasks (Month 4-6)
... [detailed breakdown of sub-package creation]
```

#### Create Timeline

```
refactoring_timeline.md

# Refactoring Timeline

## Month 1: Preparation (Weeks 1-4)
Week 1: Setup testthat, convert 5 test files
  - Days 1-2: Install testthat, convert ONTOLOGY tests
  - Days 3-4: Convert DATA_MANAGER tests
  - Day 5: Create integration test

Week 2: Document & baseline
  - Days 1-2: Generate API reference
  - Days 3-4: Profile performance
  - Day 5: Create architecture diagrams

... [detailed week-by-week breakdown]
```

#### Identify Risks

```markdown
# refactoring_risks.md

## High Risk Items

### Risk 1: Breaking R-C++ Interface
**Probability:** Medium
**Impact:** High (breaks core functionality)
**Mitigation:**
- Don't move C++ code until Phase 3
- Test C++ functions extensively before splitting
- Keep integration tests for ODE solver

### Risk 2: Performance Regression in ODE Solver
**Probability:** Low-Medium
**Impact:** High (makes simulations unusably slow)
**Mitigation:**
- Benchmark before any changes to JHEEM_engine
- Profile after each major change
- Have rollback plan ready

### Risk 3: Users Update Mid-Refactoring
**Probability:** High (if public package)
**Impact:** Medium (confused users)
**Mitigation:**
- Do refactoring on development branch
- Only merge to main when phase is complete and tested
- Use version numbers clearly (1.9.x = old, 2.0.x = refactored)

... [list all risks with mitigation plans]
```

**Success Criteria:**
- ✅ Detailed task breakdown created
- ✅ Timeline with weekly milestones
- ✅ Risk assessment completed
- ✅ Team has reviewed and signed off

---

### Checkpoint: Are We Ready to Proceed?

Before starting Phase 2 refactoring, verify:

**Tests:**
- [ ] ≥ 15 automated tests passing
- [ ] ≥ 1 integration test (full workflow)
- [ ] CI/CD running successfully

**Baselines:**
- [ ] Performance benchmarks saved
- [ ] File size metrics documented
- [ ] API reference generated
- [ ] Lintr baseline saved

**Team:**
- [ ] Architecture understood by all developers
- [ ] Timeline agreed upon
- [ ] Risks assessed and mitigation plans ready
- [ ] Backup strategy in place (can rollback)

**Documentation:**
- [ ] ARCHITECTURE_ANALYSIS.md reviewed
- [ ] Task breakdown created
- [ ] Timeline created

If all checkboxes are ✅, proceed to Phase 2.

If any are ❌, address them before continuing.

---

## Conclusion

### What We Know

**The Good:**
- jheem2 has a solid conceptual architecture with natural layer separation
- File naming conventions make intent clear
- R6 usage is appropriate for stateful simulation objects
- C++ integration is well-executed for performance
- Ontology system is elegant and powerful

**The Challenge:**
- 11 circular dependencies create a "big ball of mud" in practice
- 2 mega-files (20k and 14k lines) are difficult to navigate
- Global state management makes testing hard
- No formal test framework despite complex codebase

**The Opportunity:**
- This codebase is **highly splittable** - the modules are logically separate
- Breaking cycles requires **interfaces and inversion**, not rewrites
- 6-month incremental migration is achievable
- Can maintain backward compatibility throughout

### Core Insight

**The problem is not architecture - it's dependency management.**

The modules are well-conceived. The file organization is clear. The abstractions make sense. The issue is that these well-designed modules have grown tangled dependencies that prevent:
- Isolated testing
- Independent evolution
- Team parallelization
- Sub-package extraction

**The solution is not redesign - it's extraction and inversion.**

### Recommended Approach

**Start Small, Prove the Pattern:**

1. **Week 1-2:** Add tests to make changes safe
2. **Week 3-4:** Break ONE cycle (SPECIFICATION ↔ INTERVENTIONS) using dependency inversion
3. **Month 2:** If that works, apply pattern to remaining cycles
4. **Month 3:** Split mega-files while maintaining facades
5. **Month 4-6:** Extract sub-packages with umbrella package for backward compatibility

**Success depends on:**
- ✅ Incremental steps with tests at each stage
- ✅ Backward compatibility maintained throughout
- ✅ Performance benchmarks prevent regressions
- ✅ Clear communication with users about changes

### Final Recommendation

**Do this refactoring.**

The benefits are substantial:
- Easier onboarding for new developers
- Faster development cycles (teams can work independently)
- Better testing (can test components in isolation)
- More flexible deployment (users install only what they need)
- Cleaner architecture for future extensions

The risks are manageable:
- Incremental approach limits blast radius
- Comprehensive testing catches regressions
- Backward compatibility prevents user disruption
- 6-month timeline allows careful execution

**But do it incrementally.**

Don't try to do everything at once. Break ONE cycle first. If that succeeds, you've proven the pattern works and can systematically apply it to the rest.

---

## Appendix

### Resources

**R Package Development:**
- [R Packages Book](https://r-pkgs.org/) - Hadley Wickham & Jennifer Bryan
- [R6 Documentation](https://r6.r-lib.org/)
- [testthat Documentation](https://testthat.r-lib.org/)

**Architecture Patterns:**
- [Dependency Inversion Principle](https://en.wikipedia.org/wiki/Dependency_inversion_principle)
- [Hexagonal Architecture](https://alistair.cockburn.us/hexagonal-architecture/)
- [Strangler Fig Pattern](https://martinfowler.com/bliki/StranglerFigApplication.html)

**Related Work:**
- tidyverse package ecosystem (good example of sub-packages)
- Bioconductor package guidelines (complex domain like jheem2)

### Contact

For questions about this analysis or refactoring strategy:
- Review ARCHITECTURE_ANALYSIS.md (this document)
- Check refactoring_tasks.md for detailed work items
- See refactoring_timeline.md for schedule

---

**Document Version:** 1.0
**Last Updated:** 2025-10-23
**Status:** Proposal - Awaiting Team Review
