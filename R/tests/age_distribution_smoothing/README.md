# Age Distribution Smoothing Analysis

This folder contains the complete analysis and development of improved age distribution smoothing methods for the `restratify.age.counts()` function.

## Problem
The original cubic spline methods produced unrealistic "too steep" decline in the epidemiologically critical 65-100 age range, particularly when extrapolating from a single "55+ years" bin containing 22,555 people across 45 years.

## Analysis Progression

### 1. Original Method Comparison
- **File**: `show_current_workflow.R` → `Rplots.pdf` 
- **Methods**: Fritsch-Carlson, Hyman, Steffen cubic splines
- **Finding**: All methods showed problematic tail behavior; Hyman was smoothest overall

### 2. Demographic Constraint Test  
- **File**: `test_85plus_constraint.R` → `85plus_constraint_comparison.pdf`
- **Approach**: Split 55+ bin using national HIV prevalence (0.84% in 85+)
  - 55-84 years: 22,365 people (99.16%)
  - 85-100 years: 190 people (0.84%)
- **Finding**: 6-bin Hyman spline dramatically improved but still had artificial cliff at age 85

### 3. PCLM Solution
- **File**: `pclm_method_comparison.R` → `pclm_method_comparison.pdf` 
- **Methods**: 6-bin Hyman spline vs 5-bin PCLM vs 6-bin PCLM
- **Key Finding**: 5-bin PCLM (no constraints needed) produces excellent tail behavior

## Final Results

**Tail Behavior (mean PDF ages 80-95):**
- 6-bin Hyman spline: 83.6 
- 5-bin PCLM: 60.3 (28% improvement)
- 6-bin PCLM: 40.4 (52% improvement)

**Smoothness (lower = smoother):**
- 6-bin spline: 1155.2
- 5-bin PCLM: 902.4 (21.9% smoother)  
- 6-bin PCLM: 965.5 (16.4% smoother)

**Count Preservation**: All methods preserve total and bin-wise counts exactly (differences < 1)

## Recommendation
**5-bin PCLM** provides the best balance of realistic tail behavior and implementation simplicity, requiring no additional demographic constraints or data preprocessing.

## Integration
The PCLM approach can be integrated into `restratify.age.counts()` by adding `'pclm'` as a method option alongside existing `'monoH.FC'` and `'hyman'` methods.

## Files
- `restratify_age_counts_reprex_for_Nick.R/Rdata` - Original test case
- `show_current_workflow.R` - Original spline method comparison
- `test_85plus_constraint.R` - Demographic constraint testing
- `pclm_method_comparison.R` - **Final clean comparison script**
- `*.pdf` - Analysis outputs and visualizations