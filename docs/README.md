# Bio-Engineering Analysis Documentation

This project reverse-engineers the creature level calculation formula from Star Wars: Galaxies Bio-Engineering system.

## Documentation

| File | Description |
|------|-------------|
| [creature_level_analysis.md](creature_level_analysis.md) | Full analysis journey and findings |
| [creature_level_formulas.md](creature_level_formulas.md) | Quick reference for derived formulas |
| [data_quality_filters.md](data_quality_filters.md) | Data exclusions and rationale |
| [historical_guide.md](historical_guide.md) | 2003 BE Guide (historical reference) |
| [mount_speeds.md](mount_speeds.md) | Mount speed data (no CL correlation found) |

## Key R Scripts

### Data Loading
- `R/data.R` - Main data loading and filtering script

### Analysis Scripts (in rough order of investigation)
| Script | Purpose |
|--------|---------|
| `R/creature_level_model.R` | Initial model fitting |
| `R/compare_armor_regimes.R` | Armored vs unarmored comparison |
| `R/derive_formulas.R` | Clean formula derivation |
| `R/test_clean_coefficients.R` | Coefficient validation |
| `R/explore_nonlinear.R` | Non-linear term investigation |
| `R/investigate_outlier_skins.R` | Skin-specific effects |
| `R/skin_level_effects.R` | Skin fixed effects model |
| `R/investigate_low_stat_outliers.R` | Outlier pattern analysis |
| `R/nonlinear_model_refinement.R` | Non-linear model testing |
| `R/final_model_combination.R` | Final combined analysis |
| `R/retest_residuals_filtered.R` | Residual normality testing |

### Exploratory/Scratch Scripts
- `R/creature_level.R` - Original exploration (large file)
- `R/creature_level_scratchpad.R` - Early experiments
- `R/damage_scratchpad.R` - Damage formula testing
- `R/health_scratchpad.R` - Health relationship testing

## Quick Start

```r
# Load filtered data
source("R/data.R")

# Data now available:
# - creatures: 370 filtered creatures
# - normalized_df: 370 creatures with template data joined
# - experiments: experiment data
# - samples: DNA sample data

# Split by armor
armored <- normalized_df %>% filter(armor == 1 & fortitude >= 500)
unarmored <- normalized_df %>% filter(armor == 0)
```

## Key Findings Summary

1. **Two separate formulas** for armored (fortitude >= 500) and unarmored creatures
2. **Fortitude effect flips**: +0.06 for armored, -0.02 for unarmored
3. **Resists matter**: kinen (kinetic+energy avg) more important than other resists
4. **Skin adjustments**: Up to ±5 levels (Rancor +5.2, Woolamander -2.5)
5. **~2 level variance**: Unexplained, likely crafting randomness
6. **R² = 0.98**: Formulas explain 98% of level variance

See [creature_level_formulas.md](creature_level_formulas.md) for the actual formulas.
