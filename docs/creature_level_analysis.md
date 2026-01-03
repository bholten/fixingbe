# Creature Level Formula Analysis

This document summarizes the investigation into reverse-engineering the Bio-Engineering creature level calculation formula from Star Wars: Galaxies (SWG). The analysis uses historical data collected from the Furrycat database.

## Table of Contents

1. [Background](#background)
2. [Data Overview](#data-overview)
3. [Initial Formulas](#initial-formulas)
4. [Investigation Journey](#investigation-journey)
5. [Final Formulas](#final-formulas)
6. [Skin Adjustments](#skin-adjustments)
7. [Persistent Outliers](#persistent-outliers)
8. [Conclusions](#conclusions)
9. [Future Work](#future-work)

---

## Background

Bio-Engineering in SWG allowed players to create custom creatures ("pets") by combining DNA samples. The creature's level (CL) determined who could tame it and its overall power. Understanding the CL formula was crucial for crafters aiming to create optimal pets.

Key game mechanics:
- **Armor threshold**: Creatures with fortitude >= 500 gain armor
- **HAM**: Health, Action, Mind pools (with ~60/30/10 damage distribution)
- **Resists**: Kinetic, Energy, Blast, Heat, Cold, Electricity, Acid, Stun
- **DPS attributes**: Damage (from power), Speed (from courage), To-Hit (from cleverness)
- **Skin**: Determines appearance, movement speed, aggression, and minimum CL

Historical reference: The 2003 BE Guide (see `docs/historical_guide.md`) stated that CL "mostly depends on damage, resists and the CL of the donors" but noted the algorithm was not well understood.

---

## Data Overview

**Source**: Furrycat database (archived SWG data)

**Dataset after filtering**:
- 370 total creatures (79 armored, 291 unarmored)
- 24 skin types with n >= 3 observations
- Filtered out: minimum-CL specimens (at skin floor), obvious data errors

**Key variables from templates**:
- Primary attributes: hardiness, fortitude, dexterity, endurance, intellect, cleverness, courage, dependability, fierceness, power
- Derived: kinen (kinetic+energy resists avg), nonkinen (other 6 resists avg)

---

## Initial Formulas

The initial investigation derived separate formulas for armored and unarmored creatures:

### Armored Creatures (fortitude >= 500)

```
level = -23
  + 0.01  × hardiness
  + 0.06  × fortitude
  + 0.005 × dexterity
  + 0.01  × intellect
  + 0.025 × cleverness
  + 0.015 × power
  + 0.10  × kinen
  + 0.08  × nonkinen
```

**Performance**:
- R² = 0.979
- SD = 1.8 levels
- Shapiro-Wilk p = 0.564 (residuals are normally distributed!)

### Unarmored Creatures (fortitude < 500)

```
level = 9
  + 0.01  × hardiness
  - 0.02  × fortitude
  + 0.01  × dexterity
  + 0.01  × intellect
  + 0.025 × cleverness
  + 0.015 × power
  + 0.12  × kinen
  + 0.06  × nonkinen
```

**Performance**:
- R² = 0.938
- SD = 2.3 levels
- Shapiro-Wilk p = 0.0003 (residuals NOT normal)

### Key Observations

1. **Fortitude flip**: Coefficient is positive for armored (+0.06) but negative for unarmored (-0.02)
2. **Kinen dominance**: Kinetic/Energy resists matter more than other resists
3. **DPS contribution**: cleverness (to-hit) and power (damage) both contribute positively
4. **Armor penalty**: The -23 intercept for armored creates a "cost" for having armor

---

## Investigation Journey

### Phase 1: Historical Document Review

Reviewed `docs/mount_speeds.md` and `docs/historical_guide.md`:

- **Mount speeds**: No correlation with creature level (r = -0.001, p = 0.99)
- **Minimum CLs by skin**: Confirmed skin-based floors (CL 2, 5, 10, 15, 20, 25, 30, 35, 40)
- **Historical formulas**: Confirmed damage ≈ power×0.8, speed ≈ 2.5-courage/1000, to_hit ≈ 0.19+cleverness/1500

### Phase 2: DPS Investigation

**Finding**: DPS (damage × speed × to_hit) significantly affects UNARMORED level (p < 0.0001) but NOT armored (p = 0.34).

This makes sense mechanically: unarmored creatures can't rely on damage mitigation, so their offensive capability matters more for threat assessment.

### Phase 3: Effective Health Investigation

Tested whether "effective HP" (health adjusted for armor and resists) better predicts level.

**Formula tested**:
```
damage_taken = (1 - resist/100) × armor_multiplier
effective_hp = health / damage_taken
```
Where armor_multiplier = 0.5 for armored, 1.0 for unarmored.

**Result**: Effective HP correlates 0.907 with level (vs 0.662 for raw health). However, this was largely confounded with existing variables.

### Phase 4: Weighted HAM Investigation

Tested if HAM damage distribution (60% Health, 30% Action, 10% Mind) matters.

**Finding**: The optimal weights were 25% Health, 60% Action, 15% Mind - but this was CONFOUNDED:
- Action correlates with armor/fortitude (p = 0.97 after controlling)
- Mind correlates with DPS attributes (independent effect remains, p < 0.001)

**Conclusion**: Action's apparent importance was entirely due to its correlation with armor. Mind has a genuine independent effect beyond its contribution to DPS.

### Phase 5: Special Attacks and Ranged

**Special attacks**: NOT significant (p = 0.76)
**Ranged capability**: NOT significant (p = 0.19)

These features may have been implemented for gameplay but don't affect level calculation.

### Phase 6: Non-Linear Effects

Tested various non-linear terms:

| Effect | Significance |
|--------|--------------|
| cleverness >= 200 threshold | +0.88 levels |
| cleverness >= 300 threshold | +1.59 levels |
| cleverness >= 400 threshold | +2.06 levels |
| intellect² | Significant (p < 0.001) |
| power × cleverness interaction | Marginal (p = 0.09) |
| kinen >= 0 threshold | Significant |

However, adding these terms did NOT improve residual normality and sometimes made it worse.

### Phase 7: Skin Effects Investigation

**Major finding**: Different skins have inherent level adjustments!

Model with skin fixed effects:
- R² improved from 0.936 to 0.947
- SD reduced from 2.1 to 1.9 levels

Significant skin adjustments identified (see [Skin Adjustments](#skin-adjustments) section).

### Phase 8: Outlier Analysis

36 creatures (13%) have |residual| > 3 levels even after skin adjustment.

**Patterns identified**:
1. High cleverness/power creatures are over-predicted (actual level lower than expected)
2. Some low-stat creatures are also over-predicted
3. Certain skin/stat combinations under-predicted (e.g., falumpaset)

These may represent edge cases in the game's algorithm or data quality issues.

---

## Final Formulas

### Armored Creatures

```
level = -23
  + 0.01  × hardiness
  + 0.06  × fortitude
  + 0.005 × dexterity
  + 0.01  × intellect
  + 0.025 × cleverness
  + 0.015 × power
  + 0.10  × kinen
  + 0.08  × nonkinen
```

Where:
- `kinen = (kinetic_resist + energy_resist) / 2`
- `nonkinen = (blast + heat + cold + electricity + acid + stun) / 6`

**Accuracy**: R² = 0.979, SD = 1.8 levels, normally distributed residuals

### Unarmored Creatures

```
level = 9
  + 0.01  × hardiness
  - 0.02  × fortitude
  + 0.01  × dexterity
  + 0.01  × intellect
  + 0.025 × cleverness
  + 0.015 × power
  + 0.12  × kinen
  + 0.06  × nonkinen
  + skin_adjustment
```

**Accuracy**: R² = 0.947 (with skin effects), SD = 1.9 levels

---

## Skin Adjustments

Based on regression with skin fixed effects (relative to bantha baseline):

| Skin | Adjustment | n | Notes |
|------|------------|---|-------|
| Rancor | +5.2 | 3 | Significantly higher than predicted |
| Merek | +1.9 | - | |
| Kima | +1.6 | - | |
| Falumpaset | +1.2 | - | |
| Mott | +0.6 | 5 | |
| Durni | +0.6 | - | |
| Cu pa | +0.4 | 35 | |
| Brackaset | +0.3 | 7 | |
| Choku | +0.3 | - | |
| Bantha | 0.0 | 34 | (baseline) |
| Dewback | 0.0 | - | |
| Kaadu | 0.0 | - | |
| Gnort | +0.1 | 21 | |
| Gurreck | 0.0 | 7 | |
| Bearded Jax | 0.0 | - | |
| Ikopi | 0.0 | 7 | |
| Razor Cat | -0.2 | 42 | |
| Dune Lizard | -0.2 | - | |
| Narglatch | -0.4 | 9 | |
| Gurrcat | -0.8 | 24 | |
| Huurton | -1.2 | 3 | |
| Slice Hound | -1.4 | 3 | |
| Torton | -1.5 | 3 | |
| Woolamander | -2.5 | 7 | Significantly lower than predicted |

**Interpretation**: Some skins may have hidden bonuses/penalties in the game code, or represent different "tiers" of creatures with inherent level adjustments.

---

## Persistent Outliers

36 creatures (13% of unarmored) remain as outliers with |residual| > 3 levels:

### Over-Predicted (actual level LOWER than predicted)

| Serial | Skin | Level | Predicted | Residual | Cleverness | Power |
|--------|------|-------|-----------|----------|------------|-------|
| dcnqk0gj | razor_cat | 15 | 21.1 | -6.1 | 151 | 370 |
| lvn43jrf | bantha | 8 | 13.6 | -5.6 | 21 | 28 |
| plu7hqkc | durni | 24 | 29.4 | -5.4 | 160 | 568 |
| 10pq6ihd | razor_cat | 22 | 27.2 | -5.2 | 184 | 407 |
| kslg1d70 | gurrcat | 25 | 29.2 | -4.2 | 420 | 474 |

### Under-Predicted (actual level HIGHER than predicted)

| Serial | Skin | Level | Predicted | Residual | Cleverness | Power |
|--------|------|-------|-----------|----------|------------|-------|
| pdefjush | razor_cat | 49 | 41.3 | +7.7 | 456 | 534 |
| 1lc95n55 | falumpaset | 30 | 23.8 | +6.2 | 193 | 292 |
| norqrpou | cu_pa | 10 | 5.1 | +4.9 | 11 | 130 |
| 66h9j346 | slice_hound | 19 | 14.7 | +4.3 | 55 | 358 |

**Possible explanations**:
1. Crafting randomness (final combine adds variance)
2. Hidden game mechanics not captured
3. Data collection artifacts
4. Edge cases in the game's level algorithm

---

## Conclusions

### What We Learned

1. **Armor is the primary bifurcation**: Armored and unarmored creatures use different formulas with opposite fortitude effects.

2. **Resists matter, especially kinetic/energy**: kinen coefficient is higher than nonkinen in both formulas.

3. **DPS attributes contribute**: cleverness (0.025) and power (0.015) add to level.

4. **Skin effects are real**: Up to ±5 levels depending on creature type.

5. **~2 levels of unexplained variance**: Likely crafting system randomness.

### What Doesn't Matter

- Mount speed (no correlation)
- Special attacks (p = 0.76)
- Ranged capability (p = 0.19)
- Weighted HAM beyond what's captured by armor correlation

### Model Quality

| Metric | Armored | Unarmored |
|--------|---------|-----------|
| R² | 0.979 | 0.947 |
| SD | 1.8 levels | 1.9 levels |
| Normal residuals? | Yes (p=0.56) | No (p=0.0002) |
| Outliers (>3 levels) | ~5% | ~13% |

---

## Future Work

1. **Investigate outlier patterns**: The 36 persistent outliers may reveal additional game mechanics.

2. **Cross-validate with other data sources**: If additional historical data exists, test formula generalization.

3. **Test non-linear cleverness thresholds**: The 200/300/400 thresholds showed some signal but need more data.

4. **Minimum CL by skin**: Consider adding floor constraints (e.g., rancor minimum CL 35).

5. **Template quality effects**: The historical guide mentions template quality affecting outcomes.

---

## Files Reference

Analysis scripts (in `R/` directory):
- `data.R` - Data loading and filtering
- `retest_residuals_filtered.R` - Residual analysis with filters
- `investigate_outlier_skins.R` - Skin-specific investigation
- `skin_level_effects.R` - Skin fixed effects analysis
- `nonlinear_model_refinement.R` - Non-linear term testing
- `final_model_combination.R` - Combined model analysis
- `investigate_low_stat_outliers.R` - Diminishing returns investigation

Historical documents (in `docs/` directory):
- `historical_guide.md` - 2003 BE Guide
- `mount_speeds.md` - Mount speed data

---

*Analysis conducted January 2026*
