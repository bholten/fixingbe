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

### Phase 9: The "Uber CL10" Cluster and Vulnerability-Aware Kinen

**Cluster identified**: ~25 unarmored CL10 creatures with health ~10k, kinetic
resist clamped near 60, and energy resist heavily negative (often -40 to -60).
The baseline formula over-predicts them by 1-3 levels because `kinen =
(kinetic + energy)/2` averages a near-cap positive against a heavily-negative
number, then credits the small positive remainder linearly.

**Identification problem**: Free-coefficient models (separate kinetic, energy)
appear to show kinetic contributes ~0 to level. This is an artifact, not a
mechanism — kinetic in the dataset is heavily concentrated near the in-game
60% cap (24% of unarmored creatures have kinetic >= 50), so it carries almost
no statistical leverage. Bio-Engineers strongly prefer kinetic resist, so
"well-made" creatures cluster at the cap. The asymmetric pos/neg model
(R² = 0.956) fits better but its k ≠ e weighting is unidentified — game
mechanics should treat kinetic and energy symmetrically since both share the
same 60% cap.

**Symmetric candidate (M6 — superseded by M7)**: Replace `kinen` with
`pmax(kinen, 0)`. The average of kinetic and energy still drives level, but
contribution is floored at zero. Improves R² 0.938 → 0.949, but on the
uber-CL10 cluster itself the spread *increases* (SD 1.10 → 1.36) — M6
captures the cluster's mean but not its tightness, because creatures with
kinetic=60 and energy=-50 still see kinen ≈ 5 of credit they shouldn't get.

**Canonical replacement (M7)**: Use the **weaker resist** of kinetic and
energy, floored at zero: `pmax(pmin(kinetic, energy), 0)`. Mechanically:
"vulnerability on either side erases all kin/eng resist credit." Symmetric
in k and e — both are 60%-capped in-engine, so the game treats them as the
same resist class.

```
level = 7.87
  + 0.013  × hardiness
  - 0.019  × fortitude
  + 0.004  × dexterity
  + 0.010  × intellect
  + 0.025  × cleverness
  + 0.015  × power
  + 0.167  × pmax(pmin(kinetic, energy), 0)
  + 0.054  × nonkinen
```

**Performance** (unarmored, n = 291):

| Model | R² | Resid SD | AIC | Uber-CL10 SD |
|---|---|---|---|---|
| M0 baseline `kinen` | 0.938 | 2.11 | 1279.2 | 1.10 |
| M5 `kinen_pos + kinen_neg` | 0.950 | 1.90 | 1221.3 | 1.37 |
| M6 `pmax(kinen, 0)` | 0.949 | 1.91 | 1221.0 | 1.36 |
| **M7 `pmax(pmin(k,e), 0)`** | **0.952** | **1.86** | **1205.7** | **0.70** |

LR tests confirm: M5 → M6 simpler form is preferred (p=0.21); M6 → M7
is a real win (AIC drops 15 points). M8 (M7 + `pmax(kinen,0)` together)
fits marginally better (R² 0.955) but the two terms partially overlap and
the extra parameter likely captures noise rather than a second mechanism.

**On the documented persistent-outlier list** (mean |residual|): baseline
5.42 → M6 4.85 → M7 **3.97**.

**On the uber-CL10 cluster** (n=25): SD drops from baseline 1.10 to M7
**0.70** — the same fit quality the asymmetric (k ≠ e) M3 model achieved,
but with full k=e symmetry intact.

**Mechanism check**: 54% of unarmored creatures have `pmin(kinetic, energy)
> 0`, vs 75% with `pmax(kinen, 0) > 0`. M7 says nearly half of all
unarmored creatures get *zero* kin/eng resist credit because they have a
vulnerability on one side. This matches the BE crafting reality where
positive energy is hard to keep alongside cap-60 kinetic.

**Residuals are still non-normal** (Shapiro p ≈ 3e-6 under M7), so there is
additional structure left — see Phase 10 below.

### Phase 11: Re-verifying the fort=500 breakpoint

The original two-formula structure (unarmored vs armored, split at fortitude
= 500) was re-tested with M7 in place. Three questions: is the breakpoint
real, is the sign flip real, and is the *negative* fortitude coefficient
under fort < 500 a literal game term or a statistical artifact?

**Result 1: the breakpoint is real and overwhelming.**

| Model | R² | Resid SD | AIC |
|---|---|---|---|
| Single global linear (no break) | 0.927 | 3.85 | 2067.0 |
| GAM smooth fortitude | 0.978 | 2.05 | 1614.2 |
| Segmented regression (data-driven break at fort=445) | 0.977 | 2.17 | 1644.6 |
| **Two formulas, hard split at fort=500** | **0.982** | **1.92** | **1567.3** |

Split-at-500 beats single-formula by ~500 AIC points and beats a flexible
GAM smooth by 47 — meaning the relationship has a true *discontinuity* at
500, not just a slope change. The GAM-implied shape, holding all other
stats at mean, shows level *decreasing* steadily from fort=0 to fort=450
(slope ≈ -0.02/pt), then *jumping +4.5 levels* to fort=500, then rising
again (slope ≈ +0.08/pt). Classic step function in game code.

**Result 2: the sign flip is iron-clad.**

- Unarmored fortitude coef: -0.019 (p ≈ 1e-32)
- Armored   fortitude coef: +0.059 (p ≈ 1e-10)

Both coefficients are extremely significant; the flip is not a small-sample
fluke.

**Result 3: the negative coefficient is most likely a stat-budget /
collinearity artifact, not a literal game-code term.**

Within the unarmored subset, fortitude correlates 0.847 with hardiness and
0.844 with health — BE crafters wire these together. Dropping fortitude
from the unarmored regression:

- R² goes 0.951 → 0.921 (real degradation)
- **Hardiness coefficient flips from +0.013 to ~0**

This is the smoking gun: the regression had been crediting hardiness
positively and fortitude negatively as a *paired* signal. What the model
actually identifies is the contrast between them, not their independent
contributions. VIF on fortitude in the unarmored model is 4.7 (moderate
collinearity); intellect (9.7) and cleverness (7.5) are worse offenders.

**Mechanistic reading**: the game most plausibly gives unarmored fortitude
*zero* level contribution — it doesn't matter for damage mitigation without
armor — but BE crafters co-vary fortitude alongside hardiness/health when
optimizing creatures, so the regression discovers an empirical
(hardiness +, fortitude -) coefficient pair that fits the data well even
though only one of them is mechanistically real.

**Practical upshot**: keep the two-formula structure with the empirical
coefficients (it predicts well, R² = 0.982 combined), but don't read literal
mechanism into the negative-fortitude term in the unarmored formula. The
"uber CL10" phenomenon's apparent fortitude-penalty effect is more
plausibly explained as: high-fortitude unarmored creatures statistically
have high HAM, the M7 resist mechanism (vulnerability erases credit), and
BE crafters' tendency to push fortitude toward 500 alongside everything
else.

> **⚠️ Updated by Phase 14.** Phases 11/12 concluded the negative
> fortitude coefficient was probably a stat-budget / collinearity
> artifact. Subsequent tests (constrained regression and elastic-net)
> rejected that hypothesis: the coefficient is signal, not bookkeeping.
> See Phase 14.

### Phase 12: Effective vs Special Resists — Ruled Out as the Driver

A second hypothesis for the negative-fortitude coefficient: in the game,
kinetic and energy resists come from two sources — **effective** (derived
directly from fortitude as `floor(fortitude / 10)`, and `floor((fortitude -
500) / 10)` for armored) and **special** (from DNA samples). When both are
present on a DNA combine, special wins and effective drops out. For
creatures whose displayed resist is "effective", the kinetic/energy column
M7 uses is *literally a function of fortitude* — a potential double-count
that could create a spurious fortitude coefficient.

**The data confirms the formula** (`R/investigate_effective_resists.R`):

- 34 / 40 unarmored creatures with non-zero `kinetic.effective` have
  `kinetic.effective = floor(fortitude / 10)` exactly.
- The 6 mismatches all align with Phase 10's data-quality suspects (e.g.
  `lvn43jrf`, `j7rql94q`) — likely furrycat data-entry errors.
- No row has both effective AND special non-zero — confirming the "special
  wins" mechanic.

**But the effective-resist double-count does NOT explain the negative
fortitude coefficient.** Splitting the unarmored data by resist source:

| Subset | n | fortitude coef | p-value | R² |
|---|---|---|---|---|
| Full unarmored | 290 | -0.01942 | 1e-32 | 0.951 |
| All-special (k & e from DNA only) | 238 | **-0.02004** | 1e-27 | 0.953 |
| Has effective k or e | 52 | -0.02071 | 0.0003 | 0.911 |

The coefficient is **essentially identical** on the all-special subset,
where there is no algebraic relationship between fortitude and the
kinetic/energy values M7 consumes. This rules out the effective-resist
double-count as the cause. The stat-budget / collinearity diagnosis from
Phase 11 stands.

**Other findings**:
- Adding `k_effective` / `e_effective` flags to M7 reduces AIC by only 1.5
  points — too small to merit promotion. The `k_effective` coefficient is
  -0.81: creatures with effective kinetic average ~0.8 levels lower than
  M7 predicts, but this is mostly absorbed by other low-stat correlates.
- Zeroing out effective resists from `ke_floor` (treating them as if the
  game ignored them) makes the model **worse** by ~40 AIC. The level
  formula appears to credit effective and special resists at the same
  rate, using whichever value is displayed.

**Conclusion**: M7 uses the right resist column. No source-aware refactor
is needed. The negative fortitude coefficient is *not* an effective-resist
algebraic confound. (See Phase 14 for the resolution of the stat-budget /
collinearity hypothesis, which Phase 12 left open.)

### Phase 13: Robustness Check — Re-fit With Suspects Removed

To check whether the suspected data-quality issues from Phases 10 and 12 are
biasing the M7 coefficients, the unarmored model was refit on a "clean"
subset with 10 suspects removed (low-health-for-skin: `lvn43jrf`,
`qd5n95rf`, `j7rql94q`, `qkuvjn3m`, `pumhbd0k`, `n4fcvdo8`; effective-resist
formula mismatches: `e27uddsi`, `njm5aqfv`, `o4jedp5e`, `rfjtgs7o`).

| | Full (n=290) | Clean (n=280) |
|---|---|---|
| R² | 0.9513 | 0.9549 |
| Resid SD | 1.861 | 1.806 |
| Outliers \|resid\| > 3 | 27 | 21 |
| Shapiro-Wilk p | 2.4e-6 | 7.4e-7 |

Combined fit (armored + unarmored): R² = 0.9818 → 0.9827, SD = 1.92 → 1.88.

**The formula is robust.** Coefficient shifts are tiny (largest: intercept
+0.34, nonkinen +0.007, ke_floor −0.003). The fortitude coefficient moves
from −0.01942 to −0.02032 — essentially unchanged. Refitting on cleaner
data doesn't pull the model around.

**Most outliers persist.** Only 6 of the 27 |resid|>3 outliers are
explained by data quality; the remaining 21 are real modeling gaps —
chiefly the high-DPS under-prediction at the top end (rancor CL48,
falumpaset CL32) and the near-armor-border zone (fortitude 380-540).
Shapiro-Wilk normality actually *worsens* slightly on the clean set,
confirming that residual non-normality is structural, not data-quality.

**Decision**: keep the suspects in the dataset. The improvement from
removing them is small (~3% SD reduction) and the model is robust to
their presence. The bad_data filter in `R/data.R` is unchanged.

### Phase 10: Low-Stat Negative-Residual Cluster (Separate Puzzle)

A small set of creatures resists the M6 fix and remains heavily over-predicted:

| Serial | Skin | CL | Pred (base) | Pred (M6) | Health | Fortitude |
|---|---|---|---|---|---|---|
| lvn43jrf | bantha | 8 | 13.3 | 12.7 | 2,850 | 1 |
| qd5n95rf | bantha | 14 | 18.3 | 17.6 | 3,406 | 45 |
| j7rql94q | veermok | 14 | 18.1 | 16.9 | 2,598 | 1 |

These have low health AND fortitude near zero. The kinen-vulnerability
mechanism doesn't explain them — they appear to be a *different* mode,
possibly a HAM-floor or skin-tier effect where the formula gives full credit
for hardiness but the underlying creature isn't really "hardy." Flagged as
its own investigation.

### Phase 14: Stress-Testing the Negative-Fortitude Coefficient — Signal, Not Artifact

Phase 11 left the negative-fortitude coefficient with a tentative
"stat-budget / collinearity artifact" reading: dropping fortitude from
the regression made the hardiness coefficient flip from +0.013 to ~0,
suggesting the regression was identifying a (hardiness +, fortitude −)
*contrast* rather than two independent effects. Phase 12 ruled out one
specific artifact mechanism (the effective-resist double-count) but
the stat-budget hypothesis itself was untested.

Phase 14 tests it directly with two follow-on analyses, and **rejects
the artifact hypothesis**.

**Test 1: Constrained regression** (`R/investigate_constrained_regression.R`)

Force `fortitude >= 0` via quadratic programming (`quadprog::solve.QP`).
By KKT, when the unconstrained estimate violates the constraint, the
constrained optimum sets that coefficient to zero and re-fits the rest
— equivalent to a drop-fortitude `lm`. If the negative coefficient is
pure artifact, the constraint should cost no predictive performance.
It does not:

| | Unconstrained M7 | Constrained (fort ≥ 0) | Δ |
|---|---|---|---|
| R² | 0.9518 | 0.9215 | −0.030 |
| Resid SD | 1.859 | 2.373 | +0.514 |
| AIC | 1205.7 | 1347.7 | +142 |
| fortitude coef | −0.0194 | 0.0000 | binding |

The cost is concentrated in a U-shape across fortitude bands: the
constrained model under-predicts low-fortitude creatures by ~0.6
levels in the 0–100 band and over-predicts high-fortitude ones by
~1.3 levels in the 400–500 band. That residual signature *is* a
missing linear term in fortitude.

The uber-CL10 cluster (n=39) is the most affected: mean residual goes
from −0.35 to **−1.86** under the constraint. The negative fortitude
coefficient was specifically pulling these creatures from "predicted
~12" down to "actually CL 10."

**Test 2: Elastic-net under cross-validation**
(`R/investigate_elastic_net.R`)

Refit with regularized regression (ridge α=0, elastic-net α=0.5,
lasso α=1.0) at 10-fold CV-optimal `lambda.min`. Lasso would gladly
zero out a redundant predictor; elastic-net handles correlated
predictors gracefully via the L2 component. Both pull on the
collinear (hardiness, fortitude) pair — neither zeroes fortitude out:

| Term | OLS | Ridge.min | ElasticNet.min | Lasso.min |
|---|---|---|---|---|
| hardiness | +0.0130 | +0.0063 | +0.0123 | +0.0124 |
| **fortitude** | **−0.0194** | **−0.0118** | **−0.0186** | **−0.0186** |
| ke_floor | +0.1665 | +0.1623 | +0.1668 | +0.1668 |
| nonkinen | +0.0543 | +0.0460 | +0.0531 | +0.0531 |

Even at the more aggressive `lambda.1se`, fortitude stays at −0.012.
Walking the lasso path from λ=∞ down to λ=0, predictors enter in
order of signal strength: `intellect → cleverness → ke_floor → power →
nonkinen → dexterity → fortitude → hardiness`. **Hardiness enters
last**, after fortitude. The fragile coefficient of the collinear
pair is hardiness, not fortitude.

The (hardiness + fortitude) sum is preserved at roughly −0.005 to
−0.006 across all models — that's the genuinely identified contrast.
Regularization can shrink both endpoints proportionally, but the
direction between them is fixed by the data.

**Verdict**: signal, not artifact.

| Test | Predicted under "artifact" | Observed |
|---|---|---|
| Drop fortitude | No fit loss; another coef absorbs | Hardiness flips to ~0; **R² drops 3 pts** |
| Constrained regression | No fit loss | **R² drops 3 pts; U-shape in residuals** |
| Effective resists | Coefficient explained by fort→resist channel | Coefficient identical on all-special subset |
| Elastic-net | fortitude shrinks to zero | **fortitude stays at −0.019; hardiness enters last** |

**Two live mechanism hypotheses remain.** The data does not let us
choose between them, but for the SWGEmu re-implementation the choice
does not matter — the empirical coefficient is what retail ran.

1. **Real game-formula term**. The unarmored CL formula literally
   subtracts a fortitude term as a "balance lever" — a counterintuitive
   gameplay choice but mechanically possible. It would explain the
   BE-folklore observation that pushing fortitude toward 499 minimized
   its CL contribution.
2. **Bug or post-rebalance damper**. Pre-launch SWG allowed pets to
   equip medium armor; that was removed in a major creature-system
   rebalance. A developer could have flipped the sign on the unarmored
   fortitude term either accidentally during the rebalance or as a
   deliberate "armor pays for itself" damper to keep heavy-resist
   unarmored pets from being too tameable. The hypothesis is
   consistent with the "jump from −0.019 below 500 to +0.059 above
   500" discontinuity (Phase 11): a single-line code change.

**Practical upshot for SWGEmu**: keep the empirical coefficient. The
formula `level = ... − 0.019 * fortitude + ...` for unarmored is the
right predictive answer, and the four-test stress battery rules out
the most plausible "this is just OLS picking a bad allocation" story.

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

### Unarmored Creatures (M7 — canonical as of Phase 9)

```
level = 7.87
  + 0.013 × hardiness
  - 0.019 × fortitude
  + 0.004 × dexterity
  + 0.010 × intellect
  + 0.025 × cleverness
  + 0.015 × power
  + 0.167 × pmax(pmin(kinetic, energy), 0)
  + 0.054 × nonkinen
```

The kinetic-energy resist term is the **weaker side, floored at zero** —
vulnerability on either kinetic or energy erases all credit for kin/eng resist.
Symmetric in kinetic and energy (the game treats both as the 60%-capped resist
class), no asymmetric weighting.

**Accuracy**: R² = 0.952, SD = 1.86 levels (without skin adjustments). See
Phase 9 for derivation. The earlier `kinen`-based form is preserved in
git history; the live formula is implemented in `R/creature_level_model.R`
as `creature_level_no_armor_lm`.

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

### Settled (don't relitigate)

- **The two-formula structure** at fortitude=500 is confirmed (Phase 11) and the discontinuity is too sharp for a smooth transition (split beats GAM smooth by ~50 AIC).
- **M7 weaker-resist-floor** is the canonical unarmored resist term (Phase 9). Vulnerability on either kinetic or energy erases all kin/eng resist credit.
- **The negative unarmored fortitude coefficient is signal, not artifact** (Phase 14). Ridge / elastic-net / lasso under cross-validation all preserve it; lasso never zeroes it out. Hardiness is the more fragile coefficient of the collinear pair.
- **Most plausible mechanism** for the negative coefficient: a deliberate balance lever introduced during the post-launch creature rebalance (when pre-launch medium-armor pets were stripped), to prevent heavy-resist unarmored pets from dominating the tameable mid-CL bracket. The structure is too clean and the magnitude too "designed" to be accidental. The data alone cannot prove this over "real game-formula term," but the SWGEmu re-implementation should keep the empirical coefficient either way.

### Next-session priorities

**Priority 1: High-DPS under-prediction (apex creatures)**

A handful of high-cleverness/power creatures remain under-predicted by M7:

| Serial | Skin | Level | M7 prediction | Residual |
|---|---|---|---|---|
| `6j048r2a` | rancor | 48 | ~41 | +7.0 |
| `01oatm1v` | falumpaset | 32 | ~26 | +5.9 |
| `pdefjush` | razor_cat | 49 | ~44 | +5.4 |

These are the apex of the CL distribution, and the formula systematically misses them. Earlier scratchpad work (`R/investigate_low_stat_outliers.R`, Phase 6) suggested cleverness threshold effects at 200/300/400 with magnitudes +0.88 / +1.59 / +2.06 levels respectively, but those tests were under the old `kinen` baseline. Worth re-running under M7.

Specific next steps:
1. Add `pmax(cleverness - 200, 0)`, `pmax(cleverness - 300, 0)`, `pmax(cleverness - 400, 0)` as additional terms; test individually and jointly.
2. Test multiplicative DPS terms: `damage * speed * to_hit` (already `dps` in `data.R`), or `cleverness * power` interaction.
3. Test whether the under-prediction is *skin-specific* — if rancor and falumpaset always under-predict regardless of cleverness, it may be a skin-tier effect (cf. existing skin-adjustments table) rather than a non-linearity.

If players currently feel SWGEmu's apex CL pets are mis-leveled, this is the regime to fix.

**Priority 2: Near-armor-border zone (fortitude 380–540)**

Both formulas fit worst in this band:
- Unarmored creatures with fortitude 380–499 over-predict
- Armored creatures with fortitude 500–540 under-predict

The hard step in `custom_model` may be hiding a transition mechanic. Possibilities:
- **Partial / light-armor mechanic** — pre-launch pets had medium armor; perhaps a vestigial "light armor" formula exists for fort 500–540 that's neither the unarmored nor full-armored formula.
- **Skin-specific armor floor** — armor unlock might depend on skin × fortitude, not fortitude alone.
- **Smooth transition band** — the game might blend the two formulas over fort 450–550 rather than switching hard at 500.

Specific next steps:
1. Refit a piecewise model with three segments: fort < 450, 450 ≤ fort < 540, fort ≥ 540. Compare AIC vs the current two-formula model.
2. Look at residuals in the 380–540 band stratified by skin — does the misprediction concentrate in particular skins?
3. Check whether any creatures in `combined_data` have an `armor` flag set but fortitude < 500, or vice versa. The armor flag in `creatures.csv` should be the ground truth on which formula to use; if it disagrees with the fort=500 split, that's the answer.

### Longer-term ideas

- **Low-stat negative cluster (Phase 10)**: Investigate as a separate mechanism. Likely a HAM-floor or skin-tier effect; the M7 fix doesn't help here. Several entries (e.g. `qkuvjn3m` bantha CL7 with health 875, `pumhbd0k` cu_pa CL13 with health 2473) look like furrycat data-entry issues — worth flagging as suspect rather than chasing.
- **Cross-validate with other data sources**: If additional historical data exists, test formula generalization.
- **Minimum CL by skin**: Consider adding floor constraints (e.g., rancor minimum CL 35).
- **Template quality effects**: The historical guide mentions template quality affecting outcomes.

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
- `investigate_uber_cl10.R` - Uber-CL10 cluster and vulnerability-aware kinen (Phase 9)
- `investigate_fortitude_breakpoint.R` - Re-verified fort=500 breakpoint and sign flip (Phase 11)
- `investigate_effective_resists.R` - Verified fort→effective-resist formula; ruled out as fort-coefficient cause (Phase 12)
- `investigate_constrained_regression.R` - QP-constrained refit of M7 with fortitude ≥ 0 (Phase 14)
- `investigate_elastic_net.R` - Ridge / elastic-net / lasso under CV; lasso entry order (Phase 14)

Historical documents (in `docs/` directory):
- `historical_guide.md` - 2003 BE Guide
- `mount_speeds.md` - Mount speed data

---

*Analysis conducted January 2026*
