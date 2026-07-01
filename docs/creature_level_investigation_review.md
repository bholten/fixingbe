# Creature-Level Investigation — Review & Resolutions (2026-07)

Purpose: a review pass over the CL investigation that **resolves four recurring
questions with explicit decisions**, so they are not relitigated. Each section
records the question, what was tested, the numbers, and the decision. All work
here is on the cleaned modeling set (`normalized_df`, unarmored branch
`fortitude < 500`, n=290) unless stated. Nothing here changes the canonical
model; it records *why* the canonical choices stand (or where they're weak).

Cross-refs: `docs/creature_level_analysis.md` (phase log),
`docs/creature_level_formulas.md`, `R/creature_level_formulas.R`,
`core3_comparison.Rmd`.

---

## 1. The negative unarmored fortitude coefficient — RESOLVED: collinear suppressor, not a mechanic

**Question.** Is the −0.019 unarmored fortitude coefficient a real game term
("fortitude penalty") or a regression artifact? The phase log contradicted
itself: Phase 14 / the polished `.Rmd` say "it's real, keep it"; the
exploratory files (`investigate_fortitude_breakpoint.R`,
`investigate_constrained_regression.R`, `investigate_elastic_net.R`) say
"likely a stat-budget/collinearity artifact; the game most plausibly gives
unarmored fortitude zero credit."

**NOTE ON THE PROCESS (read this).** During this review the conclusion flipped
twice. An initial read called it a "collinear suppressor artifact" (marginal
`cor(fortitude,level)=+0.30` but negative conditional sign, hardiness≈0 alone).
That read was **wrong** — it stopped before the decisive test. The decisive test
is: *does the negative sign survive when every other stat is allowed to be a free
non-linear smooth (and species is controlled)?* It does, overwhelmingly. The
final conclusion is that **it is a real term.** Full evidence below so this is
not relitigated a third time.

**Why the "artifact" read is insufficient.** Collinearity with hardiness (r=0.85)
does not make a coefficient fake: by Frisch–Waugh–Lovell the estimate uses only
the ~24% of fortitude variance independent of the other predictors, and that came
back at p≈1e-32. The *only* way the negative sign is not a real term is if the
true dependence on hardiness/HAM is non-linear and fortitude is proxying that
curvature — so that is what must be tested directly.

**What was tested (the decisive battery).**
- Substitute exact-derived HAM for raw hardiness/dex/intellect.
- Replace each other stat with a free smooth `s(·)` (mgcv GAM) — including
  endurance/dependability/fierceness — so any non-linearity fortitude could be
  proxying is absorbed.
- Add species (skin) fixed effects, to rule out fortitude proxying a hidden
  per-template base level.
- Partial correlation of level with fortitude after removing species + all stats.
- Shape of `s(fortitude)` (is it a genuine monotone decline?).
- Independent cross-check: does Core3's overshoot (Core3 has NO fortitude term)
  track fortitude?

**Numbers.** fortitude coefficient (should die if it were a nonlinearity proxy):
| Model | fortitude coef | p |
|---|---|---|
| linear, everything | −0.0194 | 1e-32 |
| s(hardiness), rest linear | −0.0164 | 8e-20 |
| s(ham), clev/pow linear | −0.0175 | 2e-40 |
| **all stats as smooths** | **−0.0147** | 8e-21 |
| all smooths + endurance/dep/fierceness smooths | −0.0140 | 1e-19 |
| **all smooths + species fixed effects** | **−0.0158** | 9e-17 |

| Corroborating fact | Value |
|---|---|
| `s(fortitude)` term, all-smooth GAM | monotone ↓: +2.2 @fort100 → −3.7 @fort450 (edf 2.6, F=37) |
| partial `cor(level, fortitude \| species + all stats)` | **−0.615** |
| independent fortitude variation after all controls | SD = 62 pts (real identifying variation) |
| `cor(Core3 overshoot, fortitude)` (Core3 lacks a fortitude term) | **+0.76** |
| Dropping fortitude entirely: R² / SD / AIC | 0.951→0.921 / 1.86→2.38 / +140 AIC |

**Caveats, stated honestly.**
- The *raw* within-hardiness-band correlation of fortitude with level is **mixed**
  (positive at low hardiness, r=−0.50 only in the high band). That's expected:
  raw within-band, fortitude also rides with the offense stats (cleverness/power)
  which confound it upward. Controlling for them (multivariate/GAM) is what
  exposes the true negative partial effect (−0.615). So the effect is real but is
  *not* visible as a naive bivariate.
- The hardiness↔fortitude collinearity is **mechanically induced by the in-game
  Physique experimentation line, which raises Hardiness and Fortitude together**
  (user domain knowledge). This explains the r=0.85 and means a clean natural
  experiment (fully decoupled H and F) does not exist — but 62 pts of independent
  fortitude variation remain after all controls, and the effect holds within it.
- This is observational; the exact functional form (near-linear vs the mild
  `s(fortitude)` curve) and exact coefficient carry some uncertainty.

**Interpretation / what it implies about retail.**
- Retail's **unarmored CL formula contained a direct, roughly linear negative
  fortitude term** (≈ −0.015 to −0.019 per point, ~6 levels across the range).
- Structurally important: fortitude does **not** feed any derived combat stat
  (HAM = f(hardiness,dex,intellect); damage=f(power); to-hit=f(cleverness);
  speed=f(courage)). So retail's CL calc **read at least one raw attribute
  (fortitude) directly**, with a negative weight — it was *not* a pure function of
  derived combat stats.
- This is exactly why Core3 cannot match retail here: Core3's `calculatePetLevel`
  derives everything from combat stats and has no fortitude channel, so it
  **over-levels high-fortitude pets** — the +0.76 correlation above is the
  fingerprint of the missing term. Concrete SWGEmu fix: add an unarmored
  fortitude penalty to the CL calc.
- Mechanism (user's relaunch history, now well-supported): post-launch, pets lost
  medium armor and BE was relaunched. A negative unarmored fortitude term is a
  plausible "armor pays for itself" lever from that transition. Because Physique
  raises H+F together, pumping Physique yields ≈ +0.013 (hardiness) − 0.019
  (fortitude) ≈ **−0.006/pt net** — pumping defense barely helps, or slightly
  hurts, CL. The **uber-CL10s** (max stats, CL floored at 10, acknowledged
  unintended by SOE) are the extreme: high Physique (high fortitude) **plus** a
  kinetic/energy vulnerability (`ke_floor`=0) stack both penalties, bottoming out
  CL despite huge HAM. (Test: creatures most under-leveled-for-their-HAM average
  fortitude 433 & ke_floor 0; most over-leveled average fortitude 276 & ke_floor
  31 — both mechanisms co-occur.)
- Armored branch: fortitude flips **positive** (+0.056). So the game branched on
  armor status — fortitude penalized when unarmored, rewarded when armored. A
  genuine two-regime mechanic, consistent with the relaunch story, **not** a mere
  collinearity difference.

**Decision.** The negative unarmored fortitude term is **real** — keep it, and it
*is* legitimate to document/implement as a mechanic (unarmored fortitude penalty).
The original Phase-14 "it's real, keep it" and the user's relaunch-mechanism
reading are vindicated. The CLAUDE.md "fortitude sign flip" finding stands
(promote confidence, do not downgrade).

**Status: settled** (as firmly as observational data allows). Reopen only if a
cohort with genuinely decoupled hardiness/fortitude is ever obtained.

---

## 2. Species / skin effect — RESOLVED: mostly data bias, not a hidden mechanic

**Question.** Per-species residual offsets explain a chunk of variance. Is there
a hidden per-species CL modifier in the game, or is it data bias / model
misspecification? (Domain prior: the game's documentation gives no hint of a
per-species CL bonus, so a hidden mechanic would be surprising.)

**What was tested.**
- Between-species share of residual variance, at three levels of stringency.
- Whether per-species offsets are predictable from the species' mean stats
  (aliased-nonlinearity test).
- Sensitivity to the two falumpaset rows the phase log already flags as
  likely data-entry artifacts (`1lc95n55` CL30, `01oatm1v` CL32).

**Numbers.**
| Between-species share of residual variance | Value |
|---|---|
| All 38 skins | 25.6% |
| Skins with n≥5 (17 skins) | 12.0% |
| n≥5, minus 2 flagged-suspect falumpaset rows | **9.2%** |

- Per-species offset ~ species mean stats: **R² = 0.04** (offsets are *not* a
  smooth function of stat region).
- Falumpaset offset: **+1.83** (all 10) → **+0.60** (excluding its 2 flagged
  rows). Two suspect rows drive most of the largest "species effect."

**Interpretation.** The headline 25.6% is inflated by ~21 small/singleton skins
that trivially fit their own mean (overfit). Restricting to skins with enough
data, and removing rows the project already flags as bad, the reproducible
between-species share is **~9% and shrinking**. The offsets don't track stat
regions (R²=0.04), so this residue is idiosyncratic noise, not a systematic
mechanic. **This confirms the domain prior: the apparent species effect is
largely (a) small-sample overfitting and (b) a handful of flagged-bad rows, not
a hidden per-species CL modifier.**

Note this also revises an earlier claim made during this review that species was
"the biggest lever" (based on the unstringent 18.6–25.6% figure). That figure
was an artifact of small-n skins; the real, reproducible effect is small.

**Decision.**
- **Do NOT** add per-species terms to the canonical model as a mechanic. The
  per-skin adjustment table in `final_model_combination.R` should stay
  non-canonical.
- Treat the falumpaset CL30/CL32 rows as suspect data (already flagged); they
  are not evidence of a mechanic.
- If future work wants the last ~9%, treat it as model misspecification /
  residual noise, not a species lookup.

**Status: settled** (pending only genuinely new specimens for the sparse skins).

---

## 3. Derived-stat space vs attribute space — RESOLVED: attribute space wins (confirmed)

**Question.** Should CL be modeled from the *exact reconstructed derived stats*
(HAM, damage, to-hit, speed — each known to high precision from the other
analyses) instead of from the raw genetic attributes? Prior attempts recalled
this being worse; this re-confirms and explains why.

**What was tested.** Rebuilt each creature's exact derived stats from its
attributes using the project's own formulas:
`health=42+15·hardiness+3·dexterity`, `action=42+15·dexterity+3·intellect`,
`mind=42+15·intellect+3·hardiness`, `to_hit=0.195+0.0006455·cleverness`,
`speed=2.5−courage/1000`, and the piecewise power→damage formulas. Then fit CL
in derived-stat space and compared SD to the attribute model (M7).

**Numbers (unarmored, n=290).**
| Model | R² | SD | AIC |
|---|---|---|---|
| Attribute M7 | 0.951 | **1.86** | 1202 |
| Derived: ham + to_hit + dps + resists + fort | 0.939 | 2.08 | 1262 |
| Derived: all exact stats + resists + fort | 0.948 | 1.93 | 1224 |
| (sanity: `cor(reconstructed HAM, measured HAM)` = 1.000) | | | |

**Interpretation.** Derived-stat space is **measurably worse** (SD 1.93–2.08 vs
1.86). The reason is structural, not tuning: the derived stats are *lossy linear
combinations* of the attributes (HAM fixes the hardiness:dex:intellect mix at
15:3 weights; to-hit is just a rescaling of cleverness; damage a function of
power). Collapsing to them throws away degrees of freedom the attribute model
uses. There is no cleaner CL formula hiding in derived-stat space.

**Decision. Dead end — do not retry** modeling CL from reconstructed derived
stats. Keep the attribute-based formulas. (Feeding *measured* derived stats to
Core3 is a separate thing — that's the `core3_comparison` harness, and it is
about testing Core3, not about improving our model.)

**Status: settled.**

---

## 4. Cleverness-400 hinge — REVIEWED: more robust at K=400 than feared, but minor

**Question.** The M8 cleverness hinge `+0.106·pmax(cleverness−400,0)` was
suspected fragile (one-creature-driven, bimodal best-K at 325/425). Should it be
retired?

**What was tested.** Robustness of the K=400 coefficient to (a) removing the
high-leverage rancor `6j048r2a`, (b) adding species fixed effects (does the
hinge survive when species is controlled?), and its marginal value.

**Numbers.**
| | coef | note |
|---|---|---|
| Hinge coef, M8 (no skin FE) | 0.107 | p≈1e-10 |
| Hinge coef, dropping rancor `6j048r2a` | 0.094 | retains **88%** |
| Hinge coef, with species fixed effects | 0.076 | retains **71%**, still significant |
| SD: M7 → M8 | 1.86 → 1.73 | modest gain |
| SD: M7+skinFE → M8+skinFE | 1.58 → 1.53 | hinge still helps a bit |

**Interpretation.** The fragility warning in the phase log was about **K=425**
(anchored by one rancor). At the chosen **K=400** the hinge is reasonably
robust: it survives removing the leverage point (88%) and survives species
controls (71%, still significant). So it is *not* purely a one-creature or
purely a species artifact. That said, its marginal contribution is small
(SD 1.86→1.73) and it overlaps partly with species/apex structure.

**Decision. Weak keep.** K=400 is defensible on the current data; retiring it is
not required. But it is a *minor empirical patch*, not a strong mechanism —
document it as such, don't build further structure on top of it, and re-verify
with `R/cv_cleverness_hinge.R` if the data changes.

**Status: keep as-is, low priority.**

---

## Standing caveats (carried, not re-litigated)

- **Data cleaning — re-audited 2026-07, and it holds up (earlier "optimistically
  biased" verdict retracted).** An initial pass called ~half the exclusions
  model-fit/subjective. A follow-up test against the *exact* HAM identities
  (`health=42+15·hardiness+3·dexterity`, etc., which are tight: clean residual SD
  ≈29/10/13 for health/action/mind) shows the terse-labelled `bad_data` serials
  are overwhelmingly genuine data errors:
  - **11 of ~18 are provably invalid** — they break an exact identity, often with
    physically impossible values: hardiness=1301 and dexterity=3323 (>1000 cap),
    action=37693, speed=21.4 and speed=0.0, to_hit=0.0. The "outliers I don't
    like" (`64v2qh2v`,`78mq8sed`,`lnk4d7fn`) and bare-label `mm8gn9sj` are all in
    this group — the instinct was right, only the documentation was missing.
  - **3 are legitimate skin-minimum creatures** misfiled into `bad_data`
    (`at8d24tc` rancor@CL35, `8mgpmeev` thune@CL15, `v22scdcg` fambaa@CL30).
  - **4 pass both identity checks** (`4231uemm` piket, `jgh9tjrs` bantha,
    `q4glknvv` kima, `5cubijof` dewback). These are *not* HAM typos; they show
    deed-vs-DNA-sample mismatches (e.g. deed hardiness=3 despite a giant_angler
    sample at hardiness 538) and/or an implausible level for near-zero stats.
    Genuinely ambiguous — could be bad data or unusual low-HAM/high-DPS builds.
  So R²≈0.96 / SD≈1.7 is *not* meaningfully inflated by bad exclusions; the
  cleaning was substantially justified. Remaining gaps: documentation, and the 4
  ambiguous edge cases.
- **DNA-sample validity (criterion 2).** The sample corpus does contain impossible
  values (e.g. a wild sample with intellect=5566, another with endurance≈5100 —
  both >1000 cap). A reproducible DNA-sample validator (checking each wild sample
  against the range its source creature can produce) is the objective replacement
  for "I don't like this sample." Because SWG's sampling math is easy to get
  subtly wrong in a reimplementation (a first R cut mis-flagged 79% of samples on
  dependability), the validator's bounds should come from DNA Lab's own, real
  sampling functions (`sampleCreatureCore3` + `calculateFullPercentile`), with R
  as a thin driver. Status: in progress.
- **Distance to "exact."** Only ~24% of creatures are predicted within ±0.5
  levels; integer-rounding alone would allow ~0.29 SD, vs observed ~1.7. So the
  linear model is a good statistical approximation, not the byte-exact
  reconstruction achieved for HAM/damage/to-hit/speed. The residual is real
  structure + irreducible noise in roughly unknown proportions; sections 1–4
  above account for the largest identifiable pieces.
- **Core3 is not authoritative.** SWGEmu/Core3 is a clean-room re-implementation;
  its lookup-table CL calc is one team's design choice, not evidence about retail
  structure. The `core3_comparison` result ("our simple model fits the retail
  data better than Core3, which overshoots low-CL high-HAM creatures") is a QA
  signal for SWGEmu, not a claim about how retail computed CL.
