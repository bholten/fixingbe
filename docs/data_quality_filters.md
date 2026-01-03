# Data Quality Filters

This document describes the data quality filters applied in `R/data.R` before analysis.

## Explicit Bad Data Exclusions

These specific creature serials are excluded due to obvious data errors or anomalies:

### Weird/Typo Values
| Serial | Issue |
|--------|-------|
| dta2275t | Unusual hardiness value |
| r8tfng1v | Unusual hardiness value |
| er9nq4et | Unusual action/dexterity |
| 38u9raer | Unusual action/dexterity |
| 6ko7k4ql | Unusual action/dexterity |
| 6td1segp | Bad speed value |
| 6f9lmdbu | Bad speed value |
| frfksnne | Possibly bad to-hit (kept for interest) |

### Logical Inconsistencies
| Serial | Issue |
|--------|-------|
| kjvtv7d7 | Has fortitude < 500 AND light armor (impossible) |

### Skin Minimum Level Violations
These creatures are at or below their skin's minimum level and throw off CL calculations:

| Serial | Skin | Issue |
|--------|------|-------|
| 4231uemm | piket | Trash mob in piket skin (min CL 25) |
| at8d24tc | rancor | At rancor minimum (CL 35) |
| 8mgpmeev | thune | At thune minimum |
| v22scdcg | fambaa | At fambaa minimum |
| ceh4m90v | kimogila | At kimogila minimum |
| mm8gn9sj | kimogila | At kimogila minimum |

### Health/Hardiness Outliers
| Serial | Issue |
|--------|-------|
| 64v2qh2v | Health ~ hardiness + dexterity outlier |
| 78mq8sed | Health ~ hardiness + dexterity outlier |
| lnk4d7fn | Health ~ hardiness + dexterity outlier |

### Data Quality Issues (Residual Analysis)
These were identified during residual analysis as likely data entry errors:

| Serial | Skin | Level | Health | Issue |
|--------|------|-------|--------|-------|
| jgh9tjrs | bantha | 9 | 139 | Health/level ratio = 15 (expected ~420) |
| q4glknvv | kima | 20 | 965 | Health/level ratio = 48 (expected higher) |
| 5cubijof | dewback | 12 | 803 | Health/level ratio = 67 (unusual) |

## Minimum Level Filters

Creatures at their skin's exact minimum level are excluded because they may have been artificially capped by the game engine:

### CL 2 Minimum Skins
Filtered: angler, bearded_jax (also 3, 4), boar_wolf, bocatt, choku, durni (also 3, 4, 5, 6), eopi, gnort (also 3, 4), hermit_spider (also 9), huurton (also 10), kima (also 9), krahbu, kusak, langlatch, mott, roba, shear_mite, slice_hound (also 10), squall (also 9), swirl_prong, vir_vur (also 7)

### CL 5 Minimum Skins
Filtered: bageraset (also 10), bantha (also 6), blurrg, bol (also 10), bolle_bol (also 10), bolma, bordok, brackaset (also 8), carrion_spat (also 10), cu_pa (also 9), dalyrake, dewback (also 6), dune_lizard (also 6), falumpaset (also 8), gualama, guf_drolg, gurnaset, gurrcat (also 6), gurreck (also 7), ikopi, kaadu (also 8), kahmurra, kwi, mawgax, narglatch, pugoriss, verne, zucca_boar

### CL 10 Minimum Skins
Filtered: huf_dun, piket, razor_cat, veermok, woolamander

### CL 15 Minimum Skins
Filtered: gronda, kliknik, ronto, snorbal, thune, tyblis, vesp

### CL 20 Minimum Skins
Filtered: malkloc, torton

### CL 25 Minimum Skins
Filtered: graul, merek, sharnaff

### CL 30 Minimum Skins
Filtered: fambaa

### CL 35 Minimum Skins
Filtered: rancor

### CL 40 Minimum Skins
Filtered: kimogila

## Summary

After all filters:
- **370 creatures remain** (from 461 original)
- **79 armored** (fortitude >= 500 with armor = 1)
- **291 unarmored** (armor = 0)

## Rationale

1. **Typo/error data**: Obviously incorrect values that would skew regression
2. **Minimum level creatures**: These may be at an artificial floor imposed by the skin, not the formula
3. **Logical inconsistencies**: Data that violates known game rules
4. **Health/level outliers**: Extreme deviations suggesting data entry errors

## Potential Future Exclusions

36 persistent outliers (|residual| > 3) were identified but NOT excluded:
- These may be legitimate edge cases
- Could be added to filters if confirmed as data errors
- See `creature_level_analysis.md` for details
