# Creature Level Formulas - Quick Reference

This is the quick reference for the derived creature level formulas. For full analysis details, see `creature_level_analysis.md`.

## Armored Creatures (fortitude >= 500)

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

**Accuracy**: R² = 0.979, ±1.8 levels (1 SD)

## Unarmored Creatures (fortitude < 500) — M8

```
level = 8.13
      + 0.012  × hardiness
      - 0.019  × fortitude
      + 0.004  × dexterity
      + 0.011  × intellect
      + 0.020  × cleverness
      + 0.016  × power
      + 0.170  × ke_floor
      + 0.050  × nonkinen
      + 0.106  × pmax(cleverness − 400, 0)        # high-DPS hinge
      + skin_adjustment
```

**Accuracy**: R² = 0.958, ±1.73 levels (1 SD)

## Variable Definitions

```
ke_floor = pmax(pmin(kinetic_resist, energy_resist), 0)   # weaker side, floored at 0
nonkinen = (blast + heat + cold + electricity + acid + stun) / 6
```

`ke_floor` reflects that vulnerability on either kinetic or energy erases all
kin/eng resist credit (Phase 9). The cleverness hinge adds extra level for
apex-DPS pets above clev 400 (Phase 15) — effective slope above the knot is
0.020 + 0.106 = 0.126.

## Notable Skin Adjustments

(Computed under M7. Apply alongside the unarmored formula. The rancor row was
absorbed by the M8 cleverness hinge — do not apply it on top of M8.)

| Skin | Adjustment |
|------|------------|
| Merek | +1.9 |
| Falumpaset | +1.2 |
| Woolamander | -2.5 |
| Torton | -1.5 |
| Huurton | -1.2 |

Most other skins: ±0.5 levels

## Minimum CL by Skin

From historical guide:
- CL 2: Angler, Boar Wolf, Bocatt, Choku, Durni, etc.
- CL 5: Bantha, Bol, Dewback, Cu Pa, Kaadu, etc.
- CL 10: Razor Cat, Veermok, Woolamander, etc.
- CL 15: Kliknik, Ronto, Snorbal, Vesp, etc.
- CL 20: Malkloc, Torton
- CL 25: Graul, Merek, Sharnaff
- CL 30: Fambaa
- CL 35: Rancor
- CL 40: Kimogila

## Key Insights

1. **Armor flips fortitude**: +0.06 for armored, -0.019 for unarmored
2. **Kinetic/Energy dominant**: ke_floor coefficient > nonkinen, and weaker
   side floored at zero (vulnerability erases credit)
3. **DPS contributes**: cleverness (0.020) + power (0.016), with a steeper
   slope above clev 400 (effective 0.126)
4. **~1.7 level variance** unexplained (likely crafting randomness)
