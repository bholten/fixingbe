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

## Unarmored Creatures (fortitude < 500)

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

**Accuracy**: R² = 0.947, ±1.9 levels (1 SD)

## Variable Definitions

```
kinen    = (kinetic_resist + energy_resist) / 2
nonkinen = (blast + heat + cold + electricity + acid + stun) / 6
```

## Notable Skin Adjustments

| Skin | Adjustment |
|------|------------|
| Rancor | +5.2 |
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

1. **Armor flips fortitude**: +0.06 for armored, -0.02 for unarmored
2. **Kinetic/Energy dominant**: kinen coefficient > nonkinen
3. **DPS contributes**: cleverness (0.025) + power (0.015)
4. **~2 level variance**: Unexplained (likely crafting randomness)
