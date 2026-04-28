# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

fixingbe is an R-based statistical analysis project for reverse-engineering the Bio-Engineering system from Star Wars: Galaxies. It uses Generalized Additive Models (GAMs) to analyze creature attributes from historical data scraped from the Furrycat archive.

## Build Commands

```bash
# Build all RMarkdown files to HTML (outputs to html/)
make all

# Build a single analysis document (also runs install_packages.R)
make creature_level_analysis.html

# Render directly without dep-check (faster iteration)
Rscript -e "rmarkdown::render('health_analysis.Rmd', output_format = 'html_document', output_dir = 'html')"

# Install R dependencies
Rscript install_packages.R

# Clean generated HTML files
make clean
```

Key R packages: `mgcv` (GAMs), `segmented` (breakpoint detection — relevant to the fortitude=500 split), `gbm` (variable importance), `broom`/`lmtest`/`ppcor` (diagnostics).

## Ruby Scripts (data scraping)

The Ruby scripts in `scripts/` were used to scrape data from furrycat's archive. The scraped data is already committed to `data/raw/` and `data/clean/`, so these scripts are generally not needed for analysis work.

```bash
# If scraping is needed
cd scripts && bundle install
bundle exec ruby scrape_samples.rb

# Run Ruby tests (single file: bundle exec rspec spec/furrycat_page_spec.rb)
cd scripts && bundle exec rspec
```

## Architecture

### Data Pipeline
- `R/data.R` - Central data loading, cleaning, **and feature engineering**. Loads CSVs from `data/clean/furrycat/`, applies extensive serial-ID-based filters for known bad entries, and derives the `kinen` / `nonkinen` aggregates the formulas depend on. Edits here silently change every downstream model.
- `data/raw/furrycat/` - Raw HTML snapshots from archive
- `data/clean/furrycat/` - Processed CSVs: creatures.csv, templates.csv, experiments.csv, samples.csv

### Analysis Layer
The canonical pipeline is `R/data.R` + `R/creature_level_model.R`. Everything else in `R/` is exploratory and **not sourced by the Rmd files** — treat it as a notebook history, not authoritative code:
- `R/creature_level.R` - Main creature level analysis (~1200 lines)
- `R/creature_level_model.R` - GAM model definitions (canonical)
- `R/investigate_*.R`, `R/test_*.R`, `R/*_scratchpad.R`, `R/derive_formulas.R`, `R/final_model_combination.R` - exploratory; results may have been superseded

### Publication
- `index.Rmd` - GH Pages landing page. Links every `*_analysis.html`; new analyses must be added here too.
- `*.Rmd` files in root - RMarkdown analysis documents
- Published to GitHub Pages via `.github/workflows/publish.yml`
- Uses `ghcr.io/<owner>/r-lua-builder:latest` Docker image for builds

## Key Patterns

### Statistical Modeling
Primary modeling approach uses `mgcv::gam()` with smooth terms:
```r
gam(outcome ~ s(predictor1) + s(predictor2), data = df)
```

### Data Quality
`R/data.R` contains extensive filtering for problematic creatures identified by serial ID. When encountering data anomalies, check if the creature is already filtered or needs to be added to the filter list.

### Analysis Assumptions
- Game values were likely rounded to whole numbers
- Original implementation was efficient C++98 code
- Fortitude values affect creature stats (see "Fort" comments in data.R)

## Key Findings: Creature Level

### Two-Formula Hypothesis (Confirmed)
The game uses **completely different formulas** for armored vs unarmored creatures:

- **Armored** (fortitude >= 500): Clean linear fit (R² ≈ 0.98)
- **Unarmored** (M8, Phase 15): R² ≈ 0.96, SD ≈ 1.73

### Final Formulas

**Armored (fortitude >= 500):**
```
level = -23 + 0.01*hardiness + 0.06*fortitude + 0.005*dexterity
      + 0.01*intellect + 0.025*cleverness + 0.015*power
      + 0.1*kinen + 0.08*nonkinen
```

**Unarmored (fortitude < 500) — M8:**
```
level = 8.13 + 0.012*hardiness - 0.019*fortitude + 0.004*dexterity
      + 0.011*intellect + 0.020*cleverness + 0.016*power
      + 0.170*ke_floor + 0.050*nonkinen
      + 0.106*pmax(cleverness - 400, 0)
```

Where:
- `ke_floor = pmax(pmin(kinetic, energy), 0)` (weaker resist, floored at 0)
- `nonkinen = (blast + heat + cold + electricity + acid + stun) / 6`
- The cleverness hinge gives apex-DPS pets a steeper slope above clev 400.

### Fortitude Sign Flip
The most significant finding: **fortitude has opposite effects** depending on armor status:
- Unarmored: fortitude **lowers** level (coefficient -0.02)
- Armored: fortitude **raises** level (coefficient +0.06)

This explains why bio-engineers could create high-fortitude, low-level unarmored creatures.

### Resist Impact
Kinetic/Energy resists have higher impact on unarmored creatures (`ke_floor` coef 0.17) vs armored (`kinen` coef 0.10). Unarmored creatures rely on resists for survivability — and a vulnerability on either kinetic or energy zeroes out the credit (`ke_floor` is the weaker side, floored at 0).

### Data Cleaning Notes
- Skin minimum levels cause contamination (creatures clamped to skin's minimum)
- Extensive filters in `R/data.R` remove these
- Damage formula changed in patch 14.1; see `damage_analysis.Rmd`
