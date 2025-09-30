# Oil Markets: Brent vs WTI

Reproducible R analysis of Brent–WTI price dynamics and the spread.

## Structure
- `scripts/` – analysis scripts (`01_load.R`, `02_clean.R`, `03_analysis.R`)
- `outputs/figures/` – key plots saved by scripts (tracked)
- `data/` – raw/processed data (mostly ignored; see notes below)
- `reports/` – R Markdown / Quarto report (optional)
- `oil-markets-brent-wti.Rproj` – open this to work in RStudio

## Quick start
```r
install.packages("renv")
renv::restore()
source("scripts/01_load.R")
source("scripts/02_clean.R")
source("scripts/03_analysis.R")  # saves plots to outputs/figures/