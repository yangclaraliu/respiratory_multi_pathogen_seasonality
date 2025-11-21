# Respiratory Multi-Pathogen Seasonality

Analysis of within-year and between-year patterns of seasonal influenza and RSV disease burden, and quantification of concurrent outbreak risks across eight cities in mainland China.

## 📁 Project Structure

```
respiratory_multi_pathogen_seasonality/
├── code/                  # R scripts for data processing, analysis, and visualization
│   ├── 0_LoadData.R      # Data loading and preprocessing
│   ├── 1_main_compile.R  # Main analysis pipeline
│   ├── 2_visualisation_*.R  # Figure generation scripts
│   ├── 3_run_*.R         # Statistical analysis scripts
│   └── 4_*_*.R           # Supplemental table/figure generation
│
├── data/                  # Data directory
│   ├── raw/              # Raw data files from literature
│   └── processed/        # Processed time series data
│
├── figures/              # Generated figures (PDFs, PNGs, JPGs)
│   └── diagnostics/     # Diagnostic plots
│
├── results/              # Analysis results (RDS, QS files)
│
└── writing/              # LaTeX manuscript
    ├── main.tex          # Main manuscript file
    ├── sections/         # Individual section files
    ├── bib/              # Bibliography files
    ├── build/            # Compiled PDFs (tracked for sharing)
    └── README.md         # Detailed writing workflow instructions
```

## 🚀 Quick Start

### For Analysis:
See individual R scripts in `code/` directory. The main pipeline is in `1_main_compile.R`.

### For Manuscript:
See `writing/README.md` for detailed instructions on compiling the LaTeX manuscript.

## 📊 Key Components

- **Code**: R scripts for time-series decomposition (MSTL), forward simulations, and survival analysis
- **Data**: Long-term surveillance data (≥5 years) for influenza and RSV from 8 Chinese cities
- **Figures**: Visualizations of seasonal patterns, model diagnostics, and concurrent outbreak projections
- **Writing**: LaTeX manuscript for submission to Epidemics journal

## 📧 Contact

**Corresponding Author**: Yang Liu (yang.liu@lshtm.ac.uk)  
**Affiliation**: London School of Hygiene & Tropical Medicine
