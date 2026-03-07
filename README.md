# Airbnb Market Segmentation — PCA + K-Means Clustering

## Overview

This project performs **unsupervised market segmentation** on large-scale Airbnb data from four major U.S. cities (New York, Los Angeles, Dallas, Chicago) using publicly available data from the [Inside Airbnb](http://insideairbnb.com/) initiative.

The core analytical question is:

> **Are there naturally occurring segments of Airbnb listings that share similar amenity profiles, pricing levels, quality signals, and booking patterns?**

This repository was developed as part of a **team-based academic project** (STAT 480 — Big Data Analytics, University of Illinois Urbana-Champaign). It focuses on the **Market Segmentation component**, contributed by **Felipe Grima Rodriguez**, and includes the shared **data preprocessing pipeline** developed collaboratively with Diego Luis de Mazarredo Martin.

---

## Dataset

Data sourced from **Inside Airbnb**, covering four cities:

| City | Abbrev. |
|------|---------|
| New York | NY |
| Los Angeles | LA |
| Dallas | DA |
| Chicago | CH |

Each city provides:
- A `listings` file — property characteristics, amenities, host information, review scores
- A `calendar` file — day-level availability and pricing

The raw datasets are **large and not included** in this repository. Preprocessed, modeling-ready objects (`.rds`) are provided for full reproducibility.

---

## Project Structure

```
.
├── data/
│   └── processed/
│       ├── all_listings.rds       # Cleaned, merged listing-level dataset (all cities)
│       └── amenities_sparse.rds   # High-dimensional sparse amenity matrix
├── report/
│   └── Final_Proyect_Report.pdf   # Full team academic report
├── src/
│   ├── pre.R                      # Data preprocessing pipeline (shared, Diego + Felipe)
│   ├── MarketSegmentation.Rmd     # Main segmentation analysis (Felipe Grima)
│   ├── MarketSegmentation.pdf     # Rendered output of the segmentation analysis
│   └── PCA+Kmeans_sparse.R        # Standalone R script version of the segmentation
├── .gitignore
├── LICENSE
└── README.md
```

---

## Methodology

### 1. Data Preprocessing (`src/pre.R`)

A joint pipeline (Felipe Grima & Diego Luis de Mazarredo) that transforms raw multi-city CSVs into two modeling-ready objects:

- **`all_listings.rds`** — unified listing-level table with cleaned features, calendar aggregates (occupancy rate, average available price), and engineered variables (`log_price`, `high_occupancy`, binary indicators)
- **`amenities_sparse.rds`** — a high-dimensional sparse binary matrix (listings × top-400 normalized amenities), built using `sparseMatrix` for memory efficiency

Key steps:
- Calendar cleaning: date parsing, temporal features, price normalization
- Listings cleaning: type coercion, amenity count extraction, dropping irrelevant fields
- Calendar aggregation: occupancy rate and average available price per listing
- Missing value strategy: targeted imputation by group (city × room type) and median fallback
- Sparse amenity matrix: text normalization, frequency ranking, top-400 selection

---

### 2. Market Segmentation (`src/MarketSegmentation.Rmd`)

**Authored by: Felipe Grima Rodriguez**

Two complementary segmentation analyses are performed:

#### Part A — Amenity-Based Segmentation

*Can we infer meaningful similarities between listings based solely on their amenity profiles?*

- **PCA on the sparse amenity matrix** using truncated SVD (`irlba` package) — chosen for memory and speed on sparse data
- 6 principal components retained (explain ~2/3 of total variance)
- Each PC receives a semantic interpretation based on its top loadings:

| PC | Interpretation |
|----|----------------|
| PC1 | Modern Comfort vs. Basic Interior Setup |
| PC2 | Indoor Heating & Essentials vs. Outdoor/Shared Features |
| PC3 | Urban Self-Service Units vs. Home-Style Amenities |
| PC4 | Outdoor & Pet-Friendly vs. Indoor Personal-Care Focus |
| PC5 | Full Kitchen Functionality vs. Host Safety/Essentials |
| PC6 | Practical/Security Amenities vs. Luxury Building Features |

- **K-Means clustering** on PC scores with K=8 (selected by elbow + silhouette analysis)
- Produces 8 interpretable amenity-based clusters (e.g., *Urban Secure Studios*, *Outdoor Self-Service Homes*, *Full-Kitchen Apartments*)

#### Part B — Price–Quality–Occupancy Segmentation

*Can we identify clusters of listings with similar performance profiles?*

- Variables selected: `log_price`, `occupancy_rate`, review scores, amenity count, host characteristics, availability
- Standard PCA (`prcomp`) on scaled features — 6 PCs explain ~80% of variance
- **K-Means clustering** with K=6 (silhouette peaks clearly at K=6)
- Produces 6 performance-based segments (e.g., *Urban Budget Micro-Studios*, *Luxury Comfort Homes*)

---

## Results Summary

- **Amenity segmentation (K=8)** reveals distinct listing types based purely on what they offer — from security-focused urban studios to outdoor self-service homes
- **Performance segmentation (K=6)** identifies market tiers combining price, occupancy, reviews, and host professionalism
- Together, the two segmentations provide a multi-dimensional view of the Airbnb market landscape across four major U.S. cities

For detailed results and visualizations, see [src/MarketSegmentation.pdf](src/MarketSegmentation.pdf) and the [Full Report](report/Final_Proyect_Report.pdf).

---

## Individual Contributions

### Felipe Grima Rodriguez *(this repository)*
- Designed and implemented both segmentation analyses (amenity-based and performance-based)
- Chose and justified PCA dimensionality reduction strategy for sparse and dense data
- Performed K selection via elbow plots and silhouette analysis
- Interpreted and named all PCA components and K-Means clusters
- Co-developed the data preprocessing pipeline

### Diego Luis de Mazarredo Martin
- Co-developed the data preprocessing pipeline
- Built the booking prediction models (logistic regression + LASSO regularization)
- See [Diego's repository](https://github.com/diegodlmm) for the full preprocessing and modeling work

### Other team members (Adriana Zugasti Alvarez, Jaime Martinez Martinez)
- Price modeling and treatment effect analysis
- Report writing and result interpretation

---

## Technologies & Tools

- **R** / RStudio
- R Markdown
- `irlba` — truncated SVD for sparse PCA
- `cluster` — silhouette analysis
- `ggplot2`, `reshape2` — visualization
- `Matrix` — sparse matrix representation
- `data.table`, `dplyr` — data manipulation
- `fastDummies` — dummy encoding
- Unsupervised learning: PCA, K-Means

---

## How to Run

The preprocessing pipeline has already been executed. The resulting `.rds` files are included in `data/processed/` and can be used directly.

### Prerequisites

Install required R packages:

```r
install.packages(c("irlba", "cluster", "ggplot2", "reshape2", "dplyr", "fastDummies"))
```

### Reproducing the Segmentation Analysis

1. Open the project in RStudio.
2. Set your working directory to `data/processed/` before knitting, **or** update the `readRDS()` paths at the top of the `.Rmd` to use relative paths:

   ```r
   # Change these two lines in MarketSegmentation.Rmd if needed:
   all_listings <- readRDS("../data/processed/all_listings.rds")
   amen        <- readRDS("../data/processed/amenities_sparse.rds")
   ```

3. Render the analysis:

   ```r
   rmarkdown::render("src/MarketSegmentation.Rmd")
   ```

   This will load the preprocessed data and produce all PCA interpretations, K-Means clusters, and visualizations reported in the analysis.

---

## Academic Context

**Course:** STAT 480 — Big Data Analytics
**Institution:** University of Illinois Urbana-Champaign
**Full team report:** [report/Final_Proyect_Report.pdf](report/Final_Proyect_Report.pdf)