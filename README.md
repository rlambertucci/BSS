# BSS: Comprehensive Statistical Suite for jamovi & R

[![Version](https://img.shields.io/badge/version-0.6.0-blue.svg)](https://github.com/rlambertucci/BSS)
[![License: GPL v2+](https://img.shields.io/badge/License-GPL%20v2%2B-green.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.html)
[![jamovi](https://img.shields.io/badge/jamovi-module-orange.svg)](https://www.jamovi.org)

**BSS** is an advanced statistical analysis suite developed by **Prof. Rafael Lambertucci** (Federal University of São Paulo — UNIFESP) for **jamovi** and **R**. It extends standard analytic pipelines with robust, non-parametric, multivariate, psychometric, and design-planning tools tailored for biomedical, psychological, and behavioral research.

---

## 🔬 Scientific Architecture & Modules

The package provides 10 integrated analysis engines:

1. **Network Analysis (`networkanalysis`):**
   - Gaussian Graphical Models (EBICglasso) for continuous data and Mixed Graphical Models (`mgm`) for mixed categorical/continuous variables.
   - Robust centrality indices (Strength, Closeness, Betweenness) following Opsahl et al. (2010) and Expected Influence (Robinaugh et al., 2016).

2. **Missing Data Analysis & Imputation (`dataimputation`):**
   - Pattern visualization, Little’s MCAR test, and modern multiple imputation algorithms via Random Forest (`missRanger`) and Chained Equations (`MICE`).

3. **Descriptive Discriminant Analysis (`dda`):**
   - Canonical post-hoc analysis for MANOVA, canonical correlation ratios ($\eta^2$), structural coefficients (Courville & Thompson, 2001), and cross-validation (*jackknife*).

4. **Aligned Rank Transform (`art`):**
   - Nonparametric factorial ANOVA for multifactor designs and interaction testing (Wobbrock et al., 2011) with post-hoc pairwise contrast tests (`art.con`).

5. **Comprehensive Effect Sizes (`effectsize`):**
   - Pairwise standardized and non-parametric effect sizes for independent and repeated-measures designs: Cohen's $d$, Hedges' $g$, Glass's $\Delta$ (honoring control group), Glass's $\Delta_z$ longitudinal (Becker, 1988), Common Language Effect Size (CLES / $P(A > B)$), and bootstrap confidence intervals.

6. **Sample Size & Resource Equation (`samplesize`):**
   - Survey sampling for finite populations (Cochran, 1977; Slovin, 1960) and laboratory animal sample size calculation using Mead's Resource Equation Approach (Festing & Altman, 2002; Charan & Kantharia, 2013; Arifin & Wan Mohammad, 2017) across completely randomized, factorial, and repeated measures designs.

7. **Multi-Method Outlier Detection (`outliers`):**
   - Dixon’s Q Test ($3 \le N \le 30$) with exact critical values (Rorabacher, 1991), Tukey's IQR fences (1.5×, 2.2×, 3.0×), and Modified Z-Score based on median absolute deviation (MAD; Iglewicz & Hoaglin, 1993; Leys et al., 2013).

8. **Correlation Comparison (`correlationcomparison`):**
   - Statistical testing between two correlation coefficients:
     - Independent groups using Fisher's $Z$-transformation (1921).
     - Dependent overlapping correlations sharing a common variable using Williams' $t$-test (1959) / Steiger's test (1980).
     - 95% Confidence Intervals for correlation differences ($\Delta r$) via Zou (2007).

9. **Data Presentation & Normative Tables (`datapresentation`):**
   - High-quality visualization (bar charts, boxplots, mean & dispersion graphs with individual jittered data points) and automated descriptive tables formatted according to major scientific style guides: **APA (7th ed.)**, **ABNT (NBR 14724)**, **Vancouver (ICMJE)**, and **Chicago (17th ed.)**.

10. **Power Transformations for Normality (`datatransform`):**
    - Box-Cox (with optional automatic positive shift), Yeo-Johnson, and Johnson curve translation systems ($S_U, S_B, S_L, S_N$).
    - Automated Shapiro-Wilk diagnostics (before/after), Q-Q plots, histograms, and spreadsheet variable export.

---

## 📖 Citation

If you use BSS in your research, please cite:

```bibtex
@manual{BSS2026,
  author = {Lambertucci, Rafael},
  title = {BSS: Comprehensive Statistical Suite for jamovi and R},
  year = {2026},
  version = {0.6.0},
  url = {https://github.com/rlambertucci/BSS}
}
```

---

## 📄 License

This package is licensed under the [GNU General Public License (GPL >= 2)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.html).
