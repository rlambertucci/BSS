# BSS: Comprehensive Statistical Suite for jamovi & R

[![Version](https://img.shields.io/badge/version-0.6.1-blue.svg)](https://github.com/rlambertucci/BSS)
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
   - Regression-model pooling uses the fitted models' residual degrees of freedom after missing-case omission and rank determination. If a fitted model has no positive residual degrees of freedom, or these differ across completed datasets, pooling is unavailable with an explanatory note rather than reporting unsupported p-values and confidence intervals.
   - Keyboard assignment uses the native transfer arrows: select variables, Tab to an arrow, use Up/Down to choose the destination, and press Enter. See [keyboard navigation](docs/IMPUTATION-KEYBOARD.md).

3. **Descriptive Discriminant Analysis (`dda`):**
   - Scores retain original spreadsheet row identifiers and can be saved through the native discriminant-score Output control.
   - Canonical post-hoc analysis for MANOVA, canonical correlation ratios ($\eta^2$), structural coefficients (Courville & Thompson, 2001), and cross-validation (*jackknife*).

4. **Aligned Rank Transform (`art`):**
   - Nonparametric factorial ANOVA for multifactor designs and interaction testing (Wobbrock et al., 2011) with post-hoc pairwise contrast tests (`art.con`).

5. **Comprehensive Effect Sizes (`effectsize`):**
   - Pairwise standardized and non-parametric effect sizes for independent and repeated-measures designs: Cohen's $d$, Hedges' $g$, Glass's $\Delta$ (honoring control group), Glass's $\Delta_z$ longitudinal (Becker, 1988), Common Language Effect Size (CLES / $P(A > B)$), and bootstrap confidence intervals.

6. **Sample Size & Resource Equation (`samplesize`):**
   - Survey sampling for finite populations (Cochran, 1977; Slovin, 1960) and laboratory animal sample size calculation using Mead's Resource Equation Approach (Festing & Altman, 2002; Charan & Kantharia, 2013; Arifin & Wan Mohammad, 2017) across completely randomized, factorial, and repeated measures designs.

7. **Multi-Method Outlier Detection (`outliers`):**
   - Modified Z and IQR boxplots are separate native images per selected variable.
   - Dixon’s Q Test ($3 \le N \le 30$) with exact critical values (Rorabacher, 1991), Tukey's IQR fences (1.5×, 2.2×, 3.0×), and Modified Z-Score based on median absolute deviation (MAD; Iglewicz & Hoaglin, 1993; Leys et al., 2013).

8. **Correlation Comparison (`correlationcomparison`):**
   - Statistical testing between two correlation coefficients:
     - Independent groups using Fisher's $Z$-transformation (1921).
     - Dependent overlapping correlations sharing a common variable using Williams' $t$-test (1959) / Steiger's test (1980).
     - 95% Confidence Intervals for correlation differences ($\Delta r$) via Zou (2007).

9. **Data Presentation & Normative Tables (`datapresentation`):**
   - High-quality visualization (bar charts, boxplots, mean & dispersion graphs with individual jittered data points, and empirical density plots) and automated descriptive tables formatted according to major scientific style guides: **APA (7th ed.)**, **ABNT (NBR 14724)**, **Vancouver (ICMJE)**, and **Chicago (17th ed.)**.
   - Normal (default), Gamma, and Inverse Gaussian summaries for bar and mean-dispersion plots. Gamma and Inverse Gaussian use descriptive maximum-likelihood fits by group and occasion, with fitted SD, asymptotic SE, or profile-likelihood confidence intervals; they are not imported GZLM estimates and do not support covariate adjustment. Density plots are empirical only. See [distribution-dependent presentation](docs/DATA-PRESENTATION-DISTRIBUTIONS.md) for methods and limitations.

   - Copy tables and save figures through jamovi's native result menu; analyses do not write figure files to an arbitrary directory. Edit table titles and annotations with the native result controls. Correlation coefficients remain numeric, with separate significance symbols. Rows with missing grouping values are excluded consistently from both descriptive tables and plots.

10. **Power Transformations for Normality (`datatransform`):**
    - Box-Cox (with optional automatic positive shift), Yeo-Johnson, and Johnson curve translation systems ($S_U, S_B, S_L, S_N$).
    - Automated Shapiro-Wilk diagnostics (before/after), Q-Q plots, histograms, and spreadsheet variable export.
    - Selected Q-Q, histogram, and density panels share one before/after diagnostic image per variable, including native figure export.

---

## Windows Builds

Use the isolated locale-safe launcher and two-build comparison described in [docs/BUILD-WINDOWS.md](docs/BUILD-WINDOWS.md). Build outputs stay outside the source checkout.

---

## 📖 Citation

If you use BSS in your research, please cite:

```bibtex
@manual{BSS2026,
  author = {Lambertucci, Rafael},
  title = {BSS: Comprehensive Statistical Suite for jamovi and R},
  year = {2026},
  version = {0.6.1},
  url = {https://github.com/rlambertucci/BSS}
}
```

---

## 📄 License

This package is licensed under the [GNU General Public License (GPL >= 2)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.html).
