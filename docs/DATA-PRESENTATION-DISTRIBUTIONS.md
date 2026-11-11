# Distribution-dependent presentation

## Scope

The Distribution selector applies to Bar chart and Mean & dispersion. Normal
retains the existing implementation, including covariate-adjusted estimated
marginal means. Gamma and Inverse Gaussian fit the selected observations separately
within each observed group combination and occasion. They are descriptive fits,
not imported GZLM estimates, adjusted marginal means, or tests of longitudinal
change. Their covariate field must be empty; a selected covariate is never silently
ignored or removed.

The descriptive table and plot consume the same fitted summaries. N, median,
minimum and maximum describe the observed non-missing values. Correlation outputs
are unchanged. Repeated-measures fits use available observations at each occasion,
not listwise deletion across occasions; they do not estimate within-subject
contrasts or account for dependence between occasions in a joint model.

## Estimation and uncertainty

For positive observations x, let n be their count and m their arithmetic mean.
Both families have maximum-likelihood mean m. Internally, observations are scaled
by m to improve numerical stability; this is a reversible numerical change of
units, not a transformation of the fitted response or an offset to accommodate
zeros.

- Gamma shape k solves `log(k) - digamma(k) = log(m) - mean(log(x))`.
  Scale is `m / k` and fitted SD is `m / sqrt(k)`.
- Inverse Gaussian uses mean m and shape lambda, with
  `1 / lambda = mean((x - m)^2 / (m^2 * x))`.
  Its fitted SD is `sqrt(m^3 / lambda)`.
- SE is the plug-in asymptotic standard error `fitted SD / sqrt(n)`.
  It is not the dispersion of individual observations.
- Confidence limits for the mean solve
  `2 * (logLik_MLE - logLik_profile(mean)) = qchisq(confidence, 1)`.
  The nuisance shape is re-estimated at each candidate mean. Root searches use
  log mean ratios so finite limits remain positive. This is a likelihood-ratio
  interval with an asymptotic chi-square cutoff, not an exact small-sample or
  prediction interval. Its coverage is not guaranteed in very small samples.

For the inverse Gaussian, with z = x / m and
`d = mean((z - 1)^2 / z)`, the profile likelihood-ratio statistic at mean m*t is
`n * log1p((1/t - 1)^2 / d)`. In particular, the upper confidence limit can be
unbounded. This is reported explicitly rather than replaced by a finite bound.

SD and SE bars are symmetric around the fitted mean and are not coverage
intervals. They are not truncated at zero. CI bars use their actual lower and
upper limits, not a symmetric half-width. The plot caption and table note identify
the family, estimator and uncertainty measure.

## Safeguards

Gamma and Inverse Gaussian require at least two finite, non-missing, strictly
positive observations per cell, with nonzero dispersion. Missing observations
are omitted in the usual available-case calculation, but zero, negative and
infinite observations are not discarded. Missing grouping values follow the
existing exclusion rule and are reported separately.

Invalid cells, numerical estimation failures and unavailable finite confidence
limits suppress the affected dependent-variable plot (or the combined repeated
measures plot). Other independent plots remain available. Status messages identify
the variable or occasion, grouping levels and reason. Failed fitted estimates are
missing in the table, with observed counts and descriptive range retained where
defined. No distribution substitution, data shift or silent filtering occurs.

## Saved analyses and empirical plots

The new `summary_distribution` option defaults to `normal`. The legacy
`theo_dist` option remains readable but hidden; it is not repurposed or mapped to
the new selector. Theoretical density curves are retired. Reopened density plots
with a non-`none` legacy selection show an explicit migration notice and empirical
density only. Boxplots and densities ignore the summary-distribution selection;
their descriptive tables retain the existing calculation.

## Primary references

- R stats, Gamma distribution (`GammaDist`): mean and variance parameterization.
- R stats4, `mle`, `profile-methods`, `confint-methods`: maximum likelihood and
  profile-likelihood confidence intervals.
- Giner G, Smyth GK (2016). statmod: Probability Calculations for the Inverse
  Gaussian Distribution. The R Journal 8(1), 339-351. Mean/shape parameterization.

The likelihoods above are evaluated directly with R base/stats; no new runtime
dependency is required. Normal retains its existing sample-SD and Student-t
calculations, or its existing emmeans-based covariate-adjusted calculation.
