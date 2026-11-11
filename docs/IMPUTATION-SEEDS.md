# Imputation seed compatibility

The three seed options (`seed_mice_cs`, `seed_mice_long`, `seed_mr`) are Integer options. Each has a separate Boolean `use_seed_*` option. New analyses default to random operation: the checkbox is off, and the stored integer 0 is unused. Enabling the checkbox makes 0 a valid fixed seed, not a random sentinel. Negative integer seeds are not repurposed as random sentinels.

## Saved analyses and R calls

The module-local options subclass in `R/zz-dataimputation-options.R` normalizes legacy protobuf values before jmvcore validates Integer options. A blank legacy string becomes integer 0 with fixed mode off; a finite integer-valued numeric string retains its value with fixed mode on. Spaces, signs, leading zeros and scientific notation are accepted when they represent an integer in [-2147483647, 2147483647]. The input protobuf is cloned; the normalized output uses integer and Boolean fields and can be saved and restored again.

The same policy applies to direct options construction and the public R function. The public function keeps the generated signature and forwards only arguments actually supplied by the caller, so supplying a seed still enables fixed mode automatically. Non-seed expressions retain the generated function's lazy evaluation and unquoted variable-selection semantics. An explicitly supplied `use_seed_*` value takes precedence. The following examples illustrate seed arguments; other required variable selections for the generated API must also be supplied:

```r
BSS::dataimputation(data = dataset, seed_mice_cs = 42)
BSS::dataimputation(data = dataset, seed_mice_cs = "42")
BSS::dataimputation(data = dataset, seed_mice_cs = 0)
BSS::dataimputation(data = dataset, seed_mice_cs = 42, use_seed_mice_cs = FALSE)
```

The first three calls select fixed seeds 42, 42 and 0; the last explicitly selects random operation while retaining 42 as an inactive setting. Omitting seeds preserves the random default. Blank legacy strings remain random. The frontend handler mirrors normal legacy-file migration when opening the options panel; there is only one visual owner per seed and checkbox.

Fractional, non-finite, malformed, vector-valued and out-of-range seeds are rejected with an actionable error. They are not silently truncated, clamped or replaced with a random/fixed default. A saved analysis using such a legacy value must be corrected explicitly. This intentional validation change avoids silently changing a random stream, particularly for fractional missRanger bases across the imputation offsets.

## Algorithm contract

MICE receives the chosen integer or `NA_integer_` for random operation. missRanger receives `NULL` for random operation, or the existing `base seed + imputation index` sequence (the first index is 1). The latter must remain within R's integer range; an overflowing offset is rejected before imputation, rather than wrapping or changing the seed.

No global jmvcore/client code is patched. Headers remain compiler-generated; the adapter loads after them and subclasses only this module's options. The three seed-mode options participate in the same result invalidation rules as their seeds. Imputation methods, wave coding, pooling and output identity are unchanged.
