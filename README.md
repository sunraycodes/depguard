# depguard
<!-- badges: start -->
  [![R-CMD-check](https://github.com/sunraycodes/depguard/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sunraycodes/depguard/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->
  
Manifest-based, transitive-aware dependency conflict detection for R, built
for sandboxed and ephemeral notebook environments (Kaggle, Colab, Binder)
where a full `renv` lockfile workflow doesn't fit.

## Why

Hosted notebooks ship a pre-installed package set at fixed versions.
Installing a new package can silently upgrade a dependency that's already
loaded elsewhere in your session, breaking code downstream with no
install-time error. `renv` and `pak` are great but assume you own and can
persist the environment. `depguard` fills the narrower gap: lightweight,
local-first checks that work without lockfile ownership.

## Install

```r
# install.packages("remotes")
remotes::install_github("yourusername/depguard")
```

## Usage

**Snapshot / diff**, around an install:

```r
library(depguard)

snap <- dep_snapshot()
install.packages("someNewPackage")
dep_diff(snap)
```

**Manifest**, declared once per project:

```r
dep_manifest(dplyr = "1.1.4", ggplot2 = "3.5.0")
dep_check()
```

**One-shot health check**, at the top of a notebook:

```r
dep_healthcheck()
```

**Single-package rollback**:

```r
dep_fix("stringr", "1.5.0")
```

See `vignette("kaggle-colab-workflow")` for the full walkthrough.

## Scope

`dep_check()` is local-only by default (no network calls) and covers
transitive dependencies. Pass `check_cran = TRUE` to additionally query
CRAN's live metadata. `dep_fix()` performs a single-package rollback only;
it does not resolve cascading conflicts -- use `renv::restore()` or `pak`'s
solver for that.

## License

MIT
