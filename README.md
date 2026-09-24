# depguard

<!-- badges: start -->
[![R-CMD-check](https://github.com/sunraycodes/depguard/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sunraycodes/depguard/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/depguard)](https://CRAN.R-project.org/package=depguard)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/depguard)](https://cran.r-project.org/package=depguard)
[![DOI](https://img.shields.io/badge/doi-10.32614/CRAN.package.depguard-blue.svg)](https://doi.org/10.32614/CRAN.package.depguard)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
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
install.packages("depguard")
```

Development version:

```r
# install.packages("remotes")
remotes::install_github("sunraycodes/depguard")
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

See the [vignette](https://cran.r-project.org/web/packages/depguard/vignettes/kaggle-colab-workflow.html) for the full walkthrough, or run `vignette("kaggle-colab-workflow")` locally.

## Scope

`dep_check()` is local-only by default (no network calls) and covers
transitive dependencies. Pass `check_cran = TRUE` to additionally query
CRAN's live metadata. `dep_fix()` performs a single-package rollback only;
it does not resolve cascading conflicts -- use `renv::restore()` or `pak`'s
solver for that.

## Citation

If you use depguard in your work, please cite:

> Shah S, Patel K (2026). _depguard: Manifest-Based Dependency Conflict
> Detection for Sandboxed R Sessions_. R package version 0.1.0,
> <https://CRAN.R-project.org/package=depguard>.
> doi:10.32614/CRAN.package.depguard

Or in R:

```r
citation("depguard")
```

## Contributing

Bug reports and pull requests are welcome at
[github.com/sunraycodes/depguard/issues](https://github.com/sunraycodes/depguard/issues).

## Authors

- **Samruddhi Amol Shah** — author, maintainer
- **Kartik Patel** — author
- **Amrit Pal** — contributor

## License

MIT + file [LICENSE](LICENSE)
