# depguard

<!-- badges: start -->
[![R-CMD-check](https://github.com/sunraycodes/depguard/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sunraycodes/depguard/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/depguard)](https://CRAN.R-project.org/package=depguard)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/depguard)](https://cran.r-project.org/package=depguard)
[![DOI](https://img.shields.io/badge/doi-10.32614/CRAN.package.depguard-blue.svg)](https://doi.org/10.32614/CRAN.package.depguard)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

Manifest-based, transitive-aware dependency conflict detection for R. Built
for hosted notebooks (Kaggle, Colab, Binder) where a full `renv` lockfile
workflow doesn't fit, and just as useful on a normal desktop.

## Why

Hosted notebooks ship a pre-installed package set at fixed versions.
Installing a new package can silently upgrade or downgrade a dependency that's
already loaded elsewhere in your session, breaking code downstream with no
install-time error. The same happens on a desktop when libraries are shared
between projects. `renv` and `pak` are great but assume you own and can
persist the environment. `depguard` fills the narrower gap: lightweight checks
that work from locally installed metadata, **offline**, without lockfile
ownership.

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

**Snapshot / diff**, around an install. Changes are detected on disk, so an
upgrade of an already-loaded package is caught even though R keeps running the
old version until restart. Save the snapshot to survive the restart hosted
notebooks require:

```r
library(depguard)

snap <- dep_snapshot(path = "snapshot.rds")
install.packages("someNewPackage")
dep_diff(snap)              # or, after a restart: dep_diff("snapshot.rds")
```

**Manifest**, declared once per project. Bare versions mean "at least";
operators give you control; text manifests are readable and diff-friendly:

```r
dep_manifest(dplyr = ">= 1.1.4", ggplot2 = "3.5.0", cli = "== 3.6.2")
dep_manifest_freeze(path = "depguard.txt")   # pin what you use right now
dep_check()                                  # or dep_check(path = "depguard.txt")
```

`dep_check()` also verifies the constraints your packages declare on each
other (`Imports: cli (>= 3.4.0)`), which is where real conflicts hide.
Use `dep_check(stop_on_problem = TRUE)` in scripts and CI.

**One-shot health check**, at the top of a notebook:

```r
dep_healthcheck()
```

**Diagnostics**:

```r
dep_env()        # Kaggle / Colab / Binder / RStudio / desktop; writable libraries
dep_libraries()  # packages hidden by a different copy in another library
```

**Single-package rollback** (uses `pak` or `remotes` if installed, otherwise
base R and the CRAN archive):

```r
dep_fix("stringr", "1.5.0", dry_run = TRUE)
dep_fix("stringr", "1.5.0")
```

See the [vignette](https://cran.r-project.org/web/packages/depguard/vignettes/kaggle-colab-workflow.html)
for the full walkthrough, or run `vignette("kaggle-colab-workflow")` locally.

## Scope

All checks are local-only by default (no network calls) and cover transitive
dependencies. Pass `check_cran = TRUE` to additionally query CRAN for the
latest versions. `dep_fix()` performs a single-package rollback only; it does
not resolve cascading conflicts. Use `renv::restore()` or `pak`'s solver for
that.

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
