## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local Windows 11, R 4.6.1
* macOS (via win-builder, R release) — status OK
* R-hub (Linux: multiple gcc/clang versions, Ubuntu, macOS, macOS-arm64) — all checks passed except:
  - `nosuggests`: expected failure, package has Suggests-only vignette/test dependencies
  - `rchk`: not applicable, package has no compiled C/C++ code

## Downstream dependencies

This is a new submission; there are no downstream dependencies.

## Resubmission notes

This is a resubmission addressing CRAN reviewer feedback:

* Added `\value` documentation to `print.depguard_snapshot.Rd`.
* Removed all use of `installed.packages()`; `dep_check()` now uses
  `utils::packageVersion()` for per-package lookups instead of scanning
  the whole library.
* Unwrapped `\dontrun{}` from the `dep_healthcheck()`, `dep_manifest()`,
  and `dep_snapshot()` examples, all of which now run unconditionally
  and complete in well under 5 seconds with no side effects on the
  user's filesystem (the `dep_manifest()` example now writes to
  `tempfile()` and cleans up after itself).
* `dep_fix("stringr", "1.5.0")` remains wrapped in `\dontrun{}`. This
  function installs/downgrades a real package via `pak` or `remotes`
  and requires network access. We tested wrapping it in `\donttest{}`
  instead, but this fails under `R CMD check --run-donttest` in a
  clean/sandboxed environment because the `pak` subprocess cannot
  resolve `R_USER_CACHE_DIR`. Since the example genuinely cannot be
  executed unattended in the check environment, `\dontrun{}` is kept
  for this one example only.