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