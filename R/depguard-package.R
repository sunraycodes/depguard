#' depguard: Manifest-Based Dependency Conflict Detection for R
#'
#' `depguard` helps you catch R package version conflicts in hosted notebooks
#' (Kaggle, Colab, Binder) and on ordinary desktops, where a full `renv`
#' lockfile workflow is impractical or overkill.
#'
#' Three complementary tools:
#'
#' - **Manifest mode**: declare the packages/versions you need with
#'   [dep_manifest()] (or pin the current ones with [dep_manifest_freeze()]),
#'   then verify the live environment, including every transitive
#'   `Imports`/`Depends` constraint, with [dep_check()].
#' - **Snapshot mode**: capture a baseline with [dep_snapshot()] before an
#'   install, then use [dep_diff()] afterwards to see what changed on disk
#'   and whether it affects packages already loaded in your session.
#' - **Diagnostics**: [dep_env()] describes the environment, and
#'   [dep_libraries()] finds packages shadowed by another library.
#'
#' [dep_healthcheck()] runs the right combination automatically, and
#' [dep_fix()] rolls a single package back to a specific version.
#'
#' All checks run from locally installed metadata and work offline.
#'
#' @keywords internal
"_PACKAGE"

#' @importFrom stats setNames
NULL
