#' depguard: Manifest-Based Dependency Conflict Detection for Sandboxed R Sessions
#'
#' `depguard` helps you catch R package version conflicts in sandboxed or
#' ephemeral notebook environments (Kaggle, Colab, Binder) where a full
#' `renv` lockfile workflow is impractical because you don't own or persist
#' the environment.
#'
#' Two complementary modes:
#'
#' - **Manifest mode**: declare the packages/versions you need with
#'   [dep_manifest()], then verify the live environment (including
#'   transitive dependencies) with [dep_check()].
#' - **Snapshot mode**: capture a baseline with [dep_snapshot()] before an
#'   install, then use [dep_diff()] afterward to see what changed and
#'   whether it affects packages already loaded in your session.
#'
#' [dep_healthcheck()] is a convenience entry point that picks the
#' appropriate mode automatically.
#'
#' @keywords internal
"_PACKAGE"

#' Print a depguard snapshot
#'
#' @param x A `depguard_snapshot` object.
#' @param ... Ignored.
#' @export
print.depguard_snapshot <- function(x, ...) {
  cli::cli_h3("depguard snapshot")
  cli::cli_text("Captured: {x$time}")
  cli::cli_text("Packages recorded: {nrow(x$packages)}")
  invisible(x)
}
