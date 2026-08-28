#' Roll back a single package to a specific version
#'
#' Reinstalls one package at a specific version, e.g. to undo a version that
#' was silently pulled in as a side effect of another install. This performs
#' a **single-package rollback only** — it does not resolve cascading
#' conflicts that the rollback might introduce with other packages. For full
#' dependency resolution, use `renv::restore()` or `pak`'s solver instead.
#'
#' Requires the `remotes` or `pak` package (listed in Suggests, not
#' Imports, so `depguard`'s core checking functions stay dependency-light).
#'
#' @param package Name of the package to roll back.
#' @param version Target version string, e.g. `"1.5.0"`.
#' @param method Which backend to use: `"pak"` (default, if available) or
#'   `"remotes"`.
#'
#' @return Invisibly, `TRUE` on success.
#' @export
#'
#' @examples
#' \dontrun{
#' dep_fix("stringr", "1.5.0")
#' }
dep_fix <- function(package, version, method = c("pak", "remotes")) {
  method <- match.arg(method)

  cli::cli_alert_warning(
    "Rolling back a single package can break other packages that depend on \\
    the newer version. This does not perform cascading conflict resolution."
  )

  if (method == "pak" && requireNamespace("pak", quietly = TRUE)) {
    pak::pkg_install(paste0(package, "@", version))
  } else if (requireNamespace("remotes", quietly = TRUE)) {
    remotes::install_version(package, version = version)
  } else {
    cli::cli_abort(
      "Neither {.pkg pak} nor {.pkg remotes} is installed. Install one of \\
      them to use {.fn dep_fix}."
    )
  }

  cli::cli_alert_success("{package} rolled back to {version}.")
  invisible(TRUE)
}
