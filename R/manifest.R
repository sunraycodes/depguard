#' Declare a dependency manifest
#'
#' Records the packages (and minimum versions) your project needs, so that
#' [dep_check()] can later verify the live environment satisfies them. This
#' is a lightweight alternative to a full `renv` lockfile, intended for
#' sandboxed or ephemeral notebook sessions where you don't own or persist
#' the environment.
#'
#' @param ... Named arguments of the form `package = "version"`, e.g.
#'   `dplyr = "1.1.4"`. A version of `NA` or `""` means "any version is
#'   acceptable, just check it's installed".
#' @param path File path to store the manifest. Defaults to
#'   `.depguard_manifest.rds` in the current working directory.
#'
#' @return Invisibly, the manifest as a named character vector.
#' @export
#'
#' @examples
#' \dontrun{
#' dep_manifest(dplyr = "1.1.4", ggplot2 = "3.5.0")
#' }
dep_manifest <- function(..., path = default_manifest_path()) {
  reqs <- c(...)

  if (length(reqs) == 0) {
    cli::cli_abort("Provide at least one {.code package = \"version\"} pair.")
  }
  if (is.null(names(reqs)) || any(names(reqs) == "")) {
    cli::cli_abort("All arguments to {.fn dep_manifest} must be named, e.g. {.code dplyr = \"1.1.4\"}.")
  }

  reqs <- as.character(reqs)
  names(reqs) <- names(c(...))

  saveRDS(reqs, path)
  cli::cli_alert_success("Manifest saved to {.path {path}} ({length(reqs)} package{?s}).")
  invisible(reqs)
}

#' Read an existing dependency manifest
#'
#' @param path File path to the manifest. Defaults to
#'   `.depguard_manifest.rds` in the current working directory.
#'
#' @return A named character vector of package requirements, or `NULL`
#'   (invisibly) with a message if no manifest is found.
#' @export
dep_manifest_read <- function(path = default_manifest_path()) {
  if (!file.exists(path)) {
    cli::cli_alert_info("No manifest found at {.path {path}}. Use {.fn dep_manifest} to create one.")
    return(invisible(NULL))
  }
  readRDS(path)
}

default_manifest_path <- function() {
  file.path(getwd(), ".depguard_manifest.rds")
}
