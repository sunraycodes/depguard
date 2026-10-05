#' Find packages installed in more than one library
#'
#' R searches library paths in order and loads the *first* copy it finds.
#' On hosted notebooks (a system library plus a user library) and on
#' desktops with several libraries, an old copy can shadow the newer one you
#' just installed, so `packageVersion()` and the version you think you
#' installed disagree. This is one of the commonest causes of "I upgraded it
#' but nothing changed".
#'
#' @return Invisibly, a data frame with one row per shadowed copy and columns
#'   `package`, `active_library`, `active_version`, `shadowed_library`,
#'   `shadowed_version` and `differs` (whether the versions are different).
#'   It has zero rows when nothing is shadowed.
#' @export
#'
#' @examples
#' dep_libraries()
dep_libraries <- function() {
  all_ip <- utils::installed.packages(lib.loc = .libPaths(), noCache = TRUE)
  df <- data.frame(
    package = unname(all_ip[, "Package"]),
    library = unname(all_ip[, "LibPath"]),
    version = unname(all_ip[, "Version"]),
    stringsAsFactors = FALSE
  )
  active <- !duplicated(df$package)
  shadowed <- df[!active, , drop = FALSE]
  act <- df[active, , drop = FALSE]
  m <- match(shadowed$package, act$package)

  result <- data.frame(
    package = shadowed$package,
    active_library = act$library[m],
    active_version = act$version[m],
    shadowed_library = shadowed$library,
    shadowed_version = shadowed$version,
    stringsAsFactors = FALSE
  )
  result$differs <- result$active_version != result$shadowed_version
  rownames(result) <- NULL

  n_diff <- sum(result$differs)
  if (n_diff == 0L) {
    cli::cli_alert_success("No package is shadowed by a different version in another library.")
  } else {
    cli::cli_alert_warning(
      "{n_diff} package cop{?y/ies} shadowed by a different version in another library."
    )
    shown <- utils::head(result[result$differs, , drop = FALSE], 5L)
    for (i in seq_len(nrow(shown))) {
      pkg <- shown$package[i]; av <- shown$active_version[i]; sv <- shown$shadowed_version[i]
      cli::cli_bullets(c(" " = "{pkg}: using {av}; {sv} is hidden in another library"))
    }
  }
  invisible(result)
}
