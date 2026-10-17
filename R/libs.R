#' Find packages installed in more than one library
#'
#' R searches library paths in order and loads the *first* copy it finds.
#' On hosted notebooks (a system library plus a user library) and on
#' desktops with several libraries, an old copy can shadow the newer one you
#' just installed, so `packageVersion()` and the version you think you
#' installed disagree. This is one of the commonest causes of "I upgraded it
#' but nothing changed".
#'
#' Shadowing is often harmless. Hosted notebook images commonly ship newer
#' copies of packages in an earlier library than the ones bundled with R, so
#' the *active* copy is the newer one. depguard only raises a warning when
#' the opposite happens: an older copy is winning and a newer one is hidden.
#'
#' @return Invisibly, a data frame with one row per shadowed copy and columns
#'   `package`, `active_library`, `active_version`, `shadowed_library`,
#'   `shadowed_version`, `differs` (whether the versions are different) and
#'   `hidden_newer` (the hidden copy is newer than the one R will load,
#'   which is the case that usually needs attention). It has zero rows when
#'   nothing is shadowed.
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
  cmp <- mapply(ver_cmp, result$active_version, result$shadowed_version,
                USE.NAMES = FALSE)
  result$hidden_newer <- if (length(cmp)) !is.na(cmp) & cmp < 0L else logical()
  rownames(result) <- NULL

  n_risky <- sum(result$hidden_newer)
  n_benign <- sum(result$differs & !result$hidden_newer)

  if (n_risky > 0L) {
    cli::cli_alert_warning(
      "{n_risky} package{?s} {?is/are} running an OLDER copy while a newer one is hidden in another library."
    )
    shown <- utils::head(result[result$hidden_newer, , drop = FALSE], 5L)
    for (i in seq_len(nrow(shown))) {
      pkg <- shown$package[i]; av <- shown$active_version[i]; sv <- shown$shadowed_version[i]
      cli::cli_bullets(c(" " = "{pkg}: using {av}; newer {sv} is hidden in another library"))
    }
    if (n_risky > nrow(shown)) {
      cli::cli_bullets(c(" " = "... and {n_risky - nrow(shown)} more (see the returned data frame)."))
    }
  } else {
    cli::cli_alert_success("No newer package copy is hidden by an older one in another library.")
  }
  if (n_benign > 0L) {
    cli::cli_alert_info(
      "{n_benign} other cop{?y/ies} {?is/are} shadowed by a NEWER one, which is normal when an image ships updated packages."
    )
  }
  invisible(result)
}
