#' Snapshot currently loaded package versions
#'
#' Captures the versions of all currently loaded/attached packages, so a
#' later call to [dep_diff()] can detect what an install silently changed.
#' This is a thin wrapper around [sessioninfo::session_info()].
#'
#' @return An object of class `depguard_snapshot` (invisibly printable),
#'   suitable for passing to [dep_diff()].
#' @export
#'
#' @examples
#' \dontrun{
#' snap <- dep_snapshot()
#' install.packages("someNewPkg")
#' dep_diff(snap)
#' }
dep_snapshot <- function() {
  info <- sessioninfo::session_info(pkgs = "loaded")
  structure(
    list(
      time = Sys.time(),
      packages = info$packages
    ),
    class = "depguard_snapshot"
  )
}

#' Diff the current environment against a prior snapshot
#'
#' Compares package versions now against a snapshot taken earlier with
#' [dep_snapshot()], flagging anything that changed. Packages that changed
#' *and* are still loaded in the current session are flagged as higher risk,
#' since those are the ones most likely to cause silent downstream breakage.
#'
#' @param snapshot A `depguard_snapshot` object from [dep_snapshot()].
#'
#' @return A data frame with columns `package`, `before`, `after`,
#'   `changed`, `currently_loaded`, `risk`.
#' @export
dep_diff <- function(snapshot) {
  if (!inherits(snapshot, "depguard_snapshot")) {
    cli::cli_abort("{.arg snapshot} must be created with {.fn dep_snapshot}.")
  }

  before <- snapshot$packages
  after <- sessioninfo::session_info(pkgs = "loaded")$packages

  all_pkgs <- union(before$package, after$package)
  currently_loaded <- after$package[after$loadedversion != "" & !is.na(after$loadedversion)]

  rows <- lapply(all_pkgs, function(pkg) {
    before_v <- before$loadedversion[before$package == pkg]
    after_v <- after$loadedversion[after$package == pkg]
    before_v <- if (length(before_v) == 0) NA_character_ else before_v
    after_v <- if (length(after_v) == 0) NA_character_ else after_v

    changed <- !identical(before_v, after_v)
    loaded_now <- pkg %in% currently_loaded

    risk <- if (changed && loaded_now) {
      "high"
    } else if (changed) {
      "low"
    } else {
      "none"
    }

    data.frame(
      package = pkg,
      before = ifelse(is.na(before_v), "-", before_v),
      after = ifelse(is.na(after_v), "-", after_v),
      changed = changed,
      currently_loaded = loaded_now,
      risk = risk,
      stringsAsFactors = FALSE
    )
  })

  result <- do.call(rbind, rows)
  result <- result[order(result$risk != "high", result$risk != "low"), ]
  rownames(result) <- NULL

  report_diff_result(result)
  invisible(result)
}

report_diff_result <- function(result) {
  changed <- result[result$changed, ]
  high_risk <- result[result$risk == "high", ]

  if (nrow(changed) == 0) {
    cli::cli_alert_success("No package versions changed since the snapshot.")
    return(invisible(NULL))
  }

  cli::cli_alert_info("{nrow(changed)} package{?s} changed since the snapshot.")
  if (nrow(high_risk) > 0) {
    cli::cli_alert_danger(
      "{nrow(high_risk)} of those {?is/are} currently loaded \\
      and may cause downstream breakage: {paste(high_risk$package, collapse = ', ')}"
    )
  }
}
