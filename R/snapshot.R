# Package state collection --------------------------------------------------

# One row per package: where it lives, the version on disk (what the *next*
# R session would load) and the version loaded in *this* session (if any).
collect_packages <- function(scope = c("all", "loaded"), pkgs = NULL,
                             ip = active_installed(),
                             loaded = loaded_versions()) {
  scope <- match.arg(scope)
  if (is.null(pkgs)) {
    pkgs <- if (scope == "all") ip[, "Package"] else names(loaded)
  }
  pkgs <- unique(unname(pkgs))
  idx <- match(pkgs, ip[, "Package"])
  data.frame(
    package = pkgs,
    disk_version = unname(ip[idx, "Version"]),
    library = unname(ip[idx, "LibPath"]),
    loaded_version = unname(loaded[pkgs]),
    stringsAsFactors = FALSE
  )
}

#' Snapshot package versions
#'
#' Records, for every installed package, the version on disk (what a fresh R
#' session would load) and the version currently loaded in this session. A
#' later call to [dep_diff()] compares against the snapshot to reveal what an
#' install silently changed.
#'
#' The most recent snapshot is remembered for the rest of the session, so
#' `dep_diff()` can be called without arguments. In hosted notebooks the
#' session usually has to be restarted after an install; pass `path` to save
#' the snapshot to disk so it survives the restart, then call
#' `dep_diff("path/to/snapshot.rds")`.
#'
#' @param scope `"all"` (default) records every installed package, so silent
#'   changes to packages you have not loaded yet are caught too. `"loaded"`
#'   records only packages loaded in this session.
#' @param path Optional file path (`.rds`) to save the snapshot to.
#'
#' @return An object of class `depguard_snapshot`, suitable for passing to
#'   [dep_diff()]. Its `packages` element is a data frame with columns
#'   `package`, `disk_version`, `library` and `loaded_version`.
#' @export
#'
#' @examples
#' snap <- dep_snapshot()
#' dep_diff(snap)
dep_snapshot <- function(scope = c("all", "loaded"), path = NULL) {
  scope <- match.arg(scope)
  snap <- structure(
    list(
      time = Sys.time(),
      scope = scope,
      platform = detect_platform(),
      r_version = R.version.string,
      packages = collect_packages(scope)
    ),
    class = "depguard_snapshot"
  )
  .state$last_snapshot <- snap
  if (!is.null(path)) {
    saveRDS(snap, path)
    cli::cli_alert_success("Snapshot saved to {.path {path}}.")
  }
  snap
}

#' @param x A `depguard_snapshot` object.
#' @param ... Ignored.
#' @rdname dep_snapshot
#' @export
print.depguard_snapshot <- function(x, ...) {
  cli::cli_h3("depguard snapshot")
  cli::cli_text("Captured: {format(x$time)} on {.field {x$platform}}")
  n_loaded <- sum(!is.na(x$packages$loaded_version))
  cli::cli_text(
    "Packages recorded: {nrow(x$packages)} (scope: {x$scope}; {n_loaded} loaded)"
  )
  invisible(x)
}

#' Diff the current environment against a prior snapshot
#'
#' Compares package versions now against a snapshot taken earlier with
#' [dep_snapshot()]. Changes are detected on disk, so an upgrade is caught
#' even though R keeps running the old version in memory until restart.
#'
#' Risk levels:
#' - `"high"`: the package is loaded in this session *and* it changed on disk
#'   (or the loaded version no longer matches disk). Code already run used
#'   one version; code run after a restart will use another.
#' - `"low"`: the package changed but is not loaded.
#' - `"none"`: nothing changed.
#'
#' @param snapshot A `depguard_snapshot` from [dep_snapshot()], a path to a
#'   snapshot saved with `dep_snapshot(path = )`, or `NULL` (default) to use
#'   the most recent snapshot taken in this session.
#'
#' @return Invisibly, a data frame with one row per package and columns
#'   `package`, `before`, `after`, `change` (`"upgraded"`, `"downgraded"`,
#'   `"added"`, `"removed"`, `"none"`), `changed`, `currently_loaded`,
#'   `loaded_version`, `session_stale` (loaded version differs from disk) and
#'   `risk`.
#' @export
dep_diff <- function(snapshot = NULL) {
  if (is.null(snapshot)) {
    snapshot <- .state$last_snapshot
    if (is.null(snapshot)) {
      cli::cli_abort(
        "No snapshot to compare against. Create one with {.fn dep_snapshot} first."
      )
    }
  } else if (is.character(snapshot) && length(snapshot) == 1L) {
    if (!file.exists(snapshot)) {
      cli::cli_abort("Snapshot file {.path {snapshot}} does not exist.")
    }
    snapshot <- readRDS(snapshot)
  }
  if (!inherits(snapshot, "depguard_snapshot")) {
    cli::cli_abort("{.arg snapshot} must be created with {.fn dep_snapshot}.")
  }

  before <- snapshot$packages
  loaded <- loaded_versions()
  after <- if (identical(snapshot$scope, "loaded")) {
    collect_packages("loaded", pkgs = before$package, loaded = loaded)
  } else {
    collect_packages("all", loaded = loaded)
  }

  result <- compute_diff(before, after, loaded)
  report_diff_result(result)
  invisible(result)
}

# Pure comparison logic (no session state), unit-tested directly.
compute_diff <- function(before, after, loaded) {
  pk <- union(before$package, after$package)
  b <- before$disk_version[match(pk, before$package)]
  a <- after$disk_version[match(pk, after$package)]

  change <- vapply(seq_along(pk), function(i) {
    if (is.na(b[i]) && is.na(a[i])) return("none")
    if (is.na(b[i])) return("added")
    if (is.na(a[i])) return("removed")
    cmp <- ver_cmp(a[i], b[i])
    if (is.na(cmp) || cmp == 0L) {
      if (identical(a[i], b[i])) "none" else "upgraded"
    } else if (cmp > 0L) "upgraded" else "downgraded"
  }, character(1))

  loaded_v <- unname(loaded[pk])
  is_loaded <- !is.na(loaded_v)
  changed <- change != "none"
  stale <- is_loaded & !is.na(a) & loaded_v != a

  risk <- ifelse((changed | stale) & is_loaded, "high",
                 ifelse(changed, "low", "none"))

  result <- data.frame(
    package = pk,
    before = ifelse(is.na(b), "-", b),
    after = ifelse(is.na(a), "-", a),
    change = change,
    changed = changed,
    currently_loaded = is_loaded,
    loaded_version = ifelse(is_loaded, loaded_v, "-"),
    session_stale = stale,
    risk = risk,
    stringsAsFactors = FALSE
  )
  rank <- c(high = 1L, low = 2L, none = 3L)[result$risk]
  result <- result[order(rank, result$package), , drop = FALSE]
  rownames(result) <- NULL
  result
}

report_diff_result <- function(result) {
  changed <- result[result$changed, ]
  high <- result[result$risk == "high", ]

  if (nrow(changed) == 0L && nrow(high) == 0L) {
    cli::cli_alert_success("No package versions changed since the snapshot.")
    return(invisible(NULL))
  }

  if (nrow(changed) > 0L) {
    counts <- table(changed$change)
    parts <- paste0(counts, " ", names(counts))
    cli::cli_alert_info(
      "{nrow(changed)} package{?s} changed since the snapshot ({paste(parts, collapse = ', ')})."
    )
  }
  if (nrow(high) > 0L) {
    shown <- utils::head(high, 10L)
    cli::cli_alert_danger(
      "{nrow(high)} loaded package{?s} at risk of breaking code in this session:"
    )
    for (i in seq_len(nrow(shown))) {
      pkg <- shown$package[i]; was <- shown$before[i]; now <- shown$after[i]
      run <- shown$loaded_version[i]
      cli::cli_bullets(c(" " = "{pkg}: {was} -> {now} on disk (running {run})"))
    }
    if (nrow(high) > nrow(shown)) {
      cli::cli_bullets(c(" " = "... and {nrow(high) - nrow(shown)} more (see the returned data frame)."))
    }
  }
  if (any(result$session_stale)) {
    cli::cli_alert_warning(
      "R is still running older versions of some packages. {restart_hint()}"
    )
  }
  invisible(NULL)
}
