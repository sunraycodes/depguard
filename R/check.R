#' Check the live environment against a dependency manifest
#'
#' Verifies that the packages declared in a manifest are installed at the
#' required versions, and (by default) walks their transitive dependency
#' tree to catch *real* conflicts: for every package in the tree, each
#' `Imports`/`Depends`/`LinkingTo` constraint such as `cli (>= 3.4.0)` is
#' compared against the version actually installed. This is the situation
#' that bites in practice, e.g. a newer package was installed but a
#' dependency was left at an older version.
#'
#' Everything is computed from locally installed package metadata, so it
#' works with no internet connection. The network is only used if
#' `check_cran = TRUE`.
#'
#' The `status` column is one of:
#' - `"ok"`: requirement satisfied;
#' - `"mismatch"`: installed, but the version does not satisfy the requirement;
#' - `"missing"`: not installed;
#' - `"restart"`: the version on disk is fine, but the older version still
#'   loaded in this R session is not. Restart R to pick up the new one.
#'
#' @param manifest A named character vector as returned by [dep_manifest()]
#'   / [dep_manifest_read()]. If `NULL`, the manifest is read from `path`.
#' @param path File path to the manifest, used only if `manifest` is `NULL`.
#' @param recursive Logical; if `TRUE` (default), also check transitive
#'   dependencies of each manifest package, not just the packages listed
#'   directly.
#' @param check_cran Logical; if `TRUE`, additionally query CRAN for the
#'   latest available version of each package (adds `cran_latest` and
#'   `outdated` columns). Requires network access; if CRAN cannot be reached
#'   a warning is shown and the rest of the result is returned unchanged.
#' @param stop_on_problem Logical; if `TRUE`, raise an error when any
#'   requirement is not `"ok"`. Useful in scripts and CI.
#'
#' @return Invisibly, a data frame (class `depguard_check`) with columns
#'   `package`, `required`, `installed`, `status`, `depth`
#'   (`"direct"`/`"transitive"`), `required_by`, `loaded` (version loaded in
#'   this session, or `"-"`) and `reason`. Printing it shows only the
#'   problem rows; use `print(x, all = TRUE)` to see everything.
#' @export
#'
#' @examples
#' dep_check(manifest = c(cli = "1.0.0", utils = ""), recursive = FALSE)
dep_check <- function(manifest = NULL,
                      path = default_manifest_path(),
                      recursive = TRUE,
                      check_cran = FALSE,
                      stop_on_problem = FALSE) {
  if (is.null(manifest)) {
    manifest <- dep_manifest_read(path)
    if (is.null(manifest)) {
      return(invisible(NULL))
    }
  }
  manifest <- validate_manifest(manifest)

  ip <- active_installed()
  loaded <- loaded_versions()
  direct_pkgs <- names(manifest)

  rows <- list(check_rows(
    pkg = direct_pkgs, required = unname(manifest), depth = "direct",
    required_by = NA_character_, ip = ip, loaded = loaded
  ))

  if (recursive) {
    edges <- dependency_edges(direct_pkgs, ip)
    if (nrow(edges) > 0L) {
      has_constraint <- !is.na(edges$requirement)
      keep <- !(edges$package %in% direct_pkgs) | has_constraint
      edges <- edges[keep, , drop = FALSE]
      rows[[2L]] <- check_rows(
        pkg = edges$package, required = edges$requirement,
        depth = "transitive", required_by = edges$parent,
        ip = ip, loaded = loaded
      )
    }
  }

  result <- unique(do.call(rbind, rows))
  rownames(result) <- NULL

  if (check_cran) {
    result <- add_cran_latest(result)
  }
  result <- structure(result, class = c("depguard_check", "data.frame"))

  report_check_result(result)
  if (stop_on_problem && any(result$status != "ok")) {
    cli::cli_abort(
      "{sum(result$status != 'ok')} requirement{?s} not satisfied. See the {.cls depguard_check} result for details."
    )
  }
  invisible(result)
}

# Walk the dependency graph using locally installed metadata only.
# Returns one row per (parent -> package) edge reachable from `direct`,
# with the version requirement declared by `parent` (NA if none).
dependency_edges <- function(direct, ip) {
  empty <- data.frame(parent = character(), package = character(),
                      requirement = character(), stringsAsFactors = FALSE)
  fields <- c("Depends", "Imports", "LinkingTo")
  installed_direct <- direct[direct %in% ip[, "Package"]]
  if (length(installed_direct) == 0L) return(empty)

  tree <- tools::package_dependencies(
    installed_direct, db = ip, recursive = TRUE, which = fields
  )
  nodes <- unique(c(installed_direct, unlist(tree, use.names = FALSE)))
  nodes <- nodes[!is.na(nodes) & nodes %in% ip[, "Package"]]

  base_pkgs <- ip[ip[, "Priority"] %in% "base", "Package"]
  out <- lapply(nodes, function(parent) {
    row <- ip[match(parent, ip[, "Package"]), , drop = FALSE]
    deps <- do.call(rbind, lapply(fields, function(f) parse_dep_field(row[1L, f])))
    if (is.null(deps) || nrow(deps) == 0L) return(NULL)
    deps <- deps[!deps$package %in% base_pkgs, , drop = FALSE]
    if (nrow(deps) == 0L) return(NULL)
    data.frame(parent = parent, package = deps$package,
               requirement = deps$requirement, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, out)
  if (is.null(out)) empty else unique(out)
}

# Vectorised requirement evaluation -> one data frame, one row per input.
check_rows <- function(pkg, required, depth, required_by, ip, loaded) {
  n <- length(pkg)
  installed <- ip_version(pkg, ip)
  loaded_v <- unname(loaded[pkg])

  required_label <- character(n)
  status <- character(n)
  reason <- character(n)

  parsed <- lapply(required, parse_requirement)
  for (i in seq_len(n)) {
    req <- parsed[[i]]
    required_label[i] <- requirement_label(req)
    inst <- installed[i]
    lv <- loaded_v[i]

    if (is.na(inst)) {
      status[i] <- "missing"
      reason[i] <- "not installed"
    } else if (!version_satisfies(inst, req)) {
      status[i] <- "mismatch"
      reason[i] <- sprintf("installed %s does not satisfy %s", inst, required_label[i])
    } else if (!is.na(lv) && lv != inst && !version_satisfies(lv, req)) {
      status[i] <- "restart"
      reason[i] <- sprintf("on disk %s is fine, but this session still runs %s", inst, lv)
    } else {
      status[i] <- "ok"
      reason[i] <- ""
    }
  }

  data.frame(
    package = pkg,
    required = required_label,
    installed = ifelse(is.na(installed), "not installed", installed),
    status = status,
    depth = depth,
    required_by = ifelse(is.na(required_by), "-", required_by),
    loaded = ifelse(is.na(loaded_v), "-", loaded_v),
    reason = reason,
    stringsAsFactors = FALSE
  )
}

# Single-row convenience wrapper (kept for backward compatibility).
check_one_package <- function(pkg, required, depth, required_by,
                              ip = NULL, loaded = NULL) {
  check_rows(
    pkg, required, depth, required_by,
    ip = ip %||% active_installed(), loaded = loaded %||% loaded_versions()
  )
}

add_cran_latest <- function(result) {
  repos <- getOption("repos")
  if (is.null(repos) || any(repos %in% "@CRAN@")) {
    repos <- c(CRAN = "https://cloud.r-project.org")
  }
  old <- options(timeout = min(getOption("timeout"), 15))
  on.exit(options(old), add = TRUE)

  avail <- tryCatch(
    suppressWarnings(utils::available.packages(repos = repos, type = "source")),
    error = function(e) NULL
  )
  if (is.null(avail) || nrow(avail) == 0L) {
    cli::cli_alert_warning("Could not reach CRAN for a live version check; skipping {.arg check_cran}.")
    return(result)
  }
  latest <- unname(avail[match(result$package, avail[, "Package"]), "Version"])
  result$cran_latest <- ifelse(is.na(latest), "unknown", latest)
  result$outdated <- mapply(function(inst, lat) {
    cmp <- ver_cmp(lat, inst)
    !is.na(cmp) && cmp > 0L
  }, result$installed, latest, USE.NAMES = FALSE)
  result
}

report_check_result <- function(result) {
  problems <- result[result$status != "ok", , drop = FALSE]
  n_missing <- sum(result$status == "missing")
  n_mismatch <- sum(result$status == "mismatch")
  n_restart <- sum(result$status == "restart")

  if (nrow(problems) == 0L) {
    cli::cli_alert_success("All {nrow(result)} checked requirement{?s} satisfied.")
    return(invisible(NULL))
  }

  if (n_missing > 0L) cli::cli_alert_danger("{n_missing} requirement{?s} not installed.")
  if (n_mismatch > 0L) cli::cli_alert_danger("{n_mismatch} requirement{?s} not satisfied by the installed version.")
  if (n_restart > 0L) cli::cli_alert_warning("{n_restart} package{?s} need a session restart.")

  shown <- utils::head(problems, 8L)
  for (i in seq_len(nrow(shown))) {
    pkg <- shown$package[i]; why <- shown$reason[i]; by <- shown$required_by[i]
    if (identical(by, "-")) {
      cli::cli_bullets(c(" " = "{pkg}: {why}"))
    } else {
      cli::cli_bullets(c(" " = "{pkg}: {why} (required by {by})"))
    }
  }
  if (nrow(problems) > nrow(shown)) {
    cli::cli_bullets(c(" " = "... and {nrow(problems) - nrow(shown)} more."))
  }
  if (n_restart > 0L) cli::cli_alert_info("{restart_hint()}")
  invisible(NULL)
}

#' @param x A `depguard_check` object.
#' @param all Logical; show every row rather than only problem rows.
#' @param ... Passed on to the data frame print method.
#' @rdname dep_check
#' @export
print.depguard_check <- function(x, all = FALSE, ...) {
  df <- as.data.frame(unclass_check(x))
  show <- if (all) {
    df
  } else if (any(df$status != "ok")) {
    df[df$status != "ok", , drop = FALSE]
  } else {
    df[df$depth == "direct", , drop = FALSE]
  }
  if (!all && nrow(show) < nrow(df)) {
    cli::cli_text("Showing {nrow(show)} of {nrow(df)} row{?s}; use {.code print(x, all = TRUE)} for all.")
  }
  print(show, row.names = FALSE, ...)
  invisible(x)
}

unclass_check <- function(x) {
  class(x) <- "data.frame"
  x
}
