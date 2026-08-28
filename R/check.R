#' Check the live environment against a dependency manifest
#'
#' Walks the transitive dependency tree of each package declared in the
#' manifest and compares installed versions against requirements. By
#' default this is entirely local (reads installed package `DESCRIPTION`
#' metadata) and makes no network calls, which matters in sandboxed
#' notebooks with limited or no internet access.
#'
#' @param manifest A named character vector as returned by [dep_manifest()]
#'   / [dep_manifest_read()]. If `NULL`, the manifest is read from `path`.
#' @param path File path to the manifest, used only if `manifest` is `NULL`.
#' @param recursive Logical; if `TRUE` (default), also check transitive
#'   dependencies of each manifest package, not just the packages listed
#'   directly.
#' @param check_cran Logical; if `TRUE`, additionally query CRAN's live
#'   metadata for newer available versions. Requires network access and is
#'   `FALSE` by default since sandboxed notebooks may have flaky or no
#'   internet.
#'
#' @return A data frame with columns `package`, `required`, `installed`,
#'   `status` (`"ok"`, `"mismatch"`, `"missing"`), `depth`
#'   (`"direct"`/`"transitive"`), and `required_by`.
#' @export
dep_check <- function(manifest = NULL,
                       path = default_manifest_path(),
                       recursive = TRUE,
                       check_cran = FALSE) {
  if (is.null(manifest)) {
    manifest <- dep_manifest_read(path)
    if (is.null(manifest)) {
      return(invisible(NULL))
    }
  }

  direct_pkgs <- names(manifest)
  installed_db <- utils::installed.packages()

  if (recursive) {
    dep_tree <- tools::package_dependencies(
      direct_pkgs,
      db = installed_db,
      recursive = TRUE,
      which = c("Depends", "Imports", "LinkingTo")
    )
  } else {
    dep_tree <- stats::setNames(vector("list", length(direct_pkgs)), direct_pkgs)
  }

  rows <- list()

  for (pkg in direct_pkgs) {
    rows[[length(rows) + 1]] <- check_one_package(
      pkg = pkg,
      required = manifest[[pkg]],
      depth = "direct",
      required_by = NA_character_,
      installed_db = installed_db
    )

    transitive_pkgs <- dep_tree[[pkg]]
    for (tpkg in transitive_pkgs) {
      if (tpkg %in% direct_pkgs) next # already covered as a direct requirement
      rows[[length(rows) + 1]] <- check_one_package(
        pkg = tpkg,
        required = NA_character_,
        depth = "transitive",
        required_by = pkg,
        installed_db = installed_db
      )
    }
  }

  result <- do.call(rbind, rows)
  result <- unique(result)
  rownames(result) <- NULL

  if (check_cran) {
    result <- add_cran_latest(result)
  }

  report_check_result(result)
  invisible(result)
}

check_one_package <- function(pkg, required, depth, required_by, installed_db) {
  installed_version <- if (pkg %in% rownames(installed_db)) {
    installed_db[pkg, "Version"]
  } else {
    NA_character_
  }

  status <- if (is.na(installed_version)) {
    "missing"
  } else if (is.na(required) || required == "") {
    "ok"
  } else if (utils::compareVersion(installed_version, required) < 0) {
    "mismatch"
  } else {
    "ok"
  }

  data.frame(
    package = pkg,
    required = ifelse(is.na(required) || required == "", "any", required),
    installed = ifelse(is.na(installed_version), "not installed", installed_version),
    status = status,
    depth = depth,
    required_by = ifelse(is.na(required_by), "-", required_by),
    stringsAsFactors = FALSE
  )
}

add_cran_latest <- function(result) {
  if (!requireNamespace("utils", quietly = TRUE)) {
    return(result)
  }
  tryCatch({
    cran_db <- tools::CRAN_package_db()
    latest <- stats::setNames(cran_db$Version, cran_db$Package)
    result$cran_latest <- unname(latest[result$package])
    result$cran_latest[is.na(result$cran_latest)] <- "unknown"
    result
  }, error = function(e) {
    cli::cli_alert_warning("Could not reach CRAN for live version check: {conditionMessage(e)}")
    result
  })
}

report_check_result <- function(result) {
  n_mismatch <- sum(result$status == "mismatch")
  n_missing <- sum(result$status == "missing")

  if (n_mismatch == 0 && n_missing == 0) {
    cli::cli_alert_success("All {nrow(result)} checked package{?s} satisfy the manifest.")
  } else {
    if (n_missing > 0) {
      cli::cli_alert_danger("{n_missing} package{?s} missing.")
    }
    if (n_mismatch > 0) {
      cli::cli_alert_warning("{n_mismatch} package{?s} below the required version.")
    }
    cli::cli_alert_info("Run {.code print(result)} on the returned data frame for details.")
  }
}
