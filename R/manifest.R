#' Declare a dependency manifest
#'
#' Records the packages and versions your project needs, so that
#' [dep_check()] can later verify the live environment satisfies them. This
#' is a lightweight alternative to a full `renv` lockfile, intended for
#' sandboxed or ephemeral notebook sessions and for quick desktop projects
#' where you don't want to manage a lockfile.
#'
#' Each requirement can be:
#' - a bare version such as `"1.1.4"`: **at least** that version;
#' - a version with an operator: `">= 1.1.4"`, `"> 1.1"`, `"== 1.1.4"`
#'   (exactly), `"< 2.0"`, `"<= 2.0"` or `"!= 1.2"`;
#' - `NA` or `""`: any version, as long as it is installed.
#'
#' Use `"=="` when an *upgrade* would also be a conflict (see
#' [dep_manifest_freeze()] to pin whatever you have now).
#'
#' The file format follows the extension of `path`. `.rds` (the default)
#' stores an R object. Any other extension (for example `depguard.txt`)
#' writes a human-readable text file, one `package (op version)` per line,
#' which is convenient to edit and to commit to version control.
#'
#' @param ... Named arguments of the form `package = "requirement"`, e.g.
#'   `dplyr = "1.1.4"` or `ggplot2 = ">= 3.5.0"`.
#' @param path File path to store the manifest. Defaults to
#'   `.depguard_manifest.rds` in the current working directory (or
#'   `getOption("depguard.manifest_path")` if set).
#'
#' @return Invisibly, the manifest as a named character vector.
#' @export
#'
#' @examples
#' tmp <- tempfile(fileext = ".rds")
#' dep_manifest(dplyr = "1.1.4", ggplot2 = ">= 3.5.0", path = tmp)
#' unlink(tmp)
#'
#' # Human-readable text format
#' txt <- tempfile(fileext = ".txt")
#' dep_manifest(cli = "== 3.6.2", path = txt)
#' readLines(txt)
#' unlink(txt)
dep_manifest <- function(..., path = default_manifest_path()) {
  reqs <- c(...)

  if (length(reqs) == 0) {
    cli::cli_abort("Provide at least one {.code package = \"version\"} pair.")
  }
  if (is.null(names(reqs)) || any(names(reqs) == "")) {
    cli::cli_abort("All arguments to {.fn dep_manifest} must be named, e.g. {.code dplyr = \"1.1.4\"}.")
  }

  reqs <- validate_manifest(setNames(as.character(reqs), names(reqs)))
  write_manifest(reqs, path)
  cli::cli_alert_success("Manifest saved to {.path {path}} ({length(reqs)} package{?s}).")
  invisible(reqs)
}

#' Read an existing dependency manifest
#'
#' @param path File path to the manifest (`.rds`, or a text file written by
#'   [dep_manifest()]). Defaults to `.depguard_manifest.rds` in the current
#'   working directory.
#'
#' @return A named character vector of package requirements, or `NULL`
#'   (invisibly) with a message if no manifest is found.
#' @export
dep_manifest_read <- function(path = default_manifest_path()) {
  if (!file.exists(path)) {
    cli::cli_alert_info("No manifest found at {.path {path}}. Use {.fn dep_manifest} to create one.")
    return(invisible(NULL))
  }
  if (is_rds_path(path)) {
    validate_manifest(readRDS(path))
  } else {
    read_text_manifest(path)
  }
}

#' Pin the packages you are using right now
#'
#' Writes a manifest from the versions currently installed, so a later
#' [dep_check()] (for example at the top of a re-run notebook, or after
#' installing something new) tells you if any of them moved.
#'
#' @param packages Character vector of packages to pin. Defaults to the
#'   packages attached in this session (the ones you called `library()` on),
#'   excluding base R packages and depguard itself.
#' @param exact If `TRUE` (default) each package is pinned with `"=="`, so
#'   both downgrades and upgrades are reported. If `FALSE`, versions are
#'   recorded as minimums.
#' @inheritParams dep_manifest
#'
#' @return Invisibly, the manifest as a named character vector.
#' @export
#'
#' @examples
#' tmp <- tempfile(fileext = ".txt")
#' dep_manifest_freeze("cli", path = tmp)
#' readLines(tmp)
#' unlink(tmp)
dep_manifest_freeze <- function(packages = NULL, exact = TRUE,
                                path = default_manifest_path()) {
  ip <- active_installed()
  if (is.null(packages)) {
    attached <- setdiff(.packages(), "depguard")
    packages <- attached[!ip[match(attached, ip[, "Package"]), "Priority"] %in% "base"]
  }
  if (length(packages) == 0L) {
    cli::cli_abort(c(
      "No packages to pin.",
      "i" = "Attach packages with {.fn library} first, or pass {.arg packages}."
    ))
  }
  versions <- ip_version(packages, ip)
  if (anyNA(versions)) {
    cli::cli_abort("Not installed: {.pkg {packages[is.na(versions)]}}.")
  }
  reqs <- if (exact) paste0("==", versions) else versions
  names(reqs) <- packages
  do.call(dep_manifest, c(as.list(reqs), list(path = path)))
}

# Internals ---------------------------------------------------------------

is_rds_path <- function(path) {
  ext <- tolower(tools::file_ext(path))
  ext %in% c("rds", "")
}

validate_manifest <- function(m) {
  if (!is.character(m) || is.null(names(m)) || any(names(m) == "")) {
    cli::cli_abort("A manifest must be a named character vector (package = requirement).")
  }
  for (i in seq_along(m)) {
    tryCatch(
      parse_requirement(m[[i]]),
      error = function(e) {
        cli::cli_abort(
          "Invalid requirement for {.pkg {names(m)[i]}}: {.val {m[[i]]}}.",
          parent = e
        )
      }
    )
  }
  m
}

write_manifest <- function(reqs, path) {
  if (is_rds_path(path)) {
    saveRDS(reqs, path)
    return(invisible(path))
  }
  lines <- vapply(seq_along(reqs), function(i) {
    req <- parse_requirement(reqs[[i]])
    if (req$op == "any") names(reqs)[i]
    else sprintf("%s (%s %s)", names(reqs)[i], req$op, req$version)
  }, character(1))
  writeLines(c("# depguard manifest: package (operator version)", lines), path)
  invisible(path)
}

read_text_manifest <- function(path) {
  lines <- sub("#.*$", "", readLines(path, warn = FALSE))
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  specs <- lapply(lines, parse_spec)
  bad <- vapply(specs, is.null, logical(1))
  if (any(bad)) {
    cli::cli_abort(c(
      "Could not parse line{?s} in {.path {path}}:",
      setNames(cli_escape(lines[bad]), rep("x", sum(bad)))
    ))
  }
  if (length(specs) == 0L) {
    cli::cli_abort("The manifest {.path {path}} contains no packages.")
  }
  reqs <- vapply(specs, `[[`, character(1), "requirement")
  names(reqs) <- vapply(specs, `[[`, character(1), "package")
  validate_manifest(reqs)
}
