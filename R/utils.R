# Internal helpers shared across depguard ---------------------------------

# Package-level state (e.g. the most recent snapshot taken this session).
.state <- new.env(parent = emptyenv())

`%||%` <- function(x, y) if (is.null(x)) y else x

has_pkg <- function(pkg) requireNamespace(pkg, quietly = TRUE)

# installed.packages() across all library paths, de-duplicated so that only
# the copy R would actually load (first match on .libPaths()) is kept.
active_installed <- function(lib.loc = .libPaths()) {
  ip <- utils::installed.packages(lib.loc = lib.loc, noCache = TRUE)
  ip[!duplicated(ip[, "Package"]), , drop = FALSE]
}

# Named character vector: namespace -> version currently loaded in this R
# process (which may differ from what is on disk after an install).
loaded_versions <- function() {
  ns <- loadedNamespaces()
  v <- vapply(ns, function(p) {
    tryCatch(as.character(getNamespaceVersion(p)),
             error = function(e) NA_character_)
  }, character(1), USE.NAMES = FALSE)
  names(v) <- ns
  v
}

ip_version <- function(pkg, ip) {
  unname(ip[match(pkg, ip[, "Package"]), "Version"])
}

lib_is_writable <- function(lib) {
  dir.exists(lib) && file.access(lib, 2L) == 0L
}

first_writable_lib <- function() {
  libs <- .libPaths()
  ok <- vapply(libs, lib_is_writable, logical(1))
  if (any(ok)) libs[ok][1L] else NULL
}

default_manifest_path <- function() {
  getOption("depguard.manifest_path") %||%
    file.path(getwd(), ".depguard_manifest.rds")
}

# Escape braces in user-supplied text before handing it to cli, which would
# otherwise try to interpolate "{...}".
cli_escape <- function(x) gsub("}", "}}", gsub("{", "{{", x, fixed = TRUE), fixed = TRUE)

# TRUE when two paths point at the same place, whatever their spelling
# (slashes vs backslashes on Windows, "a/../b", trailing slashes, ...).
same_path <- function(a, b) {
  norm <- function(x) normalizePath(x, winslash = "/", mustWork = FALSE)
  !is.na(a) & !is.na(b) & norm(a) == norm(b)
}
