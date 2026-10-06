# Test helpers -------------------------------------------------------------

# Run with a clean, non-hosted platform unless a test sets one itself.
local_clean_platform <- function(.env = parent.frame()) {
  vars <- c("KAGGLE_KERNEL_RUN_TYPE", "KAGGLE_URL_BASE", "COLAB_RELEASE_TAG",
            "COLAB_GPU", "COLAB_BACKEND_VERSION", "BINDER_SERVICE_HOST",
            "BINDER_REQUEST", "RSTUDIO")
  for (v in vars) withr::local_envvar(stats::setNames(list(NA), v), .local_envir = .env)
  withr::local_options(depguard.platform = NULL, .local_envir = .env)
}

# Fresh library prepended to .libPaths() for the duration of a test.
local_test_lib <- function(.env = parent.frame()) {
  lib <- withr::local_tempdir(.local_envir = .env)
  withr::local_libpaths(lib, action = "prefix", .local_envir = .env)
  normalizePath(lib, winslash = "/")
}

# Write the source tree of a tiny package; returns its directory.
build_test_src <- function(name, version, imports = NULL, root = tempfile("pkgsrc")) {
  src <- file.path(root, name)
  dir.create(file.path(src, "R"), recursive = TRUE)
  desc <- c(paste0("Package: ", name), paste0("Version: ", version),
            "Title: Test", "Description: Test package.", "License: MIT")
  if (!is.null(imports)) desc <- c(desc, paste0("Imports: ", imports))
  writeLines(desc, file.path(src, "DESCRIPTION"))
  writeLines("export(f)", file.path(src, "NAMESPACE"))
  writeLines("f <- function() 1", file.path(src, "R", "f.R"))
  src
}

# Really install a tiny package into `lib` (no dependency/version checks, so
# we can build deliberately inconsistent libraries).
install_test_pkg <- function(lib, name, version, imports = NULL) {
  src <- build_test_src(name, version, imports)
  on.exit(unlink(dirname(src), recursive = TRUE), add = TRUE)
  status <- system2(
    file.path(R.home("bin"), "R"),
    c("CMD", "INSTALL", "--no-test-load", paste0("--library=", shQuote(lib)), shQuote(src)),
    stdout = FALSE, stderr = FALSE
  )
  stopifnot(status == 0L)
  invisible(file.path(lib, name))
}

# A local CRAN-like mirror (file://) holding the given package versions.
# `specs` is a list of list(name, version); the highest version of each
# package lives in src/contrib, older ones in src/contrib/Archive/<name>.
make_test_mirror <- function(specs, .env = parent.frame()) {
  mirror <- withr::local_tempdir(.local_envir = .env)
  contrib <- file.path(mirror, "src", "contrib")
  dir.create(contrib, recursive = TRUE)
  best <- tapply(
    vapply(specs, `[[`, "", "version"), vapply(specs, `[[`, "", "name"),
    function(v) v[order(numeric_version(v), decreasing = TRUE)][1L]
  )
  for (s in specs) {
    root <- tempfile("mirrorsrc")
    build_test_src(s$name, s$version, root = root)
    dest <- if (identical(best[[s$name]], s$version)) contrib else {
      d <- file.path(contrib, "Archive", s$name)
      dir.create(d, recursive = TRUE, showWarnings = FALSE); d
    }
    withr::with_dir(root, utils::tar(
      file.path(dest, sprintf("%s_%s.tar.gz", s$name, s$version)),
      files = s$name, compression = "gzip", tar = "internal"
    ))
    unlink(root, recursive = TRUE)
  }
  tools::write_PACKAGES(contrib, type = "source", verbose = FALSE)
  paste0("file://", normalizePath(mirror, winslash = "/"))
}

# Minimal fake installed.packages() matrix for pure unit tests.
fake_ip <- function(...) {
  rows <- list(...)
  cols <- c("Package", "Version", "LibPath", "Priority", "Depends", "Imports", "LinkingTo")
  m <- do.call(rbind, lapply(rows, function(r) {
    v <- stats::setNames(rep(NA_character_, length(cols)), cols)
    v[names(r)] <- unlist(r)
    v
  }))
  rownames(m) <- m[, "Package"]
  m
}
