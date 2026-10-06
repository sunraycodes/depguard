test_that("resolve_fix_method picks the first available backend", {
  testthat::local_mocked_bindings(has_pkg = function(pkg) pkg %in% c("remotes"))
  expect_equal(resolve_fix_method("auto"), "remotes")
  expect_equal(resolve_fix_method("pak"), "remotes")      # pak missing -> falls through
  expect_equal(resolve_fix_method("remotes"), "remotes")
  expect_equal(resolve_fix_method("archive"), "archive")

  testthat::local_mocked_bindings(has_pkg = function(pkg) TRUE)
  expect_equal(resolve_fix_method("auto"), "pak")

  testthat::local_mocked_bindings(has_pkg = function(pkg) FALSE)
  expect_equal(resolve_fix_method("auto"), "archive")
  expect_equal(resolve_fix_method("remotes"), "archive")
})

test_that("archive_urls checks the archive first, then current sources", {
  u <- archive_urls("foo", "1.2.3", mirror = "https://example.org/cran")
  expect_equal(u[1], "https://example.org/cran/src/contrib/Archive/foo/foo_1.2.3.tar.gz")
  expect_equal(u[2], "https://example.org/cran/src/contrib/foo_1.2.3.tar.gz")
})

test_that("dep_fix validates its inputs", {
  expect_error(dep_fix(c("a", "b"), "1.0"), "single package")
  expect_error(dep_fix("a", ">= 1.0"), "plain version")
  expect_error(dep_fix("a", "latest"), "Invalid version requirement")
})

test_that("dep_fix explains when there is no writable library", {
  expect_error(
    suppressMessages(dep_fix("cli", "1.0.0", lib = file.path(tempdir(), "nope"), method = "archive")),
    "No writable package library"
  )
})

test_that("dry_run reports a plan and installs nothing", {
  lib <- local_test_lib()
  install_test_pkg(lib, "dgfix", "2.0")
  plan <- suppressMessages(dep_fix("dgfix", "1.0", method = "archive", lib = lib, dry_run = TRUE))
  expect_equal(plan$from, "2.0")
  expect_equal(plan$to, "1.0")
  expect_equal(plan$method, "archive")
  expect_equal(unname(utils::installed.packages(lib.loc = lib)["dgfix", "Version"]), "2.0")
  expect_message(dep_fix("dgfix", "1.0", method = "archive", lib = lib, dry_run = TRUE), "Dry run")
})

test_that("already-at-version is a no-op", {
  lib <- local_test_lib()
  install_test_pkg(lib, "dgfix", "1.0")
  expect_message(
    res <- dep_fix("dgfix", "1.0", method = "archive", lib = lib),
    "already at 1.0"
  )
  expect_true(res)
})

test_that("archive rollback installs the requested version end to end, offline", {
  skip_on_cran()
  skip_on_os("windows")
  mirror <- make_test_mirror(list(
    list(name = "dgroll", version = "1.0"),
    list(name = "dgroll", version = "1.5"),
    list(name = "dgroll", version = "2.0")
  ))
  withr::local_options(depguard.cran_mirror = mirror)
  lib <- local_test_lib()
  install_test_pkg(lib, "dgroll", "2.0")

  expect_message(res <- dep_fix("dgroll", "1.5", method = "archive", lib = lib),
                 "rolled back to 1.5")
  expect_true(res)
  expect_equal(unname(utils::installed.packages(lib.loc = lib)["dgroll", "Version"]), "1.5")

  # The current (non-archived) release is reachable too.
  expect_true(suppressMessages(dep_fix("dgroll", "2.0", method = "archive", lib = lib)))
  expect_equal(unname(utils::installed.packages(lib.loc = lib)["dgroll", "Version"]), "2.0")

  # And a package that was not installed at all can be installed at a version.
  expect_true(suppressMessages(dep_fix("dgroll", "1.0", method = "archive", lib = lib)))
})

test_that("a missing version gives a clear download error", {
  skip_on_os("windows")
  mirror <- make_test_mirror(list(list(name = "dgroll", version = "1.0")))
  withr::local_options(depguard.cran_mirror = mirror)
  lib <- local_test_lib()
  expect_error(
    suppressMessages(dep_fix("dgroll", "9.9", method = "archive", lib = lib)),
    "Could not download"
  )
})

test_that("rolling back a loaded package warns that a restart is needed", {
  skip_on_cran()
  skip_on_os("windows")
  mirror <- make_test_mirror(list(
    list(name = "dgroll", version = "1.0"), list(name = "dgroll", version = "2.0")
  ))
  withr::local_options(depguard.cran_mirror = mirror)
  local_clean_platform()
  withr::local_envvar(COLAB_RELEASE_TAG = "r1")
  lib <- local_test_lib()
  install_test_pkg(lib, "dgroll", "2.0")
  loadNamespace("dgroll")
  withr::defer(try(unloadNamespace("dgroll"), silent = TRUE))

  expect_message(dep_fix("dgroll", "1.0", method = "archive", lib = lib), "Runtime")
})

test_that("a rollback hidden by an earlier library is reported, not claimed as success", {
  skip_on_cran()
  skip_on_os("windows")
  mirror <- make_test_mirror(list(
    list(name = "dgroll", version = "1.0"), list(name = "dgroll", version = "2.0")
  ))
  withr::local_options(depguard.cran_mirror = mirror)
  front <- withr::local_tempdir()
  back <- withr::local_tempdir()
  withr::local_libpaths(c(front, back), action = "prefix")
  install_test_pkg(front, "dgroll", "2.0")      # wins on the path

  expect_message(dep_fix("dgroll", "1.0", method = "archive", lib = back),
                 "still load 2.0")
})

test_that("auto falls back to the archive when the preferred backend fails", {
  lib <- local_test_lib()
  calls <- character()
  testthat::local_mocked_bindings(
    has_pkg = function(pkg) TRUE,
    install_with = function(backend, package, version, lib) {
      calls <<- c(calls, backend)
      stop("simulated network failure\nsecond line")
    },
    install_from_archive = function(package, version, lib) {
      calls <<- c(calls, "archive")
      install_test_pkg(lib, package, version)
    }
  )
  expect_message(
    suppressWarnings(dep_fix("dgfallback", "1.0", lib = lib)),
    "simulated network failure"
  )
  expect_equal(calls, c("pak", "archive"))
  expect_equal(unname(utils::installed.packages(lib.loc = lib)["dgfallback", "Version"]), "1.0")
})

test_that("an explicitly chosen backend does not silently fall back", {
  lib <- local_test_lib()
  testthat::local_mocked_bindings(
    has_pkg = function(pkg) TRUE,
    install_with = function(backend, package, version, lib) stop("boom"),
    install_from_archive = function(...) stop("should not be called")
  )
  expect_error(suppressMessages(dep_fix("dgfallback", "1.0", method = "remotes", lib = lib)), "boom")
})

test_that("same_path ignores spelling differences (slashes, '..', trailing /)", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "a"))
  expect_true(same_path(file.path(d, "a"), file.path(d, "a", "..", "a")))
  expect_true(same_path(file.path(d, "a"), paste0(file.path(d, "a"), "/")))
  if (.Platform$OS.type == "windows") expect_true(same_path(d, chartr("/", "\\", d)))
  expect_false(same_path(file.path(d, "a"), d))
  expect_false(same_path(NA_character_, d))
})

test_that("already-at-version still short-circuits when the library is spelled differently", {
  lib <- local_test_lib()
  install_test_pkg(lib, "dgfix", "1.0")
  odd <- file.path(lib, "..", basename(lib))      # same directory, different spelling
  expect_message(
    res <- dep_fix("dgfix", "1.0", method = "archive", lib = odd),
    "already at 1.0"
  )
  expect_true(res)
})
