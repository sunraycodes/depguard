test_that("dep_manifest requires named arguments", {
  tmp <- tempfile(fileext = ".rds")
  expect_error(dep_manifest(path = tmp), "at least one")
  expect_error(dep_manifest("1.0", path = tmp), "must be named")
})

test_that("dep_manifest writes and dep_manifest_read reads back the same data", {
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp))

  m <- suppressMessages(dep_manifest(dplyr = "1.1.4", ggplot2 = "3.5.0", path = tmp))
  expect_equal(unname(m), c("1.1.4", "3.5.0"))
  expect_equal(names(m), c("dplyr", "ggplot2"))
  expect_equal(dep_manifest_read(path = tmp), m)
})

test_that("dep_manifest_read returns NULL when no manifest exists", {
  tmp <- tempfile(fileext = ".rds")
  expect_null(suppressMessages(dep_manifest_read(path = tmp)))
})

test_that("operators are accepted and invalid requirements rejected up front", {
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp))
  m <- suppressMessages(dep_manifest(a = ">= 1.0", b = "== 2.0", c = "< 3", d = NA, path = tmp))
  expect_equal(unname(m[1:3]), c(">= 1.0", "== 2.0", "< 3"))
  expect_error(dep_manifest(a = "newest", path = tmp), "Invalid requirement for")
  expect_error(dep_manifest(a = ">>1", path = tmp), "Invalid requirement")
})

test_that("text manifests round-trip and are human readable", {
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp))
  suppressMessages(dep_manifest(dplyr = "1.1.4", cli = "== 3.6.2", rlang = NA, path = tmp))

  lines <- readLines(tmp)
  expect_true("dplyr (>= 1.1.4)" %in% lines)
  expect_true("cli (== 3.6.2)" %in% lines)
  expect_true("rlang" %in% lines)

  m <- dep_manifest_read(tmp)
  expect_equal(names(m), c("dplyr", "cli", "rlang"))
  expect_equal(unname(m), c(">= 1.1.4", "== 3.6.2", NA))
})

test_that("hand-written text manifests tolerate comments, blanks and styles", {
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp))
  writeLines(c("# my notebook", "", "dplyr>=1.1.4  # keep", "  cli == 3.6.2", "rlang"), tmp)
  m <- dep_manifest_read(tmp)
  expect_equal(names(m), c("dplyr", "cli", "rlang"))
  expect_equal(unname(m), c(">=1.1.4", "== 3.6.2", NA))
})

test_that("malformed or empty text manifests give clear errors", {
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp))
  writeLines("this is {not} valid", tmp)
  expect_error(dep_manifest_read(tmp), "Could not parse")
  writeLines("# nothing", tmp)
  expect_error(dep_manifest_read(tmp), "no packages")
})

test_that("dep_manifest_freeze pins installed versions exactly by default", {
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp))
  m <- suppressMessages(dep_manifest_freeze(c("cli", "utils"), path = tmp))
  expect_equal(names(m), c("cli", "utils"))
  expect_true(all(grepl("^==", m)))
  expect_equal(m[["cli"]], paste0("==", as.character(utils::packageVersion("cli"))))

  m2 <- suppressMessages(dep_manifest_freeze("cli", exact = FALSE, path = tmp))
  expect_false(grepl("^==", m2[["cli"]]))
})

test_that("dep_manifest_freeze defaults to attached non-base packages", {
  lib <- local_test_lib()
  skip_on_cran()
  install_test_pkg(lib, "dgattached", "1.2.3")
  suppressPackageStartupMessages(library("dgattached", character.only = TRUE))
  withr::defer(detach("package:dgattached", unload = TRUE, character.only = TRUE))

  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp))
  m <- suppressMessages(dep_manifest_freeze(path = tmp))
  expect_equal(m[["dgattached"]], "==1.2.3")
  expect_false("stats" %in% names(m))
  expect_false("depguard" %in% names(m))
})

test_that("dep_manifest_freeze errors for uninstalled packages", {
  expect_error(dep_manifest_freeze("notInstalledXYZ", path = tempfile()), "Not installed")
})

test_that("manifest path can be set through an option", {
  tmp <- tempfile(fileext = ".rds")
  withr::local_options(depguard.manifest_path = tmp)
  expect_equal(default_manifest_path(), tmp)
  suppressMessages(dep_manifest(cli = "1.0.0"))
  expect_true(file.exists(tmp))
  unlink(tmp)
})
