test_that("dep_snapshot returns a depguard_snapshot with disk and loaded versions", {
  snap <- dep_snapshot()
  expect_s3_class(snap, "depguard_snapshot")
  expect_true(all(c("package", "disk_version", "library", "loaded_version") %in%
                    names(snap$packages)))
  expect_true("cli" %in% snap$packages$package)
  expect_false(is.na(snap$packages$loaded_version[snap$packages$package == "cli"]))
  expect_message(print(snap), "snapshot")
})

test_that("scope = 'loaded' records only loaded packages", {
  all <- dep_snapshot("all")
  loaded <- dep_snapshot("loaded")
  expect_lt(nrow(loaded$packages), nrow(all$packages))
  expect_true(all(!is.na(loaded$packages$loaded_version)))
})

test_that("dep_diff reports no changes when nothing changed", {
  snap <- dep_snapshot()
  result <- suppressMessages(dep_diff(snap))
  expect_true(all(!result$changed))
  expect_true(all(result$risk == "none"))
  expect_true(all(result$change == "none"))
})

test_that("dep_diff errors on invalid or missing input", {
  expect_error(dep_diff(list(foo = "bar")), "dep_snapshot")
  expect_error(dep_diff("/nonexistent/snap.rds"), "does not exist")
  rm(list = ls(.state), envir = .state)
  expect_error(dep_diff(), "dep_snapshot")
})

test_that("dep_diff() with no argument uses the latest snapshot", {
  dep_snapshot()
  expect_s3_class(suppressMessages(dep_diff()), "data.frame")
})

test_that("snapshots saved to disk can be diffed later (e.g. after a restart)", {
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp))
  suppressMessages(dep_snapshot(path = tmp))
  expect_true(file.exists(tmp))
  rm(list = ls(.state), envir = .state)
  expect_s3_class(suppressMessages(dep_diff(tmp)), "data.frame")
})

# --- pure comparison logic --------------------------------------------------

pk <- function(package, disk, loaded = NA_character_) {
  data.frame(package = package, disk_version = disk, library = "/lib",
             loaded_version = loaded, stringsAsFactors = FALSE)
}

test_that("compute_diff flags a loaded package upgraded on disk as high risk", {
  before <- pk(c("a", "b"), c("1.0", "2.0"), c("1.0", NA))
  after  <- pk(c("a", "b"), c("1.1", "2.0"), c("1.0", NA))
  d <- compute_diff(before, after, c(a = "1.0"))

  expect_equal(d$package[1], "a")
  expect_equal(d$change[d$package == "a"], "upgraded")
  expect_equal(d$risk[d$package == "a"], "high")
  expect_true(d$session_stale[d$package == "a"])
  expect_equal(d$risk[d$package == "b"], "none")
})

test_that("compute_diff handles downgrade, added, removed, and unloaded changes", {
  before <- pk(c("down", "gone", "quiet"), c("2.0", "1.0", "1.0"))
  after  <- pk(c("down", "new", "quiet"), c("1.0", "0.1", "1.1"))
  d <- compute_diff(before, after, loaded = c(down = "2.0", gone = "1.0"))
  g <- function(p, col) d[[col]][d$package == p]

  expect_equal(g("down", "change"), "downgraded")
  expect_equal(g("down", "risk"), "high")
  expect_equal(g("new", "change"), "added")
  expect_equal(g("new", "risk"), "low")
  expect_equal(g("gone", "change"), "removed")
  expect_equal(g("gone", "risk"), "high")        # removed but still loaded
  expect_equal(g("quiet", "change"), "upgraded")
  expect_equal(g("quiet", "risk"), "low")        # not loaded: low
})

test_that("a loaded package that is stale vs disk is high risk even if unchanged", {
  before <- pk("a", "1.1", "1.0")
  after  <- pk("a", "1.1", "1.0")
  d <- compute_diff(before, after, c(a = "1.0"))
  expect_equal(d$change, "none")
  expect_true(d$session_stale)
  expect_equal(d$risk, "high")
})

test_that("results are ordered high, low, none", {
  before <- pk(c("z", "m", "a"), c("1", "1", "1"))
  after  <- pk(c("z", "m", "a"), c("2", "1", "2"))
  d <- compute_diff(before, after, c(a = "1"))
  expect_equal(d$risk, c("high", "low", "none"))
})

# --- regression test for the original v0.1.0 bug -------------------------------

test_that("an upgrade of a LOADED package on disk is detected (v0.1.0 missed it)", {
  skip_on_cran()
  lib <- local_test_lib()
  install_test_pkg(lib, "dgloaded", "1.0")
  loadNamespace("dgloaded")
  withr::defer(try(unloadNamespace("dgloaded"), silent = TRUE))

  snap <- dep_snapshot()
  install_test_pkg(lib, "dgloaded", "1.1")          # upgrade behind R's back

  expect_equal(as.character(getNamespaceVersion("dgloaded")), "1.0")  # still running 1.0
  d <- suppressMessages(dep_diff(snap))
  row <- d[d$package == "dgloaded", ]
  expect_equal(row$before, "1.0")
  expect_equal(row$after, "1.1")
  expect_equal(row$loaded_version, "1.0")
  expect_true(row$session_stale)
  expect_equal(row$risk, "high")
  expect_message(dep_diff(snap), "dgloaded")
})

test_that("a newly installed package shows up as added when scope = 'all'", {
  skip_on_cran()
  lib <- local_test_lib()
  snap <- dep_snapshot("all")
  install_test_pkg(lib, "dgnewpkg", "0.3")
  d <- suppressMessages(dep_diff(snap))
  expect_equal(d$change[d$package == "dgnewpkg"], "added")
})

test_that("the restart advice follows the platform", {
  skip_on_cran()
  local_clean_platform()
  withr::local_envvar(KAGGLE_KERNEL_RUN_TYPE = "Interactive")
  lib <- local_test_lib()
  install_test_pkg(lib, "dgkaggle", "1.0")
  loadNamespace("dgkaggle")
  withr::defer(try(unloadNamespace("dgkaggle"), silent = TRUE))
  snap <- dep_snapshot()
  install_test_pkg(lib, "dgkaggle", "2.0")
  expect_message(dep_diff(snap), "Kaggle")
})
