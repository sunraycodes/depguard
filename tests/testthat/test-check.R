test_that("dep_check flags a missing package correctly", {
  manifest <- c(thisPackageDoesNotExistXYZ = "1.0.0")
  result <- suppressMessages(dep_check(manifest = manifest, recursive = FALSE))
  expect_equal(result$status[result$package == "thisPackageDoesNotExistXYZ"], "missing")
  expect_equal(result$installed, "not installed")
})

test_that("dep_check flags an installed package as ok when no version required", {
  manifest <- c(base = NA_character_)
  result <- suppressMessages(dep_check(manifest = manifest, recursive = FALSE))
  expect_true("base" %in% result$package)
  expect_equal(result$status, "ok")
})

test_that("dep_check returns expected columns and class", {
  result <- suppressMessages(dep_check(manifest = c(tools = "0.1.0"), recursive = FALSE))
  expect_s3_class(result, "depguard_check")
  expect_s3_class(result, "data.frame")
  expect_true(all(c("package", "required", "installed", "status", "depth",
                    "required_by", "loaded", "reason") %in% names(result)))
  expect_equal(names(result)[1:6],
               c("package", "required", "installed", "status", "depth", "required_by"))
})

test_that("check_one_package detects version mismatch (backward compatible)", {
  row <- check_one_package("base", "999.999.999", "direct", NA_character_)
  expect_equal(row$status, "mismatch")
  expect_match(row$reason, "does not satisfy")
})

test_that("manifest operators are enforced", {
  inst <- as.character(utils::packageVersion("cli"))
  chk <- function(req) {
    suppressMessages(dep_check(manifest = c(cli = req), recursive = FALSE))$status
  }
  expect_equal(chk(paste0("==", inst)), "ok")
  expect_equal(chk(paste0(">=", inst)), "ok")
  expect_equal(chk("== 0.0.1"), "mismatch")
  expect_equal(chk("< 0.0.1"), "mismatch")
  expect_equal(chk("> 0.0.1"), "ok")
  expect_equal(chk(paste0("!=", inst)), "mismatch")
})

test_that("check_rows reports 'restart' when disk is fine but the session is stale", {
  ip <- fake_ip(list(Package = "pkgx", Version = "1.5"))
  r <- check_rows("pkgx", ">= 1.2", "direct", NA, ip, loaded = c(pkgx = "1.0"))
  expect_equal(r$status, "restart")
  expect_match(r$reason, "still runs 1.0")

  r2 <- check_rows("pkgx", ">= 1.2", "direct", NA, ip, loaded = c(pkgx = "1.3"))
  expect_equal(r2$status, "ok")

  r3 <- check_rows("pkgx", NA, "direct", NA, ip, loaded = c(pkgx = "1.0"))
  expect_equal(r3$status, "ok")   # no constraint, nothing to restart for
})

test_that("stop_on_problem turns problems into errors", {
  expect_error(
    suppressMessages(dep_check(manifest = c(notInstalledXYZ = "1.0"),
                               recursive = FALSE, stop_on_problem = TRUE)),
    "not satisfied"
  )
  expect_no_error(
    suppressMessages(dep_check(manifest = c(cli = ""), recursive = FALSE,
                               stop_on_problem = TRUE))
  )
})

test_that("dep_check reads the manifest from path when none is supplied", {
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp))
  writeLines("cli", tmp)
  r <- suppressMessages(dep_check(path = tmp, recursive = FALSE))
  expect_equal(r$package, "cli")
  expect_null(suppressMessages(dep_check(path = tempfile())))
})

test_that("an invalid manifest is rejected", {
  expect_error(dep_check(manifest = c(cli = "bogus")), "Invalid requirement")
  expect_error(dep_check(manifest = c("1.0")), "named character")
})

# --- dependency graph (pure) -------------------------------------------------

test_that("dependency_edges follows transitive constraints and skips base packages", {
  ip <- fake_ip(
    list(Package = "top", Version = "1.0", Imports = "mid (>= 2.0), utils"),
    list(Package = "mid", Version = "1.0", Imports = "leaf", Depends = "R (>= 3.5)"),
    list(Package = "leaf", Version = "0.5"),
    list(Package = "utils", Version = "4.3", Priority = "base")
  )
  e <- dependency_edges("top", ip)
  expect_equal(nrow(e), 2L)
  expect_true(any(e$parent == "top" & e$package == "mid" & e$requirement == ">= 2.0"))
  expect_true(any(e$parent == "mid" & e$package == "leaf" & is.na(e$requirement)))
  expect_false("utils" %in% e$package)
  expect_false("R" %in% e$package)
})

test_that("dependency_edges copes with missing and dependency-free packages", {
  ip <- fake_ip(list(Package = "lonely", Version = "1.0"))
  expect_equal(nrow(dependency_edges("lonely", ip)), 0L)
  expect_equal(nrow(dependency_edges("absent", ip)), 0L)
})

# --- integration: real conflicts in a real (temporary) library -----------------

test_that("a transitive constraint violation is caught, fully offline", {
  skip_on_cran()
  lib <- local_test_lib()
  install_test_pkg(lib, "dgdepb", "2.0")
  install_test_pkg(lib, "dgdepa", "1.0", imports = "dgdepb (>= 2.0)")

  # Healthy at first.
  r <- suppressMessages(dep_check(manifest = c(dgdepa = "1.0")))
  expect_true(all(r$status == "ok"))
  expect_true("transitive" %in% r$depth)

  # Something downgrades dgdepb behind our back.
  install_test_pkg(lib, "dgdepb", "1.0")

  # Make any attempt to hit the network fail loudly.
  withr::local_options(repos = c(CRAN = "http://127.0.0.1:1"), timeout = 1)
  expect_no_warning(
    r <- suppressMessages(dep_check(manifest = c(dgdepa = "1.0")))
  )
  bad <- r[r$status != "ok", ]
  expect_equal(bad$package, "dgdepb")
  expect_equal(bad$status, "mismatch")
  expect_equal(bad$required, ">= 2.0")
  expect_equal(bad$installed, "1.0")
  expect_equal(bad$required_by, "dgdepa")
  expect_equal(bad$depth, "transitive")

  expect_message(dep_check(manifest = c(dgdepa = "1.0")), "required by dgdepa")
  expect_error(
    suppressMessages(dep_check(manifest = c(dgdepa = "1.0"), stop_on_problem = TRUE)),
    "not satisfied"
  )
})

test_that("recursive = FALSE skips the transitive walk", {
  skip_on_cran()
  lib <- local_test_lib()
  install_test_pkg(lib, "dgdepb", "1.0")
  install_test_pkg(lib, "dgdepa", "1.0", imports = "dgdepb (>= 2.0)")
  r <- suppressMessages(dep_check(manifest = c(dgdepa = "1.0"), recursive = FALSE))
  expect_equal(nrow(r), 1L)
  expect_equal(r$status, "ok")
})

test_that("a manifest package that is also a constrained dependency is checked both ways", {
  skip_on_cran()
  lib <- local_test_lib()
  install_test_pkg(lib, "dgdepb", "1.0")
  install_test_pkg(lib, "dgdepa", "1.0", imports = "dgdepb (>= 2.0)")
  r <- suppressMessages(dep_check(manifest = c(dgdepa = "1.0", dgdepb = "1.0")))
  expect_equal(sum(r$package == "dgdepb"), 2L)
  expect_equal(r$status[r$package == "dgdepb" & r$depth == "direct"], "ok")
  expect_equal(r$status[r$package == "dgdepb" & r$depth == "transitive"], "mismatch")
})

test_that("a stale loaded dependency yields a 'restart' row", {
  skip_on_cran()
  lib <- local_test_lib()
  install_test_pkg(lib, "dgdepb", "1.0")
  install_test_pkg(lib, "dgdepa", "1.0", imports = "dgdepb (>= 2.0)")
  loadNamespace("dgdepb")
  withr::defer(try(unloadNamespace("dgdepb"), silent = TRUE))
  install_test_pkg(lib, "dgdepb", "2.0")     # fixed on disk, session still runs 1.0

  r <- suppressMessages(dep_check(manifest = c(dgdepa = "1.0")))
  expect_equal(r$status[r$package == "dgdepb"], "restart")
  expect_message(dep_check(manifest = c(dgdepa = "1.0")), "restart|Restart")
})

test_that("printing shows problems only, or everything with all = TRUE", {
  r <- suppressMessages(dep_check(manifest = c(cli = "", notInstalledXYZ = "1.0"),
                                  recursive = FALSE))
  expect_output(print(r), "notInstalledXYZ")
  problems_only <- capture.output(print(r))
  expect_false(any(grepl("\\bcli\\b", problems_only)))   # the ok row is hidden
  everything <- capture.output(print(r, all = TRUE))
  expect_true(any(grepl("\\bcli\\b", everything)))
})

# --- check_cran ----------------------------------------------------------------

test_that("check_cran degrades gracefully when CRAN is unreachable", {
  withr::local_options(repos = c(CRAN = "file:///definitely/not/here"))
  expect_message(
    r <- dep_check(manifest = c(cli = ""), recursive = FALSE, check_cran = TRUE),
    "Could not reach CRAN"
  )
  expect_false("cran_latest" %in% names(r))
  expect_equal(r$status, "ok")
})

test_that("check_cran adds latest versions and an outdated flag from a repository", {
  skip_on_cran()
  skip_on_os("windows")
  mirror <- make_test_mirror(list(
    list(name = "dgrepo", version = "1.0"), list(name = "dgrepo", version = "2.0")
  ))
  withr::local_options(repos = c(CRAN = mirror))
  lib <- local_test_lib()
  install_test_pkg(lib, "dgrepo", "1.0")

  r <- suppressMessages(dep_check(manifest = c(dgrepo = ""), recursive = FALSE, check_cran = TRUE))
  expect_equal(r$cran_latest, "2.0")
  expect_true(r$outdated)
})
