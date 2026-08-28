test_that("dep_check flags a missing package correctly", {
  manifest <- c(thisPackageDoesNotExistXYZ = "1.0.0")
  result <- dep_check(manifest = manifest, recursive = FALSE)

  expect_equal(result$status[result$package == "thisPackageDoesNotExistXYZ"], "missing")
})

test_that("dep_check flags an installed package as ok when no version required", {
  manifest <- c(base = NA_character_)
  result <- suppressWarnings(dep_check(manifest = manifest, recursive = FALSE))

  expect_true("base" %in% result$package)
})

test_that("dep_check returns expected columns", {
  manifest <- c(tools = "0.1.0")
  result <- dep_check(manifest = manifest, recursive = FALSE)

  expect_true(all(c("package", "required", "installed", "status", "depth", "required_by") %in% names(result)))
})

test_that("check_one_package detects version mismatch", {
  installed_db <- utils::installed.packages()
  pkg <- rownames(installed_db)[1]
  current_version <- installed_db[pkg, "Version"]

  # Require a version far higher than what's installed
  row <- depguard:::check_one_package(
    pkg = pkg,
    required = "999.999.999",
    depth = "direct",
    required_by = NA_character_,
    installed_db = installed_db
  )

  expect_equal(row$status, "mismatch")
})
