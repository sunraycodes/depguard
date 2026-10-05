test_that("healthcheck without a manifest returns a snapshot", {
  local_clean_platform()
  withr::local_options(depguard.manifest_path = tempfile(fileext = ".rds"))
  res <- NULL
  expect_message(res <- dep_healthcheck(), "No manifest found")
  expect_s3_class(res, "depguard_snapshot")
})

test_that("healthcheck with a manifest returns the check result", {
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp))
  writeLines("cli", tmp)
  res <- suppressMessages(dep_healthcheck(path = tmp))
  expect_s3_class(res, "depguard_check")
  expect_equal(res$package, "cli")
})

test_that("healthcheck reports the platform it is running on", {
  local_clean_platform()
  withr::local_envvar(KAGGLE_KERNEL_RUN_TYPE = "Interactive")
  expect_message(dep_healthcheck(path = tempfile()), "kaggle")
})

test_that("libraries = FALSE skips the shadowing scan", {
  local_clean_platform()
  msgs <- testthat::capture_messages(dep_healthcheck(path = tempfile(), libraries = FALSE))
  expect_false(any(grepl("shadowed", msgs)))
})
