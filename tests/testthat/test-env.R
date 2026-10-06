test_that("platform detection recognises hosted notebooks", {
  local_clean_platform()
  expect_equal(detect_platform(), "desktop")

  withr::local_envvar(KAGGLE_KERNEL_RUN_TYPE = "Interactive")
  expect_equal(detect_platform(), "kaggle")
})

test_that("colab, binder and rstudio are detected", {
  local_clean_platform()
  withr::local_envvar(COLAB_RELEASE_TAG = "release-123")
  expect_equal(detect_platform(), "colab")

  local_clean_platform()
  withr::local_envvar(BINDER_SERVICE_HOST = "10.0.0.1")
  expect_equal(detect_platform(), "binder")

  local_clean_platform()
  withr::local_envvar(RSTUDIO = "1")
  expect_equal(detect_platform(), "rstudio")
})

test_that("the platform option overrides detection", {
  local_clean_platform()
  withr::local_envvar(KAGGLE_KERNEL_RUN_TYPE = "Interactive")
  withr::local_options(depguard.platform = "colab")
  expect_equal(detect_platform(), "colab")
})

test_that("restart advice is platform specific", {
  expect_match(restart_hint("kaggle"), "Kaggle")
  expect_match(restart_hint("colab"), "Runtime")
  expect_match(restart_hint("rstudio"), "Restart R")
  expect_match(restart_hint("desktop"), "Restart R")
  expect_true(is_hosted_notebook("kaggle"))
  expect_false(is_hosted_notebook("desktop"))
})

test_that("dep_env describes the session without touching the network", {
  local_clean_platform()
  env <- dep_env()
  expect_s3_class(env, "depguard_env")
  expect_equal(env$platform, "desktop")
  expect_true(length(env$libs) >= 1L)
  expect_true(is.na(env$online))
  expect_message(print(env), "environment")
})

test_that("dep_env warns when no library is writable", {
  env <- dep_env()
  env$writable_libs <- character()
  expect_message(print(env), "No writable library")
})

test_that("first_writable_lib prefers the first writable path", {
  lib <- local_test_lib()
  expect_true(same_path(first_writable_lib(), lib))
})
