test_that("dep_manifest requires named arguments", {
  tmp <- tempfile(fileext = ".rds")
  expect_error(dep_manifest(path = tmp), "at least one")
})

test_that("dep_manifest writes and dep_manifest_read reads back the same data", {
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp))

  m <- dep_manifest(dplyr = "1.1.4", ggplot2 = "3.5.0", path = tmp)
  expect_equal(unname(m), c("1.1.4", "3.5.0"))
  expect_equal(names(m), c("dplyr", "ggplot2"))

  m2 <- dep_manifest_read(path = tmp)
  expect_equal(m, m2)
})

test_that("dep_manifest_read returns NULL when no manifest exists", {
  tmp <- tempfile(fileext = ".rds")
  expect_null(dep_manifest_read(path = tmp))
})
