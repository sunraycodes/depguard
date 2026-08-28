test_that("dep_snapshot returns a depguard_snapshot object", {
  snap <- dep_snapshot()
  expect_s3_class(snap, "depguard_snapshot")
  expect_true("packages" %in% names(snap))
})

test_that("dep_diff reports no changes when nothing changed", {
  snap <- dep_snapshot()
  result <- dep_diff(snap)

  expect_true(all(!result$changed))
  expect_true(all(result$risk == "none"))
})

test_that("dep_diff errors on invalid snapshot input", {
  expect_error(dep_diff(list(foo = "bar")), "dep_snapshot")
})
