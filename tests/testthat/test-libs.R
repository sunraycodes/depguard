test_that("dep_libraries finds nothing odd in a clean setup or returns a typed frame", {
  r <- suppressMessages(dep_libraries())
  expect_s3_class(r, "data.frame")
  expect_true(all(c("package", "active_library", "active_version",
                    "shadowed_library", "shadowed_version", "differs") %in% names(r)))
})

test_that("a package hidden by another library copy is detected, with the right winner", {
  skip_on_cran()
  old_lib <- withr::local_tempdir()
  withr::local_libpaths(old_lib, action = "prefix")
  install_test_pkg(old_lib, "dgshadow", "1.0")
  new_lib <- withr::local_tempdir()
  install_test_pkg(new_lib, "dgshadow", "2.0")
  # new_lib is *later* on the path, so the old copy wins and hides it.
  withr::local_libpaths(new_lib, action = "suffix")

  r <- suppressMessages(dep_libraries())
  row <- r[r$package == "dgshadow", ]
  expect_equal(nrow(row), 1L)
  expect_equal(row$active_version, "1.0")
  expect_equal(row$shadowed_version, "2.0")
  expect_true(row$differs)
  expect_equal(normalizePath(row$active_library), normalizePath(old_lib))
  expect_message(dep_libraries(), "dgshadow")
})

test_that("identical copies in two libraries are not flagged as a conflict", {
  skip_on_cran()
  a <- withr::local_tempdir(); b <- withr::local_tempdir()
  withr::local_libpaths(c(a, b), action = "prefix")
  install_test_pkg(a, "dgsame", "1.0")
  install_test_pkg(b, "dgsame", "1.0")
  r <- suppressMessages(dep_libraries())
  expect_false(r$differs[r$package == "dgsame"])
})
