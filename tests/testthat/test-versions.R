test_that("parse_requirement handles all forms", {
  expect_equal(parse_requirement("1.2.3"), list(op = ">=", version = "1.2.3"))
  expect_equal(parse_requirement(">= 1.2"), list(op = ">=", version = "1.2"))
  expect_equal(parse_requirement("==1.2.3"), list(op = "==", version = "1.2.3"))
  expect_equal(parse_requirement("= 1.2.3")$op, "==")
  expect_equal(parse_requirement("< 2.0")$op, "<")
  expect_equal(parse_requirement("!= 1.0")$op, "!=")
  expect_equal(parse_requirement(NA)$op, "any")
  expect_equal(parse_requirement("")$op, "any")
  expect_equal(parse_requirement("0.4-3")$version, "0.4-3")
})

test_that("parse_requirement rejects nonsense", {
  expect_error(parse_requirement("latest"), "Invalid version requirement")
  expect_error(parse_requirement(">=>1"), "Invalid version requirement")
  expect_error(parse_requirement(c("1", "2")), "single value")
})

test_that("version_satisfies applies each operator", {
  sat <- function(inst, req) version_satisfies(inst, parse_requirement(req))
  expect_true(sat("1.5.0", "1.5.0"))
  expect_true(sat("1.6.0", "1.5.0"))
  expect_false(sat("1.4.9", "1.5.0"))
  expect_true(sat("1.5.0", "== 1.5.0"))
  expect_false(sat("1.5.1", "== 1.5.0"))
  expect_true(sat("1.9", "< 2.0"))
  expect_false(sat("2.0", "< 2.0"))
  expect_true(sat("2.0", "<= 2.0"))
  expect_true(sat("2.1", "> 2.0"))
  expect_true(sat("2.1", "!= 2.0"))
  expect_true(sat("0.0.1", NA))
  expect_false(sat(NA_character_, NA))
  expect_true(sat("10.0", ">= 9.9"))  # numeric, not lexicographic
})

test_that("parse_dep_field extracts packages and constraints, dropping R", {
  d <- parse_dep_field("R (>= 3.5), cli (>= 3.4.0),\n    rlang, glue(>=1.0)")
  expect_equal(d$package, c("cli", "rlang", "glue"))
  expect_equal(d$requirement, c(">= 3.4.0", NA, ">=1.0"))
  expect_equal(nrow(parse_dep_field(NA_character_)), 0L)
  expect_equal(nrow(parse_dep_field("")), 0L)
})

test_that("parse_spec handles text-manifest line styles", {
  expect_equal(parse_spec("dplyr")$requirement, NA_character_)
  expect_equal(parse_spec("dplyr (>= 1.1.4)")$requirement, ">= 1.1.4")
  expect_equal(parse_spec("dplyr>=1.1.4")$requirement, ">=1.1.4")
  expect_equal(parse_spec("dplyr == 1.1.4")$requirement, "== 1.1.4")
  expect_null(parse_spec("not a valid line!"))
})
