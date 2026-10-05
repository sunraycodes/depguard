# Version-requirement parsing and comparison -------------------------------

# Parse one requirement such as "1.2.3" (minimum), ">= 1.2", "== 1.2.3",
# "< 2.0", NA or "" (any). Returns list(op, version).
parse_requirement <- function(x) {
  if (length(x) != 1L) {
    cli::cli_abort("A version requirement must be a single value.")
  }
  if (is.na(x)) {
    return(list(op = "any", version = NA_character_))
  }
  x <- trimws(as.character(x))
  if (!nzchar(x)) {
    return(list(op = "any", version = NA_character_))
  }
  m <- regmatches(
    x, regexec("^(>=|<=|==|!=|>|<|=)?\\s*([0-9][0-9A-Za-z._-]*)$", x)
  )[[1L]]
  if (length(m) == 0L) {
    cli::cli_abort(c(
      "Invalid version requirement {.val {x}}.",
      "i" = "Use a version like '1.2.3' (minimum), or an operator: '>= 1.2', '== 1.2.3', '< 2.0'."
    ))
  }
  op <- if (m[2L] == "") ">=" else m[2L]
  if (op == "=") op <- "=="
  list(op = op, version = m[3L])
}

requirement_label <- function(req) {
  if (req$op == "any") "any" else paste(req$op, req$version)
}

# Safe wrapper: NA when either side is missing / unparseable.
ver_cmp <- function(a, b) {
  if (is.na(a) || is.na(b)) return(NA_integer_)
  tryCatch(utils::compareVersion(a, b), error = function(e) NA_integer_)
}

# Does `installed` satisfy requirement `req`? NA installed -> FALSE.
version_satisfies <- function(installed, req) {
  if (req$op == "any") return(!is.na(installed))
  cmp <- ver_cmp(installed, req$version)
  if (is.na(cmp)) return(FALSE)
  switch(req$op,
    ">=" = cmp >= 0L, ">" = cmp > 0L,
    "<=" = cmp <= 0L, "<" = cmp < 0L,
    "==" = cmp == 0L, "!=" = cmp != 0L,
    FALSE
  )
}

# Parse "pkg", "pkg (>= 1.0)", "pkg>=1.0", "pkg == 1.0" -> list(package, requirement)
parse_spec <- function(x) {
  x <- trimws(x)
  m <- regmatches(
    x,
    regexec("^([A-Za-z][A-Za-z0-9.]*)\\s*\\(?\\s*((?:>=|<=|==|!=|>|<|=)?\\s*[0-9][0-9A-Za-z._-]*)?\\s*\\)?$",
            x, perl = TRUE)
  )[[1L]]
  if (length(m) == 0L) return(NULL)
  list(package = m[2L], requirement = if (nzchar(m[3L])) m[3L] else NA_character_)
}

# Parse a DESCRIPTION dependency field ("cli (>= 3.0), utils") into a
# data frame, dropping R itself.
parse_dep_field <- function(value) {
  empty <- data.frame(package = character(), requirement = character(),
                      stringsAsFactors = FALSE)
  if (is.null(value) || is.na(value) || !nzchar(value)) return(empty)
  parts <- trimws(strsplit(gsub("[\r\n]+", " ", value), ",")[[1L]])
  parts <- parts[nzchar(parts)]
  specs <- lapply(parts, parse_spec)
  specs <- specs[!vapply(specs, is.null, logical(1))]
  if (length(specs) == 0L) return(empty)
  out <- data.frame(
    package = vapply(specs, `[[`, character(1), "package"),
    requirement = vapply(specs, `[[`, character(1), "requirement"),
    stringsAsFactors = FALSE
  )
  out[out$package != "R", , drop = FALSE]
}
