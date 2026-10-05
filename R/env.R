# Environment detection ----------------------------------------------------

env_set <- function(x) !is.na(Sys.getenv(x, unset = NA_character_))

# Which kind of R environment are we in? Override with
# options(depguard.platform = "kaggle") (mainly useful for testing).
detect_platform <- function() {
  opt <- getOption("depguard.platform")
  if (!is.null(opt)) return(opt)
  if (env_set("KAGGLE_KERNEL_RUN_TYPE") || env_set("KAGGLE_URL_BASE")) {
    "kaggle"
  } else if (env_set("COLAB_RELEASE_TAG") || env_set("COLAB_GPU") ||
             env_set("COLAB_BACKEND_VERSION")) {
    "colab"
  } else if (env_set("BINDER_SERVICE_HOST") || env_set("BINDER_REQUEST")) {
    "binder"
  } else if (identical(Sys.getenv("RSTUDIO"), "1")) {
    "rstudio"
  } else {
    "desktop"
  }
}

is_hosted_notebook <- function(platform = detect_platform()) {
  platform %in% c("kaggle", "colab", "binder")
}

restart_hint <- function(platform = detect_platform()) {
  switch(platform,
    kaggle  = "Restart the notebook session (Kaggle 'Run' menu), then re-run your library() calls.",
    colab   = "Restart the runtime (Colab: Runtime > Restart session), then re-run your library() calls.",
    binder  = "Restart the kernel, then re-run your library() calls.",
    rstudio = "Restart R (RStudio: Session > Restart R), then re-run your library() calls.",
    "Restart R, then re-run your library() calls."
  )
}

is_online <- function(url = "https://cloud.r-project.org", timeout = 3) {
  tryCatch({
    args <- list(url)
    if ("timeout" %in% names(formals(curlGetHeaders))) args$timeout <- timeout
    suppressWarnings(do.call(curlGetHeaders, args))
    TRUE
  }, error = function(e) FALSE)
}

#' Describe the current R environment
#'
#' Detects whether R is running in a hosted notebook (Kaggle, Colab, Binder),
#' RStudio, or a plain desktop/server session, and reports which package
#' libraries exist and which are writable. `depguard` uses this to tailor
#' its advice (for example, how to restart the session after an install).
#'
#' Detection looks at environment variables only and makes no network calls
#' unless `check_online = TRUE`. Set `options(depguard.platform = "kaggle")`
#' (or `"colab"`, `"binder"`, `"rstudio"`, `"desktop"`) to override it.
#'
#' @param check_online Logical; if `TRUE`, make a short (3 second timeout)
#'   request to CRAN to see whether the internet is reachable.
#'
#' @return An object of class `depguard_env`, a list with elements
#'   `platform`, `os`, `r_version`, `interactive`, `libs`, `writable_libs`,
#'   `work_dir` and `online` (`NA` unless `check_online = TRUE`).
#' @export
#'
#' @examples
#' dep_env()
dep_env <- function(check_online = FALSE) {
  libs <- .libPaths()
  writable <- vapply(libs, lib_is_writable, logical(1))
  structure(
    list(
      platform = detect_platform(),
      os = Sys.info()[["sysname"]],
      r_version = R.version.string,
      interactive = interactive(),
      libs = libs,
      writable_libs = libs[writable],
      work_dir = getwd(),
      online = if (check_online) is_online() else NA
    ),
    class = "depguard_env"
  )
}

#' @param x A `depguard_env` object.
#' @param ... Ignored.
#' @rdname dep_env
#' @export
print.depguard_env <- function(x, ...) {
  cli::cli_h3("depguard environment")
  cli::cli_text("Platform: {.field {x$platform}} ({x$os}); {x$r_version}")
  cli::cli_text("Library paths: {length(x$libs)} ({length(x$writable_libs)} writable)")
  if (length(x$writable_libs) == 0L) {
    cli::cli_alert_warning(
      "No writable library found: installs and {.fn dep_fix} will need a user library (see {.code ?.libPaths})."
    )
  }
  if (!is.na(x$online)) {
    cli::cli_text("CRAN reachable: {if (x$online) 'yes' else 'no'}")
  }
  invisible(x)
}
