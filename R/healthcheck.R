#' Run a one-shot dependency health check
#'
#' Convenience entry point intended to be run at the top of a notebook or
#' script. It reports the environment (hosted notebook or desktop, writable
#' libraries), warns about packages shadowed by another library, and then
#' either checks the environment against your manifest (see
#' [dep_manifest()]) or, if there is none, captures a baseline snapshot you
#' can compare against later with [dep_diff()].
#'
#' @inheritParams dep_check
#' @param libraries Logical; also run [dep_libraries()] to look for shadowed
#'   packages (default `TRUE`).
#'
#' @return Invisibly, either the result of [dep_check()] (if a manifest
#'   exists) or a `depguard_snapshot` object (if not).
#' @export
#'
#' @examples
#' dep_healthcheck()
dep_healthcheck <- function(path = default_manifest_path(),
                            check_cran = FALSE,
                            libraries = TRUE) {
  cli::cli_h2("depguard health check")
  print(dep_env())
  if (libraries) dep_libraries()

  if (file.exists(path)) {
    cli::cli_h3("Checking environment against manifest")
    return(dep_check(path = path, check_cran = check_cran))
  }

  cli::cli_h3("No manifest found: capturing a baseline snapshot")
  cli::cli_alert_info(
    "Tip: use {.fn dep_manifest} or {.fn dep_manifest_freeze} to declare required package versions for stronger checks next time."
  )
  snap <- dep_snapshot()
  cli::cli_alert_success(
    "Snapshot captured ({nrow(snap$packages)} installed packages). Call {.fn dep_diff} after installing something to see what changed."
  )
  invisible(snap)
}
