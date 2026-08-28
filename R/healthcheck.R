#' Run a one-shot dependency health check
#'
#' Convenience entry point intended to be run at the top of a notebook
#' session (e.g. on Kaggle or Colab). If a manifest exists (see
#' [dep_manifest()]), checks the environment against it. Otherwise, falls
#' back to reporting a plain snapshot of currently loaded package versions
#' so you have a baseline to diff against later with [dep_diff()].
#'
#' @inheritParams dep_check
#'
#' @return Invisibly, either the result of [dep_check()] (if a manifest
#'   exists) or a `depguard_snapshot` object (if not).
#' @export
#'
#' @examples
#' \dontrun{
#' dep_healthcheck()
#' }
dep_healthcheck <- function(path = default_manifest_path(), check_cran = FALSE) {
  if (file.exists(path)) {
    cli::cli_h2("depguard: checking environment against manifest")
    return(dep_check(path = path, check_cran = check_cran))
  }

  cli::cli_h2("depguard: no manifest found, capturing baseline snapshot")
  cli::cli_alert_info(
    "Tip: use {.fn dep_manifest} to declare required package versions for \\
    stronger checks next time."
  )
  snap <- dep_snapshot()
  cli::cli_alert_success(
    "Snapshot captured ({nrow(snap$packages)} loaded packages). Save this \\
    object and pass it to {.fn dep_diff} later to detect drift."
  )
  invisible(snap)
}
