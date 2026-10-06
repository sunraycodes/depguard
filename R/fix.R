#' Roll back a single package to a specific version
#'
#' Reinstalls one package at a specific version, e.g. to undo a version that
#' was silently pulled in as a side effect of another install. This performs
#' a **single-package rollback only**: it does not resolve cascading
#' conflicts that the rollback might introduce with other packages. For full
#' dependency resolution, use `renv::restore()` or `pak`'s solver instead.
#'
#' Three backends are supported. With `method = "auto"` the first one
#' available is used: `pak`, then `remotes` (both in Suggests), then
#' `"archive"`, which needs nothing beyond base R: it downloads the source
#' tarball from the CRAN archive and installs it. (Installing from source
#' needs Rtools on Windows, and the Xcode command line tools on macOS if the
#' package contains compiled code; Kaggle/Colab-style Linux images normally
#' have what is needed.)
#'
#' With `method = "auto"`, if the chosen backend fails (for example because
#' the internet is flaky), `dep_fix()` falls back to the archive backend
#' before giving up. Set `options(depguard.cran_mirror = "<url>")` to use a
#' different CRAN mirror for the `remotes` and `archive` backends (`pak`
#' uses its own repository settings).
#'
#' After installing, the version on disk is verified. If the package is
#' loaded in the current session, R keeps running the old version until it
#' is restarted, and you are told so.
#'
#' @param package Name of the package to roll back.
#' @param version Target version string, e.g. `"1.5.0"`.
#' @param method Which backend to use: `"auto"` (default), `"pak"`,
#'   `"remotes"` or `"archive"`. If the requested backend is not available,
#'   the next one in that order is used.
#' @param lib Library to install into. Defaults to the first writable
#'   library on [.libPaths()].
#' @param dry_run Logical; if `TRUE`, show what would be done without
#'   installing anything.
#'
#' @return Invisibly, `TRUE` on success; with `dry_run = TRUE`, a list
#'   describing the plan.
#' @export
#'
#' @examples
#' \dontrun{
#' dep_fix("stringr", "1.5.0")
#' dep_fix("stringr", "1.5.0", dry_run = TRUE)
#' }
dep_fix <- function(package, version,
                    method = c("auto", "pak", "remotes", "archive"),
                    lib = NULL, dry_run = FALSE) {
  method <- match.arg(method)

  if (!is.character(package) || length(package) != 1L || is.na(package)) {
    cli::cli_abort("{.arg package} must be a single package name.")
  }
  req <- parse_requirement(version)
  if (req$op != ">=" || !grepl("^[0-9]", as.character(version))) {
    cli::cli_abort("{.arg version} must be a plain version such as {.val 1.5.0}, not an operator expression.")
  }
  version <- req$version

  lib <- lib %||% first_writable_lib()
  if (is.null(lib) || !lib_is_writable(lib)) {
    cli::cli_abort(c(
      "No writable package library available.",
      "i" = "Create one (e.g. {.code dir.create(Sys.getenv('R_LIBS_USER'), recursive = TRUE)}), add it with {.code .libPaths()}, or pass {.arg lib}."
    ))
  }

  ip <- active_installed()
  current <- ip_version(package, ip)
  backend <- resolve_fix_method(method)
  plan <- list(package = package, from = current, to = version,
               method = backend, lib = lib)

  cli::cli_alert_warning(
    "Rolling back a single package can break other packages that depend on the newer version. This does not perform cascading conflict resolution."
  )

  if (!is.na(current) && identical(current, version) &&
      same_path(ip[match(package, ip[, "Package"]), "LibPath"], lib)) {
    cli::cli_alert_success("{package} is already at {version}.")
    return(invisible(TRUE))
  }

  if (dry_run) {
    cli::cli_alert_info(
      "Dry run: would install {package} {version} (currently {current %||% 'not installed'}) into {.path {lib}} using {.field {backend}}."
    )
    return(invisible(plan))
  }

  result <- tryCatch(install_with(backend, package, version, lib), error = function(e) e)
  if (inherits(result, "error")) {
    if (method != "auto" || backend == "archive") stop(result)
    why <- strsplit(conditionMessage(result), "\n", fixed = TRUE)[[1L]][1L]
    cli::cli_alert_warning(
      "{backend} could not install {package} {version} ({why}). Falling back to the CRAN archive."
    )
    install_from_archive(package, version, lib)
  }

  verify_fix(package, version, lib)
  invisible(TRUE)
}

install_with <- function(backend, package, version, lib) {
  switch(backend,
    pak = pak::pkg_install(paste0(package, "@", version), lib = lib, ask = FALSE),
    remotes = {
      args <- list(package, version = version, lib = lib, upgrade = "never")
      if (!is.null(getOption("depguard.cran_mirror"))) {
        args$repos <- c(CRAN = getOption("depguard.cran_mirror"))
      }
      do.call(remotes::install_version, args)
    },
    archive = install_from_archive(package, version, lib)
  )
}

resolve_fix_method <- function(method) {
  order <- switch(method,
    auto = , pak = c("pak", "remotes", "archive"),
    remotes = c("remotes", "archive"),
    archive = "archive"
  )
  for (m in order) {
    if (m == "archive" || has_pkg(m)) return(m)
  }
}

cran_mirror <- function() {
  getOption("depguard.cran_mirror") %||% {
    repo <- getOption("repos")[["CRAN"]]
    if (is.null(repo) || identical(repo, "@CRAN@")) "https://cloud.r-project.org" else repo
  }
}

archive_urls <- function(package, version, mirror = cran_mirror()) {
  file <- sprintf("%s_%s.tar.gz", package, version)
  c(
    paste(mirror, "src/contrib/Archive", package, file, sep = "/"),
    paste(mirror, "src/contrib", file, sep = "/")
  )
}

install_from_archive <- function(package, version, lib) {
  urls <- archive_urls(package, version)
  dest <- file.path(tempdir(), basename(urls[1L]))
  got <- FALSE
  for (u in urls) {
    ok <- tryCatch(
      suppressWarnings(utils::download.file(u, dest, mode = "wb", quiet = TRUE)) == 0L,
      error = function(e) FALSE
    )
    if (isTRUE(ok) && file.exists(dest) && file.size(dest) > 0L) { got <- TRUE; break }
  }
  if (!got) {
    cli::cli_abort(c(
      "Could not download {package} {version}.",
      "i" = "Tried: {.url {urls}}",
      "i" = "Check the version exists on CRAN and that the internet is reachable."
    ))
  }
  on.exit(unlink(dest), add = TRUE)
  utils::install.packages(dest, repos = NULL, type = "source", lib = lib)
}

verify_fix <- function(package, version, lib) {
  ip_lib <- utils::installed.packages(lib.loc = lib, noCache = TRUE)
  now <- ip_version(package, ip_lib)
  if (is.na(now) || now != version) {
    cli::cli_abort("Install finished but {package} is {now %||% 'missing'} in {.path {lib}}, not {version}.")
  }

  active <- active_installed()
  active_v <- ip_version(package, active)
  if (!identical(active_v, version)) {
    active_lib <- active[match(package, active[, "Package"]), "LibPath"]
    cli::cli_alert_warning(
      "{package} {version} was installed into {.path {lib}}, but R will still load {active_v} from {.path {active_lib}} (earlier on the library path)."
    )
    return(invisible(FALSE))
  }

  cli::cli_alert_success("{package} rolled back to {version}.")
  loaded <- loaded_versions()
  if (package %in% names(loaded) && !identical(unname(loaded[package]), version)) {
    cli::cli_alert_warning(
      "{package} {loaded[[package]]} is still loaded in this session. {restart_hint()}"
    )
  }
  invisible(TRUE)
}
