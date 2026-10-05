# depguard 0.2.0

## Bug fixes

* `dep_diff()` now detects upgrades and downgrades of packages that are
  loaded in the running session. Version 0.1.0 compared the *loaded*
  version, which does not change until R restarts, so the headline
  `dep_snapshot()` -> `install.packages()` -> `dep_diff()` workflow reported
  "no changes" for exactly the packages it was meant to protect. Comparison is
  now on the on-disk version, and a loaded package whose running version no
  longer matches disk is flagged (`session_stale`).
* `dep_check()` is now genuinely offline by default. It previously called
  `tools::package_dependencies()` without a local database, which queried CRAN;
  with no internet the transitive tree came back empty and the check
  reported success. Dependencies are now resolved from installed metadata.
* Malformed lines in a text manifest containing `{}` no longer break the
  error message.
* Fixed a stray merge-conflict fragment in the vignette.

## New features

* `dep_check()` verifies the version constraints that packages declare on
  each other (`Imports: cli (>= 3.4.0)`), not just that transitive
  dependencies are installed, so real conflicts are caught. New columns
  `loaded` and `reason`; new status `"restart"`; `stop_on_problem` for
  scripts and CI; `check_cran = TRUE` now adds `cran_latest` and `outdated`
  and fails gracefully offline.
* Manifests accept operators: `">= 1.2"`, `"== 1.2.3"`, `"< 2.0"`, `"!= 1.0"`.
  A bare version still means "at least", so existing manifests keep working.
* Manifests can be human-readable text files (any extension other than
  `.rds`), convenient for version control.
* `dep_manifest_freeze()` pins the packages you are using now.
* `dep_snapshot()` records every installed package by default
  (`scope = "all"`), can be saved with `path`, and `dep_diff()` accepts a
  snapshot, a file path, or nothing (the latest snapshot). Saved snapshots
  survive the session restart that hosted notebooks require after installs.
* `dep_env()` detects Kaggle, Colab, Binder, RStudio and plain desktop
  sessions and reports writable libraries. Restart advice in messages is
  tailored to the platform.
* `dep_libraries()` finds packages shadowed by a different version in another
  library.
* `dep_healthcheck()` now reports the environment and shadowed packages.
* `dep_fix()` gains `lib`, `dry_run` and `method = "archive"`, a base-R
  backend that needs neither `pak` nor `remotes`. It verifies the result and
  warns if a restart is needed or if an earlier library hides the new copy.

## Breaking changes

* The `depguard_snapshot` structure changed (`packages` now has columns
  `disk_version`, `library`, `loaded_version`); snapshots saved by 0.1.0
  cannot be diffed.
* `dep_check()` results have two extra columns and the class
  `depguard_check` (still a data frame); the `required` column shows the
  operator form (e.g. `">= 1.1.4"`).
* `sessioninfo` is no longer imported.
* `dep_check()` prints the summary of problems; printing the result shows
  only problem rows (`print(x, all = TRUE)` for everything).

# depguard 0.1.0

* Initial CRAN release.
