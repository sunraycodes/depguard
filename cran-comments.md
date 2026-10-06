## Submission

This is an update from 0.1.0 to 0.2.0. It fixes two bugs where documented
behaviour did not work: dep_diff() missed upgrades of loaded packages, and
dep_check() contacted CRAN despite being documented as offline.

## Test environments
* local Windows 11, R 4.6.1
* GitHub Actions (OS and R versions from the workflow)
* win-builder (devel and release)

## R CMD check results
0 errors | 0 warnings | 0 notes

## Breaking changes
The structure of snapshot objects and the columns returned by dep_check()
changed; details are in NEWS.md.

## Notes on tests
Tests that install small test packages are skipped on CRAN
(skip_on_cran()) and write only to tempdir(). No function accesses the
network unless the user opts in via check_cran = TRUE or calls dep_fix().