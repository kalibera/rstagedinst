# rstagedinst

This repository contains results from package checks related to staged
installation of packages in R and some related utilities.

`hardcoded` contains results from recent checking of CRAN and BIOC 3.9
packages for hard-coded temporary installation directory names. Included are
packages where `R CMD INSTALL --staged-install` fails because it finds such
hardcoded paths. For each such package, there is an output from running
`sicheck.R` tool (also included) which reports the full hard-coded path
names and also in which R objects they were found. The outputs are filtered
to exclude directory prefix specific to the system where the experiment has
been run. Package repositories (CRAN, BIOC) and versions are detailed in
`README` file.



