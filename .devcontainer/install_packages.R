#!/usr/bin/env Rscript

options(repos = c(
  POSIT = "https://packagemanager.posit.co/cran/__linux__/jammy/latest",
  CRAN  = "https://cloud.r-project.org"
))

install.packages(c(
  "tidyverse", "ragg", "rmarkdown", "here", "posterior",
  "bayesplot", "patchwork", "gt", "glue", "cli", "digest",
  "uuid", "suncalc", "timeDate", "yaml", "remotes", "devtools",
  "rstan", "StanHeaders"
))

remotes::install_github("wdfw-fp/creelutils")