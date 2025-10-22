# ================= install_and_load.R =================
# Bootstrap your everyday R workflow -------------------

pkgs <- c(
  # Core tidyverse
  "tidyverse", "ggplot2", "dplyr", "tidyr", "readr",
  "purrr", "tibble", "stringr", "forcats",
  # Pipes / helpers
  "magrittr",  "data.table", "lubridate",
  # Reporting
  "rmarkdown", "knitr",      "plotly", "httpgd",
  # App & package dev
  "shiny",     "devtools", "renv", "here")

# ---- 1. Install anything missing ----
installed <- rownames(installed.packages())
to_install <- setdiff(pkgs, installed)

if (length(to_install)) {
  message("Installing: ", paste(to_install, collapse = ", "))
  install.packages(to_install, dependencies = TRUE)
}

# ---- 2. Attach only what isn’t already on the search path ----
already_attached <- pkgs[paste0("package:", pkgs) %in% search()]
to_attach        <- setdiff(pkgs, already_attached)

if (length(to_attach)) {
  suppressPackageStartupMessages(
    invisible(lapply(to_attach, library, character.only = TRUE))
  )
}

# ---- 3. Status report ----
msg <- sprintf(
  "%d newly installed, %d newly attached, %d were already attached.",
  length(to_install), length(to_attach), length(already_attached)
)
message(msg)
# ======================================================

# From an R console or an RMarkdown/Quarto setup chunk
# setwd("C:/Users/henri/Documents/publicdataprojects/blog/code/")
# source("install_and_load.R")
# source("C:/Users/henri/Documents/publicdataprojects/blog/code/install_and_load.R")

# To run all this: source(here("blog", "code", "install_and_load.R")) if here() is set to pdp root.