# Install packages ohne renv

# pacman 
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  shiny,
  DT,
  shinyjs,
  officer,
  flextable,
  readr,
  dplyr,
  writexl,
  magick,
  lubridate,
  stringr
)
