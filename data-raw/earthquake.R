## code to prepare `earthquake` dataset goes here

library(readr)

earthquake_raw <- readr::read_tsv('data-raw/earthquakes-2023-11-28_02-16-21_+0700.tsv')
usethis::use_data(earthquake_raw, overwrite = TRUE)
