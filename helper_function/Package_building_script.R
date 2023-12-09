library(devtools)
library(grid)
library(tidyverse)
library(htmltools)
library(leaflet)
library(rlang)
load_all()
# library(tidyverse)


usethis::use_testthat() # Should've done from the start to speed up testing :((
usethis::use_test()
usethis::use_data_raw('earthquake')
usethis::use_package_doc()
# Taken from hurricane data, might add other function when needed
usethis::use_import_from("dplyr", 'mutate', load = F)
usethis::use_import_from("dplyr", 'rowwise', load = F)
usethis::use_import_from("dplyr", 'ungroup', load = F)
usethis::use_import_from("dplyr", 'group_by', load = F)
usethis::use_import_from("dplyr", 'slice', load = F)
usethis::use_import_from("tidyr", 'pivot_wider', load = F)
usethis::use_import_from("tidyr", 'pivot_longer', load = F)
usethis::use_import_from("dplyr", 'summarise', load = F)
usethis::use_import_from("dplyr", 'select', load = F)
usethis::use_import_from("dplyr", 'filter', load = F)
usethis::use_import_from("dplyr", 'desc', load = F)
usethis::use_import_from("dplyr", 'arrange', load = F)
usethis::use_import_from("dplyr", 'rename', load = F)
usethis::use_import_from("dplyr", 'coalesce', load = F)
usethis::use_import_from("dplyr", 'rowwise', load = F)
usethis::use_import_from("ggplot2", 'layer', load = F)
usethis::use_import_from("ggplot2", 'Geom', load = F)
usethis::use_import_from("ggplot2", 'ggproto', load = F)
usethis::use_import_from("ggplot2", 'ggplot', load = F)
usethis::use_import_from("ggplot2", 'ggproto', load = F)
usethis::use_import_from("ggplot2", 'aes', load = F)
usethis::use_import_from("ggplot2", 'draw_key_point', load = F)
usethis::use_import_from("ggplot2", 'alpha', load = F)
usethis::use_import_from("ggplot2", 'labs', load = F)
usethis::use_import_from("ggplot2", 'scale_size_continuous', load = F)
usethis::use_import_from("ggplot2", 'scale_color_continuous', load = F)
usethis::use_import_from("ggplot2", 'theme_classic', load = F)
usethis::use_import_from("ggplot2", 'position_nudge', load = F)
usethis::use_import_from("grid", 'pointsGrob', load = F)
usethis::use_import_from("grid", 'unit', load = F)
usethis::use_import_from("grid", 'gList', load = F)
usethis::use_import_from("grid", 'gpar', load = F)
usethis::use_import_from("grid", 'gTree', load = F)
usethis::use_import_from("grid", 'gList', load = F)
usethis::use_import_from("magrittr", "%>%", load = F)
usethis::use_import_from("readr", 'read_tsv', load = F)
usethis::use_import_from("stringr", 'str_extract', load = F)
usethis::use_import_from("stringr", 'str_detect', load = F)
usethis::use_import_from("stringr", 'str_to_title', load = F)
usethis::use_import_from("stringr", 'str_c', load = F)
usethis::use_import_from("stringr", 'str_replace', load = F)
usethis::use_import_from("lubridate", 'year', load = F)
usethis::use_import_from("lubridate", 'month', load = F)
usethis::use_import_from("lubridate", 'day', load = F)
usethis::use_import_from("lubridate", 'ymd', load = F)
usethis::use_import_from("lubridate", 'make_date', load = F)
usethis::use_import_from("tidyr", 'replace_na', load = F)
usethis::use_import_from("purrr", 'map_chr', load = F)
usethis::use_import_from("leaflet", 'leaflet', load = F)
usethis::use_import_from("leaflet", 'addTiles', load = F)
usethis::use_import_from("leaflet", 'addCircleMarkers', load = F)
usethis::use_import_from("htmltools", 'htmlEscape', load = F)
usethis::use_import_from("rlang", 'list2', load = F)
load_all()
usethis::use_readme_rmd()
use_vignette("courseraquqakes-vignette")
build_rmd('vignettes/courseraquqakes-vignette.Rmd')
usethis::use_gpl3_license()
devtools::document()

knitr::knit('README.Rmd')
devtools::test()

column_name <- names(earthquake_raw)
doc_column <- character()
for (item in column_name){
  result <- (paste0("#\'   \\item{", item, "}{Lorem ipsum}"))
  # result <- substring(result, 2)
  doc_column <- c(doc_column, result)
}
cat(doc_column, sep = "\n")

load_all()
cleaned_df <- initial_cleaning()
timeline(df = cleaned_df, y = Country, minyear = 2004, maxyear = 2005, minmonth = 02,
                    countries = c('INDIA', 'INDONESIA'))

leaflet(data = ftrial) %>% addTiles() %>% addCircleMarkers(~Longitude, ~Latitude, popup = ~Location)

a <- leaflet(data = ftrial2) %>% addTiles()
addCircleMarkers(data = a, ~Longitude, ~Latitude, popup = ~maplabel)

eq_map(df = ftrial, annotation = "popup_text")
eq_map(df = ftrial, annotation = "Location")

load_all()
cleaned_df <- initial_cleaning()
trial_df <- cleaned_df %>% datecountry_filter(minyear = 2013, maxyear = 2017, countries = c('CHINA', 'INDONESIA')) %>% nmax_sort(12)
tested_df<- cleaned_df %>% datecountry_filter(minyear = 2013, minmonth = 02, maxyear = 2017, maxmonth = 08, maxday = 08,
                                countries = c('CHINA', 'INDONESIA', 'INDIA', 'BRAZIL'))

plotdata <- timeline(df = cleaned_df, y= 'Country', minyear = 2004, maxyear = 2005, minmonth = 02,
                    countries = c('INDIA', 'INDONESIA'), nmax = 12, label = TRUE)
# timeline_with_label(df=trial_df, y= 'Country', nmax = 2, label = FALSE)

trial_df2 <- clean_df %>% rowwise() %>%
                       mutate(annotated = as.character(Location)) %>%
                       mutate(annotated = htmlEscape(annotated)) %>%
                       mutate(annotated = str_c("<b>", 'Location', ": ", "</b>", annotated))

finalplot <- ggplot() +
  geom_timeline(
    data = filtered_df,
    mapping = aes(
      x    = Date,
      y    = Country,
      size = Magnitude,
      col  = `Total Deaths`
    ),
    alpha = 0.8
  ) +
  labs(x = "DATE")                                     +
  scale_size_continuous (name = "Richter scale value") +
  scale_color_continuous(name = "# deaths") +
  theme_classic() +
  geom_timeline_label(
    data = filtered_df,
    mapping = aes(
      x = Date,
      y = Country,
      label = Location,
      Magnitude = `Total Deaths`
    ),
    nmax = 10
  )


timeline(data = clean_df, y = Country, nmax = 10, label = TRUE,
        minyear =  2012, maxyear = 2021, countries = c('ARGENTINA', 'PALESTINE'))
