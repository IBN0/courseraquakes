
test_that("No error in interactive map", {
  clean_df <- initial_cleaning()
  trial_df <- clean_df %>% datecountry_filter(minyear = 2013, minmonth = 02, maxyear = 2017,
                                              countries = c('CHINA', 'INDONESIA', 'INDIA', 'BRAZIL'))
  inmap <- eq_map(trial_df, "popup_text")
  expect_no_error(inmap)
})

test_that("No error in interactive map with filter", {
  clean_df <- initial_cleaning()
  inmap <- eq_map(clean_df, "popup_text", minyear = 2013, minmonth = 02, maxyear = 2017,
                  countries = c('CHINA', 'INDONESIA', 'INDIA', 'BRAZIL'))
  expect_no_error(inmap)
})
