test_that("Filter working correctly", {
  clean_df <- initial_cleaning()
  trial_df <- clean_df %>% datecountry_filter(minyear = 2013, minmonth = 02, maxyear = 2017, maxmonth = 08, maxday = 08,
                                              countries = c('CHINA', 'INDONESIA', 'INDIA', 'BRAZIL'))
  expect_equal(min(trial_df$Date), make_date(2013, 02, 19))
  expect_equal(max(trial_df$Date), make_date(2017, 08, 08))
  expect_false('GERMANY' %in% trial_df$Country)
})

test_that("No error in timeline plot", {
  clean_df <- initial_cleaning()
  plotdata <- timeline(df = clean_df, y= 'Country', minyear = 2004, maxyear = 2005, minmonth = 02,
                       countries = c('INDIA', 'INDONESIA'), nmax = 12, label = TRUE)
  expect_no_error(plotdata)

})
