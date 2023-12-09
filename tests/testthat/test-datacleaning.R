test_that("Location with ':' converted correctly", {
  expect_equal(eq_location_clean("INDONESIA: JAVA ISLAND"), "Java Island")
})
test_that("Location without ':' converted correctly", {
  expect_equal(eq_location_clean("INDONESIA"), "Indonesia")
})

test_that("Data frame is converted correctly",{
  clean_df <- initial_cleaning()
  expect_s3_class(clean_df$Date, 'Date')
  expect_type(clean_df$Location, 'character')
  expect_type(clean_df$Country, 'character')
  expect_in(c("Magnitude", "MMI", "Tsunami", "Date"), names(clean_df))
})
