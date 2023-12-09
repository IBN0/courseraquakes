#' National Centers for Environmental Information earthquake Data
#'
#' Raw Earthquake data recorded by NCIE from -2150 to 2023
#'
#' @format ' A data frame with 6404 rows and 39 columns:
#' \describe{
#'   \item{Search Parameters}{Lorem ipsum}
#'   \item{Year}{Lorem ipsum}
#'   \item{Mo}{Lorem ipsum}
#'   \item{Dy}{Lorem ipsum}
#'   \item{Hr}{Lorem ipsum}
#'   \item{Mn}{Lorem ipsum}
#'   \item{Sec}{Lorem ipsum}
#'   \item{Tsu}{Lorem ipsum}
#'   \item{Vol}{Lorem ipsum}
#'   \item{Location Name}{Lorem ipsum}
#'   \item{Latitude}{Lorem ipsum}
#'   \item{Longitude}{Lorem ipsum}
#'   \item{Focal Depth (km)}{Lorem ipsum}
#'   \item{Mag}{Lorem ipsum}
#'   \item{MMI Int}{Lorem ipsum}
#'   \item{Deaths}{Lorem ipsum}
#'   \item{Death Description}{Lorem ipsum}
#'   \item{Missing}{Lorem ipsum}
#'   \item{Missing Description}{Lorem ipsum}
#'   \item{Injuries}{Lorem ipsum}
#'   \item{Injuries Description}{Lorem ipsum}
#'   \item{Damage ($Mil)}{Lorem ipsum}
#'   \item{Damage Description}{Lorem ipsum}
#'   \item{Houses Destroyed}{Lorem ipsum}
#'   \item{Houses Destroyed Description}{Lorem ipsum}
#'   \item{Houses Damaged}{Lorem ipsum}
#'   \item{Houses Damaged Description}{Lorem ipsum}
#'   \item{Total Deaths}{Lorem ipsum}
#'   \item{Total Death Description}{Lorem ipsum}
#'   \item{Total Missing}{Lorem ipsum}
#'   \item{Total Missing Description}{Lorem ipsum}
#'   \item{Total Injuries}{Lorem ipsum}
#'   \item{Total Injuries Description}{Lorem ipsum}
#'   \item{Total Damage ($Mil)}{Lorem ipsum}
#'   \item{Total Damage Description}{Lorem ipsum}
#'   \item{Total Houses Destroyed}{Lorem ipsum}
#'   \item{Total Houses Destroyed Description}{Lorem ipsum}
#'   \item{Total Houses Damaged}{Lorem ipsum}
#'   \item{Total Houses Damaged Description}{Lorem ipsum}
#' }
#' @source <https://www.ngdc.noaa.gov/hazel/view/hazards/earthquake/event-data?maxYear=2023&minYear=-2150>
"earthquake_raw"

#' Helper function for initial data cleaning
#'
#' @param locname String from raw earthquake data with country data
#'
#' @keywords internal
#'
#' @return Country name if the input has no ":", otherwise any string after the country
eq_location_clean <- function(locname) {
  if (!(str_detect(locname, ":")))
    return(str_to_title(locname))
  name <- str_extract(locname, ":.*")
  name <- str_extract(name, "[:alnum:].*")
  name <- str_to_title(name)
  return(name)
}

#' Data cleaning for raw earthquake data recorded by NCIE from -2150 to 2023.
#' @description
#' A function to clean up NCIE earthquake data, this includes replacing missing value,
#' changing non-descriptive column name, separating country from its area, and creating
#' column with date class.
#'
#'
#' @param df Tibble data frame taken from NCIE, if you have different data, you can supply your own.
#'
#' @return Cleaned tibble data frame with separate country and area column, along with improved date stamp
#' @export
#'
#' @examples
#' clean_df <- initial_cleaning()
initial_cleaning <- function(df = earthquake_raw) {
  output <- df %>%
    slice(-1) %>%
    replace_na(list(
      Mo = 01L,
      Dy = 01L,
      `Location Name` = 'Unknown'
    )) %>%
    rename(
      Tsunami = Tsu,
      Volcano = Vol,
      Magnitude = Mag,
      `MMI` = `MMI Int`
    ) %>%
    mutate(Date = make_date(Year, Mo, Dy), .keep = "unused") %>%
    mutate(
      Location = map_chr(.x = `Location Name`, .f = eq_location_clean),
      Country = str_extract(`Location Name`, "[^:]*"),
      .keep = "unused"
    ) %>%
    select(!c(`Search Parameters`, Hr, Mn, Sec))
}
