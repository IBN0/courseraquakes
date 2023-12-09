#' Helper function for timeline plot creation
#'
#' @param df data frame that needs to be filtered
#' @param minyear,maxyear Integer value of minimum and maximum year, accept any number
#' @param minmonth,maxmonth Integer value of minimum and maximum month, only accept 1-12
#' @param minday,maxday Integer value of minimum and maximum date, make sure it is valid date
#' @param countries Character vector containing country name in all upper case
#'
#' @return filtered data fram
#' @export
#'
#' @examples
#' clean_df <- initial_cleaning()
#' filtered_df <- clean_df %>% datecountry_filter(2012, 03, 01, 2021, 12, 21, c('MALAYSIA', 'PAKISTAN'))
datecountry_filter <-
  function(df,
           minyear = -2150,
           minmonth = 01,
           minday = 01,
           maxyear = 2023,
           maxmonth = 12,
           maxday = 31,
           countries = NULL) {
    mindate <- make_date(minyear, minmonth, minday)
    maxdate <- make_date(maxyear, maxmonth, maxday)
    stopifnot(mindate <= maxdate)
    if (!is.null(countries)) {
      df <- df %>% filter(`Country` %in% {
        {
          countries
        }
      })
    }
    df <- df %>% filter(.data$Date >= mindate & .data$Date <= maxdate)
  }

#' @title `Timeline construction`
#' @description This is wrapper function that filter out the data frame and then pass it into
#' custom \code{geom_timeline} and (optionally) \code{geom_timeline_label} The theme and label are
#' already chosen, if you wish to customize those, you might  want to construct your own
#' ggplot2 using \code{geom_timeline} and \code{geom_timeline_label}
#'
#'
#' @param df Data frame or its extension containing cleaned up earthquake data
#' @param y Column name that will be used as the basis of timeline separation
#' @param label Boolean value, whether you want to show label containing location name or not.
#' @param nmax Optional, an integer expressing the amount of label you want to show
#' @inheritDotParams datecountry_filter
#'
#' @return ggplot2 plot
#' @export
#'
#' @examples
#' clean_df <- initial_cleaning()
#' timeline_plot <- clean_df %>% timeline(y = Country, nmax = 10, label = TRUE,
#'                               minyear =  2012, maxyear = 2021, countries = c('ARGENTINA', 'PALESTINE'))
timeline <- function(df,
                     y = NULL,
                     # minyear = -2150,
                     # maxyear = 2023,
                     # countries = NULL,
                     nmax = 0,
                     label = FALSE,
                     ...) {
  filtered_df <-
    df %>% datecountry_filter(# minyear = minyear,
                              # maxyear = maxyear,
                              # countries = countries,
                              ...)
  # labeled <- filtered_df %>% nmax_sort(nmax = nmax)
  finalplot <- ggplot() +
    geom_timeline(
      data = filtered_df,
      mapping = aes(
        x    = .data$Date,
        y = {{y}},
        size = .data$Magnitude,
        col  = .data$Deaths
      ),
      alpha = 0.8
    ) +
    labs(x = "DATE")                                     +
    scale_size_continuous (name = "Richter scale value") +
    scale_color_continuous(name = "# deaths") +
    theme_classic()
  if (label == TRUE) {
    finalplot <- finalplot + geom_timeline_label(
      data = filtered_df,
      mapping = aes(
        x = .data$Date,
        y = {{y}},
        label = .data$Location,
        Magnitude = .data$Magnitude
      ),
      nmax = nmax
    )
  }
  return(finalplot)
}
