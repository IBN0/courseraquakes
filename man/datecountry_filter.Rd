% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_and_timeline.R
\name{datecountry_filter}
\alias{datecountry_filter}
\title{Helper function for timeline plot creation}
\usage{
datecountry_filter(
  df,
  minyear = -2150,
  minmonth = 1,
  minday = 1,
  maxyear = 2023,
  maxmonth = 12,
  maxday = 31,
  countries = NULL
)
}
\arguments{
\item{df}{data frame that needs to be filtered}

\item{minyear, maxyear}{Integer value of minimum and maximum year, accept any number}

\item{minmonth, maxmonth}{Integer value of minimum and maximum month, only accept 1-12}

\item{minday, maxday}{Integer value of minimum and maximum date, make sure it is valid date}

\item{countries}{Character vector containing country name in all upper case}
}
\value{
filtered data fram
}
\description{
Helper function for timeline plot creation
}
\examples{
clean_df <- initial_cleaning()
datecountry_filter(df = clean_df, 2012, 03, 01, 2021, 12, 21, c('MALAYSIA', 'PAKISTAN'))
}
