library(ggmap)
library(tidyverse)
library(geosphere)
library(maps)

####################DATA LOADING####################

# setwd('Documents/R_trial/get_me_out_of_here/_7ed6a595f3e1ac944ccbb1f07db4caae_hurricanes_data/')

ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                       4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day",
                         "hour", "year", "latitude", "longitude",
                         "max_wind", "min_pressure", "rad_max_wind",
                         "eye_diameter", "pressure_1", "pressure_2",
                         paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                         "storm_type", "distance_to_land", "final")

####################DATA CLEANING####################


#' Data cleaning
#'
#' @param input_df data frame (or forceable to tibble_df) from hurricane data
#' @details
#' The dataframe combines all stuffs as needed, convert the nautical miles unit
#' to metres, and spread the data frame to follow the guideline
#'
#'
#' @return Cleaned tibble df containing 9 columns
#' @import dplyr tidyr
#' @export
#'
#' @examples
#' \dontrun{cleandf <-ext_tracks %>% data_cleaning() %>% data_filter() %>% data_pivot()}
data_cleaning <- function(input_df){
  if(!is_tibble(input_df)) input_df <- tibble::as_tibble(input_df)

  df <- input_df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(storm_id = paste(storm_name, year, collapse = "-"),
                  date = strptime(paste(year, month, day, hour), '%Y %m %d %H'),
                  longitude = longitude*(-1), .keep = 'unused') %>%
    dplyr::ungroup() %>%
    dplyr::group_by(max_wind) %>%
    tidyr::pivot_longer(cols = starts_with('radius'),
                        names_prefix = 'radius_', values_to = 'speed') %>%
    dplyr::mutate(wind_speed = str_extract(name, '[0-9]{1,2}'),
                  direction = str_extract(name, '[a-z]+')) %>%
    dplyr::group_by(wind_speed) %>%
    tidyr::pivot_wider(names_from = direction, values_from = speed) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(storm_id, date, latitude, longitude, wind_speed) %>%
    dplyr::summarise(
      ne = sum(ne, na.rm = TRUE) * 1852,
      se = sum(se, na.rm = TRUE) * 1852,
      sw = sum(sw, na.rm = TRUE) * 1852,
      nw = sum(nw, na.rm = TRUE) * 1852,
      .groups = 'drop')
  return(df)
} # End of function data cleaning

######End result:#####
# storm_id     date                latitude longitude wind_speed    ne    se    sw    nw
# <chr>        <dttm>                 <dbl>     <dbl> <chr>      <dbl> <dbl> <dbl> <dbl>
#   1 ALBERTO 1988 1988-08-05 18:00:00     32       -77.5 34             0     0     0     0
# 2 ALBERTO 1988 1988-08-05 18:00:00     32       -77.5 50             0     0     0     0
# 3 ALBERTO 1988 1988-08-05 18:00:00     32       -77.5 64             0     0     0     0
# 4 ALBERTO 1988 1988-08-06 00:00:00     32.8     -76.2 34             0     0     0     0
# 5 ALBERTO 1988 1988-08-06 00:00:00     32.8     -76.2 50             0     0     0     0
# 6 ALBERTO 1988 1988-08-06 00:00:00     32.8     -76.2 64             0     0     0     0


#' Data filtering
#'
#' @param input_df cleaned data frame that follows the format as the course requirement
#' For now only filter for 2008 ike hurricane at 13 september because that's all we need.
#'
#' @return Filtered data frame containing ike hurricane at 13 september 2008
#' @import dplyr
#' @export
#'
#' @examples
data_filter <- function(input_df){
  df <- input_df %>%
    dplyr::select(storm_id, date, latitude, longitude, wind_speed, ne, se, sw, nw) %>%
    dplyr::filter(storm_id == 'IKE 2008', date == '2008-09-13')
  return(df)
}

#' Data pivoting
#'
#' @param data filtered hurricane data frame (can also do it unfiltered, but it become slow)
#' @description
#' Contains these columns:
#' "storm_id", "date", "lat", "lon", "wind_speed",
#' "name", "distance", "direction", "degree"
#' Also compute the degree based on direction. Luckily, the directions as factor
#' are already sorted by compass' quadrant.
#'
#' @import dplyr tidyr
#' @return Data frame containing 9 columns, combine all the ne/se/sw/nw into 2 column
#' @export
#'
#' @examples
data_pivot <- function(data){
  finaldf <- data %>%
    tidyr::pivot_longer(cols = c(ne, se, sw, nw)) %>%
    dplyr::mutate(direction = as_factor(name)) %>%
    dplyr::mutate(degree = as.integer(direction) * 90) %>%
    dplyr::rename(distance = value, lon = longitude, lat = latitude)
}
#####End result:#####
# storm_id date                  lat   lon wind_speed name  distance direction degree
# <chr>    <dttm>              <dbl> <dbl> <chr>      <chr>    <dbl> <fct>      <dbl>
#   1 IKE 2008 2008-09-13 00:00:00  28.3   -94 34         ne      444480 ne            90
# 2 IKE 2008 2008-09-13 00:00:00  28.3   -94 34         se      370400 se           180
# 3 IKE 2008 2008-09-13 00:00:00  28.3   -94 34         sw      277800 sw           270
# 4 IKE 2008 2008-09-13 00:00:00  28.3   -94 34         nw      314840 nw           360
# 5 IKE 2008 2008-09-13 00:00:00  28.3   -94 50         ne      277800 ne            90
# 6 IKE 2008 2008-09-13 00:00:00  28.3   -94 50         se      296320 se           180


####################STAT GGPROTO####################


# DON'T FORGET THE SELF ARGUMENT
# IT IS IMPERATIVE SO YOU DON'T SPEND DAYS DEBUGGING LIKE ME :((

#' Computation
#'
#' @param self To access data given to stat layer
#' @param data data frame containing filtered and pivoted data
#' @param scales Required for stat ggproto even when not used
#' @description
#' Compute the outer edge of a hurricane based on range using `geosphere::destPoint`
#' Since geompoly require x and y aes, we rename the lon and lat from desPoint
#' into x and y. nb: theoretically this is where we  can input scale_radii.
#' @importFrom geosphere destPoint
#' @importFrom dplyr bind_rows
#' @importFrom dplyr rename
#' @return Data  frame containing 4 columns: wind_speed, direction, x, and y
#' @export
#'
#' @examples
computation <- function(self, data, scales) {
  df_all <- data.frame()
  for(i in 1:nrow(data)){
    df_temp <- data.frame(
      wind_speed = data[i, ]$wind_speed,
      direction = data[i, ]$direction,
      geosphere::destPoint(p = c(data[i, ]$lon, data[i, ]$lat),
                           b = (data[i, ]$degree - 90):data[i, ]$degree,
                           d = data[i, ]$distance * 1
      )
    )
    df_all <- dplyr::bind_rows(df_all, df_temp)
  }
  df_all <- dplyr::rename(.data = df_all, x = lon, y = lat)
  return(df_all)
}


StatHurricane <- ggplot2::ggproto("StatHurricane", Stat,
                                  compute_group = computation,
                                  required_aes = c('lon', 'lat', 'direction',
                                                   'distance', 'degree', 'wind_speed')
) # end of StatHurricane ggproto

#' Stat hurricane wrapper
#'
#' @inheritParams ggplot2::stat_identity
#'
#' @importFrom ggplot2 layer
#' @return Data for geom layer
#' @export
#'
stat_hurricane <- function(mapping = NULL, data = NULL, geom = "hurricane",
                           position = "identity", na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE, bandwidth = NULL,
                           ...) {
  ggplot2::layer(
    stat = StatHurricane, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
} # end of stat_hurr layer function


####################GEOM GGPROTO####################

# Everything is already done in stat proto
# So we just need to make one that inherit geompoly and change required aes

GeomHurricane <- ggproto("GeomHurricane", GeomPolygon,
                         required_aes= c('x', 'y', 'wind_speed', 'direction')
)

#' Geom hurricane layer
#'
#' @inheritParams ggplot2::geom_polygon
#'
#' @importFrom ggplot2 GeomPolygon
#' @importFrom ggplot2 layer
#' @return plot
#' @export
#'
geom_hurricane <- function(mapping = NULL, data = NULL, stat = "hurricane",
                           position = "identity", na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomHurricane, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

####################RUNNING####################


ext_tracks <- read_fwf("ebtrk_atlc_1988_2015.txt",
                       fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                       na = "-99")

cleandf <-ext_tracks %>% data_cleaning() %>% data_filter() %>% data_pivot()

# The old type is no longer usable, had to try another one,
# thanks for our good teachers who never updated their teaching material :)
map_data <- get_map("Louisiana", zoom = 6, maptype = "roadmap")
base_map <- ggmap(map_data, extent = "device")

hurricane_ike <- base_map +
  geom_hurricane(data = cleandf,
                 aes(lon = lon, lat = lat,
                     direction = direction, distance = distance,
                     degree = degree, wind_speed = wind_speed, fill = wind_speed,
                     colour = wind_speed, alpha = 0.5, group = wind_speed)) +
  guides(alpha = 'none') +
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red4", "orange4", "yellow4")) +
  scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red", "orange", "yellow"))

# Yes, there is no scale_radii, I won't make it, at least not now. The current
# code already fried my brain as-is.


png('hurricane_ike.png')
print(hurricane_ike)
dev.off()
