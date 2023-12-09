#' Earthquake interactive map builder
#' @description
#' A wrapper for leaflet package that shows interactive map containing marker.
#' The marker might or might not have popup information. If annotation = NULL, return
#' interactive map without any popup. If annotation = 'popup_text', put location, magnitude,
#' and total death into popup information. Any missing information will be removed.
#' Alternatively, you can specify specific column.
#'
#'
#' @param df cleaned data frame, preferably one that has been filtered
#' @param annotation Character string containing either NULL, "popup_text", or column name
#' @inheritDotParams datecountry_filter
#'
#' @return interactive map made by leaflet
#' @seealso [datecountry_filter()]
#' @export
#'
#' @examples
#' clean_df <- intial_cleaning()
#' map <- clean_df %>% eq_map(annotation = "popup_text", minyear = 2012, maxyear = 2022, countries = c('IRAN', 'IRAQ'))
eq_map <- function(df, annotation = "popup_text", ...) {
  df <- df %>% datecountry_filter(...)

  if (is.null(annotation))
    return(leaflet(data = df) %>% addTiles() %>% addCircleMarkers( ~ Longitude, ~
                                                                     Latitude))
  if (annotation == "popup_text") {
    df <- df %>% rowwise() %>%
      mutate(Location = htmlEscape(Location),
             Magnitude = htmlEscape(Magnitude),
             `Total Deaths` = htmlEscape(`Total Deaths`)) %>%
      mutate(Location = str_c("<b>", "Location :", "</b>", Location)) %>%
      mutate(Magnitude = str_c("<b>", "Magnitude :", "</b>", Magnitude)) %>%
      mutate(`TDeath` = str_c("<b>", "Total Deaths :", "</b>", `Total Deaths`)) %>%
      mutate(maplabel = str_c(
        coalesce(Location, ""),
        coalesce(Magnitude, ""),
        coalesce(TDeath, ""),
        sep = "<br/>"
      )) %>%
      mutate(maplabel = str_replace(maplabel, "<br/><br/>", "<br/>"))
    return(
      leaflet(data = df) %>% addTiles() %>% addCircleMarkers( ~ Longitude, ~
                                                                Latitude, popup = ~ maplabel)
    )
  }
  df <- df %>% rowwise() %>%
    mutate(annotated = as.character(.data[[annotation]])) %>%
    mutate(annotated = htmlEscape(annotated)) %>%
    mutate(annotated = str_c("<b>", annotation, ": ", "</b>", annotated))
  return(
    leaflet(data = df) %>% addTiles() %>% addCircleMarkers( ~ Longitude, ~
                                                              Latitude, popup = ~ annotated)
  )
}
