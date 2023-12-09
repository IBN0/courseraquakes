timeline_with_label <- function(df, y = `Country`, minyear = -2150, maxyear = 2023,
                                countries = NULL, nmax = NULL, ...){
  filtered_df <- df %>% datecountry_filter(minyear = minyear, maxyear = maxyear, countries = countries, ...)
  labeled <- filtered_df %>% nmax_sort(nmax = nmax)
  finalplot <- ggplot() +
    geom_timeline(data = filtered_df,
                  mapping = aes(
                    x    = .data$Date,
                    y    = {{y}},
                    size = .data$Magnitude,
                    col  = .data$Deaths
                  ),
                  alpha = 0.8
    ) +
    labs(x = "DATE")                                     +
    scale_size_continuous (name = "Richter scale value") +
    scale_color_continuous(name = "# deaths"           ) +
    theme_classic() +
    geom_spoke(data = labeled, mapping = aes(x    = .data$Date,
                                             y    = {{y}}),
               angle = 90,
               radius = 0.1,
               na.rm = T) +
    geom_text(data = labeled, mapping = aes(x    = .data$Date,
                                            y    = {{y}},
                                            label = .data$Location),
              hjust = 0,
              position = position_nudge(y = 0.1),
              angle = 45,
              na.rm = T)
  return(finalplot)
}

timeline_without_label <- function(df, y = `Country`, minyear = -2150, maxyear = 2023,
                                   countries = NULL, ...){
  filtered_df <- df %>% datecountry_filter(minyear = minyear, maxyear = maxyear, countries = countries, ...)
  finalplot <- ggplot() +
    geom_timeline(data = filtered_df,
                  mapping = aes(
                    x    = .data$Date,
                    y    = {{y}},
                    size = .data$Magnitude,
                    col  = .data$Deaths
                  ),
                  alpha = 0.8
    ) +
    labs(x = "DATE")                                     +
    scale_size_continuous (name = "Richter scale value") +
    scale_color_continuous(name = "# deaths"           ) +
    theme_classic()
  return(finalplot)
}

nmax_sort <- function(df, nmax = NULL) {
  if(!is.null(nmax)){
    df <- df %>% arrange(desc(.data$Magnitude)) %>% slice(1:nmax)
    # df[-(1:nmax), 'Date'] <- as.Date(0, origin ="1970-01-01")

  }
  return(df)
}
