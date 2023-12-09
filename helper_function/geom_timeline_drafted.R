#' @title A \code{ggplot2} \code{Geom} prototype object for timelines
#'
#' @description \code{GeomTimeline} is a \code{ggplot2} \code{geom} prototype
#' object for timelines.  The geom plots a time line of earthquakes ranging
#' from \code{xmin} to \code{xmax} dates with a marker for each earthquake.
#' Optional aesthetics include \code{color}, \code{size}, and \code{alpha} (for transparency).
#' The \code{x} aesthetic is a \code{Date} and an optional \code{y} aesthetic is a \code{factor}
#' indicating stratification in which case multiple time lines will be plotted for each
#' level of the factor (e.g. country).
#'
#' @return \code{GeomTimeline} is a \code{ggplot2} prototype object; a collection of
#'     functions and does not return anything perse.  In design pattern parlance,
#'     \code{GeomTimeline} represents a geometry strategy.
#'
#' @seealso \link{geom_timeline}
#'
#' @format NULL
#' @usage NULL
#'
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 draw_key_point
#' @importFrom grid pointsGrob
#' @importFrom ggplot2 alpha
#' @importFrom grid unit
#' @importFrom grid gList
#' @importFrom grid gpar
#'
#' @example inst/examples/example_timeline_label.R
#'
GeomTimeline <- ggplot2::ggproto(
  "GeomTimeline",

  ggplot2::Geom,

  required_aes = c("x"),

  optional_aes = c(
    "y"       , "size"    , "shape"   , "colour", "fill",
    "linesize", "linetype", "fontsize", "stroke"
  ),

  default_aes  = ggplot2::aes(
    shape    = 19     ,
    y        = 1      ,
    size     = 5      ,
    alpha    = 0.15   ,
    colour   = 'black',
    fill     = 'black',
    linesize = 0.5    ,
    linetype = 1      ,
    fontsize = 10     ,
    stroke   =  1
  ),

  draw_key = ggplot2::draw_key_point,

  setup_data = function(data, params) {
    data[                       # filter based on given xmin and xmax
      data$x >= params$xmin &
        data$x <= params$xmax,
    ]
  },

  draw_panel = function(data, panel_scales, coord) {
    coords <-coord$transform(data, panel_scales)
    if (length(unique(coords$y)) == 1) coords$y = 0.1875 # plot line close to the bottom of the panel
    points <- grid::pointsGrob(
      x    = coords$x                      ,
      y    = coords$y                      ,
      size = grid::unit(coords$size , "mm"),
      pch  = coords$shape                  ,
      gp   = grid::gpar(
        col      = ggplot2::alpha(coords$colour, coords$alpha),
        fill     = coords$fill                                ,
        fontsize = grid::unit(coords$fontsize, "points")
      )
    )
    ys     <- unique(coords$y)
    rangex <- range(coords$x)
    lines  <- grid::segmentsGrob(
      x0 = rangex[1],
      x1 = rangex[2],
      y0 = ys,
      y1 = ys,
      gp = grid::gpar(
        col  = ggplot2::alpha(coords$colour[1], coords$alpha[1]),
        fill = ggplot2::alpha(coords$colour[1], coords$alpha[1]),
        lwd  = grid::unit(coords$linesize[1], "mm"),
        lty  = coords$linetype[1]
      )
    )
    grid::gTree(children = grid::gList(lines, points))
  })


#' @title A \code{ggplot2} layer function for timelines
#'
#' @description \code{geom_timeline} is a \code{ggplot2} layer function
#' representing timelines.  \code{geom_timeline} plots a time line of events
#' ranging in time from \code{xmin} to \code{xmax} dates, with a point for each event.
#' Optional aesthetics include \code{color}, \code{size}, and \code{alpha} (for transparency).
#' The \code{x} aesthetic is usually a \code{Date} and an optional \code{y} aesthetic is
#' a \code{factor} indicating stratification, in which case multiple time lines will be
#' plotted for each factor level (e.g. factors may be countries so that quakes are
#' grouped by country in the timeline plot).
#'
#' @inheritParams ggplot2::layer
#'
#' @param xmin any type coercible to a \code{numeric} without additional parameters.
#' \code{xmin} is usually a \code{Date} and should correspond to the given \code{x}
#' aesthetic, since it specifies the lower bound that aesthetic.  That's so say, the
#' timeline will only show events in the data where \code{x >= xmin}.
#' @param xmax any type coercible to a \code{numeric} without additional parameters.
#' \code{xmin} is usually a \code{Date} and should correspond to the given \code{x}
#' aesthetic, since it's specifies the upper bound for that aesthetic.  That's so say, the
#' timeline will only show events in the data where \code{x <= xmin}.
#' @param na.rm a \code{boolean} indicating whether or not to remove NAs.
#' \code{na.rm = FALSE} by default.
#' @param ... a \code{...} indicates a list of additional parameters
#' used for a geom.  \code{geom_timeline} doesn't make use of these.
#'
#' @return a \code{ggplot2} layer object.
#'
#' @seealso link{geom_timeline_label}
#'
#' @importFrom ggplot2 layer
#'
#' @example inst/examples/example_timeline_label.R
#'
#' @export
geom_timeline <- function(mapping     = NULL,       data        = NULL, stat = "identity",
                          position    = "identity", na.rm       = FALSE,
                          show.legend = NA        , inherit.aes = TRUE,
                          xmin        = .Machine$double.xmin,
                          xmax        = .Machine$double.xmax,
                          ...) {
  ggplot2::layer(
    geom = GeomTimeline      , mapping     = mapping    , position = position,
    data = data              , stat        = stat       ,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      xmin  = as.numeric(xmin), # deals with dates and factors
      xmax  = as.numeric(xmax), # deals with dates and factors
      na.rm = na.rm,
      ...
    )
  )
}
