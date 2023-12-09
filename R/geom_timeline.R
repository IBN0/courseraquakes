#' @rdname geom_timeline
#'
#' @export
GeomTimeline <- ggplot2::ggproto(
  "GeomTimeline",

  ggplot2::Geom,

  required_aes = c("x"),

  optional_aes = c(
    "y"       ,
    "size"    ,
    "shape"   ,
    "colour",
    "fill",
    "linesize",
    "linetype",
    "fontsize",
    "stroke"
  ),

  default_aes  = ggplot2::aes(
    shape    = 19     ,
    y        = 0.15   ,
    size     = 5      ,
    alpha    = 0.5    ,
    colour   = 'black',
    fill     = 'black',
    linesize = 0.5    ,
    linetype = 1      ,
    fontsize = 10     ,
    stroke   =  1
  ),

  draw_key = ggplot2::draw_key_point,

  draw_panel = function(data, panel_scales, coord) {
    coords <- coord$transform(data, panel_scales)
    if (length(unique(coords$y)) == 1)
      coords$y = 0.15
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
  }
)


#' @title `Timeline Visualiser`
#'
#' @description \code{GeomTimeline} and \code{geom_timeline} are ggplot2 proto class and layer function
#' to display timeline of earthquake data. The x axis is expected to be date, and y aesthetic
#' is factorized column that separates the data into separate line. The geom does not filter
#' inputted data frame.
#' To simplify data analysis, use \code{timeline} function
#'
#' @inheritParams ggplot2::layer
#'
#' @seealso [timeline()]
#' @seealso [geom_timeline_label()]
#'
#' @export
#'
geom_timeline <-
  function(mapping     = NULL,
           data        = NULL,
           stat        = "identity",
           position    = "identity",
           na.rm       = FALSE,
           show.legend = NA        ,
           inherit.aes = TRUE,
           ...) {
    layer(
      geom = GeomTimeline      ,
      mapping     = mapping    ,
      position    = position   ,
      data        = data       ,
      stat        = stat       ,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm,
                    ...)
    )
  }
