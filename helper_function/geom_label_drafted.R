#' @title A TimelineLabel geometry prototype object
#'
#' @description \code{GeomTimelineLabel} is the geometry prototype object
#' used by the \link{geom_timeline_label} geometry, layer function.
#'
#' @inheritParams ggplot2::Geom
#'
#' @format NULL
#' @usage NULL
#'
#' @return \code{GeomTimelineLabel} doesn't return anything per se.
#' Instead \code{GeomTimelineLabel} is as a prototype, or template
#' for objects of this type.
#'
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 alpha
#' @importFrom grid nullGrob
#' @importFrom grid textGrob
#' @importFrom grid textGrob
#' @importFrom grid segmentsGrob
#' @importFrom grid gpar
#' @importFrom grid unit
#' @importFrom grid gTree
#' @importFrom grid gList
#'
#' @example inst/examples/example_timeline_label.R
#'
GeomTimelineLabel2  <- ggplot2::ggproto(

  "GeomTimelineLabel2",

  ggplot2::Geom,

  required_aes = c("x", "label"),

  optional_aes = c(
    "y", "size", "shape", "colour",
    "linesize", "linetype", "fontsize", "stroke",
    "pointerheight", "angle", "labelcolour",
    "fill"
  ),

  default_aes  = ggplot2::aes(
    shape         = 19     , # shape 19 is a circle.
    y             = 1      ,
    size          = 5      ,
    alpha         = 0.15   ,
    colour        = "black",
    linesize      = 0.5    ,
    linetype      = 1      ,
    fontsize      = 10     ,
    stroke        = 1      ,
    pointerheight = 0.05   ,
    angle         = 45
  ),

  draw_key = function(data, params, size) grid::nullGrob(), # Don't want any key for labels.  This is a null function.

  draw_panel = function(data, panel_scales, coord) {
    coords <-coord$transform(data, panel_scales)
    if (length(unique(coords$y)) == 1) coords$y = 0.15 # plot line close to the bottom of the panel
    txt <- grid::textGrob(
      coords$label,
      coords$x,
      coords$y + (1.2 * coords$pointerheight), # 1.2 provides reasonable spacing.
      default.units = "native"    ,
      just          = "left"      ,
      rot           = coords$angle,
      gp = grid::gpar(
        col       = coords$labelcolour,
        fontsize  = grid::unit(coords$fontsize, "points")
      ),
      check.overlap = FALSE # check_overlap
    )
    lines <- grid::segmentsGrob(
      x0 = coords$x,
      x1 = coords$x,
      y0 = coords$y,
      y1 = coords$y + coords$pointerheight,
      gp = grid::gpar(
        col  = ggplot2::alpha(coords$colour, coords$alpha),
        fill = ggplot2::alpha(coords$colour, coords$alpha),
        lwd  = grid::unit(coords$linesize, "mm"),
        lty  = coords$linetype
      )
    )
    grid::gTree(children = grid::gList(txt, lines))
  }
)

#' @md
#' @title Add text labels to timelines produced with \link{geom_timeline}
#'
#' @description \code{geom_timeline_label} adds annotations to earthquake data
#' timelines produced with \link{geom_timeline}. This geom adds a vertical
#' line to each data point with a text annotation (e.g. the location of an
#' earthquake) attached atop each line.
#' \code{geom_timeline_label} provides an option to subset to \code{n_max} number
#' of earthquakes, where \code{n_max} selects the largest (by magnitude) earthquakes.
#' This geometry supports the following aesthetics:
#' * \code{x} is the date of the earthquake and
#' * \code{label} is the column name from which annotations will be obtained
#'
#' @inheritParams ggplot2::layer
#'
#' @param pointerheight is a \code{numeric} indicating the height of lable, pointer lines
#' This height is specified as a faction of the viewport height,
#' and will usually not require adjustment.  The default value for this parameter
#' is 0.05.
#' @param angle is a \code{numeric} indicating the text label angle. Text is printed
#' at an angle to allow good readability and to reduce label collisions for dense
#' time lines.  This angle won't usually require adjustment and is set at 45 degrees
#' by default.
#' @param n_max is a \code{numeric} giving the maximum number of labels to display.  This
#' reduces the number of label collisions in dense, timelime plots.  The labels to show
#' are chosen based on the \code{size} aestheic.  Setting \code{n_max} will result
#' in a maximum of \code{n_max} labels, ordered by the aesthetic \code{size}, being rendered.
#' @param labelcolour is a \code{string} giving the label colour.  This is set to 'black' by
#' default.
#' @param na.rm a \code{boolean} indicating whether or not to remove NAs.
#' \code{na.rm = FALSE} by default.
#' @param fill a \code{string} indicating the label, fill colour.
#' \code{fill = 'black'} by default.
#' @param xmin is a \code{numeric} specifiying the minimum \code{x} value to consider.
#' \code{xmin = .Machine$double.xmin} by default.
#' @param xmax is a \code{numeric} specifiying the maximum \code{x} value to consider.
#' \code{xmax = .Machine$double.xmax} by default.
#' @param ... a \code{...} indicates a list of additional parameters
#' used for a geom.  \code{geom_timeline_label} doesn't make use of these.
#'
#' @return a \code{ggplot2} object representing timeline labels.  This is intended to be
#' used with \link{geom_timeline}.
#'
#' @example inst/examples/example_timeline_label.R
#
#' @export
geom_timeline_label2 <- function(mapping     = NULL       , data          = NULL , stat        = "identity",
                                position    = "identity" , na.rm         = FALSE, show.legend = NA        ,
                                inherit.aes = TRUE       , pointerheight = 0.05 , angle       = 45        ,
                                labelcolour = "black"    ,
                                fill        = "black"    ,
                                ...) {
  ggplot2::layer(
    geom = GeomTimelineLabel2, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      pointerheight = pointerheight,
      angle         = angle,
      labelcolour   = labelcolour,
      na.rm         = na.rm,
      ...
    )
  )
}
