#' @rdname geom_timeline_label
#'
#' @export
GeomTimelineLabel <- ggplot2::ggproto(
  "GeomTimelineLabel",
  ggplot2::Geom,

  draw_panel = function(data,
                        panel_params,
                        coord,
                        parse = FALSE,
                        na.rm = FALSE,
                        check_overlap = FALSE,
                        size.unit = "mm") {
    #####Text Grob#####
    nmax <- unique(data$nmax)
    if (nmax > 0) {
      data <- data %>% arrange(desc(.data$Magnitude))
      data[-(1:nmax), 'x'] <- NA
    }
    coords <- coord$transform(data, panel_params)
    if (length(unique(coords$y)) == 1)
      coords$y = 0.15 # plot line close to the bottom of the panel
    coords <- ggplot2::remove_missing(
      coords,
      na.rm = na.rm,
      c("x", "y", "xend", "yend", "linetype", "linewidth", "shape"),
      name = "geom_segment"
    ) # To make sure any data with lower magnitude gets removed
    size.unit <- ggplot2::.pt

    # Final result for text
    text_grob <- grid::textGrob(
      coords$label,
      coords$x,
      coords$y + coords$radius,
      default.units = "native",
      rot = coords$angle,
      gp = gpar(
        col = ggplot2::alpha(coords$colour, coords$alpha),
        fontsize = coords$fontsize,
        fontfamily = coords$family,
        fontface = coords$fontface,
        lineheight = coords$lineheight
      ),
      check.overlap = check_overlap
    ) #end of text grob

    #####Segment Grob####
    coords$xend <- coords$x
    coords$yend <- coords$y + coords$radius
    coords$linewidth <- coords$size
    line_grob <- grid::segmentsGrob(
      coords$x,
      coords$y,
      coords$xend,
      coords$yend,
      default.units = "native",
      gp = grid::gpar(
        col = ggplot2::alpha(coords$colour, coords$alpha),
        fill = ggplot2::alpha(coords$colour, coords$alpha),
        lwd = coords$linewidth * ggplot2::.pt,
        lty = coords$linetype
      ),
      arrow = NULL
    )


    grid::gTree(children = grid::gList(text_grob, line_grob))
  },

  required_aes = c("x", "label"),

  optional_aes = c('y', 'nmax', "Magnitude"),

  non_missing_aes = c('radius', 'angle'),

  draw_key = grid::nullGrob(),

  default_aes = aes(
    y = 1,
    colour = "black",
    fill = "black",
    size = 0.1,
    linetype = 1,
    alpha = 0.9,
    angle = 45,
    radius = 0.1,
    fontsize = 10
  )
)



#' @title Timeline label
#'
#' @description
#' \code{GeomTimelineLabel} and \code{geom_timeline_label} are ggproto class and function
#' that is used to add label to geom_timeline. If you don't want to show every single label,
#' use nmax to specify how many labels you want to show. If you use nmax, you have to specify
#' Magnitude so the geom can sort the data frame and pick nmax number of row from the top.
#' The geom will not issue warning if there is removed data from NA value, as it's the way the geom
#' sort and filter the data. The user have to make sure their data contain no unintended NA value.
#'
#' @inheritParams ggplot2::layer
#' @param na.rm If FALSE, missing values are removed with a warning. If TRUE, the default, missing values are silently removed
#' @param ... Other arguments passed on to layer(). These are often aesthetics, used to set an aesthetic to a fixed value, like colour = "red" or size = 3. They may also be parameters to the paired geom/stat.
#' @param parse If TRUE, the labels will be parsed into expressions and displayed as described in ?plotmath.
#' @param check_overlap If TRUE, text that overlaps previous text in the same layer will not be plotted.
#' @param size.unit The unit used to determine the size of label
#' @param nmax Optional, an integer expressing the amount of label you want to show, if you do use nmax, then you need to determine how to sort using Magnitude aes
#'
#' @seealso [timeline()]
#' @seealso [geom_timeline()]
#'
#' @return ggplot2 label layer
#' @export
#'
geom_timeline_label <- function(mapping = NULL,
                                data = NULL,
                                stat = "identity",
                                position = "identity",
                                ...,
                                parse = FALSE,
                                check_overlap = FALSE,
                                size.unit = "mm",
                                na.rm = TRUE,
                                show.legend = NA,
                                inherit.aes = TRUE,
                                nmax = NULL)
{
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTimelineLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      parse = parse,
      check_overlap = check_overlap,
      size.unit = size.unit,
      na.rm = na.rm,
      nmax = nmax,
      ...
    )
  )
}
