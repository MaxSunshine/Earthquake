#'@title Theme theme_timeline
#'@description This is a simple theme for the plot.
# A simple theme for the plot
#'@export
theme_timeline <- function() {
  ggplot2::theme(
    plot.background = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    axis.line.x = ggplot2::element_line(size = 1),
    axis.ticks.y = ggplot2::element_blank(),
    legend.position = "bottom"

  )
}

#'@title Geom GeomTimeline
#'@description This is a geom for plotting a time line of earthquakes ranging from xmin to xmaxdates
#'with a point for each earthquake. The min and max dates are defined by filtering the dataset on the DATE field
#' Optional aesthetics include color, size, and alpha (for transparency).
#'The xaesthetic is a date and an optional y aesthetic is a factor indicating some stratification in which
#'case multiple time lines will be plotted for each level of the factor (e.g. country)
#'@param c(x) A required aesthetic of earthquake dates.
#'@param color An optional aesthetic for the color scale based on DEATHS value.
#'@param size A optional aesthetic for size of points based on the EQ_PRIMARY value
#'@param alpha A optional aesthetic for transparency
#'@param y A optional y aesthetic is a factor indicating some stratification in which
#'case multiple time lines will be plotted for each level of the factor (e.g. country)
#' @export
GeomTimeline <- ggproto("GeomTimeline", Geom,
                       required_aes = c("x"),
                       non_missing_aes = c("size", "shape", "colour"),
                       default_aes = aes(y=1, shape = 19,size=1.5,colour = "black",stroke = 0.5,alpha=.2, fill=NA),
                       draw_key = draw_key_point,
                       draw_panel = function(data, panel_params, coord) {
                       coords <- coord$transform(data, panel_params)


                        ## Construct a grid grob
                       tl_points<- grid::pointsGrob(
                           x = coords$x,
                           y = coords$y,
                           default.units = "npc",
                           gp=grid::gpar(col = ggplot2::alpha(coords$colour, coords$alpha),
                                   fill = ggplot2::alpha(coords$fill, coords$alpha),
                                   fontsize = coords$size * .pt + coords$stroke * .stroke / 2
                                   ,cex=.25/nrow(as.data.frame(unique(coords$y)))
                           ),
                           size=grid::unit(coords$size,"char"),
                           pch = coords$shape
                         )

                       tl_line<-grid::segmentsGrob(
                          x0=min(panel_params$x.minor),
                          x1=max(panel_params$x.minor),
                          y0=coords$y,
                          y1=coords$y
                        )


                        #grobTree( tl_line)
                        grid::gTree(children = grid::gList(tl_points, tl_line))
                       }



                       )



#'@title geom_timeline
#'@description This is the wrapper to the layer function for the GeomTimeline
#'
#' The wrapper is just to plot the object. See \code{ggplot2} package documenation
#' for more specifics on geom_* and layer_* functions.
#' @export
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                         position = "identity", na.rm = FALSE,
                         show.legend = NA, inherit.aes = TRUE,...) {
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )

}

