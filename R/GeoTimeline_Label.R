#'@description This is the wrapper to the layer function for the GeomTimeLineLabel
#'
#' The wrapper is just to labels. See \code{ggplot2} package documenation
#' for more specifics on geom_* and layer_* functions.
#'@export
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", na.rm = FALSE,
                                show.legend = FALSE, inherit.aes = TRUE, n_max = 5, ...) {


  #select top n rows by COuntry
  topn <- dplyr::top_n(mod2[order(mod2$COUNTRY,-mod2$EQ_PRIMARY),] %>% dplyr::group_by(COUNTRY),n_max,wt=EQ_PRIMARY)

  ggplot2::layer(
    geom = GeomTimeLineLabel, mapping = mapping,
    data = topn, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )

}


#'@title GeomTimeLineLable
#'@description This is the GeomTimeLineLable. It creates the labels for the top n earthquakes
#'@param x A required aesthetic of representing the year.
#'@param label A required aesthetic of name of the location
#'@export

GeomTimeLineLabel <- ggproto("GeomTimeLineLabel", Geom,
                             required_aes = c("x","label"),
                             default_aes = ggplot2::aes(y=0.1, colour = "black", size = 0.2,scale_vline=1,
                             linetype = 1, alpha = NA, angle = 45,
                             hjust = 0, vjust = 0,fill=1,numLabels=5,
                             family = "", fontface = 2, lineheight = 1.5),
                             draw_key = draw_key_label,
                             draw_group = function(data,panel_scales,coord) {
                             coords <- coord$transform(data,panel_scales)
                             vline <- grid::segmentsGrob(
                                 x0 = coords$x, x1 = coords$x,
                                 y0 = coords$y, y1 = coords$y + 0.1/coords$scale_vline, # need to scale size
                                 default.units = "npc",
                                 gp = grid::gpar(
                                   size=0.5,
                                   alpha=1,
                                   color="black"
                                   ,fill=1)
                               )

                               line_label <- grid::textGrob(
                                 coords$label,
                                 x=coords$x,
                                 y=coords$y + 0.1/coords$scale_vline,
                                 default.units = "npc",
                                 hjust = coords$hjust,
                                 vjust = coords$vjust,
                                 rot = coords$angle,
                                 gp = grid::gpar(
                                   col = "black",
                                   alpha=1,fill=1,
                                   n_max=numLabels,
                                   fontsize = 3.5 * .pt,
                                   fontfamily = coords$family,
                                   fontface = coords$fontface,
                                   lineheight = coords$lineheight
                                 )
                               )
                               timeline_label <- grid::gTree(children = grid::gList(vline,line_label))
                             }

)



