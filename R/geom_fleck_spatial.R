#' Fleck polygon
#'
#' Fleck polygon is an extension of ggplot designed to plot geographic data. This is inspired by the designs of James Cheshire and Oliver Uberti.
#'
#' @import ggplot2 grid
#' @eval rd_aesthetics("geom", "fleck", "polygon")
#' @inheritParams layer
#' @param na.rm If `FALSE`, the default, missing values are removed with
#'   a warning. If `TRUE`, missing values are silently removed.
#' @param ... other arguments passed on to [layer()]. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   `color = "red"` or `size = 3`. They may also be parameters
#'   to the paired geom/stat.
#' @inheritParams layer
#' @export
#' @examples
#'
#' df <- data.frame(x = c(0,1,1,0, 0,1,1,0), y = c(0,0,1,2,2,1,4,4), z = c(1,1,1,1,2,2,2,2))
#' ggplot(df, aes(x, y, z = z)) + geom_fleck_polygon()+ coord_fixed() + theme_void()
#'
#'
#'
#'
geom_fleck_spatial <- function(mapping = NULL, data = NULL, stat = "identity",
                               position = "identity", na.rm = FALSE, show.legend = NA,
                               inherit.aes = TRUE, ...) {
  layer(
    geom = GeomFleckSpatial, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

GeomFleckSpatial <- ggproto("GeomFleckSpatial", Geom,
                            required_aes = c("x", "y", "z"),
                            non_missing_aes = c("size", "shape", "colour", "density"),
                            default_aes = aes(
                              shape = 19, colour = "grey60", size = 0.1,
                              density = 20,
                              fill = NA,
                              alpha = NA, stroke = 0.1
                            ),

                            draw_group = function(data, panel_scales, coord) {
                              # print(coord)
                              # print(panel_scales)
                              n <- nrow(data)
                              if (n <= 2) return(grid::nullGrob())

                              # if (any(data$z > 1 | data$z < 0)){
                              # data$z <- (data$z - min(data$z))/(max(data$z) - min(data$z))
                              # }
#
#                               print(data)
#
#                               browser()
#                               poly1 <- list(Polygon(data[,c("x", "y")]))
#                               print(typeof(poly1))
#                               print(class(poly1))
#                               print(poly1)
#                               poly <- Polygons(poly1, ID = data$group[1])
#                               print(typeof(poly))
#                               print(class(poly))

                              data.spatial <- SpatialPolygons(list(Polygons(list(Polygon(SpatialPointsDataFrame(coords = data[,c("x", "y")], data = data))), ID = unique(data$group))))


                              data2 <- dotsInPolys(data.spatial, as.integer(mean(data$z) * mean(data$density)), f="random")@coords
                              colnames(data2) <- c("x","y")

                              data <- as.data.frame(merge(data[1,setdiff(colnames(data), c("x", "y"))], data2))

                              coords <- coord$transform(data, panel_scales)

                              # coords <- data

                              # print(coords)

                              pointsGrob(
                                coords$x, coords$y,
                                pch = coords$shape,
                                gp = gpar(
                                  col = alpha(coords$colour, coords$alpha),
                                  fill = alpha(coords$fill, coords$alpha),
                                  # Stroke is added around the outside of the point
                                  fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
                                  lwd = coords$stroke * .stroke / 2
                                )
                              )
                            },

                            draw_key = draw_key_point
)

