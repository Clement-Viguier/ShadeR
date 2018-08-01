#' Compute slope shading per group for the given data
#'
#' The aspect shading statistic transform the height data into aspect shading data value.
#'
#' @import ggplot2
#' @inheritParams layer
#' @inheritParams geom_fleck
#' @export
#' @examples
#' # Example with the volcano dataset
#'
#'
stat_slope_shading <- function(mapping = NULL, data = NULL,
                                geom = "point", position = "identity",
                                sun.elevation = 1/3*pi,
                                res = 1,
                                ...,
                                show.legend = NA,
                                inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatSlopeShading,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      res = res,
      na.rm = FALSE,
      sun.elevation = sun.elevation,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatSlopeShading <- ggproto("StatSlopeShading", Stat,
                             compute_group = function(data, scales, params, sun.elevation = pi/3, res = 1) {


                               # compute angle:
                               data[, c("dx", "dy")] <- metR::Derivate(data$z ~ data$x + data$y)[c(1,3)]
                               # print(data)
                               data$angle <- atan(abs(data$dy) + abs(data$dx))
                               data$angle <- data$angle
                               data$z <- cos(sun.elevation - (data$angle+pi/2))
                               # print(summary(data))

                               return(data)
                             },
                             required_aes = c("x", "y", "z")
)
