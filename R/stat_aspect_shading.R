#' Compute aspect shading per group for the given data
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
stat_aspect_shading <- function(mapping = NULL, data = NULL,
                          geom = "point", position = "identity",
                          sun.angle = 1/3*pi,
                          res = 1,
                          ...,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatAspectShading,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      res = res,
      na.rm = FALSE,
      sun.angle = sun.angle,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatAspectShading <- ggproto("StatAspectShading", Stat,
                        compute_group = function(data, scales, params, sun.angle = pi/3, res = 1) {

                          # print(res)
                          # # adjust resolution:
                          # if (is.null(res)){
                          #   res <- median(diff(data$x))
                          # }
                          # if (res < 0){
                          #   res = 1
                          # }
                          # print(res)
                          # res = 1

                          # compute angle:
                          data[, c("dx", "dy")] <- metR::Derivate(data$z ~ data$x + data$y)[c(1,3)]
                          # print(data)
                          data$angle <- atan2(-data$dy, -data$dx)
                          # turn z into shade
                          data$z <- (cos(data$angle + data$sun.angle)+1)/2
                          # print(data)

                          return(data)
                        },
                        required_aes = c("x", "y", "z", "sun.angle")
)
