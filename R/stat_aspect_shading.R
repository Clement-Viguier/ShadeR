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
                          slope= F,
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
      slope = slope,
      na.rm = FALSE,
      sun.angle = sun.angle,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @importFrom metR Derivate
#' @export
StatAspectShading <- ggproto("StatAspectShading", Stat,
                        compute_group = function(data, scales, sun.angle = pi/3, slope = F, params,  res = 1) {

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

                          # print(data)
                          # print(typeof(data))
                          # compute angle:
                          # print(summary(data))
                          # print("before metR")
                          data[, c("dx", "dy")] <- metR::Derivate(z ~ x + y, data)
                          # print(summary(data))
                          data$angle <- atan2(data$dy, data$dx)
                          # print(summary(data))
                          # turn z into shade
                          # data$z <- (cos(data$angle + sun.angle)+1)/2
                          # data$z <- (cos(data$angle + sun.angle)+1)/2
                          data$z <- (sin((data$angle + sun.angle))+1)
                          # print(sun.angle)
                          # print(summary(data))

                          if (slope ){
                            data$z <- data$z * (abs(data$dx) + abs(data$dy) )
                          }
                          # print(summary(data))
                          # print(data)

                          return(data)
                        },
                        required_aes = c("x", "y", "z")
)
