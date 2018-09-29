#' Compute aspect shading per group for the given data
#'
#' The aspect shading statistic transform the height data into aspect shading data value.
#'
#' @import ggplot2 rayshader reshape2
#' @inheritParams layer
#' @inheritParams geom_fleck
#' @export
#' @examples
#' # Example with the volcano dataset
#'
#'
stat_ray_shading <- function(mapping = NULL, data = NULL,
                                geom = "point", position = "identity",
                                sun.angle = 1/3*pi,
                                slope= F,
                                res = 1,
                                zscale = 1,
                                ...,
                                show.legend = NA,
                                inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatRayShading,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      res = res,
      zscale = 1,
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
#' @export
StatRayShading <- ggproto("StatRayShading", Stat,
                             compute_group = function(data, scales, sun.angle = pi/3, slope = F, params,  res = 1, zscale = 1) {

                               deg <- ((sun.angle) * (180/pi)) %% 360

                               height <- dcast(data,x~y, value.var = "z" )
                               shade2 <- rotate(rotate(ray_shade(flipv(as.matrix(height[,-1])), sunangle = deg, zscale = zscale, remove_edges = F)))
                               rownames(shade2) <- height[,1]
                               colnames(shade2) <- colnames(height)[-1]
                               data2 <- melt((shade2))

                               colnames(data2) <- c("x", "y", "z")
                               #data$y <- - as.numeric(data$y)
                               # data2$y <-  sign(data$y) * data2$y
                               return(cbind(data2, data[,setdiff(colnames(data), colnames(data2))]))
                             },
                             required_aes = c("x", "y", "z")
)

rotate <- function(x) t(apply(x, 2, rev))


