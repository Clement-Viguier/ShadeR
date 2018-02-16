#' Fleck
#'
#' Fleck is an extension of ggplot designed to plot geographic data. This is inspired by the designs of James Cheshire and Oliver Uberti.
#' The geom_fleck draws multiple points randomly distributed on a grid defined by the aes x and y with a maximum density defined by the aesthetic density.
#' It can be used with stat = aspect_shading (option by default) or stat = identity that do not compute the shading value from the height z.
#'
#' @import ggplot2 grid
#' @eval rd_aesthetics("geom", "fleck")
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
#' ggplot(volcano2, aes(x, y, z = z)) + geom_fleck()+ coord_fixed() + theme_void()
#'
#'
geom_fleck <- function(mapping = NULL, data = NULL,
                       stat = "aspect_shading", position = "identity",
                       res = 1,
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFleck,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      res = res,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomFleck <- ggproto("GeomFleck", Geom,
                     required_aes = c("x", "y", "z"),
                     non_missing_aes = c("size", "shape", "colour", "density"),
                     default_aes = aes(
                       shape = 19, colour = "grey60", size = 0.1,
                       density = 20,
                       fill = NA,
                       alpha = NA, stroke = 0.1
                     ),

                     draw_panel = function(data, panel_params, coord, res = 1, na.rm = FALSE) {
                       # Checks on density and resolution
                       # adjust resolution:
                       if (is.null(res)){
                         res <- median(diff(data$x))
                       }
                       # print("in geom!")

                       # scale z
                       if (any(data$z > 1 | data$z < 0)){
                         data$z <- (data$z - min(data$z))/(max(data$z) - min(data$z))
                       }
                       # Need to create new points at this step:
                       # (requires nb of new points per group, computed from density and z)
                       data$id <- 1:dim(data)[1]
                       # print(head(data))
                       # print(summary(data))
                       # print(density)
                       # print("before generate!")
                       data2 <- generate.points(data[,c("x", "y", "z", "id", "density")], res = res) # need to add res parameter
                       # print(data2)


                       # print(summary(data2))
                       data <- merge(data[,setdiff(colnames(data), c("x", "y"))], data2)

                       coords <- coord$transform(data, panel_params)
                       # print(head(coords))
                       # ggname("geom_fleck",
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
# )
                       # grid::pointsGrob(
                       #   coords$x, coords$y,
                       #   pch = coords$shape,
                       #   gp = grid::gpar(col = coords$colour)
                       # )
                     },

                     draw_key = draw_key_point
)


generate.points <- function(data,  res = 1){
  # print(res)
  points <- (apply(data, 1, function(p,  res){
    # print(p)
    # print((p[3]+1))
    n <- floor(p[5] * p[3])
    if (is.na(n)){
      n <- 0
    }
    # print(n)
    pts <- replicate(2, expr = runif(n, -0.5 * res, 0.5 * res))
    # print(pts)
    # pts <- matrix(pts, ncol = 2)
    if (length(dim(pts))>0){
      pts <- matrix(pts, ncol = 2)
      pts[,1] <- pts[,1] + p[1]
      pts[,2] <- pts[,2] + p[2]
      pts <- cbind(pts, rep(p[4], n))}
    # print(dim(pts))
    # pts <- cbind(pts, rep(p[4], n))
    return(pts)
  },  res
  )
  )
  points <- points[sapply(points, function(m){
    # print(m)
    length(dim(m)) > 0} )]

  fleck <- (do.call(rbind, points))
  colnames(fleck) <- c("x", "y", "id")
  rownames(fleck) <- 1: dim(fleck)[1]
  fleck <- as.data.frame(fleck)
  # print(fleck)
  return(fleck)
}


