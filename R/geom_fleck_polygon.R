#' Fleck polygon
#'
#' Fleck polygon is an extension of ggplot designed to plot geographic data. This is inspired by the designs of James Cheshire and Oliver Uberti.
#'
#' @import ggplot2 grid
#' @importFrom rgl triangulate
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
geom_fleck_polygon <- function(mapping = NULL, data = NULL, stat = "identity",
                               position = "identity", na.rm = FALSE, show.legend = NA,
                               inherit.aes = TRUE, ...) {
  layer(
    geom = GeomFleckPolygon, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

GeomFleckPolygon <- ggproto("GeomFleckPolygon", Geom,
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

                              data$id <- 1:dim(data)[1]

                              data2 <- generate.points.poly(data[,c("x", "y", "z", "id", "density")]) # need to add res parameter

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


generate.points.poly <- function(data){
  # print(data)
  triangles <- rgl::triangulate(data$x, data$y)

  triangles.areas <- apply(triangles, 2, function(tri, data){
    triangle.area(data[tri[1], c("x", "y")], data[tri[2], c("x", "y")], data[tri[3], c("x", "y")])
  }, data)

  triangles <- rbind(triangles, triangles.areas)
  points <- apply(triangles, 2, function(tri, data, total.area){
    # extract points A, B and C:
    # print(tri)
    a <- data[tri[1], c("x", "y")]
    b <- data[tri[2], c("x", "y")]
    c <- data[tri[3], c("x", "y")]

    n <- data[1, "density" ] * data[1, "z" ] * tri[4]/total.area

    p <- replicate(n, runif.triangle(a,b,c), simplify = F )
    p <- do.call(rbind, p)

    # print(p)
    # p <- as.data.frame(t(unlist(c(p, data[tri[1], "id"]))))
    p <- as.data.frame(p)
    p$id <- data[tri[1], "id"]
    # print(p)
    return(p)
  }, data, total.area = sum(triangles.areas))

  # print(points)
  points[sapply(points, function(m){
    # print(m)
    length(dim(m)) > 0} )]

  fleck <- (do.call(rbind, points))
  colnames(fleck) <- c("x", "y", "id")
  rownames(fleck) <- 1: dim(fleck)[1]
  fleck <- as.data.frame(fleck)
  # print(fleck)
  return(fleck)
}

runif.triangle <- function(a, b, c){
  # print(c(a,b,c))
  r1 <- runif(1)
  r2 <- runif(1)

  p <- (1-sqrt(r1)) * a + (sqrt(r1) * (1 - r2)) * b + (r2 * sqrt(r1)) * c
  return(p)
}

triangle.area <- function(a, b, c){
  area <- abs(a$x * (b$y - c$y) + b$x * (c$y - a$y) + c$x * (a$y - b$y) )/2
  return(area)
}

generate.points.poly2 <- function(data){
  triangles <- rgl::triangulate(data$x, data$y)

  points <- apply(triangles, 2, function(tri, data){
    # extract points A, B and C:
    a <- data[tri[1], c("x", "y")]
    b <- data[tri[2], c("x", "y")]
    c <- data[tri[3], c("x", "y")]
    p <- c(mean(c(a$x, b$x, c$x)), mean(c(a$y, b$y, c$y)))
    p <- as.data.frame(t(c(p, data[tri[1], "id"])))
    return(p)
  }, data)


  fleck <- (do.call(rbind, points))
  colnames(fleck) <- c("x", "y", "id")
  rownames(fleck) <- 1: dim(fleck)[1]
  fleck <- as.data.frame(fleck)
  print(fleck)
  return(fleck)
}

