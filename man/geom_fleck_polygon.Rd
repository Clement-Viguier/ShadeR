% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_fleck_polygon.R
\name{geom_fleck_polygon}
\alias{geom_fleck_polygon}
\title{Fleck polygon}
\usage{
geom_fleck_polygon(mapping = NULL, data = NULL, stat = "identity",
  position = "identity", na.rm = FALSE, show.legend = NA,
  inherit.aes = TRUE, ...)
}
\arguments{
\item{na.rm}{If `FALSE`, the default, missing values are removed with
a warning. If `TRUE`, missing values are silently removed.}

\item{...}{other arguments passed on to [layer()]. These are
often aesthetics, used to set an aesthetic to a fixed value, like
`color = "red"` or `size = 3`. They may also be parameters
to the paired geom/stat.}
}
\description{
Fleck polygon is an extension of ggplot designed to plot geographic data. This is inspired by the designs of James Cheshire and Oliver Uberti.
}
\examples{

df <- data.frame(x = c(0,1,1,0, 0,1,1,0), y = c(0,0,1,2,2,1,4,4), z = c(1,1,1,1,2,2,2,2))
ggplot(df, aes(x, y, z = z)) + geom_fleck_polygon()+ coord_fixed() + theme_void()




}
