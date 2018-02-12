# data(volcano)
# volcano <- as.data.table(melt(volcano, varnames = c("x", "y"),
#                               value.name = "h"))
#
# volcano[, c("dx", "dy") := metR::Derivate(h ~ x + y)]
# volcano[, angle := atan2(-dy, -dx)]
#
# sun.angle <- pi/3
#
# volcano$shade <-  cos(volcano$angle + sun.angle)
#
#
# # create some points for
#
# res <- min(abs(diff(head(volcano$x))))
#
# density <- 20
#
# generate.points <- function(pos, density, res = 1){
#   points <- (apply(pos, 1, function(p, density, res){
#     # print(p)
#     # print((p[3]+1))
#     n <- floor(density * (p[3]+1))
#     if (is.na(n)){
#       n <- 0
#     }
#     pts <- replicate(2, expr = runif(n, -0.5 * res, 0.5 * res))
#     # print(pts)
#     if (length(dim(pts))>0){
#     pts[,1] <- pts[,1] + p[1]
#     pts[,2] <- pts[,2] + p[2]}
#     # print(dim(pts))
#     return(pts)
#   }, density, res
#   )
#   )
#   points <- points[sapply(points, function(m){
#     # print(m)
#     length(dim(m)) > 0} )]
#   return(do.call(rbind, points))
# }
# pts <- as.data.frame(generate.points(volcano[,c("x", "y", "shade")], 20))
# colnames(pts) <- c("x", "y")
#
# ggplot(volcano, aes(x,y)) + geom_raster(aes(fill =  cos(angle + sun.angle)))+scale_fill_gradient2(low = "white", high = "white", mid = "gray20",midpoint = sun.angle) +
#   geom_point(data = pts, aes(), size = 0.1, alpha = 0.2)
