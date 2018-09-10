## Vignette: https://ggplot2.tidyverse.org/articles/extending-ggplot2.html#creating-a-new-geom

library(grid)
library(proto)
library(ggplot2)

StatChull <- ggproto("StatChull", Stat,
                      compute_group = function(data, scales) {
                        data[chull(data$x, data$y), , drop = FALSE]
                      },
                      
                      required_aes = c("x", "y")
)

stat_chull <- function(mapping = NULL, data = NULL, geom = "polygon",
                       position = "identity", na.rm = FALSE, show.legend = NA, 
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatChull, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

ggplot(mpg, aes(displ, hwy)) + 
  geom_point() + 
  stat_chull(fill = NA, colour = "black")

ggplot(mpg, aes(displ, hwy, colour = drv)) + 
  geom_point() + 
  stat_chull(fill = NA)










GeomSimplePoint <- ggproto("GeomSimplePoint", Geom,
                           required_aes = c("x", "y"),
                           default_aes = aes(shape = 19, colour = "black"),
                           draw_key = draw_key_point,
                           
                           draw_panel = function(data, panel_params, coord) {
                             coords <- coord$transform(data, panel_params)
                             grid::pointsGrob(
                               coords$x, coords$y,
                               pch = coords$shape,
                               gp = grid::gpar(col = coords$colour)
                             )
                           }
)

geom_simple_point <- function(mapping = NULL, data = NULL, stat = "identity",
                              position = "identity", na.rm = FALSE, show.legend = NA, 
                              inherit.aes = TRUE, ...) {
  layer(
    geom = GeomSimplePoint, mapping = mapping,  data = data, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

ggplot(mpg, aes(displ, hwy)) + 
  geom_simple_point()

