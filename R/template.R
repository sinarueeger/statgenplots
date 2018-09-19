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







library(ggplot2)
library(magrittr)
####################### 
StatChull <- ggproto("StatChull", Stat,
                     compute_group = function(data, scales) {
                       
                       ## equidistance
                       data2 <- data %>% dplyr::arrange(x, colour) %>% dplyr::mutate(tmp = 1, cumsum.x = cumsum(tmp))
                       ## real distance
                       # dat <- gwasResults %>% arrange(CHR, BP) %>% mutate(tmp = diff from start, x = cumsum(tmp))
                       
                       
                       ## new x axis
                       #                       med.dat <- data %>% group_by(group) %>% summarise(median.x = median(cumsum.tmp))
                       #scale_x_continuous(breaks = med.dat$median.x, labels = med.dat$CHR)
                      data2$x <- data$cumsum.x
                      data.frame(x = data$cumsum.x, y = data2$y, colour = data2$colour)
                      },
                     
                     required_aes = c("x", "y")
)

stat_chull <- function(mapping = NULL, data = NULL, geom = "point",
                       position = "identity", na.rm = FALSE, show.legend = NA, 
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatChull, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

dat <- qqman::gwasResults#, chr="CHR", bp="BP", snp="SNP", p="P" )
dat <- dat %>% dplyr::rename(x = BP)

ggplot(dat, aes(x = x, y = -log10(P), colour = as.factor(CHR))) + 
  stat_chull()

ggplot(mpg, aes(x = displ, y = hwy, x.super = drv, colour = drv)) + 
  stat_chull()











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

