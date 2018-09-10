
##make manhattan plot
## ggplot(data, aes(x = c(chr, pos), y = P)) 
## > should turn this into

## Geom QQ Plot
## ------------------
StatManhattan <- ggproto("StatManhattan", Stat,
                     compute_group = function(data, scales) {
                       
                       ## equidistance
                       data2 <- data %>% dplyr::arrange(x1, x2) %>% dplyr::mutate(tmp = 1, cumsum.tmp = cumsum(tmp))
                       ## real distance
                       # dat <- gwasResults %>% arrange(CHR, BP) %>% mutate(tmp = diff from start, x = cumsum(tmp))
                       
                       
                       ## new x axis
#                       med.dat <- data %>% group_by(group) %>% summarise(median.x = median(cumsum.tmp))
                       #scale_x_continuous(breaks = med.dat$median.x, labels = med.dat$CHR)
                       
                       
                      data.frame(x = data2$cumsum.tmp, y = data2$y)
                      
                      },
                     
                     required_aes = c("y", "x2", "x1")
              
)

stat_manhattan <- function(mapping = NULL, data = NULL, geom = "point",
                       position = "identity", na.rm = FALSE, show.legend = NA, 
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatManhattan, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}



## Example
## ------------
dat <- qqman::gwasResults#, chr="CHR", bp="BP", snp="SNP", p="P" )

## default
qp <- ggplot(dat, aes(x2 = BP, y = -log10(P), x1 = CHR)) + 
  stat_manhattan() + 
  geom_hline(yintercept = 8)
print(qp)


## different x axis scheme

## adding nice stuff

## color >> not working
qp <- ggplot(dat, aes(x2 = BP, y = -log10(P), x1 = CHR)) + 
  stat_manhattan(aes(color = factor(CHR))) + 
  geom_hline(yintercept = 8)
print(qp) 

## group

## facet
