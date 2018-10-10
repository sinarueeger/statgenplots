
##make manhattan plot
## ggplot(data, aes(x = c(chr, pos), y = P)) 
## > should turn this into


## Geom QQ Plot
## ------------------
StatManhattan <- ggproto("StatManhattan", Stat,
                        compute_group = function(data, scales) {
                       
                       ## equidistance
                      # data2 <- data %>% dplyr::arrange(x1, x2) %>% dplyr::mutate(tmp = 1, cumsum.tmp = cumsum(tmp))
                       ## real distance
                       # dat <- gwasResults %>% arrange(CHR, BP) %>% mutate(tmp = diff from start, x = cumsum(tmp))
                       
                       
                       ## new x axis
#                       med.dat <- data %>% group_by(group) %>% summarise(median.x = median(cumsum.tmp))
                       #scale_x_continuous(breaks = med.dat$median.x, labels = med.dat$CHR)
                       
                       return(data)
                      },
                     
                     required_aes = c("x", "y")
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
library(tidyverse)
dat <- qqman::gwasResults#, chr="CHR", bp="BP", snp="SNP", p="P" )

dat <- dat %>% 
  dplyr::arrange(CHR, BP) %>% 
  dplyr::mutate(tmp = 1, cumsum.tmp = cumsum(tmp)) ## create x axis

## create labels x axis
med.dat <- dat %>% group_by(CHR) %>% summarise(median.x = median(cumsum.tmp))
dat <- dat %>% right_join(med.dat)

## real distance
# dat <- gwasResults %>% arrange(CHR, BP) %>% mutate(tmp = diff from start, x = cumsum(tmp))










library(tidyverse)

## default
qp <- ggplot(dat, aes(x = cumsum.tmp, y = -log10(P), colour = factor(CHR))) + 
  stat_manhattan() + 
  geom_hline(yintercept = 8)
print(qp)


## different x axis scheme
qp <- ggplot(dat, aes(x = cumsum.tmp, y = -log10(P), colour = factor(CHR))) + 
  stat_manhattan() + 
  geom_hline(yintercept = 8) + 
  scale_x_continuous(breaks = med.dat$median.x, labels = med.dat$CHR)

print(qp)


## adding nice stuff , like color 
dat <- dat %>% mutate(CHR.bin = CHR %% 2, CHR.col = case_when(
  CHR.bin == 0 ~ "black",
  CHR.bin == 1 ~ "red"
)) ## bin CHR to have alternating colors
## mutate(CHR = case_when(CHR == "X" ~ 23, CHR == "Y" ~ 24, CHR == "MT" ~ 24, TRUE ~ CHR))  ## if X, Y, MT

## like alternating color 
qp <- ggplot(data = NULL) + 
  stat_manhattan(data = dat, aes(x = cumsum.tmp, y = -log10(P), colour = CHR.col, group = CHR.col)) + 
  geom_hline(yintercept = 8)
print(qp) 

## group
qp <- ggplot(dat, aes(x = cumsum.tmp, y = -log10(P), group = factor(CHR))) + 
  stat_manhattan() + 
  geom_hline(yintercept = 8)
print(qp)


## facet
qp <- ggplot(dat, aes(x = cumsum.tmp, y = -log10(P))) +  
              facet_wrap(~ CHR) + 
  stat_manhattan() + 
  geom_hline(yintercept = 8)
print(qp)

