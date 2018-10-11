
##make manhattan plot
## ggplot(data, aes(x = c(chr, pos), y = P)) 
## > should turn this into

## some copied from here: 
## https://github.com/tidyverse/ggplot2/blob/master/R/stat-qq.r

## Geom QQ Plot
## ------------------
StatManhattan <- ggproto("StatManhattan", Stat,
                         default_aes = aes(y = stat(y), x = stat(x)),
                         
                         required_aes = c("x", "y"),
                         
                      compute_group = function(data, scales){#, dparams = list(),
                                             #  na.rm = FALSE) {
                        
                        ## untangle X1 and X2, when using glue.xaxis(CHR, BP)
                        data <- data %>% tidyr::separate(x, c("x1", "x2"), ":", convert = TRUE)
                        
                        # untangle X1 and X2, when using list
                       # data <- data 
                        
                        ## equidistance
                        data <- data %>% 
                          dplyr::arrange(x1, x2) %>% 
                          dplyr::mutate(temporary = 1) %>% dplyr::mutate(cumsum.tmp = cumsum(temporary))
                        
                   #     data.trans <- data.frame(x = data$cumsum.tmp, y = data$y)
                      data.trans <- data.frame(x = data$cumsum.tmp, y = data$y)
                        return(data.trans)
                        ## real distance
                        ## dat <- gwasResults %>% arrange(CHR, BP) %>% mutate(tmp = diff from start, x = cumsum(tmp))
                       
                        ## new x axis
                        ## med.dat <- data %>% group_by(group) %>% summarise(median.x = median(cumsum.tmp))
                       
                      }
)


stat_manhattan <- function(mapping = NULL, data = NULL, geom = "point",
                       position = "identity", na.rm = TRUE, show.legend = NA, 
                       inherit.aes = TRUE, ...) {#, dparams = list()
  layer(
    stat = StatManhattan, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes#, 
   # params = list(
   #   dparams = dparams,
   #   na.rm = na.rm,
  #    ...
   # )
    )
}

          
geom_manhattan <- stat_manhattan

## Example
## ------------
library(tidyverse)
glue.xaxis <- function(x, y) paste(x, y, sep = ":")
# glue.xaxis(CHR, BP)
calc.cumsum <- function(data) data %>% dplyr::arrange(CHR, BP) %>% dplyr::mutate(temporary = 1) %>% dplyr::mutate(cumsum.tmp = cumsum(temporary))

dat <- qqman::gwasResults %>% filter(P < 0.05)

## default
qp <- ggplot(data = dat) +
  geom_manhattan(aes(x = glue.xaxis(CHR, BP), y = -log10(P))) + 
  geom_hline(yintercept = 8) + 
  theme_bw()
print(qp)

datgg <- ggplot_build(qp)



## how it should look like
ggplot(calc.cumsum(dat)) + 
  geom_point(aes(x = cumsum.tmp, y = -log10(P))) + 
  geom_hline(yintercept = 8)



## why characters
## why not working when "chrpos"
## how to add x1 and x2

data <- qqman::gwasResults %>% filter(P < 0.05) %>% mutate(x = glue.xaxis(CHR, BP)) %>% separate(x, c("x1", "x2"), ":", convert = TRUE)

## equidistance
data <- data %>% 
  dplyr::arrange(x1, x2) %>% 
  dplyr::mutate(temporary = 1) %>% dplyr::mutate(cumsum.tmp = cumsum(temporary))
str(data)








## default + color
qp <- ggplot(dat, aes(x = x, y = -log10(P), colour = factor(CHR))) + 
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

