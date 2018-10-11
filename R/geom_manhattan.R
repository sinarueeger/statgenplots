
##make manhattan plot
## ggplot(data, aes(x = c(chr, pos), y = P)) 
## > should turn this into

## some copied from here: 
## https://github.com/tidyverse/ggplot2/blob/master/R/stat-qq.r


## real distance
## dat <- gwasResults %>% arrange(CHR, BP) %>% mutate(tmp = diff from start, x = cumsum(tmp))

## new x axis
## med.dat <- data %>% group_by(group) %>% summarise(median.x = median(cumsum.tmp))
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
                         
                         required_aes = c("y", "x2", "x1"),
                         default_aes = aes(y = stat(y), x1 = stat(x1), x2 = stat(x2))
                         
)

stat_manhattan <- function(mapping = NULL, data = NULL, geom = "point",
                           position = "identity", na.rm = FALSE, show.legend = NA, 
                           inherit.aes = TRUE, ...) { #, dparams = list()
  layer(
    stat = StatManhattan, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
    # params = list(
    #   dparams = dparams,
    #   na.rm = na.rm,
    #    ...
    # )
  )
}


## Example
## ------------
dat <- qqman::gwasResults %>% filter(P < 0.05)#, chr="CHR", bp="BP", snp="SNP", p="P" )

## default
qp <- ggplot(dat) + 
  stat_manhattan(aes(x2 = BP, y = -log10(P), x1 = CHR)) + 
  geom_hline(yintercept = 8) + 
  ggtitle("sfsdfsdf")
print(qp)



## how it should look like
calc.cumsum <- function(data) data %>% dplyr::arrange(CHR, BP) %>% dplyr::mutate(temporary = 1) %>% dplyr::mutate(cumsum.tmp = cumsum(temporary))
ggplot(calc.cumsum(dat)) + 
  geom_point(aes(x = cumsum.tmp, y = -log10(P))) + 
  geom_hline(yintercept = 8)

datgg <- ggplot_build(qp)




# 
# 
# 
# 
# ## why characters
# ## why not working when "chrpos"
# ## how to add x1 and x2
# 
# data <- qqman::gwasResults %>% filter(P < 0.05) %>% mutate(x = glue.xaxis(CHR, BP)) %>% separate(x, c("x1", "x2"), ":", convert = TRUE)
# 
# ## equidistance
# data <- data %>% 
#   dplyr::arrange(x1, x2) %>% 
#   dplyr::mutate(temporary = 1) %>% dplyr::mutate(cumsum.tmp = cumsum(temporary))
# str(data)
# 
# 
# 
# 
# 
# 
# 
# 
# ## default + color
# qp <- ggplot(dat, aes(x = x, y = -log10(P), colour = factor(CHR))) + 
#   stat_manhattan() + 
#   geom_hline(yintercept = 8)
# print(qp)
# 
# 
# ## different x axis scheme
# qp <- ggplot(dat, aes(x = cumsum.tmp, y = -log10(P), colour = factor(CHR))) + 
#   stat_manhattan() + 
#   geom_hline(yintercept = 8) + 
#   scale_x_continuous(breaks = med.dat$median.x, labels = med.dat$CHR)
# 
# print(qp)
# 
# 
# ## adding nice stuff , like color 
# dat <- dat %>% mutate(CHR.bin = CHR %% 2, CHR.col = case_when(
#   CHR.bin == 0 ~ "black",
#   CHR.bin == 1 ~ "red"
# )) ## bin CHR to have alternating colors
# ## mutate(CHR = case_when(CHR == "X" ~ 23, CHR == "Y" ~ 24, CHR == "MT" ~ 24, TRUE ~ CHR))  ## if X, Y, MT
# 
# ## like alternating color 
# qp <- ggplot(data = NULL) + 
#   stat_manhattan(data = dat, aes(x = cumsum.tmp, y = -log10(P), colour = CHR.col, group = CHR.col)) + 
#   geom_hline(yintercept = 8)
# print(qp) 
# 
# ## group
# qp <- ggplot(dat, aes(x = cumsum.tmp, y = -log10(P), group = factor(CHR))) + 
#   stat_manhattan() + 
#   geom_hline(yintercept = 8)
# print(qp)
# 
# 
# ## facet
# qp <- ggplot(dat, aes(x = cumsum.tmp, y = -log10(P))) +  
#               facet_wrap(~ CHR) + 
#   stat_manhattan() + 
#   geom_hline(yintercept = 8)
# print(qp)
# 
