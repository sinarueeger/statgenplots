## old code
## ---------

# QQplot <- function(p, neff = NULL, main = "", col = "black", add = FALSE, ...) {
#   p <- p[!is.na(p)]
#   N <- length(p)
#   
#   if (is.null(neff))
#   {
#     p0 <- sort(-log10((1:N)/N-1/(2*N)))
#     col <- ifelse(length(col) > 1, order(-log10((1:N)/N-1/(2*N))), col)
#     
#   }else
#   {
#     p0.tmp <- seq(1/neff, 1, length.out = N)
#     p0 <- sort(-log10(p0.tmp))
#     col <- ifelse(length(col) > 1, order(-log10(p0.tmp)), col)
#   }
#   
#   if(add)
#   {
#     points(p0,sort(-log10(p)),col=col, pch=16, ...)
#   }else{
#     plot(p0,sort(-log10(p)),col=col, pch=16,xlab="Expected -log10(P)",ylab="Observed -log10(P)",
#          main = main, las = 1, ...)
#   }
#   
#   lines(-log10(p0),-log10(p0),type="l",col=gray(0.3))
# }
# 
# QQplot(df$P)
# abline(a=0, b = 1)

## Testing





## Geom QQ Plot
## ------------------
StatQQplot <- ggproto("StatQQplot", Stat,
                     compute_group = function(data, scales) {
                       y <- data$y#[!is.na(data$x)]
                       N <- length(y)
                       
                      ## expected
                      expected <- sort(-log10((1:N)/N-1/(2*N)))
                      observed <- sort(-log10(y))
                      data.frame(x = expected, y = observed)
                      
                      },
                     
                     required_aes = c("y")
                     #setup_data = function(data, params) {
                   #    data.frame(x = data$expected, y = data$observed)
                  #   },
                  #   setup_params = function(data, params) {
                  #     min <- 0
                  #     max <- max(c(data$y, data$x))
                  #     
                  #     list(
                  #       min = min,
                  #       max = max,
                  #       na.rm = params$na.rm,
                  #       xlab = "sdfsdf",
                  #       ylab = "sdfsdfsdf"
                  #     )
                  #   }
)

stat_qqplot <- function(mapping = NULL, data = NULL, geom = "point",
                       position = "identity", na.rm = FALSE, show.legend = NA, 
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatQQplot, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


## Geom Inflation factor
## ------------------
# https://academic.oup.com/hmg/article/17/R2/R122/2527210
lambda_gc <- function(x)
{
  # x = P values
  ## turn P values into Zstats, then square them
  chisq <- qnorm(x/2)^2
  
  lambda <- median(chisq)/qchisq(0.5,1)
  return(lambda)
}
  

StatLambdaGC <- ggproto("StatLambdaGC", Stat,
                      compute_group = function(data, scales) {
                       gc.val = lambda_gc(data$y)
                       label = glue::glue("lambda = {format(gc.val, digits = 3)}")
                       data.frame(x = 1, y = 1, label = label)
                      },
                      
                      required_aes = c("y")
)

stat_lambda_gc <- function(mapping = NULL, data = NULL, geom = "text",
                        position = "identity", na.rm = FALSE, show.legend = NA, 
                        inherit.aes = TRUE, parse = TRUE, hjust = "left", vjust = "outward", ...) {
  layer(
    stat = StatLambdaGC, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, parse = parse, 
                  hjust = hjust, vjust = vjust, ...)
  )
}


## Example



n.sample <- 10000
df <- data.frame(P = runif(n.sample), GWAS = sample(c("a","b"), n.sample, replace = TRUE))

## default
qp <- ggplot(df, aes(y = P)) + 
  stat_qqplot() + 
  geom_abline(intercept = 0, slope = 1) 
print(qp)

## adding nice stuff
qp + 
    theme(aspect.ratio=1) + ## square shaped
    expand_limits(x = -log10(max(df$P)), y = -log10(max(df$P))) + ## identical limits (meaning truely square)
    ggtitle("QQplot") + ## title
    xlab("Expected -log10(P)") + ## axis labels
    ylab("Observed -log10(P)")
  
## add GC 
ggplot(df, aes(y = P)) + 
    stat_qqplot() + 
    geom_abline(intercept = 0, slope = 1) + 
    stat_lambda_gc()
    
## color 
ggplot(df, aes(y = P, color = GWAS)) + 
    stat_qqplot() + 
    geom_abline(intercept = 0, slodpe = 1) + 
 
## group
ggplot(df, aes(y = P, group = GWAS)) + 
    stat_qqplot() + 
    geom_abline(intercept = 0, slope = 1) + 
    
## facet
qp +  facet_wrap(~GWAS) + 
  stat_lambda_gc()
