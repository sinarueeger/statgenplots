QQplot <- function(p, neff = NULL, main = "", col = "black", add = FALSE, ...) {
  p <- p[!is.na(p)]
  N <- length(p)
  
  if (is.null(neff))
  {
    p0 <- sort(-log10((1:N)/N-1/(2*N)))
    col <- ifelse(length(col) > 1, order(-log10((1:N)/N-1/(2*N))), col)
    
  }else
  {
    p0.tmp <- seq(1/neff, 1, length.out = N)
    p0 <- sort(-log10(p0.tmp))
    col <- ifelse(length(col) > 1, order(-log10(p0.tmp)), col)
  }
  
  if(add)
  {
    points(p0,sort(-log10(p)),col=col, pch=16, ...)
  }else{
    plot(p0,sort(-log10(p)),col=col, pch=16,xlab="Expected -log10(P)",ylab="Observed -log10(P)",
         main = main, las = 1, ...)
  }
  
  lines(-log10(p0),-log10(p0),type="l",col=gray(0.3))
}

## Testing
QQplot(runif(10000))

df <- data.frame(P = runif(10000))
p <- ggplot(df, aes(sample = y))
p + stat_qq(distribution = qunif) + stat_qq_line() + scale_x_log10() +
  scale_y_log10()


StatChull <- ggproto("StatChull", Stat,
                     compute_group = function(data, scales) {
                       x <- data$x#[!is.na(data$x)]
                       N <- length(x)
                       
                       ## expected
                      y <- sort(-log10((1:N)/N-1/(2*N)))
                       x<- sort(-log10(x))
                      data.frame(x = x, y = y)
                      
                      },
                     
                     required_aes = c("x")
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


ggplot(df, aes(x = P)) + 
  stat_chull(geom = "point")