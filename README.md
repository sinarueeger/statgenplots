# statgenplots

*work-in-progress*

An R-Package (soon ;-) that implements QQplot and Manhattanplot as a geom in `ggplot2`.

This is similar to the package [`qqman`](http://www.gettinggeneticsdone.com/2014/05/qqman-r-package-for-qq-and-manhattan-plots-for-gwas-results.html), except that it should have the look and functionality of `ggplot2`.

## Functionality

Here is what the functions should do. 

Let's say we have GWAS summary statistics (P-value) for a number of SNPs (rowwise). That would look like this:
`
SNPid   P
rs3342  1e-2
rs83    1e-2
`

What we want is first, a [Q-Q-plot](https://en.wikipedia.org/wiki/Q%E2%80%93Q_plot) representation of the P-values. Something like this. 

Secondly, we want a [Manhattan plot](https://en.wikipedia.org/wiki/Manhattan_plot).

Reference-style: 
![alt text][https://en.wikipedia.org/wiki/Manhattan_plot#/media/File:Manhattan_Plot.png]

[logo]: https://en.wikipedia.org/wiki/Manhattan_plot#/media/File:Manhattan_Plot.png "Logo Title Text 2"



https://www.nature.com/articles/ncomms5757/figures/2


## Inspiration

- https://www.r-graph-gallery.com/wp-content/uploads/2018/02/Manhattan_plot_in_R.html

