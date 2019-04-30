# ASQ_pilot
modified: "26/04/2019"

pilot ASQ analyses for alcohol grant

### Cool keyboard shortcuts
opt + arrow moves a whole line up or down

To see all the native colors in R type

* colors()


To create custom color paletes use these commands:

* library("colorspace") 
* pal <- choose_palette()

### Multi-pane charts/graphs
From:
https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html

The easiest approach to assemble multiple plots on a page is to use the grid.arrange() function from the gridExtra package; in fact, that’s what we used for the previous figure. With grid.arrange(), one can reproduce the behaviour of the base functions par(mfrow=c(r,c)), specifying either the number of rows or columns,

grid.arrange(p1, p2, nrow = 1)



###	Facets
From:
https://flowingdata.com/2016/03/22/comparing-ggplot2-and-r-base-graphics/

This is by far the most useful part of ggplot2, and if I use the package again it will be for facets. During exploratory data analysis, you often need to create graphs of various categories. 
