library(ggplot2)
library(plotly)
m=mtcars
head(m)
str(m)
m$cyl=as.factor(m$cyl)
m$gear=as.factor(m$gear)
