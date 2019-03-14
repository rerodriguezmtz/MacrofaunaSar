setwd("~/GitHub/MacrofaunaSar")
Abu <- read.csv("~/GitHub/MacrofaunaSar/HMMFS3.csv") # data by zone

# Load Libraries
library(gplots)  # for heatmap.2
library (vegan)  # for hierachical clustering
library(RColorBrewer)

# Obtain matrix
row.names(Abu) <- Abu$Code
Abu <- Abu[, -1]
Abu.prop <- Abu/rowSums(Abu)

# make the heatmap
Abu.prop[Abu.prop==0] = NA # for cero cells to appear in white

#heatmap color
heatmap.2(as.matrix(Abu.prop), Rowv = FALSE, Colv=FALSE,
col = colorRampPalette(c("gold", "darkred")), margins = c(15, 6),
trace = "none", density.info = "none", xlab = "Taxa", ylab = "Month-Zone",
lhei = c(2, 8))

#heatmap grey
heatmap.2(as.matrix(Abu.prop), Rowv = FALSE, Colv=FALSE,
          col = colorRampPalette(c("grey", "black")), margins = c(15, 6),
          trace = "none", density.info = "none", xlab = "Taxa", ylab = "Month-Zone",
          lhei = c(2, 8))

