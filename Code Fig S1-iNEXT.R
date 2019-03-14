setwd("~/GitHub/MacrofaunaSar")
Abu <- read.csv("~/GitHub/MacrofaunaSar/iNEXT.csv") # data by zone
Abum <- read.csv("~/GitHub/MacrofaunaSar/iNEXT-Month.csv") # data by month

### Load libraries
library (ggplot2)
library(iNEXT)

###Graphs Species richness (type=1)
out1 <-iNEXT(Abu[,c("Total")], q=0, datatype="abundance", size=NULL, endpoint=NULL, knots=40, se=TRUE, conf=0.95, nboot=500)
out1 # print values
ggiNEXT(out1, type=1,  grey=TRUE) + theme_bw(base_size=18)# graph Species diverstity vs N

### Species diversity by zone (Richness, Shannon, Simpson)
out1 <- iNEXT(Abu[,c("X2m","X50m", "X500m")], q=0, datatype="abundance", size=NULL, endpoint=NULL, knots=40, se=TRUE, conf=0.95, nboot=500) 
out1 # print values

# Species diversity by month (Richness, Shannon, Simpson)
out1 <- iNEXT(Abum[,c("Sep","Oct", "Nov")], q=0, datatype="abundance", size=NULL, endpoint=NULL, knots=40, se=TRUE, conf=0.95, nboot=50) 
out1 # print values


