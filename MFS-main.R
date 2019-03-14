rm(list=ls(all=TRUE))
setwd("~/GitHub/MacrofaunaSar")
Abu <- read.csv("~/GitHub/MacrofaunaSar/NvsSarGr.csv") # to estimate abundance

### Load libraries
library (ggplot2)
library(plyr) 
library(lsr)
library (pgirmess) 

# Abundance analysis (all taxa together)

Abu$Site = as.factor(Abu$Site)
Abu$Taxa = as.numeric(Abu$Taxa)
Abu$N = as.numeric (Abu$N)
Abu$SarGr=as.numeric (Abu$SarGr)

### Order output
Abu$Zone=factor(Abu$Zone, levels = c("2m", "50m", "500m"))
Abu$Month=factor(Abu$Month, levels = c("Sep", "Oct", "Nov"))


### Abundance (Mean and SD) per Month and Zone
with(Abu, tapply(Nkg,Month, function(x) {
    sprintf("M (SD), = %1.2f (%1.2f)", mean(x), sd(x))
}))

### Plot abundance by month 
ggplot(Abu,aes(x=Month, y = Nkg))+
    geom_boxplot(aes(fill=Month))+
    xlab("Month")+ ylab("Ind/Kg")+
    theme(text = element_text(size=12))+
    scale_fill_manual("legend", values = c("Sep" = "darkblue",
                                       "Oct"="orange", "Nov"="chartreuse4"))

### Plot abundance by zone 
ggplot(Abu,aes(x=Zone, y = Nkg))+
    geom_boxplot(aes(fill=Zone))+
    xlab("Zone")+ ylab("Ind/Kg")+
    theme(text = element_text(size=12))+
    scale_fill_manual("legend", values = c("2m" = "darkblue",
                                           "50m"="orange", "500m"="chartreuse4"))
### Kruskal-Wallis test of Abundance vs Month and Abundance vs Zone
kruskal.test(Abu$Nkg~ Abu$Month) # by Month
kruskalmc(Abu$Nkg~ Abu$Month) 

kruskal.test(Abu$Nkg~ Abu$Zone) # by zone

#Analisis by phylum and taxa
Abu2 <- read.csv("~/GitHub/MacrofaunaSar/AbuMesSitio.csv") #Abundance by month and site

### Order output
Abu2$Zone=factor(Abu2$Zone, levels = c("2m", "50m", "500m"))
Abu2$Month=factor(Abu2$Month, levels = c("Sep", "Oct", "Nov"))
Abu2$Phyla=factor(Abu2$Phyla, levels = c("Arthropoda", "Annelida", "Mollusca", "Chordata","Nemertea", "Nemathelminthes", "Platyhelminthes", "Sipuncula"))

# Abundance per Phyla (standarized 1kg)
Phy<-ddply(Abu2, .(Phyla, Month, Zone), summarise, N= sum(N))
Phy

## Obtain relative abundance per Phyla
Phy$AR<-ifelse(Phy$Month=="Sep" & Phy$Zone =="2m",Phy$N*100/42,
         ifelse(Phy$Month=="Sep" & Phy$Zone =="50m",Phy$N*100/537,
         ifelse(Phy$Month=="Sep" & Phy$Zone =="500m",Phy$N*100/438,
         ifelse(Phy$Month=="Oct" & Phy$Zone =="2m", Phy$N*100/771,
         ifelse(Phy$Month=="Oct" & Phy$Zone =="50m",Phy$N*100/1316,
         ifelse(Phy$Month=="Oct" & Phy$Zone =="500m",Phy$N*100/1992,
         ifelse(Phy$Month=="Nov" & Phy$Zone =="2m",Phy$N*100/2887,
         ifelse(Phy$Month=="Nov" & Phy$Zone =="50m",Phy$N*100/836,
         ifelse(Phy$Month=="Nov" & Phy$Zone =="500m",Phy$N*100/1477,1)))))))))

ggplot(Phy,aes(x=Zone, y = AR, group=Phyla))+
    geom_bar(aes(fill= Phyla), stat = "identity")+
    xlab("Zone")+ ylab("Relative abundance (%)")+
    facet_grid(Month~.)+
    theme(text = element_text(size=12))+
    scale_fill_manual("legend", values = c("Arthropoda" = "darkblue",
                                           "Annelida"="orange", "Mollusca"="chartreuse4", "Chordata" = "black", "Nemertea"="yellow",
                                           "Nemathelminthes"="grey", "Platyhelminthes"="darkgoldenrod4", "Sipuncula" = "darkolivegreen1"))


# Three dominant species
Pd <- Abu2[Abu2$Taxa =="Platynereis dumerilii",]

ggplot(data=Pd) +
    geom_boxplot(aes(x=Month, y=Nkg)) +
    xlab("Zone")+
    ylab("Ind/kg")+
    guides(fill=FALSE)+
    theme(text= element_text(size=18, face = "bold"))+
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

kruskal.test(Pd$Nkg~ Pd$Zone) 

kruskal.test(Pd$Nkg~ Pd$Month) 
kruskalmc(Pd$Nkg~ Pd$Month) 

Lf <- Abu2[Abu2$Taxa =="Latreutes fucorum",]

ggplot(data=Lf) +
    geom_boxplot(aes(x=Zone, y=Nkg)) +
    xlab("Zone")+
    ylab("Ind/kg")+
    guides(fill=FALSE)+
    theme(text= element_text(size=18, face = "bold"))+
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

kruskal.test(Lf$Nkg~ Lf$Zone) 
kruskalmc(Lf$Nkg~ Lf$Zone) 

kruskal.test(Lf$Nkg~ Lf$Month) 
 

Lm <- Abu2[Abu2$Taxa =="Litiopa melanostoma",]

ggplot(data=Lm) +
    geom_boxplot(aes(x=Month, y=Nkg)) +
    xlab("Zone")+
    ylab("Ind/kg")+
    guides(fill=FALSE)+
    theme(text= element_text(size=18, face = "bold"))+
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

kruskal.test(Lm$Nkg~ Lm$Zone) 

kruskal.test(Lm$Nkg~ Lm$Month)
kruskalmc(Lm$Nkg~Lm$Month)
