# Hydra length to age conversion
library(tidyverse)
library(here)
library(ggplot2)

load("hydraDataList_msk.rda")
species <- hydraDataList_msk$speciesList
K <-as.numeric(hydraDataList_msk$growthK)
Linf <- as.numeric(hydraDataList_msk$growthLinf)
t0 <- -1
ages <- -1:20

L<- matrix(NA, nrow=length(ages), ncol=length(species))

for (i in 1:length(species)) {
  for (a in 1:length(ages)) {
  L[a,i]<- Linf[i]*(1-exp(-K[i]*(a-t0)))
  }
}
  

Lengths <- as.data.frame(L)
colnames(Lengths)<-species

for (i in species) {
  plot<-ggplot2::ggplot()+geom_line(aes(y=Lengths[[i]], x=ages))+
    labs(y=i)
  setwd(here("conversions"))
 ggsave(plot, file=paste0(i,".png"))
  }

# size bins
binwidth <- hydraDataList_msk$binwidth

#map out what bins are representing 
bin1 <- hydraDataList_msk$binwidth$sizebin1
bin2 <- bin1 + hydraDataList_msk$binwidth$sizebin2 
bin3 <- bin2 + hydraDataList_msk$binwidth$sizebin3
bin4 <- bin3 + hydraDataList_msk$binwidth$sizebin4
bin5 <- bin4 + hydraDataList_msk$binwidth$sizebin5

sizebins <- as.data.frame(cbind(bin1, bin2, bin3, bin4, bin5), row.names = species)

#what age structure would we want for each species

