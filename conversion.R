# Hydra length to age conversion
library(tidyverse)
library(here)
library(ggplot2)
library(DLMtool) # data limited methods

load(here("hydraDataList_msk.rda"))
species <- data.frame(name=hydraDataList_msk$speciesList, species= c(1:10))
K <-as.numeric(hydraDataList_msk$growthK)
Linf <- as.numeric(hydraDataList_msk$growthLinf)
t0 <- 0 # is there something else I should be using here?
vonbert <- data.frame(species,K, Linf,t0)

# plot curves
ages <- -1:40
L<- matrix(NA, nrow=length(ages), ncol=length(species$name))

for (i in 1:length(species$name)) {
  for (a in 1:length(ages)) {
  L[a,i]<- Linf[i]*(1-exp(-K[i]*(a-t0)))
  }
}

Lengths <- as.data.frame(L)
colnames(Lengths)<-species$name

for (i in species$name) {
  plot<-ggplot2::ggplot()+geom_line(aes(y=Lengths[[i]], x=ages))+
    labs(y=i)
  setwd(here("conversions"))
 ggsave(plot, file=paste0(i,".png"))
  }

# size bins
binwidth <- hydraDataList_msk$binwidth%>%
  cbind(species)

# map out what bins are representing 
sizebin0 <- 0
sizebin1 <- hydraDataList_msk$binwidth$sizebin1
sizebin2 <- bin1 + hydraDataList_msk$binwidth$sizebin2 
sizebin3 <- bin2 + hydraDataList_msk$binwidth$sizebin3
sizebin4 <- bin3 + hydraDataList_msk$binwidth$sizebin4
sizebin5 <- bin4 + hydraDataList_msk$binwidth$sizebin5
sizebin <- as.data.frame(cbind(sizebin0,sizebin1, sizebin2, sizebin3, sizebin4, sizebin5, species))%>%
  full_join(vonbert)
  
  
sizebins <- as.data.frame(cbind(sizebin0,sizebin1, sizebin2, sizebin3, sizebin4, sizebin5, species))%>%
gather(bin,endbin, sizebin0:sizebin5)%>%
  group_by(species)%>%
  mutate(startbin= lag(endbin))%>%
  filter(bin!="sizebin0")


# calculate the number of fish in each bin- Problem this is not an integer
sum(hydraDataList_msk$observedSurvSize$inpN)
observedSurvSize<- hydraDataList_msk$observedSurvSize
data <- gather(observedSurvSize, bin, prop, sizebin1:sizebin5)%>%
  mutate(N= round(inpN*prop))%>% #fix by rounding?
  full_join(sizebins, by=c("species", "bin"))%>%
  full_join(vonbert)


# Problem- Linf is smaller than sizebin5 end bin so fish are being assign lengths outside their possible length
summary <- data%>%
  group_by(species,name)%>%
  summarise(across(c(endbin,Linf), max))

new_end<-data%>%
  rowwise()%>%
  mutate(end = min(endbin, Linf)) #fix by choosing whichever is smallest?

#repeat rows based on N- each individual has its own row
rep <- as.data.frame(lapply(new_end, rep, dist$N))

# some start bins are larger than Linf which will cause errors for calculating length
check <- rep%>%
  filter(startbin>end)

test <- anti_join(rep, check)  # leave out problem lengths

# assign length with random uniform distribution across bins
lengths <- test%>%
  rowwise()%>%
  mutate(L=runif(1, min=startbin, max=end))
summary(lengths)


# convert length at age using inverse Von Bert
age <- lengths%>%
  rowwise()%>%
  mutate(age=ceiling(((-log(1-L/Linf))/K+t0))) #round up

# plot age distributions
ggplot(age, aes(age))+ geom_histogram(binwidth=1)+ facet_wrap(vars(name), scales="free")

# summary stats for each species
stats <- age%>%
  group_by(name)%>%
  summarise(as_tibble(rbind(summary(age))))

counts <- age%>%
  group_by(name)%>%
  count(age)

