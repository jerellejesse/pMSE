get_lengthConvert <- function(hydraData){
  
  # we want each species as a sub-list like stock object in groundfish MSE
  
  # assign length with random uniform distribution across bins
  Survlengths <- rowwise(hydraData$observedSurSize)%>%
    mutate(L=runif(1, min=startbin, max=end))
  
  Catchlengths <- rowwise(hydraData$observedCatchSize)%>%
    mutate(L=runif(1, min=startbin, max=end))
  
  # convert length at age using inverse Von Bert
  Survage <- Survlengths%>%
    rowwise()%>%
    mutate(age=ceiling(((-log(1-L/Linf))/K+t0)))%>% #round up
    filter(species==1)
  
  Catchage <- Catchlengths%>%
    rowwise()%>%
    mutate(age=ceiling(((-log(1-L/Linf))/K+t0)))%>% #round up
    filter(species==1)%>%
    select(fishery, year,species, name, age)
  
  # create plus group
  Survage$age[Survage$age>10]<- 10
  Catchage$age[Catchage$age>10]<- 10  
  
  # calculate proportions at age and bring annual total data back in
  paaSurv <-  Survage%>%
    select(survey, year,species, name, age)%>%
    group_by(survey, year, species, name, age)%>%
    count()%>%
    group_by(survey, year, species, name)%>%
    mutate(total=sum(n), paa=n/total)%>%
    select(survey, year,species,name,age,paa)%>%
    spread(age,paa)#%>% 
    #as.tibble()
    #as.list()
 matrix(paaSurv$paa, ncol=42) # probably need the same number of ages for each year/ species
      
  paaCN <- Catchage%>%
    select(fishery, year,species, name, age)%>%
    group_by(fishery, year, species, name, age)%>%
    count()%>%
    group_by(fishery, year, species, name)%>%
    mutate(total=sum(n), paa=n/total)%>%
    select(fishery, year,species,name,age,paa)%>%
    spread(age,paa)
    #spread(age,paa) %>%
    #group_by(name)%>%
    #group_split()
  
# fill in NA with zero
paaSurv[is.na(paaSurv)]<-0
paaCN[is.na(paaCN)]<- 0

cod <- list(paaIN=filter(paaSurv, survey==1),
            paaCN=paaCN,
            sumIN=filter(hydraDataList_msk$observedBiomass, species==1 & survey==1),
            sumCW=filter(hydraDataList_msk$observedCatch, species==1))

}
