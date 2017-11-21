library(readxl)
library(tidyverse)

##LOOP TO READ IN ALL MONTHS
for (x in c('a','b','c','d','e','f','g','h','i','j','k','l')){
#READ IN EACH MONTH
  did <- read_excel("Tables-2a-2l.xlsx", 
                  sheet = paste0("Table 2",x))
#REMOVE HEADER AND FOOTE5
  did <- did[c(min(which(did[3]!='NA')):nrow(did)),]
  colnames(did) <- did[1,]
  did <- did[c(4:(min(which(did[1]=='IHP'))-1)),] #change from 4 to 5 for All/GP data
#CHANGE REGION CODE TO REGION NAME
  did[1][did[1]=='Y54'] <- 'North of England'
  did[1][did[1]=='Y55'] <- 'Midlands and East of England'
  did[1][did[1]=='Y56'] <- 'London'
  did[1][did[1]=='Y57'] <- 'South of England'
#CHANGE AREA TEAM CODE TO REGION NAME
  did[1][did[1]=='Q44'] <- 'North of England'
  did[1][did[1]=='Q45'] <- 'North of England'
  did[1][did[1]=='Q46'] <- 'North of England'
  did[1][did[1]=='Q47'] <- 'North of England'
  did[1][did[1]=='Q48'] <- 'North of England'
  did[1][did[1]=='Q49'] <- 'North of England'
  did[1][did[1]=='Q50'] <- 'North of England'
  did[1][did[1]=='Q51'] <- 'North of England'
  did[1][did[1]=='Q52'] <- 'North of England'
  did[1][did[1]=='Q53'] <- 'Midlands and East of England'
  did[1][did[1]=='Q54'] <- 'Midlands and East of England'
  did[1][did[1]=='Q55'] <- 'Midlands and East of England'
  did[1][did[1]=='Q56'] <- 'Midlands and East of England'
  did[1][did[1]=='Q57'] <- 'Midlands and East of England'
  did[1][did[1]=='Q58'] <- 'Midlands and East of England'
  did[1][did[1]=='Q59'] <- 'Midlands and East of England'
  did[1][did[1]=='Q60'] <- 'Midlands and East of England'
  did[1][did[1]=='Q64'] <- 'South of England'
  did[1][did[1]=='Q65'] <- 'South of England'
  did[1][did[1]=='Q66'] <- 'South of England'
  did[1][did[1]=='Q67'] <- 'South of England'
  did[1][did[1]=='Q68'] <- 'South of England'
  did[1][did[1]=='Q69'] <- 'South of England'
  did[1][did[1]=='Q70'] <- 'South of England'
  did[1][did[1]=='Q71'] <- 'London'
  #SHA CODE TO REGION NAME
  did[1][did[1]=='Q30'] <- 'North of England'
  did[1][did[1]=='Q31'] <- 'North of England'
  did[1][did[1]=='Q32'] <- 'North of England'
  did[1][did[1]=='Q33'] <- 'Midlands and East of England'
  did[1][did[1]=='Q34'] <- 'Midlands and East of England'
  did[1][did[1]=='Q35'] <- 'Midlands and East of England'
  did[1][did[1]=='Q37'] <- 'South of England'
  did[1][did[1]=='Q38'] <- 'South of England'
  did[1][did[1]=='Q39'] <- 'South of England'
  did[1][did[1]=='Q36'] <- 'London'
#ADD MONTH
  if (x=='a') {
    month <- '01-April'
  } else if (x=='b'){
    month <- '01-May'
  } else if (x=='c'){
    month <- '01-June'
  } else if (x=='d'){
    month <- '01-July'
  } else if (x=='e'){
    month <- '01-August'
  } else if (x=='f'){
    month <- '01-September'
  } else if (x=='g'){
    month <- '01-October'
  } else if (x=='h'){
    month <- '01-November'
  } else if (x=='i'){
    month <- '01-December'
  } else if (x=='j'){
    month <- '01-January'
  } else if (x=='k'){
    month <- '01-Febuary'
  } else if (x=='l'){
    month <- '01-March'
  }
#ADD YEAR
  if (x%in%c('a','b','c','d','e','f','g','h','i')){
    year <- 2012 #change depending on year
  } else{
    year <- 2013
  }
#ALL OR GP DATA
#  for (p in c(4,6,8,10,12)){
#    colnames(did)[p+1] <- colnames(did)[p]
#  }
#  did <- did[-c(5,7,9,11,13)] #remove GP data
#  did <- did[-c(4,6,8,10,12)] #remove all data
#RENAME AND GATHER DATA
   did <- did %>% rename(Code=`Org Code`,Name=`Provider Name`,Region=`SHA Code`) %>% #Region NAME CHANGE DEPENDING ON YEAR
    gather(test,number,-c(Region,Code,Name)) %>%
    cbind(month,year) %>% 
    mutate(number=as.numeric(number)) %>% 
     unite(date,c(month,year),sep = '-')
  #ADD ALL MONTHS TOGETHER
  if(x=='a') {
    all <- did
  } else {
    all <- rbind(all,did)
  }
}

all$date <- as.Date(all$date,'%d-%B-%Y')

##PLOT TRENDS
#BAR GRAPH FACETED BY REGION
all %>% ggplot(aes(test,number))+
  geom_bar(stat = 'identity', col='red')+
  facet_wrap(~Region)+
  theme(axis.text.x = element_text(angle = 60,hjust = 1))
#TIME SERIES TREND OF THE YEAR
all %>% group_by(test,date,Region) %>% 
  summarise(average=mean(number,na.rm=TRUE)) %>%  
  ggplot(aes(date,average,col=test))+
    geom_line()+
    facet_wrap(~Region)+
    theme(axis.text.x = element_text(angle = 60,hjust = 1))+
    theme_bw()+
    labs(title = 'Figure 4: Timeseries of the median number of days waiting for Diagnostic Imaging Tests',
         subtitle = 'Cancer diagnosing tests refered from a GP',
         x = 'Date',
         y = 'Number of Days')
