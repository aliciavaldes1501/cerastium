library(Rcmdr)
library(dplyr)

#Read data on flowering stages and temperatures
data17_stages<-read.table("data/clean/cerastium_2017_stages.csv",header=T,sep="\t",dec=",")
head(data17_stages)
str(data17_stages)
data17_stages$temp<-as.numeric(sub(",", ".", as.character(data17_stages$temp), fixed = TRUE))

with(data17_stages,aggregate(id~plot,FUN=length)) #See number of ids per plot

data17_stages$date<-as.Date(as.character(data17_stages$date),format="%d/%m/%Y")

# Flowering stages for Cerastium fontanum:	
#   Stage	Description
# VS (Vegetative: small)	Only vegetative growth, the plant is < 2 cm
# VL (Vegetative: large)	Only vegetative growth, the plant is > 2 cm
# B1 (Bud stage 1)	Buds are just starting to form, very small
# B2 (Bud stage 2)	Buds are at medium size
# B3 (Bud stage 3)	Buds are large but still completely closed
# B4 (Bud stage 4)	Buds are large and almost starting to flower
# FL (Flowering)	At least one flower has opened
# FL100 (100% flowering)	All flowers have opened, none are yet wilted
# W (Wilted)	At least one flower has wilted
# W100 (100% wilted)	All flowers are wilted
# 
# Stages to account for grazing damage and lost or dead plants:	
#   Stage	Description
# X (Lost)	Neither plant nor nail was not found that day
# D (Dead)	Plant still in place but dead
# G (Grazed/gone)	Nail was found but no sign of plant
# SG (Stem grazed/gone)	All stems grazed/gone but with visible stubs
# /G (Grazing damage)	One or more stems have been grazed but there are still some left, phenology is measured for the one that is most mature

levels(data17_stages$stage) #See the levels

#Correct stage values
data17_stages$stage_corr<-as.character(data17_stages$stage)
data17_stages$stage_corr[data17_stages$stage_corr=="B1/G"] <- "B1"
data17_stages$stage_corr[data17_stages$stage_corr=="B1/SG"] <- "B1"
data17_stages$stage_corr[data17_stages$stage_corr=="B2/G"] <- "B2"
data17_stages$stage_corr[data17_stages$stage_corr=="B3/G"] <- "B3"
data17_stages$stage_corr[data17_stages$stage_corr=="B4/G"] <- "B4"
data17_stages$stage_corr[data17_stages$stage_corr=="FL/G"] <- "FL"
data17_stages$stage_corr[data17_stages$stage_corr=="VL "] <- "VL"
data17_stages$stage_corr[data17_stages$stage_corr=="VL/G"] <- "VL"
data17_stages$stage_corr[data17_stages$stage_corr=="W "] <- "W"
data17_stages$stage_corr[data17_stages$stage_corr=="W/G"] <- "W"
data17_stages$stage_corr[data17_stages$stage_corr=="W/SG"] <- "W"
data17_stages$stage_corr[data17_stages$stage_corr=="W10"] <- "W100"
data17_stages$stage_corr[data17_stages$stage_corr=="W100 "] <- "W100"
data17_stages$stage_corr[data17_stages$stage_corr=="W100/G"] <- "W100"

data17_stages$stage_corr[data17_stages$stage_corr=="comment"] <- "wrong"
data17_stages$stage_corr[data17_stages$stage_corr=="D"] <- "wrong"
data17_stages$stage_corr[data17_stages$stage_corr=="G"] <- "wrong"
data17_stages$stage_corr[data17_stages$stage_corr=="missing data"] <- "wrong"
data17_stages$stage_corr[data17_stages$stage_corr=="SG"] <- "wrong"
data17_stages$stage_corr[data17_stages$stage_corr=="WRONG"] <- "wrong"
data17_stages$stage_corr[data17_stages$stage_corr=="wrong sp/"] <- "wrong"
data17_stages$stage_corr[data17_stages$stage_corr=="wrong species"] <- "wrong"
data17_stages$stage_corr[data17_stages$stage_corr=="X"] <- "wrong"
data17_stages$stage_corr[data17_stages$stage_corr=="X "] <- "wrong"
data17_stages$stage_corr[data17_stages$stage_corr=="X/G"] <- "wrong"

data17_stages$stage_corr<-as.factor(data17_stages$stage_corr)
levels(data17_stages$stage_corr)
nrow(data17_stages)

#To remove
subset(data17_stages,stage_corr=="wrong")
nrow(subset(data17_stages,stage_corr=="wrong")) #287 rows
unique(subset(data17_stages,stage_corr=="wrong")$id) #from 104 unique ids

#Select data with meaningful stages
data17_stages_complete<-subset(data17_stages,!stage_corr=="wrong")
nrow(data17_stages_complete)

data17_stages_complete$stage_corr<-droplevels(data17_stages_complete$stage_corr)
levels(data17_stages_complete$stage_corr)

#Calculate stage in numeric
data17_stages_complete <- within(data17_stages_complete, {
  stage_num <- Recode(stage_corr, 
               '"VS"=1; "VL"=2; "B1"=3; "B2"=4; "B3"=5; "B4"=6; "B5"=7; "FL"=8; "FL100"=9; "W"=10; "W50"=11; "W100"=12', 
               as.factor.result=F)})
str(data17_stages_complete)
hist(data17_stages_complete$stage_num)

subset(data17_stages_complete,stage_num==7) #Only 1, recode?
subset(data17_stages_complete,stage_num==9) #Only 6, recode?
subset(data17_stages_complete,stage_num==11) #Only 10, recode?

with(data17_stages_complete,aggregate(id~plot,FUN=length)) #See number of ids per plot
with(subset(data17_stages_complete,stage_num>=8),aggregate(id~plot,FUN=length)) #See number of ids per plot that actually flowered

#Calculate flowering/not flowering as 1/0
data17_stages_complete <- within(data17_stages_complete, {
  stage_bin <- Recode(stage_corr, 
               '"VS"=0; "VL"=0; "B1"=0; "B2"=0; "B3"=0; "B4"=0; "B5"=0; "FL"=1; "FL100"=1; "W"=1; "W50"=1; "W100"=1', 
               as.factor.result=F)})

# last_nfl=max date when stage_bin=0 - last date as vegetative
# first_fl=min data when stage_bin=1 - first date as flowering

last_nfl<-data.frame(subset(data17_stages_complete,stage_bin==0) %>% 
  group_by(id) %>%   
  summarise(max_date= max(date)))

first_fl<-data.frame(subset(data17_stages_complete,stage_bin==1) %>% 
  group_by(id) %>%   
  summarise(min_date= min(date)))

nrow(last_nfl) #417
nrow(first_fl) #300

head(last_nfl)
head(first_fl)

FFD<-merge(last_nfl,first_fl,all.x=T,all.y=T)
FFD$FFD<-(FFD$min_date-FFD$max_date)
FFD$FFD<-as.Date(rowMeans(cbind(FFD$min_date,FFD$max_date)),origin="1970-01-01") #FFD mid-interval
head(FFD)

# Mean temperature for each plant
temp<-data.frame(data17_stages_complete %>% 
    group_by(id) %>%   
    summarise(mean_temp= mean(temp,na.rm=T),min_temp= min(temp,na.rm=T),max_temp= max(temp,na.rm=T),
    sd_temp=sd(temp,na.rm=T)))
temp$diff_temp<-temp$max_temp-temp$min_temp
hist(temp$diff_temp)

nrow(temp) #417 pls w temp
nrow(subset(temp,diff_temp>5)) #54 pls w temp diff > 5 - 13%
nrow(subset(temp,diff_temp>10)) #5 pls w temp diff >10 - 1%

#Add temperature in first recording
temp<-merge(temp,subset(data17_stages_complete,record==1)[c(2,9)])
temp$temp1<-temp$temp
temp$temp<-NULL
head(temp)

#Merge data FFD and temperature

FFD_temp<-merge(FFD[c(1,4)],temp[c(1:2,7)])
FFD_temp<-merge(FFD_temp,unique(data17_stages_complete[2:3])) #Add plot
head(FFD_temp)
str(FFD_temp)

write.table(FFD_temp,"data/clean/cerastium_2017_FFD_temp.txt",sep="\t",dec=".")


