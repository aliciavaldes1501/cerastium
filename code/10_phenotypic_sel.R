library(MASS)
library(lubridate)
library(visreg)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(effects)
library(RColorBrewer)
library(ggeffects)
library(car)
library(MuMIn)

data17_seeds<-read.table("data/clean/cerastium_2017_seeds.csv",header=T,sep="\t",dec=",")
head(data17_seeds)
str(data17_seeds)
hist(data17_seeds$total_seeds,breaks=100)

data17_seeds$seed_data<-ifelse(is.na(data17_seeds$total_seeds),"no","yes")

FFD_temp_fit<-merge(FFD_temp,data17_seeds[c(1:3,6:7)],all.x=T,all.y=T)

subset(FFD_temp_fit,!is.na(FFD))
subset(FFD_temp_fit,is.na(FFD))

FFD_temp_fit$n_fruits<-as.numeric(with(FFD_temp_fit,ifelse(is.na(FFD),NA,ifelse(!is.na(FFD)&!is.na(n_fr),n_fr,0))))
FFD_temp_fit$n_seeds<-as.numeric(with(FFD_temp_fit,ifelse(n_fruits==0,0,ifelse(n_fruits>0,total_seeds,NA))))
FFD_temp_fit$seeds_per_fr<-FFD_temp_fit$n_seeds/FFD_temp_fit$n_fruits

subset(FFD_temp_fit,!is.na(FFD)&is.na(n_fruits)) #none
subset(FFD_temp_fit,!is.na(FFD)&is.na(n_seeds)) #3 pls w fruit data but no seed data

hist(FFD_temp_fit$n_seeds,breaks=100)
hist(FFD_temp_fit$n_fruits,breaks=100)
hist(FFD_temp_fit$seeds_per_fr,breaks=100)

#Plants with FFD=NA but n_seeds not NA
subset(FFD_temp_fit,is.na(FFD)&!is.na(n_seeds)) #None
#Plants with FFD but n_seeds=NA
subset(FFD_temp_fit,!is.na(FFD)&is.na(n_seeds)) #3 pls (with fruit data)
#Plants with FFD and n_seeds
nrow(subset(FFD_temp_fit,!is.na(FFD)&!is.na(n_seeds))) #297 
#Plants with FFD, n_seeds and temperature
nrow(subset(FFD_temp_fit,!is.na(FFD)&!is.na(n_seeds)&!is.na(temp1))) #297 --> use for phenotypic selection analysis
#Plants with FFD, n_fruits and temperature
nrow(subset(FFD_temp_fit,!is.na(FFD)&!is.na(n_fruits)&!is.na(temp1))) #300 --> use for phenotypic selection analysis

with(FFD_temp_fit,plot(n_seeds~FFD))
with(subset(FFD_temp_fit,n_seeds<10000),plot(n_seeds~FFD))
with(subset(FFD_temp_fit,n_seeds<6000),plot(n_seeds~FFD))

with(FFD_temp_fit,plot(n_seeds~temp1))
with(subset(FFD_temp_fit,n_seeds<10000),plot(n_seeds~temp1))
with(subset(FFD_temp_fit,n_seeds<6000),plot(n_seeds~temp1))

data_peak<-read.table("data/clean/cerastium_2017_peak.csv",header=T,sep="\t",dec=",")
head(data_peak)
str(data_peak)

FFD_temp_fit<-merge(FFD_temp_fit,data_peak[c(2:8)],all.x=T,all.y=T)
head(FFD_temp_fit)

FFD_temp_fit$diam1<-as.numeric(FFD_temp_fit$diam1)
FFD_temp_fit$diam2<-as.numeric(FFD_temp_fit$diam2)
FFD_temp_fit$nfl<-as.numeric(FFD_temp_fit$nfl)
FFD_temp_fit$maxh<-as.numeric(FFD_temp_fit$maxh)

FFD_temp_fit$area<-FFD_temp_fit$diam1*FFD_temp_fit$diam2*pi

#Subsets
subset_seeds<-subset(FFD_temp_fit,!is.na(FFD)&!is.na(n_seeds)) #pls w seed data
subset_seeds_2<-subset(subset_seeds,n_seeds<10000) #Remove one observation with a very large number of seeds
subset_seeds_3<-subset(subset_seeds_2,!is.na(nfl)) #also w fl data

subset_fruits<-subset(FFD_temp_fit,!is.na(FFD)&!is.na(n_fruits)) #pls w fruit data
subset_fruits_2<-subset(subset_fruits,n_fruits<500) #Remove one observation with a very large number of fruits
subset_fruits_3<-subset(subset_fruits_2,!is.na(nfl)) #also w fl data

#Relative fitness and standardized traits for each subset

subset_seeds$n_seeds_rel<-subset_seeds$n_seeds/mean(subset_seeds$n_seeds)
subset_seeds_2$n_seeds_rel<-subset_seeds_2$n_seeds/mean(subset_seeds_2$n_seeds)
subset_seeds_3$n_seeds_rel<-subset_seeds_3$n_seeds/mean(subset_seeds_3$n_seeds)

subset_fruits$n_fruits_rel<-subset_fruits$n_fruits/mean(subset_fruits$n_fruits)
subset_fruits_2$n_fruits_rel<-subset_fruits_2$n_fruits/mean(subset_fruits_2$n_fruits)
subset_fruits_3$n_fruits_rel<-subset_fruits_3$n_fruits/mean(subset_fruits_3$n_fruits)

subset_fruits$seeds_per_fr_rel<-ifelse(!is.na(subset_fruits$seeds_per_fr),subset_fruits$seeds_per_fr/mean(subset_fruits$seeds_per_fr,na.rm=T),NA)
subset_fruits_2$seeds_per_fr_rel<-ifelse(!is.na(subset_fruits_2$seeds_per_fr),subset_fruits_2$seeds_per_fr/mean(subset_fruits_2$seeds_per_fr,na.rm=T),NA)
subset_fruits_3$seeds_per_fr_rel<-ifelse(!is.na(subset_fruits_3$seeds_per_fr),subset_fruits_3$seeds_per_fr/mean(subset_fruits_3$seeds_per_fr,na.rm=T),NA)

subset_seeds$FFD_std<-(yday(subset_seeds$FFD)-mean(yday(subset_seeds$FFD)))/sd(yday(subset_seeds$FFD))
subset_seeds_2$FFD_std<-(yday(subset_seeds_2$FFD)-mean(yday(subset_seeds_2$FFD)))/sd(yday(subset_seeds_2$FFD))
subset_seeds_3$FFD_std<-(yday(subset_seeds_3$FFD)-mean(yday(subset_seeds_3$FFD)))/sd(yday(subset_seeds_3$FFD))

subset_seeds_3$nfl_std<-(subset_seeds_3$nfl-mean(subset_seeds_3$nfl))/sd(subset_seeds_3$nfl)
subset_seeds_3$maxh_std<-(subset_seeds_3$maxh-mean(subset_seeds_3$maxh))/sd(subset_seeds_3$maxh)
subset_seeds_3$medianh_std<-(subset_seeds_3$medianh-mean(subset_seeds_3$medianh))/sd(subset_seeds_3$medianh)
subset_seeds_3$area_std<-(subset_seeds_3$area-mean(subset_seeds_3$area,na.rm=T))/sd(subset_seeds_3$area,na.rm=T)

subset_fruits$FFD_std<-(yday(subset_fruits$FFD)-mean(yday(subset_fruits$FFD)))/sd(yday(subset_fruits$FFD))
subset_fruits_2$FFD_std<-(yday(subset_fruits_2$FFD)-mean(yday(subset_fruits_2$FFD)))/sd(yday(subset_fruits_2$FFD))
subset_fruits_3$FFD_std<-(yday(subset_fruits_3$FFD)-mean(yday(subset_fruits_3$FFD)))/sd(yday(subset_fruits_3$FFD))

subset_fruits_3$nfl_std<-(subset_fruits_3$nfl-mean(subset_fruits_3$nfl))/sd(subset_fruits_3$nfl)




#Phenotypic selection analysis

#Selection via total female fitness

#Linear models

model_sel1<-lm(n_seeds_rel~FFD_std*temp1,data=subset_seeds)
summary(model_sel1)

model_sel2<-lm(n_seeds_rel~FFD_std+temp1,data=subset_seeds) #No interaction
summary(model_sel2)

model_sel3<-lm(n_seeds_rel~FFD_std*temp1,data=subset_seeds_2) #Without possible outlier
summary(model_sel3)

model_sel4<-lm(n_seeds_rel~FFD_std+temp1,data=subset_seeds_2) #No interaction
summary(model_sel4)

cor(subset_seeds_3[c(5,15:18,20)],use="pairwise.complete.obs")
subset_seeds4<-subset(subset_seeds_3,!is.na(area)) #with all traits
model_sel5<-lm(n_seeds_rel~(FFD_std+medianh_std+nfl_std+area_std)*temp1,data=subset_seeds4, na.action = "na.fail") #also with condition traits
# model_sel5<-lm(n_seeds_rel~(FFD_std+medianh_std)*temp1,data=subset_seeds4, na.action = "na.fail") #also with condition traits
summary(model_sel5)
Anova(model_sel5,type="II")
Anova(model_sel5,type="III")
vif(model_sel5)
summary(stats::step(model_sel5))
dredge(model_sel5)
summary(model.avg(dredge(model_sel5), subset = delta < 2)) 

visreg(model_sel5,xvar="FFD_std",by="temp1",breaks=20, overlay=T,band=F)
visreg(model_sel5,xvar="medianh_std",by="temp1",breaks=20, overlay=T,band=F)

visreg2d(model_sel5, "FFD_std", "temp1", plot.type="persp")
visreg2d(model_sel5, "FFD_std", "temp1", plot.type="image")
visreg2d(model_sel5, "medianh_std", "temp1", plot.type="persp")
visreg2d(model_sel5, "medianh_std", "temp1", plot.type="image")

#Visualization of interactions with ggplot

interaction1<-data.frame(effect(term="FFD_std:temp1", mod=model_sel5,
              xlevels=list(temp1=seq(4,33,1), FFD_std=seq(-2.6,3.8,0.1))))
interaction2<-data.frame(effect(term="medianh_std:temp1", mod=model_sel5,
              xlevels=list(temp1=seq(4,33,1), medianh_std=seq(-1.9,5.2,0.1))))

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))

grid.arrange(ggplot(interaction1, aes(FFD_std,fit, group = as.factor(temp1)))+
  geom_smooth(method=lm,se=F,size=0.5,aes(FFD_std,fit,color=temp1))+
  xlab("Standardized FFD")+ylab("Relative number of seeds")+theme_base()+scale_colour_gradientn(colours = myPalette(100))+
  theme(legend.position="top")+labs(colour="Soil temperature")+
  theme(text=element_text(family="serif"))+theme(plot.background=element_rect(fill="white", colour=NA)),
ggplot(interaction2, aes(medianh_std,fit, group = as.factor(temp1)))+
  geom_smooth(method=lm,se=F,size=0.5,aes(medianh_std,fit,color=temp1))+
  xlab("Standardized median height")+ylab("Relative number of seeds")+theme_base()+scale_colour_gradientn(colours = myPalette(100))+
  theme(legend.position="top")+labs(colour="Soil temperature")+
  theme(text=element_text(family="serif"))+theme(plot.background=element_rect(fill="white", colour=NA)),
ncol=2)

#Interaction of height x area? - YES! And height x area x temp!

model_sel6<-lm(n_seeds_rel~(FFD_std+nfl_std+medianh_std*area_std)*temp1,data=subset_seeds4, na.action = "na.fail") #also with condition traits
summary(model_sel6)
summary(stats::step(model_sel6))

visreg(model_sel6,xvar="medianh_std",by="area_std",breaks=20, overlay=T,band=F)
visreg2d(model_sel6, "medianh_std", "area_std", plot.type="image")

interaction3<-data.frame(effect(term="medianh_std:area_std", mod=model_sel6,
              xlevels=list(area_std=seq(-1.3,2.3,0.1), medianh_std=seq(-1.9,5.2,0.1))))

ggplot(interaction3, aes(medianh_std,fit, group = as.factor(area_std)))+
  geom_smooth(method=lm,se=F,size=0.5,aes(medianh_std,fit,color=area_std))+
  xlab("Standardized median height")+ylab("Relative number of seeds")+theme_base()+scale_colour_gradientn(colours = myPalette(100))+
  theme(legend.position="top")+labs(colour="Standardized area")+
  theme(text=element_text(family="serif"))+theme(plot.background=element_rect(fill="white", colour=NA))

interaction4<-data.frame(effect(term="medianh_std:area_std:temp1", mod=model_sel6,
              xlevels=list(area_std=seq(-1.3,2.3,0.1), medianh_std=seq(-1.9,5.2,0.1),temp1=seq(5,35,5))))

ggplot(interaction4, aes(medianh_std,fit, group = as.factor(area_std)))+
  geom_smooth(method=lm,se=F,size=0.5,aes(medianh_std,fit,color=area_std))+
  facet_wrap(~temp1)+
  xlab("Standardized median height")+ylab("Relative number of seeds")+theme_base()+scale_colour_gradientn(colours = myPalette(100))+
  theme(legend.position="top")+labs(colour="Standardized area")+
  theme(text=element_text(family="serif"))+theme(plot.background=element_rect(fill="white", colour=NA))

#Another way of plotting interactions using ggpredict
# interaction4 <- ggpredict(model_sel6, terms = c("medianh_std", 
#                 "area_std [-1.25,-1,-0.75,-0.5,-0.25,0,0.25,0.5,0.75,1,1.25,1.5,1.75,2,2.25]",
#                 "temp1 [5,10,15,20,25,30,35]"))
# 
# ggplot(interaction4, aes(x = x, y = predicted, colour = as.factor(group))) +
#   stat_smooth(method = "lm", se = F, fullrange = T) +
#   facet_wrap(~facet)+
#   xlab("Standardized median height")+ylab("Relative number of seeds")+
#   theme_base()+labs(colour="Standardized area")

#Plots of fitness vs traits and temp
plot(n_seeds_rel~FFD_std,subset_seeds_2)
abline(lm(n_seeds_rel~FFD_std,data=subset_seeds_2))

plot(n_seeds_rel~FFD_std,subset_seeds_3)
abline(lm(n_seeds_rel~FFD_std,data=subset_seeds_3))

plot(n_seeds_rel~area_std,subset_seeds_3)
abline(lm(n_seeds_rel~area_std,data=subset_seeds_3))

plot(n_seeds_rel~nfl_std,subset_seeds_3)
abline(lm(n_seeds_rel~nfl_std,data=subset_seeds_3))

plot(n_seeds_rel~medianh_std,subset_seeds_3)
abline(lm(n_seeds_rel~medianh_std,data=subset_seeds_3))

plot(n_seeds_rel~temp1,subset_seeds_2)
abline(lm(n_seeds_rel~temp1,data=subset_seeds_2))

#models with FFD, temp, FFD*temp and nfl
model_sel7<-lm(n_seeds_rel~FFD_std*temp1+nfl_std,data=subset_seeds_3, na.action = "na.fail")
model_sel7<-lm(n_seeds_rel~FFD_std+temp1+nfl_std,data=subset_seeds_3, na.action = "na.fail")
summary(model_sel7)
summary(stats::step(model_sel7))
Anova(model_sel7,type="II")
Anova(model_sel7,type="III")
vif(model_sel7)
summary(model.avg(dredge(model_sel7), subset = delta < 2)) 

summary(lm(n_seeds_rel~FFD_std*temp1+nfl_std,data=subset_seeds_3, na.action = "na.fail"))
summary(lm(n_seeds_rel~FFD_std+temp1+nfl_std,data=subset_seeds_3, na.action = "na.fail"))
summary(lm(n_seeds_rel~FFD_std+temp1,data=subset_seeds_3, na.action = "na.fail"))

Anova(lm(n_seeds_rel~FFD_std*temp1+nfl_std,data=subset_seeds_3, na.action = "na.fail"))
Anova(lm(n_seeds_rel~FFD_std+temp1+nfl_std,data=subset_seeds_3, na.action = "na.fail"))
Anova(lm(n_seeds_rel~FFD_std+temp1,data=subset_seeds_3, na.action = "na.fail"))

#Differences among plots?
model_sel8<-lm(n_seeds_rel~FFD_std*plot,data=subset_seeds_3, na.action = "na.fail")
summary(model_sel8)
Anova(model_sel8)
summary(stats::step(model_sel8))

Anova(lm(n_seeds_rel~FFD_std*plot,data=subset_seeds_3, na.action = "na.fail"))



#####################################################################################################

#Selection via fitness components: fruit number and number of seeds per fruit

#Fruit number

#Linear models
model_sel8<-lm(n_fruits_rel~FFD_std*temp1+nfl_std,data=subset_fruits_3, na.action = "na.fail")
summary(model_sel8)
summary(stats::step(model_sel8))

#Plots of fitness components vs FFD and temp
plot(n_fruits_rel~FFD_std,subset_fruits_3)
abline(lm(n_fruits_rel~FFD_std,data=subset_fruits_3))

plot(n_fruits_rel~temp1,subset_fruits_3)
abline(lm(n_fruits_rel~temp1,data=subset_fruits_3))

#Number of seeds per fruit...

#...

#Total fitness, with quadratic terms
model_sel9<-lm(n_seeds_rel~(FFD_std+I(FFD_std^2))*(temp1+I(temp1^2))+nfl_std,data=subset_seeds_3)
summary(model_sel9)
summary(stats::step(model_sel9))

#Fruit set...

#...

write.table(FFD_temp_fit,"data/clean/cerastium_2017_FFD_temp_fit.txt",sep="\t",dec=".")

