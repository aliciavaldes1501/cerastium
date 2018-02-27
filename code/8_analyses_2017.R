library(ggplot2)
library(ggthemes)
library(lubridate)
library(lme4)

head(FFD_temp)
str(FFD_temp)

with(FFD_temp,aggregate(id~plot,FUN=length)) #See number of ids per plot
#HC2a-HC2b: merge?

hist(FFD_temp$FFD,breaks=50)
hist(FFD_temp$temp1,breaks=50)

#Differences among plots
ggplot(FFD_temp,aes(x=plot,y=temp1))+ geom_boxplot()+
  xlab("Plot")+ylab("Soil temperature")+theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))
with(FFD_temp,summary(lm(temp1~plot)))
with(FFD_temp,Anova(lm(temp1~plot)))
with(FFD_temp,TukeyHSD(aov(lm(temp1~plot)))) 

ggplot(FFD_temp,aes(x=plot,y=FFD))+ geom_boxplot()+
  xlab("Plot")+ylab("FFD")+theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))
with(FFD_temp,summary(lm(yday(FFD)~plot)))
with(FFD_temp,Anova(lm(yday(FFD)~plot)))
with(FFD_temp,TukeyHSD(aov(lm(yday(FFD)~plot)))) 

#There are differences in mean temperature and FFD among plots

#FFD against soil t (overall + for each plot)
ggplot(FFD_temp,aes(x=temp1,y=FFD))+geom_point(aes(color=plot))+
  geom_smooth(method = "lm",color="black",size=2)+
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))+
  xlab("Soil temperature")+ylab("FFD") #Overall fit

ggplot(FFD_temp,aes(x=temp1,y=FFD))+geom_point(aes(color=plot))+
  geom_smooth(aes(color=plot),method = "lm",size=1.5,se=F,fullrange=F)+
  geom_smooth(method = "lm",color="black",size=2)+
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))+
  xlab("Soil temperature")+ylab("FFD") #Fit for each plot

summary(lm(yday(FFD) ~ temp1, data = FFD_temp)) #Linear regr pooled data, * 
#R2=0.03 with mean_temp
#R2=0.02 with temp1

model_17 <- lm(yday(FFD) ~ temp1*plot,FFD_temp)
summary(model_17) #Different slopes and intercepts
Anova(model_17) #Plot and interaction significant
model2_17 <- lm(yday(FFD) ~ temp1+plot,FFD_temp)
summary(model2_17) #Common slope, different intercepts
Anova(model2_17) #Only plot significant
#Likelihood ratio test comparing the full and reduced models 
anova(model_17,model2_17, test="Chisq") #Support for significant differences between slopes-->keep model_17


anova(model_17) #Type I, one var after the other
anova(lm(yday(FFD) ~ temp1+plot+temp1:plot,FFD_temp))
anova(lm(yday(FFD) ~ plot+temp1+temp1:plot,FFD_temp)) #Different results!

model3_17<-lm(yday(FFD) ~ temp1, data = FFD_temp)
summary(model3_17) #Temp significant

anova(model2_17,model3_17, test="Chisq") #Highly significant differences in intercepts between streams-->keep model2

#Model for effects of plot mean temperatures on mean FFD
FFD_temp_means<-aggregate(cbind(FFD, temp1) ~ plot, data=FFD_temp, FUN=mean)
FFD_temp_means
FFD_temp_means$FFD_date<-as.Date(FFD_temp_means$FFD,origin="1970-01-01")
with(FFD_temp_means,plot(FFD_date~temp1))
with(FFD_temp_means,summary(lm(FFD~temp1))) #p=0.06, p<0.03 with mean_temp
with(FFD_temp_means,abline(lm(FFD~temp1)))
with(FFD_temp_means,text(FFD~temp1, labels=plot, cex= 1,pos=3))




