#Relationship between mother+site and fl phen in the c. garden####
head(data_cgarden_short)
hist(data_cgarden_short$first_fl,breaks=40)

library(lubridate)
hist(yday(data_cgarden_short$first_fl))
plotNormalHistogram(yday(data_cgarden_short$first_fl)) #Quite normal
plotNormalHistogram(log(yday(data_cgarden_short$first_fl))) #More close to normal!

data_cgarden_short$patch_new<-as.factor(paste(data_cgarden_short$stream,data_cgarden_short$patch,sep="_"))
data_cgarden_short$first_fl_j<-yday(data_cgarden_short$first_fl)

mod5<-lm(first_fl_j ~ stream/mother_pl_id_new, data = data_cgarden_short)
summary(mod5) 
Anova(mod5) #Effect of mother and stream 
plot(mod5)
hist(residuals(mod5))

subset_data<-subset(data_cgarden_short,!is.na(first_fl_j))

mod6<-lmer(first_fl_j ~ stream+(1|mother_pl_id_new), REML=T,data = subset_data)
mod6<-lme(first_fl_j ~ stream, random=~1|mother_pl_id_new, method="ML",data =subset_data)
mod7<-lme(first_fl_j ~ 1, random=~1|mother_pl_id_new, method="ML", data = subset_data)
anova(mod6,mod7) #mod6 is better

summary(mod6)
Anova(mod6) #Effect of stream
#Maybe better to treat mother as random, cause we are not specifically interested in all these mothers
#Test significance of random effect of mother?
mod7<-lm(first_fl_j ~ stream, data = subset_data)

library(RLRsim)
#Only applies to lmer
exactLRT(m=mod6,m0=mod7)

library(lmerTest)
#Likelihood ratio test comparing a model with a given random effect to that same model without the random effect
rand(mod6) #mother is significant
#Differences among streams and among mothers within streams in first flowering day in c.garden

######################
mod8<-lmer(first_fl_j ~(1|mother_pl_id_new), data = data_cgarden_short)
summary(mod8)
rand(mod8)

mod9<-lmer(first_fl_j ~(1|stream)+(1|mother_pl_id_new), data = data_cgarden_short)
summary(mod9)
rand(mod9)


