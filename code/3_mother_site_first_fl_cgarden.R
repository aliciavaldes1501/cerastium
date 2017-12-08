#Relationship between mother+site and fl phen in the c. garden####
head(data_cgarden_short)
hist(data_cgarden_short$first_fl,breaks=40)

library(lubridate)
hist(yday(data_cgarden_short$first_fl))
plotNormalHistogram(yday(data_cgarden_short$first_fl)) #Quite normal
plotNormalHistogram(log(yday(data_cgarden_short$first_fl))) #More close to normal!

data_cgarden_short$patch_new<-as.factor(paste(data_cgarden_short$stream,data_cgarden_short$patch,sep="_"))
data_cgarden_short$first_fl_j<-yday(data_cgarden_short$first_fl)

mod5<-lm(first_fl_j ~ stream/mother_pl_id_new, data = data_cgarden_short)#Keep####
summary(mod5) 
Anova(mod5) #Effect of mother and stream 
plot(mod5)
hist(residuals(mod5))

mod6<-lmer(first_fl_j ~ stream+(1|mother_pl_id_new), data = data_cgarden_short)
summary(mod6)
Anova(mod6) #Effect of stream




