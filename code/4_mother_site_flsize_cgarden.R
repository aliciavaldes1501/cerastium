#Relationship between mother+site and flsize in the c. garden####
head(data_cgarden_short)
hist(data_cgarden_short$diam_fl_mean)
hist(log(data_cgarden_short$diam_fl_mean))

mod7<-lm(diam_fl_mean ~ stream/mother_pl_id_new, data = data_cgarden_short)#Keep####
summary(mod7)
Anova(mod7) #effect of mother

mod8<-lmer(diam_fl_mean ~ stream+(1|mother_pl_id_new), data = data_cgarden_short)
summary(mod8)
Anova(mod8) #stream NS
