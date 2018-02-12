#Relationship between mother+site and flsize in the c. garden####
head(data_cgarden_short)
hist(data_cgarden_short$diam_fl_mean)
hist(log(data_cgarden_short$diam_fl_mean))

mod10<-lm(diam_fl_mean ~ stream/mother_pl_id_new, data = data_cgarden_short)
summary(mod10)
Anova(mod10) #effect of mother but not stream
plot(mod10)
hist(residuals(mod10))

subset_data2<-subset(data_cgarden_short,!is.na(diam_fl_mean))

mod11<-lmer(diam_fl_mean ~ stream+(1|mother_pl_id_new), REML=T,data = subset_data2)
summary(mod11)
Anova(mod11) #No effect of stream

#Likelihood ratio test comparing a model with a given random effect to that same model without the random effect
rand(mod11) #mother is significant
#Differences among mothers within streams, but not among streams in first flower size in c.garden

mod12<-lmer(diam_fl_mean ~ (1|mother_pl_id_new), REML=T,data = subset(data_cgarden_short,!is.na(diam_fl_mean)))
anova(mod11,mod12)



