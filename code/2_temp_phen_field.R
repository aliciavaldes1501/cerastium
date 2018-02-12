#Relationship between soil t and fl phen in the field?####
# head(data_field)
# with(data_field,hist(stage))
# with(data_field,hist(log(stage)))
# with(data_field,hist(log(5-stage)))
# with(data_field,hist(sqrt(stage)))
# with(data_field,hist(sqrt(5-stage)))
# with(data_field,hist(scale(stage,center=F,scale=T)))
# 
# 
# with(data_field, plot(stage~date))
# abline(lm(stage~date,data_field))
# summary(with(data_field,lm(stage~date)))
# plot(with(data_field,lm(stage~date)))
# 
# with(data_field, plotMeans(stage, as.factor(date), error.bars="se", connect=F)) #Differences in stages among dates
# with(data_field, plotMeans(stage, stream, error.bars="se", connect=F)) #due to different streams sampled in different dates?
# 
# #Not correcting stage by date by now (only 3 days difference)
# 
# #Linear model stage~temp####
# summary(with(data_field,lm(stage~temp)))
# plot(with(data_field,lm(stage~temp))) #Not OK
# 
# #Tukey transformation####
# library(rcompanion)
# data_field$stage_tuk = transformTukey(data_field$stage,plotit=T)
# with(data_field,hist(stage_tuk))
# summary(with(data_field,lm(stage_tuk~temp)))
# plot(with(data_field,lm(stage_tuk~temp))) #Same!
# 
# plotNormalHistogram(data_field$stage)
# plotNormalHistogram(data_field$stage_tuk)
# 
# #Box-Cox tranformation####
# library(MASS)
# 
# Box = boxcox(data_field$stage ~ 1)
# Cox = data.frame(Box$x, Box$y)            # Create a data frame with the results
# Cox2 = Cox[with(Cox, order(-Cox$Box.y)),] # Order the new data frame by decreasing y
# Cox2[1,]                                  # Display the lambda with the greatest log likelihood
# lambda = Cox2[1, "Box.x"]                 # Extract that lambda
# data_field$stage_box = (data_field$stage ^ lambda - 1)/lambda   # Transform the original data
# 
# plotNormalHistogram(data_field$stage_box)
# summary(with(data_field,lm(stage_box~temp)))
# plot(with(data_field,lm(stage_box~temp))) #Same!
# 
# #Box-Cox transformation for model####
# Box = boxcox(stage ~ temp,data = data_field,lambda = seq(-6,6,0.1))
# Cox = data.frame(Box$x, Box$y)
# Cox2 = Cox[with(Cox, order(-Cox$Box.y)),]
# Cox2[1,]
# lambda = Cox2[1, "Box.x"]
# data_field$stage_box = (data_field$stage ^ lambda - 1)/lambda   
# 
# model = lm(stage_box~temp,data=data_field)
# summary(model)
# library(car)
# Anova(model, type="II")
# plot(model) #Same!
# 
# #Try different GLMs####
# data_field$stage_i<-6-data_field$stage
# summary(glm(stage_i~temp,data_field,family="poisson"))
# plot(glm(stage_i~temp,data_field,family="poisson"))
# summary(glm.nb(stage_i~temp,data_field))
# plot(glm.nb(stage_i~temp,data_field))
# 
# #Try beta regression####
# library(betareg)
# data_field$stage_p<-data_field$stage/5
# summary(betareg(stage_p~temp,data_field)) #Error
# 
# y.transf.betareg <- function(y){
#   n.obs <- sum(!is.na(y))
#   (y * (n.obs - 1) + 0.5) / n.obs
# }
# 
# summary(betareg(y.transf.betareg(stage_p)~temp,data_field)) #OK
# plot(betareg(y.transf.betareg(stage_p)~temp,data_field)) #OK??
# hist(residuals(betareg(y.transf.betareg(stage_p)~temp,data_field)))
# 
# #LMM####
# library(nlme)
# library(lme4)
# library(lmerTest)
# data_field$patch_new<-as.factor(paste(data_field$stream,data_field$patch,sep="_"))
# 
# xtabs(~ stream + patch_new, data_field, drop = TRUE, sparse = TRUE)
# 
# mod1<-lme(stage ~ temp, data = data_field, random = ~ 1|stream,method="ML")
# mod2<-lmer(stage ~ temp+(1|stream), data = data_field,REML=F)
# mod3<-lm(stage ~ temp+stream, data = data_field)
# mod4<-lme(stage ~ 1, data = data_field, random = ~ 1|stream,method="ML")
# 
# summary(mod1) #No effect
# summary(mod2) #No effect
# summary(mod3) #No effect
# summary(mod4)
# 
# plot(mod1)
# qqnorm(resid(mod1))
# qqline(resid(mod1))  
# 
# anova(mod1,mod4)
# 
# hist(data_field$stage)
# hist(data_field$stage_tuk)
# hist(data_field$stage_box)
# hist(data_field$stage_i)
# hist(data_field$stage_p)
# 
# library(ggplot2)
# ggplot(aes(stage, temp), data = data_field) + 
#   geom_point() + 
#   facet_wrap(~ stream) + 
#   xlab("stage") + 
#   ylab("temp")
# 
# ggplot(data_field, aes(x = temp, y = stage, colour = patch)) +
#   facet_wrap(~stream, nrow=2) +
#   geom_point() +
#   theme_classic() +
#   geom_line(data = cbind(data_field, pred = predict(mod1)), aes(y = pred)) +
#   theme(legend.position = "none")
# 
# ggplot(data_field, aes(x = as.factor(stage), y = temp)) +
#   geom_boxplot(size = .75) +
#   geom_jitter(alpha = .5) +
#   facet_grid(stream ~ patch, margins = TRUE) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
# 
# #Ordinal regression####
# library(ordinal)
# summary(clmm(as.factor(stage)~temp+(1|stream/patch_new), data=data_field))

#Differences among streams####
ggplot(data_field,aes(x=stream,y=temp))+ geom_boxplot()+
  xlab("Stream")+ylab("Soil temperature")+theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))
with(data_field,summary(lm(temp~stream)))
with(data_field,Anova(lm(temp~stream)))
with(data_field,TukeyHSD(aov(lm(temp~stream)))) #All different from all

ggplot(data_field,aes(x=stream,y=stage))+ geom_boxplot()+
  xlab("stream")+ylab("Phenological stage")
with(data_field,summary(lm(stage~stream)))
with(data_field,Anova(lm(stage~stream)))
with(data_field,TukeyHSD(aov(lm(stage~stream)))) #All different from 17

ggplot(data_field,aes(x=stream))+ geom_bar(aes(fill=as.factor(flowered)),position = "dodge")+
  xlab("Stream")+ylab("Count")
with(data_field,summary(glm(flowered~stream,family="binomial")))
with(data_field,Anova(glm(flowered~stream,family="binomial")))
with(data_field,TukeyHSD(aov(glm(flowered~stream,family="binomial")))) 
#All different from 17, Parking and Main also different

#There are differences in mean temperature, phenological stage and prob. flowering among streams

data_field$flowered<-with(data_field,ifelse(stage==5,1,0))
plot(as.factor(data_field$flowered),xlab="Probability of flowering",ylab="Count")
hist(data_field$stage,ylab="Count",xlab="Stage",main=NULL)

#Prob. flowering against soil t (overall + for each stream)
ggplot(data_field,aes(x=temp,y=flowered))+geom_point(aes(color=stream))+
  geom_smooth(aes(color=stream),method = "glm", method.args=c(family="binomial"),size=1.5,se=F,fullrange=F)+
  geom_smooth(method = "glm", method.args=c(family="binomial"),color="black",size=2)+
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))+
  xlab("Soil temperature")+ylab("Probability of flowering")
ggplot(data_field,aes(x=temp,y=flowered))+geom_point(aes(color=stream))+
  geom_smooth(method = "glm", method.args=c(family="binomial"),color="black",size=2)+
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))+
  xlab("Soil temperature")+ylab("Probability of flowering")
summary(glm(flowered ~ temp, data = data_field,family="binomial")) #Logistic regr pooled data
summary(glm(flowered ~ temp, data = subset(data_field,stream=="17"),family="binomial")) #Logistic regr for each stream
summary(glm(flowered ~ temp, data = subset(data_field,stream=="Main"),family="binomial"))
summary(glm(flowered ~ temp, data = subset(data_field,stream=="Parking"),family="binomial"))
summary(glm(flowered ~ temp, data = subset(data_field,stream=="WS"),family="binomial"))

#Phen stage against soil t (overall + for each stream) - linear model is not good
ggplot(data_field,aes(x=temp,y=stage))+geom_point(aes(color=stream))+
  geom_smooth(aes(color=stream),method = "lm", size=0.8,se=F,fullrange=F)+
  geom_smooth(method = "lm", color="black",size=2)+
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))+
  xlab("Soil temperature")+ylab("Phenological stage")

# #Quadratic stage-temp
# ggplot(data_field,aes(x=temp,y=stage))+geom_point()+
#   stat_smooth(aes(y = stage),method = "lm", formula = y ~ x + I(x^2),color="red")+geom_smooth(method="lm")

#slopes and intercepts from the model specified entirely in terms of random effects: a population
# of regression slopes predicted within each stream with temp as the continuous explanatory variable, and a
# population of intercepts for each stream:
summary(glmer(flowered~1+(temp|stream),family="binomial",data_field))
coef(glmer(flowered~1+(temp|stream),family="binomial",data_field))

mixed.model1 <- glmer(flowered~temp*stream+(1|stream),family="binomial",data_field)
#full factorial, with different slopes and intercepts for each of the 4 streams
mixed.model2 <- glmer(flowered~temp+stream+(1|stream),family="binomial",data_field)
#common slope but different intercepts for the 4 streams
mixed.model3 <- glmer(flowered~temp+(1|stream),family="binomial",data_field) #main effect of temp
mixed.model4 <- glmer(flowered~1+(1|stream),family="binomial",data_field) #main effect of stream
anova(mixed.model1,mixed.model2,mixed.model3,mixed.model4) #Select model 3 - lowest AIC?
anova(mixed.model1,mixed.model2) #Select model 1?


model <- glm(flowered~temp*stream,family="binomial",data_field)
summary(model) #Different slopes and intercepts
model2 <- glm(flowered~temp+stream,family="binomial",data_field)
summary(model2) #Common slope, different intercepts
#Likelihood ratio test comparing the full and reduced models 
anova(model,model2, test="Chisq") #No support for significant differences between slopes-->keep model2


anova(model2)$Pr(>F)  # 0.0002695578


model3 <- glm(flowered~temp,family="binomial",data_field)
anova(model2,model3, test="Chisq") #Highly significant differences in intercepts between streams-->keep model2

# #LM, quadratic####
# summary(lm(stage~temp+I(temp^2),data_field))
# plot(lm(stage~temp+I(temp^2),data_field))
# 
# ggplot(data_field,aes(x=temp,y=stage))+geom_point()+
#   stat_smooth(aes(y = stage),method = "lm", formula = y ~ x + I(x^2),color="red")+geom_smooth(method="lm")

#Models for effects of stream mean temperatures on mean flowering stages - NS, only 4 points!
data_field_means<-aggregate(cbind(flowered, stage, temp) ~ stream, data=data_field, FUN=mean)
data_field_means
with(data_field_means,plot(flowered~temp))
with(data_field_means,summary(glm(flowered~temp,family=binomial)))
with(data_field_means,plot(stage~temp))
with(data_field_means,summary(lm(stage~temp)))
