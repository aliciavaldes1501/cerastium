#Relationship between soil t and fl phen in the field?####
head(data_field)
with(data_field,hist(stage))
with(data_field,hist(log(stage)))
with(data_field,hist(log(5-stage)))
with(data_field,hist(sqrt(stage)))
with(data_field,hist(sqrt(5-stage)))
with(data_field,hist(scale(stage,center=F,scale=T)))


with(data_field, plot(stage~date))
abline(lm(stage~date,data_field))
summary(with(data_field,lm(stage~date)))
plot(with(data_field,lm(stage~date)))

with(data_field, plotMeans(stage, as.factor(date), error.bars="se", connect=F)) #Differences in stages among dates
with(data_field, plotMeans(stage, stream, error.bars="se", connect=F)) #due to different streams sampled in different dates?

#Not correcting stage by date by now (only 3 days difference)

#Linear model stage~temp####
summary(with(data_field,lm(stage~temp)))
plot(with(data_field,lm(stage~temp))) #Not OK

#Tukey transformation####
library(rcompanion)
data_field$stage_tuk = transformTukey(data_field$stage,plotit=T)
with(data_field,hist(stage_tuk))
summary(with(data_field,lm(stage_tuk~temp)))
plot(with(data_field,lm(stage_tuk~temp))) #Same!

plotNormalHistogram(data_field$stage)
plotNormalHistogram(data_field$stage_tuk)

#Box-Cox tranformation####
library(MASS)

Box = boxcox(data_field$stage ~ 1)
Cox = data.frame(Box$x, Box$y)            # Create a data frame with the results
Cox2 = Cox[with(Cox, order(-Cox$Box.y)),] # Order the new data frame by decreasing y
Cox2[1,]                                  # Display the lambda with the greatest log likelihood
lambda = Cox2[1, "Box.x"]                 # Extract that lambda
data_field$stage_box = (data_field$stage ^ lambda - 1)/lambda   # Transform the original data

plotNormalHistogram(data_field$stage_box)
summary(with(data_field,lm(stage_box~temp)))
plot(with(data_field,lm(stage_box~temp))) #Same!

#Box-Cox transformation for model####
Box = boxcox(stage ~ temp,data = data_field,lambda = seq(-6,6,0.1))
Cox = data.frame(Box$x, Box$y)
Cox2 = Cox[with(Cox, order(-Cox$Box.y)),]
Cox2[1,]
lambda = Cox2[1, "Box.x"]
data_field$stage_box = (data_field$stage ^ lambda - 1)/lambda   

model = lm(stage_box~temp,data=data_field)
summary(model)
library(car)
Anova(model, type="II")
plot(model) #Same!

#Try different GLMs####
data_field$stage_i<-6-data_field$stage
summary(glm(stage_i~temp,data_field,family="poisson"))
plot(glm(stage_i~temp,data_field,family="poisson"))
summary(glm.nb(stage_i~temp,data_field))
plot(glm.nb(stage_i~temp,data_field))

#Try beta regression####
library(betareg)
data_field$stage_p<-data_field$stage/5
summary(betareg(stage_p~temp,data_field)) #Error

y.transf.betareg <- function(y){
  n.obs <- sum(!is.na(y))
  (y * (n.obs - 1) + 0.5) / n.obs
}

summary(betareg(y.transf.betareg(stage_p)~temp,data_field)) #OK
plot(betareg(y.transf.betareg(stage_p)~temp,data_field)) #OK??
hist(residuals(betareg(y.transf.betareg(stage_p)~temp,data_field)))

#LMM####
library(nlme)
library(lme4)
library(lmerTest)
data_field$patch_new<-as.factor(paste(data_field$stream,data_field$patch,sep="_"))

xtabs(~ stream + patch_new, data_field, drop = TRUE, sparse = TRUE)

mod1<-lme(stage ~ temp, data = data_field, random = ~ 1|stream,method="ML")
mod2<-lmer(stage ~ temp+(1|stream), data = data_field,REML=F)
mod3<-lm(stage ~ temp+stream, data = data_field)
mod4<-lme(stage ~ 1, data = data_field, random = ~ 1|stream,method="ML")

summary(mod1) #No effect
summary(mod2) #No effect
summary(mod3) #No effect
summary(mod4)

plot(mod1)
qqnorm(resid(mod1))
qqline(resid(mod1))  

anova(mod1,mod4)

hist(data_field$stage)
hist(data_field$stage_tuk)
hist(data_field$stage_box)
hist(data_field$stage_i)
hist(data_field$stage_p)

library(ggplot2)
ggplot(aes(stage, temp), data = data_field) + 
  geom_point() + 
  facet_wrap(~ stream) + 
  xlab("stage") + 
  ylab("temp")

ggplot(data_field, aes(x = temp, y = stage, colour = patch)) +
  facet_wrap(~stream, nrow=2) +
  geom_point() +
  theme_classic() +
  geom_line(data = cbind(data_field, pred = predict(mod1)), aes(y = pred)) +
  theme(legend.position = "none")

ggplot(data_field, aes(x = as.factor(stage), y = temp)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  facet_grid(stream ~ patch, margins = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

#Ordinal regression####
library(ordinal)
summary(clmm(as.factor(stage)~temp+(1|stream/patch_new), data=data_field))

#Binomoial GLM: flower/not flower####
data_field$flowered<-with(data_field,ifelse(stage==5,1,0))

mod1a<-glmer(flowered ~ temp+(1|stream), data = data_field,family="binomial")
summary(mod1a) #temp NS (p=0.069)
mod1a<-glm(flowered ~ temp+stream, data = data_field,family="binomial") #Keep####
summary(mod1a) #temp NS, stream*
mod1a<-glm(flowered ~ stream/temp, data = data_field,family="binomial") #Keep####
summary(mod1a)
mod1a<-glm(flowered ~ temp, data = data_field,family="binomial")
summary(mod1a) #temp*
mod1a<-glm(flowered ~ stream, data = data_field,family="binomial")
summary(mod1a) #stream*

with(data_field, plotMeans(height, stream, error.bars="se", connect=F))
data_field$n_fl_bud<-data_field$n_fl+data_field$n_bud
mod1a<-glm(flowered ~ stream+n_fl_bud+temp, data = data_field,family="binomial")
summary(mod1a) #stream*

library(sjPlot)

plot_model(mod1a, type = "pred", terms = c("temp"))+theme(text=element_text(family="serif"))+
  theme(plot.background=element_rect(fill="white", colour=NA))+ggtitle(NULL)

# sjp.glmer(mod1a, type = "fe.slope") #does not adjust for other predictors,plots the relation between model terms and response
# sjp.glmer(mod1a, type = "ri.slope",facet.grid = F) #does not adjust for other predictors,plots the relation between model terms and response
# sjp.glmer(mod1a, type = "eff") #marginal effects, adjusted for all predictors
# plot_model(mod1a, type = "eff",terms=c("temp")) #marginal effects, adjusted for all predictors
# sjp.glmer(mod1a, type = "pred",vars=c("temp","stream"),facet.grid=F) #predicted values against reponse, for particular model terms
# sjp.glmer(mod1a, type = "pred",vars=c("temp","patch_new"),facet.grid=F) #predicted values against reponse, for particular model terms
# sjp.glmer(mod1a, type = "pred",vars=c("temp","stream","patch_new"),facet.grid=F)#predicted values against reponse
# #predicted probabilities for the response, related to specific model predictors and conditioned on random effects
# sjp.glmer(mod1a, type = "ma") 

#Investigate these graphs more!

#LM, quadratic####
summary(lm(stage~temp+I(temp^2),data_field))
plot(lm(stage~temp+I(temp^2),data_field))

ggplot(data_field,aes(x=temp,y=stage))+geom_point()+
  stat_smooth(aes(y = stage),method = "lm", formula = y ~ x + I(x^2),color="red")+geom_smooth(method="lm")

with(data_field,plot(temp,stage))
fit<-lm(stage~temp+I(temp^2),data=data_field)
abline(fit)
