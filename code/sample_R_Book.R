yields <- read.table("C:/Users/User/Dropbox/SU/The_R_Book/farms.txt",header=T)
attach(yields)
plot(N,size,pch=rep(16:19,each=40),col=farm)
linear.models <- lmList(size~N|farm,yields)
coef(linear.models)
linear.models
summary(linear.models)

ggplot(yields,aes(x=N,y=size))+geom_point(aes(color=as.factor(farm)))+
  geom_smooth(aes(color=as.factor(farm)),method = "lm", size=0.8,se=F,fullrange=F)+
  geom_smooth(method = "lm", color="black",size=2)+
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))+
  xlab("N")+ylab("size")

random.model <- lme(size~1,random=~N|farm)
coef(random.model)
summary(random.model)

mm <- coef(random.model)
ll <- coef(linear.models)
windows(7,4)
par(mfrow=c(1,2))
plot(ll[,1],mm[,1],pch=16,xlab="linear",ylab="random effects")
abline(0,1)
plot(ll[,2],mm[,2],pch=16,xlab="linear",ylab="random effects")
abline(0,1)

farm <- factor(farm)
mixed.model1 <- lme(size~N*farm,random=~1|farm,method="ML")
mixed.model2 <- lme(size~N+farm,random=~1|farm,method="ML")
mixed.model3 <- lme(size~N,random=~1|farm,method="ML")
mixed.model4 <- lme(size~1,random=~1|farm,method="ML")
anova(mixed.model1,mixed.model2,mixed.model3,mixed.model4)

model <- lm(size~N*factor(farm))
summary(model)

model2 <- lm(size~N+factor(farm))
anova(model,model2)

model3 <- lm(size~N)
anova(model2,model3)
