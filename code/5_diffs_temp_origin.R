#Diffs. among motherplants related to temp at site of origin?####

#first_fl
with(data_cgarden_short,plot(temp_ori,first_fl_j))
abline(lm(first_fl_j~temp_ori,data_cgarden_short))

ggplot(data_cgarden_short,aes(x=temp_ori,y=first_fl_j))+geom_point(aes(color=stream))+
  geom_smooth(method = "lm", color ="black",size=1)+ scale_y_reverse()+
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))+
  xlab("Soil temperature at origin")+ylab("First flowering day")

ggplot(data_cgarden_short,aes(x=temp_ori,y=first_fl_j))+geom_point(aes(color=stream))+
  geom_smooth(aes(color=stream),method = "lm", size=0.8,se=F,fullrange=F)+
  geom_smooth(method = "lm", color ="black",size=1)+ scale_y_reverse()+
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))+
  xlab("Soil temperature at origin")+ylab("First flowering day")

ggplot(data_cgarden_short,aes(x=temp_ori,y=first_fl_j))+geom_point(aes(color=stream))+
  geom_smooth(aes(color=stream),method = "lm", size=0.8,se=F,fullrange=F)+
  geom_smooth(method = "lm", color = alpha("black", 0.3),size=1)+ scale_y_reverse()+
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))+
  xlab("Soil temperature at origin")+ylab("First flowering day")

mod13<-lmer(first_fl_j ~ temp_ori+(1|stream)+(1|mother_pl_id_new),data = data_cgarden_short)
#This takes into account that mother is nested within stream
summary(mod13) #Temperature*
rand(mod13) #Stream and mother*

mod13a<-lmer(first_fl_j ~ temp_ori+(1|mother_pl_id_new),data = subset(data_cgarden_short,stream=="17"))
mod13b<-lmer(first_fl_j ~ temp_ori+(1|mother_pl_id_new),data = subset(data_cgarden_short,stream=="Main"))
mod13c<-lmer(first_fl_j ~ temp_ori+(1|mother_pl_id_new),data = subset(data_cgarden_short,stream=="Park"))
mod13d<-lmer(first_fl_j ~ temp_ori+(1|mother_pl_id_new),data = subset(data_cgarden_short,stream=="WS"))
summary(mod13a) #NS
summary(mod13b) #p=0.05
summary(mod13c) #NS
summary(mod13d) #NS


mod14<-lm(first_fl_j ~ temp_ori*stream,data = data_cgarden_short)
mod15<-lm(first_fl_j ~ temp_ori+stream,data = data_cgarden_short)
anova(mod14,mod15) #No support for different slopes for each stream

mod14<-lmer(first_fl_j ~ temp_ori*stream+(1|mother_pl_id_new),data = data_cgarden_short)
mod15<-lmer(first_fl_j ~ temp_ori+stream+(1|mother_pl_id_new),data = data_cgarden_short)
anova(mod14,mod15) #No support for different slopes for each stream

#Overall relationship between maternal temperature at site of origin and offspring phenology
summary(lm(first_fl_j~temp_ori,data=data_cgarden_short)) 
with(data_cgarden_short,plot(first_fl_j~temp_ori))
abline(lm(first_fl_j~temp_ori,data=data_cgarden_short)) 


#diam_fl_mean
with(data_cgarden_short,plot(temp_ori,diam_fl_mean))
abline(lm(diam_fl_mean~temp_ori,data_cgarden_short))

ggplot(data_cgarden_short,aes(x=temp_ori,y=diam_fl_mean))+geom_point(aes(color=stream))+
  geom_smooth(aes(color=stream),method = "lm", size=0.8,se=F,fullrange=F)+
  geom_smooth(method = "lm", color = alpha("black", 0.3),size=1)+
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))+
  xlab("Soil temperature at origin")+ylab("Flower diameter")

mod16<-lmer(diam_fl_mean ~ temp_ori+(1|stream)+(1|mother_pl_id_new),data = data_cgarden_short)
#This takes into account that mother is nested within stream
summary(mod16) #Temperature NS
rand(mod16) #Stream NS, mother*

mod16a<-lmer(diam_fl_mean ~ temp_ori+(1|mother_pl_id_new),data = subset(data_cgarden_short,stream=="17"))
mod16b<-lmer(diam_fl_mean ~ temp_ori+(1|mother_pl_id_new),data = subset(data_cgarden_short,stream=="Main"))
mod16c<-lmer(diam_fl_mean ~ temp_ori+(1|mother_pl_id_new),data = subset(data_cgarden_short,stream=="Park"))
mod16d<-lmer(diam_fl_mean ~ temp_ori+(1|mother_pl_id_new),data = subset(data_cgarden_short,stream=="WS"))
summary(mod16a) #NS
summary(mod16b) #NS
summary(mod16c) #NS
summary(mod16d) #NS


