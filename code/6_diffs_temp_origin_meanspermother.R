#Means per motherplant
head(data_cgarden_short)
data_cgarden_means<-unique(merge(aggregate(cbind(first_fl_j,last_fl_j) ~ mother_pl_id_new, data=data_cgarden_short, FUN=mean), 
      data_cgarden_short[c(1:4,6,14,15)], by = c("mother_pl_id_new", "mother_pl_id_new")))
data_cgarden_means$mother_pl_id_new<-droplevels(data_cgarden_means$mother_pl_id_new)
data_cgarden_means$mother_pl_id_new<-droplevels(data_cgarden_means$mother_pl_id_new)

head(data_cgarden_means)
str(data_cgarden_means)

plot(data_cgarden_means$mother_pl_id_new) #1 for each
data_cgarden_means

hist(data_cgarden_means$first_fl_j)
hist(data_cgarden_means$last_fl_j)

Anova(lm(first_fl_j~temp_ori+stream,data_cgarden_means))
summary(lm(first_fl_j~temp_ori+stream,data_cgarden_means))

Anova(lm(first_fl_j~stream/temp_ori,data_cgarden_means))
summary(lm(first_fl_j~stream/temp_ori,data_cgarden_means))

Anova(lm(first_fl_j~temp_ori*stream,data_cgarden_means))
summary(lm(first_fl_j~temp_ori*stream,data_cgarden_means))

Anova(lm(first_fl_j~temp_ori,data_cgarden_means))

Anova(lmer(first_fl_j~temp_ori+(1|stream),data_cgarden_means))
summary(lmer(first_fl_j~temp_ori+(1|stream),data_cgarden_means))
rand(lmer(first_fl_j~temp_ori+(1|stream),data_cgarden_means))

ggplot(data_cgarden_means,aes(x=temp_ori,y=first_fl_j))+geom_point()+geom_smooth(method="lm")
ggplot(data_cgarden_means,aes(x=temp_ori,y=first_fl_j,color=stream))+geom_point()+geom_smooth(method="lm",fullrange=F)
ggplot(data_cgarden_means,aes(x=temp_ori,y=first_fl_j))+geom_point()+geom_smooth(method="lm",fullrange=F)+
  facet_wrap( ~ stream, ncol=2)

ggplot(data_cgarden_means,aes(x=temp_ori,y=first_fl_j))+geom_point(aes(color=stream))+
  geom_smooth(aes(color=stream),method = "lm", size=0.8,se=F,fullrange=F)+
  geom_smooth(method = "lm", color = alpha("black", 0.3),size=1)+
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))+
  xlab("Soil temperature at origin")+ylab("First flowering day (=later phenology)")
