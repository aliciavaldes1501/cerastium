ggplot(data_cgarden_short,aes(x=temp_ori,y=first_fl_j,color=stream))+geom_point()+geom_smooth(method="lm",fullrange=F)

AggregatedData
with(AggregatedData,plot(temp_ori,first_fl_j))
abline(lm(first_fl_j~temp_ori,AggregatedData))
summary(lm(first_fl_j~temp_ori,AggregatedData))

data_means<-merge(data_cgarden_means,data_field[c(4,9,15)],by.x="mother_pl_id_new",by.y="pl_id")
with(data_means,plot(stage,first_fl_j))
abline(lm(first_fl_j~stage,data_means))
summary(lm(first_fl_j~stage,data_means))

with(data_means,plot(as.factor(flowered),first_fl_j,ylab="First fl day c.garden",xlab="Fl in the field"))
summary(lm(first_fl_j~as.factor(flowered),data_means))





