library(agricolae)
hsd_15=HSD.test(aov(temp~stream_new,data=data_f_2015), "stream_new", group=T)
data_f_2015.summarized = data_f_2015 %>% group_by(stream_new) %>% summarize(Max.temp=max(temp))

iris.summarized = iris %>% group_by(Species) %>% summarize(Max.Petal.Length=max(Petal.Length))


ggplot(data_f_2015,aes(x=reorder(stream_new,temp,FUN = median),y=temp))+geom_boxplot()+
  geom_text(data=data_f_2015.summarized,aes(x=stream_new,y=0.2+Max.temp,label=hsd_15$groups$groups),vjust=0)



ggplot(data_f_2015,aes(x=stream_new,y=temp))+geom_boxplot()+xlab("Stream")+ylab("Soil temperature (ºC)")+my_theme()+


