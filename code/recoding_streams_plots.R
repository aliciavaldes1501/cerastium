ggplot(data_f_2015,aes(x=temp,y=flowered))+geom_point(aes(color=stream))+
  geom_smooth(aes(color=stream),method = "glm", method.args=c(family="binomial"),size=1,se=F,fullrange=F)+
  geom_smooth(method = "glm", method.args=c(family="binomial"),color="black",size=1.5)+
  my_theme()+xlab("Soil temperature")+ylab("Probability of flowering\nin the field")+
  theme(legend.position=c(0.85,0.3))+ggtitle("A) 2015")

data_f_2015$stream_new<-as.factor(ifelse(data_f_2015$stream=="Main"|data_f_2015$stream=="Parking",
                                         "Main_Parking",as.character(data_f_2015$stream)))


kable(prettify(with(data_f_2015,Anova(lm(stage~stream_new)))))
kable(prettify(as.data.frame(with(data_f_2015,TukeyHSD(aov(lm(stage~stream_new))))$stream_new)))

kable(prettify(with(data_f_2015,Anova(glm(flowered~stream_new,family="binomial")))))
kable(prettify(as.data.frame(with(data_f_2015,TukeyHSD(aov(as.numeric(flowered)~stream_new)))$stream_new)))

kable(prettify(with(data_f_2015,Anova(lm(temp~stream_new)))))
kable(prettify(as.data.frame(with(data_f_2015,TukeyHSD(aov(lm(temp~stream_new))))$stream_new)))

# Different slopes and intercepts for each stream
model1<-glm(flowered~temp*stream_new,family="binomial",data_f_2015)
kable(prettify(summary(model1)))
# Common slope, different intercepts for each stream
model2<-glm(flowered~temp+stream_new,family="binomial",data_f_2015)
kable(prettify(summary(model2)))
anova(model1,model2, test="Chisq") # Likelihood ratio test comparing both models

##################

data_f_2017$plot_new<-as.factor(ifelse(data_f_2017$plot=="H13","a",ifelse(data_f_2017$plot=="H05"|data_f_2017$plot=="H04","b",
                      ifelse(data_f_2017$plot=="H10"|data_f_2017$plot=="H03"|data_f_2017$plot=="H02","c",
                             ifelse(data_f_2017$plot=="H01"|data_f_2017$plot=="H08"|data_f_2017$plot=="H09","d",
                                    ifelse(data_f_2017$plot=="HC6"|data_f_2017$plot=="HC5","e","f"))))))
  

ggplot(data_f_2017,aes(x=temp1,y=yday(FFD)))+geom_point(aes(color=plot_new))+
  geom_smooth(aes(color=plot_new),method = "lm",size=1,se=F,fullrange=F)+
  geom_smooth(method = "lm",color="black",size=1.5)+scale_y_reverse()+
  my_theme()+xlab("Soil temperature")+ylab("FFD in the field")+
  theme(legend.position="right")+ggtitle("B) 2017")

# Different slopes and intercepts for each plot
model3<-lm(yday(FFD)~temp1*plot_new,data_f_2017)
kable(prettify(Anova(model3)))
# Common slope, different intercepts for each plot
model4<-lm(yday(FFD)~temp1+plot_new,data_f_2017)
kable(prettify(Anova(model4)))
anova(model3,model4, test="Chisq") # Likelihood ratio test comparing both models 




