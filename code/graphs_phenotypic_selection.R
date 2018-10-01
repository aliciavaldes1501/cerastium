data_f_2017_sel$FFD_julian<-yday(data_f_2017_sel$FFD)
sel_model<-lmer(log(n_seeds+1)~FFD_julian+temp1+FFD_julian:temp1+nfl_std+(1|plot_new),data=data_f_2017_sel)
interaction1<-data.frame(effect(term="FFD_julian:temp1", mod=sel_model,
                                xlevels=list(FFD_julian=seq(156,211,1), temp1=seq(4,33,0.5))))
ggplot(interaction1, aes(FFD_julian,fit, group = as.factor(temp1)))+
  geom_smooth(method=loess,se=F,size=0.3,aes(FFD_julian,fit,color=temp1))+
  xlab("FFD")+ylab("N seeds (log)")+theme_base()+#scale_colour_gradientn(colours = myPalette(100))+
  scale_color_gradient(low = "blue", high = "red")+
  theme(legend.position="top")+labs(colour="Soil temperature")+
  theme(text=element_text(family="serif"))+theme(plot.background=element_rect(fill="white", colour=NA))
