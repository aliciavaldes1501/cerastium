#Diffs. among motherplants related to temp at site of origin?####

#first_fl
with(data_cgarden_short,plot(temp_ori,first_fl_j))
abline(lm(first_fl_j~temp_ori,data_cgarden_short))

#LMs
Anova(lm(first_fl_j ~ temp_ori+stream,data = data_cgarden_short)) #temp (p=0.059) and stream*
Anova(lm(first_fl_j ~ temp_ori+stream+mother_pl_id_new,data = data_cgarden_short)) #temp*, stream and mother*
Anova(lm(first_fl_j ~ temp_ori+stream/mother_pl_id_old,data = data_cgarden_short)) #temp*, stream* and mother* nested in stream ####
Anova(lm(first_fl_j ~ stream/temp_ori+stream/mother_pl_id_new,data = data_cgarden_short)) #stream*, temp* nested in stream and mother* nested in stream ####

#LMMs
Anova(lmer(first_fl_j ~ temp_ori+stream+(1|mother_pl_id_new),data = data_cgarden_short)) # stream* --> Takes nesting into account?
Anova(lmer(first_fl_j ~ temp_ori+(1|stream/mother_pl_id_new),data = data_cgarden_short)) #temp*
Anova(lmer(first_fl_j ~ temp_ori+stream+(1|stream/mother_pl_id_new),data = data_cgarden_short)) #stream*, gives warnings
Anova(lmer(first_fl_j ~ stream/temp_ori+(1|stream/mother_pl_id_new),data = data_cgarden_short)) #stream*, gives warnings


# sjp.lmer(mod9, type = "fe.slope") #does not adjust for other predictors,plots the relation between model terms and response
# sjp.lmer(mod9, type = "ri.slope",facet.grid = F) #does not adjust for other predictors,plots the relation between model terms and response
# sjp.lmer(mod9, type = "eff") #marginal effects, adjusted for all predictors
# plot_model(mod9, type = "eff",terms=c("temp_ori")) #marginal effects, adjusted for all predictors
# sjp.lmer(mod9, type = "pred",vars=c("temp_ori","stream"),facet.grid=F) #predicted values against reponse, for particular model terms
# sjp.lmer(mod9, type = "pred",vars=c("temp_ori","patch_new"),facet.grid=F) #predicted values against reponse, for particular model terms
# sjp.lmer(mod9, type = "pred",vars=c("temp_ori","stream","patch_new"),facet.grid=F)#predicted values against reponse
# #predicted probabilities for the response, related to specific model predictors and conditioned on random effects

#Flower size

#LMs
Anova(lm(diam_fl_mean ~ temp_ori+stream,data = data_cgarden_short)) #all NS
Anova(lm(diam_fl_mean ~ temp_ori+stream+mother_pl_id_new,data = data_cgarden_short)) #stream and mother*
Anova(lm(diam_fl_mean ~ temp_ori+stream/mother_pl_id_old,data = data_cgarden_short)) #mother* ####
Anova(lm(diam_fl_mean ~ stream/temp_ori+stream/mother_pl_id_new,data = data_cgarden_short)) #temp* and mother* ####

#LMMs
Anova(lmer(diam_fl_mean ~ temp_ori+stream+(1|mother_pl_id_new),data = data_cgarden_short)) # all NS --> Takes nesting into account?
Anova(lmer(diam_fl_mean ~ temp_ori+(1|stream/mother_pl_id_new),data = data_cgarden_short)) #all NS
Anova(lmer(diam_fl_mean ~ temp_ori+stream+(1|stream/mother_pl_id_new),data = data_cgarden_short)) #all NS, gives warnings
Anova(lmer(diam_fl_mean ~ stream/temp_ori+(1|stream/mother_pl_id_new),data = data_cgarden_short)) #all NS, gives warnings


##############################
#some graphs
with(data_cgarden_short, plotMeans(first_fl_j, stream, error.bars="se", connect=F))
with(data_cgarden_short, plotMeans(temp_ori, stream, error.bars="se", connect=F))

#last_fl####
data_cgarden_short$last_fl_j<-yday(data_cgarden_short$last_fl)
with(data_cgarden_short,plot(temp_ori,last_fl_j))
abline(lm(last_fl_j~temp_ori,data_cgarden_short))

#LMs
Anova(lm(last_fl_j ~ temp_ori+stream,data = data_cgarden_short)) #stream*
Anova(lm(last_fl_j ~ temp_ori+stream+mother_pl_id_new,data = data_cgarden_short)) #stream (p=0.060) and mother*
Anova(lm(last_fl_j ~ temp_ori+stream/mother_pl_id_old,data = data_cgarden_short)) #stream* and mother* nested in stream ####
Anova(lm(last_fl_j ~ stream/temp_ori+stream/mother_pl_id_new,data = data_cgarden_short)) #stream*, temp* nested in stream and mother* nested in stream ####

#LMMs
Anova(lmer(last_fl_j ~ temp_ori+stream+(1|mother_pl_id_new),data = data_cgarden_short)) # stream* --> Takes nesting into account?
Anova(lmer(last_fl_j ~ temp_ori+(1|stream/mother_pl_id_new),data = data_cgarden_short)) #NS
Anova(lmer(last_fl_j ~ temp_ori+stream+(1|stream/mother_pl_id_new),data = data_cgarden_short)) #NS, gives warnings
Anova(lmer(last_fl_j ~ stream/temp_ori+(1|stream/mother_pl_id_new),data = data_cgarden_short)) #temp*, gives warnings

#dur_fl####

#LMs
Anova(lm(dur_fl ~ temp_ori+stream,data = data_cgarden_short)) #stream* and temp*
Anova(lm(dur_fl ~ temp_ori+stream+mother_pl_id_new,data = data_cgarden_short)) #mother*
Anova(lm(dur_fl ~ temp_ori+stream/mother_pl_id_old,data = data_cgarden_short)) #stream* and mother* nested in stream ####
Anova(lm(dur_fl ~ stream/temp_ori+stream/mother_pl_id_new,data = data_cgarden_short)) #stream* and mother* nested in stream ####

#LMMs
Anova(lmer(dur_fl ~ temp_ori+stream+(1|mother_pl_id_new),data = data_cgarden_short)) # stream* and temp*--> Takes nesting into account?
Anova(lmer(dur_fl ~ temp_ori+(1|stream/mother_pl_id_new),data = data_cgarden_short)) #temp*
Anova(lmer(dur_fl ~ temp_ori+stream+(1|stream/mother_pl_id_new),data = data_cgarden_short)) #temp*, gives warnings
Anova(lmer(dur_fl ~ stream/temp_ori+(1|stream/mother_pl_id_new),data = data_cgarden_short)) #temp (p=0.05), gives warnings

#peak_fl
data_cgarden_short$peak_fl_j<-yday(data_cgarden_short$peak_fl)
with(data_cgarden_short,plot(temp_ori,peak_fl))
abline(lm(peak_fl~temp_ori,data_cgarden_short))

#LMs
Anova(lm(peak_fl_j ~ temp_ori+stream,data = data_cgarden_short)) #stream*
Anova(lm(peak_fl_j ~ temp_ori+stream+mother_pl_id_new,data = data_cgarden_short)) #stream and mother*
Anova(lm(peak_fl_j ~ temp_ori+stream/mother_pl_id_old,data = data_cgarden_short)) #stream* and mother* nested in stream ####
Anova(lm(peak_fl_j ~ stream/temp_ori+stream/mother_pl_id_new,data = data_cgarden_short)) #stream*, temp* nested in stream and mother* nested in stream ####

#LMMs
Anova(lmer(peak_fl_j ~ temp_ori+stream+(1|mother_pl_id_new),data = data_cgarden_short)) # stream* --> Takes nesting into account?
Anova(lmer(peak_fl_j ~ temp_ori+(1|stream/mother_pl_id_new),data = data_cgarden_short)) #temp (p=0.06)
Anova(lmer(peak_fl_j ~ temp_ori+stream+(1|stream/mother_pl_id_new),data = data_cgarden_short)) #NS, gives warnings
Anova(lmer(peak_fl_j ~ stream/temp_ori+(1|stream/mother_pl_id_new),data = data_cgarden_short)) #temp*, gives warnings



