head(data17_stages_complete)

# B5-->B4 
# FL100-->FL
# W50-->W

data17_stages_complete <- within(data17_stages_complete, {
  stage_corr_rec <- Recode(stage_corr, '"B5"="B4"; "FL100"="FL"; "W50"="W"', as.factor.result=TRUE)
}) #Recode stages with few counts

data17_stages_complete <- within(data17_stages_complete, {
  stage_num_rec <- Recode(stage_corr_rec, '"VS"=1; "VL"=2; "B1"=3; "B2"=4; "B3"=5; "B4"=6; "FL"=7; "W"=9; "W100"=10', as.factor.result=F)
}) #Convert to numeric

plot(data17_stages_complete$stage_num_rec)

hist(data17_stages_complete$stage_num)
hist(data17_stages_complete$stage_num_rec)
table(data17_stages_complete$stage_num_rec)

str(data17_stages_complete)

#Some Preliminary models
summary(lm(stage_num_rec~date*temp,data17_stages_complete))
summary(lm(stage_num_rec~date,data17_stages_complete))
summary(lm(stage_num_rec~temp,data17_stages_complete))

