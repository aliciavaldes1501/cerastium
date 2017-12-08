#Read and clean data field####

data_field<-read.table("data/raw/cerastium_field.txt",header=T,sep="\t",dec=".")

data_field$date<-as.Date(as.character(data_field$date),format="%d/%m/%Y") #Convert date to date format
data_field$patch<-as.factor(data_field$patch) #Convert patch to factor
data_field$pl_id<-as.factor(data_field$pl_id) #Convert pl_id to factor
colSums(is.na(data_field)) #Check for NAs
data_field$n_fl[is.na(data_field$n_fl)] <- 0 #Replace NAs by zeros in n_fl
subset(data_field,is.na(stage)) #Stage should be 5 here
data_field$stage[is.na(data_field$stage)] <- 5 #Replace NAs by 5 in stage
colSums(is.na(data_field)) #No NAs now

data_field$dist_water<-as.character(data_field$dist_water) #Replace weird values in dist_water
data_field$dist_water[data_field$dist_water=="?300"]<-"300"
data_field$dist_water[data_field$dist_water==">1000"]<-"1000"
data_field$dist_water[data_field$dist_water==">300"]<-"300"
data_field$dist_water<-as.integer(data_field$dist_water) 

head(data_field)
str(data_field)

#Read and clean data common garden####

data_cgarden<-read.table("data/raw/cerastium_cgarden.txt",header=T,sep="\t",dec=",")

data_cgarden$stream[data_cgarden$stream=="Main "]<-"Main" #Fix spaces in stream names
data_cgarden$stream[data_cgarden$stream=="Park "]<-"Park"
data_cgarden$stream<-droplevels(data_cgarden$stream) #Remove unused levels
levels(data_cgarden$stream) #4 levels
data_cgarden$patch<-as.factor(data_cgarden$patch) #Convert patch to factor
data_cgarden$mother_pl_id_old<-as.character(data_cgarden$mother_pl_id_old) #Replace weird values in mother_pl_id_old
data_cgarden$corrected<-ifelse(
  data_cgarden$mother_pl_id_old=="120 New"| data_cgarden$mother_pl_id_old=="122 New"|
    data_cgarden$mother_pl_id_old=="250 New"| data_cgarden$mother_pl_id_old=="250 New "|
    data_cgarden$mother_pl_id_old=="290 New"| data_cgarden$mother_pl_id_old=="327 New?"|
    data_cgarden$mother_pl_id_old=="357 New"| data_cgarden$mother_pl_id_old=="42 New"|
    data_cgarden$mother_pl_id_old=="553 New"| data_cgarden$mother_pl_id_old=="776 New?"|
    data_cgarden$mother_pl_id_old=="New 357","YES","NO") #"YES" for pls w seeds collected close to original pl

data_cgarden$mother_pl_id_new<-data_cgarden$mother_pl_id_old

#Maximum mother_pl_id_old is 1076
#Rename from 1077 onwards
data_cgarden$mother_pl_id_new[data_cgarden$mother_pl_id_old=="120 New"]<-"1077"
data_cgarden$mother_pl_id_new[data_cgarden$mother_pl_id_old=="122 New"]<-"1078"
data_cgarden$mother_pl_id_new[data_cgarden$mother_pl_id_old=="250 New"]<-"1079"
data_cgarden$mother_pl_id_new[data_cgarden$mother_pl_id_old=="250 New "]<-"1079" 
data_cgarden$mother_pl_id_new[data_cgarden$mother_pl_id_old=="290 New"]<-"1080"
data_cgarden$mother_pl_id_new[data_cgarden$mother_pl_id_old=="327 New?"]<-"1081"
data_cgarden$mother_pl_id_new[data_cgarden$mother_pl_id_old=="357 New"]<-"1082"
data_cgarden$mother_pl_id_new[data_cgarden$mother_pl_id_old=="42 New"]<-"1083"
data_cgarden$mother_pl_id_new[data_cgarden$mother_pl_id_old=="553 New"]<-"1084"
data_cgarden$mother_pl_id_new[data_cgarden$mother_pl_id_old=="776 New?"]<-"1085"
data_cgarden$mother_pl_id_new[data_cgarden$mother_pl_id_old=="New 357"]<-"1082"

data_cgarden$mother_pl_id_old[data_cgarden$mother_pl_id_old=="120 New"]<-"120"
data_cgarden$mother_pl_id_old[data_cgarden$mother_pl_id_old=="122 New"]<-"122"
data_cgarden$mother_pl_id_old[data_cgarden$mother_pl_id_old=="250 New"]<-"250"
data_cgarden$mother_pl_id_old[data_cgarden$mother_pl_id_old=="250 New "]<-"250" 
data_cgarden$mother_pl_id_old[data_cgarden$mother_pl_id_old=="290 New"]<-"290"
data_cgarden$mother_pl_id_old[data_cgarden$mother_pl_id_old=="327 New?"]<-"327"
data_cgarden$mother_pl_id_old[data_cgarden$mother_pl_id_old=="357 New"]<-"357"
data_cgarden$mother_pl_id_old[data_cgarden$mother_pl_id_old=="42 New"]<-"42"
data_cgarden$mother_pl_id_old[data_cgarden$mother_pl_id_old=="553 New"]<-"553"
data_cgarden$mother_pl_id_old[data_cgarden$mother_pl_id_old=="776 New?"]<-"776"
data_cgarden$mother_pl_id_old[data_cgarden$mother_pl_id_old=="New 357"]<-"357"

#data_cgarden$mother_pl_id_old that are not in field data as data_field$pl_id
setdiff(data_cgarden$mother_pl_id_old, data_field$pl_id)
# [1] "3x"  "5x"  "397" "549" "750" "118" "1xa" "1xb" "2x"  "572" "4x" 

#data_cgarden$mother_pl_id_new that are not in field data as data_field$pl_id
setdiff(data_cgarden$mother_pl_id_new, data_field$pl_id)
#[1] "1078" "1083" "1077" "1079" "1081" "3x"   "1080" "1082" "5x"   "397"  "1084" 
#"1085" "549"  "750"  "118"  "1xa"  "1xb"  "2x"   "572"  "4x"  

data_field$pl_id[data_field$pl_id=="397"] #Not present
data_field$pl_id[data_field$pl_id=="549"] #Not present
data_field$pl_id[data_field$pl_id=="750"] #Not present
data_field$pl_id[data_field$pl_id=="118"] #Not present
data_field$pl_id[data_field$pl_id=="572"] #Not present

#data_field$pl_id that are not in c. garden data as data_cgarden$mother_pl_id_old
setdiff(data_field$pl_id,data_cgarden$mother_pl_id_old)
# [1] "759" "504"
#759 must be 750 (same patch)
data_cgarden$mother_pl_id_old[data_cgarden$mother_pl_id_old=="750"]<-"759" #Change
data_cgarden$mother_pl_id_new[data_cgarden$mother_pl_id_old=="750"]<-"759" #Change

setdiff(data_cgarden$mother_pl_id_old, data_field$pl_id)
#[1] "3x"  "5x"  "397" "549" "118" "1xa" "1xb" "2x"  "572" "4x" 
#10 pls from cgarden with no corresp in field

data_cgarden$corrected<-as.factor(ifelse(data_cgarden$mother_pl_id_old=="3x"|
             data_cgarden$mother_pl_id_old=="5x"|data_cgarden$mother_pl_id_old=="397"|
             data_cgarden$mother_pl_id_old=="549"|data_cgarden$mother_pl_id_old=="118"|
             data_cgarden$mother_pl_id_old=="1xa"|data_cgarden$mother_pl_id_old=="1xb"|
             data_cgarden$mother_pl_id_old=="2x"|data_cgarden$mother_pl_id_old=="572"|
             data_cgarden$mother_pl_id_old=="4x","NODATA",data_cgarden$corrected)) #"NODATA" for pls in cgarden w no field data

data_cgarden$mother_pl_id_old<-as.factor(data_cgarden$mother_pl_id_old) #Convert mother_pl_id_old to factor
data_cgarden$mother_pl_id_new<-as.factor(data_cgarden$mother_pl_id_new) #Convert mother_pl_id_new to factor
data_cgarden$offspring_id_new<-as.factor(data_cgarden$offspring_id_new) #Convert offspring_id_new to factor

head(data_cgarden)
str(data_cgarden)

#diam_fl: 3 measures, convert to summary stats
hist(data_cgarden$diam_fl1)
hist(data_cgarden$diam_fl2)
hist(data_cgarden$diam_fl3)

#Calculate mean fl diameter from available data (1,2 or 3 fls, NA for those with no data)
data_cgarden$diam_fl_mean<-rowMeans(data_cgarden[c("diam_fl1","diam_fl2","diam_fl3")], na.rm=TRUE)
#How many flowers were sampled? (0-3)
data_cgarden$n_sampl_fl<-apply(data_cgarden[21:23], 1, function(x) length(which(!is.na(x))))

head(data_cgarden)
str(data_cgarden)

#Rows with only missing values 
data_cgarden[rowSums(!is.na(data_cgarden[7:25])) == 0,] #265 rows-->remove
data_cgarden<-data_cgarden[rowSums(!is.na(data_cgarden[7:25])) > 0,] #606 rows remaining

#Convert from wide to long format
library(tidyr)
library(dplyr)

data_cgarden_long<-data_cgarden[c(1:2,4,6:22,26:29)] %>%
  # transfer to 'long' format
  gather(type, value, n_bud.170511:n_fl.170619) %>%
  # separate the column into type and date
  separate(type, into = c("type", "date"), "\\.") %>%
  # transfer to 'short' format
  spread(type, value) %>% #Error
  mutate(date = as.numeric(date)) %>%
  arrange(date)

#Change date to date format
data_cgarden_long$date[data_cgarden_long$date=="170511"]<-"2017-05-11"
data_cgarden_long$date[data_cgarden_long$date=="170518"]<-"2017-05-18"
data_cgarden_long$date[data_cgarden_long$date=="170521"]<-"2017-05-21"
data_cgarden_long$date[data_cgarden_long$date=="170524"]<-"2017-05-24"
data_cgarden_long$date[data_cgarden_long$date=="170528"]<-"2017-05-28"
data_cgarden_long$date[data_cgarden_long$date=="170602"]<-"2017-06-02"
data_cgarden_long$date[data_cgarden_long$date=="170609"]<-"2017-06-09"
data_cgarden_long$date[data_cgarden_long$date=="170616"]<-"2017-06-16"
data_cgarden_long$date[data_cgarden_long$date=="170619"]<-"2017-06-19"

data_cgarden_long$date<-as.Date(data_cgarden_long$date,format="%Y-%m-%d") #Convert date to date format
data_cgarden_long<-data_cgarden_long[with(data_cgarden_long, order(stream, patch,mother_pl_id_old,mother_pl_id_new,offspring_id_new,date)), ]

head(data_cgarden_long)
str(data_cgarden_long)

#BUT: xa and xb not sure --> remove 
data_cgarden_long<-subset(data_cgarden_long,!mother_pl_id_new=="1xa"&!mother_pl_id_new=="1xb")
data_cgarden<-subset(data_cgarden,!mother_pl_id_new=="1xa"&!mother_pl_id_new=="1xb")

head(data_cgarden_long)
str(data_cgarden_long)

plot(data_cgarden_long$corrected)
#"NODATA" for pls in cgarden w no field data
#"YES" for pls w seeds collected close to original pl
#"NO" for pls w seeds collected from original pl

with(data_cgarden_long,plot(n_bud~date))
with(data_cgarden_long,plot(n_fl~date))

#Calculate first-last flowering and bud day + peak fl

first_fl<-aggregate(date ~ stream+patch+mother_pl_id_new+offspring_id_new, 
          subset(data_cgarden_long,!is.na(n_fl)), function(x) min(x))
last_fl<-aggregate(date ~ stream+patch+mother_pl_id_new+offspring_id_new, 
          subset(data_cgarden_long,!is.na(n_fl)), function(x) max(x))
first_bud<-aggregate(date ~ stream+patch+mother_pl_id_new+offspring_id_new, 
                    subset(data_cgarden_long,!is.na(n_bud)), function(x) min(x))
last_bud<-aggregate(date ~ stream+patch+mother_pl_id_new+offspring_id_new, 
                   subset(data_cgarden_long,!is.na(n_bud)), function(x) max(x))
peak_fl<-as.data.frame(data_cgarden_long %>% group_by(stream,patch,mother_pl_id_new,offspring_id_new) %>% slice(which.max(n_fl)))[c(1,2,6,4,9)]

names(first_fl)[5]<-"first_fl"
names(last_fl)[5]<-"last_fl"
names(first_bud)[5]<-"first_bud"
names(last_bud)[5]<-"last_bud"
names(peak_fl)[5]<-"peak_fl"

first_last<-Reduce(function(x, y) merge(x, y, all=T), list(first_fl, last_fl, first_bud,last_bud,peak_fl))
head(first_last)

data_cgarden_short<-merge(data_cgarden[c(1:2,4,6,26:28)],first_last,all=T)

subset(data_cgarden_short,is.na(first_fl)&is.na(first_bud)) #All pls w data

#Add data temp site of origin to cgarden data
data_cgarden_short<-merge(data_cgarden_short,data_field[c(4,6)],by.x="mother_pl_id_old",by.y="pl_id")
names(data_cgarden_short)[13]<-"temp_ori"

#Calculate means and durations 
data_cgarden_short$mean_fl<-rowMeans(cbind(data_cgarden_short$first_fl,data_cgarden_short$last_fl))
data_cgarden_short$dur_fl<-as.integer(difftime(data_cgarden_short$last_fl,data_cgarden_short$first_fl,units=c("days")))

data_cgarden_short<-data_cgarden_short[c(2,3,1,5,4,6:12,14,15,13)] #Reorder

head(data_cgarden_short)
str(data_cgarden_short)


#Correspondence field-c.garden####

#Keep for analyses only those pls where both field and in c.garden data is available
#Keep also those in c.garden where seeds were taken from a closeby pl from that used in the field
#(and temperature value comes from the original plant)

#pls from c.garden with no correspondence in field
setdiff(data_cgarden_short$mother_pl_id_old, data_field$pl_id) #None! all plants from c.garden have field data

#pls from field with no correspondence in c.garden
setdiff(data_field$pl_id, data_cgarden_short$mother_pl_id_old) 
#[1] "109"  "1076" "597"  "504" 
#4 pls from field with no correspondence in c.garden-->remove from field data

data_field<-subset(data_field,!pl_id==109&!pl_id==1076&!pl_id==597&!pl_id==504)

write.table(data_field,"data/clean/cerastium_field_clean.txt",sep="\t",dec=".")
write.table(data_cgarden_short,"data/clean/cerastium_cgarden_clean.txt",sep="\t",dec=".")

#Use for analyses! --> other script

