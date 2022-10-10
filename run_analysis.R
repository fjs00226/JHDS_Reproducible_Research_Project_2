# download, read data and get a general idea of the data
url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
if(!file.exists("./raw_data")){
  dir.create("./raw_data")
}
download.file(url,"./raw_data/download.csv.bz2")
data<-read.csv("./raw_data/download.csv.bz2")
head(data)
str(data)
rn_data<-nrow(data)
rn_data #902297
data$PROPDMGEXP<-tolower(data$PROPDMGEXP)
data$CROPDMGEXP<-tolower(data$CROPDMGEXP)
# any duplication of data?
nrow(data[duplicated(data),]) # 0
# any data out of the date range?
data$BGN_DATE<-as.Date(data$BGN_DATE,format="%m/%d/%Y")
summary(data$BGN_DATE) # 1950-01-03-2011-11-30, no missing date

# Delete not relative columns
# Fatalities and injuries for question 1, property and crop damage for question 2 
data_v1<-data[,c("STATE","COUNTYNAME","STATE","EVTYPE","FATALITIES","INJURIES"
                 ,"PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")]

# Look at the PROPDMGEXP. It should only have "", k, b, and m
# use lower case for all labels
class(data_v1$PROPDMGEXP) # character
table(data_v1$PROPDMGEXP)
table(data_v1$CROPDMGEXP)
rn_data_v1<-nrow(data)
rn_data_v1 

# Across the United States, which types of events 
# (as indicated in the EVTYPE variable) are most harmful with respect 
# to population health?

# Clean the event type and group them
# Use lower case
data_v1$EVTYPE<-tolower(data_v1$EVTYPE)
# Replace "/", "-","\\"with " "
data_v1$EVTYPE<-gsub("/"," ",data_v1$EVTYPE)
data_v1$EVTYPE<-gsub("-"," ",data_v1$EVTYPE)
data_v1$EVTYPE<-trimws(data_v1$EVTYPE,"both")
events<-unique(data_v1$EVTYPE) # 865 types of different events
# EVTYPE with summary? remove them
sum(grepl("summary",data_v1$EVTYPE) ==TRUE) # 76 input with summary
data_v1<-data_v1[!grepl("summary",data_v1$EVTYPE),]
rn_data_v1<-nrow(data_v1)
rn_data_v1 # 902221 rows
# Remove numbers from events
data_v1$EVTYPE<-gsub('[0-9.]', "",data_v1$EVTYPE)
# run events again
events<-unique(data_v1$EVTYPE) # 729 types of different events
events # too many events types, need to group them
events<- paste(events,collapse = " ")
events<-as.data.frame(strsplit(events," "))
events_count<-as.data.frame(table(events))
colnames(events_count)<-c("word","counts")
events_count<-events_count[order(-events_count$counts),]
events_count$word
# regroup
data_v1[grepl("snow|snowfall|avalanche|snows",data_v1$EVTYPE),"EVCat"]<-"snow/avalanche"
data_v1[grepl("wind|winds|win|wind,|winds;|wins",data_v1$EVTYPE),"EVCat"]<-"wind"
data_v1[grepl("flood|flooding|floods|floodin|floooding",data_v1$EVTYPE),"EVCat"]<-"flood"
data_v1[grepl("rain|rainfall|rains|rain;|rainstorm|glaze",data_v1$EVTYPE),"EVCat"]<-"rain"
data_v1[grepl("thunderstorm|thunderstorms|storm|tstm|thunderstormw|thunderstrom
              |thuderstorm|thundeerstorm|thunderestorm|thundertorm|thundertsorm|thundestorm
              |thunerstorm|tstmw|tunderstorm",data_v1$EVTYPE),"EVCat"]<-"thunderstorm/storm"
data_v1[grepl("cold|ice|freezing|freeze|frost|icy|low",data_v1$EVTYPE),"EVCat"]<-"cold/ice"
data_v1[grepl("hail|hail()|hailstorm|hailstorms",data_v1$EVTYPE),"EVCat"]<-"hail"
data_v1[grepl("flash|lightning|lighting|ligntning",data_v1$EVTYPE),"EVCat"]<-"flash/lightning"
data_v1[grepl("heat|high|dry|drought|dryness|warmth|warm|hot",data_v1$EVTYPE),"EVCat"]<-"heat/dry"
data_v1[grepl("sleet",data_v1$EVTYPE),"EVCat"]<-"sleet"
data_v1[grepl("blizzard",data_v1$EVTYPE),"EVCat"]<-"blizzard"
data_v1[grepl("hurricane",data_v1$EVTYPE),"EVCat"]<-"hurricane"
data_v1[grepl("tornado|tornadoes|tornadoes,|tornados|torndao|gustnado",data_v1$EVTYPE),"EVCat"]<-"tornado"
data_v1[grepl("fog",data_v1$EVTYPE),"EVCat"]<-"fog"
data_v1[grepl("typhoon",data_v1$EVTYPE),"EVCat"]<-"typhoon"
data_v1[grepl("cloud|funnel",data_v1$EVTYPE),"EVCat"]<-"cloud"
data_v1[grepl("waterspout|water spout",data_v1$EVTYPE),"EVCat"]<-"waterspout"
data_v1[grepl("rip current",data_v1$EVTYPE),"EVCat"]<-"rip current"
data_v1[grepl("fire|wildfire",data_v1$EVTYPE),"EVCat"]<-"fire"
data_v1[grepl("slide",data_v1$EVTYPE),"EVCat"]<-"slide"
# any row without cat?
data_v1[is.na(data_v1$EVCat),"EVCat"]<-"other"

fatality<-aggregate(data_v1$FATALITIES,list(data_v1$EVCat),sum)
colnames(fatality)<-c("EVTYPE","fa_counts")
injury<-aggregate(data_v1$INJURIES,list(data_v1$EVCat),sum)
colnames(injury)<-c("EVTYPE","in_counts")

# merge fatality and injury
po_health<-merge(fatality,injury,by="EVTYPE")
colnames(po_health)<-c("EVTYPE","fatality","injury")
po_health$SUM<-po_health$fatality+po_health$injury
po_health<-po_health[order(-po_health$SUM),]
po_health_5<-po_health[1:5,]

#plot
barplot(t(as.matrix(po_health_5))[2:3,1:5]
        ,names.arg=c("tornado","heat/dry","thunderstorm\n/storm","flash\n/lightning","flood")
        ,col=c("green","blue")
        ,main="Fatality and injury of harmful events",xlab="Event type",ylab="Counts"
        ,legend.text=c("fatality","injury"))
# Tornado is the most harmful event for public health.

# Across the United States, which types of events have 
# the greatest economic consequences?
# property damage and crop damage
# remove PROPDMGEXP not in "", k, b, m
data_v2<-data_v1[data_v1$PROPDMGEXP %in% c("","k","b","m"),]
rn_data_v2<-nrow(data_v2)
table(data_v2$PROPDMGEXP) # only have "", b, m, k
rn_data_v1-rn_data_v2 # 321 rows removed
# remove CROPDMGEXP not in "", k, b, m
data_v3<-data_v1[data_v1$CROPDMGEXP %in% c("","k","b","m"),]
rn_data_v3<-nrow(data_v3) # 902270
table(data_v3$CROPDMGEXP) # only have "", b, m, k
rn_data_v1-rn_data_v3 # 27 rows removed
# recalculate DMG
data_v2[which(data_v2$PROPDMGEXP==""),"PROPDMG_D"]<-data_v2[which(data_v2$PROPDMGEXP==""),"PROPDMG"]
data_v2[which(data_v2$PROPDMGEXP=="k"),"PROPDMG_D"]<-data_v2[which(data_v2$PROPDMGEXP=="k"),"PROPDMG"]*1000
data_v2[which(data_v2$PROPDMGEXP=="b"),"PROPDMG_D"]<-data_v2[which(data_v2$PROPDMGEXP=="b"),"PROPDMG"]*1000000000
data_v2[which(data_v2$PROPDMGEXP=="m"),"PROPDMG_D"]<-data_v2[which(data_v2$PROPDMGEXP=="m"),"PROPDMG"]*1000000

data_v3[which(data_v3$CROPDMGEXP==""),"CROPDMG_D"]<-data_v3[which(data_v3$CROPDMGEXP==""),"CROPDMG"]
data_v3[which(data_v3$CROPDMGEXP=="k"),"CROPDMG_D"]<-data_v3[which(data_v3$CROPDMGEXP=="k"),"CROPDMG"]*1000
data_v3[which(data_v3$CROPDMGEXP=="b"),"CROPDMG_D"]<-data_v3[which(data_v3$CROPDMGEXP=="b"),"CROPDMG"]*1000000000
data_v3[which(data_v3$CROPDMGEXP=="m"),"CROPDMG_D"]<-data_v3[which(data_v3$CROPDMGEXP=="m"),"CROPDMG"]*1000000
# Sum property damage and crop damage
property_dmg<-aggregate(data_v2$PROPDMG_D,list(data_v2$EVCat),sum)
colnames(property_dmg)<-c("EVTYPE","DMG")
crop_dmg<-aggregate(data_v3$CROPDMG_D,list(data_v3$EVCat),sum)
colnames(crop_dmg)<-c("EVTYPE","DMG")
# merge property damage and crop damage
dmg<-merge(property_dmg,crop_dmg,by="EVTYPE")
colnames(dmg)<-c("EVTYPE","PROPDMG","CROPDMG")
dmg$SUM<-dmg$PROPDMG+dmg$CROPDMG
# order and get the top 5 sum
dmg<-dmg[order(-dmg$SUM),]
dmg_5<-dmg[1:5,]

# plot
barplot(t(as.matrix(dmg_5))[2:3,1:5]
        ,main="Economic damage of harmful events"
        ,names.arg=c("flood","thunderstorm\n/storm","typhoon","tornado","heat/dry")
        ,col=c("blue","green")
        ,xlab="Event type", ylab="Dollars",legend.text =c("property","crop"))

