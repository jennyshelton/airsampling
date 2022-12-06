
##################
### Load files ###
##################

library(ggplot2)

#### SS18 + landcover + tasmax
setwd("C:/Users/jenny/Dropbox/Jen's desktop/NERC PhD/spatialdata/MetOfficedata/SS18")
ss18 <- read.csv("SS18_UKonly_landcover1-21_tasmax.csv")

#### AE18 + landcover + tasmax
setwd("C:/Users/jenny/Dropbox/Jen's desktop/NERC PhD/spatialdata/MetOfficedata/AE18")
ae18 <- read.csv("AE18_UKonly_landcover1-21_tasmax.csv")

#### WS18 + landcover + tasmax
setwd("C:/Users/jenny/Dropbox/Jen's desktop/NERC PhD/spatialdata/MetOfficedata/WS18")
ws18 <- read.csv("WS18_UKonly_landcover1-21_tasmax.csv")

#### SA19 + landcover + tasmax
setwd("C:/Users/jenny/Dropbox/Jen's desktop/NERC PhD/spatialdata/MetOfficedata/SA19")
sa19 <- read.csv("SA19_UKonly_landcover1-21_tasmax.csv")

#### unique composter locations with landcover
## England
setwd("C:/Users/jenny/Dropbox/Jen's desktop/NERC PhD/spatialdata/EA_Pippa_composters")
engcomp <- read.csv("Final list of included sites_unique_landcover.csv")
engcomp$country <- "England"
## Wales
setwd("C:/Users/jenny/Dropbox/Jen's desktop/NERC PhD/spatialdata/Welsh_composters")
welshcomp <- read.csv("FINAL_included_Welsh_activecomposters_landcover.csv")
welshcomp$country <- "Wales"
## Scotland
setwd("C:/Users/jenny/Dropbox/Jen's desktop/NERC PhD/spatialdata/Scottish_composters")
scotcomp <- read.csv("Scottish_operationalcomposters_unique_landcover.csv")
scotcomp$country <- "Scotland"
## NI
setwd("C:/Users/jenny/Dropbox/Jen's desktop/NERC PhD/spatialdata/NI_composters")
nicomp <- read.csv("NI_composters_landcover.csv")
nicomp$country <- "NI"
## combined
ukcomp <- data.frame(postcode = c(engcomp$postcode_edit, welshcomp$postcode_edit, scotcomp$postcode_edit, nicomp$postcode_e),
                     latitude = c(engcomp$Latitude, welshcomp$latitude, scotcomp$latitude, nicomp$Latitude),
                     longitude = c(engcomp$Longitude, welshcomp$longitude, scotcomp$longitude, nicomp$Longitude),
                     landcover = c(engcomp$landcover1, welshcomp$landcover_1, scotcomp$landcover1, nicomp$landcover1),
                     country = c(engcomp$country, welshcomp$country, scotcomp$country, nicomp$country))
remove(engcomp); remove(welshcomp); remove(scotcomp); remove(nicomp)

#### Load in Scotland, Wales and Northern Ireland postcodes from https://www.doogal.co.uk/PostcodeDownloads.php
setwd("C:/Users/jenny/Dropbox/Jen's desktop/NERC PhD/spatialdata/UKpostcodes_Doogal")
englandpc <- read.csv("England postcodes_inuse.csv")   # 1,464,262
scotlandpc <- read.csv("Scotland postcodes_inuse.csv") #   158,819
walespc <- read.csv("Wales postcodes_inuse.csv")       #    91,509
nipc <- read.csv("BT postcodes_inuse.csv")             #    62,052
## combine the list
allukpc <- data.frame(postcode = c(englandpc$Postcode, scotlandpc$Postcode, walespc$Postcode, nipc$Postcode),
                      latitude = c(englandpc$Latitude, scotlandpc$Latitude, walespc$Latitude, nipc$Latitude),
                      longitude = c(englandpc$Longitude, scotlandpc$Longitude, walespc$Longitude, nipc$Longitude))
remove(englandpc); remove(scotlandpc); remove(walespc); remove(nipc)

#### Load in % arable land in 2km buffer for GB and NI
setwd("C:/Users/jenny/Dropbox/Jen's desktop/NERC PhD/spatialdata/arable_buffer/airsamples")
gbbuffer <- read.csv("GB_uniqpc_2kmbuffer_arable.csv")
nibuffer <- read.csv("NI_uniqpc_2kmbuffer_arable.csv")
ukbuffer <- data.frame(postcode = c(gbbuffer$postcode, nibuffer$postcode),
                       percentarable = c(gbbuffer$arab_perc, nibuffer$ni.lcm.2_1))

#--------------------------------------------------------------------------------------------------

### Spreadsheet of all samples combined across sampling dates

## Take out postcode & lat/longs from each air sampling spreadsheet and combine into new dataframe
locs <- data.frame(round = c(rep("21June2018",712), rep("24Sept2018",398), rep("21Dec2018",321), rep("20March2019",463)),
                   sample = c(ss18$Code, ae18$Code, ws18$Code, sa19$Code),
                   date_collected = c(ss18$Date_colle, ae18$Date_colle, ws18$Date_colle, sa19$Date_colle),
                   postcode = c(ss18$Location_p, ae18$Location_p, ws18$Location_p, sa19$Location_p), 
                   lat = c(ss18$Latitude, ae18$Latitude, ws18$Latitude, sa19$Latitude), 
                   long = c(ss18$Longitude, ae18$Longitude, ws18$Longitude, sa19$Longitude),
                   easting = c(ss18$easting, ae18$Easting, ws18$easting, sa19$easting),
                   northing = c(ss18$northing, ae18$Northing, ws18$northing, sa19$northing),
                   landcover = c(ss18$SAMPLE_1_e, ae18$SAMPLE_1_e, ws18$SAMPLE_1_e, sa19$SAMPLE_1_e),
                   maxtemp = c(ss18$tasmax, ae18$tasmax, ws18$tasmax, sa19$tasmax),
                   numafum = c(ss18$number_col, ae18$number_col, ws18$number_col, sa19$number_col),
                   numaraf = c(ss18$number_ARA, ae18$ARAf_numbe, ws18$number_ARA, sa19$number_ARA))

## Code 01 for Afum and ARAf
locs$afum01 <- 0; locs$afum01[locs$numafum > 0] <- 1; table(locs$afum01)
locs$araf01 <- 0; locs$araf01[locs$numaraf > 0] <- 1; table(locs$araf01)

## Code urban or rural
locs$urban_rural <- "urban"; locs$urban_rural[locs$landcover < 20] <- "rural"; table(locs$urban_rural)

## Make landcover as factor so it is not cseen as a continuous variable in GLMS
locs$landcover <- as.factor(locs$landcover)

## Reorder levels of sampling round
table(locs$round)
#locs$round <- factor(locs$round, levels=c("21June2018","24Sept2018","21Dec2018","20March2019"), labels=c("summer/n(n=712)","autumn/n(n=398)","winter/n(n=320)","spring/n(n=463)"))
locs$round <- factor(locs$round, levels=c("21June2018","24Sept2018","21Dec2018","20March2019"), labels=c("summer","autumn","winter","spring"))
levels(locs$round)

## Assign country to samples for later subsetting
locs$country <- "England"
locs$country[locs$postcode %in% wales$Postcode.1] <- "Wales"
locs$country[locs$postcode %in% c("AF1","AF2")] <- "Wales"
locs$country[locs$postcode %in% scotland$Postcode_edit] <- "Scotland"
locs$country[locs$postcode %in% c("AiS2","PT1","PT2","AS1","AS2","JR_ap","CL1","CL2","EW1","EW2","MR2")] <- "Scotland"
locs$country[locs$lat > 51.38 & locs$long < -5.6 & locs$lat < 55.5] <- "Ireland"
locs$country[locs$postcode %in% ni$Postcode.1] <- "N.Ireland"
locs$country[locs$postcode %in% c("BT370GB","BT477SB")] <- "N.Ireland"
table(locs$country)

#remove DX76HR as has no lat/long so lacks associated data
locs <- locs[locs$postcode != "DX76HR",]

## Combine locs with arable buffer data
testdata <- merge(locs, ukbuffer, by = "postcode", all.x = T)
testdata$percentarable[is.na(testdata$percentarable)] <- 0

## Ireland needs to be excluded as does not have max temp data
uklocs <- testdata[testdata$country != "Ireland",] #1889
table(uklocs$country)

#--------------------------------------------------------------------------------------------------

### Unique locations across sampling dates

## Sort all locations by postcodes and visually inspect to make sure samples without postcodes are identical between sampling rounds
sort <- uklocs[order(uklocs$postcode),]
## Now filter combined dataframe to get unique postcodes
length(unique(uklocs$postcode)) #799
uniqpc <- uklocs[!duplicated(uklocs[,"postcode"]),]

### Unique locations for composters

length(unique(ukcomp$postcode)) #257
setwd("C:/Users/jenny/Dropbox/Jen's desktop/NERC PhD/spatialdata/UK_composters")
write.table(ukcomp, "UKcomposters_unique_landcover.csv", sep=",", row.names = F)
table(ukcomp$landcover, ukcomp$country)

#---------------------------------------------------------------------------------------

### Calculate distance between English samples and closest composter

## Calculate distance between lat and long locations
#install.packages("geosphere")
library(geosphere)
distHaversine(c(-2.143308, 57.14092), c(-2.108829, 57.15976)) #AB15 6BG to AB24 3NU = 3km, verified using https://www.doogal.co.uk/MeasureDistances.php


## Loop to calculate distance from each unique UK sampling postcode and its nearest composter
df <- data.frame()
for (j in 1:nrow(uniqpc)){
  vector <- vector()
  for (i in 1:nrow(ukcomp)){ #where i is composter
    distance <- distHaversine(c(uniqpc$long[j],uniqpc$lat[j]),c(ukcomp$longitude[i],ukcomp$latitude[i]))
    vector <- c(vector, distance)
    data <- data.frame(samplepostcode = uniqpc$postcode[j], shortestdistance = min(vector), composterpostcode = ukcomp$postcode[which.min(vector)], composterlandcover = ukcomp$landcover[which.min(vector)])
  }
  df <- rbind(df,data)
}

## Combine UK sampling locations with distance to closest composter
uklocscomp <- merge(uklocs, df, by.x = "postcode", by.y = "samplepostcode")
setwd("C:/Users/jenny/Dropbox/Jen's desktop/NERC PhD/spatialdata/UK_composters")
write.table(uklocscomp, "UKsamplinglocations_nearestopenwindrowcomposter.csv", sep=",", row.names = F)

subset <- allukpc[1298886:nrow(allukpc),]

## Loop to calculate distance from each unique all UK postcode and its nearest composter
df <- data.frame()
for (j in 1:nrow(subset)){
  vector <- vector()
  for (i in 1:nrow(ukcomp)){ #where i is composter
    distance <- distHaversine(c(subset$longitude[j],subset$latitude[j]),c(ukcomp$longitude[i],ukcomp$latitude[i]))
    vector <- c(vector, distance)
    data <- data.frame(samplepostcode = subset$postcode[j], shortestdistance = min(vector), composterpostcode = ukcomp$postcode[which.min(vector)])
  }
  df <- rbind(df,data)
}

## Do this in chunks and run it overnight
#write.table(df, "allUKactivepostcodes_distanceOWOAcomposter_1-367594.csv", sep=",", row.names = F)
#write.table(df, "allUKactivepostcodes_distanceOWOAcomposter_367595-651546.csv", sep=",", row.names = F)
#write.table(df, "allUKactivepostcodes_distanceOWOAcomposter_651547-1298885.csv", sep=",", row.names = F)
#write.table(df, "allUKactivepostcodes_distanceOWOAcomposter_1298886-1776642.csv", sep=",", row.names = F)

setwd("C:/Users/jenny/Dropbox/Jen's desktop/NERC PhD/spatialdata/UKpostcodes_Doogal")
df1 <- read.csv("allUKactivepostcodes_distanceOWOAcomposter_1-367594.csv")
df2 <- read.csv("allUKactivepostcodes_distanceOWOAcomposter_367595-651546.csv")
df3 <- read.csv("allUKactivepostcodes_distanceOWOAcomposter_651547-1298885.csv")
df4 <- read.csv("allUKactivepostcodes_distanceOWOAcomposter_1298886-1776642.csv")

allukpccomp <- rbind(df1, df2, df3, df4)
allukpccomp$shortestdistance_km <- allukpccomp$shortestdistance/1000
# something weird happening with 5609.3572km -> exclude
filter <- allukpccomp[allukpccomp$shortestdistance_km < 5609,]
summary(filter$shortestdistance_km)
length(which(filter$shortestdistance_km <= 2))
inc <- allukpccomp[allukpccomp$shortestdistance_km <= 90,]

ggplot(inc, aes(x=shortestdistance_km)) + 
  geom_histogram(binwidth = 1,color="darkblue", fill="lightblue") +
  labs(x="Distance to closest OW/OA composting site (km)", y="Count of UK postcodes") 
#theme(axis.title.x = ggtext::element_markdown())

#---------------------------------------------------------------------------------------

### uklocscomp is UK (exc RoI & "DX76HR") data with distance to nearest composter for GLMs

setwd("C:/Users/jenny/Dropbox/Jen's desktop/NERC PhD/spatialdata/GWR")

## Classify composter landcover as urban or rural
uklocscomp$urban_rural_composter <- "urban"
uklocscomp$urban_rural_composter[uklocscomp$composterlandcover < 20] <- "rural"
table(uklocscomp$urban_rural_composter)
## Need to make composter landcover a factor so it doesn't count as continuous
uklocscomp$composterlandcover <- as.factor(uklocscomp$composterlandcover)

#- UK logistic regression for Afum01 uses uklocscomp (n=1889) - models1
write.table(uklocscomp, "n1889_UKlogisticregression_GWR.csv", sep=",", row.names = F)

#- UK negative binomial regression for numAfum uses possamp -models2
possamp <- uklocscomp[uklocscomp$numafum > 0,] #918
write.table(possamp, "n918_numafum_UKnegativebinomialregression_GWR.csv", sep=",", row.names = F)
#- UK logistic regression for ARAf01 uses uklocs (n=1889) - models3
#- UK negative binomial regression for numARAf uses arafsamp - models4
arafsamp <- uklocscomp[uklocscomp$numaraf > 0,] #73
write.table(arafsamp, "n73_numARAf_UKnegativebinomialregression_GWR.csv", sep=",", row.names = F)

uklocscomp <- read.csv("n1889_UKlogisticregression_GWR.csv")

#---------------------------------------------------------------------------------------

### GLM models

# https://stats.idre.ucla.edu/r/dae/logit-regression/

## My continuous data is count data that is not Normally-distributed so this website suggests poisson regression is best: https://bookdown.org/ndphillips/YaRrr/regression-on-non-normal-data-with-glm.html
## Overdispersion means negative binomial regression is required

### Binomial regression: UK samples grow Afum (0/1)

#- UK logistic regression for Afum01 uses uklocscomp (n=1889) - models1
setwd("C:/Users/jenny/Dropbox/Jen's desktop/NERC PhD/spatialdata/GWR")
uklocscomp <- read.csv("n1889_UKlogisticregression_GWR.csv")
uklocscomp$landcover <- as.factor(uklocscomp$landcover)
uklocscomp$round <- factor(uklocscomp$round, levels=c("summer","autumn","winter","spring"))

null1 <- glm(uklocscomp$afum01 ~ 1, family="binomial")
summary(null1) #2619.2

### CLIMATE variables
## Afum01 ~ season
model1a <- glm(uklocscomp$afum01 ~ uklocscomp$round, family="binomial")
summary(model1a) #AIC 2574.9
table(uklocscomp$afum01, uklocscomp$round)
## Afum01 ~ date collected
model1b <- glm(uklocscomp$afum01 ~ uklocscomp$date_collected, family="binomial")
summary(model1b) #AIC 2588.8
## Afum01 ~ max temp
model1c <- glm(uklocscomp$afum01 ~ uklocscomp$maxtemp, family="binomial")
summary(model1c) #AIC 2591.9
# Compare models 1a-c to null model
anova(null1, model1a, test="Chisq") # Models 1a-c are all a significant improvement on the null model

### LANDUSE variables
## Afum01 ~ land cover
model1d <- glm(uklocscomp$afum01 ~ uklocscomp$landcover, family="binomial")
summary(model1d) #AIC 2615.8
## Afum01 ~ urban/rural
model1e <- glm(uklocscomp$afum01 ~ uklocscomp$urban_rural, family="binomial")
summary(model1e) #AIC 2619.8
## Afum01 ~ arable buffer
model1f <- glm(uklocscomp$afum01 ~ uklocscomp$percentarable, family="binomial")
summary(model1f) #AIC 2617.5
## Afum01 ~ nearest composter
model1g <- glm(uklocscomp$afum01 ~ uklocscomp$shortestdistance, family="binomial")
summary(model1g) #AIC 2606.2
## Afum01 ~ composter landcover
model1h <- glm(uklocscomp$afum01 ~ uklocscomp$composterlandcover, family="binomial")
summary(model1h) #AIC 2610.7
## Afum01 ~ composter urban or rural
model1i <- glm(uklocscomp$afum01 ~ uklocscomp$urban_rural_composter, family="binomial")
summary(model1i) #AIC 2618

# Compare models 1d-f to null model
anova(null1, model1d, test="Chisq") # Models d, g and h are a significant improvement on the null model, but models e, f and i aren't.

## Cannot include season and date collected in same model as nested
## Afum01 ~ season + max temp + land cover + percent arable + nearest composter + composter landcover
model1acdfgh <- glm(uklocscomp$afum01 ~ uklocscomp$round + uklocscomp$maxtemp + uklocscomp$landcover + uklocscomp$percentarable + uklocscomp$shortestdistance + uklocscomp$composterlandcover, family="binomial")
summary(model1acdfgh) #AIC 2558.6
## Afum01 ~ date collected + max temp + land cover + percent arable + nearest composter + composter landcover
model1bcdfgh <- glm(uklocscomp$afum01 ~ uklocscomp$date_collected + uklocscomp$maxtemp + uklocscomp$landcover + uklocscomp$percentarable + uklocscomp$shortestdistance + uklocscomp$composterlandcover, family="binomial")
summary(model1bcdfgh) #AIC 2574.3
## As percent arable buffer is only marginally significant, does it need to stay in?
## Afum01 ~ season + max temp + land cover + nearest composter
model1acdgh <- glm(uklocscomp$afum01 ~ uklocscomp$round + uklocscomp$maxtemp + uklocscomp$landcover + uklocscomp$shortestdistance + uklocscomp$composterlandcover, family="binomial")
summary(model1acdgh) #AIC 2560.9

## Does including an interaction between composter distance & landcover improve the fit?
## Afum01 ~ season + max temp + land cover + percent arable + nearest composter*composter landcover
model1acdfg_h <- glm(uklocscomp$afum01 ~ uklocscomp$round + uklocscomp$maxtemp + uklocscomp$landcover + uklocscomp$percentarable + uklocscomp$shortestdistance*uklocscomp$composterlandcover, family="binomial")
summary(model1acdfg_h) #AIC 2563.8

## Model1acdfgh is best
exp(cbind(OR=coef(model1acdfgh), confint(model1acdfgh)))

#------------------------------
### Model1acdg for airARAfpaper
#null1 = AIC 2619.2, model1a = AIC 2574.9, model1c = AIC 2591.9, model1d = AIC 2615.8, model1g = AIC 2606.2
model1ac <- glm(uklocscomp$afum01 ~ uklocscomp$round + uklocscomp$maxtemp, family="binomial"); summary(model1ac) #2571.4
model1ad <- glm(uklocscomp$afum01 ~ uklocscomp$round + uklocscomp$landcover, family="binomial"); summary(model1ad) #2572
model1ag <- glm(uklocscomp$afum01 ~ uklocscomp$round + uklocscomp$shortestdistance, family="binomial"); summary(model1ag) #2563.4
model1cd <- glm(uklocscomp$afum01 ~ uklocscomp$maxtemp + uklocscomp$landcover, family="binomial"); summary(model1cd) #2590.7
model1cg <- glm(uklocscomp$afum01 ~ uklocscomp$maxtemp + uklocscomp$shortestdistance, family="binomial"); summary(model1cg) #2583.8
model1dg <- glm(uklocscomp$afum01 ~ uklocscomp$landcover + uklocscomp$shortestdistance, family="binomial"); summary(model1dg) #2607.8

model1acd <- glm(uklocscomp$afum01 ~ uklocscomp$round + uklocscomp$maxtemp + uklocscomp$landcover, family="binomial"); summary(model1acd) #2570
model1acg <- glm(uklocscomp$afum01 ~ uklocscomp$round + uklocscomp$maxtemp + uklocscomp$shortestdistance, family="binomial"); summary(model1acg) #2563.1
model1cdg <- glm(uklocscomp$afum01 ~ uklocscomp$maxtemp + uklocscomp$landcover + uklocscomp$shortestdistance, family="binomial"); summary(model1cdg) #2586


model1acdg <- glm(uklocscomp$afum01 ~ uklocscomp$round + uklocscomp$maxtemp + uklocscomp$landcover + uklocscomp$shortestdistance, family="binomial"); summary(model1acdg) #AIC = 2564.9
#model1acg has the lowest AIC
exp(cbind(OR=coef(model1acg), confint(model1acg)))
#------------------------------


### Tables/plots of significant variables
# sampling round - stacked % barplot
tab <- data.frame(table(uklocscomp$afum01, uklocscomp$round))
names(tab) <- c("afum01","round","value")
tab$round <- factor(tab$round, levels=c("summer","autumn","winter","spring"))
tab$afum01 <- factor(tab$afum01, levels=c(1,0), labels=c("yes","no"))

ggplot(tab, aes(x=round, y=value, fill=afum01)) +
  geom_bar(position="fill", stat="identity") +
  labs(x="Sampling round", y="Percentage of samples (%)", fill="*A. fumigatus*") +
  theme(legend.title = ggtext::element_markdown()) +
  scale_x_discrete(labels=c("summer\n(n=712)","autumn\n(n=398)","winter\n(n=320)","spring\n(n=463)"))

# max temp - boxplot
uklocscomp$afum01 <- factor(uklocscomp$afum01, levels=c(0,1))

table(uklocscomp$afum01)

ggplot(uklocscomp, aes(x=afum01, y=maxtemp)) +
  geom_boxplot(varwidth = T, color="darkblue", fill="lightblue") +
  scale_y_continuous(breaks = seq(0, 30, by = 5)) +
  labs(x="Growth of *A. fumigatus* from sample", y="Maximum daily temperature at sampling location (oC)") +
  theme(axis.title.x = ggtext::element_markdown()) +
  scale_x_discrete(labels=c("no\n(n=971)","yes\n(n=918)"))

# land cover of sampling location - stacked % barplot
tab <- data.frame(table(uklocscomp$afum01, uklocscomp$landcover))
names(tab) <- c("afum01","landcover","value")
tab$afum01 <- factor(tab$afum01, levels=c(1,0), labels=c("yes","no"))

table(uklocscomp$landcover)

ggplot(tab, aes(x=landcover, y=value, fill=afum01)) +
  geom_bar(position="fill", stat="identity") +
  labs(x="Land cover class of sampling location", y="Percentage of samples (%)", fill="*A. fumigatus*") +
  theme(legend.title = ggtext::element_markdown()) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_x_discrete(labels=c("1 (n=27)","2 (n=1)","3 (n=29)","4 (n=87)","5 (n=2)","10 (n=2)","20 (n=455)","21 (n=1,286)"))

# proximity to composter - boxplot
uklocscomp$shortestdistance_km <- uklocscomp$shortestdistance/1000

ggplot(uklocscomp, aes(x=afum01, y=shortestdistance_km)) +
  geom_boxplot(varwidth = T, color="darkblue", fill="lightblue") +
  labs(x="Growth of *A. fumigatus* from sample", y="Distance to closest OW/OA composting site (km)") +
  theme(axis.title.x = ggtext::element_markdown()) +
  scale_x_discrete(labels=c("no\n(n=971)","yes\n(n=918)"))


# land cover of sampling location - stacked % barplot
tab <- data.frame(table(uklocscomp$afum01, uklocscomp$composterlandcover))
names(tab) <- c("afum01","landcover","value")
tab$afum01 <- factor(tab$afum01, levels=c(1,0), labels=c("yes","no"))

table(uklocscomp$composterlandcover)

ggplot(tab, aes(x=landcover, y=value, fill=afum01)) +
  geom_bar(position="fill", stat="identity") +
  labs(x="Land cover class of nearest OW/OA composter", y="Percentage of samples (%)", fill="*A. fumigatus*") +
  theme(legend.title = ggtext::element_markdown()) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_x_discrete(labels=c("1 (n=56)","3 (n=337)","4 (n=421)","5 (n=65)","7 (n=18)","12 (n=7)","16 (n=27)","19 (n=2)","20 (n=416)","21 (n=540)"))

# percent arable - boxplot
ggplot(uklocscomp, aes(x=afum01, y=percentarable)) +
  geom_boxplot(varwidth = T, color="darkblue", fill="lightblue") +
  labs(x="Growth of *A. fumigatus* from sample", y="Percentage of arable land in 2km buffer\nsurrounding sampling location (%)") +
  theme(axis.title.x = ggtext::element_markdown()) +
  scale_x_discrete(labels=c("no\n(n=971)","yes\n(n=918)"))


#---------------------------------------------------------------------------------------------------------------------------------

### Negative binomial regression: positive UK samples, number of Afum grown

## Ratio of residual variance to df should be one. Greater than this indicates over-dispersion -> use quassipoison. 
## https://biometry.github.io/APES/LectureNotes/2016-JAGS/Overdispersion/OverdispersionJAGS.pdf

#- UK negative binomial regression for numAfum uses possamp -models2
possamp <- uklocscomp[uklocscomp$numafum > 0,] #918
#setwd("C:/Users/jenny/Dropbox/Jen's desktop/NERC PhD/spatialdata/GWR")
#possamp <- read.csv("n918_numafum_UKnegativebinomialregression_GWR.csv")


library(MASS)

null2_nb <- glm.nb(possamp$numafum ~ 1)
summary(null2_nb) #3770.6

### CLIMATE variables
## Afum samples: numAfum ~ season
model2a_nb <- glm.nb(possamp$numafum ~ possamp$round)
summary(model2a_nb) #3740.8
## Afum samples: numAfum ~ date collected
model2b_nb <- glm.nb(possamp$numafum ~ possamp$date_collected)
summary(model2b_nb) #3766.4
## Afum samples: numAfum ~ maximum air temperature
model2c_nb <- glm.nb(possamp$numafum ~ possamp$maxtemp)
summary(model2c_nb) #3772.6
anova(null2_nb, model2b_nb) # a and b are significant improvements on null, but not c.

### LANDUSE variables
## Afum samples: numAfum ~ land cover
model2d_nb <- glm.nb(possamp$numafum ~ possamp$landcover)
summary(model2d_nb) #AIC 3770.7
## Afum samples: numAfum ~ urban/rural
model2e_nb <- glm.nb(possamp$numafum ~ possamp$urban_rural)
summary(model2e_nb) #AIC 3772.5
## Afum samples: numAfum ~ arable buffer
model2f_nb <- glm.nb(possamp$numafum ~ possamp$percentarable)
summary(model2f_nb) #AIC 3772.5
## Afum samples: numAfum ~ nearest composter
model2g_nb <- glm.nb(possamp$numafum ~ possamp$shortestdistance)
summary(model2g_nb) #AIC 3762.4
## Afum samples: numAfum ~ composter landcover
model2h_nb <- glm.nb(possamp$numafum ~ possamp$composterlandcover)
summary(model2h_nb) #AIC 3777.9
## Afum samples: numAfum ~ composter urban or rural
model2i_nb <- glm.nb(possamp$numafum ~ possamp$urban_rural_composter)
summary(model2i_nb) #AIC 3768.8

anova(null2_nb, model2i_nb) # d-f & h are not a significant improvement on null, but g is. i is marginal (p = 0.05)

## Cannot include season and date collected in same model as nested
## Afum samples: numAfum ~ season + nearest composter
model2ag_nb <- glm.nb(possamp$numafum ~ possamp$round + possamp$shortestdistance)
summary(model2ag_nb) #3732.7
## Afum samples: numAfum ~ date collected + nearest composter
model2bg_nb <- glm.nb(possamp$numafum ~ possamp$date_collected + possamp$shortestdistance)
summary(model2bg_nb) #3757.3

## Composter urban or rural was marginally significant. Does it improve the model?
## Afum samples: numAfum ~ season + nearest composter + composter urban or rural
model2agi_nb <- glm.nb(possamp$numafum ~ possamp$round + possamp$shortestdistance + possamp$urban_rural_composter)
summary(model2agi_nb) #3730.3

## Model agi is best

### Tables/plots of significant variables

# sampling round - boxplot
table(possamp$round)
possamp$round <- factor(possamp$round, levels=c("summer","autumn","winter","spring"))

ggplot(possamp, aes(x=round, y=numafum)) + 
  geom_boxplot(varwidth = T, color="darkblue", fill="lightblue") +
  scale_y_continuous(breaks = seq(0, 35, by = 5)) +
  labs(x="Sampling round", y="Number of *A. fumigatus* colonies grown from sample") +
  theme(axis.title.y = ggtext::element_markdown()) +
  scale_x_discrete(labels=c("summer\n(n=408)","autumn\n(n=190)","winter\n(n=152)","spring\n(n=168)"))

# proximity to nearest composter - scatterplot
ggplot(possamp, aes(x=shortestdistance_km, y=numafum)) + 
  geom_point(color="darkblue", fill="lightblue") +
  geom_smooth(method=lm) +
  scale_y_continuous(breaks = seq(0, 35, by = 5)) +
  labs(x="Distance to closest OW/OA composting facility (km)", y="Number of *A. fumigatus* colonies grown from sample") +
  theme(axis.title.y = ggtext::element_markdown())

# urban/rural composter - boxplot
table(possamp$urban_rural_composter)

ggplot(possamp, aes(x=urban_rural_composter, y=numafum)) +
  geom_boxplot(varwidth = T, color="darkblue", fill="lightblue") +
  scale_y_continuous(breaks = seq(0, 35, by = 5)) +
  labs(x="Rural or urban location of closest OW/OA composting facility", y="Number of *A. fumigatus* colonies grown from sample") +
  theme(axis.title.y = ggtext::element_markdown()) + 
  scale_x_discrete(labels=c("rural\n(n=434)","urban\n(n=484)"))


#-------

### Binomial regression: UK samples grow ARAf (0/1)

null3 <- glm(uklocscomp$araf01 ~ 1, family="binomial")
summary(null3) #620.13

### CLIMATE variables
# ARAf01 ~ season
model3a <- glm(uklocscomp$araf01 ~ uklocscomp$round, family="binomial")
summary(model3a) #AIC 625.7
# ARAf01 ~ date collected
model3b <- glm(uklocscomp$araf01 ~ uklocscomp$date_collected, family="binomial")
summary(model3b) #AIC 685.12
# ARAf01 ~ maximum daily temperature
model3c <- glm(uklocscomp$araf01 ~ uklocscomp$maxtemp, family="binomial")
summary(model3c) #AIC 621.75
anova(null3, model3c, test="Chisq") #a-c do not significantly improve on the null model

### LANDUSE variables
# ARAf01 ~ landcover
model3d <- glm(uklocscomp$araf01 ~ uklocscomp$landcover, family="binomial")
summary(model3d) #AIC 630.17
# ARAf01 ~ urban/rural
model3e <- glm(uklocscomp$araf01 ~ uklocscomp$urban_rural, family="binomial")
summary(model3e) #AIC 621.49
# ARAf01 ~ percent arable buffer
model3f <- glm(uklocscomp$araf01 ~ uklocscomp$percentarable, family="binomial")
summary(model3f) #AIC 621.98
# ARAf01 ~ nearest composter
model3g <- glm(uklocscomp$araf01 ~ uklocscomp$shortestdistance, family="binomial")
summary(model3g) #AIC 622.13
# ARAf01 ~ composter landcover
model3h <- glm(uklocscomp$araf01 ~ uklocscomp$composterlandcover, family="binomial")
summary(model3h) #AIC 632.33
# ARAf01 ~ composter landcover
model3i <- glm(uklocscomp$araf01 ~ uklocscomp$urban_rural_composter, family="binomial")
summary(model3i) #AIC 619.37


anova(null3, model3i, test="Chisq") #d-i do not significantly improve on the null model


#-------

### Negative binomial regression: ARAf-positive UK samples, number of ARAf grown

library(MASS) #for glm.nb

null4_nb <- glm.nb(arafsamp$numaraf ~ 1)
summary(null4_nb) #219.73

### CLIMATE variables
## ARAf samples: numARAf ~ season
model4a_nb <- glm.nb(arafsamp$numaraf ~ arafsamp$round)
summary(model4a_nb) #223.87
## ARAf samples: numARAf ~ date collected
model4b_nb <- glm.nb(arafsamp$numaraf ~ arafsamp$date_collected)
summary(model4b_nb) #238
## ARAf samples: numARAf ~ maximum air temperature
model4c_nb <- glm.nb(arafsamp$numaraf ~ arafsamp$maxtemp)
summary(model4c_nb) #221.07
anova(null4_nb, model4c_nb) # none improve on the null model.

hist(arafsamp$numaraf, binwidth=1)
boxplot(arafsamp$numaraf ~ arafsamp$round)

### LANDUSE variables
## ARAf samples: numARAf ~ land cover
model4d_nb <- glm.nb(arafsamp$numaraf ~ arafsamp$landcover)
summary(model4d_nb) #AIC 225.42
## ARAf samples: numARAf ~ urban/rural
model4e_nb <- glm.nb(arafsamp$numaraf ~ arafsamp$urban_rural)
summary(model4e_nb) #AIC 221.73
## ARAf samples: numARAf ~ arable buffer
model4f_nb <- glm.nb(arafsamp$numaraf ~ arafsamp$percentarable)
summary(model4f_nb) #AIC 220.88
## ARAf samples: numARAf ~ nearest composter
model4g_nb <- glm.nb(arafsamp$numaraf ~ arafsamp$shortestdistance)
summary(model4g_nb) #AIC 221.73
## ARAf samples: numARAf ~ composter landcover
model4h_nb <- glm.nb(arafsamp$numaraf ~ arafsamp$composterlandcover)
summary(model4h_nb) #AIC 221.32
## ARAf samples: numARAf ~ urban or rural composter
model4i_nb <- glm.nb(arafsamp$numaraf ~ arafsamp$urban_rural_composter)
summary(model4i_nb) #AIC 221.64

anova(null4_nb, model4i_nb) # none are a significant improvement on null


#-----------

## Model8h_nb is the best.
table(arafengsamp$composterlandcover)
arafengsamp$composterlandcover <- factor(arafengsamp$composterlandcover, levels=c("1","3","4","5","20","21"),
                                         labels=c("1/n(n=4)","3/n(n=7)","4/n(n=18)","5/n(n=3)","20/n(n=11)","21/n(n=17)"))

ggplot(arafengsamp, aes(x=composterlandcover, y=numaraf)) + 
  geom_boxplot(varwidth = T, color="darkblue", fill="lightblue") +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  labs(x="Land cover classification of nearest composter", y="Number of AR*Af* colonies grown from sample") +
  theme(axis.title.y = ggtext::element_markdown())


#---------------------------------------------------------------------------------------

### GWR models

install.packages("spgwr")
install.packages("maptools")
library(spgwr)
library(ggplot2)
library(maptools)

## My continuous data is count data that is not Normally-distributed so this website suggests poisson regression is best: https://bookdown.org/ndphillips/YaRrr/regression-on-non-normal-data-with-glm.html

### Re-run model2s with only +ve samples
possamp <- locs[locs$numafum > 0,] #918
## Afum samples: numAfum ~ season + land cover
model3c <- glm(possamp$numafum ~ possamp$round + possamp$landcover, family="poisson")
summary(model3c) #AIC 4352.8
plot(model3c)
anova(model3c, test="Chisq")

resids<-residuals(model3c)
colours <- c("dark blue", "blue", "red", "dark red") 
#here it is assumed that your eastings and northings coordinates are stored in columns called x and y in your dataframe
map.resids <- SpatialPointsDataFrame(data=data.frame(resids), coords=cbind(possamp$lat,possamp$long)) 
#for speed we are just going to use the quick sp plot function, but you could alternatively store your residuals back in your LondonWards dataframe and plot using geom_point in ggplot2
spplot(map.resids, cuts=quantile(resids), col.regions=colours, cex=1) 

#calculate kernel bandwidth
GWRbandwidth <- gwr.sel(possamp$numafum ~ possamp$round + possamp$landcover, coords=cbind(possamp$lat,possamp$long),adapt=T)
#run the gwr model
gwr.model = gwr(possamp$numafum ~ possamp$round + possamp$landcover, coords=cbind(possamp$lat,possamp$long), adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE) 
#print the results of the model
gwr.model #AIC 4772.556

#---------------------------------------------------------------------------------------

### GWR models specifying Binomial or Poisson distribution

install.packages("GWmodel")
library(GWmodel)

#create SpatialPointsDataFrame for ggwr.basic
spdf <- SpatialPointsDataFrame(data=uklocs, coords=cbind(uklocs$lat,uklocs$long))
#calculate bandwidth
bw <- bw.ggwr(afum01 ~ round + landcover, data=spdf, family="binomial", approach="CV")
gwrmodel2 <- ggwr.basic(numafum ~ round + landcover, data=spdf, family="poisson")


#---------------------------------------------------------------------------------------

