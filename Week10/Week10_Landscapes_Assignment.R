# Load the packages from this week's tutorial, aka vignette
  # https://github.com/Team-FRI/dbfishR
#We looked at brook trout population demographics in relationship to water quality and stream flashiness.
pkgs <- installed.packages()
if (!('devtools' %in% pkgs)) { install.packages('devtools') }
if ('dbfishR' %in% pkgs) { remove.packages('dbfishR') }

devtools::install_github(repo = 'Team-FRI/dbfishR', upgrade = 'never')

library(dbfishR)

sites <- get_sites()
events <- get_events()
events_meta <- merge(sites, events[,c("SiteCode","EventCode","WaterTemp","pH","SpecCond","Alk","DO")])
events_meta$year <-substring(as.character(events_meta$EventCode),1,4)

fish_rec <- get_fish_records()

brown_count <- aggregate(ID~EventCode, data = subset(fish_rec, Species == "Brown Trout" & Pass == "Pass 1"), FUN = length)
colnames(brown_count)[2] <- "TotalCount"
small_brown_count <- aggregate(ID~EventCode, data = subset(fish_rec, Length_mm < 100 & Species == "Brown Trout" & Pass == "Pass 1"), FUN = length)
colnames(small_brown_count)[2] <- "SmallCount"
big_brown_count <- aggregate(ID~EventCode, data = subset(fish_rec, Length_mm > 99 & Species == "Brown Trout" & Pass == "Pass 1"), FUN = length)
colnames(big_brown_count)[2] <- "BigCount"


df_list <- list(brown_count,small_brown_count, big_brown_count)
all_browns <- Reduce(function(x, y) merge(x,y, all= TRUE), df_list)

all_browns$SmallCount[is.na(all_browns$SmallCount)] <- 0 #this allows the replace NA below to only take care of 100% YOY NAs
all_browns$YOYRatio <- all_browns$SmallCount/(all_browns$BigCount+all_browns$SmallCount)
all_browns$YOYRatio[is.na(all_browns$YOYRatio)] <- 1 #NAs are 100% YOY.

browns_events <- merge(all_browns, events_meta)

library(dataRetrieval)

HUC6 <- "020501"#North Branch Susquehanna
HUC_list <-paste(rep(HUC6,10), seq(0, 9, length.out = 10), sep="0")#To do a full HUC6 at once, just pick your HUC6 and auto-populate the subwatersheds (only works up to 9 HUC8 in a HUC6)

gage_df <- readNWISdata(huc = HUC_list, parameterCd = "00060", startDate = "2010-01-01", endDate = "2020-12-31")

devtools::install_github(repo = 'leppott/ContDataQC', force = TRUE)
library(ContDataQC)

gage_df$year <- sapply(strsplit(as.character(gage_df$dateTime), "-"),"[[",1)#Create year to get annual R-B index

R_B_HUC <- aggregate(X_00060_00003~year+site_no, data = gage_df, FUN = RBIcalc)#Aggregate by year and site w/in the HUC
colnames(R_B_HUC)[3] <- "RBI" #rename column

stations_meta <- readNWISsite(unique(R_B_HUC$site_no))

medium_stations <- subset(stations_meta, drain_area_va > 10 & drain_area_va < 100)

library(sf)
medium_stations_so <- st_as_sf(medium_stations,coords = c("dec_lat_va", "dec_long_va"))

events_so <- st_as_sf(browns_events[!is.na(browns_events$SiteLon),], coords = c("SiteLat","SiteLon"))#remove NAs to create spatial object

fish_flow_tmp <- st_join(events_so, medium_stations_so, join = st_nearest_feature)

install.packages("nngeo")
library(nngeo)
#distances are in degrees
fish_flow_tmp$dist <- unlist(st_nn(events_so, medium_stations_so, returnDist = T)$dist)
fish_flow_tmp <- subset(fish_flow_tmp, dist < 0.5)

#because of year and spatial join needed to change order of operations. Space first, then time
fish_flow <- merge(fish_flow_tmp, R_B_HUC, by = c("year", "site_no"))

mod <- lm(TotalCount~RBI, data = fish_flow)
summary(mod)

mod2 <- lm(BigCount~RBI, data = fish_flow)
summary(mod2)

mod3 <- lm(SmallCount~RBI, data = fish_flow)
summary(mod3)

#getting there but he r squared could be better#

mod4 <- lm(YOYRatio~RBI, data = fish_flow)
summary(mod4) #clock it#

install.packages("itsadug")
library(itsadug)
plot(mod4$residuals)#residuals from the YOYRatio lm() above

gam.mod <- gam(YOYRatio~RBI, data = fish_flow, na.action = na.omit, method = "REML")#RBI only
summary(gam.mod)
AIC(gam.mod) #65.47

plot_smooth(gam.mod, view="RBI", rm.ranef=FALSE)

gam.mod <- gam(YOYRatio~RBI+Alk, data = fish_flow, na.action = na.omit, method = "REML")#RBI + alkalinity
summary(gam.mod)
AIC(gam.mod) #66.26

par(mfrow=c(1,2)) 
plot_smooth(gam.mod, view="RBI", rm.ranef=FALSE)
plot_smooth(gam.mod, view="Alk", rm.ranef=FALSE, ylab = "", xlab = "Specific Conductivity")

#not good anymore
gam.mod <- gam(YOYRatio~RBI+Alk+SpecCond, data = fish_flow, na.action = na.omit, method = "REML")#RBI, alk, and specific conductivity
summary(gam.mod)
AIC(gam.mod) #67.44

par(mfrow=c(1,3)) 
plot_smooth(gam.mod, view="RBI", rm.ranef=FALSE)
plot_smooth(gam.mod, view="Alk", rm.ranef=FALSE, ylab = "", xlab = "Alkalinity")
plot_smooth(gam.mod, view="SpecCond", rm.ranef=FALSE, ylab = "", xlab = "Specific Conductivity")


#1: Give two specific conclusions you can make from these patterns. (4 pts)

#The count of big/small fish is not necessarily the most important "demographic" but the YOY ratio - meaning 
#there may be some more patterns associated with how young a fish population is. From the patterns of YOY and RBI, ALK, and COND,
#we see that there is a strong relationship between YOY and RBO + Alk, but not YOY sand RBO+Alk+Spec Cond, meaning 
#that alkalinty and flashiness are more closely related.#


#2: Rerun this analysis with either (a) a different metric of brook trout populations or a different species from the database. (6 pts)


#code at top#



#3: How do the results of your analysis compare to the vignette? (5 pts)

#The brown trout and brook trout summaries were similar in terms of pattern, like the AICs of the 
#models and the plots. However, the AICs for the brown trout were actually lower than the brookies.#
#AIC isn't comparable because the y is different between the models.


#4: For your final project you'll need to find two separate data sources to combine similar to the process here.
  #In prep for that, find one data source to compare with either the data in dbfishR OR DataRetrieval. (5 pts)
  #Read data from that source into your script. (5 pts)
  #Create any analysis of your choice that combines the two data sources, this can be as simple as a linear model. (5 pts)


#read in and organize data#
library(readxl)
setwd("C:/GitHub/hornam/Week10")
survey_data <- read_excel("FSY19_YukonCharley_AKSSFdataexportAFFI.xlsx", sheet="Survey")
survey_data$Date <- as.Date(survey_data$surveyDate)
class(survey_data$Date) 
str(survey_data$Date)
site_data   <- read_excel("FSY19_YukonCharley_AKSSFdataexportAFFI.xlsx", sheet="Site")
fish_indiv   <- read_excel("FSY19_YukonCharley_AKSSFdataexportAFFI.xlsx", sheet = "Fish Indivual")
fish_obs    <- read_excel("FSY19_YukonCharley_AKSSFdataexportAFFI.xlsx", sheet="Fish Observation")




#get HUCs to USGS sites#

hucs <- as.list(site_data$HUCCode)
sites <- read_waterdata_monitoring_location(hydrologic_unit_code=19070506)
sites$hydrologic_unit_code
#"190705062003" "190705062100"

#used these hucs on the page - huc search to find monitoring location with new data ret is a pain#

#get mean discharge + temp#


cql <-  '{
"op": "and",
"args": [
  {
    "op": "in",
    "args": [
      { "property": "parameter_code" },
      [ "00060", "00010" ]
    ]
  },
 {
    "op": "in",
    "args": [
      { "property": "monitoring_location_id" },
      [ "USGS-15356000"]
    ]
  }
]
}'




library(dataRetrieval)
?read_waterdata
gage_df <- read_waterdata(service="daily",
                          CQL=cql,
                          time = c("2019-08-01", "2019-08-31"))

#orgnaize for merge#

colnames(gage_df)[5] <- 'Date'
?merge
fish_combined <- merge(fish_indiv, fish_obs, by = "fishObservationID")
fish_with_env <- merge(fish_combined, survey_data, by = "surveyID")
final_df <- merge(fish_with_env, site_data, by = "siteID")
final_df$Date <- as.Date(final_df$surveyDate)
final_df <-final_df[,-117]

library(sf)
final_df <- st_drop_geometry(final_df) #used gemini here because i could not understand the error#
gage_df <- st_drop_geometry(gage_df)

colnames(final_df)[40] <- 'Date'
colnames(gage_df)[5] <- 'Date'
df.usgs <- merge(final_df, gage_df, by.x  ="BetterDate",by.y="Date", all.x=TRUE)
final_df$Date
final_df$BetterDate <- substring(as.character(final_df$Date),1,10)
final_df$BetterDate
gage_df$Date


#linear model#

fish_model <- lm(length ~ tempWater , data = df.usgs)
summary(fish_model)

#I can't get the model to run with "value". But worked with tempWater!

plot(length ~ tempWater , data = df.usgs)
abline(fish_model)
#I had to look at it.







