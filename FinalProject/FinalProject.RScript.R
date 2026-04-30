##FINAL PROJECT##
##Goal: Integrate density of populations/universities with species observations from GBIF datasheets##
## Hope is to understand how population/university density may influence the frequency of observations and 
## if concentration of population is influencing observational distribution, frequency, or quality##
##If not pop pr university density, then what?##

#Load datasets and packages#

setwd("C:/GitHub/hornam/FinalProject")
install.packages("ggspatial")
install.packages("osmdata")
install.packages("sampbias")
install.packages("elevatr")
install.packages("rayshader")
install.packages("tigris")
install.packages("prettymapr")
library(rgbif)
library(sampbias)
library(osmdata)
library(sf)
library(spocc)
library(ggplot2)
library(geodata)
library(dplyr)
library(tigris)
library(viridis)
library(prettymapr)


#GBIF login#
user = "annahorn11" # your gbif.org username
pwd = "AMHbrandi1405@" # your gbif.org password
email = "horna@susqu.edu" # your emaiuser = "annahorn11" # your gbif.org username

#load species occurrence data#


#I chose 30,000 because while likely there is many more, I think this will be an adequate number for the GLM to do its thing#


#LOOP METHOD#
species_list <- c("Buteo jamaicensis", "Canis latrans", "Phragmites australis")
all_data <- list() # Store results in a list

target_total <- 30000
batch_size <- 1000  # GBIF's maximum records per single internal request

# 2. Create a container for the data
all_species_list <- list()

for (sp in species_list) {
  message("--- Starting download for: ", sp, " ---")
  
  species_data <- list()
  
  # The nested loop handles the "paging" to get past the 1,000 limit
  for (start_row in seq(0, target_total - 1, by = batch_size)) {
    
    message("Fetching records ", start_row, " to ", start_row + batch_size)
    
    # query the data
    obs <- occ_data(
      scientificName = sp,
      
      limit = batch_size,
      start = start_row,
      hasCoordinate = TRUE,
      occurrenceStatus = "PRESENT"
    )
    
    # Check if data was actually returned
    if (!is.null(obs$data) && nrow(obs$data) > 0) {
      # Select only the columns you need to save memory
      species_data[[length(species_data) + 1]] <- obs$data %>%
        select(any_of(c("scientificName", "decimalLatitude", "decimalLongitude", "basisOfRecord")))
    } else {
      message("No more data found for this species.")
      break
    }
    
    # IMPORTANT: Pause to prevent the server from timing you out
    Sys.sleep(1.2)
  }
  
  # Combine the batches for THIS species into one data frame
  all_species_list[[sp]] <- bind_rows(species_data)
}

list2env(all_species_list, envir = .GlobalEnv)
?saveRDS
saveRDS(all_species_list, file="all_species_list.rds")
restored_data <- readRDS("all_species_list.rds")

#Make it a data frame, filter out occurences without lat/long#
all_species_df <- bind_rows(restored_data) %>%
  filter(!is.na(decimalLatitude), !is.na(decimalLongitude))

#Convert to spatial object#
all_species_sf <- st_as_sf(all_species_df, 
                           coords = c("decimalLongitude", "decimalLatitude"), 
                           crs = 4326)

#UNIVERSITY DENSITY#
postsec.data <- read.csv("C:/GitHub/hornam/FinalProject/data/Postsecondary_School_Locations_2022-23.csv")#no "data" folder
postsec.data$ZIP <- sub("-.*", "", postsec.data$ZIP)
postsec.data

#aggregate uni data so there's only one row per zip#
postsec.data$ZIP5 <- substr(postsec.data$ZIP, 1, 5)
postsec.data$ZIP5 <- sprintf("%05s", as.character(postsec.data$ZIP5))
uni_counts <- aggregate(NAME ~ ZIP, data = postsec.data, FUN = length)
colnames(uni_counts) <- c("ZIP5", "uni_count")

head(uni_counts)


##POP DENSITY OPTION 2##
#went with this option, had more zip codes#
census.data <- read.csv("C:/GitHub/hornam/FinalProject/CensusData.csv")#also missing
census.data.1 <- census.data[-1,-4]
census.data.1$ZCTA5 <- gsub(".*US", "", census.data.1$GEO_ID)
colnames(census.data.1)[3] <- "pop_total"
census.data.1$pop_total <- as.numeric(as.character(census.data.1$pop_total))


##Geometry##

zip.usa <- zctas(cb = TRUE, year = 2020)
zip.usa <- zip.usa[, c("ZCTA5CE20", "ALAND20", "geometry")]
zip.usa$sq_miles_calc <- as.numeric(st_area(zip.usa) / 2589988)

#convert species df sf to be in same coord#
all_species_sf.new <- st_transform(all_species_sf, st_crs(zip.usa))
combined.df.1 <- st_join(all_species_sf.new, zip.usa)

#density calculation#
zcta.density <- merge(zip.usa, census.data.1, by.x = "ZCTA5CE20", by.y = "ZCTA5")
zcta.density$pop_total <- as.numeric(zcta.density$pop_total)
class(zcta.density$pop_total)
zcta.density$pop_density <- zcta.density$pop_total / zcta.density$sq_miles


#MERGE#

species.density.df <- st_join(combined.df.1, zcta.density, left=TRUE)
species.density.df.1 <- na.omit(species.density.df)
write.csv(species.density.df.1, file ="SpeciesDensity.csv")


#Analysis#

#create a df with the number of sightings per zipcode and test distros#
sighting_counts <- aggregate(scientificName ~ ZCTA5CE20.x, 
                             data = species.density.df.1, 
                             FUN = length)


university.density.df <- merge(sighting_counts, uni_counts,  by.x="ZCTA5CE20.x",  by.y = "ZIP5")

#ALL#
library(fitdistrplus)
obs_counts <- as.numeric(sighting_counts$scientificName)
fit_p <- fitdist(obs_counts, "pois")
fit_nb<- fitdist(obs_counts, "nbinom")
fit_g <- fitdist(obs_counts, "gamma")

gofstat(list(fit_p, fit_nb, fit_g), fitnames = c("Poisson", "Neg-Binom", "Gamma"))

#nbinom + gam it is#



#REDHAWK#
hawk.df <- species.density.df.1[-25187:-52469,-7]
hawk.dens.df <- aggregate(scientificName ~ ZCTA5CE20.x, data = hawk.df, FUN=length)
hawk.uni.df <- merge(hawk.dens.df, uni_counts, by.x="ZCTA5CE20.x", by.y="ZIP5")
hawk.combined.df <- merge(hawk.uni.df, zcta.density, by.x = "ZCTA5CE20.x", by.y="ZCTA5CE20")

hawk.dens.model <- glm.nb(scientificName ~ pop_density, data=hawk.combined.df)
summary(hawk.dens.model)
#p very tiny, AIC=9489.2)

hawk.uni.model <- glm.nb(scientificName ~ uni_count, data=hawk.combined.df)
summary(hawk.uni.model)
#p no sig, AIC=9502)

hawk.int.model <- glm.nb(scientificName ~ pop_density*uni_count, data=hawk.combined.df)
summary(hawk.int.model)
#only pop density is sig, AIC = 9489.2#

#TRY GAMMA#

hawk.dens.model.g <- glm(scientificName ~ pop_density, family=Gamma(link="log"), data=hawk.combined.df)
summary(hawk.dens.model.g)
#p = 0.049, AIC=9141.7#

hawk.uni.model.g <- glm(scientificName ~ uni_count, family=Gamma(link="log"), data=hawk.combined.df)
summary(hawk.uni.model.g)
#p=0.567#
#AIC=9156.3#

hawk.int.model.g <- glm(scientificName ~ pop_density*uni_count, family=Gamma(link="log"), data=hawk.combined.df)
summary(hawk.int.model.g)

#Nothing is significant#
#AIC = 9141.2#


#GAMMA was the best fit#


plot(scientificName ~ pop_density, data=hawk.combined.df, xlab="Population Density per ZCTA5 (m^2)", ylab="Number of hawk occurences", pch=19)
plot(scientificName ~ uni_count, data=hawk.combined.df, xlab= "Number of university per ZCTA5", ylab = "Number of hawk occurences", pch=19)
abline(lm(scientificName ~ pop_density, data = hawk.combined.df), col = "blue")

#COYOTE#
coyote.df <- species.density.df.1[-1:-25186,]
coyote.df <- species.density.df.1 [-49153:-52469,]
coy.dens.df <- aggregate(scientificName ~ ZCTA5CE20.x, data = coyote.df, FUN=length)
coy.uni.df <- merge(coy.dens.df, uni_counts, by.x="ZCTA5CE20.x", by.y="ZIP5")
coy.combined.df <- merge(coy.uni.df, zcta.density, by.x = "ZCTA5CE20.x", by.y="ZCTA5CE20")

coy.dens.model <- glm.nb(scientificName ~ pop_density, data=coy.combined.df)
summary(coy.dens.model)
#p sig, AIC=13012)


coy.uni.model <- glm.nb(scientificName ~ uni_count, data=coy.combined.df)
summary(coy.uni.model)
#p no sig but approaching, AIC=13013)

coy.int.model <- glm.nb(scientificName ~ pop_density*uni_count, data=coy.combined.df)
summary(coy.int.model)
#only pop density is sig, AIC = 13010#

#GAMMA#

coy.dens.model.g <- glm(scientificName ~ pop_density, family=Gamma(link="log"), data=coy.combined.df)
summary(coy.dens.model.g)
#AIC=12682# 
#p = 0.342#

coy.uni.model.g <- glm(scientificName ~ uni_count, family=Gamma(link="log"), data=coy.combined.df)
summary(coy.uni.model.g)
#AIC=12683#
#p=0.43#


coy.int.model.g <- glm(scientificName ~ pop_density*uni_count, family=Gamma(link="log"), data=coy.combined.df)
summary(coy.int.model.g)
#no sig, AIC=12680#

plot(scientificName ~ pop_density, data=coy.combined.df, xlab="Population Density per ZCTA5 (m^2)", ylab="Number of coyote occurences")
plot(scientificName ~ uni_count, data=coy.combined.df, xlab= "Number of university per ZCTA5", ylab = "Number of coyote occurences", pch=19)




#COMMON REED#
reed.df <- species.density.df.1 [-1:-49152,]
reed.dens.df <- aggregate(scientificName ~ ZCTA5CE20.x, data = reed.df, FUN=length)
reed.uni.df <- merge(reed.dens.df, uni_counts, by.x="ZCTA5CE20.x", by.y="ZIP5")
reed.combined.df <- merge(reed.uni.df, zcta.density, by.x = "ZCTA5CE20.x", by.y="ZCTA5CE20")

reed.dens.model <- glm.nb(scientificName ~ pop_density, data=reed.combined.df)
summary(reed.dens.model)
#p not sig, AIC = 1396.3#

reed.uni.model <- glm.nb(scientificName ~ uni_count, data=reed.combined.df)
summary(reed.uni.model)
#p no sig , AIC=1395.4#

reed.int.model <- glm.nb(scientificName ~ pop_density*uni_count, data=reed.combined.df)
summary(reed.int.model)
#no#

#Gamma#

reed.dens.model.g <- glm(scientificName ~ pop_density, family=Gamma(link="log"), data=reed.combined.df)
summary(reed.dens.model.g)
#p=0.912, AIC=1257.7#

reed.uni.model.g <- glm(scientificName ~ uni_count, family=Gamma(link="log"),data=reed.combined.df)
summary(reed.uni.model.g)
#p=0.497, AIC=1256.3#

reed.int.model.g <- glm(scientificName ~ pop_density*uni_count, family=Gamma(link="log"),data=reed.combined.df)
summary(reed.int.model.g)
#p=no sig, AIC=1259.6#



#Compare with a more known to be urban species#

#Settings#
species_name <- "Columba livia"
country_code <- "US"
target_total  <- 30000
batch_size    <- 1000
start_val    <- 0

# Store each pull# 
data_chunks <- list()

# --- The Loop --- # Got this code using gemini because my previous loop was not working#
while (start_val < target_total) {
  
  cat("Requesting records starting at:", start_val, "\n")
  
  #Query#
  obs <- occ_data(
    scientificName = species_name,
    limit = batch_size,
    country = country_code,
    start = start_val,
    hasCoordinate = TRUE
  )
  
  # Safety check: if no data is returned, break the loop
  if (is.null(obs$data) || nrow(obs$data) == 0) {
    message("No more data available.")
    break
  }
  
  # I only want a few columns to save space#
  cols_to_keep <- c("decimalLatitude", "decimalLongitude", "basisOfRecord")
  existing_cols <- intersect(cols_to_keep, names(obs$data))
  
  # Add the data frame#
  data_chunks[[length(data_chunks) + 1]] <- obs$data[, existing_cols]
  
  # Increment the starting position for the next iteration#
  start_val <- start_val + batch_size
  
  # Optional: pause for 0.2 seconds to stay under API rate limits
  Sys.sleep(0.2)
}

#put into big df#
columba_data <- do.call(rbind, data_chunks)
cat("Final record count:", nrow(columba_data), "\n")
head(columba_data)

#save#
saveRDS(columba_data, file="columba_data.rds")
restored_data <- readRDS("columba_data.rds")


#Convert to spatial object#
pigeon_sf <- st_as_sf(columba_data, 
                           coords = c("decimalLongitude", "decimalLatitude"), 
                           crs = 4326)

pigeon_sf.new <- st_transform(pigeon_sf, st_crs(zcta.density))
pigeon_zcta.df <- st_join(pigeon_sf.new, zcta.density)
pigeon_zcta.df <- na.omit(pigeon_zcta.df)
pigeon_zcta.df$uid <- 1:nrow(pigeon_zcta.df)

pigeon.dens.df <- aggregate(uid ~ ZCTA5CE20, data = pigeon_zcta.df, FUN=length)
pigeon.uni.df <- merge(pigeon.dens.df, uni_counts, by.x="ZCTA5CE20", by.y="ZIP5")
pigeon.combined.df <- merge(pigeon.uni.df, zcta.density, by.x = "ZCTA5CE20", by.y="ZCTA5CE20")


#nb models#

pigeon.dens.model <- glm.nb(uid ~ pop_density, data=pigeon.combined.df)
summary(pigeon.dens.model)
#p sig, AIC = 11609#

pigeon.uni.model <- glm.nb(uid ~ uni_count, data=pigeon.combined.df)
summary(pigeon.uni.model)
#p=7.94e-08, AIC: 11799#

pigeon.int.model <- glm.nb(uid~ pop_density*uni_count, data=pigeon.combined.df)
summary(pigeon.int.model)
#all are sig, AIC=11529#


#Gamma#

pigeon.dens.model.g <- glm(uid ~ pop_density, family=Gamma(link="log"), data=pigeon.combined.df)
summary(pigeon.dens.model.g)
#p=2e-16, AIC = 11256#

pigeon.uni.model.g <- glm(uid ~ uni_count, family=Gamma(link="log"),data=pigeon.combined.df)
summary(pigeon.uni.model.g)
#p=8.69e-09, AIC=11466#

pigeon.int.model.g <- glm(uid~ pop_density*uni_count, family=Gamma(link="log"),data=pigeon.combined.df)
summary(pigeon.int.model.g)

#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)            1.173e+00  8.433e-02  13.911  < 2e-16 ***
  #pop_density            4.103e-05  5.600e-06   7.326 3.40e-13 ***
  #uni_count              2.096e-01  4.545e-02   4.613 4.22e-06 ***
  #pop_density:uni_count -3.849e-06  1.942e-06  -1.982   0.0476 * 

plot(uid ~ pop_density, data=pigeon.combined.df, xlab="Population Density per ZCTA5 (m^2)", ylab="Number of pigeon occurences")
plot(uid ~ uni_count, data=pigeon.combined.df, xlab= "Number of university per ZCTA5", ylab = "Number of pigeon occurences", pch=19)

par(mfrow = c(1, 2))
plot(uid ~ uni_count, data=pigeon.combined.df, xlab= "Number of university per ZCTA5", ylab = "Number of pigeon occurences", pch=19)
plot(scientificName ~ uni_count, data=hawk.combined.df, xlab= "Number of university per ZCTA5", ylab = "Number of hawk occurences", pch=19)

#Realized I neded to combine to run an all analysis#
colnames(reed.combined.df)[2] <- "reed_occur"
colnames(coy.combined.df)[2] <- "coy_occur"
colnames(hawk.combined.df)[2]<- "hawk_occur"
colnames(pigeon.combined.df)[2] <- "pig_occur"
colnames(pigeon.combined.df)[1] <- "ZCTA5CE20.x"

reed.coy.df <-  merge(reed.combined.df, coy.combined.df, 
                           by = c("ZCTA5CE20.x", "uni_count", "pop_density"), 
                           all = TRUE)
reed.coy.hawk.df <- merge(reed.coy.df, hawk.combined.df, 
                          by = c("ZCTA5CE20.x", "uni_count", "pop_density"), 
                          all = TRUE)

four.species.df <- merge(reed.coy.hawk.df, pigeon.combined.df,
                         by = c("ZCTA5CE20.x", "uni_count", "pop_density"), 
                         all = TRUE)

four.species.df <- four.species.df[,-26:-31]
four.species.df <- four.species.df[,-19:-24]
four.species.df <- four.species.df[,-17:-12]
four.species.df <- four.species.df[,c(-8,-10)]

#ALL Species Dens#
library(MASS)
#making NAs 0 and making a total occurences column#
four.species.df[is.na(four.species.df)] <- 0
four.species.df$totaloccur <- four.species.df$reed_occur+four.species.df$coy_occur+four.species.df$hawk_occur+four.species.df$pig_occur


all.data.model <- glm.nb(totaloccur ~ pop_density, four.species.df)
summary(all.data.model)
#p < 2e-16#
#AIC = 20690#

all.data.gam.model <- glm(totaloccur ~ pop_density, family=Gamma(log="link"), data=four.species.df)
summary(all.data.gam.model)

#p=2e-16#
#AIC=27486#


#ALL UNI to Species#
univ.all.model <- glm.nb(totaloccur ~ uni_count, data=four.species.df)
summary(univ.all.model)

#p=0.9.81e-07#
#AIC=20803#


univ.gam.model <- glm(totaloccur ~ uni_count, family = Gamma(link="log"), four.species.df)
summary(univ.gam.model)

#p=0.0069#
#AIC = 20565# 


int.model.all <- glm.nb(totaloccur~pop_density*uni_count, data=four.species.df)
summary(int.model.all)

#Coefficients:
 # Estimate Std. Error z value Pr(>|z|)    
#(Intercept)            2.250e+00  4.498e-02  50.019  < 2e-16 ***
 # pop_density            2.705e-05  3.382e-06   7.997 1.28e-15 ***
 # uni_count              7.848e-02  2.489e-02   3.153  0.00161 ** 
 # pop_density:uni_count -1.993e-06  1.173e-06  -1.698  0.08947 .  
#AIC - 20682#

int.model.gamma <- glm(totaloccur~pop_density*uni_count, family=Gamma(link="log"), data=four.species.df)
summary(int.model.all)

#same results#




#side by side scatter plots#


par(mfrow = c(2, 2))
plot(coy_occur ~ pop_density, data=four.species.df, xlab= "Population Density per Zipcode (m^2)", ylab = "Coyote Occurrences", pch=19 )
plot(reed_occur ~ pop_density, data=four.species.df, xlab= "Population Density per Zipcode(m^2)", ylab = "Common Reed Occurences",   pch=19  )
plot(hawk_occur ~ pop_density, data=four.species.df, xlab= "Population Density per Zipcode(m^2)", ylab = "Red Hawk Occurences",  pch=19 )
plot(pig_occur ~ pop_density, data=four.species.df, xlab= "Population Density per Zipcode(m^2)", ylab = "Rock Pigeon Occurences",  pch=19  )



#Rpubs tutorial on how to do side by side stacked bar#
#Make data long format#

four.species.df.subset <- four.species.df[, c("uni_count", "pig_occur", "reed_occur", "hawk_occur", "coy_occur")]
four.species.df.subset[is.na(four.species.df.subset)] <- 0





four.species.long <- reshape(four.species.df.subset, 
                   varying = c("pig_occur", "reed_occur", "hawk_occur", "coy_occur"), 
                   v.names = "occurrence_count",
                   timevar = "species",
                   times = c("Pigeon", "Reed", "Hawk", "Coyote"),
                   direction = "long")

#ggplot#

ggplot(four.species.long, aes(x = as.factor(uni_count), y = occurrence_count, fill = species)) +
  geom_bar(stat = "identity") +  
  theme_minimal() +
  labs(title = "Species Occurrences by University Presence",
       x = "Number of Universities per ZIP Code",
       y =  "Number of Occurences",
       fill = "Species") +
  scale_fill_brewer(palette = "Set2")



#maps#

ma_zctas <- zctas(cb = TRUE, starts_with = c("01", "02"), year = 2020) #picking zips for massachusetss
ma_border <- states(cb = TRUE) %>% filter(NAME == "Massachusetts")
ma_only_pigeon_data <- st_intersection(pigeon_zcta.df, ma_border)
ma_only_others_data <- st_intersection(combined.df.1, ma_border)



ggplot() +
  # Add the basemap - "osm" or "cartolight" work well
  annotation_map_tile(type = "osm", zoom = 6) + 
  geom_sf(data = ma_zctas, fill = "purple", alpha = 0.3, color = "purple") +
  # Ensure the coordinate system matches the tiles
  coord_sf(crs = st_crs(3857)) +
  theme_minimal() +
  labs(
    title = "Massachusetts",
    caption = "Basemap: OpenStreetMap"
  ) +
geom_sf(data = ma_only_pigeon_data , color = "red", size = 0.4)+
  geom_sf(data=ma_only_others_data, color="blue", size=0.4)


#Looks cool, want it interactive though because it will give me more control over how I capture it#

library(leaflet)


#convert everything to same projection#
ma_zctas_gl <- st_transform(ma_zctas, 4326)
pigeon_gl <- st_transform(ma_only_pigeon_data, 4326)
others_gl <- st_transform(ma_only_others_data, 4326)

leaflet() %>%
  addTiles() %>%  #just doing default back#
  
  addPolygons(data = ma_zctas_gl, 
              fillColor = "purple", fillOpacity = 0.1, 
              color = "purple", weight = 1,
              group = "ZCTAs") %>%
  
  addCircleMarkers(data = pigeon_gl, 
                   color = "red", radius = 2, stroke = FALSE, fillOpacity = 0.7,
                   popup = ~paste("Type: Pigeon"),
                   group = "Pigeons") %>%
  
  addCircleMarkers(data = others_gl, 
                   color = "blue", radius = 2, stroke = FALSE, fillOpacity = 0.7,
                   popup = ~paste("Type: Other"),
                   group = "Others")
  
  
 
#total number of occurences# 
#I got curious about this while writing my report#

reed.total <- sum(four.species.df$reed_occur)
#799#
hawk.total <- sum(four.species.df$hawk_occur)
#8350#
pigeon.total <- sum(four.species.df$pig_occur)
#12889#
coyote.total <- sum(four.species.df$coy_occur)
#14543#
