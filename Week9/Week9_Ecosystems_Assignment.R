# For this week it's time to start exploring your own ideas and questions in R.
  # There are at least five options in the dataset to create the following comparisons.

library(readxl)
setwd("C:/GitHub/hornam/Week9")

#PULL AND ORGANIZE DATA#
#I want to look at abiotic and veg transects#
abiotic.tibble <- read_excel("Penaetal_2016_data.xlsx", sheet = "Abiotic factors")
abiotic <- as.data.frame(abiotic.tibble)
abiotic


veg.transects <- read_excel("Penaetal_2016_data.xlsx", sheet = "Vegetation_transects")
veg.transects <- as.data.frame(veg.transects)
veg.transects

veg.transects <- sapply(veg.transects, as.numeric )
veg.transects1 <- veg.transects[,-1:-2] #remove NAs#
veg.transects1

#only 1 sample per parcel ooposed to 5 per parcel for abiotic#
#org by parcel#

?aggregate
abiotic.means <- aggregate(x=abiotic, by=list(abiotic$Parcel), FUN = "mean")
abiotic.means

#generated NAs but 19 columns indicated it worked#
#get rid of NAs/clean up for analysis#

abiotic.means1 <- abiotic.means[,-16] # NA column
abiotic.means2 <- abiotic.means1[,-1:-6] # Plot and NA columns
abiotic.means2 <- sapply(abiotic.means2, as.numeric ) # Make sure everything is numeric.
abiotic.means2 <- as.data.frame(abiotic.means2) # Make sure it's in the right format.
abiotic.means2

#okay for some reason the authors of this paper HATE me and want me to die because they left out A4. What did 
#A4 ever do to you? Now I have to take out A4 of my transects.

veg.transects2 <- veg.transects1[-15,]
veg.transects2

# (Q1 - 12 pts) Use the dataset from the tutorial to complete one redundancy analysis (RDA) with variance partitioning on a different community (NOT the nematodes).
  # Explain the ecological importance of your significant predictor variables, or the importance if none are significant for your community.

library(vegan)

colnames(abiotic.means2)
#Thoughts before analysis: All the nutrients here relate HEAVILY to plant growth and root health, Al is 
#toxic to plants, and pH plays a large role in how plants can absorb nutrients, etc., so really they are all important#

ord <- rda(veg.transects2 ~ pH + totalN + Perc_ash + Kalium + Magnesium + Ca + Al + TotalP + OlsenP, abiotic.means2)
ord
anova(ord)
plot(ord, ylim = c(-2,2), xlim = c(-5,5)) 

#65 % is explained, model value is sig#
#Species driving relationship are European ash (Fraxinus excelsoir), ground elder (Aegopodium podagraria),
#and lamium galeobdolon).#

#run ordistep() to review which are sig#

ord <- rda(veg.transects2 ~., abiotic.means2) # shorthand for the model that includes everything.
ord.int <- rda(veg.transects2 ~1, abiotic.means2) # shorthand for the model that only includes intercepts.

step.mod <- ordistep(ord.int, scope = formula(ord), selection = "both")
step.mod$anova

step.R2mod <- ordiR2step(ord.int, scope = formula(ord), selection = "forward")

#From this, Total P is only significant, BUT all variables has the higher R^2 
#than Total P.#

ordP <- rda(veg.transects2 ~ TotalP + OlsenP + Kalium + pH, abiotic.means2)
ordP
anova(ordP)
plot(ordP)

ordPh <- rda(veg.transects2 ~ TotalP * pH, abiotic.means2)
ordPh
anova(ordPh)
plot(ordPh)

#interactive is less variance than the additive#


#ANSWER: Total Phosphorous and Olsen Phosphorus are the drivers of the overall signifigance of the 
#all inclusive model. All around, phosphorus is a large driver of plant growth.
#The use of Olsen Phosphorous tells me that the soils tested may be more alkaline, which means
#phosphorus becomes either locked up with calcium or is available for plants. When running with calcium
#and phosphorus, the relationship is significant. In comparison, a model with just phosphorus 
#explains less than a model with both calcium AND phosphorus. A model with pH, calcium, and phosphorous 
#is still less than the all-inclusive model as far as variance goes, but is also significant#
#I think overall, all the abiotic measures are predictors in small ways, but phosphorus is the largest
#predictor because of the interaction between phosphorus, calcium, and pH#



# (Q2 - 12 pts) Then use the dataset from the tutorial to create a linear model related to your RDA. Try multiple predictors to find the best fit model.
  # Explain the ecological importance of the significant predictors, or lack of significant predictors.


#I'm gonna pick Fraxinus excelsior since it was driving a relationship in the rda#
#The predictors I will look at will be land use to begin, but maybe see if theres any species competition type predictors#

hist(veg.transects$Fraxinus_excelsior)
#def not a normal distro#

abiotic.means2$Parcel <- unique(abiotic$Parcel)
veg.transects <- read_excel("Penaetal_2016_data.xlsx", sheet = "Vegetation_transects")
veg.transetcs3 <- veg.transects[-14,] #need to line up and take out A4 again#
veg.transects <- as.data.frame(veg.transects)

soil.vegtrans <- merge(abiotic.means2, veg.transects, by = "Parcel")

library(fitdistrplus)
library(logspline)

?fitdist

fit.norm <- fitdist(soil.vegtrans$Fraxinus_excelsior, distr = "norm")
fit.gamma <- fitdist(soil.vegtrans$Fraxinus_excelsior, distr = "gamma")
fit.nbinom <- fitdist(soil.vegtrans$Fraxinus_excelsior, distr = "nbinom")
fit.logis <- fitdist(soil.vegtrans$Fraxinus_excelsior, distr = "logis")
fit.geom <- fitdist(soil.vegtrans$Fraxinus_excelsior, distr = "geom")

gofstat(fit.norm)
gofstat(fit.gamma)
gofstat(fit.nbinom)
gofstat(fit.logis)
gofstat(fit.geom)

#best fit nbinom, gonna need a diff glm than the norm#

?glm
library(MASS)

mod1 <- glm.nb(Fraxinus_excelsior ~ Landuse + TotalP+ totalN, data=soil.vegtrans)
summary(mod1)
anova(mod1)
AIC(mod1)

#totalN not sig#

mod2 <- glm.nb(Fraxinus_excelsior ~ Landuse + TotalP+ pH, data=soil.vegtrans)
summary(mod2)
anova(mod2)
AIC(mod2)

#pH not sig#

mod3 <- glm.nb(Fraxinus_excelsior ~ Landuse + TotalP+ Kalium, data=soil.vegtrans)
summary(mod3)
anova(mod3)
AIC(mod3)

#better fit#

mod4 <- glm.nb(Fraxinus_excelsior ~ Landuse + TotalP*Kalium, data=soil.vegtrans)
summary(mod4)
anova(mod4)
AIC(mod4)

#Best fit, lowest AIC#



# (Q3 - 6 pts) Provide a 3-4 sentence synthesis of how these results relate to one another and the value of considering both together for interpreting biotic-abiotic interactions.

#Both models tell me that Total phosphorus is the most sig predictor of the plant populations, but
#that Phosphorus and Calcium interactively working together is also very important. I think
#that any of the micro nutrients are somewhat important, but from the ash tree models, we can confirm 
#that the phosphorus calcium interactions are the best predictors for this population of plants. 
#Although pH was not sig in this model, I still think that it plays a role, just maybe a smaller one. Looking
# at the paper, phosphorus differed greatly from ag to ancient sites, confirming that is the driver of 
#relationships in this data set with vegetation#
