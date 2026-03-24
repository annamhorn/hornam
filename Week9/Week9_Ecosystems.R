# Startup ####
# This week will include multiple analyses to identify links between biotic and abiotic measurements.
#Data from:
  # https://datadryad.org/stash/dataset/doi:10.5061/dryad.5hk04
  # Beyond plant-soil feedbacks: mechanisms driving plant community shifts due to land-use legacies in post-agricultural forests
    # Pena, et al. Functional Ecology 2016
# These data are formatted as several sheets in an excel workbook
# We will use the readxl package and read_excel() function to convert individual sheets into data frames.

library(readxl)

setwd("C:/GitHub/hornam/Week9")

# First, read in the abiotic data:
  # Make sure the excel file is NOT open on your computer or it will generate an error (unlike read.csv)
abiotic.tibble <- read_excel("Penaetal_2016_data.xlsx", sheet = "Abiotic factors")

#Unfortunately, the read_excel function transforms our data into a "tibble" format
  # Tibbles are almost as evil as pie charts. 
  # Or, like cats, tibbles might look pretty but some day they will bite you when you least expect it.
# This can easily be fixed with our old friend as.data.frame:
abiotic <- as.data.frame(abiotic.tibble)

# First let's see what patterns might exist in the soil chemistry by land use or site:
head(abiotic)
boxplot(pH ~ Site, data= abiotic, main = "pH by Site")
boxplot(pH ~ Land_Use, data= abiotic, main = "pH by Land Use")

# What about a micronutrient like potassium AKA Kalium (the authors are Dutch)
boxplot(Kalium ~ Site, data= abiotic, main = "K by Site")
boxplot(Kalium ~ Land_Use, data= abiotic, main = "K by Land Use")

# Or a macronutrient,like total nitrogen:
boxplot(totalN ~ Site, data= abiotic, main = "N by Site")
boxplot(totalN ~ Land_Use, data= abiotic, main = "N by Land Use")
  # I am surprised! I expected post-ag to have high legacy nitrogen in the soils.

# We can continue to look at more comparisons like this. 
  # For now, suffice that there are clearly differences in soil chemistry between use and site details.

# Nematodes ####
# We'll start by looking at community comparisons between the soil and the nematodes:
# To do this we'll read in the sheet with nematode community data.
  # I don't know who goes through the trouble to identify nematodes, but luckily for us these authors did.
nema.tibble <- read_excel("Penaetal_2016_data.xlsx", sheet = "Nematode_community")
nema <- as.data.frame(nema.tibble)
head(nema)

# The first thing to notice here is there are 3 nematode samples per plot, but 5 abiotic samples.
  # The location data are compatible, but the number of samples are not.
# The most straightforward correction is to average data by plot to create a 1-to-1 comparison.
# Unfortunately for us, the $plot is repeated by location and type.
  # so we need a unique identifier for each plot within each land use, within each location.
# For this, we will turn to the paste() function
abiotic$names <- paste(abiotic$Site, abiotic$Land_Use, abiotic$Plot)

head(abiotic)
#Same for the nematodes, but with different column names:
nema$names <- paste(nema$Location, nema$Land_use, nema$Plot)

#Now we can use aggregate() to create means by the $names column:
abiotic.means <- aggregate(x = abiotic, by = list(abiotic$names), FUN = "mean")
  # This created warnings, so we should see what the data frame looks like:
head(abiotic.means)
  # The warnings are a result of the non-numeric columns, which have just been converted to NAs
  # Not a big deal for our purposes.

# Nematodes as well:
nema.means <- aggregate(x = nema, by = list(nema$names), FUN = "mean")
# This also created warnings, so we should see what the data frame looks like:
head(nema.means)
  # It's the same issue of creating NAs, which we can ignore.

# For our multivariate analysis we need to remove the NA and plot columns:
abiotic.means1 <- abiotic.means[,-16] # NA column
abiotic.means2 <- abiotic.means1[,-1:-6] # Plot and NA columns
abiotic.means2 <- sapply(abiotic.means2, as.numeric ) # Make sure everything is numeric.
abiotic.means2 <- as.data.frame(abiotic.means2) # Make sure it's in the right format.

nema.means1 <- nema.means[,-41] # Remove NAs
nema.means2 <- as.data.frame(nema.means1[,-1:-4]) # Remove plot and NAs
nema.means2 <- sapply(nema.means2, as.numeric )

# And we can FINALLY compare the abiotic data against the biotic communities:
library(vegan)
colnames(abiotic.means2)
ord <- rda(nema.means2 ~ pH + totalN + Perc_ash + Kalium + Magnesium + Ca + Al + TotalP + OlsenP, abiotic.means2)
ord
#63% of the variance explained is pretty good!
  # It looks like almost all of it comes from Axis 1
anova(ord)  
  # But the p-value is not significant. It appears our data might be over-fitted
plot(ord, ylim = c(-2,2), xlim = c(-5,5))  
  # I've added the ylim and xlim to remove two extreme outliers and make the plot easier to interpret.
  # The first hint at our high p-value is the co-linear terms. Some arrows are nearly on top of each other.
  # This is penalizing the model, so we should use a selection process to remove predictors that are not important.

# The ordistep() and ordiR2step() functions will do this. They use a selection process to keep or remove variables.
  # ordistep() will generate p-values with significant predictors.
  # ordiR2step() will give the associate R-squared values.
# This is done by removing variables one "step" at a time to see how the predictive ability of the model changes.
# We need to give it a bit of help. The step function needs something to compare against.
  # We will use an intercept model, essentially acting as the null hypothesis:
  #
ord <- rda(nema.means2 ~., abiotic.means2) # shorthand for the model that includes everything.
ord.int <- rda(nema.means2 ~1, abiotic.means2) # shorthand for the model that only includes intercepts.

# The "both" selection method essentially takes two steps forward, then one step back, and compares the results.
  # Then selects which variable to drop and repeats the process
step.mod <- ordistep(ord.int, scope = formula(ord), selection = "both")
step.mod$anova

step.R2mod <- ordiR2step(ord.int, scope = formula(ord), selection = "forward")
# No ANOVA test for the ordiR2step function.

# From this, only nitrogen is predicting the community.
  # It is the only significant predictor and has an R-squared that is actually higher than including all of the variables.

# We'll re-run with just nitrogen and look at the results:
ord2 <- rda(nema.means2 ~ totalN, abiotic.means2)
ord2
anova(ord2)
plot(ord2)
  # We are explaining much less variance now, but it's a significant relationship.

# Botany ####
# It doesn't make much intuitive sense why nematodes respond to soil nitrogen.
# So we'll look at which nutrients matter for the plants next - a potential food source and food's food source for nematodes.
# The authors performed an herbivory experiment within a subset of these plots.
  # Next we will bring the mean abiotic data together to identify any potential effects of soil chemistry on plant growth.

#Read in the plant experiment data:
plants.tibble <- read_excel("Penaetal_2016_data.xlsx", sheet = "Data_intro_exp_4_species")
plants <- as.data.frame(plants.tibble)

# Now we need a column to merge our data frames.
  # "Parcel" is an identifier I have made sure matches across all of the spreadsheets, where relevant.
# The abiotic means will be more important than individual measurements again, because we do not know which soil sample matches which individual plant.
  # The aggregate function calculates means in row order, so we can use the unique() function to create a one-to-one match.
  # Then create a column called "Parcel" that allows us to merge with the plants data frame based on Parcel numbers.
abiotic.means2$Parcel <- unique(abiotic$Parcel)

# So we merge by "Parcel" and accept all other defaults.
  # Merge can be used for more complex problems if needed (see ?merge). Fortunately, in our data the Parcel match is clear.
soil.plants <- merge(abiotic.means2, plants, by = "Parcel")

# Let's take a quick look at our data.
  # This data frame is getting large enough that View() might be more helpful than head() for an initial look:
View(soil.plants)

# The most consistently measured marker of plant growth is "Leaves" so we will use this as the y in our model.

# We'll quickly do a distribution test of our y (leaves):
library(fitdistrplus)
library(logspline)


#Plot distribution against ideals
##fit all possible/likely distribution models and check AIC/BIC
fit.weibull <- fitdist(soil.plants$Leaves, distr = "weibull")
fit.norm <- fitdist(soil.plants$Leaves, distr = "norm")
fit.gamma <- fitdist(soil.plants$Leaves, distr = "gamma")
fit.lnorm <- fitdist(soil.plants$Leaves, distr = "lnorm")
fit.nbinom <- fitdist(soil.plants$Leaves, distr = "nbinom")
fit.logis <- fitdist(soil.plants$Leaves, distr = "logis")
fit.geom <- fitdist(soil.plants$Leaves, distr = "geom")

#Use AIC comparisons of distributions to see which is best:
#call from:
gofstat(list(fit.weibull, fit.norm, fit.gamma, 
             fit.lnorm, fit.nbinom, fit.logis, fit.geom))
# Log normal is clearly the best fit. 


# While we have not checked the distribution of the $Leaves response, this is a good indication we just might be able to get away with a linear model. 
  # We will confirm with the residuals.
# Let's look at the available predictor variables before we build the model:
colnames(soil.plants)
#"Parcel"              "pH"                  "totalN"              "Perc_ash"            "Kalium"             
# "Magnesium"           "Ca"                  "Al"                  "TotalP"              "OlsenP"             
# "ID"                  "Species_code"        "Land_use"            "Code"                "Number"             
# "Length_cm"           "Stems"               "Herb_marks_no_holes" "Leaves"              "eaten_leaves"       
# "Herb_proportion"     "Herbivory_index"    
 
# Based on the View() of the data, we can already tell some of these explanatory variables show exactly the same thing.
  # So we will choose our "global" (most complicated) model based on the predictors that are ecologically reasonable.
  # In other words, land use is more important than ID.
# Based on the available options, we'll start with this global model:
mod1 <- lm(Leaves ~ pH + totalN + Kalium + Magnesium + Ca + Al + TotalP + Land_use + Species_code, data = soil.plants)
summary(mod1)
anova(mod1)
AIC(mod1)
  # Notice the NAs in the summary? We have way too many variables relative to the number of samples.
# We will also look at the model's adjusted R-squared with the following line.
  # This is a shorthand to extract the R-squared rather than looking at the entire summary printout:
summary(mod1)$adj.r.squared

#The first step for improving the mode will be to remove Mg, Ca, AL, P, and Land use
  # There is not enough variation in these variables to be meaningful (hence the NA's).
mod2 <- lm(Leaves ~ pH + totalN + Kalium + Species_code,soil.plants)
summary(mod2)
anova(mod2)
AIC(mod1,mod2)
  # Now we have an AIC to compare against.
# Check the residuals:
plot(mod2$residuals)
  # The residuals aren't great, but they aren't terrible either.
  # It's a good sign we'll be able to stick with a linear model.

summary(mod2)$adj.r.squared
# The adjusted R-squared of the model is pretty good for ecological data at 0.55

# We'll drop the kalium (aka potassium) from the model now because it's not showing much predictive ability.
mod3 <- lm(Leaves ~ pH + totalN + Species_code,soil.plants)
summary(mod3)
anova(mod3)
AIC(mod2, mod3)
plot(mod3$residuals)
summary(mod3)$adj.r.squared

  # Hmmm, this really didn't do much of anything to the model. The AIC is just a little lower and R-squared is the same
  # Our residuals don't look any better either.
  # Perhaps potassium has an interactive effect with one of the other variables.

# So adding kalium back in and looking at interactive effects in soil chemistry is the next step.
mod4 <- lm(Leaves ~ pH*totalN*Kalium + Species_code,soil.plants)
summary(mod4)
anova(mod4)
AIC(mod2,mod3,mod4)
plot(mod4$residuals)
summary(mod4)$adj.r.squared

#Our AIC and the residuals have not improved.
  # Even more, the interactions are all NA meaning they are not relevant in this model. 
  # Soil chemistry and nutrients might interact, but we do not have enough data to effective test whether this is the case.

# If we look at our anova or summary outputs thus far, the species are always significant, so growth varies by species.
  # But we know not all species have the same response or needs for soil quality and nutrients.
  # Perhaps individual soil variables interact with the species.

#We'll try total nitrogen first as it has the strongest relationship with leave growth of the abiotic factors.
mod5 <- lm(Leaves ~ pH + Kalium + totalN*Species_code,soil.plants)
summary(mod5)
anova(mod5)
AIC(mod2,mod3,mod4,mod5)
plot(mod5$residuals)
summary(mod5)$adj.r.squared
# Our AIC is MUCH better and the residuals are more evenly distributed around zero. 
  # It looks like we are headed in the right direction!
# Also notice explanatory ability of potassium now? 
  # There was an interactive pattern between N and species that hid the value of a micronutrient.

# Maybe pH also has an interactive effect?
mod6 <- lm(Leaves ~ Kalium + pH*totalN*Species_code,soil.plants)
summary(mod6)
anova(mod6)
AIC(mod2,mod3,mod4,mod5,mod6)
plot(mod6$residuals)
summary(mod6)$adj.r.squared

# Our updated model has nearly the same AIC (within 2 of each other), with slightly different relationships. 

# Because mod5 has the same number of predictor variables yet fewer interactions, we will consider that our "best" fit.
  
# When looking at the anova(mod5) results it's clear potassium, nitrogen, and plant species are important predictors of leaf growth.
  # In addition, the effect of nitrogen is not the same for all species (some respond more than others).

# As nematodes are mostly omnivores or predators, 
  # perhaps their relationship with total nitrogen is actually a result of the increased primary production.

# When combining multiple analyses like this you can create a more complete picture of the entire ecosystem.






