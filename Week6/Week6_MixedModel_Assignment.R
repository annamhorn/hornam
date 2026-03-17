# Read in the "Toscano_Griffen_Data.csv" data from GitHub and load the three packages we used in the tutorial this week.
# The paper these data came from is uploaded to Canvas as "Toscano&Griffen_2014_JAE..."
library(MASS)
library(MuMIn)
library(mgcv)

setwd("C:/GitHub/hornam/Week6")
df <- read.csv("Toscano_Griffen_Data.csv")
head(df)
?glmmPQL
?family

#ADDITIVE MODEL#
glmm.mod.add <- glmmPQL(prop.cons~carapace.width+claw.width+activity.level, random = ~ 1 | block, family = binomial, data= df)
summary(glmm.mod.add)
plot(glmm.mod.add)
#Slightly funky looking residuals#

#INTERACTIVE MODEL#
glmm.mod.int <- glmmPQL(prop.cons~carapace.width*claw.width+activity.level, random = ~ 1 | block, family = binomial, data= df)
summary(glmm.mod.int)
plot(glmm.mod.int)
#Still funky looking residuals, more sig p values for both widths, and carapace width and claw width#

# First create models with the same (y) and method (GLMM) as the published paper, using the GLMM function from this week's tutorial. 
  #Create two different models using the same 3 predictor (x) variables from the dataset. (4 points each) 
    # In one model only include additive effects.
    # In the other model include one interactive effect.
    # Use a binomial distribution and block as a random effect in both models to match the paper's analyses. Remember ?family to find distribution names.



# The authors used proportional consumption of prey as the (y) in their model, but did not include this in the dataset.
  # So we are going to create it - run the following line, assuming "df" is your data frame (feel free to change that):
df$prop.cons <- df$eaten/df$prey #out of order with models

# (Q1) - The code I've provided in line 13 above is performing two operations at once. What are they? (2 pts)

#The code is creating a row in our df by dividing the values in two columns already existing in our df.#

# (Q2) - Did the interactive effect change which variables predict proportional consumption? How, SPECIFICALLY, did the results change? (5 pts)

#Yes, initially none of the variables were good predictors based on the p-values. However
#adding the interactive effect changed it so that carapace width, claw width, and 
#carapace width*claw width were significant predictors. 

# (Q3) - Plot the residuals of both models. Do you think either model is a good fit? Why or why not? (3 pts)

#Based on the residuals neither model is a great fit. They both have residuals that almost 
#have a pattern to them, and are not random-looking.#

# Re-run both models as generalized additive models instead (using gam). Then compare the AIC of both models. (4 points each)

gam.mod.add <- gam(prop.cons~carapace.width+claw.width+activity.level, random = ~ 1 | block, family = binomial, data= df)
summary(gam.mod.add)
plot(gam.mod.add$residuals, ylim = c(-3,3))

gam.mod.int <- gam(prop.cons~carapace.width*claw.width+activity.level, random = ~ 1 | block, family = binomial, data= df)
summary(gam.mod.int)
plot(gam.mod.int$residuals, ylim=c(-3,3))

AIC(gam.mod.add, gam.mod.int)
#interactive model lower AIC by about 2 points#

# (Q4) - Which model is a better fit? (2 pt)

#Based on the AIC, the interactive model is a better fit.#

# (Q5) - Based on the residuals of your generalized additive models, how confident are you in these results? (2 pts)

#I am not entirely confident in these results. The residuals look better compared to the glmm,
#but they still don't look entirely random. I think the residuals of the interactive gam
#actually look worse than the additive gam, despite the AIC.#
#I agree. They're still funky.




