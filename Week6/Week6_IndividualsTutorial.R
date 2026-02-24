# This weeks data come from: https://datadryad.org/stash/dataset/doi:10.5061/dryad.21f0h31
  # Uchida et al. 2019 Decreased vigilance or habituation to humans? Mechanisms on increased boldness in urban animals. Behavioural Ecology.
  # The paper is available as a PDF in this week's folder on Canvas.

setwd("C:/GitHub/hornam/Week6")

df <- read.csv("Uchidaetal_2019_Data.csv")
# Take a look and see what our data look like this week:
head(df)

#Generalized Linear MIXED model: ####
# The authors used ANOVAs to test the significance of different potential 'predators' in generating a flight response among squirrels
  # They also tested this across rural or urban locations in several regions.
  # They did not, however, consider individual variation in the models - i.e. some squirrels are actually scaredy cats.
  # So we are going to test if there is a random effect of individual on their results that was not picked up by the ANOVAs
#First install and load these two packages - they work in tandem with each other.
library(MASS)
install.packages("MuMIn")
library(MuMIn)

# We are going to use the glmmPQL() function as it is designed to handle non-normal data in a linear model.
# However, the "PQL" in the glmmPQL() function stands for "Penalized Quasi-Likelihood". 
  # The "quasi" likelihood unfortunately means a "correct" answer is more difficult to find.
  # It also means our standard R-squared and p-values are a little more difficult to calculate, 
    # so we will be using some specialized functions.
# For more details on this particular function:
?glmmPQL

# To start, we are going to use a assume a normal (aka Gaussian) distribution because we are using a linear model:
# The argument "random = ~ 1 | ID" is used to specify individual as a random effect. 
  # The "~ 1 |" notation is common across model functions to signify a random effect.
  # The 'family' argument is used to specify the distribution family.
      # Options for this can be found by looking up ?family.
  # Otherwise this function should look very similar to the arguments in the lm() function for fixed effects.
glmm.mod <- glmmPQL(Flight.initiation.distance..FID.~Object, family = gaussian, random = ~ 1 | ID, data = df)

# We can still use the summary() function to look at results:
summary(glmm.mod)
  # From this it looks like humans MIGHT impact how fast a squirrel runs away, but it's hardly a strong case.

#To check the r-squared we need a new function. 
  #With random effects included, we need to separate the random effects from the predictors:
r.squaredGLMM(glmm.mod)
#The R squared gives us two results - the R2m is "marginal"
  # This is the R-squared we really care about. It represents the variance explained by "fixed" effects.
  # These are the ones we have "fixed" in the model - the usual (x) values.

# The R2c value is the "conditional" variance explained by the random effect of individual
  # So this is saying the response is much more strongly predicted by boldness of the individual squirrel 
    # rather than the presence of a human or other predator. 

# Let's see if whether the squirrel comes from an urban or rural area has an impact on it's flight response:
glmm.mod2 <- glmmPQL(Flight.initiation.distance..FID.~Object + Area, family = gaussian, random = ~ 1 | ID, data = df)
summary(glmm.mod2)
  # Suddenly our "human" is significant, novel objects are not, and it's strongly dependent on whether we are in urban or rural areas.

r.squaredGLMM(glmm.mod2)
  # This is a MUCH better fit of a model based on these results. The R2m has increased 10 fold. 
    # Some variance can overlap between R2m and R2c, so it's ok these add up to more than 1.

# Now that we have a good model, let's check the residuals for homogeneity:
plot(glmm.mod2)
# Look how NOT random these values are! Clearly the data are not fitted properly.

#If we simply LOOK at the histogram we can tell the (y) is not normally distributed:
hist(df$Flight.initiation.distance..FID.)

# This skew toward zero falls in the "Gamma" family of distributions, so we'll try that family in the model:
glmm.mod3 <- glmmPQL(Flight.initiation.distance..FID.~Object + Area, family = Gamma, random = ~ 1 | ID, data = df)
plot(glmm.mod3)
  # This still isn't ideal, but it is MUCH better than before.
summary(glmm.mod3)
  # Our overall results are about the same significance.

r.squaredGLMM(glmm.mod3)
  # Notice the error (if you install the "performance" package)? This is the problem with trying to fit a non-normal distribution into a linear model. It doesn't always work
  # Between this error and our pattern in the residuals, it's time to try a non-linear option.

#Generalized ADDITIVE mixed models:####

# The "mgcv" package will allow us to create generalized additive models. 
  #It also works in tandem with "MuMIn", but we already have that loaded from the GLMM analyses.
install.packages("mgcv")
library(mgcv)

# The only difference in syntax from our glmmPQL will be how random variables are specified. 
  # This function only wants them included as a list(), as written here:

gam.mod1 <- gam(Flight.initiation.distance..FID.~Object + Area, family = Gamma, random = list(ID=~ 1), data = df)

summary(gam.mod1)
  # Our results are very similar to before.

# What about our residuals?
plot(gam.mod1$residuals, ylim = c(-.1,.1))
# Our residuals look MUCH better. I've forced the ylim to zoom in and compare with the next model.

# gam() also allows you to model how two predictor variables effect the y variable simultaneously.
  # This is one of the very rare times when I will approve of a 3D plot:
vis.gam(gam.mod1, view=c("Object","Area"), theta = 45, color = "heat")
  # It looks a bit odd with factors forced into groups, but this will become more useful for numeric predictors.

# We can also easily calculate AIC gam() models for comparisons:
AIC(gam.mod1) 

# So let's compare models. In the paper they found strong interaction effects between habitat and object.
  # All we need to do is change the '+'symbol to a '*' because interaction models also test the predictors individually.

gam.mod2 <- gam(Flight.initiation.distance..FID.~ Object*Area, family = Gamma, random = list(ID=~ 1), data = df)

summary(gam.mod2)
# Notice in the interactive model the r-squared went from 0.27 to 0.38, which is a good sign. 
  # But we've also added two interactive predictors with multiple groups in each.

# First let's look at the residuals
plot(gam.mod2$residuals, ylim = c(-.1,.1))
  # This still looks good. Full disclosure, there are a couple outliers not included in this plot but overall there isn't a pattern.

# We can compare our AIC scores side-by-side now:
AIC(gam.mod1, gam.mod2)

# The interactive model is a better fit for the data with both a higher R-squared and lower AIC, so that would be the best approximation of these data.


