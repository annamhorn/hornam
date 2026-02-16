#Load your libraries - install these if you do not have them:
library(fitdistrplus)
library(logspline)


#Read in the CSV for this week - saved somewhere you can find it:
setwd("C:/GitHub/hornam/Week5")
data <- read.csv("Ecoli_evolution.csv", header=TRUE)

#We are going to analyze distributions and recreate an analysis from:
  # Cooper et al. 2003 PNAS Parallel changes in gene expression after 20,000 generations of evolution in Escherichia coli
  # This paper was published before it became common to NOT transform data prior to analysis.
  # So we are going to play detective and see if this changes the conclusions.
  #(Paper is uploaded to Canvas)

# First, we'll recreate the plot and analysis from the paper for Figure 1(c).
  # This is a comparison of 4000+ genes between a clone and its ancestor after 20,000 generations
  # We are going to only look at a subset of the data that are available online. 
  # So the plot might be slightly different because it only includes a portion of the data
  # These data were downloaded from: https://lenski.mmg.msu.edu/ecoli/index.html
plot(data$Ara_plus1Ev ~ data$Ara_plusAnc, xlab ="Ara+ Ancestor", ylab ="Ara+1", main = "Fig 1(c)" )

  # The first thing you will notice is that there are several outliers in this plot
  # These are not included in the published figure and must have been removed - but they don't mention it!
  # It can be acceptable to remove outliers from an analysis, but you ALWAYS need to explain why.

# So is there a significant relationship between the overall combo of genes in ancestor and clone?
  # We can do a quick test with the lm() function
fig1c.mod <- lm(data$Ara_plus1Ev ~ data$Ara_plusAnc)
anova(fig1c.mod)
summary(fig1c.mod)
  # As you would expect, this is a very strong relationship with an R^2 of 0.73
# To visualize this, we will add a regression line to the plot which represents the lm() relationship:
plot(data$Ara_plus1Ev ~ data$Ara_plusAnc, xlab ="Ara+ Ancestor", ylab ="Ara+1", main = "Fig 1(c)" )
abline(fig1c.mod, col = "grey", lwd= 3)
  # The lwd argument increases the line weight to make it more visible 
  # What does it mean for values to be further above the line vs below it?
  # Increased expression over time (above) vs decreased expression (below)
  # This is a fast way to view how 4000 genes have been selected for or against after 20,000 generations
    # That is a lot of information to represent at once.
  # So in this case the strong relationship is expected and not interesting.
    # It's the values that deviate from that expectation that might tell a story.

# If you notice in the figure caption, these data have been log10 transformed. Let's see what the original data looked like:
  # The reverse of log10(x) is 10^(x) so:
nolog_data <- as.data.frame(10^data[,2:6])
  # We do not care about specific genes, so I have excluded them from the data frame.

#We'll start with the plot again, un-transformed:
plot(nolog_data$Ara_plus1Ev ~ nolog_data$Ara_plusAnc, 
     xlab ="Ara+ Ancestor", ylab ="Ara+1", main = "Fig 1(c) not transformed" )
#Wow, there are clearly 2 outliers in this dataset. Likely the result of genes that went extinct or evolved over the 20,000 generations.
# These two genes are making it impossible to see real patterns in the data, so we will remove them:
  # I am creating a new data frame this time to keep track of our steps.
  # Try to follow what this function is doing - it removes the rows where the value is over .02 for one of our variables:
lim_nolog <- as.data.frame(nolog_data[(nolog_data$Ara_plus1Ev <= 0.02),])
lim_nolog <- as.data.frame(lim_nolog[(lim_nolog$Ara_plusAnc <= 0.02),])
# Then replot

plot(lim_nolog$Ara_plus1Ev ~ lim_nolog$Ara_plusAnc, 
     xlab ="Ara+ Ancestor", ylab ="Ara+1", main = "Fig 1(c) not transformed, no outliers" )
  # The pattern is still there, but clearly a little weaker.
# We will re-run the lm() and anova() on these data to quickly inspect the results:

# First, without removing the outliers:
fig1c.mod2 <- lm(nolog_data$Ara_plus1Ev ~ nolog_data$Ara_plusAnc)
anova(fig1c.mod2)
summary(fig1c.mod2)
# These two points are soo far apart they give the indication there is no relationship at all.
# Now with the outliers removed:
fig1c.mod3 <- lm(lim_nolog$Ara_plus1Ev ~ lim_nolog$Ara_plusAnc)
anova(fig1c.mod3)
summary(fig1c.mod3)

# We have nearly the same relationship as the transformed data (which is good) AND a stronger R-squared!
  # Now replotting with a regression line makes the increase or decrease in expression more obvious.  
    # Unless it is a change in small values, then the expression is hidden in the sea of points.
plot(lim_nolog$Ara_plus1Ev ~ lim_nolog$Ara_plusAnc, 
     xlab ="Ara+ Ancestor", ylab ="Ara+1", main = "Fig 1(c) not transformed, no outliers" )
abline(fig1c.mod3, col = "grey", lwd= 3)

# We can take a closer look at that sea of points with the xlim and ylim arguments
plot(lim_nolog$Ara_plus1Ev ~ lim_nolog$Ara_plusAnc, 
     xlab ="Ara+ Ancestor", ylab ="Ara+1", main = "Fig 1(c) not transformed, no outliers", 
     xlim = c(0,0.0015), ylim= c(0,0.0015))
abline(fig1c.mod3, col = "grey", lwd= 3)
  # It is easier to view these changing genes with overall low expression now.
  # The value of the log transform was to make all points visible at once
  # The downside was hiding some of the patterns in expression
# Transformations for tests of significance are generally a bad idea, but CAN be useful for viewing data.
  # As always, it depends on the question and point you are trying to make clear from your plots.



# Distributions ####
# As we've mentioned, distributions (think histograms) can tell you a lot ecologically.


#We are working with 1 vector at a time, so I have created a shorthand here to re-run.
    # This way you can quickly test multiple vectors.
  #We'll start with the ancestor:

one.col <- lim_nolog$Ara_plusAnc #All we need to do is change the vector to re-run.

hist(one.col, main = "Ancestor")

# First try the normal distribution:
fit.norm <- fitdist(one.col, distr = "norm")
    # Error code 100 means that the function can't cope - these numbers are too far outside expectations.
    # We do have one option though - scaling the data.
# We can multiply or divide by a single value to allow the function to cope without compromising the distribution.
# We'll retry:
fit.norm <- fitdist(one.col*100, distr = "norm")
  # And it worked! Just remember that IF you need to rescale, then all of the following comparisons also need to be scaled the same.
# Now logistic
fit.logis <- fitdist(one.col*100, distr = "logis")

# And the strongly tailed distributions - Wiebull and Gamma:
fit.weibull <- fitdist(one.col*100, distr = "weibull", lower = c(0, 0), start = list(scale = 1, shape = 1))
fit.gamma <- fitdist(one.col*100, distr = "gamma", lower = c(0, 0), start = list(scale = 1, shape = 1))

# The others we discussed do not fit these data.
# For a full list of distributions to possibly be tested use:
?fitdist

# Now we can see which of these distributions fits our data best.
#Use AIC comparisons of distributions with the gofstat() function.
  # The fitdist() results need to be a list, so we use list() inside the function:
gofstat(list(fit.weibull, fit.gamma, fit.norm, fit.logis))
  # The gamma distribution is best.

# We can also retest without scaling to confirm:

fit.weibull <- fitdist(one.col, distr = "weibull", lower = c(0, 0), start = list(scale = 1, shape = 1))
fit.gamma <- fitdist(one.col, distr = "gamma", lower = c(0, 0), start = list(scale = 1, shape = 1))

#Use AIC comparisons:
gofstat(list(fit.weibull, fit.gamma))
  # And the difference between AIC scores is identical -> 36.17
  # Remember the absolute value of AIC is meaningless, it is only how it relates to other values.


#Let's test the expression of the evolved strain as well:

one.col <- lim_nolog$Ara_plus1Ev

hist(one.col, main = "Evolved")

fit.norm <- fitdist(one.col*100, distr = "norm")
fit.logis <- fitdist(one.col*100, distr = "logis")
fit.weibull <- fitdist(one.col*100, distr = "weibull", lower = c(0, 0), start = list(scale = 1, shape = 1))
fit.gamma <- fitdist(one.col*100, distr = "gamma", lower = c(0, 0), start = list(scale = 1, shape = 1))


#Use AIC comparisons of distributions:
gofstat(list(fit.norm, fit.logis, fit.weibull, fit.gamma))

  # The Wiebull distribution has a lower AIC, and is the best fit.
    # This makes sense as it can roughly be described as an "exponential Poisson" i.e. REALLY strong tail toward zero.

# What do you think this means for overall diversity of genes being expressed between the ancestor and 20,000th generation progeny?


##################

# The 20,000th generation has a stronger skew toward zero, so in general it is expressing fewer genes/lower diversity.
# This also tells you a basic linear model is the WRONG approach! Think about why.
  # We'll work more on the 'right' approach next week.






