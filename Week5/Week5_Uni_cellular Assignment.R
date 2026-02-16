# First, recreate Figure 4 from Herron et al. (2019). De novo origins of multicellularity in response to predation. Scientific reports.
  # Search datadryad.org by the paper title and download the dataset. It will include .csv files and R scripts, organized by figure.
  # Save the script and change the working directory on lines 8 and 115 to match your computer
  # Upload the plot you've created to GitHub. (4 points)
  # Zoom into your plot to look at the distribution for different strains.

library(dplyr)
library(reshape2)
library(ggplot2)
install("fitdistrplus")
library(fitdistrplus)

#Plot Product: 9 strains, 2 w/ high N, 1 w/ lower N, last 3 with wider distros, last 2 w/ "normal" distros#

# Do all of the strains in the plot have the same distributions (yes/no)? (1 pt)

#No - some have much more widespread (normal) distributions and the majority of them are clumped close to 0#


# Based on these observations of your strain distributions, why did the authors use a Kruskal-Wallis test rather than ANOVA to compare the strains? (3 pts)

#ANOVA is normally used on data that can you assume normality with. Kruskal-Wallis is used in data that is 
#not distributed normally#



# Use the fitdist() and gofstat() functions to compare the poisson, negative binomial, and logistic distributions for:
  # (1) - The number of cells of progeny (data$Num.Cells.Progeny)
  # (2) - The replication time (data$RepTime.sec)
    #HINT- "Num.Cells.Progeny" has defined breaks. To display results, use the formula with the "chisqbreaks" argument as follows:
      #gofstat(list(fit.1, fit.2, fit.3, etc), chisqbreaks=c(1,2,4,8,16,32,64))

?fitdist


fitpois <- fitdist(data.new$Num.Cells.Progeny, "pois")
fitnbinom <- fitdist(data.new$Num.Cells.Progeny, "nbinom")
fitlogis <- fitdist(data.new$Num.Cells.Progeny, distr = "logis")
gofstat(list(fitpois, fitnbinom, fitlogis), chisqbreaks=c(1,2,4,8,16,32,64))

#PROGENY#
#Goodness-of-fit criteria
#1-mle-pois 2-mle-nbinom 3-mle-logis
#Akaike's Information Criterion   8156.763     7315.569    7524.659
#Bayesian Information Criterion   8162.479     7326.999    


data.reptime <- data.frame(na.omit(data$RepTime.sec))

fitpois_rep <- fitdist(data.reptime$na.omit.data.RepTime.sec., "pois")
fitnbinom_rep <- fitdist(data.reptime$na.omit.data.RepTime.sec., "nbinom")
fitlogis_rep <- fitdist(data.reptime$na.omit.data.RepTime.sec., distr = "logis")
gofstat(list(fitpois_rep, fitnbinom_rep, fitlogis_rep))

##REPTIME##

#Goodness-of-fit criteria
#1-mle-pois 2-mle-nbinom 3-mle-logis
#Akaike's Information Criterion   90995373     56169.02    56698.89
#Bayesian Information Criterion   90995379     56180.45    56710.32


# Based on the AIC scores, which distribution is the best fit for: (5 pts each)
  # (1) - The number of cells of progeny (data$Num.Cells.Progeny)?

#The best fit here is the negative binomial distribution#

  # (2) - The replication time (data$RepTime.sec)?

#The best fit here is also the negative binomial distribution#


# Plot a generic histogram for the replication time (data$RepTime.sec) (4 pt)

?plot
  
hist(data.reptime$na.omit.data.RepTime.sec.,xlab = "RepTime (seconds)",main = "Rep Time Frequency")

# Based on the patterns of this histograms and Figure 4:
  #Give one hypothesis for an evolutionary process represented by the two tallest bars in your histogram. (8 pts)
  # Don't cheat by looking at the paper! 
    # This hypothesis does not need to be correct - it only needs to be ecologically rational based these two figures.


#There is a binomial distribution of the rep time in seconds. The two clusters may be representative of two similar "groups" of strains that have a 
#similar trait that would give them a similar replication time. This could be reflected in Figure 4,
#where we see 6 strains with a generally lower number of cells, and 3 with a higher number of cells, 
#indicating differences in reproduction between the strains#





