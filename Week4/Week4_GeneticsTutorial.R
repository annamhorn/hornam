# Installing and loading the packages and data ####
#Genetic richness, diversity, and evenness
# This section of the tutorial is adapted from SE Everhart, ZN Kamvar, and NJ Grünwald
  # Their tutorial can be found here: https://grunwaldlab.github.io/Population_Genetics_in_R/Genotypic_EvenRichDiv.html

# First we will install the required packages to open the example dataset and run basic analyses.
  # Genetic data and packages evolve very quickly so we often need to specify versions of some packages.
  # This insures all of your packages will play well with each other - click "yes" on all of the popup windows.
require(devtools)
install_version("htmltools", version = "0.5.0", repos = "http://cran.us.r-project.org")
install.packages(c("sourcetools", "shiny"))
install.packages("poppr")

# The "popper" package loads all of the others automatically with it
library(poppr)
# Example dataset we will be using:
data(monpop)
# Genetic data often have unique metadata attached to them as lists - These lines simply format the data so we can use them.
splitStrata(monpop) <- ~Tree/Year/Symptom
setPop(monpop) <- ~Symptom
#Let's take a look at the printout
monpop
  #Clearly this is not like any of the data frames we have worked with so far.

# For our purposes, we will look at the presence/absence data table:
monpop$tab
  # It looks like we have a sizable table on our hands. 
  # To make the structure a little easier to view, try the head() function:
head(monpop$tab)
  # Still a lot of columns, but there are two things we can learn:
    # (1) These data are all one's or zero's. In our case: 0 = absent and 1 = present
    # (2) Coloumns are locations and rows are sequences (based on the printout from monpop$loc.fac)
      # This means the table represents sequence presence or absence at different locations.
# How many sites and sequences?
nrow(monpop$tab)
ncol(monpop$tab)
    # We have almost 700 sequences and 95 different locations in the dataset. 

# Despite the large size, we can still approach these data in similar ways to our smaller data frames.
  # These data should feel familiar to you from last week's homework for calculating diversity metrics.
    # Last week it was number and abundance of a species at a site.
    # This week is the presence/absence of sequences at a site.
    # Both cases include location data from several points and a number of possible 'things' (species or sequences) that can be present at each site.


#Genetic Diversity ####
# First we will try running the same diversity metrics from last week on our dataset. For this we need to load the vegan package:
library(vegan)
  #The following will create a metric for each site (column), then take the mean of these values for an overall estimate:
gene.simp <- diversity(monpop$tab, index="simpson")
mean(gene.simp, na.rm = TRUE)
    # Mean Simpson's diversity for the data is 0.92
gene.shan <- diversity(monpop$tab, index="shannon")
mean(gene.shan, na.rm = TRUE)
    # Mean Shannon diversity is 2.56
#Let's check the standard deviation as a measure of variance too:
sd(gene.shan, na.rm = TRUE)
  #Well it probably isn't a good sign the standard deviation is 0. Either all of the values are identical, or there is a problem with our code.

gene.shan
  #Check the console - all of our values ARE identical. This is a byproduct of the genetic data structure...and problematic for any further analyses or interpretation.


# Because these data are presence/absence and based on sequences rather than species, geneticist have adapted these metrics.
  # The first package we loaded - poppr - is specifically designed to handle diversity measures of genetic data.

# The output from this function will generate several standard diversity measures in genetics:
poppr(monpop)
 #The abbreviations in the column names stand for the following metrics (from using ?poppr):
      #Pop  	Population name.
      #N	    Number of individuals observed.
      #MLG	  Number of multilocus genotypes (MLG) observed.
      #eMLG	  The number of expected MLG at the smallest sample size ≥ 10 based on rarefaction (we will get to rarefaction in a couple weeks)
      #SE	    Standard error based on eMLG.
      #H	    Shannon-Wiener Index of MLG diversity (Shannon, 2001).
      #G	    Stoddart and Taylor’s Index of MLG diversity (Stoddart & Taylor, 1988).
      #lambda	Simpson’s Index (Simpson, 1949).
      #E.5	  Evenness, E5 (Pielou, 1975; Ludwig & Reynolds, 1988; Grünwald et al., 2003).
      #Hexp	  Nei’s unbiased gene diversity (Nei, 1978).
      #Ia	    The index of association, IA (Brown, Feldman & Nevo, 1980; Smith et al., 1993).
      #rbarD	The standardized index of association, r¯d [@].

      # A couple of these metrics should look familiar from your assignment last week - Simpson's and Shannon diversity.

# Look at the column for 'H' and 'lambda' - these correspond to the genetic versions of Shannon and Simpson's diversity.
# Notice the values are larger? That's because these equations are modified for presence/absence.
  # The metrics we used from vegan are designed for count data.
    # The point? Make sure you understand what your data represent - if it's true counts, relative abundance, or presence/absence.


# Genotypic Evenness####

# To look at genotypic evenness we can use a variation of our old friend the histogram.
  # This time we are comparing histograms for the height (count) of dominant bars (richness) rather than the best distribution model.
mon.tab <- mlg.table(monpop)
    # If you look a the very messy plots you can see the "FR" group has much higher peak counts for a handful of genes.
    # Do you think the genotypes are more "even" in the BB or the FR population?

        # If you said BB, then you are correct! The most abundant genotypes in BB are still <1/4 as dominant as for FR.
# In case you were wondering - yes! You can indeed recreate a version of these plots with the hist() function:
  # First, the data are transposed so we will re-orient them with the t() function:
hist.tab <- t(mon.tab)

#Then create individual histograms for each group. 
  # Remember that these are single plots, so we need to specify individual vectors.
hist(hist.tab[,1], breaks = 25, main = "BB")
hist(hist.tab[,2], breaks = 25, main = "FR")
  # Notice from these FR is still dominated more by a few taxa but the pattern is less obvious. 
    # Here, again, the number of breaks matters for telling your story.

# The questions might be very different, but the methods haven't changed much yet.
# Now the methods really DO change.

# Phylogenetic trees ####
# At their core, phylogenies are comparisons of similarities and differences of specific features
    # These can be as tangible as jaw length or as challenging as entire genomes.
    # More and more, genetic methods are used almost exclusively.
  # Without going into the 'deep learning' and Bayesian methods we will use maximum parsimony in this example.
  # Aspects of maximum parsimony will also be revisited in our week on communities.

# The haplotypes package and "dna.obj" dataset work well for this:
install.packages("haplotypes")
library(haplotypes)
# If desired, you can always rename package-based data with the '<-' operation like this:
data("dna.obj")
x<-dna.obj
# The parsimnet() function will calculate the most parsimonious relationships with 95% probability:
# This is the TCS method of: Templeton, A. R., Crandall, K. A. and Sing, C. F. (1992) Genetics, 132, 619-635.
  # It uses the raw number of base pair changes/differences between sequences to compute a "distance matrix".
  # The distance matrix compares every single sequence to every other sequence.
  # Then creates the most parsimonious pathway (i.e. fewest steps) for these sequences to have evolved.
  # The output plot creates an unrooted phylogenentic tree (i.e. no assumption of ancestory)
p<-parsimnet(x)
p
# Viewing the result is a little different - here we see more summary-type information about the result.
  # The number of connection steps of haplotypes gives us a sense of the relative complexity of these relationships

## Plotting with default parameters.
plot(p)
  #From this week can see that sequences 5, 24, and 16 are all very different from each other.

# Sometimes you know where relationships should really be based on other data (fossils, geography, etc.).
  # In these cases it can be valuable to move nodes manually to force a new relationship.
  # Plot the following, 
plot(p, interactive=TRUE)
  # Now click on a node, move your cursor to another location, and click again.
  # This might take a few seconds depending on which node is moved and how far (watch the "STOP" button in the corner of the console)
#Use the escape key when you are done.
  
# If you would like to learn more about phylogentic tree building, this page is a nice summary:
    # https://fuzzyatelin.github.io/bioanth-stats/module-24/module-24.html 



# Environmental relationships ####
#For this exercise I have pulled the dataset from Dryad (open source data repository; datadryad.org) and reorganized the structure for our purposes.
# These data are from: https://datadryad.org/stash/dataset/doi:10.5061/dryad.fb436.
  # Molecular proxies for climate maladaptation in a long-lived tree (Pinus pinaster Aiton, Pinaceae) by Jaramilo-Correa et al.
  # These columns represent location data and the diversity of individuals at several different loci.
  # For context, there are some great maps of these allele frequencies in their paper...even if they do include pie charts.
    # The paper is uploaded to Canvas in this week's folder under "Jaramillo-Correa et al. - Molecular proxies and pines"
  
# Make sure to set your working directory then read the data into R. If these next two lines generate an error, then you do not have GitHub setup properly on your computer.
  # I can not stress this enough - do NOT change these lines if they don't work - change your GitHub directory setup.
setwd("C:/GitHub/hornam/Week4")
data <- read.csv("Dryad_data.csv")

# Take a look at what we are working with:
head(data)
# Starting with $A6F03 the rest of these columns all represent a measure of loci diversity. 
  #Let's see if this measure of diversity is evenly distributed across the landscape or has a geographic pattern.
#First we will create a linear model with the lm() function.
  # The $UTM.X column is the UTM version of longitude (i.e. east-west range)
  # It follows the same pattern as our plot() function with y~x so:
mod.lon <- lm(data$epi3 ~ data$UTM.X..huso.30.)
  # I've given the object a name that starts with "mod" to remind myself it's a model result
# Now let's see if the "epi3" locus is significantly predicted by the longitude of the population it came from.
  # The anova() function is a more general function that tests the significance of model results:
anova(mod.lon)
  # ...And it's not significant. The p value from the "Pr(>F)" column is 0.3634. 
  # We will talk about other parts of the result later on.

  # You can use the summary() function for a more detailed view of the model results:
summary(mod.lon)
  # The significant intercept is NOT important for the relationship of our variables.
  # There are a bunch of results in this printout. For now notice the second line from the bottom for R-squared.
  # The r-squared is essentially zero, so there is no relationship.

# Maybe there is a pattern by latitude - many species and genes follow predictable north-south patterns with climate.
mod.lat <- lm(data$epi3~data$UTM.Y..huso.30.)
anova(mod.lat)
summary(mod.lat)
  # The overall result is nearly identical. Still no relationship.

# So latitude and longitude individually don't influence the genotype. But what about lat and lon together?
# To look at the interaction between these two values we can use the multiplication symbol -> '*'
mod.latlon <- lm(data$epi3 ~ data$UTM.Y..huso.30. * data$UTM.X..huso.30.)
anova(mod.latlon)
  # Now we have a significant result! So there is a gradient that is the result of a combination between latitude and longitude.
  # The challenge before was likely a confounding effect of latitude hidden in the longitude model and vice versa.
  # R even denotes that it is significant with the '*' after the value for Lat*Lon

summary(mod.latlon)
  # In the summary we can see this relationship is still pretty weak (R-squared is small), but significant because of the large sample size.

# Additionally, if you want to view how sample locations relate to each other, you can use a scatter plot pretending one predicts the other:
plot(data$UTM.Y..huso.30. ~ data$UTM.X..huso.30.)
  # It looks at though many points are clustered tightly with a few far away from the rest.

# What if we want to compare it against factors? Say a regional factor that might group these samples together?

# We still use the same linear model. But the complicated bit is how it deals with factors to compare group means.
  # This post is a GREAT explanation of the math: https://www.r-bloggers.com/the-lm-function-with-categorical-predictors/
  # Think of it as a one-way ANOVA for several groups, and the syntax is the same:
mod.fac <- lm(data$epi3~data$Region)
anova(mod.fac)
summary(mod.fac)
 # Now we can see that a couple very specific regions have wildly different genotype means from the rest.
    # Particularly Soria. These significant regions are what likely drives the significant Lat/Lon relationship.

#You can also visualize groups by color-coding the points according to region.

plot(data$UTM.Y..huso.30. ~ data$UTM.X..huso.30., col = factor(data$Region), pch=19)


#Great, except we don't know which region is which! We need to add a legend to the plot.Just make sure the legend color scheme matches the points.
#Unfortunately if we look at the group names, someone included slashes! Because of this, R thinks they are "escape characters" and will not print a legend
levels(as.factor(data$Region))
#We *can* change these manually...but later this semester we will work through how to change a group of values like this all at once.

#Hmmm, if you look closely you'll also notice that the number of colors is lower than the number of groups! R default colors are very similar after the first few.
#To get around this we'll add shapes based on regions:

plot(data$UTM.Y..huso.30. ~ data$UTM.X..huso.30., col = factor(data$Region), pch= c(11:29)[as.integer(as.factor(data$Region))])


#Feel free to explore this dataset with other comparisons of alleles and regions.
# Next we will test relationships from a package-based dataset in this week's assignment.


