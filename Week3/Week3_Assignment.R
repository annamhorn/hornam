# (1) Approximately how many hours ahead of Sunbury was the peak flow in Lewisburg during the 2011 flood? (2 pt)
#(1) Answer: Sunbury was approx. 7-9 hours ahead of Lewisburg in terms of peak flow.   


# (2) Give one reason why information on the time between peak flow events up- and downstream could be valuable information? (4 pts)
# Flood prep + mitigation: Using previous peak flow events can inform a model of time between peak flow based on distance, helping flood
#emergency services prepare for flood events accordingly.#

# Package scavenger hunt! (12 pts each)


## (3) Using Google and ONLY packages from GitHub or CRAN:
    # Find a package that contains at least one function specifically designed to measure genetic drift.
install.packages("plot3D")
library(plot3D)
install.packages("evolved")
library(evolved)
    # Copy-paste into your script - and run - an example from the reference manual for a function within this package related to a measure of genetic drift. 
        # Depending on the function, either upload a plot of the result or use print() and copy/paste the console output into your script.

##EVOLVED PACKAGE##

N <- 32 #population size
n_alleles <- 2*N 
p_gen0 <- 0.25 #Frequency of allele A1 in the first gen
p_gen1 <- rbinom(1, n_alleles, p_gen0) / n_alleles
p_gen1
## [1] 0.265625
p_gen2 <- rbinom(1, n_alleles, p_gen1) / n_alleles
p_gen2
## [1] 0.3125
p_gen3 <- rbinom(1, n_alleles, p_gen2) / n_alleles
p_gen3
## [1] 0.25
p_gen4 <- rbinom(1, n_alleles, p_gen3) / n_alleles
p_gen4
##[1] 0.28125
p_gen5 <- rbinom(1, n_alleles, p_gen4) / n_alleles
p_gen5
##[1]0.359375
generations <- seq(from = 0, to = 5, by = 1)
p_through_time <- c(p_gen0, p_gen1, p_gen2, p_gen3, p_gen4, p_gen5)
plot(generations, p_through_time, type="l", lwd = 2, col = "darkorchid3",
     ylab = "p", xlab = "generations", las = 1)

print(p_through_time)
#[1] 0.250000 0.234375 0.250000 0.187500 0.281250 0.312500#


#RUN 5 flasks at once#
WFDriftSim(Ne = 32, n.gen = 10, p0 = 0.5, n.sim = 5)?

    # After running the function example, manipulate a parameter within the function to create a new result. 
        # Common options might be allele frequency, population size, fitness level, etc. 
        # Add the results of this manipulation to your script (if in the console) or upload the new plot.

##MANIPULATION##  
  

N <- 32 #population size
n_alleles <- 2*N 
p_gen0 <- 0.75 #Frequnecy of allele A1 in first gen
p_gen1 <- rbinom(1, n_alleles, p_gen0) / n_alleles
p_gen1
## [1] 0.265625
p_gen2 <- rbinom(1, n_alleles, p_gen1) / n_alleles
p_gen2
## [1] 0.3125
p_gen3 <- rbinom(1, n_alleles, p_gen2) / n_alleles
p_gen3
## [1] 0.25
p_gen4 <- rbinom(1, n_alleles, p_gen3) / n_alleles
p_gen4
##[1] 0.28125
p_gen5 <- rbinom(1, n_alleles, p_gen4) / n_alleles
p_gen5
##[1]0.359375
generations <- seq(from = 0, to = 5, by = 1)
p_through_time <- c(p_gen0, p_gen1, p_gen2, p_gen3, p_gen4, p_gen5)
plot(generations, p_through_time, type="l", lwd = 2, col = "darkorchid3",
     ylab = "p", xlab = "generations", las = 1) 
print(p_through_time)
##[1] 0.750000 0.796875 0.828125 0.843750 0.843750 0.828125##

          # By manipulating these parameters you can see how it impacts the results.
          # This type of manipulation is one example of how theoretical ecology and modelling are used to predict patterns in nature.



## (4) Using Google and ONLY packages from GitHub or CRAN:
    # Find a package that will generate standard diversity metrics for community ecology, specifically Simpson's Diversity Index.
    # Copy-paste into your script - and run - an example from the reference manual for a function to calculate Simpson's diversity.

#Install and load vegan package##
install.packages("vegan")
library(vegan)

##Hypothetical Community Data##
species1 <- c(2,4,6,8)
species1
species2 <- c(4,3,7,5)
species2
species3 <- c(7,3,1,0)
species3
community.df <- cbind(species1,species2,species3)

##RUN SDI##
shannon_index <- diversity(community.df, index= "shannon")

##Print Result##
cat("Shannon Diversity Index:", shannon_index)#clever (is this in the example?)
#results: Shannon Diversity Index: 0.9839614 1.0889 0.8982053 0.6662784#



# Depending on the example usage of the function, either upload a plot of the result or use print() and copy/paste the console output into your script.
# After running the function example, modify your script to generate another diversity metric that is NOT part of the example. 
# If there are multiple diversity metrics in the example script, none of these will count as the modified script.
# Hint: If the function can "only" calculate Simpson's diversity, the inverse of Simpson's diversity is another common metric. 
# Add the results of this manipulation to your script (if in the console) or upload the new plot.
  

##Hypothetical Community Data##
species1 <- c(2,4,6,8)
species1
species2 <- c(4,3,7,5)
species2
species3 <- c(7,3,1,0)
species3
community.df <- cbind(species1,species2,species3)

##Run Fisher's Diversity Indices##
fisher_index <- fisher.alpha(community.df)
##Add Simpson, UnbSimpson, and Inv Simpson##
simpson_index <- diversity (community.df, "simpson")
invsimp_index <- diversity(community.df, "inv")
unbiassimp <- simpson.unb(community.df)

##Plot##
pairs(cbind(fisher_index,simpson_index, invsimp_index, unbiassimp), pch="+", col="blue")




# Diversity metrics are frequently used in community ecology for reasons ranging from a quick comparison between sites to understanding community stability.
# Their calculation can be very tedious by hand - and very fast with a package designed for the operation.



