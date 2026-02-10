# Look at the plot and model results for our Dryad data in the tutorial. 
  # Part 1: Without knowing which points represent which groups,give one explanation for why these data might be difficult
  # to draw spatial inferences about genes.(4 points)


# It is difficult to draw spatial inferences about genes from this data because there are large samples from multiple regions being further organized. While the means grouped
#by lat/long is significant, it is difficult to look at this visually when the data is so complex. Putting it into a linear model,
#significance may be found even when there truly is none. Essentially, there is a degree of uncertainty with complex
#spatial data that can make it difficult to interpret, especially by space rather than time. 

# Part 2: Despite the drawbacks, give the result or interpretation that you feel most confident in (4 points), and EXPLAIN WHY (6 points).

##I believe that the regions with means scattered all over the place had more spatial variability compared to the regions in clusters.
#I believe this because we are only seeing the regions organized by lat long, and yet points from the same region 
#are scattered in some instances. Meaning, there is something hidden variable causing these regions to be scattered that is not 
#included in our calculations. 

# For your scripting assignment we will use the "ge_data" data frame found in the "stability" package.
  # Install the "stability" package, load it into your R environment, and use the data() function to load the "ge_data". (2 points)

install.packages("stability")
library(stability)
data("ge_data")
d <- ge_data

# Create two linear models for Yield Response: one related to the Environment and one to the Genotype. (2 points each)
  # 'Yield Response' in this dataset is a measure of phenotype expression.
  # Hint: Look at the help file for this dataset.
# Test the significance of both models and look at the model summary. (4 points each)
?ge_data
head(d)

#Breakdown of DATA#
#Gen=Genotype, Rep=Replicate, Block=Block, Yield=Yield Response#

#Linear Model#
#Yield x Env#
mod.env <- lm(d$Yield ~ d$Env)
anova(mod.env)
summary(mod.env)
#results:
#p = 2.2e-16, R2 = 0.4315#
#summary - low std. error, lots of diff values from the mean#

#Yield x Genotype#
mod.gen <- lm(d$Yield ~ d$Gen)
anova(mod.gen)
summary(mod.gen)
#results:
#p = 1.417e-11, R2 = 0.07706#
#summary - high/uniform std. error, less sig values#


  # Which model is a better fit to explain the yield response(2 pts), and WHY? (4 points)
  # Hint: Does one model seem more likely to be over-fitted?

#I think environment would be the better fit than generation. I think this because the environment has a lower std. error,
#consistently lower p-values, and a higher R^2 value than the generation. 

# Which environment would be your very WORST choice for generating a strong yield response? (2 points)

#The Sargodha environment would be the worst choice for generating a strong yield response, indicated by 
#its p-value#

