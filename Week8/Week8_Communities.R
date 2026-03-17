# First things first, load the vegan library.
  # Any time you are looking at community ecology, the package is probably going to be vegan.
library(vegan)
# We will use the "dune" dataset from vegan.
  # the dune data are plant communities from dune meadows.
data(dune)
# There is also a data frame of environmental factors that match the dune plants that we will use this week:
data(dune.env)

# We are focusing on Redundancy Analysis (RDA).
  # But all of these methods can be transferred to other multivariate techniques very easily.
  # pca() and cca() can typically just replace the rda() in any of these functions and models.
# The generic use of rda is very straightforward. All you need is a numeric species-by-site matrix.
dune #to see what I mean by a species-by-site matrix
ord <- rda(dune)
plot(ord)

# This plot shows samples as numbers and species in red text.
  # Species "driving" the relationship will be the furthest from the origin.

# The real power of RDA is the ability to explain variance via a model.
  # We'll use the dune.env data and take a quick look first to see type of data are included:
head(dune.env)
# We have moisture, management method, agricultural use, and manure application.

# Agriculture use typically impacts plant communities, so we will start there.
  # This should look very familiar to the model syntax we have used so far.
mod1 <- rda(dune ~ Use, dune.env)
mod1
  #The model call gives us constrained and unconstrained variance
    # Constrained is the percent of the axes that is explained by "Use"
    # Unconstrained is similar to the residuals of a linear model.
# We can also call the anova() function the same way that you would for a linear model:
anova(mod1)
  # This relationship is not significant.

# Moisture is another common driver of plant community composition
mod2 <- rda(dune ~ Moisture, dune.env)
mod2
anova(mod2)
  # Moisture is a strong driver of community composition at sites.
  # It explains ~33% of variance and is significant.

# Just like other models, we can include additive or interactive effects as well:
  # We can see what the cumulative effect of management and soil moisture is on the plant community:
mod3 <- rda(dune ~ Moisture + Management, dune.env)
mod3
anova(mod3)
  # Just like in linear models, additional X's can improve the model.
  # We have gone from 33% to 55% of the variance explained.

#Let's plot to see what this looks like visually:
plot(mod3)
#This can be intimidating at first.
  # Red terms are the species values
  # Numbers are site values
  # Blue arrows are our predictor variables and how they line up with our groups of points.

# Because of the amount of information, ordination plots are best done in layers, like adding regression lines.
# First create a blank plot where the axes are auto-fitted to the min and max values of "mod3"
plot(mod3, type="n", display = c("sites", "scores"))
  # Next we will add a layer where the points are displayed with a label that represents their management
text(mod3, display="sites", labels = as.character(dune.env$Management))

  # It looks like the same management types are grouping together.
  # We would expect this based on our high variance explained and significant p-value.
  # But statistically viewing this relationship can be difficult.
  # We will draw 95% confidence intervals around the "centroid" (average score) for each management group.
  # This is done with the ordiellipse() function. Notice there is an argument for the confidence interval and the grouping:
pl <- ordiellipse(mod3, dune.env$Management, kind="se", conf=0.95, lwd=2, draw = "polygon", 
                  col="skyblue", border = "blue")
summary(pl)
  # The summary gives use the centroid point for each grouping and the "area" of the ellipse
    # A larger area means a lower confidence on the location of the centroid.
      # In our case the "NM" management technique is clearly different from the others,
        # and we are most confident in the centroid for it.
# Remember too that axes are independent of one another. So "SF" and "NM" management are identical on Axis 1 patterns, 
    # but very different on Axis 2 patterns.

# We can also group data by a factor that was not in the model to identify more complex patterns:
  # We can see if the "Use" influences the communities that are already constrained by "Management":
plot(mod3, type="n", display = "sites")
text(mod3, display="sites", labels = as.character(dune.env$Use))
pl <- ordiellipse(mod3, dune.env$Use, kind="se", conf=0.95, lwd=2, draw = "polygon", 
                  col="skyblue", border = "blue")
summary(pl)


# This pattern is a bit harder to interpret. 
  # It looks like "Hayfields" and "Haypastu" *might* have different communities along Axis 2, but not significant with the data we have available.

# We can try plotting against a different axis to see if this is simply a weaker pattern obscured by Axis 1
# This requires a little data manipulation as plot() does not like to use other axes by default.
# First we need to extract the axis scores from the model
  # These are found in the following two parts of the model:
Site_Scores <- mod3$CCA$u
Species_Scores <- mod3$CCA$v

head(Site_Scores)
  #notice we now have 6 axes. That is because the original model created 6 axes that were able to constrain (explain) the data.

#We'll just focus on the site scores and plot Axis 2 against Axis 3:

plot(Site_Scores[,2:3], type="n")
text(Site_Scores[,2:3], labels = as.character(dune.env$Use))
pl <- ordiellipse(Site_Scores[,2:3], dune.env$Use, kind="se", conf=0.95, lwd=2, draw = "polygon", 
                  col="skyblue", border = "blue")
# Clearly this was not the right answer. The relationship only exists along Axis 2 and is relatively weak.
  # Feel free to test additional axes, but remember each subsequent axis has a weaker explanatory value.
    # If there is a pattern that only exists between Axis 5 and Axis 6 it might not be real or meaningful.

# Rarefaction ####
# The built-in function from vegan does not appropriately bootstrap the subsamples.
# We will use the stand-along function from Jenna Jacobs at: https://github.com/jbaumann3/Rarefaction_in_R

rarefaction<-function(x,subsample=5, plot=TRUE, color=TRUE, error=FALSE, legend=TRUE, symbol=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)){
  
  library(vegan)
  
  
  
  x <- as.matrix(x)
  y1<-apply(x, 1, sum)
  rare.data<-x                                   
  
  select<-unique(sort(c((apply(x, 1, sum)), (seq(0,(max(y1)), by=subsample)), recursive=TRUE)))
  
  
  storesummary.e<-matrix(data=NA, ncol=length(rare.data[,1]),nrow=length(select))
  rownames(storesummary.e)<-c(select)
  colnames(storesummary.e)<-rownames(x)
  storesummary.se<-matrix(data=NA, ncol=length(rare.data[,1]),nrow=length(select))
  rownames(storesummary.se)<-c(select)
  colnames(storesummary.se)<-rownames(x)
  
  
  
  
  for(i in 1:length(select))                      #the for loop
  {
    select.c<-select[i]                     #assigns the 'i'th element of select to select.c
    foo<-rarefy(x,select.c, se=T)           #use whatever vegan fn you want
    
    
    
    storesummary.e[i,]<-foo[1,]
    storesummary.se[i,]<-foo[2,]            
    
  }
  
  storesummary.e<-as.data.frame(storesummary.e)               
  richness.error<<-storesummary.se
  
  for (i in 1:(length(storesummary.e)))
  {
    storesummary.e[,i]<-ifelse(select>sum(x[i,]), NA, storesummary.e[,i])
  }
  
  
  
  ###############plot result################################
  if (plot==TRUE)
  {
    if(color==TRUE){
      plot(select,storesummary.e[,1], xlab="Individuals in Subsample", 
           xlim=c(0,max(select)), ylim=c(0, 5+(max(storesummary.e[,1:(length(storesummary.e))], na.rm=TRUE))),
           ylab="Mean Species Richness", pch =16, col=2, type="n")
      
      for (j in 1:(length(storesummary.e))){
        points(select, storesummary.e[,j], pch=16, col=j+1, type="b", lty=1)}
      
      if(error==TRUE){
        for (m in 1:(length(storesummary.e))){
          segments(select, storesummary.e[,m]+storesummary.se[,m],select, storesummary.e[,m]-storesummary.se[,m])
        }
      }
      if (legend==TRUE){
        legend("bottomright", colnames(storesummary.e), inset=0.05, lty=1, col=1:length(storesummary.e)+1, lwd=2)
      }
    }
    else
    {
      plot(select,storesummary.e[,1], xlab="Individuals in Subsample", 
           xlim=c(0,max(select)), ylim=c(0, 5+(max(storesummary.e[,1:(length(storesummary.e))], na.rm=TRUE))),
           ylab="Mean Species Richness", pch =16, col=2, type="n")
      
      for (j in 1:(length(storesummary.e))){
        points(select, storesummary.e[,j], type="l", lty=1)}
      
      for (k in 1:(length(storesummary.e))){
        symbol<-ifelse(symbol<length(storesummary.e),rep(symbol,2),symbol)
        points(as.numeric(rownames(subset(storesummary.e, storesummary.e[,k]==max(storesummary.e[,k],na.rm=TRUE)))), max(storesummary.e[,k],na.rm=TRUE), pch=symbol[k], cex=1.5)}
      
      if(error==TRUE){
        for (m in 1:(length(storesummary.e))){
          points(select, storesummary.e[,m]+storesummary.se[,m], type="l", lty=2)
          points(select, storesummary.e[,m]-storesummary.se[,m], type="l", lty=2)}}
      
      k<-1:(length(storesummary.e))
      if (legend==TRUE){
        legend("bottomright", colnames(storesummary.e), pch=symbol[k], inset=0.05, cex=1.3)
      }
    }
  }
  print("rarefaction by J. Jacobs, last update April 17, 2009")
  if(error==TRUE)(print("errors around lines are the se of the iterations, not true se of the means")  )     
  list("richness"= storesummary.e, "SE"=richness.error, "subsample"=select)        
  
}


# This function will subsample a single sample and add new taxa to the cumulative richness curve as 
  # they are identified in a subsample.

# We are using the "BCI" data from vegan for the example:
data(BCI)
# First take a look at the data:
head(BCI)
# These data have species in each column and samples by row. This is the right format for our function. 

rarefaction(BCI, subsample=50, plot=TRUE, color=TRUE, error=FALSE,  legend=TRUE, symbol)
  # This plot is very hard to read with this many samples.
    # The numbers in the legend represent individual samples, and there are differences between samples, 
      # but this is generally not that useful for us to plot individual samples.

# Because we often want to know if our sampling was enough to collect all (or most) of the taxa in a community,
  # we will condense all of our samples into a collective sum by species.
#This requires a quick reformatting of our data.
  # In the following line:
    # BCI is being transposed so that each row is a species. 
    # The rowSums() creates a single vector of the total individuals found from each species across all species.
    # But because species need to be columns we then transpose the data back, and make sure it is read as.data.frame()

samples <- as.data.frame(t(rowSums(t(BCI))))

rarefaction(samples, subsample=500, plot=TRUE, color=TRUE, error=FALSE,  legend=TRUE, symbol)
  # Two things should immediately jump out.
    # 1 - the scale of both x and y is much larger.
    # 2 - the curve also appears much steeper.
  # While the curve really is steeper the difference is not quite as dramatic as it appears, consider the much larger range of the x-axis.

#By collecting samples of 500 organisms, it looks like we will collect the majority of species after 3 samples,
  # But what if the goals is 90% or 95%? This curve has not flattened at the top.
  # This becomes a harder question the higher the percent of the community to want to effectively sample,
    # And the higher the investment per each new species found.

  # What if you want to collect just a few larger samples?
rarefaction(samples, subsample=5000, plot=TRUE, color=TRUE, error=FALSE,  legend=TRUE, symbol)
  # Now the first sample almost gets you to the top and the dramatic change in return on investment is even more clear.
  # But we have lost a lot of resolution in terms of where exactly the effort vs species curve begins to level off.

# How to apply rarefaction to real data all depends on the question you have relative to diversity and/or sampling effort.




