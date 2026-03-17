# Load the vegan package and the "SR_Inverts" csv from GitHub.
# These data are benthic macroinvertebrate samples from the West Branch Susquehanna River and White Deer Hole Creek collected in 2012.
# Sample codes refer to a riffle number for the West Branch, with smaller numbers upstream (R5, R6, etc.).
# White Deer Hole samples are abbreviated as WDH and grouped together.

library(vegan)
setwd("C:/GitHub/hornam/Week8")
df.SRinverts <- read.csv("SR_Inverts.csv")

#Data prep (10 points):
# These data are setup in a common structure for stream ecology with additional taxonomic information. 
# In our case, the file also contains the Order and Family names for each genus of aquatic insects. 
# We need to remove these for analysis. Subset the data with brackets and a negative symbol to remove the Order and Family info.
df.SRinverts <- df.SRinverts[,-c(1,2)]
#Now transpose the data - remember species need to be columns and sites need to be rows for analysis:
df.SRinverts <- t(df.SRinverts)
# Use these two lines to turn your first row into column names and then remove the first row.
  # This assumes your data frame is named "df". You are welcome to change that.
colnames(df.SRinverts) <- df.SRinverts[1,]
df.SRinverts <- df.SRinverts[-1,]
# You should now have a data frame with samples as row names and species as column names.
    # The first column should be the riffle where these samples were collected.

#Data analysis (10 points):
# It is common in community ecology to ask questions about "clustering" of communities spatially (i.e. are closer samples more similar?).
# Now test if "riffle" is a significant predictor of the macroinvertebrate community.  
  # Report your p-value and constrained variance for the model.
  # Plot Axis 1 and Axis 2 of the results with 95% confidence intervals around the riffles.
    # Hint: it will make things easier if you create two separate data frames. One with the Riffle names and one with the bugs.

riff <- df.SRinverts[,1]
bugs <- as.data.frame(df.SRinverts[,-1])
bugs <- sapply(bugs, as.numeric )


?rda
mod1 <- rda(bugs ~ riff)
mod1
anova(mod1)
plot(mod1, type= "n", display = "sites")
text(mod1,display ="sites", labels = as.character(riff))
pl <- ordiellipse(mod1, riff, kind="se", conf=0.95, lwd=2, draw = "polygon", 
                  col="skyblue", border = "blue")
#p - value: .002,sig relationship#
#constrained variance = 1.364e+04#

# If your code results in an "error: 'x' must be numeric" Then run this line of code to force all bugs to numeric
  # Assuming your data frame of macroinvertebrates is called "bugs".

bugs <- sapply(bugs, as.numeric )

# (Q1) - Which group of samples is clearly different along Axis 1? Does this make sense based on what you know about the data? (3 pts)

#The R5 and R6 sites seem to be the most out of the cluster. We know these sites are further upstream. 
#That is important because upstream sites tend to differ from downstream sites on the Susuquehanna in the 
#sense that they tend to be more forested compared to more agriculturally developed 
#downstream sites#

# Use the rarefaction function from the tutorial to plot 250-individual subsamples grouped and summed by the riffle where they were collected.
  # Hint: use the subset() function to select only the samples from a specific riffle
  # Hint 2: use two equal signs, not one, in the subset() function.
?subset
#Breakdown of code#
#made df a data frame because it was acting weird and the class was a matrix#
#grouping - used subset() to pull specefic riffles and their species data from the big df#
#summing - took our subsetted df and summed the columns. the [,-1] just gets rid of the riffle column
#important to do because that data is not numeric and cannot become numeric#
#pooled it all together with rbind#

df.SRinverts <- as.data.frame(df.SRinverts)

#?sapply - applies a function over a list or vector#

df.WDH <- subset(df.SRinverts, df.SRinverts$Riffle == "WDH")
WDH.sum <- colSums(sapply(df.WDH[,-1], as.numeric))

df.R5 <- subset(df.SRinverts, df.SRinverts$Riffle == "R5")
R5.sum <- colSums(sapply(df.WDH[,-1], as.numeric))

df.R6 <- subset(df.SRinverts, df.SRinverts$Riffle == "R6")
R6.sum <- colSums(sapply(df.R6[,-1], as.numeric))
  
df.R7 <- subset(df.SRinverts, df.SRinverts$Riffle == "R7")
R7.sum <- colSums(sapply(df.R7[,-1], as.numeric))

df.R9 <- subset(df.SRinverts, df.SRinverts$Riffle == "R9")
R9.sum <- colSums(sapply(df.R9[,-1], as.numeric))

pool.riff <- rbind(WDH.sum, R5.sum, R6.sum, R7.sum, R9.sum)

#rarefaction function#

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

rarefaction(pool.riff, subsample=250, plot=TRUE, color=TRUE, error=FALSE,  legend=TRUE, symbol)

# (Q2) - Which riffle took the most effort to effectively sample? (2 pts)
    # Hint: if you use rbind() to bring your summed riffles together it will be easier to display in a single rarefaction plot.

## R6 - indicated by longest curve. ##

# (Q3) - Do you think the differences between riffles are ecologically meaningful? (3 pts)
    # Hint: It might help to look at 800-individual subsamples to answer this question.

rarefaction(pool.riff, subsample=800, plot=TRUE, color=TRUE, error=FALSE,  legend=TRUE, symbol)

#Yes, it looks like inc sample number does not change the curves or their length/slope. In application, Riffle 6
#may have the most rare species or species richness, something along those lines. 

# (Q4) - Why do the curves stop at different locations on the x-Axis? (2 pts)

#I assume they stop because they've reached the point of the asymptote, when more samples no longer changes 
#the species richness, or sampling more is no longer effective#

