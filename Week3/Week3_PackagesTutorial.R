# This week we will explore the value of packages for improving figures and collecting data.
# First we will walk through installation of packages with the the most popular plotting package - ggplot2
# ggplot2 ####

# For comparison, we'll use the data and figure from last week - reloaded here:
unique.char <- c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o')
group.char <- c('x','x','x','x','y','y','y','z','z','z','z','z','z','z','z')
uniqu.num <- c(0,1,1,2,3,5,8,13,21,34,55,89,144,233,377)
rep.num <- c(1,2,3,1,2,3,1,2,3,5,2,3,4,0,5)
dec.num <- c(1,2.1,3.1,1.2,2.3,3.5,1.8,2.13,3.21,5.34,2.55,3.89,4.144,0.233,5.377)
df <- as.data.frame(cbind(unique.char,group.char,uniqu.num,rep.num,dec.num))
df$uniqu.num <- as.numeric(as.character(df$uniqu.num))
df$rep.num <- as.numeric(as.character(df$rep.num))
df$dec.num <- as.numeric(as.character(df$dec.num))
add.row <- data.frame("p","z",610,4,4.610)
colnames(add.row) <- colnames(df)    
df1 <- rbind(df, add.row)
row.names(df1) <- df1$unique.char
df1 <- df1[,-1]
df1

# And recreate the plot we made with the "base" R function (i.e. not from a package):
par(family = "serif")
#Then replot
plot(df1$dec.num ~ df1$uniqu.num, xlab = "Explanatory", ylab = "Response", main = "My Favorite Scatter Plot", 
     cex.axis=0.8, cex.main = 0.5, cex.lab = 1.25, pch=17, col = "grey40", cex = 1.5)

# To use packages in R they must be installed on your computer. There are two ways to do this.
# (1) - From the "Packages" tab in the bottom right pane, click the "Install" button and type the package name into the dialog box.
# (2) - To install directly from the script (remember RStudio didn't always exist) use the install.packages() function:
install.packages("ggplot2")
  # install.packages requires the package name to be in quotation marks.
#Next you need to tell R to "load" the package using the library() function.
  #It would take too much RAM to run all of you packages all the time, so this allows you to pick and choose based on what you are doing at any point in time.
library(ggplot2)
  # Notice now that ggplot2 is no longer in quotations for the library() function. 
    # Once installed, R recognizes it and it becomes an object instead of a text string.
# Every package also has a reference manual, typically on the CRAN website.
# The ggplo2 reference manual can be found here: https://cran.r-project.org/web/packages/ggplot2/ggplot2.pdf

# From the reference manual you can find the basic plotting function is ggplot():
# Let's try replotting last week's data with ggplot():
ggplot(df1, aes(x=uniqu.num, y=dec.num))
    # SO...where are the data???
    # Unlike base R plotting, ggplot requires EVERYTHING to be specified.
    # Let's try again by adding the points, literally with a +:
ggplot(df1, aes(x=uniqu.num, y=dec.num)) +
  geom_point(size=2, shape=23)
    # Great! Now we have points.


# So far this looks pretty similar to our base plot, so why go to the trouble?
  # All of the additional items you can now easily add to the plot (including analyses) are the power of ggplot:
# Let's add a regression line to these data following a linear model ("lm"):
ggplot(df1, aes(x=uniqu.num, y=dec.num)) +
  geom_point(size=2, shape=23) +
  geom_smooth(method = lm)
  # By adding a single line we were able to include a linear model and confidence interval (grey area) on our regression.
  # This requires several additional lines when using base R plotting methods.
    # Depending on your goal, sometimes a package like ggplot2 will give you the best results, but sometimes base functions are all you need.
    # This just depends on the goal of your figures or analysis and the type of visualization you want to use.
# Next we will explore an example of how R can be used to pull open data from the internet and create interactive plots.

# googleVis ####
# googleVis is a package that - as the name suggests - is support by Google.
  # This packages uses the Google API (application programmer interface) and Java wizardry to create interactive plots.
  # The majority of functions in this package will automatically open a browser window - so be prepared for that.
#First, install the package.
install.packages("googleVis")
library(googleVis)

# Open the reference manual for googleVis: https://cran.r-project.org/web/packages/googleVis/googleVis.pdf
  # You can see it is formatted similarly to ggplot2 - this structure is standard practice for R packages.
  # This particular reference manual has excellent documentation and examples with pre-loaded data.
  # Since we aren't quite sure what the package can do, let's run a few examples to see the results.

# Because the functions are alphabetical, the example data set for Hurricane Andrew is at the top.
  # It includes a mapping example - something we have not done yet so let's see what it looks like:
  # This is a direct copy-paste from the reference manual to explore an example usage.
# Run the following 14 lines and a browser window should open with the result.
data(Andrew)
AndrewGeoMap <- gvisGeoMap(Andrew, locationvar='LatLong', numvar='Speed_kt',
                           hovervar='Category',
                           options=list(width=800,height=400,
                                        region='US', dataMode='Markers'))
AndrewMap <- gvisMap(Andrew, 'LatLong' , 'Tip',
                     options=list(showTip=TRUE, showLine=TRUE,
                                  enableScrollWheel=TRUE,
                                  mapType='hybrid', useMapTypeControl=TRUE,
                                  width=800,height=400))
AndrewTable <- gvisTable(Andrew,options=list(width=800))
# Combine the outputs into one page:
AndrewVis <- gvisMerge(AndrewGeoMap, AndrewMap)
plot(AndrewVis)
# Go to the browser and click 'OK' in the popup dialog box.
  # You have just generated an interactive map of Hurricane Andrew's path!
  # Explore and notice you can click on points for more info. 
  # This seems like a great way to explore spatial data for a lay audience.

# Next is an example dataset called "Cairo" that includes an interactive calendar.
  # That sounds interesting, so let's take a look with the example script:
data(Cairo)
plot(gvisCalendar(Cairo))
  # This gives something of a time-series heatmap, with odd default colors. 
  # It might have value down the road, but we don't have an immediate use for something like this so we'll keep exploring.
  
# The "Cats" dataset and word tree example sound like a completely different figure than we've talked about.
  # Let's see what it does:
data(Cats)
plot(gvisWordTree(Cats, textvar = "Phrase"))
  # If you hover your cursor over a term you can see it's frequency of use. 
  # Following through the chart you can "read" the most common phrases.
  # Clearly this was a biased sample as everyone knows Cats -> are -> evil is the only appropriate response.
  # If you are ever interested in communication techniques, or science communication specifically, this could be a nice figure type to use.

# Annotated Timeline ####
# This particular function caught my eye as many data in ecology are time series-based.
# First, let's run the example:
data(Stock)
A1 <- gvisAnnotatedTimeLine(Stock, datevar="Date",
                            numvar="Value", idvar="Device",
                            titlevar="Title", annotationvar="Annotation",
                            options=list(displayAnnotations=TRUE,
                                         legendPosition='newRow',
                                         width="600px", height="350px")
)
plot(A1)
  # This plot seems like it might be a great way to explore hydrograph (stream flow) data over time.
  # But look in the console - there is a red message that this function has been updated.
  # Try the new gvisAnnotationChart() function instead:
A1 <- gvisAnnotationChart(Stock, datevar="Date",
                            numvar="Value", idvar="Device",
                            titlevar="Title", annotationvar="Annotation",
                            options=list(displayAnnotations=TRUE,
                                         legendPosition='newRow',
                                         width="600px", height="350px")
)
plot(A1)
  # Everything seems to work well and no error messages, so we will stick with this function.
  # Now we can test it out with real data!
  # To do that we need to install another package for the next step.


# Creating a hydrograph ####
# The USGS manages flow gauges across the US that capture discharge rates every 15 minutes.
# USGS scientists have created a package called "dataRetrieval" that will pull data directly from the USGS website for analysis.
  # However, the USGS published their package to GitHub instead of CRAN.
  # Reading packages from GitHub requires the "devtools" package so first:
install.packages("devtools")
library(devtools)
# Then use the install_github() function to download the "dataRetrieval" package from the "USGS-R" GitHub site.
install_github("USGS-R/dataRetrieval")
  # Hit an empty line in the Console to skip updates
library(dataRetrieval)

# The function we need from this package for pulling flow data is called readNWISuv():
    # NWIS stands for National Water Information System
?readNWISuv
 # From this, you can see the function collects data based on the USGS codes for location and date type based on a date range.
#We will look at the 2011 flood on the Susquehanna River in Sunbury compared to several months before and after the flood:
    # The site code for Sunbury is "01554000".
    # This line could take a minute or two depending on your internet connection speed. 
sunbury_flow <- readNWISuv("01554000","00060","2011-01-01","2013-10-10")
#This is a very large file, let's use head() to look at the format:
head(sunbury_flow)
# How big is it? We can check the length with the nrow() function:
nrow(sunbury_flow)
  # 95,000+ rows - not a small table!

# This file has the same format as the example dataset of "Stock".
  # so all we need to do is replace the object name at the beginning of the function, then the column names in their corresponding locations.
flowchart <- gvisAnnotationChart(sunbury_flow, datevar="dateTime",
                            numvar="X_00060_00000", 
                            options=list(displayAnnotations=FALSE,
                                         legendPosition='newRow',
                                         width="600px", height="350px")
)
plot(flowchart)
  # Notice how much longer it takes for this chart to load? And the interactive parts are glitchy?
  # This is A LOT of data compared to the original plot. It's too much for the function to handle effectively.
  # We could create some daily summaries to decrease the resolution of data but keep the same timeframe.
  # Since we are really interested in the flood, let's just narrow the time range instead.

# Notice all I have changed is the date range in the readNWISuv() function:
sunbury_flow <- readNWISuv("01554000","00060","2011-09-01","2011-10-01")
#We'll overwrite the original, and make it wider for clarity:
flowchart <- gvisAnnotationChart(sunbury_flow, datevar="dateTime",
                                   numvar="X_00060_00000", 
                                   options=list(displayAnnotations=FALSE,
                                                legendPosition='newRow',
                                                width="900px", height="400px")
)
plot(flowchart)

#This is MUCH better, but still a little glitchy. 
  # You can zoom in on the plot at see the main flood didn't start until after Sept 6th and was over by Sept 15th - let's narrow even more:
sunbury_flow <- readNWISuv("01554000","00060","2011-09-06","2011-09-15")
#We'll overwrite the original, and make it wider for clarity:
flowchart <- gvisAnnotationChart(sunbury_flow, datevar="dateTime",
                                   numvar="X_00060_00000", 
                                   options=list(displayAnnotations=FALSE,
                                                legendPosition='newRow',
                                                width="900px", height="400px")
)
plot(flowchart)
  # Now the plot is truly interactive and dynamic again.

# Remember the example plot had two data sources - pens and pencils?
  # Let's see how the river in Sunbury compared to a location upstream, like Lewisburg.
# First we need to grab the Lewisburg flow over exactly the same timeframe:
lewisburg_flow <- readNWISuv("01553500","00060","2011-09-06","2011-09-15")

#Then we can use rbind to create a single data frame with both sites:
flow_both <- rbind(sunbury_flow,lewisburg_flow)

#Now we will add back in an argument from the example function - "idvar" will allow us to specify mutiple locations based on the site number:
flowchart <- gvisAnnotationChart(flow_both, datevar="dateTime",
                                 numvar="X_00060_00000", idvar="site_no",
                                 options=list(displayAnnotations=FALSE,
                                              legendPosition='newRow',
                                              width="900px", height="400px")
)
plot(flowchart)


# Look at the new plot in your browser and zoom in on the peak flow.
  # You can see the West Branch Susquehanna is both lower flow and peaked earlier in time than the main river in Sunbury.
  # This makes sense since Lewisburg is upstream, but knowing how long it takes for the peak to travel can be valuable information.

#Remember the basics of how to use c() for calling multiple objects at once? Let's see if we can apply that to the readNWISuv() function:

flow_3 <- readNWISuv(c("01553500","01554000", "01552000"),"00060","2011-09-06","2011-09-15")


#Now we will overwrite and use "idvar" to specify three locations based on the site number:
flowchart <- gvisAnnotationChart(flow_3, datevar="dateTime",
                                 numvar="X_00060_00000", idvar="site_no",
                                 options=list(displayAnnotations=FALSE,
                                              legendPosition='newRow',
                                              width="900px", height="400px")
)
plot(flowchart)


#Notice the third line is smaller, peaks sooner, and is broken? Where do you think it's from?





# Now it's time to do a little package scavenger hunt of your own!


