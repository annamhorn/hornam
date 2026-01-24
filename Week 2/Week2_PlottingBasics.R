# Let's start by creating a data frame to match the structure from your homework assignment last week: ####
#The 5 vectors:
unique.char <- c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o')
group.char <- c('x','x','x','x','y','y','y','z','z','z','z','z','z','z','z')
uniqu.num <- c(0,1,1,2,3,5,8,13,21,34,55,89,144,233,377)
rep.num <- c(1,2,3,1,2,3,1,2,3,5,2,3,4,0,5)
dec.num <- c(1,2.1,3.1,1.2,2.3,3.5,1.8,2.13,3.21,5.34,2.55,3.89,4.144,0.233,5.377)
#new stuff
# Combine vectors into a data frame:
df <- as.data.frame(cbind(unique.char,group.char,uniqu.num,rep.num,dec.num))
#Make sure the numeric columns stay numeric:
df$uniqu.num <- as.numeric(as.character(df$uniqu.num))
df$rep.num <- as.numeric(as.character(df$rep.num))
df$dec.num <- as.numeric(as.character(df$dec.num))

# Create a row to add to the data frame:
add.row <- data.frame("p","z",610,4,4.610)

# Match the names of the columns:
colnames(add.row) <- colnames(df)    

# Bind rows:
df1 <- rbind(df, add.row)

# Now to move the row names and delete the column. 
row.names(df1) <- df1$unique.char
df1 <- df1[,-1]
df1


# With our formatted data frame we will now start to visualize the data.

# Histograms ####
# We will start with histograms to get a snapshot of the distributions of our data. 
# As the semester progresses, visualizing and understanding distributions will become more and more important.
# Histograms visualize the frequency that values fall within specific ranges for a single vector.
# So, for a histogram with default settings we simply specify a vector within the hist() function:
hist(df1$dec.num)
  # Note the $ symbol above to specify a single column (vector) within our data frame.
  # This looks like a nice bell curve (or normal distribution) of values.

# To take a closer look let's change the number of 'breaks' or bars created by the plot:
hist(df1$dec.num, breaks = 3)
  # Decreasing the breaks to 3 kept the same pattern, so let's try increasing them to 8:
hist(df1$dec.num, breaks = 8)
  # This pattern does not look as 'clean' anymore. We will get into the why and how of choosing breaks later.
    # For now, keep in mind the parameters you specify can change the story to someone viewing your plots.
      # With great power...you know the rest.



# Boxplots ####
# What if you want to look at distributions for multiple columns at once to compare?
  # This is where box plots can be very useful for comparing value ranges with the simple boxplot() function.
boxplot(df1)
  #...and it gives us an error. This is because factors are 'read' by boxplot() so it is attempting to make 'x', 'y', and 'z' numeric.
    # R used to attempt to "coerce" these values to numeric, but this default has been changed in the most recent version of R.

#So let's remove the character column. Remember how to use brackets:
boxplot(df1[,2:4])
  # Well this tells us how much larger the 'uniqu.num' box is than the others. However, it mushes the other two columns so that we can't see any differences between them.

# Let's just look at the last two (lower value) columns now. 
boxplot(df1[,3:4])
  # Now it is much easier to visually compare the data from these two columns. 
    # RStudio auto-fits the plotting window to minimum and maximum values of the data being plotted.
  # Within each 'box' the bottom line outside the box represents the minimum value and the top line represents the maximum value of the interquartile range.
    # The actual box represents the first and third quartile values and the dark line represents the median.

# Since the unique.num column contains much larger values let's look at it separately in it's own boxplot.
boxplot(df1$uniqu.num)
    # The circles in this box plot represent outliers. These are values outside 1.5 times the interquartile range.
      # Also notice this box plot looks "squished" toward the lower values. Let's see how it looks as a histogram.
hist(df1$uniqu.num)
    # This gives you the same information in a different visual - most values are small with a long 'tail' of large numbers.
      # We will get into distributions in a couple weeks and why this matters later.

# Bar plots ####
# Bar plots are similar to box plots in that they can be used to compare columns or vectors.
  # The key difference is that bar plots compare summary statistics (such as mean and standard deviation) rather than the distribution of all values.
  # This can be useful when several vectors and/or values grouped by factors are involved, in order to simplify the visual.
  # We will continue working with our data frame, but group values by the 'x', 'y', and 'z' factor levels of the group.char column.
# Because we are grouping one column by the factors in another column we'll use the aggregate() function.
# First, take a look at the help documentation for aggregate() this is a powerful function we will use for other purposes later in the semester:
?aggregate
# Now let's generate means for each group based on the factor value for the $rep.num column. 
  # We will also specify the function to be applied is generating mean() values with the "FUN" argument:
df.mean <- aggregate(df1$rep.num ~df1$group.char, FUN = "mean")
df.mean
# Having $ symbols inside of column names can get messy fast, so we will rename these before continuing:
colnames(df.mean) <- c("Factor","Mean")
df.mean

#Now we can plot the mean values by factor:
barplot(df.mean$Mean)

# We are missing an x-axis. This can be specified with the "names.arg" argument within the barplot() function:
barplot(df.mean$Mean, names.arg = df.mean$Factor)
# These mean values appear to follow a gradient increasing as we sing the end of our "A,B,C's". 
  # It would be useful to know how variable the values behind these means are. So let's add error bars to the plot.
  # First we need to use the 'FUN' argument to call the standard deviation, or sd(), function:
df.sd <- aggregate(df1$rep.num ~df1$group.char, FUN = "sd")
# And add column names:
colnames(df.sd) <- c("Factor","StanDev")
df.sd

# Certain plotting functions in R can be layered on top of one another as long as an object from the plot() function has been created first.
  # So to add error bars to our existing plot we will use the arrows() function.
  # First we need to create a plot object in R:
b.plot <- barplot(df.mean$Mean, names.arg = df.mean$Factor)
  
  # Next add the error bars
  # Notice we have specified 'b.plot' as the object to draw arrows on. 
  # We are specifying a minimum and maximum value for the bars by either adding or subtraction the standard deviation from the mean.
  # To create the flat top and bottom of the error bars we use the argument "angle = 90" to specify they are perpendicular to the y-axis. 
  # The "code = 3" argument is used to draw arrows on both ends (not just above or below the mean): 

arrows(b.plot, df.mean$Mean-df.sd$StanDev,
       b.plot, df.mean$Mean+df.sd$StanDev,angle=90,code=3)

# Great! We have a plot with error bars...with one small problem. What happened to the top 'z' error bar?
  # Remember that arrows are drawn on top of the plot, not as part of it. So they are only drawn where they 'fit' the plotting screen.
# To make the original plot larger to accommodate the error bars we need to expand the range of the y-axis with the 'ylim' argument.
# Notice the ylim must be a range - it's not just a maximum or minimum value:
b.plot <- barplot(df.mean$Mean, names.arg = df.mean$Factor, ylim = c(0,5))

# Then redraw the error bars:
arrows(b.plot, df.mean$Mean-df.sd$StanDev,
       b.plot, df.mean$Mean+df.sd$StanDev,angle=90,code=3)

# The values looked different when we first drew this bar graph. Now it seems that might not be the case based on the standard deviation in values.
  # We will get to the statistics to test this in a few weeks.

# There might be times when you will want to display your barplot sideways (e.g. in order to rank means by largest to smallest) for a different visual.
# This can be done through the addition of one very simple argument to the plot - "horiz = TRUE":
barplot(df.mean$Mean, names.arg = df.mean$Factor, ylim = c(0,5), horiz = TRUE)

# Scatter plots ####
# One of the most common plot types in ecology is a scatter plot. These are used to visualize the relationship between two (or more) numeric vectors.
# Typically, the x-axis is used to plot the explanatory variable and the y-axis is used to plot the response variable.
  # For example, a plot of number of buds on a tree relative to temperature would have the buds on the y and temperature on the x.
        #(The temperature can alter the number of buds, but the number of buds cannot change the temperature)
 # To help illustrate this, we will use the 'is approximated by' symbol of '~' in the format y-axis ~ x-axis: 
# In our example data frame, I made up the $dec.num vector by concatenating $rep.num and $uniqu.num. 
  # So it is reasonable to say $dec.num is explained by $rep.num:

plot(df1$dec.num ~ df1$rep.num)

#There appears to be a clear relationship here, as we would expect. Let's see if $dec.num is also explained by $uniqu.num

plot(df1$dec.num ~ df1$uniqu.num)

# There might be a logistic relationship here, but it's not as clear visually. 
  # Again, we will test the significance of these relationships in a few weeks. For now notice the wildly different scales of the x- and y-axis.
  # Scatter plots are also great for visualizing relationships between vectors that can be different orders of magnitude in scale.


# Cleaning up your plots #### 

# Let's add x and y labels to the plot axes. We'll keep using the most recent scatter plot for these.
  # These are created with the arguments "xlab" and "ylab":

plot(df1$dec.num ~ df1$uniqu.num, xlab = "Explanatory", ylab = "Response")
  # Our column names have now been replaced by something coherent to the outside world for axis labels.

# It is not always needed, but in this case let's add a title too with the "main" argument:
plot(df1$dec.num ~ df1$uniqu.num, xlab = "Explanatory", ylab = "Response", main = "My Favorite Scatter Plot")
  # When you want to save and export plots for later, adding a title can be useful for keeping them straight. We can always crop it off later.

  # What if we want the font sizes to be smaller or larger? That can be changed with variations on the "cex" argument. 
    # This is one of the unintuitive arguments in R with no reasonable way to remember it.
  # cex.axis will change the size of the numbers
  # cex.lab will change the size of the text in xlab and ylab
  # cex.main will change the size of the main title.
  # All cex values are specified based on a proportion of the default. So if you want the text twice as large then cex = 2.
  # Let's start with something modest - I want the numbers to be 80% of the default size:
plot(df1$dec.num ~ df1$uniqu.num, xlab = "Explanatory", ylab = "Response", main = "My Favorite Scatter Plot", cex.axis=0.8)

# I'm also going to just crop the title off later, so it can be much smaller:
plot(df1$dec.num ~ df1$uniqu.num, xlab = "Explanatory", ylab = "Response", main = "My Favorite Scatter Plot", cex.axis=0.8, cex.main = 0.5)

# And I want the axis labels to be just 25% larger:
plot(df1$dec.num ~ df1$uniqu.num, xlab = "Explanatory", ylab = "Response", main = "My Favorite Scatter Plot", cex.axis=0.8, cex.main = 0.5, cex.lab = 1.25)

#I've decided my plot will be on a poster where the points need to be visible from far away. 
    # Open circles won't do, so let's use the 'pch' argument to fill them in:
plot(df1$dec.num ~ df1$uniqu.num, xlab = "Explanatory", ylab = "Response", main = "My Favorite Scatter Plot", 
     cex.axis=0.8, cex.main = 0.5, cex.lab = 1.25, pch=19)
    #pch is another argument with no logical origin. To determine which mystery number corresponds to the shape you want use the help file:
?pch
    #A list of symbols and the number code is ~1/3 down the page.

#After looking at the symbol options I think I'm going to use a filled triangle instead:
plot(df1$dec.num ~ df1$uniqu.num, xlab = "Explanatory", ylab = "Response", main = "My Favorite Scatter Plot", 
     cex.axis=0.8, cex.main = 0.5, cex.lab = 1.25, pch=17)

# I'm also going to change the color. I'm going to pretend the data represent dandilions so the color "goldenrod" seems appropriate:
plot(df1$dec.num ~ df1$uniqu.num, xlab = "Explanatory", ylab = "Response", main = "My Favorite Scatter Plot", 
     cex.axis=0.8, cex.main = 0.5, cex.lab = 1.25, pch=17, col = "goldenrod")

# Nevermind! That clearly is not a color that will be obvious from across the room.
  #It might be boring, but at least a dark shade of grey will be visible:
plot(df1$dec.num ~ df1$uniqu.num, xlab = "Explanatory", ylab = "Response", main = "My Favorite Scatter Plot", 
     cex.axis=0.8, cex.main = 0.5, cex.lab = 1.25, pch=17, col = "grey40")
    # A table of R colors and their names is in the Canvas folder for this week.

    # You can also find the names of all 657 generic colors by using the colors() function:
colors()
    # OR by using the demo() function view all of the colors. 
      # To move through the color plots you must move the active cursor to the console window below and hit "enter":
demo("colors")
    # You can go back and forth between these plots with the arrows in the top left corner of the plotting pane.

# It would probably help to make the points a little bigger. For this we go back to 'cex' and use the "global" version.
  # Since we have already specified the size of all fonts, this will only effect the points in the plot:
plot(df1$dec.num ~ df1$uniqu.num, xlab = "Explanatory", ylab = "Response", main = "My Favorite Scatter Plot", 
     cex.axis=0.8, cex.main = 0.5, cex.lab = 1.25, pch=17, col = "grey40", cex = 1.5)

# With our points, font size, labels, and colors all set, let's change the font family to something a little more professional.
# This is done outside of the plot() function by setting the parameters of our working environment with the parameter "par()" function.
# The par() function can do many things for us. Right now we will just focus on changing the font family. For more on par() check out the help file:
?par
# Because the par() function changes settings of our R 'environment' we need to re-create the plot once the default settings are changed:
# First change the font:
par(family = "mono")
#Then replot
plot(df1$dec.num ~ df1$uniqu.num, xlab = "Explanatory", ylab = "Response", main = "My Favorite Scatter Plot", 
     cex.axis=0.8, cex.main = 0.5, cex.lab = 1.25, pch=17, col = "grey40", cex = 1.5)

# OK, this suddenly looks a little too much like it was created on that typewriter you found in your grandparent's basement.
# Try the "serif" family. Always a safe bet in my book:
par(family = "serif")
#Then replot
plot(df1$dec.num ~ df1$uniqu.num, xlab = "Explanatory", ylab = "Response", main = "My Favorite Scatter Plot", 
     cex.axis=0.8, cex.main = 0.5, cex.lab = 1.25, pch=17, col = "grey40", cex = 1.5)
# Much better!

# Exporting Plots ####
# I don't know about you, but I am proud of our little plot. So proud that I want to keep a copy after we close RStudio.
# In order to export plots you can simply click the "export" icon in the bottom-right pane (next to zoom) and follow the prompt.
  # But sometimes we want a slightly different format or higher resolution than the 'export' button can give us.
  # For those special cases we will use one of the functions like jpeg(), svg(), png(), or pdf().
# For this example, we are going to create a pdf of our plot
# Remember from last week that first we need to specify our working directory:
setwd("C:/GitHub/R4Eco_2026") #Don't forget to change this to YOUR working directory!

# Then specify the file names and size in inches.
  # Don't forget the ".pdf" in the file name - R gets confused easily:
  # Notice, I'm also saving this file in the Week2 folder. File paths can be broken up across multiple functions: 
pdf( file = "Week2/MyFavoritePlot.pdf", width = 6, height = 6)

# Notice the pdf() function doesn't actually create the plot? It just creates the space where we will put it. So next replot:
par(family = "serif")
plot(df1$dec.num ~ df1$uniqu.num, xlab = "Explanatory", ylab = "Response", main = "My Favorite Scatter Plot", 
     cex.axis=0.8, cex.main = 0.5, cex.lab = 1.25, pch=17, col = "grey40", cex = 1.5)

# Then we will use the dev.off() function to tell R we are done adding things to the plot and it can write the file:

dev.off()

# Open the folder location, and your plot should be waiting!
# Next we will use the data frame you created last week for some more plotting practice.