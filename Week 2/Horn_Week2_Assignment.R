# With the data frame you created last week you will:


#Last Week Data Frame# 
a <- c('a','ab','ac','ad','b','ba','bb','bc','bd','c','ca','cb','cc','cd','d','f')
a
b <- c("Red","Blue","Green","Red","Red","Red","Blue","Blue","Green","Blue","Green","Red","Green","Green","Blue","Green")
b
c <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
c
d <- c(1,2,3,4,4,5,6,7,8,8,8,9,10,11,12,12)
d
e <- c(1.1,1.31,1.7,1.82,1.90,1.33,1.29,1.56,1.0,1.77,1.26,1.9,1.40,1.11,1.03,1.90)
e

data <- cbind(a,b,c,d,e)
data 

df <- as.data.frame(data)
df

colnames(df) <- c("Code","Color","Number","Value","Grams")
df

row.names(df) <- df$Code
df

df <- df[,-1]
df

df$Value <- as.numeric(df$"Value")
df$Grams <- as.numeric(df$"Grams")
df$Number <- as.numeric(df$"Number")
df

summary(df)

# Create a barplot for one numeric column, grouped by the character vector with 3 unique values (10 points)
  # Add error bars with mean and standard deviation to the plot
  # Change the x and y labels and add a title
  # Export the plot as a PDF that is 4 inches wide and 7 inches tall.

#Creating the barplot#
#Step 1 - Use aggregate function to generate on object with grouped variables and 
#designate the function, mean#
df.barplot <- aggregate(df$Grams ~df$Color, FUN= "mean")
df.barplot

colnames(df.barplot) <- c("Color","Mean")
df.barplot

#Use barplot function to create a bar plot from the object and name x.axis with names.arg#
barplot(df.barplot$Mean)
barplot(df.barplot$Mean, names.arg = df.barplot$Color)

#Use aggregate and std dev function to create an object for the standard deviation values that will be nested in the plot#
df.sd <- aggregate(df$Grams ~df$Color, FUN = "sd")
colnames(df.sd) <- c("Color", "StanDev")
df.sd


b.plot <- barplot(df.barplot$Mean, names.arg = df.barplot$Color)

# Arrows function creates error bars#
arrows(b.plot, df.barplot$Mean-df.sd$StanDev,
       b.plot, df.barplot$Mean+df.sd$StanDev, angle=90, code=3)

b.plot <- barplot(df.barplot$Mean, names.arg = df.barplot$Color, ylim = c(0,2.5))

arrows(b.plot, df.barplot$Mean-df.sd$StanDev,
             b.plot, df.barplot$Mean+df.sd$StanDev, angle=90, code=3)

barplot(df.barplot$Mean, names.arg = df.barplot$Color, ylim = c(0,2.5), xlab = "Species", ylab = "Response Time", main = "Reponse Times of Fruit Loops to Milk", col = "darkviolet")

setwd("C:/GitHub/hornam")
pdf( file = "Week 2/Horn_barplot.pdf", width =4, height = 7)

barplot(df.barplot$Mean, names.arg = df.barplot$Color, ylim = c(0,2.5), xlab = "Species", ylab = "Response Time", main = "Reponse Times of Fruit Loops to Milk", col = "darkviolet")
arrows(b.plot, df.barplot$Mean-df.sd$StanDev,
       b.plot, df.barplot$Mean+df.sd$StanDev, angle=90, code=3)
dev.off()

# Create a scatter plot between two of your numeric columns. (10 points)
  # Change the point shape and color to something NOT used in the example.
  # Change the x and y labels and add a title
  # Export the plot as a JPEG by using the "Export" button in the plotting pane.

?pch
plot(df$Value ~ df$Number, pch=16, col= "darksalmon", xlab = "Salmon ID", ylab = "Jump Height (cm)", main = "Salmon Jump Height")
par(family = "serif")

# Upload both plots with the script used to create them to GitHub. (5 points)
  # Follow the same file naming format as last week for the script.
  # Name plots as Lastname_barplot or Lastname_scatterplot. Save them to your "Week2" folder. (5 points)
