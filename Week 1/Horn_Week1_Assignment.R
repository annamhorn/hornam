# Now it is time to create your own data frame using the tools we have learned this week.
# First, resave this script as: yourlastname_Week1_Assignment [6 point]
  # e.g. mine would be Wilson_Week1_Assignment
#Somehow didn't save the file with a ".R" extension. Followed the naming convention though (so no points lost)

# Create 3 numeric vectors and 2 character vectors that are each 15 values in length with the following structures: [10 points; 2 each]
  # One character vector with all unique values
  # One character vector with exactly 3 unique values
  # One numeric vector with all unique values
  # One numeric vector with some repeated values (number of your choosing)
  # One numeric vector with some decimal values (of your choosing)

#Creating Vectors#

#Use "c" function and same number of values for each vector
# Remember to use "" when using text 
  
a <- c('a','ab','ac','ad','b','ba','bb','bc','bd','c','ca','cb','cc','cd','d') #This hurt my brain to make sure they were unique.
a
b <- c("Red","Blue","Green","Red","Red","Red","Blue","Blue","Green","Blue","Green","Red","Green","Green","Blue")
b
c <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
c
d <- c(1,2,3,4,4,5,6,7,8,8,8,9,10,11,12)
d
e <- c(1.1,1.31,1.7,1.82,1.90,1.33,1.29,1.56,1.0,1.77,1.26,1.9,1.40,1.11,1.03)
e

#Create and Organize Data Frame#
  
# Bind the vectors into a single data frame, rename the columns, and make the character vector with unique values the row names.[3 points]

data <- cbind(a,b,c,d,e)
data

df <- as.data.frame(data)
df

#Naming columns and designating rows with a unique (meaningful) value sequence#

colnames(df) <- c("Code","Color","Number","Value","Grams")
df

row.names(df) <- df$Code
df

#Adding and Removing Data from the Data Frame#

# Remove the character vector with unique values from the data frame.[2 points]

df <- df[,-1]
df

# Add 1 row with unique numeric values to the data frame.[2 points]
#Breakdown of steps:
#1:Make new data frame consisting of new row
#2:Make the column names of new df to be the same as old df
#3:Combine using rbind 
#4 Align rows/columns

#f <- data.frame("Green",16,12,1.90)
Years <- data.frame(16,12,1.90)#I had to do some jenky things to make this code run - were lines out of order?

colnames(Years) <- colnames(df) #years doesn't exist. Is this supposed to be "f"?
df.b <- rbind(df,Years)
df.b

row.names(df.b) <- c(row.names(df[1:3]),"Years")
df.b
df.b$Value <- as.numeric(df.b$"Value")#no need for the quotes - can actually confuse things down the road.
df.b$Number <- as.numeric(df.b$"Number")
df.b$Grams <- as.numeric(df.b$"Grams")
df.b

# Export the data frame as a .csv file [2 points]

setwd("C:/GitHub/hornam/Week1")
getwd()
write.csv(df.b, file="HW1df.csv")

# Generate summary statistics of your data frame and copy them as text into your script under a new section heading. [2 points]
summary(df.b)

#Summary Statistics of Data Frame B#
"Color               Number          Value            Grams      
Length:16          Min.   : 1.00   Min.   : 1.000   Min.   :1.000  
Class :character   1st Qu.: 4.75   1st Qu.: 4.000   1st Qu.:1.222  
Mode  :character   Median : 8.50   Median : 7.500   Median :1.365  
Mean   : 8.50   Mean   : 6.875   Mean   :1.461  #shifted to the left. Not a big deal, just something to pay attention to with copy/paste.
3rd Qu.:12.25   3rd Qu.: 9.250   3rd Qu.:1.782  
Max.   :16.00   Max.   :12.000   Max.   :1.900" 
> 

# Push your script and your .csv file to GitHub in a new "Week1" folder you have created in your repository. [3 points]


