#-------------------------------------------------------------------------------  
# Data Wrangling using Core R by Dr. Alvin Ang
#-------------------------------------------------------------------------------  
#1. Check Current Working Directory

getwd()

#-------------------------------------------------------------------------------  
#2. Wrangling Weather.csv

#2a. Import CSV
#https://www.alvinang.sg/s/weather.csv 
weather = read.csv('weather.csv',header = TRUE)

#2b.  Slice Out Column using Subset
#Slice out the "Ozone" column
weather.mayOzone <- 
  subset(weather, select=Ozone, subset = Month==5)

#2c. Check which Rows have NAs
m = !is.na(weather.mayOzone)

#2d.  Compute the average Ozone level in the month of May
mean(weather.mayOzone[m])

#2e.  Filter Out all NAs in the month of May
a = weather.mayOzone[m]

#2f. Output as CSV
write.csv(a,'may_weather_data.csv')

#-------------------------------------------------------------------------------  
#3. Wrangling Mtcars 

#3a. Slicing Out mpg / am / wt columns
b = mtcars[c('mpg','am','wt')]

#3b. Viewing Heads and Tails
head(b,7)
tail(b,3)

#3c. Slicing Out mpg / hp columns
c = subset(mtcars, select=c(mpg,hp))

#3d. Output as CSV
write.csv(c,"mtcarssubset.csv")

#3e. Filter all the mpg>15 and am=1
d = mtcars[mtcars$mpg>15 & mtcars$am==1,]

#3f. Filter Out only mpg and am columns with am=1 (automatic) 
e = subset(mtcars, 
           select=c('mpg','am'), 
           subset=am==1)

#3g. Summary of mtcars subset
summary(e)


#3h. Create a Table from Mtcars AM columns
factor = factor(mtcars$am)
table(factor)

#-------------------------------------------------------------------------------  
#THE END
#-------------------------------------------------------------------------------  




