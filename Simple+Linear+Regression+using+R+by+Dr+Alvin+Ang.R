#-------------------------------------------------------------------------------
# Chapter 1: Basic Dataset
#-------------------------------------------------------------------------------

# 1a) Import Dataset and Plot
x = 1:5
y = c(1.3, 4.3, 5.5, 8.4, 14.2)
plot(x, y)

#1b) Fit Linear Model and Summary
m = lm(y~x)
summary(m)

#1c) Predict
predict(m, data.frame(x = 6))

#-------------------------------------------------------------------------------
# Chapter 2: Mtcars
#-------------------------------------------------------------------------------

#2a) Import Dataset, Plot and Fit Linear Model
plot(mpg~wt, data = mtcars)

m = lm(mpg~wt, data = mtcars)

abline(m, col = 'red')

#2b) Getting Coefficients
coef(m)

#2c) Predict
p = predict(m, data.frame(wt=3))
p

#-------------------------------------------------------------------------------
# Chapter 3: Quakes
#-------------------------------------------------------------------------------

#3a) Import Dataset, Fit Linear Model and Plot
x = quakes$stations
y = quakes$mag

m = lm(y~x)

plot(x, y)

abline(m, col = 'red')


#3b) Predict
a = predict(m, data.frame(x = 100))
a

#-------------------------------------------------------------------------------
# Chapter 4: Heart.csv
#-------------------------------------------------------------------------------

#4a) Import Heart Dataset
heart <- read.csv("https://www.alvinang.sg/s/heart.csv", 
                  header=TRUE, sep=",", na.strings = '?')

#4b) Fit Linear Model
age = heart$age
chol = heart$chol

m = lm(chol~age)

#4c) Plot
plot(age, chol)
abline(m)

#4d) Predict
predict(m, data.frame(age = 60))

#-------------------------------------------------------------------------------
# THE END
#-------------------------------------------------------------------------------
