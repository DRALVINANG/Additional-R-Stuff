#-------------------------------------------------------------------------------
# Multiple Regression using R by Dr. Alvin Ang
# Mtcars
#-------------------------------------------------------------------------------

# Importing Dataset
data = mtcars

# Fit Linear Model to Mtcars Dataset
m <- lm (mpg ~ wt+hp+disp, data)

# Getting Coefficients
coef(m)

#Prediction
predict(m, data.frame(wt = 2.62, hp = 110, disp = 160))

#-------------------------------------------------------------------------------
# THE END
#-------------------------------------------------------------------------------
