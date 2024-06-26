#------------------------------------------------------------------------------
#Step 1: Load Data into df
#------------------------------------------------------------------------------
df = mtcars

#------------------------------------------------------------------------------
#Step 2: Create a New Test Column to Store the Row Names
#------------------------------------------------------------------------------
df$test = rownames(df)

#------------------------------------------------------------------------------
#Step 3: Reset the Index Column
#------------------------------------------------------------------------------
row.names(df) = NULL

#------------------------------------------------------------------------------
#Step 4: Relocate the ‘test’ Column to the First Column
#........Previously it was all the way at the last column…
#------------------------------------------------------------------------------
library(dplyr)

df <- df %>% 
  relocate(test, .before = mpg)

#------------------------------------------------------------------------------
#Step 5: Set 'test' as the Index
#------------------------------------------------------------------------------
row.names(df) = df$test


#------------------------------------------------------------------------------
#Step 6: Delete the ‘test’ Column Away
#------------------------------------------------------------------------------
df = subset(df, 
            select = -c(test))

#------------------------------------------------------------------------------
#THE END
#------------------------------------------------------------------------------
