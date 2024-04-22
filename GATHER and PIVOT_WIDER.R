#------------------------------------------------------------------------------
# Simple Example
#------------------------------------------------------------------------------

# Create DataFrame
df = tibble(
  'A' = c(1,2,3),
  'B' = c(4,5,6)
  )

# GATHER
df1 = df %>%
  gather(key = 'col_1', 
         value = 'col_2')

# PIVOT WIDER
df2 = df1 %>%
  pivot_wider(names_from = col_1, 
              values_from = col_2) %>%
  unnest(cols = c(A, B))

#------------------------------------------------------------------------------
# MTCARS Example
#------------------------------------------------------------------------------

#Load df
df = mtcars

#Create New 'Car_Names' Column
df$Car_Names = rownames(df)

#Relocate 'Car_Names' to 1st Column
df <- df %>% 
  relocate(Car_Names, .before = mpg)

#GATHER the 'cyl' column
df1 = df %>% 
  gather(key = "col1", 
         value = "col2", 
         cyl)

#Pivot_Wider and UNNEST back....
df2 = df1 %>%
  pivot_wider(names_from = col1, 
              values_from = col2) %>%
  unnest(cols = c(cyl))


#------------------------------------------------------------------------------
# THE END
#------------------------------------------------------------------------------
