#-------------------------------------------------------------------------------
#Data Wrangling with Tidyverse by Dr Alvin Ang
#-------------------------------------------------------------------------------
#1. Install Tidyverse and Load Libraries 

install.packages("tidyverse", dependencies = TRUE)

library(tidyverse)
library(tibble)
library(tidyr)
library(dplyr)
library(readxl)
library(ggplot2)
library(lubridate)

#-------------------------------------------------------------------------------
#2. Reading in the Dengue.csv
dengue <- read_csv("dengue.csv")

#file is here: https://www.alvinang.sg/s/dengue.csv 

#or if you want to read in .xls
# dengue_xls <- read_excel("dengue.xlsx")

#-------------------------------------------------------------------------------
#3. Selecting Columns 

# Select 'year' and 'number' columns from dengue.csv
a = dengue %>%
  select(year,number)

#-------------------------------------------------------------------------------
#4. Filter data 

#Filter out 'year' == 2018 from dengue.csv
b = dengue %>%
  filter(year==2018)

#-------------------------------------------------------------------------------
#5. Filter data based on multiple conditions

#5a. Filter out 2017 and 208  
c = dengue %>%
  filter(year==2017 | year==2018 )

#5b. Filter out 2018 and 'Dengue' type
d = dengue %>%
  filter(year==2018,type_dengue=='Dengue' )

#5c. Another way to Filter out 2018 and 'Dengue' type
e = dengue %>%
  filter(year==2018) %>%
  filter(type_dengue=='Dengue')

#-------------------------------------------------------------------------------
#6. Handling Missing values in Dengue.csv Dataset

#6a. Show All NAs in "number" column
f = dengue %>%
  filter(is.na(number))

#6b. Another way of showing all NAs in all columns
g = dengue %>%
  filter(!complete.cases(.))

#6c. Showing All NO NAs (filled columns) now....
h = dengue %>%
  filter(complete.cases(.))

#-------------------------------------------------------------------------------
#7. Mutate data

#using "eweek" to create a new column called "date"...
i = dengue %>%
  mutate(date = ymd(paste0(year,"-01-01"))+weeks(eweek))

#-------------------------------------------------------------------------------
#8. Filter, Mutate then Plot

#8a.  Dengue.csv
dengue %>%
  filter(complete.cases(.)) %>%           #remove all NAs
  
  filter(type_dengue=='Dengue') %>%       #filter out only "Dengue" type
  
  mutate(date = ymd(paste0(year,"-01-01"))+weeks(eweek)) %>% 
  #create a new column called "date" 
  
  select(date,number) %>% 
  #selecting out only "date" and "number" columns to plot
  
  plot()

#-------------------------------------------------------------------------------
#8b. Another Example for Filter, Mutate then Plot (vaccination.xls)
#https://www.alvinang.sg/s/vaccination.xlsx 

vaccination <- read_excel("vaccination.xlsx")

vaccination %>%
  filter(complete.cases(.)) %>%                   #remove all NAs
  
  filter(vaccination_type=='Poliomyelitis') %>%   
  #filter out only 'Poliomyelitis' 
  
  mutate(date = ymd(paste0(year,"-01-01"))) %>%
  #create a new column called "date" 
  
  mutate(doses = no_of_doses_in_thousands) %>%
  #rename the column
  
  select(date,doses) %>%
  #selecting out only "date" and "doses" columns to plot

  plot()


#-------------------------------------------------------------------------------
#9. Filter, Mutate then Export to CSV

dengue_filtered = dengue %>%
  filter(complete.cases(.)) %>%
  filter(type_dengue=='Dengue') %>%
  mutate(date = ymd(paste0(year,"-01-01"))+weeks(eweek)) %>%
  select(date,number)
  
write_csv(dengue_filtered, path = "dengue_filtered.csv")

#-------------------------------------------------------------------------------
#10. Using Gather to Pivot Data

shampoo = data.frame(
  'A'=c(36.6,39.2,30.4,37.1,34.1),
  'B' = c(17.5,20.6,18.7,25.7,22.0),
  'C'=c(15.0,10.4,18.9,10.5,15.2))

shampoo <- as_tibble(shampoo)
shampoo %>%
  gather(brand, effect)

#-------------------------------------------------------------------------------
#11. Data Joins

df1 = data_frame(name=c('Ally','Steve','John'),age=c(45,46,47))
df2 = data_frame(name=c('Ally','Belinda','John'),age=c(45,48,47))

#11a. Left Join
left_join(df1,df2,by='name')

#11b. Right Join
right_join(df1,df2,by='name')

#11c. Inner Join
inner_join(df1,df2,by='name')

#11d. Full Join
full_join(df1,df2,by='name')

#-------------------------------------------------------------------------------
#12. Group By

#Obtain the "Average" of each Group-
sleep %>% 
  group_by(group) %>%
  summarize(avg_extra=mean(extra))

#-------------------------------------------------------------------------------
#13. Removing Column
data("starwars", package = "dplyr")
d = starwars

#13a. Remove the 'height' column
a = select(starwars, -height)

#-------------------------------------------------------------------------------
#14. Renaming the 'name' column
b = starwars %>%
  rename(BLABLABLA = name)

#-------------------------------------------------------------------------------
#15. Differences between Tibble vs Dataframe
#15a. Tibble

df <- tibble(
  'male' = c(2.3,3.5,4.6,3.2,2.5),
  'female' = c(1.3,2.6,1.7,1.9,2.1)
)
df

#15b. Dataframe
shampoo = data.frame(
  'A'=c(36.6,39.2,30.4,37.1,34.1),
  'B' = c(17.5,20.6,18.7,25.7,22.0),
  'C'=c(15.0,10.4,18.9,10.5,15.2))

#15c. As Tibble
shampoo1 <- as_tibble(shampoo)

#15d. Comparing Conversion
df1 <- data.frame(
  gender = c("Female", "Female","Male"), 
  height = c(152, 171.5, 165), 
  weight = c(81,93, 78),
  age =c(42,38,26),
  row.names=c('Ally','Belinda','Alvin')
)

df2 <- tibble(
  gender = c("Female", "Female","Male"), 
  height = c(152, 171.5, 165), 
  weight = c(81,93, 78),
  age =c(42,38,26),
  row.names=c('Ally','Belinda','Alvin')
)

#15e. Structure of Dataframe vs Tibble
str(df1)
str(df2)

#15f. Comparing Retrieving Columns
df1$ge
df2$ge

#15g. Comparing Display
#https://www.alvinang.sg/s/penguins.csv 

penguins = read.csv('penguins.csv', header = TRUE)

as.data.frame(penguins)
as_tibble(penguins)

#-------------------------------------------------------------------------------
#THE END
#-------------------------------------------------------------------------------
