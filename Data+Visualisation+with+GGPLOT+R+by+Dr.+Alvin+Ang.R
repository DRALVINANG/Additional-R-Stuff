#-------------------------------------------------------------------------------
# Data Visualization with GGPLOT R by Dr. Alvin Ang
#-------------------------------------------------------------------------------
# Using TIDYVERSE to Plot
#-------------------------------------------------------------------------------
#A) Install TIDYVERSE
install.packages("tidyverse", dependencies=TRUE)

#-------------------------------------------------------------------------------
#B) Importing Libraries
library(tidyverse)
library(tibble)
library(tidyr)
library(dplyr)
library(readxl)
library(ggplot2)
library(lubridate)

#-------------------------------------------------------------------------------
#C1) Scatter Plot 1
# Read the college dataset
# The file can be found here: https://www.alvinang.sg/s/college.csv 
college <- read_csv('college.csv')

# Take a look at the data
summary(college)

# Convert state, region, highest_degree, control, and gender to factors
college <- college %>%
  mutate(state=as.factor(state), 
         region=as.factor(region), 
         city=as.factor(city),
         highest_degree=as.factor(highest_degree),
         control=as.factor(control), 
         gender=as.factor(gender))

# Take a look at the data
summary(college)

# Let's build a simple scatterplot with tuition on the x-axis 
# and average SAT score on the y axis
ggplot(data=college) + geom_point(size=3) + aes(x=tuition, 
                                                y=sat_avg)

#-------------------------------------------------------------------------------
#C2) Scatter Plot 2
ggplot(data=college) + geom_point(size=3) + aes(x=tuition, 
                                                y=sat_avg, 
                                                shape=control, 
                                                color=control)


#-------------------------------------------------------------------------------
#C3) Scatter Plot 3
ggplot(data=college) + geom_point() + aes(x=tuition, 
                                          y=sat_avg, 
                                          color=control, 
                                          size=undergrads)

#-------------------------------------------------------------------------------
#C4) Scatter Plot 4
ggplot(data=college) + geom_point() + aes(x=tuition, 
                                          y=sat_avg, 
                                          color=control, 
                                          size=undergrads,
                                          alpha=1/100)

#-------------------------------------------------------------------------------
#C5) Add Line to Scatter Plot 1
ggplot(data=college) + geom_line() + geom_point() + aes(x=tuition, 
                                                        y=sat_avg, 
                                                        shape=control, 
                                                        color=control)

#-------------------------------------------------------------------------------
#C6) Add Line to Scatter Plot 2	
ggplot(data=college) + geom_smooth() + geom_point() + aes(x=tuition, 
                                                          y=sat_avg, 
                                                          shape=control, 
                                                          color=control)

#-------------------------------------------------------------------------------
#C7) Add Line to Scatter Plot 3
ggplot(data=college) + geom_smooth(se=FALSE) + geom_point() + aes(x=tuition, 
                                                                  y=sat_avg, 
                                                                  shape=control, 
                                                                  color=control)

#-------------------------------------------------------------------------------
#C8) Scatter Plot 5
#File can be found here: https://www.alvinang.sg/s/vaccination.xlsx 

vaccination <- read_excel("vaccination.xlsx")

vaccine <- vaccination %>%
  filter(complete.cases(.)) %>%
  mutate(date = ymd(paste0(year,"-01-01"))) %>%
  mutate(doses = no_of_doses_in_thousands) %>%
  mutate(type = vaccination_type) %>%
  select(date,type, doses)

ggplot(data=vaccine) + geom_point(size=3) + aes(x=date, y=doses, color=type)

#-------------------------------------------------------------------------------
#D1) Bar Chart 1
ggplot(data=college) + geom_bar() + aes(x=region)

#-------------------------------------------------------------------------------
#D2) Bar Chart 2
ggplot(data=college) + geom_col() + aes(x=region, y = sat_avg)

#-------------------------------------------------------------------------------
#D3) Stacked Bar Chart 3
ggplot(data=college) + geom_bar() + aes(x=region, fill=control)

#-------------------------------------------------------------------------------
#D4) Bar Chart 4
college %>%
  group_by(region) %>%	
  summarize(avg_tuition=mean(tuition)) %>%
  ggplot() + geom_col() + aes(x=region, y=avg_tuition)

#-------------------------------------------------------------------------------
#D5) Bar Chart 5
ggplot(data=vaccine) + geom_col() + aes(x=type,y=doses,fill=type)

#-------------------------------------------------------------------------------
#D6) Bar Chart 6
vaccine %>%
  group_by(type) %>%
  summarize(avg_dose=mean(doses)) %>%
  ggplot() + geom_col() + aes(x=type, y=avg_dose)

#-------------------------------------------------------------------------------
#E1) Histogram 1
ggplot(data=college) + geom_histogram(binwidth=1000) + aes(x=undergrads)

#-------------------------------------------------------------------------------
#E2) Histogram 2
#File can be found here: https://www.alvinang.sg/s/dengue.csv 
dengue <- read_csv("dengue.csv")

dengue %>%
  filter(type_dengue=='Dengue') %>%
  ggplot() + geom_histogram(binwidth =100) + aes(x=number)

#-------------------------------------------------------------------------------
#F1) Box Plot 1
ggplot(data=college) + geom_boxplot(fill='green') + aes(x=control, y=tuition)

#-------------------------------------------------------------------------------
#F2) Box Plot 2
ggplot(data=college) + geom_boxplot(fill='red') + 
  geom_jitter(col="blue") + aes(x=control, y=tuition)

#-------------------------------------------------------------------------------
#F3) Box Plot 3
vaccination <- read_excel("vaccination.xlsx")

vaccine <- vaccination %>%
  filter(complete.cases(.)) %>%
  mutate(date = ymd(paste0(year,"-01-01"))) %>%
  mutate(doses = no_of_doses_in_thousands) %>%
  mutate(type = vaccination_type) %>%
  select(date,type, doses)

ggplot(data=vaccine) + geom_boxplot(fill='red') + aes(x=type,y=doses)

#-------------------------------------------------------------------------------
#THE END
#-------------------------------------------------------------------------------