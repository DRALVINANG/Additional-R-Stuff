#-------------------------------------------------------------------------------
#Step 1: Install Packages and Load Libraries
#-------------------------------------------------------------------------------
install.packages("correlationfunnel")  

library(correlationfunnel)
library(dplyr)

#-------------------------------------------------------------------------------
#Step 2: Load and Glimpse the Data
#-------------------------------------------------------------------------------
data("customer_churn_tbl")

customer_churn_tbl %>% glimpse()

#-------------------------------------------------------------------------------
#Step 3: Binarize the Dataset
#-------------------------------------------------------------------------------
customer_churn_binarized_tbl <- customer_churn_tbl %>%
  select(-customerID) %>%
  mutate(TotalCharges = ifelse(is.na(TotalCharges), MonthlyCharges, TotalCharges)) %>%
  binarize(n_bins = 5, thresh_infreq = 0.01, name_infreq = "OTHER", one_hot = TRUE)

#-------------------------------------------------------------------------------
#Step 4: Glimpse the Binarized Dataset
#-------------------------------------------------------------------------------
customer_churn_binarized_tbl %>% glimpse()

#-------------------------------------------------------------------------------
#Step 5: Correlate the Features (X) to the Target (Y, or Customer Churn)
#-------------------------------------------------------------------------------
customer_churn_corr_tbl <- customer_churn_binarized_tbl %>%
  correlate(Churn__Yes)

#-------------------------------------------------------------------------------
#Step 6: Plot the Correlation Funnel
#-------------------------------------------------------------------------------
customer_churn_corr_tbl %>%
  plot_correlation_funnel()

#-------------------------------------------------------------------------------
#THE END
#-------------------------------------------------------------------------------
