

#Wrangling
install.packages("dplyr")
library(dplyr)

# Load the dataset
hsa = read.csv("https://www.alvinang.sg/s/hsa.csv") %>%
  
  mutate(bmi = weight / (height^2)) %>%
  
  mutate(Cancer_Status = ifelse(cancer_cell > 0, "POSITIVE", "NEGATIVE")) %>%
  
  select(-comments, -height, -weight, -cancer_cell)



hsa2 = hsa %>%
  mutate(Cancer_Status = ifelse(Cancer_Status == "NEGATIVE", 0, 1),
         gender = ifelse(gender == "Female", 0, 1),
         smoker = ifelse(smoker == "No", 0, 1),
         chest_pain = case_when(chest_pain == "none" ~ 0,
                                chest_pain == "asympt" ~ 1,
                                chest_pain == "typ_angina" ~ 2))

# Gender: 0 is Female, 1 is Male
# Smoker: 0 is No , 1 is Yes
# Chest Pain: 0 is None, 1 is Asympt, 2 is Typ_Angin


#Heatmaply------------------------------------------------

install.packages("heatmaply",
                 dependencies = TRUE)

library(heatmaply)

hsa_cor = hsa2 %>%
  cor(., method="pearson")

heatmaply(hsa_cor,
          colors = colorRampPalette(c("blue", "white", "red"))(100))


#Correlation Funnel-----------------------------
install.packages("correlationfunnel")

library(correlationfunnel)

hsa_binarized = hsa2 %>%
  binarize(n_bins =4,
           thresh_infreq=0.01)

hsa_cor_tbl = hsa_binarized %>%
  correlate(target = Cancer_Status__1)

hsa_cor_tbl %>%  
  plot_correlation_funnel(interactive=FALSE)

