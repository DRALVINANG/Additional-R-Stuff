#Step 0: Getting the Files
#https://www.alvinang.sg/s/eisenhower.txt

#-------------------------------------------------------------------------------

#Step 1: Installing Tidyverse into R
installed.packages("tidyverse", dependencies = TRUE)
#-------------------------------------------------------------------------------

#Step 2: Load Packages
library(tidyverse)
library(tibble)
library(tidyr)
library(dplyr)
library(readxl)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(tidytext)
library(wordcloud2)
library(stringr)
library(textdata)
#-------------------------------------------------------------------------------

#Step 3: Import Text File
text <- readLines(file.choose())

text
#-------------------------------------------------------------------------------

#Step 4: Tibble it (Convert to Dataframe)
length = length(text)
tb <- tibble(line = 1:length, text = text)

tb
#-------------------------------------------------------------------------------

#Step 5: Unnest (Tokenization)
tb_un <- tb %>%
  unnest_tokens(word, text)

tb_un
#-------------------------------------------------------------------------------

#Step 6: Remove Stop words
sw = stop_words

tb_un_rm <- tb_un %>%
  anti_join(stop_words)

tb_un_rm
#-------------------------------------------------------------------------------

#Step 7: Count the Most Frequent Words
tb_un_rm_c = tb_un_rm %>%
  count(word, sort = TRUE) 

tb_un_rm_c
#-------------------------------------------------------------------------------

#Step 8: Plotting the Most Frequent Words
tb_un_rm_c_plt = tb_un_rm_c %>%
  filter(n>2) %>%
  mutate(word = reorder(word, n)) %>%
  
  ggplot(aes(word, n)) + geom_col(fill = "darkred") + theme_fivethirtyeight() +
  xlab(NULL) + ylab("Word Count") + coord_flip() + ggtitle("Word Usage in Eisenhower.txt") 

tb_un_rm_c_plt
#-------------------------------------------------------------------------------

#Step 9: Creating Word Cloud
tb_un_rm_cld = tb_un_rm %>%
  count(word, sort=TRUE) %>%
  wordcloud2(word,size=2)

tb_un_rm_cld
#-------------------------------------------------------------------------------

#Step 10: Analyze Text Sentiments
#10a) NRC
# nrc categorizes words as POSITIVE / NEGATIVE / ANGER / ANTICIPATION / DISGUST 
# FEAR / JOY / SADNESS / SURPRISE AND TRUST

#10a)(i) Get Sentiments
nrc = get_sentiments("nrc")

#10a)(ii) Filter Sentiments
nrc_sentiment <- get_sentiments("nrc") %>%
  filter(sentiment == "fear")

#10a)(iii) Inner Join Sentiments to Text File + Do Word Count
tb_un_rm_nrc = tb_un_rm %>%
  inner_join(nrc_sentiment) %>%
  count(word, sort = TRUE)

tb_un_rm_nrc

#10a)(iv) Visualize
tb_un_rm_nrc %>%
  filter(n > 0) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill=n)) + theme_fivethirtyeight() + geom_col() +
  xlab(NULL) + coord_flip() + ylab("Word Count") + 
  ggtitle("Fear Words Usage in Eisenhower",
          subtitle = "Sentiment Analysis Using NRC")
#-------------------------------------------------------------------------------

#10b) Bing
# Bing categorizes words as Positive or Negative

#10b)(i) Get Sentiments
bing = get_sentiments('bing')

#10b)(ii) Inner Join Sentiments to Text File
tb_un_rm_bing = tb_un_rm %>%
  inner_join(get_sentiments('bing'))

#10b)(iii) Do Word Count + Inner Join Sentiments to Text File
tb_un_rm_bing_1 = tb_un_rm_bing %>%
  count(word, sort = TRUE) %>%
  inner_join(tb_un_rm_bing)

#10b)(iv) Visualize
tb_un_rm_bing_1 %>%
  filter(n > 0) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) + theme_fivethirtyeight() + geom_col() +
  xlab(NULL) + coord_flip() + ylab("Word Count") + 
  ggtitle("Positive / Negative Words Usage in Eisenhower",
          subtitle = "Sentiment Analysis Using Bing et al.")
#-------------------------------------------------------------------------------

#10c) AFINN
# AFINN give word scores
# -5 is Negative while 5 is Positive

#10c)(i) Get Sentiments
afinn = get_sentiments("afinn")

#10c)(ii) Inner Join Sentiments to Text File
tb_un_rm_afinn = tb_un_rm %>%
  inner_join(get_sentiments('afinn'))
  
#10c)(iii) Do Word Count + Inner Join Sentiments to Text File
tb_un_rm_afinn_1 = tb_un_rm_afinn %>%
 count(word, sort = TRUE) %>%
 inner_join(tb_un_rm_afinn)

#10c)(iv) Visualize
tb_un_rm_afinn_1 %>%
  filter(n > 0) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill=value)) + theme_fivethirtyeight() + geom_col() +
  xlab(NULL) + coord_flip() + ylab("Word Count") + 
  ggtitle("Positive (+5) / Negative (-5) Words Usage in Eisenhower",
          subtitle = "Sentiment Analysis Using AFINN")

#-------------------------------------------------------------------------------

#THE END

#-------------------------------------------------------------------------------
