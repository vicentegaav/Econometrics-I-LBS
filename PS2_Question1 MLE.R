
# PROBLEM SET 2 - ECONOMETRICS --------------------------------------------

# VICENTE GARCIA AVERELL
# Last update: 27/10/2022

# Libraries

library(tidyverse)
library(zoo)
library(cumstats)
library(readxl)

set.seed(5)

# Change working directory

setwd("C:/Users/vgarciaaverell/Documents/LBS/Econometrics/PS2/")

df <- read_xlsx("SP500Index.xlsx")

colnames(df) <- c("date","sp500")

# Rewrite date

df <- df %>% 
  mutate(date = as.Date(paste0(str_sub(date,1,4), "-",
                               str_sub(date,5,6), "-",
                               str_sub(date,7,8)),
                        format = "%Y-%m-%d"))

# returns variable

df <- df %>% 
  mutate(xt = log(sp500/first(df$sp500)))

df %>% 
  ggplot(aes(x = date, y = sp500))+
  geom_line(size = 1, color = "deepskyblue4")+
  theme_bw()

# ML estimators

# Work with returns

# For delta we just need the final observation

delta_ml <- last(df$xt)/(nrow(df))

sum_aux <- 0
for(i in 1:nrow(df)){
  
  if(i == 1){ # X_0 = 0
    sum_aux <- sum_aux + (df$xt[i] - delta_ml)^2
  }
  if(i > 1){
    sum_aux <- sum_aux + (df$xt[i] - df$xt[i-1] - delta_ml)^2
  }
   
}

sigma_ml <- sqrt(sum_aux/(nrow(df)))



