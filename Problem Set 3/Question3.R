# Econometrics I
# Problem Set 3
# Problem 3

# Author: vicentegaav
# Last update: 21/11/2022


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(car)
library(stargazer)
library(ivreg)

# Change working directory and load data

setwd("C:/Users/vicen/OneDrive/Documents/LBS/Econometrics I/Problem Set 3/")


df <- read_xlsx("wage.xlsx")

df_male <- df %>% 
  filter(male == 1)

df_female <- df %>% 
  filter(male != 1)

var.test(log(df_female$wage), log(df_male$wage))
