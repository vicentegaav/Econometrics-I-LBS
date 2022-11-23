# Econometrics I
# Problem Set 3
# Problem 6

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

df <- read_xls("PS4data.xls")
df <- df %>% 
  rename(rcnondur = `real consumption of nondurables`,
         rcserv = `real consumption of services`,
         rdi = `real disposable income`,
         nyse = `equally weighted average return on the New York Stock Exchange`)

# Reconstruct variables of interest

df <- df %>% 
  mutate(c = (rcnondur + rcserv)/population)

# (a)

mod_a <- lm(c ~ lag(c,1) + lag(c,2) + lag(c,3) + lag(c,4),
            data = df)

summary(mod_a)

stargazer(mod_a, type = "latex", out = "q6a.tex")

linearHypothesis(mod_a, c("lag(c, 2) = 0", "lag(c, 3) = 0", "lag(c, 4) = 0"))

# (b)

# Create income per capita variable

df <- df %>% 
  mutate(y = rdi/population)

# Create " changes variables"

df <- df %>% 
  mutate(delta_c = c - lag(c,1),
         delta_y = y - lag(y,1))

mod_b <- lm(delta_c~ delta_y,
            data = df)

summary(mod_b)

# (c) 


df <- df %>% 
  mutate(log_dc = log(c/lag(c,1)),
       log_dy = log(y/lag(y,1)))

mod_c <- ivreg(log_dc ~ log_dy |
                1 + lag(log_dc,2) + lag(log_dc,3) + 
                 lag(log_dc,4) + lag(log_dc,5),
               data = df)
summary(mod_c)

stargazer(mod_c, type = "latex", out = "q6c.tex")
