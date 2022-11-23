# Econometrics I
# Problem Set 3
# Problem 2

# Author: vicentegaav
# Last update: 21/11/2022


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)

# Change working directory and load data

setwd("C:/Users/vicen/OneDrive/Documents/LBS/Econometrics I/Problem Set 3/")

df <- read_xlsx("Nerlove1963/Nerlove1963.xlsx")


# Transform variables into logs

df <- df %>% 
  mutate(logTC = log(Cost),
         logQ = log(output),
         logPL = log(Plabor),
         logPK = log(Pcapital),
         logPF = log(Pfuel)) %>% 
  rename(TC = Cost)

spec_init = "logTC ~ logQ + logPL + logPK + logPF"

mod_prelim <- lm(spec_init, data = df)
summary(mod_prelim)

# (a)

spec_a <- paste0(spec_init, "+ logQ^2")

df <- df %>% 
  mutate(logQsq = logQ^2)

spec_a <- paste0(spec_init, "+logQsq")

mod_a <- lm(spec_a, data = df)
summary(mod_a)

# (b)

summary(df)
# Take q10 and q90 as range for beta 7, so that we guarantee the number
# of observations the problem asks for. (By construction we will have 15).

q10_b7 <- quantile(df$logQ,0.1)
q90_b7 <- quantile(df$logQ,0.9)

# (c)

# Simulate 1000 uniformly distributed "betas" between the quantiles

beta7_vec <- runif(1e3, q10_b7, q90_b7)

save_OLS_c <- tibble(ssr = NA, beta7 = NA, num_iter = NA)

num_aux = 1

df_c <- df %>% 
  mutate(logtc_pl = logTC - logPL,
         logpk_pl = logPK - logPL,
         logpf_pl = logPF - logPL)

for(i in beta7_vec){
  df_aux <- df_c %>% 
    mutate(z = logQ/(1 + exp(i - logQ)))
  
  model_aux <- lm(logtc_pl ~ logQ + logpk_pl + logpf_pl + z,
                  data = df_aux)
  
  df_aux_bind <- tibble(ssr = sum(resid(model_aux)^2),
                        beta7 = i,
                        num_iter = num_aux)
  
  save_OLS_c <- bind_rows(save_OLS_c, df_aux_bind)
  
  num_aux = num_aux + 1
  
  rm(df_aux, df_aux_bind)
  
}

save_OLS_c <- save_OLS_c %>% 
  filter(!is.na(ssr))

min_beta7 <- save_OLS_c %>% 
  filter(ssr == min(save_OLS_c$ssr))

# rerun regression just for that beta

df_c <- df_c %>% 
  mutate(z = logQ/(1 + exp(min_beta7$beta7 - logQ)))

model_c <- lm(logtc_pl ~ logQ + logpk_pl + logpf_pl + z,
                data = df_c)

summary(model_aux)

# still need to recover beta3

beta3_c <- 1 - coefficients(model_c)[3] - coefficients(model_c)[4]

# (d) Can use distribution of beta or bootsrap SE.
# we bootstrap, its quicker

num_bootstrap = 1e4

save_coeffs <- tibble(beta1 = NA,  beta2 = NA, beta3 = NA,
                      beta4 = NA, beta5 = NA, beta6 = NA)

for(i in 1:num_bootstrap){
  
  df_aux <- sample_n(df_c, 100, replace = T)  
  
  model_bootstrap <- lm(logtc_pl ~ logQ + logpk_pl + logpf_pl + z,
                        data = df_aux)
  
  df_aux_coeffs <- tibble(beta1 = coefficients(model_bootstrap)[1],
                          beta2 = coefficients(model_bootstrap)[2],
                          beta3 = 1 - coefficients(model_bootstrap)[3] - coefficients(model_bootstrap)[4],
                          beta4 = coefficients(model_bootstrap)[3],
                          beta5 = coefficients(model_bootstrap)[4],
                          beta6 = coefficients(model_bootstrap)[5])
  
  save_coeffs <- bind_rows(save_coeffs, df_aux_coeffs)
}

save_coeffs <- save_coeffs %>% 
  filter(!is.na(beta6))

save_coeffs %>% 
  summarise_all(sd)
