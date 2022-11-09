

# PROBLEM SET 2 - ECONOMETRICS --------------------------------------------

# VICENTE GARCIA AVERELL
# Last update: 27/10/2022

# Libraries

library(tidyverse)
library(zoo)
library(cumstats)

set.seed(5)

# Change working directory

setwd("C:/Users/vgarciaaverell/Documents/LBS/Econometrics/PS2/")

num_simul <- 1e3

# OLS estimator of regression coefficients is given by

beta_ols <- function(y,x){
  beta_output <- cov(x,y)/var(x)
  return(beta_output)
}


# (a) Unbiasedness ------------------------------------------------------------

# Empty dataset where we will save betas

beta_save <- tibble(iteration = 0,beta = NA) 

for(i in 1:num_simul){
  e <- runif(n = num_simul, min = -1, max = 1)
  z <- rchisq(n = num_simul, df = 3)
  y <- 1+0.5*z+e
  beta_aux <- tibble(iteration = i,
                     beta = beta_ols(y,z))
  
  beta_save <- bind_rows(beta_save, beta_aux)
  
  if(i%%1000==0){
    print(i)
  }
}

beta_save <- beta_save %>% 
  filter(!is.na(beta))

average_beta <- mean(beta_save$beta)

beta_save %>% 
  ggplot(aes(x = beta))+
  geom_histogram(aes(y = 100*stat(width*density)),
                 size = 1 ,
                 color = "deepskyblue4",
                 fill = "deepskyblue3",
                 alpha = 0.5, bins = 100)+
  theme_bw()+
  geom_vline(xintercept = 0.5, color = "black", size = 1, linetype = "longdash")+
  geom_vline(xintercept = average_beta, color = "indianred4", size = 1, linetype = "longdash")+
  theme(text = element_text(size = 16))+
  ylab("%")+
  labs(title = "Monte Carlo simulation of OLS estimator",
       subtitle = "Unbiasedness")

ggsave("fig_unbiasedness.jpg")
#beta_save %>% 
#  ggplot(aes(x = iteration, y = beta))+
#  geom_line(size = 1,  color = "deepskyblue4")+
#  geom_hline(yintercept = 0.5, size = 1, color = "black", linetype = "longdash")+
#  theme_bw()+
#  theme(text = element_text(size = 16))+
#  labs(title = "Monte Carlo simulation of OLS estimator",
#       subtitle = "Unbiasedness")+
#  scale_y_continuous(limits = c(0.45,0.55))
  

# (b) Consistency -------------------------------------------------------------

beta_consistency <- tibble(num_obs = 0, beta = NA, var_z = NA)


for(i in 1:num_simul){

  e <- runif(n = i, min = -1, max = 1)
  z <- rchisq(n = i, df = 3)
  y <- 1+0.5*z+e
  # Auxilliary dataframe so sample of y and z is taken from the same
  
  yz_df <- tibble(z = z, y = y)
  
  # Take a sample. Because they are randomly generated, we can take
  # the first i observations for both variables
  
  #yz_sample <-  slice(yz_df, 1:i)
  
  
  beta_aux <- tibble(num_obs = i,
                     beta = beta_ols(y = as.vector(yz_df$y),
                                     x = as.vector(yz_df$z)),
                     var_z = var(x = as.vector(yz_sample$z)))
  
  beta_consistency <- bind_rows(beta_consistency, beta_aux)
  if(i%%1000==0){
    print(paste0(round(100*i/num_simul,2),"% done"))
  }
}
  

beta_consistency <- beta_consistency %>% 
  filter(!is.na(beta))

beta_consistency <- beta_consistency %>% 
  mutate(avg_beta = cumsum(beta)/num_obs)

beta_consistency %>% 
  #filter(num_obs>10) %>% #
  ggplot(aes(x = num_obs, y = beta))+
  geom_line(size = 1, color = "forestgreen")+
  theme_bw()+
  theme(text = element_text(size = 16))+
  geom_hline(yintercept = 0.5, color = "black", linetype = "longdash")+
  labs(title = "Monte Carlo simulation of OLS estimator",
       subtitle = "Consistency")+
  scale_y_continuous(limits = c(0.45,0.55))+
  xlab("Number of observations used")+
  ylab("Estimated beta")

ggsave("fig_consistency_2.jpg")


beta_consistency <- beta_consistency %>% 
  mutate(row = num_obs - 1) %>% 
  mutate(var_beta = cumvar(beta))

beta_consistency %>% 
  filter(!is.na(var_beta),
         row > 200) %>% 
  ggplot(aes(x = num_obs, y = var_beta)) +
  geom_line(size = 1, color = "deepskyblue3")+
  theme_bw()+
  labs(title = "Further evidence of Consistency")+
  #geom_hline(yintercept = 1/18)+
  theme(text = element_text(size = 16))+
  xlab("Number of observations")+
  ylab("Estimated variance of beta")

ggsave("consistency2.jpg")
# (c) Variance ------------------------------------------------------------

beta_check <- beta_save %>% 
  filter(!is.na(beta)) %>% 
  mutate(avg_beta = cumsum(beta)/iteration) %>% 
  mutate(var_cum = cumsum(beta-avg_beta)^2)

# Variance # END UP NOT USING THIS

beta_save2 <- beta_save %>% 
  mutate(std_normal = rnorm(nrow(beta_save),mean = 0.5,sd = 1/18^2))
beta_save2 %>%
  gather(key = variable,
         value = value,
         -iteration) %>% 
  group_by(variable) %>% 
  ggplot(aes(x = value, color = variable, fill = variable))+
  stat_density(alpha = 0.5)+
  #geom_density(aes(x = std_normal),kernel = "gaussian",fill = "red", alpha = 0.1, color = "red")+
  theme_bw()+
  theme(text = element_text(size = 16))+
  ylab("%")+
  labs(title = "Monte Carlo simulation of OLS estimator",
       subtitle = "Distributions")

ggsave("fig_unbiasedness.jpg")


# RSS as unbiased predictor -----------------------------------------------


rss_save <- tibble(iteration = 0,rss = NA) 

for(i in 1:num_simul){
  e <- runif(n = num_simul, min = -1, max = 1)
  z <- rchisq(n = num_simul, df = 3)
  y <- 1+0.5*z+e
  
  mod_aux <- lm(y ~ z)
  
  rss_aux <- tibble(iteration = i,
                     rss = sum(resid(mod_aux)^2))
  
  rss_save <- bind_rows(rss_save, rss_aux)
}

rss_save %>% 
  ggplot(aes(x = rss/(num_simul-2)))+
  geom_histogram(aes(y = 100*stat(width*density)),
                 size = 1 ,
                 color = "deepskyblue4",
                 fill = "deepskyblue3",
                 alpha = 0.5,)+
  theme_bw()+
  #geom_vline(xintercept = 0.5, color = "black", size = 1, linetype = "longdash")+
  geom_vline(xintercept = mean(rss_save$rss/(num_simul-2), na.rm = T),
             color = "black", size = 1, linetype = "longdash")+
  theme(text = element_text(size = 16))+
  ylab("%")+
  xlab("RSS/(n-k)")+
  labs(title = "Monte Carlo simulation of OLS RSS",
       subtitle = "Unbiasedness of RSS/(n-k)")

ggsave("rss.jpg")
mean(rss_save$rss/(max(rss_save$iteration) -2), na.rm = T)
