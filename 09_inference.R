# Inference about skyglow in Pennsylvania
# Comparing naïve and model-based methods
library(tidyverse)
library(sf)

# Naïve way
gan <- read_rds("data/gan_enriched.rds")

m_naive <- mean(gan$sky_brightness, na.rm = TRUE)
sem_naive <- sd(gan$sky_brightness, na.rm = TRUE) / sqrt(length(na.omit(gan$sky_brightness)))
l_naive <- m_naive - 1.96*sem_naive
u_naive <- m_naive + 1.96*sem_naive

sprintf("The naïve estimate of sky brightness in Pennsylvania is %0.2f (95%% CI [%0.2f, %0.2f])", 
        m_naive, l_naive, u_naive)


# Model-based way
pred <- read_rds("data/model_fits/pred_model8.rds")
m_model <- mean(pred$est, na.rm = TRUE)
ss_between <- sum((na.omit(pred$est) - m_model)^2)
ss_within  <- sum(pred$var, na.rm = TRUE)
v_model <- (ss_between + ss_within) / (length(na.omit(pred$est)) - 1)

sem_model <- sqrt(v_model) / sqrt(length(na.omit(pred$est)))
l_model <- m_model - 1.96*sem_model
u_model <- m_model + 1.96*sem_model

ss_within/(length(na.omit(pred$est))-1)
ss_between/(length(na.omit(pred$est))-1)

sprintf("The model-based estimate of sky brightness in Pennsylvania is %0.2f (95%% CI [%0.2f, %0.2f])", 
        m_model, l_model, u_model)



