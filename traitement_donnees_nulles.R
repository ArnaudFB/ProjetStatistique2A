# Library

library(dplyr)

# Clear environnement

rm(list=ls())

# Import des données

load(file = "nappe_data_config_0.RData")
load(file = "nappe_data_config_1.RData")
load(file = "nappe_data_config_2.RData")
load(file = "nappe_data_config_3.RData")
load(file = "nappe_data_config_4.RData")
load(file = "nappe_data_config_5.RData")

# Concaténation en 1 DF

nappe_data <- rbind(nappe_data_config_0, nappe_data_config_1,
                    nappe_data_config_2, nappe_data_config_3,
                    nappe_data_config_4, nappe_data_config_5)

nappe_data <- nappe_data[,-1] 
nappe_data_config_0 <- nappe_data_config_0[,-1]
