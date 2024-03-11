# Library

library(dplyr)
library("FactoMineR")
library("factoextra")

# Clear environnement

rm(list = ls())

# Import des données

load(file = "nappe_data_config_0.RData")
load(file = "nappe_data_config_1.RData")
load(file = "nappe_data_config_2.RData")
load(file = "nappe_data_config_3.RData")
load(file = "nappe_data_config_4.RData")
load(file = "nappe_data_config_5.RData")

# Traitement des données nulles

nappe_data_config_0_non_nulles <- nappe_data_config_0 %>% select_if(colSums(.) != 0)

# Retrait de la variable X inutile

nappe_data_config_0_non_nulles <- nappe_data_config_0_non_nulles %>% subset(select = -X)

# Séparation des features et target

nappe_data_config_0_non_nulles.active <- nappe_data_config_0_non_nulles[,5:34654]

# Réalisation de l'ACP par groupe de insertion barres

res.PCA_config_0 <- PCA(nappe_data_config_0_non_nulles, scale.unit = TRUE, ncp = 2, graph = FALSE)
eig.val_PCA_config_0 <- get_eigenvalue(res.PCA_config_0)
fviz_eig(res.PCA_config_0)
fviz_pca_ind(res.PCA_config_0)
fviz_pca_var(res.PCA_config_0, select.var = list(name = c("bore", "burn.up", "P_rel", "T_entree")))
