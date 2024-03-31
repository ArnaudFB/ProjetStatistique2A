# Library

library(dplyr)
library("FactoMineR")
library("factoextra")
library(corrplot)

# Clear environnement

rm(list = ls())

# Import des données

load(file = "nappe_data_config_0.RData")
load(file = "nappe_data_config_1.RData")
load(file = "nappe_data_config_2.RData")
load(file = "nappe_data_config_3.RData")
load(file = "nappe_data_config_4.RData")
load(file = "nappe_data_config_5.RData")

# Config 0

# Traitement des données nulles

nappe_data_config_0_non_nulles <- nappe_data_config_0 %>% select_if(colSums(.) != 0)

# Retrait de la variable X inutile

nappe_data_config_0_non_nulles <- nappe_data_config_0_non_nulles %>% subset(select = -X)

# Séparation des features et target

nappe_data_config_0_non_nulles.active <- nappe_data_config_0_non_nulles[,3:34634]

# Réalisation de l'ACP par groupe de insertion barres

res.PCA_config_0 <- PCA(nappe_data_config_0_non_nulles, scale.unit = TRUE, ncp = 2, graph = FALSE)
eig.val_PCA_config_0 <- get_eigenvalue(res.PCA_config_0)
fviz_eig(res.PCA_config_0)

fviz_pca_ind(res.PCA_config_0, col.ind="cos2", geom = "point",
             gradient.cols = c("white", "#2E9FDF", "#FC4E07" ))
fviz_pca_var(res.PCA_config_0, col.var = "contrib", 
             gradient.cols = c("white", "blue", "red"),
             geom = "point",
             ggtheme = theme_minimal())
corrplot(var$cos2, is.corr=FALSE)

fviz_pca_ind(res.PCA_config_0)
fviz_pca_var(res.PCA_config_0, select.var = list(name = c("bore", "burn.up", "P_rel", "T_entree")))
print(res.PCA_config_0)

# résumé statistique des Cos^2 associé aux variables de l'ACP

summary(res.PCA_config_0$var$cos2)

# Config 1

# Traitement des données nulles/des variables X inutile
# + Séparation des features/targets

nappe_data_config_1_non_nulles <- nappe_data_config_1 %>% select_if(colSums(.) != 0)
nappe_data_config_1_non_nulles <- nappe_data_config_1_non_nulles %>% subset(select = -X)
nappe_data_config_1_non_nulles.active <- nappe_data_config_1_non_nulles[,3:34634]

# ACP

res.PCA_config_1 <- PCA(nappe_data_config_1_non_nulles, scale.unit = TRUE, ncp = 2, graph = FALSE)
eig.val_PCA_config_1 <- get_eigenvalue(res.PCA_config_1)
fviz_eig(res.PCA_config_1)
fviz_pca_ind(res.PCA_config_1)
fviz_pca_var(res.PCA_config_1, select.var = list(name = c("bore", "burn.up", "P_rel", "T_entree")))

# résumé statistique des Cos^2 associé aux variables de l'ACP

summary(res.PCA_config_1$var$cos2)

# Config 2

# Traitement des données nulles/des variables X inutile
# + Séparation des features/targets

nappe_data_config_2_non_nulles <- nappe_data_config_2 %>% select_if(colSums(.) != 0)
nappe_data_config_2_non_nulles <- nappe_data_config_2_non_nulles %>% subset(select = -X)
nappe_data_config_2_non_nulles.active <- nappe_data_config_2_non_nulles[,3:34634]

# ACP

res.PCA_config_2 <- PCA(nappe_data_config_2_non_nulles, scale.unit = TRUE, ncp = 2, graph = FALSE)
eig.val_PCA_config_2 <- get_eigenvalue(res.PCA_config_2)
fviz_eig(res.PCA_config_2)
fviz_pca_ind(res.PCA_config_2)
fviz_pca_var(res.PCA_config_2, select.var = list(name = c("bore", "burn.up", "P_rel", "T_entree")))

# résumé statistique des Cos^2 associé aux variables de l'ACP

summary(res.PCA_config_2$var$cos2)

# Config 3

# Traitement des données nulles/des variables X inutile
# + Séparation des features/targets

nappe_data_config_3_non_nulles <- nappe_data_config_3 %>% select_if(colSums(.) != 0)
nappe_data_config_3_non_nulles <- nappe_data_config_3_non_nulles %>% subset(select = -X)
nappe_data_config_3_non_nulles.active <- nappe_data_config_3_non_nulles[,3:34634]

# ACP

res.PCA_config_3 <- PCA(nappe_data_config_3_non_nulles, scale.unit = TRUE, ncp = 2, graph = FALSE)
eig.val_PCA_config_3 <- get_eigenvalue(res.PCA_config_3)
fviz_eig(res.PCA_config_3)
fviz_pca_ind(res.PCA_config_3)
fviz_pca_var(res.PCA_config_3, select.var = list(name = c("bore", "burn.up", "P_rel", "T_entree")))

# résumé statistique des Cos^2 associé aux variables de l'ACP

summary(res.PCA_config_3$var$cos2)

# Config 4

# Traitement des données nulles/des variables X inutile
# + Séparation des features/targets

nappe_data_config_4_non_nulles <- nappe_data_config_4 %>% select_if(colSums(.) != 0)
nappe_data_config_4_non_nulles <- nappe_data_config_4_non_nulles %>% subset(select = -X)
nappe_data_config_4_non_nulles.active <- nappe_data_config_4_non_nulles[,4:44644]

# ACP

res.PCA_config_4 <- PCA(nappe_data_config_4_non_nulles, scale.unit = TRUE, ncp = 2, graph = FALSE)
eig.val_PCA_config_4 <- get_eigenvalue(res.PCA_config_4)
fviz_eig(res.PCA_config_4)
fviz_pca_ind(res.PCA_config_4)
fviz_pca_var(res.PCA_config_4, select.var = list(name = c("bore", "burn.up", "P_rel", "T_entree")))

# résumé statistique des Cos^2 associé aux variables de l'ACP

summary(res.PCA_config_4$var$cos2)

# Config 5

# Traitement des données nulles/des variables X inutile
# + Séparation des features/targets

nappe_data_config_5_non_nulles <- nappe_data_config_5 %>% select_if(colSums(.) != 0)
nappe_data_config_5_non_nulles <- nappe_data_config_5_non_nulles %>% subset(select = -X)
nappe_data_config_5_non_nulles.active <- nappe_data_config_5_non_nulles[,5:55655]

# ACP

res.PCA_config_5 <- PCA(nappe_data_config_5_non_nulles, scale.unit = TRUE, ncp = 2, graph = FALSE)
eig.val_PCA_config_5 <- get_eigenvalue(res.PCA_config_5)
fviz_eig(res.PCA_config_5)
fviz_pca_ind(res.PCA_config_5)
fviz_pca_var(res.PCA_config_5, select.var = list(name = c("bore", "burn.up", "P_rel", "T_entree")))

# résumé statistique des Cos^2 associé aux variables de l'ACP

summary(res.PCA_config_5$var$cos2)
