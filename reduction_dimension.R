# Library

library(dplyr)
library("FactoMineR")
library("factoextra")
library(corrplot)
library(mclust)

# Clear environnement

rm(list = ls())

# Import des données

load(file = "nappe_data_config_0_non_nulles.RData")
load(file = "nappe_data_config_1_non_nulles.RData")
load(file = "nappe_data_config_2_non_nulles.RData")
load(file = "nappe_data_config_3_non_nulles.RData")
load(file = "nappe_data_config_4_non_nulles.RData")
load(file = "nappe_data_config_5_non_nulles.RData")

# ACP globale

nappe_data_non_nulles <- rbind(nappe_data_config_0_non_nulles, nappe_data_config_1_non_nulles,
                    nappe_data_config_2_non_nulles, nappe_data_config_3_non_nulles,
                    nappe_data_config_4_non_nulles, nappe_data_config_5_non_nulles)

# Séparation features et target

nappe_data_non_nulles.active <- nappe_data_non_nulles[,6:31245]

# Réalisation de l'ACP

res.PCA_globale <- PCA(nappe_data_non_nulles, scale.unit = TRUE, ncp = 2, graph = FALSE)
eig.val_PCA_globale <- get_eigenvalue(res.PCA_globale)
fviz_eig(res.PCA_globale, addlabels = TRUE)
fviz_pca_ind(res.PCA_globale, col.ind="cos2", geom = "point",
             gradient.cols = c("white", "#2E9FDF", "#FC4E07" ))
fviz_pca_var(res.PCA_globale, col.var = "contrib", 
             gradient.cols = c("white", "blue", "red"),
             geom = "point",
             ggtheme = theme_minimal())
fviz_pca_var(res.PCA_globale,
             select.var = list(name = c("bore", "burn.up", "P_rel",
                                        "T_entree", "insertion_barres")),
             col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE)

# résumé statistique des Cos^2 associé aux variables de l'ACP

summary(res.PCA_globale$var$cos2)

# Config 0

# Séparation des features et target

nappe_data_config_0_non_nulles.active <- nappe_data_config_0_non_nulles[,6:31245]

# Réalisation de l'ACP par groupe de insertion barres

res.PCA_config_0 <- PCA(nappe_data_config_0_non_nulles, scale.unit = TRUE, ncp = 2, graph = FALSE)
eig.val_PCA_config_0 <- get_eigenvalue(res.PCA_config_0)
fviz_eig(res.PCA_config_0, addlabels = TRUE, ncp = 5)

fviz_pca_ind(res.PCA_config_0, col.ind="cos2", geom = "point",
             gradient.cols = c("white", "#2E9FDF", "#FC4E07" ))
fviz_pca_var(res.PCA_config_0, col.var = "contrib", 
             gradient.cols = c("white", "blue", "red"),
             geom = "point",
             ggtheme = theme_minimal())

# corrplot(var$cos2, is.corr=FALSE) /!\ objet de type "closure" non indiçable

fviz_pca_ind(res.PCA_config_0)
fviz_pca_var(res.PCA_config_0, 
             select.var = list(name = c("bore", "burn.up", "P_rel", "T_entree")),
             col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE)
print(res.PCA_config_0)

# résumé statistique des Cos^2 associé aux variables de l'ACP

# summary(res.PCA_config_0$var$cos2) /!\ data inf à voir d'où cela vient

# Config 1

# Séparation des features et target

nappe_data_config_1_non_nulles.active <- nappe_data_config_1_non_nulles[,6:31245]

# ACP

res.PCA_config_1 <- PCA(nappe_data_config_1_non_nulles, scale.unit = TRUE, ncp = 2, graph = FALSE)
eig.val_PCA_config_1 <- get_eigenvalue(res.PCA_config_1)
fviz_eig(res.PCA_config_1, addlabels = TRUE, ncp = 5)
fviz_pca_ind(res.PCA_config_1, col.ind="cos2", geom = "point",
             gradient.cols = c("white", "#2E9FDF", "#FC4E07" ))
fviz_pca_var(res.PCA_config_1, 
             select.var = list(name = c("bore", "burn.up", "P_rel", "T_entree")),
             col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE)

# résumé statistique des Cos^2 associé aux variables de l'ACP

summary(res.PCA_config_1$var$cos2)

# Config 2

# Séparation des features et target

nappe_data_config_2_non_nulles.active <- nappe_data_config_2_non_nulles[,6:31245]

# ACP

res.PCA_config_2 <- PCA(nappe_data_config_2_non_nulles, scale.unit = TRUE, ncp = 2, graph = FALSE)
eig.val_PCA_config_2 <- get_eigenvalue(res.PCA_config_2)
fviz_eig(res.PCA_config_2, addlabels = TRUE, ncp = 5)
fviz_pca_ind(res.PCA_config_2, col.ind="cos2", geom = "point",
             gradient.cols = c("white", "#2E9FDF", "#FC4E07" ))
fviz_pca_var(res.PCA_config_2,
             select.var = list(name = c("bore", "burn.up", "P_rel", "T_entree")),
             col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE)

# résumé statistique des Cos^2 associé aux variables de l'ACP

summary(res.PCA_config_2$var$cos2)

# Config 3

# Séparation features et target

nappe_data_config_3_non_nulles.active <- nappe_data_config_3_non_nulles[,6:31245]

# ACP

res.PCA_config_3 <- PCA(nappe_data_config_3_non_nulles, scale.unit = TRUE, ncp = 2, graph = FALSE)
eig.val_PCA_config_3 <- get_eigenvalue(res.PCA_config_3)
fviz_eig(res.PCA_config_3, addlabels = TRUE, ncp = 5)
fviz_pca_ind(res.PCA_config_3, col.ind="cos2", geom = "point",
             gradient.cols = c("white", "#2E9FDF", "#FC4E07" ))
fviz_pca_var(res.PCA_config_3, 
             select.var = list(name = c("bore", "burn.up", "P_rel", "T_entree")),
             col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE)

# résumé statistique des Cos^2 associé aux variables de l'ACP

summary(res.PCA_config_3$var$cos2)

# Config 4

# Séparation features et target

nappe_data_config_4_non_nulles.active <- nappe_data_config_4_non_nulles[,6:31245]

# ACP

res.PCA_config_4 <- PCA(nappe_data_config_4_non_nulles, scale.unit = TRUE, ncp = 2, graph = FALSE)
eig.val_PCA_config_4 <- get_eigenvalue(res.PCA_config_4)
fviz_eig(res.PCA_config_4, addlabels = TRUE, ncp = 5)
fviz_pca_ind(res.PCA_config_4, col.ind="cos2", geom = "point",
             gradient.cols = c("white", "#2E9FDF", "#FC4E07" ))
fviz_pca_var(res.PCA_config_4, 
             select.var = list(name = c("bore", "burn.up", "P_rel", "T_entree")),
             col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE)

# résumé statistique des Cos^2 associé aux variables de l'ACP

summary(res.PCA_config_4$var$cos2)

# Config 5

# Séparation features et target

nappe_data_config_5_non_nulles.active <- nappe_data_config_5_non_nulles[,6:31245]

# ACP

res.PCA_config_5 <- PCA(nappe_data_config_5_non_nulles, scale.unit = TRUE, ncp = 2, graph = FALSE)
eig.val_PCA_config_5 <- get_eigenvalue(res.PCA_config_5)
fviz_eig(res.PCA_config_5, addlabels = TRUE, ncp = 5)
fviz_pca_ind(res.PCA_config_5, col.ind="cos2", geom = "point",
             gradient.cols = c("white", "#2E9FDF", "#FC4E07" ))
fviz_pca_var(res.PCA_config_5, 
             select.var = list(name = c("bore", "burn.up", "P_rel", "T_entree")),
             col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE)

# résumé statistique des Cos^2 associé aux variables de l'ACP

summary(res.PCA_config_5$var$cos2)




# Clustering

