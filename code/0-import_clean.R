# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# Création le 22 février 2018
# Polytechnique Montréal - Dpt de mathématiques et de Génie Industriel
# IND6212 - Exploration de données industrielles
# Contact : Mohamed Maftah (mohamed.maftah@polymtl.ca) & Mehdi Miah (mehdi.miah@gmail.com)
#
# Descriptif  : 
#
# Remarques : 
#
# ATTENTION : 
#
# BUG     :
#
# TODO    : remplacer les "" par des NA (cf category_name)
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# == 0 - Setup ========================================

rm(list=ls())
cat("\014")
setwd("~/Télécom SudParis/Stage international/IND6212 - Exploration de données industrielles/Mercari")

library(data.table) #pour fread
library(stringr)
library(tidyr)
library(dplyr)

# == 1 - Ouverture des fichiers ===============

# Eléments considérés comme NA
na_strings = c(NA, '', '.', ' ')

# Dataset de train
dataset = fread("./data/mercari/input/train.tsv", stringsAsFactors = FALSE, data.table = FALSE, sep = "\t", encoding = "UTF-8", 
              colClasses = c("character", "character", "numeric", "character", "character", "numeric", "integer", "character"),
              na.strings = na_strings)

# Dataset de test
#test = fread("./data/test.tsv", stringsAsFactors = FALSE, data.table = FALSE, sep = "\t", encoding = "UTF-8")

# == 2 - Check des données ====================

# Structure
str(dataset)

# Début du dataset
head(dataset)

# Valeurs manquantes
colSums(is.na(dataset)) %>% sort()

# Vérification de la clé
dataset$train_id %>% unique() %>% length() == nrow(dataset)

# Nombre de characters dans le nom
dataset$name %>% nchar() %>% quantile(c(0, .01, 0.1, 0.25, 0.50, 0.75, 0.9, 0.99, 1), na.rm = TRUE)
# 0%   1%  10%  25%  50%  75%  90%  99% 100% 
# 1    6   13   19   26   34   38   40   44

# Etat du produit
((dataset$item_condition_id %>% as.factor() %>% table()) / nrow(dataset) * 100) %>% round(2)
# 1     2     3     4     5 
# 43.21 25.33 29.15  2.16  0.16 

# Nombre de catégories par produit
dataset$category_name %>% str_count("/") %>% as.factor() %>% summary()
# 2       3       4    NA's 
# 1471819    1330    3059    6327 
## On a 3, 4, 5 ou 0 catégories par produit

# Les marques
dataset$brand_name %>% unique() %>% length()
# 4810 marques

# Les 10 marques les plus vendues
dataset$brand_name %>% table() %>% sort(decreasing = TRUE) %>% head(10)
# PINK    Nike    Victoria's Secret LuLaRoe   Apple   FOREVER 21  Nintendo  Lululemon   Michael Kors    American Eagle  
# 54088   54043   48036             31024     17322   15186       15007     14558       13928           13254 

# Variance des prix par marques
var_marque = dataset %>%
  group_by(brand_name) %>%
  summarise(variance = var(price)) %>%
  ungroup()

var_marque$variance %>% quantile(c(0, .01, 0.1, 0.25, 0.50, 0.75, 0.9, 0.99, 1), na.rm = TRUE)

# Variabilité des prix
quantile(dataset$price, c(0, .01, 0.1, 0.25, 0.50, 0.75, 0.9, 0.99, 1))

# Longueur de la description
dataset$item_description %>% nchar() %>% quantile(c(0, .01, 0.1, 0.25, 0.50, 0.75, 0.9, 0.99, 1), na.rm = TRUE)

# Idées: 
# - identifier les produits de luxe
# - identifier les catégories (level1,2, 3)
# - extraire à partir des textes : corrélation entre mots clés et prix
# - on laisse les 0 et les +1000$ (cf la métrique)
# - prix moyen par famille de produits
# - lien entre les prix et l'etat du produit
# - lien entre le nombre de demandes et le prix moyen
# - modele linéaire
# - modele de réseau bayésien
# - variance des prix par catégorie

# == 3 - Feature engineering ==============================================

# Création de nouvelles variables
dataset = dataset %>%
  mutate(log_price = log(price+1)) %>%
  separate(category_name, c("cat1", "cat2", "cat3", "cat4", "cat5"), sep = "/")

# En prédisant le log_price, on n'aura plus qu'à utiliser le RMSE et la moyenne arithmétique,
# au lieu du RMSLE et de la moyenne géométrique translatée

# == 4 - CrossValidation =================================================

set.seed(42) ; vect_fold = sample(1:nrow(dataset), nrow(dataset)) #resampling
n_fold = 5

vect_fold = vect_fold %% n_fold
table(vect_fold)

dataset = dataset %>%
  mutate(fold = vect_fold)

# == 5 - Sauvegarde =====================================================

write.table(dataset, "./data/mercari/output/dataset.tsv", 
            row.names = FALSE, quote = FALSE, sep = "\t", fileEncoding = "UTF-8")
