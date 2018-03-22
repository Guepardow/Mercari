# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# Création le 22 février 2018
# Polytechnique Montréal - Dpt de mathématiques et de Génie Industriel
# IND6212 - Exploration de données industrielles
# Contact : Mohamed Maftah (mohamed.maftah@polymtl.ca) & Mehdi Miah (mehdi.miah@gmail.com)
#
# Descriptif  : ouvre le fichier contient toutes les données
#             : filtre sur la catégorie 'Electronics'
#
# Remarques : 
#
# ATTENTION : 
#
# BUG     :
#
# TODO    : déplacer la phase de data engineering dans un autre fichier
#         : supprimer les [rm] des textes => name et description
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# == 0 - Setup ========================================

rm(list=ls())
cat("\014")
setwd("~/Télécom SudParis/Stage international/IND6212 - Exploration de données industrielles/Mercari")

library(data.table) #pour fread
library(stringr)
library(ggplot2)
library(tidyr)
library(dplyr)

# == 1 - Ouverture des fichiers ===============

# Eléments considérés comme NA
na_strings = c(NA, '', '.', ' ')

# Dataset de train
full_dataset = fread("./data/input/train.tsv", stringsAsFactors = FALSE, data.table = FALSE, sep = "\t", encoding = "UTF-8", 
              colClasses = c("character", "character", "numeric", "character", "character", "numeric", "integer", "character"),
              na.strings = na_strings)

# == 2 - Choix de filtre sur Electronics =============

## On ne sélectionne que les marchandises dans le domaine des 'Electronics'
# Création de nouvelles variables : on sépare les catégories entre elles (au plus 5)
# Nombre de catégories par produit
full_dataset$category_name %>% str_count("/") %>% as.factor() %>% summary()

full_dataset = full_dataset %>%
  separate(category_name, c("cat1", "cat2", "cat3", "cat4", "cat5"), sep = "/", remove = TRUE)

# Part des cat1
table(full_dataset$cat1) %>% prop.table() %>% sort(decreasing = TRUE)

# Part des cat1 par etat des articles
table(full_dataset$item_condition_id, full_dataset$cat1) %>% prop.table(margin = 2) %>% round(2)

# Part de description manquante par cat1
full_dataset %>% 
  mutate(missing_description = is.na(item_description)) %>%
  group_by(cat1, missing_description) %>% 
  summarise(effectif = n()) %>% 
  ungroup() %>% 
  group_by(cat1) %>%
  mutate(pct = effectif / sum(effectif)) %>%
  ungroup() %>%
  filter(missing_description)
## Tous pareil ...

# Part de brand manquant par cat1 
full_dataset %>% 
  mutate(missing_brand = is.na(brand_name)) %>%
  group_by(cat1, missing_brand) %>% 
  summarise(effectif = n()) %>% 
  ungroup() %>% 
  group_by(cat1) %>%
  mutate(pct = effectif / sum(effectif)) %>%
  ungroup() %>%
  filter(missing_brand) %>%
  arrange(-pct)

dataset = full_dataset %>%
  filter(cat1 == "Electronics") %>%
  select(-cat1, -cat4, -cat5)

# == 3 - Check des données ====================

# Structure
str(dataset)

# Début du dataset
head(dataset)

## Le commentaire "No description yet" correspond à un commentaire manquant ; on le remplace par NA
dataset$item_description[dataset$item_description == "No description yet"] = NA

# Valeurs manquantes
colSums(is.na(dataset)) %>% sort()
# name  item_description  brand_name  cat4    cat5
# 1     7507              62629       119631  119631

dataset$brand_name[is.na(dataset$brand_name)] = "missing"
# Cela permet de considérer l'effet de ne pas mettre de marque, ou bien d'un produit sans marque

dataset$item_description[is.na(dataset$item_description)] = "missing"
# Cela permet de considérer l'effet de ne pas mettre de description

# Vérification de la clé
dataset$train_id %>% unique() %>% length() == nrow(dataset)

# Nombre de characters dans le nom
dataset$name %>% nchar() %>% quantile(c(0, .01, 0.1, 0.25, 0.50, 0.75, 0.9, 0.99, 1), na.rm = TRUE)
# 0%   1%  10%  25%  50%  75%  90%  99% 100% 
# 2    6   13   19   27   35   39   40   44 

stat_cat2 = dataset %>%
  group_by(cat2, item_condition_id) %>%
  summarise(effectif = n()) %>%
  ungroup() %>%
  group_by(cat2) %>%
  mutate(pct = effectif / sum(effectif)) %>%
  ungroup()

ggplot(stat_cat2, aes(x = cat2, y = pct, fill = as.factor(item_condition_id))) + 
  geom_bar(stat = "identity") +
  coord_flip() + 
  theme_bw()
  
# Etat du produit
dataset$item_condition_id %>% as.factor() %>% table() %>% prop.table() %>% round(4) * 100
# 1     2     3     4     5 
# 42.54 22.73 30.74  3.01  0.98 

# Les marques
dataset$brand_name %>% unique() %>% length()
# 308 marques

# Les 10 marques les plus vendues
dataset$brand_name %>% table() %>% prop.table() %>% sort(decreasing = TRUE) %>% round(4) %>% head(10) * 100
# missing Apple Nintendo  Sony  Xbox  Samsung Beats PopSockets  Beats by Dr. Dre  Fuji
# 51.05   14.07 11.15     6.13  4.37  3.31    1.02  0.89        0.83              0.79
                                        
# Variance des prix par marques
var_marque = dataset %>%
  group_by(brand_name) %>%
  summarise(variance = var(price)) %>%
  ungroup()

var_marque$variance %>% quantile(c(0, .01, 0.1, 0.25, 0.50, 0.75, 0.9, 0.99, 1), na.rm = TRUE)

# Variabilité des prix
quantile(dataset$price, c(0, .01, 0.1, 0.25, 0.50, 0.75, 0.9, 0.99, 1))
# 0%   1%  10%  25%  50%  75%  90%  99% 100% 
# 0    3    6    9   15   31   76  326 1909

# Longueur de la description
dataset$item_description %>% nchar() %>% quantile(c(0, .01, 0.1, 0.25, 0.50, 0.75, 0.9, 0.99, 1), na.rm = TRUE)
# 0%     1%    10%    25%    50%    75%    90%    99%   100% 
# 1.0    7.0   14.0   41.0   98.0  225.0  525.1  974.0 1007.0 

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

# == x - Feature engineering ==============================================

# Création de nouvelles variables
# dataset = dataset %>%
#   mutate(log_price = log(price+1))

# En prédisant le log_price, on n'aura plus qu'à utiliser le RMSE et la moyenne arithmétique,
# au lieu du RMSLE et de la moyenne géométrique

# Longueur du nom
# TODO

# Longueur de la description
# TODO

# == 4 - Sauvegarde =====================================================

write.table(dataset, "./data/intermediate/dataset_electronics.tsv", 
            row.names = FALSE, quote = FALSE, sep = "\t", fileEncoding = "UTF-8")
