# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# Création le 26 février 2018
# Polytechnique Montréal - Dpt de mathématiques et de Génie Industriel
# IND6212 - Exploration de données industrielles
# Contact : Mohamed Maftah (mohamed.maftah@polymtl.ca) & Mehdi Miah (mehdi.miah@gmail.com)
#
# Descriptif  : Phase de data-mining
#
# Remarques : 
#
# ATTENTION : 
#
# BUG     :
#
# TODO    : 
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

# Dataset complet
dataset = fread("./data/mercari/output/dataset.tsv", stringsAsFactors = FALSE, data.table = FALSE, sep = '\t', encoding = "UTF-8")

# == 2 - Data mining ====================

# == 201 | Log du prix par catégorie ==

boxplot(log_price ~ cat1,
        las = 2, #vertical labels
        data = dataset, 
        main = "Log du prix par catégorie 1 du produit",
        xlab = "catégorie 1", 
        ylab = "log du prix") 

# == 202 | Log du prix par condition ==

log_price_moyen_par_condition = dataset %>%
  group_by(item_condition_id) %>%
  summarise(log_price_moyen = log_price %>% mean()) %>%
  ungroup()

ggplot(log_price_moyen_par_condition, aes(x = item_condition_id, y = log_price_moyen)) +
  geom_bar(stat = "identity") +
  labs(title = "Log du prix moyen par condition",
       x = "condition",
       y = "log du prix moyen") + 
  theme_bw()

# == 203 | Log du prix par shipping ==

boxplot(log_price ~ shipping,
        data = dataset, 
        main = "Log du prix par catégorie 1 du produit",
        xlab = "shipping", 
        ylab = "log du prix") 

# == 301 | Lien entre nombre de produits en vente et prix moyen par catégorie3 et condition ==

stat_cat1 = dataset %>%
  group_by(cat1, item_condition_id) %>%
  summarise(nb_ventes = n(),
            log_price_moyen = log_price %>% mean()) %>%
  ungroup()

ggplot(stat_cat1, aes(x = log(nb_ventes), y = log_price_moyen)) + 
  geom_point(aes(color = item_condition_id)) + 
  labs(title = "Lien entre cat1, condition, nombre de ventes et log du prix moyen",
       x = "nombre de ventes",
       y = "log du prix moyen") + 
  theme_bw()
