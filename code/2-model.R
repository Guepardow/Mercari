# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# Création le 26 février 2018
# Polytechnique Montréal - Dpt de mathématiques et de Génie Industriel
# IND6212 - Exploration de données industrielles
# Contact : Mohamed Maftah (mohamed.maftah@polymtl.ca) & Mehdi Miah (mehdi.miah@gmail.com)
#
# Descriptif  : programme permettant de prédire sur une base de test, d'en évaluer le score et de le sauvegarder
#
# Remarques : 
#
# ATTENTION : 
#
# BUG     :
#
# TODO    : faire la CV sur les 5 partionnements 
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

cat(sprintf("Chargement des données ... \n"))

# Dataset complet
dataset = fread("./data/mercari/output/dataset.tsv", stringsAsFactors = FALSE, data.table = FALSE, sep = '\t', encoding = "UTF-8",
                colClasses = c("integer", "character", "factor", "factor", "factor", "factor", "factor", "factor",
                               "factor", "numeric", "integer", "character", "numeric", "integer"))

# Partitionnement : train / test
train = dataset %>%
  filter(fold != 0)

test = dataset %>%
  filter(fold == 0)

# == 2 - Modèle de prédiction ================

# Listes des modèles proposés
list_models = list.files(path = "./code/model", pattern="*.R") %>% gsub(pattern = '.{2}$', replacement = '')
cat(sprintf("Les modèles proposés sont : \n\t%s\n", paste(list_models, collapse = '\n\t')))

#model = readline(prompt = "Choisissez un modèle de prédiction : ")
source("./code/model/predict_mean.R")
source("./code/model/predict_cat.R")
source("./code/model/predict_linear.R")
source("./code/model/predict_arbre.R")
predictions = predict_mean(train, test)
predictions = predict_cat(train, test, category = "cat4")
predictions = predict_linear(train, test)
predictions = predict_arbre(train, test)

# Mesure de la performance
source("./code/utils/rmse.R")
score_rmse = rmse(predictions$log_price, predictions$log_price_pred)
cat(sprintf("Le score de prédiction est de %.3f.\n", score_rmse))

# Sauvegarde
predictions = predictions %>%
  select(train_id, log_price_pred)

#write.table(predictions, "./data/prediction/predict_mean.csv", row.names = FALSE, quote = FALSE, sep = ";", fileEncoding = "UTF-8")
#write.table(predictions, "./data/prediction/predict_cat_3.csv", row.names = FALSE, quote = FALSE, sep = ";", fileEncoding = "UTF-8")
