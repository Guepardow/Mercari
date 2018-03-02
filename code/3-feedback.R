# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# Création le 26 février 2018
# Polytechnique Montréal - Dpt de mathématiques et de Génie Industriel
# IND6212 - Exploration de données industrielles
# Contact : Mohamed Maftah (mohamed.maftah@polymtl.ca) & Mehdi Miah (mehdi.miah@gmail.com)
#
# Descriptif  : permet de faire le diagnostic d'un modèle
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
library(ggplot2)
library(plotly) #pour la visualisation interactive
library(tidyr)
library(dplyr)

# == 1 - Ouverture des fichiers ===============

# Dataset complet
dataset = fread("./data/mercari/output/dataset.tsv", stringsAsFactors = FALSE, data.table = FALSE, sep = '\t', encoding = "UTF-8")

# Prédictions à évaluer
model_a_evaluer = "cat_3"
predictions = fread(paste0("./data/prediction/predict_", model_a_evaluer, ".csv"), stringsAsFactors = FALSE, data.table = FALSE, sep = ';', encoding = "UTF-8")

# == 2 - Ajout des variables explicatives ==============

predictions = predictions %>%
  left_join(dataset, by = "train_id")

# == 3 - Visualisation des erreurs de prédiction =========

# Scatter plot entre prix réel et prix prédit
ggplot(predictions, aes(x = log_price, y = log_price_pred)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, lty = 2, color = 'red') + 
  labs(title = paste0("Etat des prédictions du model : ", model_a_evaluer),
       x = "prix réel (log)",
       y = "prix prédit (log)") + 
  theme_bw()

# Histogramme des erreurs
source("./code/utils/rmse.R")
hist((predictions$log_price -predictions$log_price_pred)**2, breaks = 50)
