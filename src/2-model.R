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

source("./code/utils/rmse.R")


# == 1 - Ouverture des fichiers ===============

cat(sprintf("Chargement des données ... \n"))

# Dataset complet
dataset = fread("./data/mercari/output/dataset_electronics.tsv", stringsAsFactors = FALSE, data.table = FALSE, sep = '\t', encoding = "UTF-8",
                colClasses = c("integer", "character", "factor", "factor", "factor", "factor", "factor", "factor",
                               "factor", "factor", "numeric", "integer", "character", "numeric", "integer"))

# Ensemble des folds
list_folds = dataset$fold %>% unique() %>% sort()

# == 2 - Modèle de prédiction ================

# Listes des modèles proposés
list_models = list.files(path = "./code/model", pattern="*.R") %>% gsub(pattern = '.{2}$', replacement = '')
cat(sprintf("Les modèles proposés sont : \n\t%s\n", paste(list_models, collapse = '\n\t')))

#model = readline(prompt = "Choisissez un modèle de prédiction : ")
source("./code/model/predict_mean.R", encoding = "UTF-8")
source("./code/model/predict_cat.R", encoding = "UTF-8")
source("./code/model/predict_linear.R", encoding = "UTF-8")
source("./code/model/predict_arbre.R", encoding = "UTF-8")

vect_score = c()

for(f in list_folds){
  
  # Partitionnement : train / test
  train = dataset %>%
    filter(fold != f)
  
  test = dataset %>%
    filter(fold == f)
  
  # Apprentissage
  #predictions = predict_mean(train, test)
  #predictions = predict_cat(train, test, category = "cat3")
  predictions = predict_linear(train, test)
  #predictions = predict_arbre(train, test)
  
  
  # Mesure de la performance
  score_rmse = rmse(predictions$log_price, predictions$log_price_pred)
  cat(sprintf("Le score de prédiction est de %.3f.\n", score_rmse))
  
  vect_score = c(vect_score, score_rmse)
  
}

cat(sprintf("Le score moyen de ce prédicteur est de %.3f +/- %.3f (à 95%%).\n",
            mean(vect_score), 
            2/sqrt(length(vect_score)) * sd(vect_score)
            )
    )

# predict_mean : Le score moyen de ce prédicteur est de 0.948 +/- 0.002 (à 95%).
# predict_arbre : Le score moyen de ce prédicteur est de 0.890 +/- 0.003 (à 95%).
# predict_cat2 : Le score moyen de ce prédicteur est de 0.886 +/- 0.003 (à 95%).
# predict_cat3 : Le score moyen de ce prédicteur est de 0.743 +/- 0.004 (à 95%).
# predict_linear : Le score moyen de ce prédicteur est de 0.721 +/- 0.004 (à 95%). shipping, cat2, cat3, item_condition_id



# Best : 0.717 avec linear sur shipping, cat2, cat3, item_condition_id

# Sauvegarde
predictions = predictions %>%
  select(train_id, log_price_pred)

#write.table(predictions, "./data/prediction/predict_mean.csv", row.names = FALSE, quote = FALSE, sep = ";", fileEncoding = "UTF-8")
#write.table(predictions, "./data/prediction/predict_cat_3.csv", row.names = FALSE, quote = FALSE, sep = ";", fileEncoding = "UTF-8")
