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
library(mlr)

source("./code/utils/rmse.R")


# == 1 - Ouverture des fichiers ===============

cat(sprintf("Chargement des données ... \n"))

# Dataset complet
dataset = fread("./data/mercari/output/dataset_electronics.tsv", stringsAsFactors = FALSE, data.table = FALSE, sep = '\t', encoding = "UTF-8",
                colClasses = c("integer", "character", "factor", "factor", "factor", "factor", "factor", "factor",
                               "factor", "factor", "numeric", "integer", "character", "numeric", "integer"))

# Ensemble des folds
list_folds = dataset$fold %>% unique() %>% sort()
dataset$fold = NULL

# == 2 - Modèle de prédiction ================

# All parameters
getParamSet("regr.rpart")

# Learner
makeatree <- makeLearner("regr.rpart", predict.type = "response")

# Cross Validation
set_cv <- makeResampleDesc("CV",iters = 3L)

#Search for hyperparameters
gs <- makeParamSet(makeIntegerParam("minsplit",lower = 100, upper = 500),
                   makeIntegerParam("minbucket", lower = 5000, upper = 25000),
                   makeNumericParam("cp", lower = 0.001, upper = 0.2)
)
# Train task
trainTask <- makeRegrTask(data = dataset %>% select(-name, -item_description, -price),target = "log_price")

#do a grid search
gscontrol <- makeTuneControlGrid()

#hypertune the parameters
stune <- tuneParams(learner = makeatree, 
                      resampling = set_cv, 
                      task = trainTask, 
                      par.set = gs, 
                      control = gscontrol, 
                      measures = mse)

##using hyperparameters for modeling
t.tree <- setHyperPars(makeatree, par.vals = stune$x)

#train the model
t.rpart <- train(t.tree, trainTask)
getLearnerModel(t.rpart)

#make predictions
tpmodel <- predict(t.rpart, trainTask)

submit <- data.frame(id = dataset$train_id, log_price_pred = tpmodel$data$response)

rpart()
