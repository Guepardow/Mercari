# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# Création le 15 mars 2018
# Polytechnique Montréal - Dpt de mathématiques et de Génie Industriel
# IND6212 - Exploration de données industrielles
# Contact : Mohamed Maftah (mohamed.maftah@polymtl.ca) & Mehdi Miah (mehdi.miah@gmail.com)
#
# Descriptif  : fonction qui à partir d'un dataframe d'apprentissage, prédit le prix à partir des commentaires
#
# Remarques : 
#
# ATTENTION : 
#
# BUG     :
#
# TODO    :
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

library(randomForest)

predict_rf = function(train, test){
  
  # Modèle du random forest
  model_rf = randomForest(log_price ~ cat1 + item_condition_id + shipping, data = train[1:5000,], na.action = na.roughfix)
  
  # Importance des variables
  varImpPlot(model_rf)
  
  return(test)
}