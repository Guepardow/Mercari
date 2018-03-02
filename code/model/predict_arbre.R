# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# Création le 26 février 2018
# Polytechnique Montréal - Dpt de mathématiques et de Génie Industriel
# IND6212 - Exploration de données industrielles
# Contact : Mohamed Maftah (mohamed.maftah@polymtl.ca) & Mehdi Miah (mehdi.miah@gmail.com)
#
# Descriptif  : fonction qui à partir d'un dataframe d'apprentissage, prédit le prix moyen (moyenne arithmétique)
#
# Remarques : 
#
# ATTENTION : 
#
# BUG     :
#
# TODO    :
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

library(rpart)
library(rpart.plot)

predict_arbre = function(train, test){
  
  # Modèle de l'arbre
  model_tree = rpart(log_price ~ cat1 + shipping + item_condition_id, data = train)  
  
  # Visualisation de l'arbre
  prp(model_tree,extra=1)

  # Prédiction
  test$log_price_pred = predict(model_tree,test)
  
  return(test)
}