# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# Création le 26 février 2018
# Polytechnique Montréal - Dpt de mathématiques et de Génie Industriel
# IND6212 - Exploration de données industrielles
# Contact : Mohamed Maftah (mohamed.maftah@polymtl.ca) & Mehdi Miah (mehdi.miah@gmail.com)
#
# Descriptif  : fonction qui prédit le prix via un modèle linéaire
#
# Remarques : 
#
# ATTENTION : 
#
# BUG     :
#
# TODO    :
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

predict_linear = function(train, test){
  
  # Valeur par défaut
  ## Lorsque qu'un input est missing
  mean_log_price = train$log_price %>% mean()
  
  # Obtention de la valeur à prédire
  linear_model = lm(log_price ~ brand_name  + shipping + item_condition_id, data = train)  
  
  # Prédiction
  test$log_price_pred = predict(linear_model,test)
  
  # Remplace les NA par le prix moyen sur l'ensemble
  test$log_price_pred[is.na(test$log_price_pred)] = mean_log_price
  
  return(test)
}