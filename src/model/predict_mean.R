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

predict_mean = function(train, test){
  
  # Obtention de la valeur à prédire
  mean_log_price = train$log_price %>% mean()

  # Application de l'apprentissage sur la base de test
  test$log_price_pred = mean_log_price
  
  return(test)
}