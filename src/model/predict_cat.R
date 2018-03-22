# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# Création le 26 février 2018
# Polytechnique Montréal - Dpt de mathématiques et de Génie Industriel
# IND6212 - Exploration de données industrielles
# Contact : Mohamed Maftah (mohamed.maftah@polymtl.ca) & Mehdi Miah (mehdi.miah@gmail.com)
#
# Descriptif  : Pour un produit donné, prédit le prix moyen selon la catégorie
#             : Lorsqu'il n'existe pas d'exemple d'apprentissage, on prédit le prix moyen observé sur l'ensemble du dataset
#
# Remarques : Cela revient à faire de la prédiction à partir d'une variable !!
#
# ATTENTION : 
#
# BUG     :
#
# TODO    :
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

predict_cat = function(train, test, category){
  
  # Prix lorsqu'il n'y a pas d'exemple d'apprentissage
  ## Va au final remplacer les NA issus de la jointure future
  mean_log_price = train$log_price %>% mean()
  
  # Apprentissage sur la base de train à partir des catégories
  price_cat = train %>%
    select_('log_price', category) %>%
    group_by_(category) %>%
    summarise(log_price_pred = log_price %>% mean()) %>%
    ungroup()
  
  # Prédiction
  predictions = test %>%
    left_join(price_cat, by = category) 
  
  # Remplace les NA par le prix moyen sur l'ensemble
  predictions$log_price_pred[is.na(predictions$log_price_pred)] = mean_log_price
  
  return(predictions)
}
