# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# Création le 22 février 2018
# Polytechnique Montréal - Dpt de mathématiques et de Génie Industriel
# IND6212 - Exploration de données industrielles
# Contact : Mohamed Maftah (mohamed.maftah@polymtl.ca) & Mehdi Miah (mehdi.miah@gmail.com)
#
# Descriptif  : métrique
#
# Remarques : 
#
# ATTENTION : 
#
# BUG     :
#
# TODO    : 
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

rmsle = function(y_pred, y_true, na.rm = FALSE){
  if(length(y_pred) != length(y_true)){
    stop("Les deux vecteurs ne sont pas de même dimension !\n")
  }else{
    n_elements = length(y_pred)
    rmsle = (mean((log(y_pred+1) - log(y_true+1))**2, na.rm = na.rm)) ** (1/2)
    return(rmsle)
  }
  
}

if(FALSE){
  x1 = c(0,1,8,6)
  x2 = c(8,9,6,8,2)
  x3 = c(9,9,6,8,2)
  x4 = c(7,9,6,8,2)
  x5 = c(8,9,6,8,3)
  
  rmsle(x1,x2) #error
  rmsle(x2,x2) #0
  rmsle(x3,x2) #0.047 : il vaut mieux sur-estimer
  rmsle(x2,x3) #symétrique
  rmsle(x4,x2) #0.052 : que sous-estimer pour un prix donné
  rmsle(x5,x2) #0.128 : raisonner en ratio d'erreur
  
}