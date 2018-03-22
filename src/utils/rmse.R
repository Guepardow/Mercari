# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# Création le 26 février 2018
# Polytechnique Montréal - Dpt de mathématiques et de Génie Industriel
# IND6212 - Exploration de données industrielles
# Contact : Mohamed Maftah (mohamed.maftah@polymtl.ca) & Mehdi Miah (mehdi.miah@gmail.com)
#
# Descriptif  : métrique RMSE
#
# Remarques : 
#
# ATTENTION : 
#
# BUG     :
#
# TODO    : 
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

rmse = function(y_pred, y_true, na.rm = FALSE){
  if(length(y_pred) != length(y_true)){
    stop("Les deux vecteurs ne sont pas de même dimension !\n")
  }else{
    rmse = (mean((y_pred - y_true)**2, na.rm = na.rm)) ** (1/2)
    return(rmse)
  }
  
}

if(FALSE){
  x1 = c(0,1,8,6)
  x2 = c(8,9,6,8,2)
  x3 = c(9,9,6,8,2)
  x4 = c(7,9,6,8,2)
  x5 = c(8,9,6,8,3)
  
  rmse(x1,x2) #error
  rmse(x2,x2) #0
  rmse(x3,x2) #0.447 : 
  rmse(x2,x3) #symétrique
  rmse(x4,x2) #0.052 : identique que ce soit +/- 1
  rmse(x5,x2) #0.128 : pas d'effet du log
  
}

