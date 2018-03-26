# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# Création le 21 mars 2018
# Polytechnique Montréal - Dpt de mathématiques et de Génie Industriel
# IND6212 - Exploration de données industrielles
# Contact : Mohamed Maftah (mohamed.maftah@polymtl.ca) & Mehdi Miah (mehdi.miah@gmail.com)
#
# Descriptif  : Correction des catégories cat2 et cat3
#             : quels sont les produits outliers ... dont les prix sont si hauts / si bas
#
# Remarques : 
#
# ATTENTION :
#
# BUG     : encodage utf-8
#
# TODO    : rajouter les bigrams et les trigrams
#         : ne pas retirer les numéros
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# == 0 - Setup ========================================

rm(list=ls())
cat("\014")
setwd("~/Télécom SudParis/Stage international/IND6212 - Exploration de données industrielles/Mercari")

library(data.table) #pour fread
library(stringr)
library(ggplot2)
library(tidyr)
library(dplyr)

# == 1 - Ouverture des fichiers ===============

# Dataset en électronique
dataset = fread("./data/intermediate/dataset_electronics.tsv", 
                stringsAsFactors = FALSE, data.table = FALSE, sep = '\t', encoding = "UTF-8",
                colClasses = c("integer", "character", "factor", "factor", "factor", "factor", 
                               "numeric", "integer", "character", "numeric"))

# == 2 - Création de la table mots x cat ====================

## Pour chaque article, on : 
# - récupère le nom ; 
# - le nettoie (lower, suppression des mots inutiles, ...) ; 
# - affecte la cat2 et la cat3
# - regroupe pour normaliser la proba P(cat2 | word) et P(cat3 | word)

list_name = dataset$name

# Lowerisation
list_name = list_name %>% tolower()

# Tokenisation
list_name_tokens = list_name %>% strsplit(split = ' ')

# Stopwords
# TODO

# Remove puntuation

# Dictionnaire
dico_tokens = list_name_tokens %>% unlist() %>% unique()
len_dico = dico_tokens %>% length()
n_cat2 = dataset$cat2 %>% unique() %>% length()
n_cat3 = dataset$cat3 %>% unique() %>% length()

# Création de la table
df_word_cat = data.frame(word = rep(NA, len_dico * n_cat2 * n_cat3 / 2), 
                         cat2 = rep(NA, len_dico * n_cat2 * n_cat3 / 2), 
                         cat3 = rep(NA, len_dico * n_cat2 * n_cat3 / 2))
i = 1
articleIND = 1

pb <- txtProgressBar(min = 1, max = length(list_name_tokens), style = 3)
for(l in list_name_tokens){
  
  setTxtProgressBar(pb, i)
  
  # Pour chaque article
  for(w in l){
    df_word_cat$word[i] = w
    df_word_cat$cat2[i] = dataset$cat2[articleIND] %>% as.character()
    df_word_cat$cat3[i] = dataset$cat3[articleIND] %>% as.character()
    
    i = i + 1 
  }
  articleIND = articleIND + 1
  
}


head(df_word_cat)

df_word_cat = na.omit(df_word_cat)

write.table(df_word_cat, "./data/intermediate/df_word_cat.csv", 
            row.names = FALSE, quote = FALSE, sep = ";", fileEncoding = "UTF-8")

class_cat2 = df_word_cat %>%
  group_by(word, cat2) %>%
  summarise(effectif = n()) %>%
  ungroup() %>%
  group_by(word) %>%
  mutate(pct = effectif / sum(effectif)) %>%
  ungroup() %>%
  arrange(-pct, -effectif)
  

head(class_cat2)

##
write.table(class_cat2, "./data/intermediate/class_cat2_118484.csv", 
            row.names = FALSE, quote = FALSE, sep = ";", fileEncoding = "UTF-8") 

class_cat3 = df_word_cat %>%
  group_by(word, cat3) %>%
  summarise(effectif = n()) %>%
  ungroup() %>%
  group_by(word) %>%
  mutate(pct = effectif / sum(effectif)) %>%
  ungroup() %>%
  arrange(-pct, -effectif)


write.table(class_cat3, "./data/intermediate/class_cat3_118484.csv", 
            row.names = FALSE, quote = FALSE, sep = ";", fileEncoding = "UTF-8")
## Affectation de la catégorie

## Clustering of names (cosine similarity)

library(tm)

corpus = Corpus(VectorSource(dataset$name))

corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, stripWhitespace)

corpus <- tm_map(corpus, removeWords, stopwords())

ndocs <- length(corpus)
# ignore overly sparse terms (appearing in less than 1% of the documents)
minDocFreq <- ndocs * 0.01
# ignore overly common terms (appearing in more than 80% of the documents)
maxDocFreq <- ndocs * 1
myDtmUnigram<- DocumentTermMatrix(corpus, control = list(bounds = list(global = c(minDocFreq, maxDocFreq)),
                                                         weighting = "weightBin"))

myDtmUnigram

as.matrix(myDtmUnigram)[1:6,1:4]

findFreqTerms(myDtmUnigram, lowfreq=100)

m <- as.matrix(myDtmUnigram)

d <- stats::dist(m, method = "binary")

## 3 - Avec arules ========================================

library(arules)

df_word_cat2 = fread("./data/intermediate/df_word_cat_118484.csv", fill = TRUE, 
                     colClasses = rep("factor", 3), nrows = 29780, data.table = FALSE)
df_word_cat2$cat2 = NULL
str(df_word_cat2)

rules = apriori(df_word_cat2, parameter= list(target = "rules",supp=0.0001, conf=0.75, minlen = 2))
inspect(rules)

levels(df_word_cat2$word)[rules@lhs@data@i + 1] %>% as.character()

# == 4 - Arules auto ======================================

library(tm)

dataset_cond1 = dataset %>%
  filter(item_condition_id == 1)

corpus = Corpus(VectorSource(dataset_cond1$name))

corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, stripWhitespace)

corpus <- tm_map(corpus, removeWords, stopwords())

ndocs <- length(corpus)
# ignore overly sparse terms (appearing in less than 0.1% of the documents)
minDocFreq <- ndocs * 0.001
# ignore overly common terms (appearing in more than 80% of the documents)
maxDocFreq <- ndocs * 1
myDtmUnigram<- DocumentTermMatrix(corpus, control = list(bounds = list(global = c(minDocFreq, maxDocFreq)),
                                                         weighting = "weightBin"))

myDtmUnigram

m = as.matrix(myDtmUnigram)
m = data.frame( m > 0)
dim(m)

## mots => Accesories
## reclassifie les accessoires mal pclassé dans les autres cat
## On reclassifie dans : autres que accesoires, et cheap => on vérifie s'il y a ces mots
## Pour le reclassifier : osef, cest des accessoires


# Rajout des catégories
m$cat2 = dataset_cond1$cat2
m$cat3 = dataset_cond1$cat3

# Obtention des quantiles de prix par catégorie
## Quels sont les articles très cheap / très expensive par catégorie3 ??
quantile_cond1 = dataset_cond1 %>%
  group_by(cat3) %>%
  mutate(quant = cut(x      = price, 
                        breaks = quantile(price, probs = c(0, .1, .9, 1)),
                        include.lowest = TRUE,
                        labels = c("Cheap", "Moderate", "Expensive")
                        )
            ) %>%
  ungroup()

# Rajout du quantile
m$quant = quantile_cond1$quant

## Visu des résultats
aaa = quantile_prix_cat3 %>% filter(cat3 == "Cell Phones & Smartphones") %>% select(name, price, quant, shipping, item_description, item_condition_id, train_id)

## Exemple sur internet
## tableOne <- within(tableOne, quartile <- as.integer(cut(salesPrice, quantile(salesPrice, probs=0:4/4), include.lowest=TRUE)))

## Apriori
library(arules)
trans <- as(m, "transactions")
summary(trans)

cat3_family = paste0("cat3=", unique(quantile_cond1$cat3))
cat2_family = paste0("cat2=", unique(quantile_cond1$cat2))
quant_family = paste0("quant=", unique(quantile_cond1$quant))

rules = apriori(trans, 
                parameter = list(target = "rules", support = 0.0001, confidence= 0.8),
                appearance = list(rhs = c(cat3_family, quant_family), none = cat2_family)
)

arules::inspect(sort(rules, by = "lift"))
## Il faut insérer la classe du prix au sein de la condition
# Découvrir que c'est que les cases .... Tout ça pour ça ...


# lhs                                              rhs          support      confidence lift       count
# [1]  {cat3=Desktops & All-In-Ones,quant=Expensive} => {new}        0.0001149668 1.0000000    1.784056   6  
# [2]  {cat3=DVD & Blu-ray Players,quant=Expensive}  => {new}        0.0002107724 1.0000000    1.784056  11  
# [3]  {cat3=GPS Units & Equipment,quant=Expensive}  => {new}        0.0001724501 0.9000000    1.605651   9  
# [4]  {cat3=Consoles,quant=Expensive}               => {new}        0.0021268850 0.8951613    1.597018 111  
# [5]  {cat3=Desktops & All-In-Ones,quant=Cheap}     => {optical}    0.0001532890 0.8888889  450.390507   8  
# [6]  {cat3=Desktops & All-In-Ones,quant=Cheap}     => {windows}    0.0001532890 0.8888889  162.772710   8  
# [7]  {cat3=Desktops & All-In-Ones,quant=Cheap}     => {mac}        0.0001532890 0.8888889  167.473726   8  
# [8]  {cat3=Desktops & All-In-Ones,quant=Cheap}     => {without}    0.0001532890 0.8888889   42.058225   8  
# [9]  {cat3=Desktops & All-In-Ones,quant=Cheap}     => {buttons}    0.0001532890 0.8888889   40.269290   8  
# [10] {cat3=Desktops & All-In-Ones,quant=Cheap}     => {package}    0.0001532890 0.8888889   10.964364   8  
# [11] {cat3=Desktops & All-In-Ones,quant=Cheap}     => {compatible} 0.0001532890 0.8888889    8.721606   8  
# [12] {cat3=Tripods & Supports,quant=Cheap}         => {tripod}     0.0004598670 0.8888889  271.287849  24  
# [13] {cat3=VHS,quant=Expensive}                    => {new}        0.0001149668 0.8571429    1.529191   6  
# [14] {cat3=Blu-Ray,quant=Moderate}                 => {new}        0.0069938110 0.8057395    1.437485 365




## Vérifions cela
library(ggplot2)

prix_median_cat3 = dataset %>%
 # filter(!grepl(pattern = "case", x = tolower(dataset$name))) %>%
  group_by(cat3, item_condition_id) %>%
  summarise(prix_median = log(price+1) %>% median()) %>%
  ungroup()

ggplot(prix_median_cat3, aes(x = item_condition_id, y = prix_median, group = cat3, color = cat3)) + 
  geom_line() + 
  theme_bw() + 
  theme(legend.position = "top")

aaa = dataset %>% filter(item_condition_id == 1) %>% filter(cat2 == "Cell Phones & Accessories") %>%   filter(!grepl(pattern = "case", x = tolower(name)))


