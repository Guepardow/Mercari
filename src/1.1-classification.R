# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# Création le 21 mars 2018
# Polytechnique Montréal - Dpt de mathématiques et de Génie Industriel
# IND6212 - Exploration de données industrielles
# Contact : Mohamed Maftah (mohamed.maftah@polymtl.ca) & Mehdi Miah (mehdi.miah@gmail.com)
#
# Descriptif  : Correction des catégories cat2 et cat3
#
# Remarques : 
#
# ATTENTION :
#
# BUG     :
#
# TODO    : 
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

# Dataset complet
dataset = fread("./data/intermediate/dataset_electronics.tsv", stringsAsFactors = FALSE, data.table = FALSE, sep = '\t', encoding = "UTF-8",
                colClasses = c("integer", "character", "factor", "factor", "factor", "factor", "numeric", "integer", "character"))

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

rules = apriori(df_word_cat2, parameter= list(target = "rules",supp=0.0001, conf=0.85))
inspect(rules)
