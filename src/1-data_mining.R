# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# Création le 26 février 2018
# Polytechnique Montréal - Dpt de mathématiques et de Génie Industriel
# IND6212 - Exploration de données industrielles
# Contact : Mohamed Maftah (mohamed.maftah@polymtl.ca) & Mehdi Miah (mehdi.miah@gmail.com)
#
# Descriptif  : Phase de data-mining
#
# Remarques : 
#
# ATTENTION : 302
#
# BUG     :
#
# TODO    : cloudwords pour créer des features
#         : feature = version du modèle (ex : iPhone) => dictionnaire !
#         : grepl("Gb" pour la mémoire)
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# == 0 - Setup ========================================

rm(list=ls())
cat("\014")
setwd("~/Télécom SudParis/Stage international/IND6212 - Exploration de données industrielles/Mercari")

library(data.table) #pour fread
library(stringr)
library(ggplot2)
library(tm)
library(SnowballC)
library(tidyr)
library(dplyr)

# == 1 - Ouverture des fichiers ===============

# Dataset complet
dataset = fread("./data/mercari/output/dataset_electronics.tsv", stringsAsFactors = FALSE, data.table = FALSE, sep = '\t', encoding = "UTF-8",
                colClasses = c("integer", "character", "factor", "factor", "factor", "factor", "factor", "factor",
                               "factor", "factor", "numeric", "integer", "character", "numeric", "integer"))

# == 2 - Data mining ====================

# == 201 | Log du prix par catégorie ==

boxplot(log_price ~ cat2,
        las = 2, #vertical labels
        data = dataset, 
        main = "Log du prix par catégorie 2 du produit",
        xlab = "", 
        ylab = "log du prix", 
        size= 1) 

# == 202 | Log du prix par condition ==

log_price_moyen_par_condition = dataset %>%
  group_by(item_condition_id) %>%
  summarise(log_price_moyen = log_price %>% mean()) %>%
  ungroup()

ggplot(log_price_moyen_par_condition, aes(x = item_condition_id, y = log_price_moyen)) +
  geom_bar(stat = "identity") +
  labs(title = "Log du prix moyen par condition",
       x = "condition",
       y = "log du prix moyen") + 
  theme_bw()

# == 203 | Log du prix par shipping ==

boxplot(log_price ~ shipping,
        data = dataset, 
        main = "Log du prix par catégorie 1 du produit",
        xlab = "shipping", 
        ylab = "log du prix") 

# == 301 | Lien entre nombre de produits en vente et prix moyen par catégorie et condition ==

stat_cat2 = dataset %>%
  group_by(cat2, item_condition_id) %>%
  summarise(nb_ventes = n(),
            log_price_moyen = log_price %>% mean()) %>%
  ungroup()

ggplot(stat_cat2, aes(x = log(nb_ventes), y = log_price_moyen)) + 
  geom_point(aes(color = item_condition_id)) + 
  labs(title = "Lien entre cat2, condition, nombre de ventes et log du prix moyen",
       x = "nombre de ventes",
       y = "log du prix moyen") + 
  theme_bw()

# == 302 | Frequence des mots ==

#Mettre les commentaires dans un seul fichier
cat(dataset$item_description, file = "./data/description.txt", sep = " ")
#Lire le fichier
#tolower
description <- scan("./data/description.txt","character",sep="\n")
#Laisser juste les charact?re alpha num?rique
description<-str_to_lower(gsub("[^[:alnum:]]"," ",description))
#Cr?er un vecteur de mots
words<-unlist(strsplit(description," "))
#liste des mot ? enlever
to.remove <- c("a", "i", "and", "to", "from", "or", "and", "yet")
words <- words[! words %in% to.remove]
#Calculer la frequence
words.freq <- table(words)
words.freq <- as.data.frame(words.freq)
words.freq <- words.freq[order(words.freq$Freq,decreasing = T), ]
#Justifier un seuil (histogram?)
seuil <- integer()
words.freq <- words.freq[words.freq$Freq > seuil, ]
#Ajouter ne nombre d'occurence de chaque mot dans le dataset
for (word in words){
  cbind(str_count(string = dataset$item_condition, pattern = word))
}
#Renomer les colonnes
#setnames(data, old=c("old_name","another_old_name"), new=c("new_name", "another_new_name"))
names(dataset)[15:nrow(words.freq)] <- words.freq$word

# == 303 - Part de produits d'occasion par cat2 ==

prop_cond_by_cat2 = dataset %>%
  group_by(item_condition_id, cat2) %>%
  summarise(effectif = n()) %>%
  ungroup() %>%
  group_by(cat2) %>%
  mutate(pct = effectif / sum(effectif)) %>%
  ungroup()

ggplot(prop_cond_by_cat2, aes(x = cat2, y = pct, fill = item_condition_id)) + 
  geom_bar(stat = "identity") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
# == 304 - Part de produits usagés vs prix médian ==

## Conjecture : plus un type de produits est vendu en occasion, plus il vaut cher
##            : cad, les produits à prix bas ne sont pas revendus mais jetés ...

stat_cat2 = dataset %>%
  group_by(cat2) %>%
  summarise(prix_median = log_price %>% median(),
            part_occasion = sum(as.numeric(item_condition_id) >=2) / n()) %>%
  ungroup()

ggplot(stat_cat2, aes(x = part_occasion, y = prix_median)) + 
  geom_text(size = 3, vjust = -0.5, aes(label = cat2)) + 
  geom_point() +
  theme_bw()

# Nice !

# == 305 - Part de shipping par cat2 ==

part_shipping_cat2 = dataset %>%
  group_by(cat2) %>%
  summarise(part_ship = mean(shipping)) %>%
  ungroup()

ggplot(part_shipping_cat2, aes(x = cat2, y = part_ship)) + 
  geom_bar(stat = 'identity') + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# == 401 - commentaires ==

all_comments = paste(dataset$item_description, collapse = " ")
length(all_comments)
class(all_comments) ; typeof(all_comments)

all_comments2 = strsplit(all_comments, split = ' ') 
freq_table = all_comments2  %>% table() %>% sort(decreasing = TRUE)

# == 402 - nom des produits ==

all_names = paste(dataset$name, collapse = ' ')
all_names = strsplit(all_names, split = ' ')
freq_names_word = all_names %>% table() %>% sort(decreasing = TRUE)
freq_names_word %>% head(20)

# == 403 - Analyse des commentaires ==

#premiere phase : minuscule, sans pontuation, sans nombre, sans double espace
corpus = Corpus(VectorSource(dataset$item_description),list(language = "english"))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, stripWhitespace)

#seconde phase : correction orthographique
#aspell(as.factor("accuser"),program="aspell",control="--master=fr_FR")
#ne marche pas encore

#troisieme phase : suppression des mots de liaison
myStopwords <- c(stopwords('english'))  #rajout de stopword : [rm]
#myStopwords <- setdiff(myStopwords) #suppression de stopwords
corpus <- tm_map(corpus, removeWords, myStopwords)
#corpus <- tm_map(corpus, PlainTextDocument)
#need suppression des espaces

#quatrieme phase : racinisation, lemmitisation
#corpus <- tm_map(corpus, stemDocument, language = "french") #ne pas faire ça avant les nuages de mots, sinon c'est moche

ndocs <- length(corpus)
# ignore overly sparse terms (appearing in less than 1% of the documents)
minDocFreq <- ndocs * 0.01
# ignore overly common terms (appearing in more than 80% of the documents)
maxDocFreq <- ndocs * 0.8
myDtmUnigram<- DocumentTermMatrix(corpus, control = list(bounds = list(global = c(minDocFreq, maxDocFreq))))


as.matrix(myDtmUnigram)[1:6,1:4]
findFreqTerms(myDtmUnigram, lowfreq=100)

#plot des frequences
termFrequency <- rowSums(as.matrix(myDtmUnigram))
# wfU <- data.frame(word=names(termFrequency), freq=termFrequency)
wfU <- data.frame(word=myDtmUnigram$dimnames$Docs, freq=termFrequency)

wfU2<- subset(wfU, freq>10)
wfU2$word <- factor(wfU2$word, levels = wfU2[order(-wfU2$freq), "word"])
ggplot(data=wfU2,aes(word, freq)) + geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=45, hjust=1))

#Nuage de mots global
library("wordcloud")
wordcloud(as.character(wfU2$word), wfU2$freq, min.freq=70,random.order=FALSE)

# == 404 - relation entre cat2/3 et mot 'case' dans le nom ==

library(arules)
dataset$est_case = grepl("case", tolower(dataset$name))

ap_res = apriori(dataset %>% select(est_case, cat2, cat3))
inspect(head(sort(ap_res), n=10))

# Bonne corrélation

dataset %>% filter(est_case) %>% select(log_price) %>% summary()
dataset %>% filter(cat3 == "Cases, Covers & Skins") %>% select(log_price) %>% summary()

# == 405 - Mots les plus fréquents par cat3 ==
cat3_name = "Cases, Covers & Skins"
cat3_name = "Games"
aaa = dataset[dataset$cat3 == cat3_name, ]
aaa =aaa[,"name"] %>% tolower()
all_names = paste(aaa, collapse = '')
all_names = strsplit(all_names, split = ' ')
freq_names_word = all_names %>% table() %>% as.data.frame() %>% arrange(-Freq)
names(freq_names_word) = c("mot", "freq")
freq_names_word = freq_names_word %>% filter(!mot %in% stopwords())
freq_names_word %>% head(20)

length(all_names) ; class(all_names) ; typeof(all_names)

# == 500 - Essai avec quanteda ==

library(quanteda)

all_names = corpus(dataset$name)
df2=data.frame(dfm(all_names))

# == 600  ==

no_description_price = dataset %>%
  filter(item_description %>% is.na())

no_brand_price = dataset %>%
  filter(brand_name %>% is.na())

no_description_price$price %>% sort() %>% summary()
no_brand_price$price %>% sort() %>% summary()
dataset$price %>% sort() %>% summary()

# == 601 - prix median par cat3

stat_cat3 = dataset %>%
  group_by(cat3, item_condition_id) %>%
  summarise(prix_median = log_price %>% median()) %>%
  ungroup()

ggplot(stat_cat3 %>% filter(cat3 == "Laptops & Netbooks"), aes(x = item_condition_id, y = prix_median, group = cat3)) + 
  geom_line() + 
  theme_bw()
