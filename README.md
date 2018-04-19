# Etude sur les prix de produits électroniques vendus entre particuliers

<b> Auteurs : Mohamed Maftah (@Simo--) & Mehdi Miah (@Guepardow) </b> <br>
<i> Polytechnique Montréal - IND6212 Exploration de données industrielles - B. Agard - Hiver 2018 </i>

## Introduction

Ce sujet a été inspiré par le challenge Mercari Price Suggestion sur Kaggle.
En ne considérant que les produits électroniques, l'objectif de cette étude est de prédire le prix d'un article.

<b> Sujet sur Kaggle : </b> https://www.kaggle.com/c/mercari-price-suggestion-challenge/data

## Organisation des fichiers

- `./data` : contient l'ensemble des données initiales et traitées
- `./report` : contient les documents relatifs au rapport pdf
- `./results` : contient les images résultant de la phase d'analyse des données ainsi que le modèle de random forest

## Principaux résultats

### Sur les données

<b> Biais sur l'état d'un produit </b>

On pourrait penser que plus un produit est usé, moins il est cher, mais ce n'est pas forcément le cas lorsque l'on regarde les statistiques globales. En effet, il existe une corrélation entre le prix d'un produit et son état. Seuls les produits ayant une certaine valeur d'achat se voit mettre en vente en occasion ; pour les autres produits, peu chers, les utilisateurs préfèrent les jeter.

Cette corrélation est visible dans la figure ci-dessus qui montre que pour une sous-catégorie de produits, plus le prix médian est élevé, plus la part de produits d'occasion est forte.

![alt tag](https://github.com/Guepardow/Mercari/blob/master/results/figure/prixMedian_occasion_cat3_repel.png)

<b> Biais sur les frais d'envoi d'un produit </b>

De même, on pourrait penser qu'un produit envoyé à la charge du vendeur est plus cher qu'un autre qui ne l'est pas. Non : les statistiques globales montrent qu'un produit envoyé à la charge du vendeur coûte moins cher. Cela est dû au fait qu'il existe une corrélation entre le prix d'un produit et ses conditions d'envoi. Plus un produit est peu cher, plus la probabilité que les frais d'envoi soient comprises est élevée.

Cette corrélation est illustrée dans la figure ci-dessus qui montre que pour une sous-catégorie de produits, plus le prix médian est élevé, plus les frais d'envoi sont à la charge de l'acheteur.

![alt tag](https://github.com/Guepardow/Mercari/blob/master/results/figure/prixMedian_shipping_cat3_repel.png)

<b> Part de produits correctement classifiés par l'utilisateur </b>

Les utilisateurs ne classifient pas toujours les produits dans les bonnes catégories. Ainsi, en représentant la part des produits utilisant les termes "iphone" et/ou "case" (voir la figure ci-dessous), il s'avère que près de 10% des produits de "Cell Phones & Smartphones" peuvent correspondre à des étuis/accessoires.

![alt tag](https://github.com/Guepardow/Mercari/blob/master/results/figure/produits_mal_classes.png)

<b> Association entre mots et valeur monétaire d'un produit </b>

Il est possible d'associer une valeur monétaire à des mots à travers des règles d'association. Certaines de ces règles sont listées ci-dessous.

![alt tag](https://github.com/Guepardow/Mercari/blob/master/results/figure/rules.png)

### Sur les modèles de prédiction

<b> Score de prédiction d'une forêt aléatoire </b>

Sur la base des produits électroniques, l'erreur de prédiction est de 0.53. Le modèle a été construit à partir des noms des produits, des catégories, des frais d'envoi, de l'état du produit et de sa marque. La forêt comprend 40 arbres à 40 variables chacune.

Temps d'apprentissage du modèle : 50 minutes

<b> Points forts et faibles du forêt aléatoire </b>

Le modèle de forêt aléatoire permet une sorte d'auto-nettoyage : même si un produit est mal classifié, l'algorithme arrive tant bien que mal à distinguer les produits chers des autres. 

Cependant, ce modèle n'indique pas clairement l'impact des variables sur le prix du produit. Pour cela, il faut se référer aux analyses descriptives et aux règles d'association. De plus, il est sensible aux biais présentés ci-dessus : l'effet de l'état et des frais d'envoi n'est pas compris par le modèle.
