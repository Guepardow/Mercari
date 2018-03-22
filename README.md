# Etude sur les prix de ventes de produits électroniques entre particuliers

<b> Auteurs : Mohamed Maftah (@Simo--) & Mehdi Miah (@Guepardow) </b>
<i> Polytechnique Montréal - IND6212 Exploration de données industrielles, B. Agard - Hiver 2018 </i>

## Introduction

Ce sujet a été inspiré par le challenge Mercari Price Suggestion sur Kaggle.

<b> Sujet sur Kaggle : </b> https://www.kaggle.com/c/mercari-price-suggestion-challenge/data

<b> Lien vers le rapport LaTeX (temporaire)</b>
https://www.overleaf.com/13832130xkrqpchckkvr#/53574512/

## Organisation des fichiers

- `./data` : contient l'ensemble des données initiales et traitées
- `./src` : contient l'ensemble du code R
- `./report` : contient les documents relatifs au rapport pdf
- `./results` : contient les images résultant de la phase d'analyse des données

## Pistes de réflexion
- identifier les produits de luxe
- identifier les catégories (level1,2, 3)
- extraire à partir des textes : corrélation entre mots clés et prix
- on laisse les 0 et les +1000$ (cf la métrique)
- prix moyen par famille de produits
- lien entre les prix et l'etat du produit
- lien entre le nombre de demandes et le prix moyen
- modele linéaire
- modele de réseau bayésien
- variance des prix par catégorie
- créer des clusters de produits (éviter d'avoir des milliers de groupes)

## Rappel des deadlines
- 5 avril : présentation orale (5 min)
- 12 avril : examen écrit
- 19 avril 16h : dépôt de la base de données et du rapport pdf
