# Projet de thèse – Frequent Users des urgences psychiatriques

## Question scientifique

Décrire les caractéristiques et trajectoires des patients utilisateurs
fréquents des urgences pour motif psychiatrique et comparer leur profil
à celui des non-utilisateurs fréquents.

## Population

Patients adultes ayant bénéficié d'au moins un avis psychiatrique
au SAU du CHU Henri-Mondor entre le 01/01/2021 et le 31/12/2025.

## Définition du Frequent User

Frequent User = au moins 4 passages avec évaluation psychiatrique
dans une fenêtre glissante de 365 jours.

## Niveau des données

AVIS : une ligne = un avis psychiatrique.
PASSAGE : une ligne = un passage aux urgences.
COHORTE : une ligne = un patient.

## Diagnostics

Les diagnostics sont regroupés en grandes catégories CIM-10.
Les variables F1, F2, F3, F4 et F6 sont notamment utilisées.

Un patient est considéré comme présentant une catégorie diagnostique
si celle-ci est retrouvée au moins une fois selon la règle définie
dans le script de construction de cohorte.

## Analyse statistique

Variables quantitatives :
médiane [IQR], moyenne (écart-type) rapportée lorsque pertinent.

Variables qualitatives :
n (%).

Comparaisons :
- χ² ou Fisher pour les qualitatives ;
- Mann-Whitney ou Student selon les conditions pour les quantitatives.

Seuil de significativité : p < 0,05.

Régression logistique multivariée envisagée pour identifier les facteurs
indépendamment associés au statut de Frequent User.

## Règles importantes

Ne jamais modifier les données sources.

Ne jamais écraser les objets intermédiaires.

Ne pas changer une définition méthodologique sans le signaler.

Ne pas supprimer des observations sans expliciter la raison.

Ne pas imputer les données manquantes.

Toutes les transformations doivent être reproductibles.