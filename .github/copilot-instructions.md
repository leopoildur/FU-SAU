# Instructions projet FU-SAU

## Langue

- Répondre en français.
- Expliquer les concepts pédagogiquement.
- Les commentaires peuvent être en français.

## Style R

- Utiliser le tidyverse moderne.
- Utiliser le pipe natif `|>`.
- Respecter le tidyverse style guide.
- Utiliser des noms de variables explicites.
- Produire du code lisible et pédagogique.

## Contexte projet

- Projet de recherche clinique PMSI psychiatrique.
- Étude rétrospective monocentrique.
- Analyse des frequent attenders aux urgences psychiatriques.
- Variables principales : IDPAT, IDdos, diagnostics CIM-10, passages urgences.

## Préférences analytiques

- Préférer `dplyr`, `ggplot2`, `lubridate`, `gtsummary`.
- Éviter le code inutilement complexe.
- Expliquer les étapes statistiques.
- Favoriser les pipelines reproductibles.

## Bonnes pratiques

- Ne jamais utiliser de chemins absolus Windows.
- Utiliser `here::here()`.
- Générer du code compatible Quarto.
- Produire des commentaires explicatifs.

Instructions projet FU-SAU
Contexte général

Projet de recherche clinique en psychiatrie réalisé sous R et Positron.

Étude observationnelle rétrospective monocentrique basée sur des données PMSI des urgences psychiatriques du CHU Henri Mondor.

Objectif principal :
identifier les facteurs associés au recours fréquent aux urgences psychiatriques.

Structure des données

Deux tables principales :

PASS
unité statistique = passage aux urgences
1 ligne = 1 passage unique
identifiant passage = i_ddos
identifiant patient = idpat
AVIS
unité statistique = avis psychiatrique
plusieurs avis possibles pour un même passage
relation PASS 1 -> N AVIS
relation PATIENT 1 -> N PASS
LEGEND
dictionnaire des variables
Définition des frequent attenders

Frequent attender :




= au moins 4 passages psychiatriques sur 12 mois

Unité statistique principale :

patient
Workflow analytique attendu
Import des données
Vérification relationnelle
Nettoyage PMSI
Construction cohorte psychiatrique
Agrégation au niveau patient
Analyses descriptives
Régression logistique
Figures ggplot2
Tables publication-ready
Préférences R
utiliser tidyverse moderne
utiliser le pipe natif |>
produire du code lisible et pédagogique
utiliser des noms de variables explicites
éviter les chemins absolus Windows
utiliser here::here()
utiliser janitor::clean_names()
Packages privilégiés
tidyverse
dplyr
ggplot2
readxl
lubridate
skimr
gtsummary
gt
janitor
Style de code
commentaires en français
noms de variables en anglais
sections de scripts avec # Section ----
code reproductible
éviter les jointures dupliquant artificiellement les passages
Points méthodologiques importants
attention aux duplications PASS/AVIS
plusieurs avis possibles par passage
toujours préciser le niveau d’agrégation statistique
privilégier semi_join() quand pertinent
vérifier systématiquement les identifiants uniques