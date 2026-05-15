#! 02.2_cleaning_pass.R ================================================
#! Nettoyage table PASS
#! =====================================================================

renv::activate()

library("tidyverse")
library("janitor")
library("lubridate")
library("here")

source(
  here("R", "01_import.R")
)

#! TABLE PASS ===========================================================
#! Unité statistique = passage aux urgences
#! =====================================================================



# Structure générale PASS ===============================================

# TODO: dimensions

# TODO: types des variables

# TODO: résumé statistique

# TODO: données manquantes globales



# Identifiants PASS =====================================================

# TODO: vérifier unicité passages

# TODO: vérifier multi-recours



# Variables démographiques PASS =========================================

# TODO: âge

# TODO: sexe

# TODO: commune

# TODO: département

# TODO: couverture sociale



# Variables temporelles PASS ============================================

# TODO: date entrée

# TODO: heure entrée

# TODO: date sortie

# TODO: heure sortie

# TODO: durée passage

# TODO: passage nocturne

# TODO: week-end

# TODO: saison



# Variables médicales PASS ==============================================

# TODO: motif recours

# TODO: diagnostic principal

# TODO: diagnostics associés

# TODO: codes CIM-10

# TODO: gravité clinique

# TODO: alcoolisation

# TODO: intoxication

# TODO: tentative suicide



# Variables organisationnelles PASS =====================================

# TODO: mode entrée

# TODO: provenance

# TODO: mode sortie

# TODO: orientation

# TODO: hospitalisation

# TODO: mutation



# Variables de recours PASS =============================================

# TODO: recours répétés

# TODO: frequent flyers

# TODO: délai entre passages



# Harmonisation variables catégorielles PASS ============================

# TODO: uniformiser modalités

# TODO: gérer accents

# TODO: gérer espaces

# TODO: gérer majuscules / minuscules

# TODO: regrouper modalités rares



# Valeurs manquantes PASS ===============================================

# TODO: quantifier NA

# TODO: analyser patterns de NA

# TODO: identifier variables critiques

# TODO: décisions méthodologiques



# Valeurs aberrantes PASS ===============================================

# TODO: détecter âges aberrants

# TODO: détecter durées aberrantes

# TODO: détecter dates incohérentes

# TODO: détecter modalités impossibles

# TODO: détecter valeurs extrêmes



# Variables dérivées PASS ===============================================

# TODO: catégories âge

# TODO: temporalité recours

# TODO: variables psychiatriques synthétiques

# TODO: variables organisationnelles synthétiques

# TODO: variables analytiques finales



# Vérification relationnelle PASS / AVIS ================================

# TODO: cohérence jointure

# TODO: nombre avis par passage

# TODO: passages sans avis

# TODO: avis sans passage



# Sauvegarde données nettoyées ==========================================

# TODO: sauvegarder avis_clean

# TODO: sauvegarder pass_clean



# Contrôle qualité final global =========================================

# TODO: vérifier dimensions finales

# TODO: vérifier cohérence globale

# TODO: résumé du cleaning



#! Fin du script ========================================================