# ======================================================================
# FU-SAU
# 03_build_cohort.R
# Construction de la cohorte analytique
# ======================================================================
#
# Objectif
# --------
# Construire une cohorte analytique au niveau patient à partir des
# données PASS et AVIS nettoyées.
#
# Une ligne de la table finale correspond à un patient.
#
# Le script :
#   - importe les données nettoyées ;
#   - vérifie leur cohérence ;
#   - fusionne PASS et AVIS ;
#   - construit les variables au niveau passage ;
#   - agrège les informations au niveau patient ;
#   - crée les variables analytiques ;
#   - définit les Frequent Users ;
#   - exporte la cohorte finale.
#
# Entrées
# --------
# data/processed/pass_clean.rds
# data/processed/avis_clean.rds
#
# Sorties
# --------
# data/processed/cohort.rds
# data/exports/cohort_pvalue.csv
#
# Auteur : Léopold ENGELSTEIN
# Date : 02/08/2026
# ======================================================================



# ======================================================================
# 0. Chargement du projet
# ======================================================================



# ======================================================================
# 1. Import des données
# ======================================================================



# ======================================================================
# 2. Contrôle qualité des données
# ======================================================================
#
# Vérifications :
#   - dimensions des jeux de données
#   - nombre de patients
#   - nombre de passages
#   - nombre d'avis
#   - passages sans avis
#   - avis sans passage
#   - nombre maximal d'avis par passage
#



# ======================================================================
# 3. Fusion PASS + AVIS
# ======================================================================
#
# Objectif :
# Construire une table "pass_enrichi"
#
# Une ligne = un passage aux urgences
#



# ======================================================================
# 4. Variables au niveau passage
# ======================================================================
#
# Variables calculées pour chaque passage :
#   - premier avis
#   - dernier avis
#   - diagnostics
#   - nombre d'avis
#   - délais
#   - hospitalisation
#   - fenêtre glissante 365 jours
#



# ======================================================================
# 5. Construction de la cohorte patient
# ======================================================================
#
# Agrégation des passages
#
# Une ligne = un patient
#



# ======================================================================
# 6. Variables démographiques
# ======================================================================



# ======================================================================
# 7. Variables de recours aux urgences
# ======================================================================



# ======================================================================
# 8. Variables psychiatriques
# ======================================================================
#
# Diagnostics
# Classes CIM-10
# Nombre de diagnostics
# Diagnostic majoritaire
# Pondération diagnostique
#



# ======================================================================
# 9. Variables organisationnelles
# ======================================================================
#
# Secteur psychiatrique
# Hôpital de secteur
# Département
# Hospitalisation
# Orientation
#



# ======================================================================
# 10. Définition des Frequent Users
# ======================================================================
#
# Définition principale
# Analyses complémentaires
# Fenêtre glissante
#



# ======================================================================
# 11. Contrôle qualité de la cohorte
# ======================================================================
#
# Vérifications finales
# Statistiques descriptives
# Valeurs manquantes
# Cohérence des variables
#



# ======================================================================
# 12. Sauvegarde de la cohorte
# ======================================================================



# ======================================================================
# 13. Export pour les analyses statistiques
# ======================================================================



# ======================================================================
# Fin du script
# ======================================================================