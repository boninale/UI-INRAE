# README — Application d’Analyse des Consommations du Campus

## 1. Présentation

Application R Shiny destinée à analyser et visualiser les données de consommation énergétique du campus. Elle reproduit les traitements du notebook R `2025_Analyse_Fluide_MTP.qmd`, intègre une standardisation automatique des fichiers, et offre une interface web pour importer les données, paramétrer les prix unitaires et consulter les résultats.

Démo en ligne : <https://boninale.shinyapps.io/Analyse-Consommation-Institut-Agro/>

------------------------------------------------------------------------

## 2. Structure du dépôt

```         
app.R                      # Script principal (UI + server)

R/
    data_processing.R      # Chaîne de traitement principale
    data_standardisation.R # Standardisation des colonnes des fichiers d'entrée
    plot.R                 # Fonctions de génération des graphiques

data/                      # Exemples de fichiers d'entrée
Results/                   # Exemples de sorties générées
```

------------------------------------------------------------------------

## 3. Installation locale

Prérequis :

-   R ≥ 4.2
-   RStudio recommandé
-   Packages utilisés : shiny, tidyverse, readxl, lubridate, ggplot2, etc.

Installation :

```         
git clone https://github.com/boninale/App-Analyse-Consommation.git
cd App-Analyse-Consommation
```

Lancement en local :

```         
R -e "shiny::runApp()"
```

Ou via RStudio : ouvrir `app.R` puis cliquer sur "Run App".

------------------------------------------------------------------------

## 4. Standardisation automatique des données

La normalisation est gérée par `data_standardisation.R`. Les règles appliquées à toute table importée sont :

1.  La première colonne est forcée sous le nom `batiment`.
2.  Toute colonne contenant une année + « conso » → `année_conso`. Exemple : `Conso 2023 (kWh)` → `2023_conso`.
3.  Colonnes contenant deux années sans symbole % → `Evol_année1-année2`. Exemple : `Différence 2022 et 2023` → `Evol_2022-2023`.
4.  Colonnes contenant deux années et un symbole % → `Evol_année1-année2_p`. Exemple : `Evol 2023/2024 en %` → `Evol_2023-2024_p`.
5.  Parmi les colonnes `*_conso`, celle dont l’année est la plus élevée est dupliquée en fin de tableau sous le nom `derniere_conso`.

Cette standardisation garantit la compatibilité avec des fichiers aux structures hétérogènes.

------------------------------------------------------------------------

## 5. Traitement analytique

Le script `data_processing.R` :

• importe et nettoie les fichiers, • applique le filtrage de l’onglet `Occupation` (suppression lorsque `A SUPPRIMER` ≠ "NON"), • prépare les données normalisées, • calcule les agrégats et indicateurs de consommation, • applique les prix unitaires définis dans l’interface, • génère les tableaux consolidés et les jeux de données finaux.

Chaque exécution produit un ensemble complet de sorties reproductibles.

------------------------------------------------------------------------

## 6. Visualisations

Les graphiques sont générés via `plot.R` :

• évolutions interannuelles des consommations, • comparaisons entre bâtiments, • représentations des variations absolues et en pourcentage, • graphiques consolidés pour le tableau de bord Shiny.

Les figures apparaissent dans l’onglet "Tableaux de Bord" de l’application.

------------------------------------------------------------------------

## 7. Déploiement sur ShinyApps.io

Pour déployer :

1.  Installer le package rsconnect.
2.  Configurer le compte ShinyApps.
3.  Lancer le déploiement :

```         
rsconnect::deployApp()
```

Les journaux d’erreurs sont consultables dans l’interface ShinyApps.

------------------------------------------------------------------------

## 8. Exemple d’utilisation

1.  Déposer les fichiers d’entrée dans la section « Fichiers et Export ».
2.  Paramétrer les prix unitaires si nécessaire.
3.  Lancer l’analyse.
4.  Télécharger les résultats produits dans le répertoire dédié.
5.  Consulter les graphiques dans l’onglet « Tableaux de Bord ».

------------------------------------------------------------------------
