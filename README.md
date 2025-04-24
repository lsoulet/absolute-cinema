# Analyse des Établissements Cinématographiques en France

Cette application Shiny propose une analyse interactive des établissements cinématographiques en France, basée sur les données publiques du CNC (Centre National du Cinéma et de l'image animée).

## Comment lancer l'application

### Depuis RStudio

1. Ouvrez le fichier `app.R` dans RStudio
2. Cliquez sur le bouton "Run App" dans la barre d'outils (ou utilisez le raccourci Ctrl+Shift+Enter)
3. L'application s'ouvrira soit dans une nouvelle fenêtre, soit dans le viewer intégré de RStudio

## Dépendances

L'application utilise les packages R suivants :
- shiny
- ggplot2
- tidyverse
- DT
- plotly
- shinydashboard
- shinyWidgets

Installez-les si nécessaire avec :
```r
install.packages(c("shiny", "ggplot2", "tidyverse", "DT", "plotly", "shinydashboard", "shinyWidgets"))
```

## Structure de l'application

L'application est organisée en quatre onglets principaux :

1. **Accueil** : Présente les indicateurs clés et la distribution des établissements par type et nombre d'écrans
2. **Vue d'ensemble** : Offre une analyse comparative des régions et la répartition géographique des cinémas
3. **Analyse régionale** : Permet d'explorer en détail les caractéristiques des cinémas d'une région spécifique
4. **Données brutes** : Affiche les données complètes sous forme de tableau interactif

## Choix de visualisation

### Filtres globaux
- Un filtre par région dans la barre latérale qui s'applique à toutes les visualisations
- Des filtres par type d'établissement et nombre d'écrans pour affiner l'analyse

### Visualisations clés
- **Indicateurs synthétiques** : Nombre d'établissements, d'écrans, d'entrées et taux d'occupation moyen
- **Répartition par type** : Distinction entre multiplexes et cinémas classiques, établissements Art et Essai
- **Performance des régions** : Graphique multidimensionnel montrant le nombre d'établissements, d'écrans, d'entrées et l'efficacité commerciale (entrées par écran) pour chaque région
- **Analyse régionale détaillée** : Distribution des entrées et répartition par type d'établissement au sein d'une région sélectionnée

### Particularités techniques
- Gestion des caractères accentués dans les noms de colonnes
- Conversion explicite des colonnes numériques
- Création de variables dérivées pour enrichir l'analyse (taille d'établissement, type de cinéma, etc.)
- Tooltips interactifs pour une exploration approfondie des données

## Source des données

Les données proviennent du fichier `etablissements-cinematographiques.csv` situé dans le dossier `data/`. Ce jeu de données contient des informations détaillées sur les cinémas français, incluant leur localisation, capacité, fréquentation et programmation.
