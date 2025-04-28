Sys.setlocale("LC_ALL", "fr_FR.UTF-8")

library(shiny)
library(ggplot2)
library(tidyverse)
library(DT)
library(plotly)
library(shinydashboard)
library(shinyWidgets)
library(fresh)
library(fontawesome)
library(waiter)

data <- read_delim("data/etablissements-cinematographiques.csv", delim = ";", locale = locale(encoding = "UTF-8"))

print("Noms de colonnes originaux:")
print(names(data))

data <- data %>%
  mutate(
    ecrans = as.numeric(écrans),
    fauteuils = as.numeric(fauteuils),
    `entrées 2022` = as.numeric(`entrées 2022`),
    `entrées 2021` = as.numeric(`entrées 2021`),
    `semaines d'activité` = as.numeric(`semaines d'activité`),
    `nombre de films programmés` = as.numeric(`nombre de films programmés`)
  )

data <- data %>%
  mutate(
    taille_etablissement = case_when(
      ecrans == 1 ~ "Mono-écran",
      ecrans >= 2 & ecrans <= 4 ~ "Petit complexe (2-4 écrans)",
      ecrans >= 5 & ecrans <= 7 ~ "Complexe moyen (5-7 écrans)",
      ecrans >= 8 ~ "Grand complexe (8+ écrans)",
      TRUE ~ "Non défini" # Pour gérer les NA
    ),
    taille_etablissement = factor(taille_etablissement, 
                                  levels = c("Mono-écran", "Petit complexe (2-4 écrans)", 
                                             "Complexe moyen (5-7 écrans)", "Grand complexe (8+ écrans)", "Non défini")),
    type_cinema = ifelse(multiplexe == "OUI", "Multiplexe", "Cinéma classique"),
    statut_ae = ifelse(AE == "OUI", "Art et Essai", "Non Art et Essai"),
    taux_occupation = `entrées 2022` / (fauteuils * `semaines d'activité` * 7 * 3),
    evolution_covid = (`entrées 2022` - `entrées 2021`) / `entrées 2021` * 100
  )

# Création du dossier www s'il n'existe pas
if (!dir.exists("www")) {
  dir.create("www")
}

# Création d'un thème personnalisé avec fresh
cinema_theme <- create_theme(
  adminlte_color(light_blue = "#3C8DBC"),
  adminlte_sidebar(width = "240px", dark_bg = "#1A237E", dark_hover_bg = "#303F9F", dark_color = "#FFFFFF"),
  adminlte_global(content_bg = "#F5F5F5", box_bg = "#FFFFFF", info_box_bg = "#FFFFFF"),
  adminlte_vars(border_radius = "3px", box_border_radius = "5px", box_shadow_size = "0 2px 5px rgba(0,0,0,0.1)"),
  output_file = "www/cinema_theme.css"
)

# CSS personnalisé
custom_css <- tags$head(
  tags$style(HTML("
    .skin-blue .main-header .logo { font-family: 'Montserrat', sans-serif; font-weight: bold; }
    .box { border-top: 3px solid #3C8DBC; box-shadow: 0 2px 10px rgba(0,0,0,0.1); }
    .box-header { border-bottom: 1px solid #f4f4f4; }
    .value-box { box-shadow: 0 2px 10px rgba(0,0,0,0.1); transition: transform 0.3s; }
    .value-box:hover { transform: translateY(-5px); }
    .welcome-text { font-size: 16px; line-height: 1.6; }
    .highlight { color: #3C8DBC; font-weight: bold; }
    .author-box { background-color: #f9f9f9; padding: 15px; border-radius: 5px; }
    .author-name { font-weight: bold; color: #3C8DBC; }
    .section-title { border-bottom: 2px solid #3C8DBC; padding-bottom: 8px; margin-bottom: 20px; }
    .stat-box { text-align: center; padding: 15px; background: #fff; border-radius: 5px; box-shadow: 0 2px 5px rgba(0,0,0,0.05); }
    .stat-value { font-size: 24px; font-weight: bold; color: #3C8DBC; }
    .stat-label { font-size: 14px; color: #777; }
    .cinema-icon { margin-right: 8px; color: #3C8DBC; }
  "))
)

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = tags$span(
      tags$i(class = "fa fa-film", style = "margin-right: 10px;"), 
      "Absolute Cinema"
    ),
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Accueil", tabName = "accueil", icon = icon("home")),
      menuItem("Analyse régionale", tabName = "regional", icon = icon("chart-bar")),
      menuItem("Typologie des cinémas", tabName = "typologie", icon = icon("film")),
      menuItem("Programmation", tabName = "programmation", icon = icon("ticket-alt")),
      menuItem("Données brutes", tabName = "donnees", icon = icon("table"))
    ),
    br(),
    div(style = "padding: 0 15px;",
        h4("Filtres", class = "section-title"),
        pickerInput(
          "region_filter", tags$span(icon("map-marker-alt"), "Filtrer par région :"),
          choices = c("Toutes les régions" = "all", sort(unique(data$`région administrative`))),
          selected = "all",
          multiple = TRUE,
          options = list(`actions-box` = TRUE, `live-search` = TRUE)
        ),
        checkboxGroupInput(
          "type_filter", tags$span(icon("building"), "Type d'établissement :"),
          choices = c("Multiplexe", "Cinéma classique"),
          selected = c("Multiplexe", "Cinéma classique")
        ),
        sliderInput(
          "ecrans_filter", tags$span(icon("desktop"), "Nombre d'écrans :"),
          min = 1, max = max(data$ecrans, na.rm = TRUE),
          value = c(1, max(data$ecrans, na.rm = TRUE)),
          step = 1
        )
    )
  ),
  
  dashboardBody(
    use_theme(cinema_theme),
    custom_css,
    tabItems(
      # Onglet d'accueil
      tabItem(tabName = "accueil",
              fluidRow(
                div(class = "col-md-12",
                    div(class = "box box-solid", style = "background: linear-gradient(135deg, #1A237E, #3949AB); color: white; border: none; border-radius: 8px;",
                        div(class = "box-body", style = "padding: 30px;",
                            h1("Analyse des établissements cinématographiques en France", style = "font-weight: 700; margin-bottom: 20px; font-size: 28px;"),
                            h3("Projet de Data Visualisation - 2025", style = "font-weight: 300; margin-bottom: 30px;")
                        )
                    )
                )
              ),
              
              fluidRow(
                box(
                  width = 12,
                  status = "primary",
                  solidHeader = FALSE,
                  div(class = "welcome-text",
                      h3(icon("film", class = "cinema-icon"), "Présentation du projet"),
                      p("Cette application interactive présente une analyse approfondie des ", span("2061 établissements cinématographiques", class = "highlight"), 
                        " répartis sur le territoire français, basée sur les données officielles du Centre National du Cinéma et de l'image animée (CNC)."),
                      
                      p("Le cinéma occupe une place essentielle dans le paysage culturel français. Cette étude vise à analyser ",
                        "la structure et la diversité des établissements cinématographiques, depuis les petits cinémas mono-écran jusqu'aux grands multiplexes, ",
                        "afin de mieux comprendre les dynamiques territoriales et économiques qui façonnent ce secteur."),
                      
                      p("La France se distingue par un réseau cinématographique particulier, caractérisé par une forte densité d'établissements ",
                        "et une diversité de programmation notable. Les analyses présentées dans cette application permettent d'identifier ",
                        "les disparités régionales et les facteurs qui influencent la fréquentation et la programmation des cinémas."),
                      
                      div(style = "background-color: #f0f7ff; padding: 15px; border-left: 4px solid #3C8DBC; margin: 20px 0; border-radius: 0 5px 5px 0;",
                          h4("Objectifs de l'étude"),
                          tags$ul(
                            tags$li(icon("map-marked-alt"), " ", strong("Analyse territoriale"), " - Étudier la répartition géographique des établissements cinématographiques en France"),
                            tags$li(icon("building"), " ", strong("Typologie des établissements"), " - Caractériser les différents types de cinémas selon leur taille et leur statut"),
                            tags$li(icon("chart-line"), " ", strong("Analyse de performance"), " - Évaluer les relations entre les caractéristiques structurelles et la fréquentation"),
                            tags$li(icon("globe-europe"), " ", strong("Diversité culturelle"), " - Examiner la répartition des films selon leur origine et leur classification")
                          )
                      ),
                      
                      p("Cette application permet d'explorer les données à travers différentes visualisations interactives. ",
                        "Les filtres disponibles dans le panneau latéral permettent d'affiner l'analyse selon les régions, ",
                        "les types d'établissements ou le nombre d'écrans."),
                      
                      div(style = "margin-top: 30px;",
                          h4("Méthodologie et sources", class = "section-title"),
                          p("Cette étude s'appuie sur les données officielles du ", strong("Centre National du Cinéma et de l'image animée (CNC)"), 
                            ", l'organisme public chargé de la régulation et du soutien au secteur cinématographique en France. ",
                            "Le jeu de données comprend 40 variables couvrant :"),
                          tags$ul(
                            tags$li("L'identification et la localisation des établissements"),
                            tags$li("Les caractéristiques techniques (nombre d'écrans, capacité d'accueil)"),
                            tags$li("Les données de fréquentation (entrées 2021-2022)"),
                            tags$li("Les informations sur la programmation et l'orientation artistique")
                          )
                      ),
                      
                      div(class = "author-box", style = "margin-top: 30px;",
                          h4("Auteurs", class = "section-title"),
                          p("Ce projet a été réalisé dans le cadre d'un cours de Data Visualisation par :"),
                          div(style = "display: flex; justify-content: space-around; flex-wrap: wrap; margin-top: 15px;",
                              div(class = "stat-box", style = "flex: 1; min-width: 200px; margin: 10px;",
                                  icon("user", style = "font-size: 24px; color: #3C8DBC;"),
                                  h4(class = "author-name", "Axel Frache")
                              ),
                              div(class = "stat-box", style = "flex: 1; min-width: 200px; margin: 10px;",
                                  icon("user", style = "font-size: 24px; color: #3C8DBC;"),
                                  h4(class = "author-name", "Nathan Dilhan")
                              ),
                              div(class = "stat-box", style = "flex: 1; min-width: 200px; margin: 10px;",
                                  icon("user", style = "font-size: 24px; color: #3C8DBC;"),
                                  h4(class = "author-name", "Liam Soulet")
                              )
                          ),
                          p(style = "text-align: center; margin-top: 15px;", "Polytech Montpellier - 2025")
                      )
                  )
                )
              ),
              fluidRow(
                div(class = "col-md-12",
                    h3("Les chiffres clés", class = "section-title", style = "margin-top: 20px;")
                )
              ),
              fluidRow(
                valueBoxOutput("total_cinemas", width = 3),
                valueBoxOutput("total_ecrans", width = 3),
                valueBoxOutput("total_entrees", width = 3),
                valueBoxOutput("evolution_moyenne", width = 3)
              ),
              fluidRow(
                box(
                  title = "Répartition des établissements par région",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("plot_regions")
                ),
                box(
                  title = "Distribution du nombre d'écrans",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("plot_ecrans")
                )
              ),
              fluidRow(
                box(
                  title = "Vue d'ensemble des régions (établissements, écrans et entrées)",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("plot_regions_detail", height = 500)
                )
              )
      ),
      
      # Onglet analyse régionale
      tabItem(tabName = "regional",
              fluidRow(
                div(class = "col-md-12",
                    div(class = "box box-solid", style = "background: #f5f5f5; border-left: 4px solid #3C8DBC; margin-bottom: 20px; padding: 15px;",
                        h4("Analyse régionale", style = "margin-top: 0;"),
                        p("Cette section permet d'analyser en détail les caractéristiques des établissements cinématographiques par région. ",
                          "Sélectionnez une région pour explorer ses statistiques spécifiques, la distribution des entrées et la répartition des types d'établissements. ",
                          "Cette analyse régionale permet d'identifier les disparités territoriales et les spécificités locales du paysage cinématographique français.")
                    )
                )
              ),
              fluidRow(
                box(
                  title = "Sélection de la région",
                  width = 3,
                  status = "primary",
                  solidHeader = TRUE,
                  selectInput("region_detail", "Région :", 
                              choices = sort(unique(data$`région administrative`)))
                ),
                box(
                  title = "Statistiques régionales",
                  width = 9,
                  status = "primary",
                  solidHeader = TRUE,
                  tableOutput("stats_region")
                )
              ),
              fluidRow(
                box(
                  title = "Distribution des entrées",
                  width = 6,
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("plot_entrees_region")
                ),
                box(
                  title = "Répartition par type d'établissement",
                  width = 6,
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("plot_types_region")
                )
              )
      ),
      
      # Onglet typologie des cinémas
      tabItem(tabName = "typologie",
              fluidRow(
                div(class = "col-md-12",
                    div(class = "box box-solid", style = "background: #f5f5f5; border-left: 4px solid #3C8DBC; margin-bottom: 20px; padding: 15px;",
                        h4("Typologie des établissements cinématographiques", style = "margin-top: 0;"),
                        p("Cette section analyse les différents types d'établissements cinématographiques et leurs caractéristiques. ",
                          "Les visualisations permettent de comparer les multiplexes et cinémas classiques, d'étudier la relation entre capacité d'accueil et fréquentation, ",
                          "et d'analyser les taux d'occupation selon le type d'établissement. Cette analyse typologique met en évidence les différents modèles économiques ",
                          "qui coexistent dans le secteur cinématographique français.")
                    )
                )
              ),
              fluidRow(
                box(
                  title = "Comparaison des types d'établissements",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("plot_typologie")
                )
              ),
              fluidRow(
                box(
                  title = "Relation entre capacité et fréquentation",
                  width = 6,
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("plot_capacite_freq")
                ),
                box(
                  title = "Taux d'occupation par type d'établissement",
                  width = 6,
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("plot_occupation")
                )
              )
      ),
      
      # Onglet programmation
      tabItem(tabName = "programmation",
              fluidRow(
                div(class = "col-md-12",
                    div(class = "box box-solid", style = "background: #f5f5f5; border-left: 4px solid #3C8DBC; margin-bottom: 20px; padding: 15px;",
                        h4("Analyse de la programmation", style = "margin-top: 0;"),
                        p("Cette section examine la programmation des établissements cinématographiques en France. ",
                          "Les visualisations présentent la répartition des films selon leur origine (français, américains, européens, autres), ",
                          "le nombre de films programmés par type d'établissement, et la place du cinéma Art et Essai. ",
                          "Cette analyse de programmation permet d'évaluer la diversité culturelle de l'offre cinématographique et les spécificités de diffusion selon les types d'établissements.")
                    )
                )
              ),
              fluidRow(
                box(
                  title = "Parts de marché par origine des films",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("plot_pdm")
                )
              ),
              fluidRow(
                box(
                  title = "Nombre de films programmés par type d'établissement",
                  width = 6,
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("plot_films_programmes")
                ),
                box(
                  title = "Part de marché des films Art et Essai",
                  width = 6,
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("plot_ae")
                )
              )
      ),
      
      # Onglet données brutes
      tabItem(tabName = "donnees",
              fluidRow(
                div(class = "col-md-12",
                    div(class = "box box-solid", style = "background: #f5f5f5; border-left: 4px solid #3C8DBC; margin-bottom: 20px; padding: 15px;",
                        h4("Données brutes", style = "margin-top: 0;"),
                        p("Cette section présente l'ensemble des données brutes sur les établissements cinématographiques en France. ",
                          "Le tableau interactif permet de consulter, filtrer et trier les données selon différents critères. ",
                          "Vous pouvez effectuer des recherches, réorganiser les colonnes et exporter les données pour des analyses complémentaires. ",
                          "Cette vue détaillée donne accès à l'ensemble des variables disponibles pour chaque établissement.")
                    )
                )
              ),
              fluidRow(
                box(
                  title = "Données brutes des établissements cinématographiques",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  DTOutput("table_data")
                )
              )
      )
    )
  )
)

# Backend
server <- function(input, output, session) {
  # Fermer l'écran de chargement si présent
  if (requireNamespace("waiter", quietly = TRUE)) {
    waiter::waiter_hide()
  }
  
  filtered_data <- reactive({
    result <- data
    
    # Filtre par région
    if (!"all" %in% input$region_filter) {
      result <- result %>% filter(`région administrative` %in% input$region_filter)
    }
    
    # Filtre par type d'établissement
    result <- result %>% 
      filter(type_cinema %in% input$type_filter)
    
    # Filtre par nombre d'écrans
    result <- result %>% 
      filter(ecrans >= input$ecrans_filter[1] & ecrans <= input$ecrans_filter[2])
    
    return(result)
  })
  
  # Onglet Accueil - Indicateurs clés avec animations et styles améliorés
  output$total_cinemas <- renderValueBox({
    valueBox(
      formatC(nrow(filtered_data()), format="d", big.mark=" "),
      "Établissements cinématographiques",
      icon = icon("building"),
      color = "blue"
    )
  })
  
  output$total_ecrans <- renderValueBox({
    valueBox(
      formatC(sum(filtered_data()$ecrans, na.rm = TRUE), format="d", big.mark=" "),
      "Écrans à travers la France",
      icon = icon("desktop"),
      color = "green"
    )
  })
  
  output$total_entrees <- renderValueBox({
    total_entrees <- sum(filtered_data()$`entrées 2022`, na.rm = TRUE)
    valueBox(
      paste0(formatC(round(total_entrees / 1e6, 1), format="f", digits=1, big.mark=" "), " millions"),
      "Spectateurs accueillis en 2022",
      icon = icon("ticket-alt"),
      color = "purple"
    )
  })
  
  output$evolution_moyenne <- renderValueBox({
    evol <- mean(filtered_data()$evolution_covid, na.rm = TRUE)
    valueBox(
      paste0(formatC(round(evol, 1), format="f", digits=1), " %"),
      "Évolution de fréquentation 2021-2022",
      icon = icon("chart-line"),
      color = if(evol >= 0) "olive" else "red"
    )
  })
  
  # Graphiques de l'onglet Accueil
  output$plot_regions <- renderPlotly({
    p <- filtered_data() %>%
      count(`région administrative`) %>%
      ggplot(aes(x = reorder(`région administrative`, n), y = n, text = paste(`région administrative`, ":", n, "établissements"))) +
      geom_col(fill = "lightcoral") +
      coord_flip() +
      labs(x = "Région administrative", y = "Nombre d'établissements") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$plot_ecrans <- renderPlotly({
    p <- filtered_data() %>%
      count(type_cinema) %>%
      ggplot(aes(x = reorder(type_cinema, n), y = n, fill = type_cinema, 
                 text = paste(type_cinema, ":", n, "établissements"))) +
      geom_col() +
      coord_flip() +
      labs(x = "Type de cinéma", y = "Nombre d'établissements", 
           title = "Répartition par type de cinéma") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
  
  # Visualisation améliorée de la vue d'ensemble des régions
  output$plot_regions_detail <- renderPlotly({
    region_summary <- filtered_data() %>%
      group_by(`région administrative`) %>%
      summarise(
        `Nombre d'etablissements` = n(),
        `Nombre d'ecrans` = sum(ecrans, na.rm = TRUE),
        `Entrees totales` = sum(`entrées 2022`, na.rm = TRUE)/1000000,
        `Moyenne ecrans` = round(mean(ecrans, na.rm = TRUE), 1),
        `Ratio entrees/ecran` = round(sum(`entrées 2022`, na.rm = TRUE) / sum(ecrans, na.rm = TRUE) / 1000, 1)
      ) %>%
      mutate(
        `Efficacite` = `Ratio entrees/ecran`,
        `label_visible` = ifelse(`Entrees totales` > quantile(`Entrees totales`, 0.7) | 
                                `Nombre d'ecrans` > quantile(`Nombre d'ecrans`, 0.7), 
                                `région administrative`, "")
      )
    
    p <- ggplot(region_summary, 
               aes(x = `Nombre d'etablissements`, y = `Nombre d'ecrans`, 
                   size = `Entrees totales`, color = `Efficacite`,
                   text = paste(`région administrative`, 
                                "<br>Établissements:", `Nombre d'etablissements`,
                                "<br>Écrans:", `Nombre d'ecrans`,
                                "<br>Moyenne écrans/établissement:", `Moyenne ecrans`,
                                "<br>Entrées (millions):", round(`Entrees totales`, 1),
                                "<br>Milliers d'entrées/écran:", `Ratio entrees/ecran`))) +
      geom_point(alpha = 0.8) +
      geom_text(aes(label = `label_visible`), hjust = -0.2, vjust = 0.5, size = 3, color = "black") +
      scale_size(range = c(3, 15), name = "Entrées (millions)") +
      scale_color_viridis_c(name = "Milliers d'entrées<br>par écran", option = "plasma") +
      labs(x = "Nombre d'établissements", y = "Nombre d'écrans",
           title = "Performance des régions cinématographiques") +
      theme_minimal() +
      theme(plot.title = element_text(size = 12, face = "bold"),
            legend.title = element_text(size = 8))
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.2))
  })
  
  # Onglet Analyse régionale
  output$stats_region <- renderTable({
    region_data <- filtered_data() %>%
      filter(`région administrative` == input$region_detail)
    
    tibble(
      "Indicateur" = c("Nombre d'établissements", "Nombre d'écrans", "Nombre moyen d'écrans", 
                       "Capacité totale (fauteuils)", "Entrées 2022", "% multiplexes", "% Art et Essai"),
      "Valeur" = c(
        format(nrow(region_data), big.mark = " "),
        format(sum(region_data$ecrans, na.rm = TRUE), big.mark = " "),
        round(mean(region_data$ecrans, na.rm = TRUE), 1),
        format(sum(region_data$fauteuils, na.rm = TRUE), big.mark = " "),
        format(sum(region_data$`entrées 2022`, na.rm = TRUE), big.mark = " "),
        paste0(round(mean(region_data$multiplexe == "OUI", na.rm = TRUE) * 100, 1), "%"),
        paste0(round(mean(region_data$AE == "OUI", na.rm = TRUE) * 100, 1), "%")
      )
    )
  })
  
  output$plot_entrees_region <- renderPlotly({
    region_data <- filtered_data() %>%
      filter(`région administrative` == input$region_detail)
    
    if(nrow(region_data) > 0) {
      p <- region_data %>%
        ggplot(aes(x = `entrées 2022`, text = paste("Entrées:", format(`entrées 2022`, big.mark = " ")))) +
        geom_histogram(fill = "steelblue", bins = 30) +
        labs(x = "Entrées 2022", y = "Nombre d'établissements") +
        theme_minimal()
      
      ggplotly(p, tooltip = "text")
    } else {
      plot_ly() %>% 
        add_annotations(
          text = "Aucune donnée disponible pour cette région",
          x = 0.5, y = 0.5,
          xref = "paper", yref = "paper",
          showarrow = FALSE,
          font = list(size = 16)
        )
    }
  })
  
  output$plot_types_region <- renderPlotly({
    region_data <- filtered_data() %>%
      filter(`région administrative` == input$region_detail)
    
    if(nrow(region_data) > 0) {
      p <- region_data %>%
        filter(taille_etablissement != "Non défini") %>%
        count(taille_etablissement) %>%
        ggplot(aes(x = taille_etablissement, y = n, fill = taille_etablissement, 
                   text = paste(taille_etablissement, ":", n, "établissements"))) +
        geom_col() +
        labs(x = "Type d'établissement", y = "Nombre", fill = "Type") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p, tooltip = "text")
    } else {
      plot_ly() %>% 
        add_annotations(
          text = "Aucune donnée disponible pour cette région",
          x = 0.5, y = 0.5,
          xref = "paper", yref = "paper",
          showarrow = FALSE,
          font = list(size = 16)
        )
    }
  })
  
  # Onglet Typologie des cinémas
  output$plot_typologie <- renderPlotly({
    p <- filtered_data() %>%
      ggplot(aes(x = type_cinema, y = `entrées 2022`, fill = type_cinema, 
                 text = paste("Type:", type_cinema, "<br>Entrées:", format(`entrées 2022`, big.mark = " ")))) +
      geom_boxplot(outlier.alpha = 0.5) +
      scale_y_log10() +
      labs(x = "Type d'établissement", y = "Entrées 2022 (échelle log)", fill = "Type") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$plot_capacite_freq <- renderPlotly({
    p <- filtered_data() %>%
      ggplot(aes(x = fauteuils, y = `entrées 2022`, color = type_cinema, 
                 text = paste("Fauteuils:", fauteuils, "<br>Entrées:", format(`entrées 2022`, big.mark = " "), 
                              "<br>Type:", type_cinema))) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(x = "Nombre de fauteuils", y = "Entrées 2022", color = "Type") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$plot_occupation <- renderPlotly({
    p <- filtered_data() %>%
      filter(!is.na(taux_occupation) & taux_occupation < 1) %>% # Filtrer les valeurs aberrantes
      ggplot(aes(x = taille_etablissement, y = taux_occupation, fill = taille_etablissement,
                 text = paste("Type:", taille_etablissement, "<br>Taux d'occupation:", 
                              round(taux_occupation * 100, 1), "%"))) +
      geom_boxplot() +
      labs(x = "Type d'établissement", y = "Taux d'occupation estimé", fill = "Type") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text")
  })
  
  # Onglet Programmation
  output$plot_pdm <- renderPlotly({
    pdm_data <- filtered_data() %>%
      group_by(taille_etablissement) %>%
      summarise(
        `Films français` = mean(`PdM en entrées des films français`, na.rm = TRUE),
        `Films américains` = mean(`PdM en entrées des films américains`, na.rm = TRUE),
        `Films européens` = mean(`PdM en entrées des films européens`, na.rm = TRUE),
        `Autres films` = mean(`PdM en entrées des autres films`, na.rm = TRUE)
      ) %>%
      filter(taille_etablissement != "Non défini") %>%
      pivot_longer(cols = c(`Films français`, `Films américains`, `Films européens`, `Autres films`),
                   names_to = "Origine", values_to = "PdM")
    
    p <- ggplot(pdm_data, aes(x = taille_etablissement, y = PdM, fill = Origine,
                              text = paste(Origine, ":", round(PdM, 1), "%"))) +
      geom_col(position = "stack") +
      labs(x = "Type d'établissement", y = "Part de marché (%)", fill = "Origine") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text")
  })
  
  output$plot_films_programmes <- renderPlotly({
    p <- filtered_data() %>%
      filter(!is.na(`nombre de films programmés`)) %>%
      ggplot(aes(x = taille_etablissement, y = `nombre de films programmés`, fill = taille_etablissement,
                 text = paste("Type:", taille_etablissement, "<br>Nombre de films:", `nombre de films programmés`))) +
      geom_boxplot() +
      labs(x = "Type d'établissement", y = "Nombre de films programmés", fill = "Type") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text")
  })
  
  output$plot_ae <- renderPlotly({
    p <- filtered_data() %>%
      filter(!is.na(`PdM en entrées des films Art et Essai`)) %>%
      ggplot(aes(x = statut_ae, y = `PdM en entrées des films Art et Essai`, fill = statut_ae,
                 text = paste("Statut:", statut_ae, "<br>PdM films Art et Essai:", 
                              round(`PdM en entrées des films Art et Essai`, 1), "%"))) +
      geom_boxplot() +
      labs(x = "Statut Art et Essai", y = "Part de marché des films Art et Essai (%)", fill = "Statut") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Onglet Données brutes
  output$table_data <- renderDT({
    filtered_data() %>%
      select(nom, `région administrative`, commune, ecrans, fauteuils, `entrées 2022`, type_cinema, statut_ae) %>%
      datatable(options = list(pageLength = 15, scrollX = TRUE),
                colnames = c("Nom", "Région", "Commune", "Écrans", "Fauteuils", "Entrées 2022", "Type", "Statut AE"))
  })
}

# Lancement de l'application
shinyApp(ui, server)