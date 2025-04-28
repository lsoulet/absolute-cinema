Sys.setlocale("LC_ALL", "fr_FR.UTF-8")

library(shiny)
library(ggplot2)
library(tidyverse)
library(DT)
library(plotly)
library(shinydashboard)
library(shinyWidgets)

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

ui <- dashboardPage(
  dashboardHeader(title = "Cinémas en France"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Accueil", tabName = "accueil", icon = icon("home")),
      menuItem("Analyse régionale", tabName = "regional", icon = icon("chart-bar")),
      menuItem("Typologie des cinémas", tabName = "typologie", icon = icon("film")),
      menuItem("Programmation", tabName = "programmation", icon = icon("ticket-alt")),
      menuItem("Données brutes", tabName = "donnees", icon = icon("table"))
    ),
    br(),
    pickerInput(
      "region_filter", "Filtrer par région :",
      choices = c("Toutes les régions" = "all", sort(unique(data$`région administrative`))),
      selected = "all",
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    ),
    checkboxGroupInput(
      "type_filter", "Type d'établissement :",
      choices = c("Multiplexe", "Cinéma classique"),
      selected = c("Multiplexe", "Cinéma classique")
    ),
    sliderInput(
      "ecrans_filter", "Nombre d'écrans :",
      min = 1, max = max(data$ecrans, na.rm = TRUE),
      value = c(1, max(data$ecrans, na.rm = TRUE)),
      step = 1
    )
  ),
  
  dashboardBody(
    tabItems(
      # Onglet d'accueil
      tabItem(tabName = "accueil",
              fluidRow(
                box(
                  title = "Bienvenue sur l'explorateur de cinémas français",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  h3("Analyse des établissements cinématographiques en France"),
                  p("Le cinéma occupe une place essentielle dans la culture française, autant en tant qu'art qu'en tant qu'industrie. ",
                    "Mais derrière la magie de l'écran, la réalité des établissements cinématographiques en France révèle des dynamiques variées : ",
                    "diversité des structures, disparités territoriales, différences de fréquentation..."),
                  p("Cette application interactive vous propose une plongée statistique dans l'univers des cinémas français, ",
                    "en s'appuyant sur un jeu de données riche de 2061 observations et 40 variables. ",
                    "Notre objectif est de dresser un état des lieux synthétique mais précis du paysage cinématographique, ",
                    "en répondant à plusieurs questions clés :"),
                  tags$ul(
                    tags$li("Comment les cinémas sont-ils répartis sur le territoire français ?"),
                    tags$li("Quelles différences observe-t-on entre les types d'établissements (nombre d'écrans, capacité d'accueil, multiplexes vs cinémas indépendants) ?"),
                    tags$li("Quel est le lien entre les caractéristiques physiques d'un établissement et sa fréquentation ?"),
                    tags$li("Comment se répartissent les parts de marché entre cinéma français, films américains, européens, et cinéma Art et Essai ?")
                  ),
                  p("À travers des visualisations claires et des analyses ciblées, nous mettons en lumière non seulement les grandes tendances nationales, ",
                    "mais aussi les spécificités locales et structurelles qui façonnent l'offre cinématographique française."),
                  hr(),
                  h4("À propos du jeu de données"),
                  p("Cette étude s'appuie sur un jeu de données exhaustif recensant 2061 établissements cinématographiques répartis sur l'ensemble du territoire français. ",
                    "Les données, issues du Centre National du Cinéma et de l'image animée (CNC), offrent une vision complète du paysage cinématographique français ",
                    "à travers 40 variables couvrant l'identification, la localisation, les caractéristiques techniques, la fréquentation et l'orientation artistique des établissements."),
                  hr(),
                  h4("Auteurs du projet"),
                  p("Cette application a été développée par :"),
                  tags$ul(
                    tags$li("Axel Frache"),
                    tags$li("Nathan Dilhan"),
                    tags$li("Liam Soulet")
                  ),
                  p("Dans le cadre d'un projet de Data Visualisation.")
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
  
  # Onglet Accueil - Indicateurs clés
  output$total_cinemas <- renderValueBox({
    valueBox(
      nrow(filtered_data()),
      "Établissements",
      icon = icon("building"),
      color = "blue"
    )
  })
  
  output$total_ecrans <- renderValueBox({
    valueBox(
      sum(filtered_data()$ecrans, na.rm = TRUE),
      "Écrans",
      icon = icon("tv"),
      color = "green"
    )
  })
  
  output$total_entrees <- renderValueBox({
    valueBox(
      paste0(round(sum(filtered_data()$`entrées 2022`, na.rm = TRUE)/1000000, 1), " M"),
      "Entrées en 2022",
      icon = icon("users"),
      color = "purple"
    )
  })
  
  output$evolution_moyenne <- renderValueBox({
    evol <- mean(filtered_data()$evolution_covid, na.rm = TRUE)
    valueBox(
      paste0(round(evol, 1), "%"),
      "Évolution 2021-2022",
      icon = icon("chart-line"),
      color = if(evol > 0) "green" else "red"
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