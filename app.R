library(shiny)
library(ggplot2)
library(tidyverse)

data <- read_delim("data/etablissements-cinematographiques.csv", delim = ";")

ui <- fluidPage(
  titlePanel("Analyse des cinémas en France"),
  sidebarLayout(
    sidebarPanel(
      selectInput("region", "Choisir une région :", choices = unique(data$`région administrative`))
    ),
    mainPanel(
      plotOutput("plot_entrees")
    )
  )
)

server <- function(input, output, session) {
  output$plot_entrees <- renderPlot({
    data %>%
      filter(`région administrative` == input$region) %>%
      ggplot(aes(x = `entrées 2022`)) +
      geom_histogram(fill = "steelblue", bins = 30) +
      labs(title = paste("Entrées 2022 en", input$region), x = "Entrées", y = "Nombre d'établissements")
  })
}

shinyApp(ui, server)