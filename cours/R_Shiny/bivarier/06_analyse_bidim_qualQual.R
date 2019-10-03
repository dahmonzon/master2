library(shiny)
library(ggplot2)

# Contenu de l'interface
ui <- fluidPage(
  # Titre de l'ui
  titlePanel("Quantitative vs. Quantitative"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      fluidRow(
        column(10, 
               # Bouton de recherche du fichier à charger
               fileInput(inputId = "file1", label = "Choisir un fichier CSV",
                         accept = c("text/plain", ".csv")
               ))
      ),
      fluidRow(
        column(2, 
               # Bouton de chargement 'en retard'
               actionButton(inputId = "go", label = "Load"))
      )
    , style = "font-size: 75%"),
    mainPanel(
      tabsetPanel(
        tabPanel("Diag. Barres (1 var.)", 
                 fluidRow(
                   column(6, plotOutput("barplotUni")),
                   column(6, plotOutput("barplotOrderedUni"))
                 )
        ),
        tabPanel("Diag. Barres (2 var.)", 
                 fluidRow(
                   column(6, plotOutput("barplotBi")),
                   column(6, plotOutput("barplotDodgeBi"))
                 )
        ),
        tabPanel("Diag. Profils", 
                 fluidRow(
                   column(6, plotOutput("barplotProfils")),
                   column(6, tableOutput("contingency"))
                 )
        ),
        tabPanel("Table", dataTableOutput("table"), style = "font-size: 85%")
      )
    , style = "font-size: 75%")
  )
)

# Commandes à exécuter
server <- function(input, output){
  
  # Recherche et chargement du fichier de données
  data <- eventReactive(input$go, {
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    read.csv(inFile$datapath, header = TRUE)
  })
  
  # Données brutes
  # ----
  output$table <- renderDataTable({data()})
  
  # Diagramme en barres
  # ----
  # Unidimensionnel
  output$barplotUni <- renderPlot({
    # Diagramme en barres de la variable 'Level' avec ggplot
    ggplot(data(), aes(x = Level)) + geom_bar()
  })
  output$barplotOrderedUni <- renderPlot({
    # Diagramme en barres de la variable 'Level' avec ggplot
    tmp.data <- data()
    # On ordonne dans l'ordre naturel les différentes modalités de 'Level'
    # freshman: 1, sophomore: 2, junior: 3, senior: 4, special: 5, graduate: 6
    tmp.order <- rep(0, nrow(data()))
    tmp.order[with(tmp.data, Level == "freshman")] = 1
    tmp.order[with(tmp.data, Level == "sophomore")] = 2
    tmp.order[with(tmp.data, Level == "junior")] = 3
    tmp.order[with(tmp.data, Level == "senior")] = 4
    tmp.order[with(tmp.data, Level == "special")] = 5
    tmp.order[with(tmp.data, Level == "graduate")] = 6
    tmp.data$Level <- with(tmp.data, reorder(Level, tmp.order))
    rm(tmp.order)
    # Diagramme en barres de la variable 'Level' avec ggplot
    ggplot(tmp.data, aes(x = Level)) + geom_bar()
  })
  
  # Bidimensionnel
  output$barplotBi <- renderPlot({
    # Diagramme en barres entre les variables 'Level' et 'Sex'
    ggplot(data(), aes(x = Level, fill = Sex)) + geom_bar()
  })
  output$barplotProfils <- renderPlot({
    # Diagramme de profils entre les variables 'Level' et 'Sex'
    ggplot(data(), aes(x = Level, fill = Sex)) + geom_bar(position = "fill")
  })
  output$barplotDodgeBi <- renderPlot({
    # Diagramme de profils entre les variables 'Level' et 'Sex'
    ggplot(data(), aes(x = Level, fill = Sex)) + geom_bar(position = "dodge")
  })
  
  # Table de contingence entre 'Sex' et 'Level'
  # ----
  output$contingency <- renderTable({
    tab = with(data(), table(Sex, Level))
    round(tab/sum(tab), 3)
    tab
  })
  
}
# Association interface & commandes
shinyApp(ui = ui, server = server)
