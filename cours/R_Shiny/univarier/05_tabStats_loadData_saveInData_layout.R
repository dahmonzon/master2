library(shiny)

# Contenu de l'interface
ui <- fluidPage(
  
  fluidRow(
    column(4, 
           # Bouton de recherche du fichier à charger
           fileInput(inputId = "file1", label = "Choose CSV File",
                     accept = c("text/plain", ".csv")
           )),
    column(8,
           # Affichage d'un summary
           verbatimTextOutput(outputId = "summary"))
  ),
  
  fluidRow(
    column(2, 
           # Buton de chargement 'en retard'
           actionButton(inputId = "go", label = "Load")),
    
    column(6, offset = 2, 
           # Affichage des données
           tableOutput(outputId = "contents"))
  )
)

# Commandes à exécuter
server <- function(input, output){
  
  data <- eventReactive(input$go, {
    # Initialement, class(input$file1) = NULL
    # Après chargement, class(input$file1) = data.frame
    # avec les colonnes 'size', 'type', and 'datapath' columns. 
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    
    read.csv(inFile$datapath, header = FALSE)
  })
  
  # Commande pour le calcul du summary
  output$summary <- renderPrint({ t(summary(data())) })
  # Commande pour le chargement de données dans 'output'
  output$contents <- renderTable({ data() })
}
# Association interface & commandes
shinyApp(ui = ui, server = server)