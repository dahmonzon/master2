library(shiny)

# Contenu de l'interface
ui <- fluidPage(
  
  fluidRow(
    column(3, 
           # Bouton de recherche du fichier à charger
           fileInput(inputId = "file1", label = "Choose CSV File",
                     accept = c("text/plain", ".csv")
           )),
    column(4,
           # Affichage d'un summary
           verbatimTextOutput(outputId = "summary"))
  ),
  
  fluidRow(
    column(2, 
           # Buton de chargement 'en retard'
           actionButton(inputId = "go", label = "Load")),
    
    column(6, offset = 1, 
           # Affichage des données
           tableOutput(outputId = "contents"))
  ),
  
  textOutput(outputId = "seeVar")
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
  
  # Colonnes du tableau statistique
  tabStats <- reactive({
    # Calculer les effectifs et les effectifs cumulés
    table.tmp <- as.data.frame(table(data()))
    table.tmp <- cbind(table.tmp, cumsum(table.tmp[[2]]))
    # Calculer les fréquences et les fréquences cumulés
    table.tmp <- cbind(table.tmp, 
                       table.tmp[[2]]/nrow(data())*100,
                       table.tmp[[3]]/nrow(data())*100)
    # Ajouter des noms de colonnes
    colnames(table.tmp) <- c("Ages", "Effectifs", "Effectifs Cum.",
                             "Fréquences", "Fréquences Cum.")
    # Renvoyer le tableau statistique
    table.tmp
  })
  
  # Commande pour le calcul du summary
  output$summary <- renderPrint({ t(summary(data())) })
  # Commande pour le chargement de données dans 'output'
  output$contents <- renderTable({ tabStats() })
}
# Association interface & commandes
shinyApp(ui = ui, server = server)