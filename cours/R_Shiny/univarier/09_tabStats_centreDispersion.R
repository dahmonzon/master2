library(shiny)

# Contenu de l'interface
ui <- fluidPage(
  
  fluidRow(
    column(3, 
           # Bouton de recherche du fichier à charger
           fileInput(inputId = "file1", label = "Choose CSV File",
                     accept = c("text/plain", ".csv")
           ))
  ),
  fluidRow(
    column(2, 
           # Buton de chargement 'en retard'
           actionButton(inputId = "go", label = "Load"))
  ),
  
  fluidRow(
    column(4, 
           # Zone d'affichage du diagramme en bâtons des effectifs
           plotOutput(outputId = "effectifsDiag")),
    column(4, 
           # Zone d'affichage du diagramme en bâtons des effectifs cumulés
           plotOutput(outputId = "effectifsCumDiag"))
  ),
  
  fluidRow(
    column(4, 
           # Zone d'affichage de la boîte à moustaches
           plotOutput(outputId = "boiteMoustaches")),
    column(4, offset = 1, 
           # Zone d'affichage dispersion /  tendance centrale
           tableOutput(outputId = "centreDisp"))
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
  
  tabCentreDisp <- reactive({
    # Noms des caractéristiques
    names.tmp <- c("Maximum", "Minimum", "Moyenne", "Médiane",
                   "1e quartile", "3e quartile", "Variance", "Ecart-type")
    # Calcul des caractéristiques
    summary.tmp <- c(max(data()[,1]), min(data()[,1]), mean(data()[,1]), median(data()[,1]),
                     quantile((data()[,1]))[2], quantile((data()[,1]))[4],
                     var(data()[,1]), sqrt(var(data()[,1])))
    # Ajout des nomes au vecteur de valeurs
    summary.tmp <- cbind.data.frame(names.tmp, summary.tmp)
    # Ajout des noms de colonnes
    colnames(summary.tmp) <- c("Caractéristique", "Valeur")
    
    summary.tmp
  })
  # Commande pour le chargement de données dans 'output'
  #output$contents <- renderTable({ tabStats() })
  output$centreDisp <- renderTable({tabCentreDisp()})
  
  # Commande pour l'affichage du plot des effectifs
  output$effectifsDiag <- renderPlot({ 
    plot(table(data()), col ="green4", xlab ="âge", ylab ="Effectifs", 
    main ="Distribution des effectifs pour l'âge")
  })
  
  # Commande pour l'affichage du plot des fréquences cumulées
  output$effectifsCumDiag <- renderPlot({ 
    plot(ecdf(as.numeric(as.character(tabStats()[,1]))), 
         col ="green4", xlab ="âge", ylab ="Fréquences cumulées", 
         main ="Fréquences cumulés pour l'âge")
  })
  
  # Commande pour l'affichage de la boîte à moustaches
  output$boiteMoustaches <- renderPlot({
    # Boîte à moustaches
    boxplot( data(), col = grey(0.8), 
             main = "Age des salariés",
             ylab = "Age", las = 1)
    # Affichage complémentaires en Y des différents âges
    rug(data()[,1], side = 2)
  })
}
# Association interface & commandes
shinyApp(ui = ui, server = server)