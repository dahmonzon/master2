library(shiny)

# Contenu de l'interface
ui <- fluidPage(
  # Titre de l'ui
  titlePanel("Variable quantitative"),
  
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
               # Buton de chargement 'en retard'
               actionButton(inputId = "go", label = "Load"))
      )
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Nuage de points", 
                 fluidRow(
                   column(8, offset = 1, plotOutput("nuagePoints"))
                 ),
                 fluidRow(
                   column(4, offset = 3, textOutput("correlation"))
                 )
        ), 
        tabPanel("Caractéristiques", tableOutput("caract")),
        tabPanel("Table", tableOutput("table"))
      )
    )
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
  output$table <- renderTable({data()}, colnames = TRUE)
  
  # Caractéristiques
  # ----
  output$caract <- renderTable({
    # Définition des colonnes choisies 
    var.names <- c("NB", "EF", "CA")
    # Initialisation de la table
    caract.df <- data.frame()
    # Pour chaque colonne, calcul de min, max, mean et ecart-type
    for(strCol in var.names){
      caract.vect <- c(min(data()[, strCol]), max(data()[,strCol]), 
                       mean(data()[,strCol]), sqrt(var(data()[,strCol])))
      caract.df <- rbind.data.frame(caract.df, caract.vect)
    }
    # Définition des row/colnames
    rownames(caract.df) <- var.names
    colnames(caract.df) <- c("Minimum", "Maximum", "Moyenne", "Ecart-type")
    # Renvoyer la table
    caract.df
  }, rownames = TRUE, digits = 0)
  
  # Nuage de points
  # ----
  output$nuagePoints <- renderPlot({
    # Simple nuage de point EF vs CA
    options(scipen=999)
    x.var = "EF"; y.var = "CA";
    plot(x = data()[, x.var], y = data()[, y.var], col = "blue",
         las = 2, cex.axis = 0.7,
         main = paste(y.var, "en fonction de", x.var),
         xlab = x.var, ylab = y.var, cex.lab = 1.2
    )
    options(scipen=0)
    
  })
  
  # Calcul et affichage du coefficient de corélation linéaire
  # ---
  output$correlation <- renderText({
    coeff.tmp <- cov(data()[, "EF"], data()[, "CA"])/(sqrt(var(data()[, "EF"])*var(data()[, "CA"])))
    paste("Coefficient de corrélation linéaire =", round(coeff.tmp,digits = 2))
  })
}
# Association interface & commandes
shinyApp(ui = ui, server = server)
