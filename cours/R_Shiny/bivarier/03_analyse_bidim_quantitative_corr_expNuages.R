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
        tabPanel("Table", tableOutput("table")),
        tabPanel("Aide", 
                 fluidRow(
                   column(6, plotOutput("nuage1")),
                   column(6, plotOutput("nuage2"))
                 ),
                 fluidRow(
                   column(6, plotOutput("nuage3")),
                   column(6, plotOutput("nuage4"))
                 ),
                 fluidRow(
                   column(6, offset = 4, tableOutput("expSummary"))
                 )
        )
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
  
  # Chargement du fichier de données pour l'aide
  reactive({rnorm(input$num)})
  dataHelp <- reactive({
    inFileHelp <- "../data_shiny/exempleNuages.csv"
    if (is.null(inFileHelp)) return(NULL)
    read.csv(inFileHelp, header = FALSE)
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
  
  # Calcul et affichage du coefficient de corrélation linéaire
  # ---
  output$correlation <- renderText({
    coeff.tmp <- cov(data()[, "EF"], data()[, "CA"])/(sqrt(var(data()[, "EF"])*var(data()[, "CA"])))
    paste("Coefficient de corrélation linéaire =", round(coeff.tmp,digits = 2))
  })
  
  # Affichage des exemples de nuages de points
  # ---
  output$expSummary <- renderTable({
    expSummary.df <- data.frame()
    # Pour chaque exemple de jeu de données...
    for(i in 1:4){
      tmp.data.x <- dataHelp()[which(dataHelp()[, 1] == paste("X", i, sep = "")), 2]
      tmp.data.y <- dataHelp()[which(dataHelp()[, 1] == paste("Y", i, sep = "")), 2]
      tmp.et.x <- sqrt((1/length(tmp.data.x))*sum((tmp.data.x-mean(tmp.data.x))^2))
      tmp.et.y <- sqrt((1/length(tmp.data.y))*sum((tmp.data.y-mean(tmp.data.x))^2))
      tmp.row <- c(mean(tmp.data.x), mean(tmp.data.y), tmp.et.x, tmp.et.y,
                   cov(tmp.data.x, tmp.data.y)/(sd(tmp.data.x)*sd(tmp.data.y)))
      expSummary.df <- rbind.data.frame(expSummary.df, tmp.row)
    }
    colnames(expSummary.df) <- c("moy. X", "moy. Y", "e-t. X", "e-t. Y", "cor(X, Y)")
    rownames(expSummary.df) <- c("data1", "data2", "data3", "data4")
    expSummary.df
  }, 
  colnames = TRUE, rownames = TRUE, bordered = TRUE)
  
  # Exemple nuage1
  output$nuage1 <- renderPlot({
    x.var = dataHelp()[which(dataHelp()[, 1] == "X1"), 2]
    y.var = dataHelp()[which(dataHelp()[, 1] == "Y1"), 2]
    plot(x = x.var, y = y.var, col = "red",
         las = 1, cex.axis = 0.7, pch = 19,
         main = "Y en fonction de X (set 1)",
         xlab = "X", ylab = "Y", cex.lab = 1.2
    )
  })

  # Exemple nuage2
  output$nuage2 <- renderPlot({
    x.var = dataHelp()[which(dataHelp()[, 1] == "X2"), 2]
    y.var = dataHelp()[which(dataHelp()[, 1] == "Y2"), 2]
    plot(x = x.var, y = y.var, col = "red",
         las = 1, cex.axis = 0.7, pch = 19,
         main = "Y en fonction de X (set 2)",
         xlab = "X", ylab = "Y", cex.lab = 1.2
    )
  })
  # Exemple nuage3
  output$nuage3 <- renderPlot({
    x.var = dataHelp()[which(dataHelp()[, 1] == "X3"), 2]
    y.var = dataHelp()[which(dataHelp()[, 1] == "Y3"), 2]
    plot(x = x.var, y = y.var, col = "red",
         las = 1, cex.axis = 0.7, pch = 19,
         main = "Y en fonction de X (set 3)",
         xlab = "X", ylab = "Y", cex.lab = 1.2
    )
  })
  # Exemple nuage4
  output$nuage4 <- renderPlot({
    x.var = dataHelp()[which(dataHelp()[, 1] == "X4"), 2]
    y.var = dataHelp()[which(dataHelp()[, 1] == "Y4"), 2]
    plot(x = x.var, y = y.var, col = "red",
         las = 1, cex.axis = 0.7, pch = 19,
         main = "Y en fonction de X (set 4)",
         xlab = "X", ylab = "Y", cex.lab = 1.2
    )
  })
}
# Association interface & commandes
shinyApp(ui = ui, server = server)
