library(shiny)
library(reshape2)

# Contenu de l'interface
ui <- fluidPage(
  # Titre de l'ui
  titlePanel("Quantitative vs. Qualitative"),
  
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
      ),
      fluidRow(
        column(6, tableOutput("statsSummary"))
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Boîtes parallèles", 
                 fluidRow(
                   column(6, plotOutput("boxplotBasic")),
                   column(6, plotOutput("boxplotGgplot"))
                 ),
                 fluidRow(
                   column(4, offset = 4, textOutput("correlation"))
                 )
        ),
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
  output$statsSummary <- renderTable({
    # Initialisation de la table
    statsSummary <- data.frame()
    # Pour chaque modalité, calcul de mean et écart-type
    for(i in c(1:4)){
      tmp.stats <- c(mean(data()[,i]),
                     sqrt((1/length(data()[,i]))*sum((data()[,i]-mean(data()[,i]))^2)))
      statsSummary <- rbind.data.frame(statsSummary, tmp.stats)
    }
    data.vect <- as.vector(as.matrix(data()))
    stats.total <- c(mean(data.vect),
                     sqrt((1/length(data.vect))*sum((data.vect-mean(data.vect))^2)))
    statsSummary <- cbind.data.frame(t(statsSummary), stats.total)
    # Définition des row/colnames
    colnames(statsSummary) <- c("E0", "F0", "E1", "F1", "Total")
    rownames(statsSummary) <- c("moyenne", "écart-type")
    # Renvoyer la table
    statsSummary
  }, rownames = TRUE, digits = 1)
  
  # Boîtes parallèles
  # ----
  output$boxplotBasic <- renderPlot({
    # Reshape data()
    data.stack <- melt(data(), measure.vars = c("E0", "E1", "F0", "F1"))
    # Boxplot basique
    boxplot(data.stack$value ~ data.stack$variable , col="grey",
            xlab = "Modalités", ylab = "Mesures")
  })

  output$boxplotGgplot <- renderPlot({
    # Reshape data()
    data.stack <- melt(data(), measure.vars = c("E0", "E1", "F0", "F1"))
    # Boxplot élaborée
    qplot(x = data.stack[,1], y = data.stack[,2], 
          xlab = "Modalités", ylab = "Mesures",
          geom=c("boxplot", "jitter"), fill=data.stack[,1]) +
      theme(legend.title=element_blank())
  })
  
  # Calcul et affichage le rapport de corrélation
  # ---
  output$correlation <- renderText({
    # Calcul de la variance expliquée
    tmp.mean.y = mean(as.vector(as.matrix(data())))
    tmp.mean.yr = apply(data(), MARGIN = 2, mean)
    tmp.nl = rep(nrow(data()), 4)
    sE2 = (1/sum(tmp.nl))*sum(tmp.nl*(tmp.mean.yr-tmp.mean.y)^2)
    # Calcul de la variance résiduelle
    tmp.var.yr = apply(data(), MARGIN = 2, var)
    sR2 = (1/sum(tmp.nl))*sum(tmp.nl*tmp.var.yr)
    # Calcul du rapport de corrélation
    rCor = sqrt(sE2/(sE2+sR2))
    paste("\n\nRapport de corrélation =", round(rCor, digits = 2))
    print(sE2)
    print(sR2)
    
  })
}
# Association interface & commandes
shinyApp(ui = ui, server = server)
