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
           actionButton(inputId = "go", label = "Load")),
    column(1, align="center",
           # Buton de mise à jour de la liste rv
           actionButton(inputId = "frequences", label = "fréquences")),
    column(1, align="center",
           # Buton de mise à jour de la liste rv
           actionButton(inputId = "effectifs", label = "effectifs"))
  ),

  fluidRow(
    column(4,
           # Zone d'affichage de l'histogramme
           plotOutput(outputId = "hist"))
  )
)

# Commandes à exécuter
server <- function(input, output){
  data <- eventReactive(input$go, {
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    read.csv(inFile$datapath, header = TRUE)
  })
  
  # Récupération des valeurs fecondite
  fecondite <- reactive({
    if(!"fecondite" %in% colnames(data())) return(NULL)
    data()$fecondite
  })
  
  # On initialise liste de valeurs réactives
  # ----
  rv <- reactiveValues(hist_isFreq = TRUE, 
                       hist_yLabel = "Effectifs", 
                       hist_col = "blue")
  # On observe les clicks
  observeEvent(input$effectifs,{
    rv$hist_isFreq <- TRUE; 
    rv$hist_yLabel <- "Effectifs"; 
    rv$hist_col <- "blue";
  })
  observeEvent(input$frequences,{
    rv$hist_isFreq <- FALSE; 
    rv$hist_yLabel <- "Densité de fréquences";
    rv$hist_col <- "green"
  })
  
  # Histogramme
  # ----
  output$hist <- renderPlot({
    hist(fecondite(), freq = rv$hist_isFreq, cex.axis = 1.5, cex.main = 1.5,
          main = "Histogramme de l'indice de fécondite", col = rv$hist_col,
          xlab = "Indice de fécondité", ylab = rv$hist_yLabel, las = 1,
          breaks = seq(0.8, 3, by = 0.2), right = FALSE, cex.lab = 1.5)
  })
}
# Association interface & commandes
shinyApp(ui = ui, server = server)