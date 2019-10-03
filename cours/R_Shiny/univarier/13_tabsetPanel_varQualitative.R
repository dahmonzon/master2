library(shiny)

# Contenu de l'interface
ui <- fluidPage(
  # Titre de l'ui
  titlePanel("Variable qualitative"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      fluidRow(
        column(10, 
               # Bouton de recherche du fichier à charger
               fileInput(inputId = "file1", label = "Choose TSV File",
                         accept = c("text/plain", ".tsv")
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
        tabPanel("Diagramme en colonnes", plotOutput("colonnes")), 
        tabPanel("Diagramme en secteurs", plotOutput("secteurs")),
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
    read.table(inFile$datapath, header = TRUE, sep = "\t")
  })
  
  # Calcul des effectifs
  effectifs <- reactive({table(data())})
  
  # Diagramme en colonnes
  output$colonnes <- renderPlot({
    barplot(effectifs(), main = "Catégories Socioprofessionnelles", 
            ylab="Effectifs", las = 2,
            names.arg = substr(names(effectifs()), 1, 4))
    
  })
  
  # Diagramme en secteurs
  output$secteurs <- renderPlot({
    pie(effectifs(), labels = substr(names(effectifs()), 1, 4), 
        main = "Catégories Socioprofessionnelles", col=c())
  })
  
  # Table des effectifs
  # ----
  output$table <- renderTable({effectifs()}, colnames = FALSE)
}
# Association interface & commandes
shinyApp(ui = ui, server = server)
