library("dplyr")
library("tibble")
library("readr")
library("ggplot2")
library("shiny")
library("shinythemes")
library("ggmosaic")

convertSex <- function(val){
  res <- "Homme"
  if( val == 0) {
    res <- "Femme"
  }
  return(res)
}

convertExang <- function(val){
  res <- "Homme"
  if( val == 0) {
    res <- "Femme"
  }
  return(res)
}

convertTarget <- function(val){
  res <- "Yes"
  if( val == 0) {
    res <- "No"
  }
  return(res)
}

convertFbs <- function(val){
  res <- TRUE
  if( val == 0) {
    res <- FALSE
  }
  return(res)
}


convertThal <- function(val){
  res <- "Normal"
  if( val == 1) {
    res <- "Fixed"
  }
  if( val == 2) {
    res <- "Reversible"
  }
  return(res)
}

convertRestecg <- function(val){
  res <- "Normal"
  if( val == 1) {
    res <- "Anomalie"
  }
  if( val == 2) {
    res <- "Hypertrophie"
  }
  return(res)
}

convertSlope <- function(val){
  res <- "Remontee"
  if( val == 1) {
    res <- "Plate"
  }
  if( val == 2) {
    res <- "Descente"
  }
  return(res)
}

convertCp <- function(val){
  res <- "Typique"
  if( val == 1) {
    res <- "Atypique"
  }
  if( val == 2) {
    res <- "Non Angineuse"
  }
  if( val == 3) {
    res <- "Asymptomatique"
  }
  return(res)
}

convertCp <- function(val){
  res <- "Typique"
  if( val == 1) {
    res <- "Atypique"
  }
  if( val == 2) {
    res <- "Non Angineuse"
  }
  if( val == 3) {
    res <- "Asymptomatique"
  }
  return(res)
}



myFirstName <- "Dah"
myLastName <- "Diarrra"
myEmail <- "diarradah402@gmail.com"
#heart_data <- read_csv("/Users/dahdiarra/master2/cours/R_Shiny/heart.csv")
heart_data <- read_csv("./heart.csv")
categoryCol <- c("sex","cp", "fbs", "restecg","exang","slope","ca","thal","target")
qualitativeCol <- c("age","trestbps","chol","thalach","oldpeak")
heart_data[categoryCol] <- lapply(heart_data[categoryCol],FUN = factor)

# heart_data["sex"] <- lapply(heart_data["sex"], FUN = convertSex)
# heart_data["cp"] <- lapply(heart_data["cp"], FUN = convertCp)
# heart_data["fbs"] <- lapply(heart_data["fbs"], FUN = convertFbs)
# heart_data["exang"] <- lapply(heart_data["exang"], FUN = convertExang)
# heart_data["slope"] <- lapply(heart_data["slope"], FUN = convertSlope)
# heart_data["restecg"] <- lapply(heart_data["restecg"], FUN = convertRestecg)
# heart_data["thal"] <- lapply(heart_data["thal"], FUN = convertThal)
# heart_data["target"] <- lapply(heart_data["target"], FUN = convertTarget)


data_description_text <- "<p> Le jeu de donnée contient des resultats d'analyses sur
des patients ayant ou non une maladie cardiaque. les données  dans colonne target
ont  pour valeur 1 (a la maladie) ou 0 (n'a pas la maladie) sur chaque observation.
Dans la partie classication nous allons essayé de predire cette valeur target 
pour une observation donnée à l'aide d'une régression logistique.</p>
<p> Une analyse univarie, multivarie et un resumé  sont proposé dans leur section respective</p> 
<p>La description de chaque colonne se trouve ci-près</p>"

variable_description <- "
<ul>
<li><b>age</b> -  L'âge de la personne en années </li>
<li><b>sex</b> - le sexe de la personne (1 = homme, 0 = femme) </li>
<li><b>cp</b> - douleur à la poitrine ressentie (valeur 1: angine de poitrine typique, valeur 2: angine de poitrine atypique, valeur 3: douleur non angineuse, valeur 4: asymptomatique) </li>
<li><b>trestbps</b> - tension artérielle au repos de la personne (mm Hg à l’admission à l’hôpital) </li>
<li><b>chol</b> - mesure du cholestérol de la personne en mg / dl </li>
<li><b>fbs</b> - glycémie à jeun de la personne (> 120 mg / dl, 1 = vrai; 0 = faux) </li>
<li><b>restecg</b> -  mesure électrocardiographique au repos (0 = normal, 1 = anomalie de l'onde ST-T, 2 = hypertrophie ventriculaire gauche probable ou définie d'après les critères d'Estes) </li>
<li><b>thalach</b> - fréquence cardiaque maximale atteinte </li>
<li><b>exang</b> - angine de poitrine induite par l'exercice (1 = oui; 0 = non) </li>
<li><b>oldpeak</b> -dépression ST induite par l'exercice par rapport au repos ('ST' se rapporte aux positions sur le tracé ECG. Plus d'informations ici.)</li>
<li><b>slope </b>- la pente du segment ST d’exercice de pointe (valeur 1: remontée, valeur 2: plate, valeur 3: descente) </li>
<li><b>ca </b>- le nombre de vaiseux principaux (0-3)</li>
<li><b>thal </b>- un trouble sanguin appelé thalassémie (3 = normal; 6 = défaut fixe, 7 = défaut réversible) </li>
<li><b>target </b>- maladie cardiaque (0 = non, 1 = oui)></li>
</ul>
</br></br></br>
"
resumer <- "
<p>La régression logistique a été réalisée sur 2/3 du jeu de données principale et les 1/3
restant ont servit pour la prédiction afin de tester le modele, le resultat obtenu fut 86% de bonne prédiction</p>

<p>D'aprés le résultat de la régression logistique, On remarque que les variables cp
(douleur à la poitrine ressentie), restecg ( mesure électrocardiographique au repos),
oldpeak (dépression ST induite par l'exercice par rapport au repos), ca (le nombre de vaiseux principaux) sont statistique 
des facteur déterminant pour la détection d'une maladie cardiaque. par contre les autre 
variable n'ont pas une grande influance sur la dection du maladie.</p>
"
############## Data Panel view ##################
dataPanel <-  fluidPage(fluidRow(
  column(5,htmlOutput(outputId = "ddes")),
  column(5,offset = 2,htmlOutput(outputId = "vdes"))
),
fluidRow(
  column(12,dataTableOutput(outputId = "heart_data")))
)

############# Univariate Panel view ############

univariatePanel <- fluidPage(
  sidebarLayout(sidebarPanel = sidebarPanel(selectInput(inputId = "categ",label = "Choisis une variable categorielle" ,choices = categoryCol),
                                            selectInput(inputId = "quali",label = "Choisis une variable qualitative" ,choices = qualitativeCol),
                                            sliderInput(inputId = "slid",label = "modifier la taille du bine",value = 5,min = 0 ,max = 100),width = 3),
                mainPanel = mainPanel(fluidRow(column(htmlOutput(outputId = "tle_cat"),width = 12)),
                                      fluidRow(column(width = 12,plotOutput(outputId = "cat_bar"))),
                                      fluidRow(column(htmlOutput(outputId = "tle_qlt"),width = 12)),
                                      fluidRow(column(width = 6,plotOutput(outputId = "qlt_hist")),column(width = 6,verbatimTextOutput(outputId = "qlt_box"))),width = 9))
)

############ Bivariate Panel view #############
bivariatePanel <- fluidPage(
  sidebarLayout(sidebarPanel = sidebarPanel(
    htmlOutput(outputId = "b"),
    selectInput(inputId = "categ_b",label = "Choisis une variable categorielle" ,choices = categoryCol),
                                            selectInput(inputId = "quali_b",label = "Choisis une variable qualitative" ,choices = qualitativeCol),
                                           width = 3),
                mainPanel = mainPanel(fluidRow(column(htmlOutput(outputId = "tle_cat_b"),width = 12)),
                                      fluidRow(column(width = 12,plotOutput(outputId = "cat_bar_b"))),
                                      fluidRow(column(htmlOutput(outputId = "tle_qlt_b"),width = 12)),
                                      fluidRow(column(width = 12,plotOutput(outputId = "qlt_hist_b"))),width = 9))
)

########### Abstact Panel view ###########

abstractPanel <- fluidPage(
    fluidRow(
      column(
        htmlOutput(outputId = "sm"),
        verbatimTextOutput(outputId = "bv"),
      width = 5 ),
      column(htmlOutput(outputId = "resume"), width = 7)
    )
)


ui <- navbarPage( title = "Heart Desease",
              theme = shinytheme(theme = "united"),
              tabPanel(title = "Data",dataPanel),
              tabPanel(title = "Univariate",univariatePanel),
              tabPanel(title = "Bivariate", bivariatePanel ),
              tabPanel(title = "Classification And Abstract",abstractPanel),
              tabPanel(title = "About Me",
                       textOutput(outputId = "lastName"),
                       textOutput(outputId = "firstName"),
                       textOutput(outputId = "email")))

server <- function(input, output){
  dataUi(input,output)
  univariateUi(input,output)
  bivariateUi(input,output)
  abstractUi(input,output)
  aboutMeUi(input,output)
}

dataUi <- function(input, output){
  output$ddes <- renderText(data_description_text)
  output$vdes <- renderText(variable_description)
  output$heart_data <- renderDataTable(heart_data)
}

univariateUi <- function(input,output){
  output$tle_cat <- renderText("<h3>Variable Categorielle</h3>")
  output$tle_qlt <- renderText("<h3>Variable Qualitative</h3>") 

  output$cat_bar <- renderPlot({
    gg <- ggplot(heart_data) + 
      geom_bar(aes_string(x = input$categ, fill= input$categ), width = .4)
    gg
  })
  output$qlt_hist <- renderPlot({
    gg <- ggplot(heart_data) + 
      geom_histogram(aes_string(x = input$quali),fill = "blue", binwidth = input$slid / 2)
    gg
  })
  output$qlt_box <- renderPrint({
     summary(heart_data[input$quali])
  })
  # output$qlt_box <- renderPlot({
  #   gg <- ggplot(heart_data) + 
  #     geom_boxplot(aes( x= age, y=" "), width = .4)
  #   gg
  # })
}


bivariateUi <- function(input,output){
  output$b <- renderText("<h4> Les variables sont affichées en fonction de la target</h4>")
  output$tle_cat_b <- renderText("<h3>Variable Categorielle</h3>")
  output$tle_qlt_b <- renderText("<h3>Variable Qualitative</h3>") 
  
  output$cat_bar_b <- renderPlot({
     gg <- ggplot(heart_data)+
      geom_mosaic(aes_string(x=paste0("product(",input$categ_b,",target)"), fill=input$categ_b))+
      ylab("Frequence")+
      xlab("target")
     gg
  })
  output$qlt_hist_b <- renderPlot({
    gg <- ggplot(heart_data) +
      geom_boxplot(aes_string(x = "target",y = input$quali_b, fill= input$quali_b), width = .4 ,color= "blue")
    gg
  })
}


abstractUi <- function(input,output){
  train <- heart_data[1:200,]
  glm <- glm(target ~.,family=binomial(link='logit'),data=train)
  val <- summary(glm)
  output$sm <- renderText("Le summary de la logistique regression")
  output$bv <- renderPrint(val)
  output$resume <- renderText(resumer)
}

aboutMeUi <- function(input, output){
  output$lastName <- renderText(paste("Last Name: ",myLastName,sep = "\t"))
  output$firstName <- renderText(paste("First Name: ", myFirstName, sep = "\t"))
  output$email <- renderText(paste("Email: ",myEmail,sep = "\t"))
}

shinyApp(ui = ui , server = server)

