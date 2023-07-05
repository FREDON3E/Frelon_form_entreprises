library(shiny)
library(shinyjs)
library(openxlsx)

# Chemin du fichier Excel
fichier_excel <- "reponses.xlsx"

# Interface utilisateur
ui <- fluidPage(
  titlePanel("Formulaire d'information"),
  
  useShinyjs(),  # Utilisation de la bibliothèque shinyjs
  
  sidebarLayout(
    sidebarPanel(
      textInput("nom", "Nom :", placeholder = "Entrez votre nom"),
      numericInput("age", "Âge :", value = NULL, min = 0, max = 150),
      textInput("latitude", "Latitude :", value = ""),
      textInput("longitude", "Longitude :", value = ""),
      
      # Bouton pour géolocaliser l'utilisateur
      actionButton("geolocaliser", "Géolocaliser"),
      
      # Bouton pour envoyer les réponses
      actionButton("envoyer", "Envoyer")
    ),
    
    mainPanel(
      h4("Résumé des informations :"),
      # Tableau des réponses
      dataTableOutput("tableau")
    ),
    
  )
)

# Serveur
server <- function(input, output, session) {
  # Variable pour stocker les réponses
  reponses <- reactiveValues(data = data.frame(Nom = character(),
                                               Age = numeric(),
                                               Latitude = character(),
                                               Longitude = character()))
  
  # Fonction pour géolocaliser l'utilisateur
  geolocaliser <- function() {
    runjs("
      // Appel à l'API Geolocation de Google Maps
      navigator.geolocation.getCurrentPosition(function(position) {
        var latitude = position.coords.latitude;
        var longitude = position.coords.longitude;
        Shiny.setInputValue('latitude', latitude);
        Shiny.setInputValue('longitude', longitude);
      }, function(error) {
        alert('Erreur lors de la géolocalisation : ' + error.message);
      });
    ")
  }
  
  # Observer pour géolocaliser l'utilisateur
  observeEvent(input$geolocaliser, {
    geolocaliser()
  })
  
  # Observer pour ajouter les réponses au tableau
  observeEvent(input$envoyer, {
    reponses$data <- rbind(reponses$data, data.frame(Nom = input$nom,
                                                     Age = input$age,
                                                     Latitude = input$latitude,
                                                     Longitude = input$longitude))
    # Réinitialiser les champs du formulaire
    updateTextInput(session, "nom", value = "")
    updateNumericInput(session, "age", value = NULL)
    updateTextInput(session, "latitude", value = "")
    updateTextInput(session, "longitude", value = "")
    
    # Exporter les réponses vers Excel
    exportToExcel(reponses$data)
  })
  
  # Afficher les réponses dans la sortie "Résumé des informations"
  output$summary <- renderPrint({
    paste("Nom :", input$nom, collapse = "\n")
    paste("Âge :", input$age, collapse = "\n")
    paste("Latitude :", input$latitude, collapse = "\n")
    paste("Longitude :", input$longitude, collapse = "\n")
  })
  
  # Afficher le tableau des réponses
  output$tableau <- renderDataTable({
    reponses$data
  })
  
  # Fonction pour exporter les réponses vers Excel
  exportToExcel <- function(data) {
    if (file.exists(fichier_excel)) {
      wb <- loadWorkbook(fichier_excel)
      i= nrow(read.xlsx(fichier_excel))+1
      writeData(wb, sheet = "Réponses", x = data, startRow = max(1, nrow(reponses$data) - nrow(data)+i), startCol = 1, colNames = FALSE)
    } else {
      wb <- createWorkbook()
      addWorksheet(wb, "Réponses")
      i=1
      writeData(wb, sheet = "Réponses", x = data, startRow = max(1, nrow(reponses$data) - nrow(data)), startCol = 1)
    }
    
    
    saveWorkbook(wb, file = fichier_excel, overwrite = TRUE)
  }
}

# Lancement de l'application
shinyApp(ui = ui, server = server)