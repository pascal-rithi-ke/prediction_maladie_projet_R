# Charger les packages nécessaires
source('import.R')

# URL de connexion à MongoDB
mongo_url <- Sys.getenv("MONGO_URL")
collection <- "heart_cleveland_updated"
chatbox <- "chatbox"

# Données brutes
new_data <- data.frame(
  age = 45,
  sex = 1,
  cp = 3,
  trestbps = 120,
  chol = 220,
  fbs = 0,
  restecg = 1,
  thalach = 150,
  exang = 0,
  oldpeak = 1.5,
  slope = 2,
  ca = 0,
  thal = 2
)

# Charger le modèle Random Forest
rfFit <- readRDS("./rf_model.rds")
# Faire la prédiction
prediction <- predict(rfFit, newdata = new_data)

# Interface utilisateur
ui <- navbarPage(
  title = "Multi-page Shiny App",
  theme = shinytheme("flatly"),
  
  tabPanel(
    title = "Heatmap",
    mainPanel(
      plotOutput(outputId = "heatmapPlot")
    )
  ),
  
  tabPanel(
    title = "Sex Distribution",
    mainPanel(
      plotOutput(outputId = "sexDistPlot")
    )
  ),
  
  tabPanel(
    title = "Risk Factor Comparison",
    mainPanel(
      plotOutput(outputId = "riskFactorPlot1")
    ),
    mainPanel(
      plotOutput(outputId = "riskFactorPlot2")
    )
  ),
  
  tabPanel(
    title = "Risk disease by Factor",
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "variable_y", 
                    label = "Sélectionnez la variable Y:", 
                    choices = c("Âge" = "age", "Cholestérol" = "chol", "Pression artérielle" = "trestbps", 
                                "Fréquence cardiaque" = "thalach", "Oldpeak" = "oldpeak"),
                    selected = "chol")
      ),
      mainPanel(
        plotOutput(outputId = "riskFactorPlot3")
      )
    ),
    
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "variable_y2", 
                    label = "Sélectionnez la variable Y:", 
                    choices = c("Âge" = "age", "Cholestérol" = "chol", "Pression artérielle" = "trestbps", 
                                "Fréquence cardiaque" = "thalach", "Oldpeak" = "oldpeak"),
                    selected = "oldpeak"),
        selectInput(inputId = "condition_filter",
                    label = "Filtrer par condition:",
                    choices = c("Toutes", "0", "1"),
                    selected = "Toutes")
      ),
      mainPanel(
        plotOutput(outputId = "scatterPlot")
      )
    )
  ),
  
  tabPanel(
    title = "Model",
    sidebarLayout(
      sidebarPanel(
        numericInput(inputId = "age", label = "Âge", value = 45, min = 0, max = 120),
        numericInput(inputId = "sex", label = "Sexe (0 = Femme, 1 = Homme)", value = 1, min = 0, max = 1),
        numericInput(inputId = "cp", label = "Type de douleur thoracique", value = 3, min = 0, max = 3),
        numericInput(inputId = "trestbps", label = "Pression artérielle au repos", value = 120, min = 0, max = 200),
        numericInput(inputId = "chol", label = "Cholestérol sérique", value = 220, min = 0, max = 600),
        numericInput(inputId = "fbs", label = "Glycémie à jeun > 120 mg/dl (1 = vrai, 0 = faux)", value = 0, min = 0, max = 1),
        numericInput(inputId = "restecg", label = "Résultats électrocardiographiques au repos", value = 1, min = 0, max = 2),
        numericInput(inputId = "thalach", label = "Fréquence cardiaque maximale atteinte", value = 150, min = 0, max = 220),
        numericInput(inputId = "exang", label = "Angine induite par l'exercice (1 = oui, 0 = non)", value = 0, min = 0, max = 1),
        numericInput(inputId = "oldpeak", label = "Dépression ST induite par l'exercice", value = 1.5, min = 0, max = 6),
        numericInput(inputId = "slope", label = "Pente du segment ST", value = 2, min = 0, max = 2),
        numericInput(inputId = "ca", label = "Nombre de vaisseaux principaux colorés par fluoroscopie", value = 0, min = 0, max = 3),
        numericInput(inputId = "thal", label = "Défaut thalassémique (3 = normal, 6 = fixe, 7 = réversible)", value = 2, min = 0, max = 3),
        actionButton(inputId = "submit_button", label = "Soumettre")
      ),
      mainPanel(
        h3("Données d'entrée:"),
        verbatimTextOutput("inputData"),
        h3("Résultat de la prédiction:"),
        verbatimTextOutput("predictionResult")
      )
    )
  ),
  
  tabPanel(
    title = "Chat",
    fluidRow(
      column(
        width = 8,
        textInput(inputId = "user_message", label = "Dites nous ce que vous avez:"),
        actionButton(inputId = "send_message", label = "Envoyer"),
        hr(),
        uiOutput(outputId = "chat_log")  # Utilisez uiOutput ici
      )
    )
  )
)

# Fonction serveur
server <- function(input, output, session) {
  # Connect to MongoDB
  collection_heart_cleveland_updated <- mongo(collection = collection, url = mongo_url)
  collection_chatbox <- mongo(collection = chatbox, url = mongo_url)
  
  # Charger les données (simulation pour cet exemple)
  set.seed(2020)
  heart_disease_data <- as.data.frame(collection_heart_cleveland_updated$find())
  
  output$heatmapPlot <- renderPlot({
    corr_matrix <- cor(heart_disease_data)
    corr_melt <- melt(corr_matrix)
    
    ggplot(data = corr_melt, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1, 1), space = "Lab", 
                           name = "Correlation") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                       size = 12, hjust = 1)) + coord_fixed()
  })
  
  output$sexDistPlot <- renderPlot({
    min_count <- min(table(heart_disease_data$sex))
    sampled_data <- heart_disease_data %>%
      group_by(sex) %>%
      sample_n(min_count) %>%
      ungroup()
    
    ggplot(sampled_data, aes(x = factor(sex), fill = factor(condition))) +
      geom_bar(position = "dodge") +
      labs(title = "Distribution de la maladie par sexe", 
           x = "Sexe (0 = Femme, 1 = Homme)", 
           y = "Nombre de cas") +
      scale_fill_discrete(name = "Condition")
  })
  
  output$riskFactorPlot1 <- renderPlot({
    summary_data <- heart_disease_data %>%
      group_by(sex) %>%
      summarise(
        age = mean(age, na.rm = TRUE),
        chol = mean(chol, na.rm = TRUE),
        trestbps = mean(trestbps, na.rm = TRUE),
        thalach = mean(thalach, na.rm = TRUE),
        fbs = sum(fbs, na.rm = TRUE),
        restecg = sum(restecg, na.rm = TRUE),
        exang = sum(exang, na.rm = TRUE)
      )
    
    colnames(summary_data) <- c("Sexe", "Âge", "Cholestérol", "Pression artérielle", "Fréquence cardiaque", 
                                "Glycémie à jeun", "ECG au repos", "Angine induite par l'exercice")
    summary_data_long <- gather(summary_data, key = "Variable", value = "Valeur", -Sexe)
    summary_data_long$Sexe <- factor(summary_data_long$Sexe, labels = c("Femme", "Homme"))
    
    ggplot(summary_data_long, aes(x = Variable, y = Valeur, fill = Sexe)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      coord_flip() +
      labs(title = "Comparaison des facteurs de risque de maladie cardiaque entre les sexes",
           x = "Facteurs de risque",
           y = "Valeur",
           fill = "Sexe") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$riskFactorPlot2 <- renderPlot({
    min_count <- min(table(heart_disease_data$sex))
    sampled_data <- heart_disease_data %>%
      group_by(sex) %>%
      sample_n(min_count) %>%
      ungroup()
    
    summary_data2 <- sampled_data %>%
      group_by(sex) %>%
      summarise(
        oldpeak = mean(oldpeak, na.rm = TRUE),
        slope = mean(slope, na.rm = TRUE),
        ca = mean(ca, na.rm = TRUE),
        thal = mean(thal, na.rm = TRUE)
      )
    
    colnames(summary_data2) <- c("Sexe", "Oldpeak", "Pente du segment ST", "Nombre de vaisseaux colorés", 
                                 "Défaut thalassémique")
    summary_data_long2 <- gather(summary_data2, key = "Variable", value = "Valeur", -Sexe)
    summary_data_long2$Sexe <- factor(summary_data_long2$Sexe, labels = c("Femme", "Homme"))
    
    ggplot(summary_data_long2, aes(x = Variable, y = Valeur, fill = Sexe)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      coord_flip() +
      labs(title = "Comparaison des facteurs de risque de maladie cardiaque entre les sexes",
           x = "Facteurs de risque",
           y = "Valeur",
           fill = "Sexe") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$riskFactorPlot3 <- renderPlot({
    y_var <- input$variable_y
    
    ggplot(heart_disease_data, aes_string(x = "factor(condition)", y = y_var, fill = "factor(condition)")) +
      geom_boxplot() +
      labs(title = paste(y_var, "par condition de la maladie"), 
           x = "Condition (0 = Pas de maladie, 1 = Maladie)", 
           y = y_var) +
      scale_fill_manual(values = c("green", "red"))
  })
  
  output$scatterPlot <- renderPlot({
    y_var2 <- input$variable_y2
    condition_filter <- input$condition_filter
    
    filtered_data <- if (condition_filter == "Toutes") {
      heart_disease_data
    } else {
      heart_disease_data %>% filter(condition == as.numeric(condition_filter))
    }
    
    ggplot(filtered_data, aes_string(x = "age", y = y_var2, color = "factor(condition)")) +
      geom_point(size = 3) +
      labs(title = paste("Scatter Plot de", y_var2, "par âge et condition de la maladie"), 
           x = "Âge", 
           y = y_var2, 
           color = "Condition (0 = Pas de maladie, 1 = Maladie)") +
      scale_color_manual(values = c("green", "red"))
  })
  
  # Liste pour stocker les messages
  messages <- reactiveVal(character())
  
  # Ajouter un nouveau message à la liste
  observeEvent(input$send_message, {
    new_message <- input$user_message
    if (new_message != "") {
      # Ajouter la date et l'heure au message
      timestamped_message <- paste(Sys.time(), ":", new_message)
      
      messages(c(messages(), timestamped_message))
      
      # Insérer le message dans MongoDB
      collection_chatbox$insert(list(message = timestamped_message))
      
      # Réinitialiser le champ de saisie du message
      updateTextInput(session, "user_message", value = "")
    }
  })
  output$inputData <- renderPrint({
    new_data <- data.frame(
      age = input$age,
      sex = input$sex,
      cp = input$cp,
      trestbps = input$trestbps,
      chol = input$chol,
      fbs = input$fbs,
      restecg = input$restecg,
      thalach = input$thalach,
      exang = input$exang,
      oldpeak = input$oldpeak,
      slope = input$slope,
      ca = input$ca,
      thal = input$thal
    )
    new_data
  })
  observeEvent(input$submit_button, {
    new_data <- data.frame(
      age = input$age,
      sex = input$sex,
      cp = input$cp,
      trestbps = input$trestbps,
      chol = input$chol,
      fbs = input$fbs,
      restecg = input$restecg,
      thalach = input$thalach,
      exang = input$exang,
      oldpeak = input$oldpeak,
      slope = input$slope,
      ca = input$ca,
      thal = input$thal
    )
    prediction <- predict(rfFit, newdata = new_data)
    output$predictionResult <- renderText({
      paste("La prédiction pour l'individu est:", prediction)
    })
  })
  
  # Afficher les messages avec des sauts de ligne HTML
  output$chat_log <- renderUI({
    HTML(paste(messages(), collapse = "<br>"))
  })
}

# Créer l'application Shiny
shinyApp(ui = ui, server = server)