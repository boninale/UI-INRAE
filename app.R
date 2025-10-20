# ============================================================
# Application Shiny : Analyse des fluides - Centre Occitanie Montpellier
# Basée sur le notebook de N. Hilgert et I. Sanchez (INRAE MISTEA)
# ============================================================

ui <- dashboardPage(
    dashboardHeader(title = "Analyse des fluides - La Gaillarde"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Tableaux de bord", tabName = "plots", icon = icon("chart-bar")),
        menuItem("Fichiers & Export", tabName = "files", icon = icon("folder-open"))
      )
    ),
    
    dashboardBody(
      useShinyjs(),
      
      tabItems(
        
        # --- File management tab ---
        tabItem(
          tabName = "files",
          fluidPage(
            div(class = "container", style = "max-width:900px; margin: 40px auto;",
                h2("Chargement des fichiers", style = "text-align:center; font-weight:700; margin-bottom:30px;"),
                
                # File inputs on separate lines
                fileInput("file_etat", "Importer : La Gaillarde.xlsx", accept = ".xlsx", width = "100%"),
                fileInput("file_occup", "Importer : 2025-Enquete occupation-MISTEA.xlsx", accept = ".xlsx", width = "100%"),
                fileInput("file_conso", "Importer : Synthèse Consommations.xlsx", accept = ".xlsx", width = "100%"),
                
                br(),
                fluidRow(
                  column(6, actionButton("process", "Lancer l'analyse", class = "btn-primary btn-lg", width = "100%")),
                  column(6, downloadButton("download_data", "Télécharger les résultats", class = "btn-success btn-lg", width = "100%"))
                ),
                
                br(),
                tableOutput("file_table"),
                uiOutput("error_box")
            )
          )
        ),
        
        # --- Plots tab ---
        tabItem(
          tabName = "plots",
          fluidPage(
            h2("Visualisations des consommations", style = "text-align:center; margin-top:10px; margin-bottom:25px;"),
            
            fluidRow(
              box(title = "Consommation par énergie", width = 12, status = "primary", solidHeader = TRUE,
                  plotlyOutput("energy_plotly", height = "500px"))
            ),
            
            fluidRow(
              box(title = "Évolution temporelle", width = 12, status = "primary", solidHeader = TRUE,
                  plotOutput("time_series_plot", height = "400px"))
            ),
            
            fluidRow(
              box(title = "Répartition par bâtiment", width = 12, status = "primary", solidHeader = TRUE,
                  plotlyOutput("building_plot", height = "400px"))
            )
          )
        )
      )
    )
)

# =========================================
#               Server 
# =========================================
server <- function(input, output, session) {
  library(shiny)
  library(shinydashboard)
  library(plotly)
  library(readxl)
  library(writexl)
  library(dplyr)
  library(shinyjs)
  
  # --- INITIALIZATION (same as the Rmd file) ------------------------------------------------------
  
  
  # Detect machine-specific path
  nodename <- Sys.info()[["nodename"]]
  
  if (nodename == "MISTEA-PRUNUS") {
    project.dir <- "C:/Users/sanchez/Documents/INRA MISTEA/analyse_fluides_mtp"
  } else if (nodename == "mistea-sureau") {
    project.dir <- "/home/sanchezi/Documents/INRA/UMR MISTEA/analyse_fluides_mtp"
  } else if (nodename == "MISTEA-ABELIA") {
    project.dir <- "C:/Users/hilgert.MTP/Documents/NadineMaia2021/labo/cr/UMR/GESTION/UMRadmin/2025/FluidesCentre/analyse_fluides_mtp"
  } else {
    project.dir <- "/Users/abonin/Desktop/Etude Jema"
  }
  
  # Validate
  if (!file.exists(project.dir)) {
    stop(paste("Project directory not found:", project.dir))
  }
  
  # Define subpaths
  data.dir   <- file.path(project.dir, "data")
  script.dir <- file.path(project.dir, "R")
  
  # Source modular scripts
  source(file.path(script.dir, "data_processing.R"), local = TRUE)
  #source(file.path(script.dir, "plot.R"), local = TRUE)
  
  # Reactive store for processed data
  processed_data <- reactiveVal(NULL)
  analysis_error <- reactiveVal(NULL)
  
  # --- File check table
  output$file_table <- renderTable({
    data.frame(
      "Fichier" = c("La Gaillarde.xlsx", "2025-Enquete occupation-MISTEA.xlsx", "Synthèse Consommations.xlsx"),
      "Importé" = c(!is.null(input$file_etat),
                    !is.null(input$file_occup),
                    !is.null(input$file_conso))
    )
  })
  
  # --- Run analysis when button pressed
  observeEvent(input$process, {
    
    req(input$file_etat, input$file_occup, input$file_conso)
    
    analysis_error(NULL)  # reset previous errors
    showNotification("Analyse en cours...", type = "message", duration = NULL, id = "processing")
    
    tryCatch({
      # Process data in the same way as the Rmd script, while silencing outputs with capture.outputs
      invisible(capture.output(
        result <- run_energy_analysis(
          etat_path  = input$file_etat$datapath,
          occup_path = input$file_occup$datapath,
          conso_path = input$file_conso$datapath
        )
      ))
      
      # Store data in a reactive value
      processed_data(result)
      analysis_error(NULL)
      removeNotification("processing")
      showNotification("Analyse terminée avec succès.", type = "message")
      
    }, error = function(e) {
      removeNotification("processing")
      processed_data(NULL)
      analysis_error(conditionMessage(e))
      showNotification("Erreur lors de l'analyse.", type = "error")
    })
  })
  
  # --- Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("2025_export_aggregation_energies_consommees_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      export_energy_results(processed_data(), file)
    }
  )
  # --- Display any analysis errors in the UI
  output$error_box <- renderUI({
    req(analysis_error())
    box(
      title = "Erreur lors de l'analyse",
      width = 12,
      status = "danger",
      solidHeader = TRUE,
      pre(analysis_error())
    )
  })
  
  # --- Plot rendering
  output$energy_plotly <- renderPlotly({
    req(processed_data())
    ggplotly(plot_energy_by_type(processed_data()))
  })
  
  output$time_series_plot <- renderPlot({
    req(processed_data())
    plot_time_series(processed_data())
  })
  
  output$building_plot <- renderPlotly({
    req(processed_data())
    ggplotly(plot_building_distribution(processed_data()))
  })
}


shinyApp(ui, server)
