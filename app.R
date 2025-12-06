# ============================================================
# Application Shiny : Analyse des fluides - Centre Occitanie Montpellier
# Basée sur le notebook de N. Hilgert et I. Sanchez (INRAE MISTEA)
# ============================================================
library(plotly)
library(readxl)
library(writexl)
library(dplyr)
library(shinyjs)
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
    dashboardHeader(title = "Analyse des fluides - La Gaillarde"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Fichiers & Export", tabName = "files", icon = icon("folder-open")),
        menuItem("Tableaux de bord", tabName = "plots", icon = icon("chart-bar"))
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
                
                h2("Chargement des fichiers",
                   style = "text-align:center; font-weight:700; margin-bottom:30px;"),
                
                # File inputs
                fileInput("file_etat",  "Importer : La Gaillarde.xlsx",
                          accept = ".xlsx", width = "100%"),
                fileInput("file_occup", "Importer : 2025-Enquete occupation-MISTEA.xlsx",
                          accept = ".xlsx", width = "100%"),
                fileInput("file_conso", "Importer : Synthèse Consommations.xlsx",
                          accept = ".xlsx", width = "100%"),
                
                br(),
                
                #   Paramètres de calcul
                h3("Paramètres de calcul", style = "font-weight:600; margin-top:20px;"),
                
                fluidRow(
                  column(4, numericInput("ct_edf",    "Coût électricité (€/kWh)",   value = 0.09, step = 0.01)),
                  column(4, numericInput("ct_gaz",    "Coût gaz (€/kWh)",           value = 0.07, step = 0.01)),
                  column(4, numericInput("ct_fod",    "Coût FOD (€/L)",             value = 0.32, step = 0.01))
                ),
                
                fluidRow(
                  column(4, numericInput("ct_eau",    "Coût eau (€/m³)",            value = 3.50, step = 0.01)),
                  column(4, numericInput("ct_reseau", "Coût réseau chaleur (€/kWh)",value = 0.07, step = 0.01))
                ),
                
                br(),
                
                fluidRow(
                  column(6, actionButton("process", "Lancer l'analyse",
                                         class = "btn-primary btn-lg", width = "100%")),
                  column(6, downloadButton("download_all", "Télécharger les résultats complets",
                                           class = "btn-success btn-lg", width = "100%"))
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
  print("Before sourcing script")
  invisible(source(file.path(script.dir, "data_processing.R"), local = TRUE))
  print("After Sourcing script")
  # source(file.path(script.dir, "create_excel.R"), local = TRUE)
  
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
          conso_path = input$file_conso$datapath,
          ct_edf     = input$ct_edf,
          ct_gaz     = input$ct_gaz,
          ct_fod     = input$ct_fod,
          ct_eau     = input$ct_eau,
          ct_reseau  = input$ct_reseau
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
  output$download_all <- downloadHandler(
      filename = function() {
        paste0("Analyse_fluides_", Sys.Date(), ".zip")
      },
      content = function(file) {
        req(processed_data())
        data <- processed_data()
        # Create temporary Excel files
        tmp_dir  <- tempdir()
        agg_path <- file.path(tmp_dir, "2025_export_aggregation_energies_consommees.xlsx")
        stats_path <- file.path(tmp_dir, "2025_export_stats_energies_consommees.xlsx")
        
        # Write Excel files
        writexl::write_xlsx(list(
          Elect  = data$myconso_elec2,
          Gaz    = data$myconso_gaz2,
          Eau    = data$myconso_eau2,
          FOD    = data$myconso_fod2,
          Reseau = data$myconso_reseau2
        ), agg_path)
        
        writexl::write_xlsx(list(
          `Par Batiments`          = data$myconso_parbat,
          `Par Unites`             = data$myconso_parunite,
          `Par batiment et unites` = data$myconso_parbatunite
        ), stats_path)
        
        # Zip them
        zip::zipr(
          zipfile = file,
          files   = c(agg_path, stats_path)
        )
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
  
#   # --- Plot rendering
#   output$energy_plotly <- renderPlotly({
#     req(processed_data())
#     ggplotly(plot_energy_by_type(processed_data()))
#   })
#   
#   output$time_series_plot <- renderPlot({
#     req(processed_data())
#     plot_time_series(processed_data())
#   })
#   
#   output$building_plot <- renderPlotly({
#     req(processed_data())
#     ggplotly(plot_building_distribution(processed_data()))
#   })
}

shinyApp(ui, server)
