rm(list=objects())

project.dir <- "/Users/abonin/Desktop/Etude Jema"
stopifnot(file.exists(project.dir))


source(paste0(project.dir,"/R/data_processing.R"))

data.dir <- paste0(project.dir, "/data")
script.dir <- paste0(project.dir, "/R")

etat_path<- paste0(data.dir,"/La Gaillarde.xlsx")
occup_path<- paste0(data.dir,"/","2025-Enquete occupation-MISTEA-20250709.xlsx")
conso_path <- paste0(data.dir,"/","Synthèse evolutions Consommations  10-02-2025.xlsx")

result <- run_energy_analysis(etat_path, occup_path, conso_path)

myconso_elec2   <- result$myconso_elec2
myconso_gaz2    <- result$myconso_gaz2
myconso_eau2    <- result$myconso_eau2
myconso_fod2    <- result$myconso_fod2
myconso_reseau2 <- result$myconso_reseau2

myconso_parbat           <- result$myconso_parbat
myconso_parunite         <- result$myconso_parunite
myconso_parbatunite      <- result$myconso_parbatunite

