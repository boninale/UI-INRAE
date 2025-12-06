rm(list=objects())

project.dir <- getwd()
stopifnot(file.exists(project.dir))

data.dir <- paste0(project.dir, "/data")
script.dir <- paste0(project.dir, "/R")
source(paste0(script.dir,"/data_processing.R"))
source(paste0(script.dir,"/data_standardisation.R"))


etat_path<- paste0(data.dir,"/La Gaillarde.xlsx")
occup_path<- paste0(data.dir,"/","2025-Enquete occupation-MISTEA-20250709.xlsx")
conso_path <- paste0(data.dir,"/","Synthèse evolutions Consommations  10-02-2025.xlsx")

#test standardization

# Import des différentes feuilles
myconso_elec <- as.data.frame(
  read_excel(path = conso_path, sheet = 1, na = c("ND", "NA"))
)
myconso_eau <- as.data.frame(
  read_excel(path = conso_path, sheet = 2, na = c("ND", "NA"))
)
myconso_gaz <- as.data.frame(
  read_excel(path = conso_path, sheet = 3, na = c("ND", "NA"))
)
myconso_reseau <- as.data.frame(
  read_excel(path = conso_path, sheet = 4, na = c("ND", "NA"))
)
myconso_fod <- as.data.frame(
  read_excel(path = conso_path, sheet = 5, na = c("ND", "NA"))
)

print("Elec :")
print(names(myconso_elec))
myconso_elec <- standardise_table(myconso_elec)
print(names(myconso_elec))

print("Eau :")
print(names(myconso_eau))
myconso_eau <- standardise_table(myconso_eau)
print(names(myconso_eau))

print("Gaz :")
print(names(myconso_gaz))
myconso_gaz <- standardise_table(myconso_gaz)
print(names(myconso_gaz))

print("Reseau :")
print(names(myconso_reseau))
myconso_reseau <- standardise_table(myconso_reseau)
print(names(myconso_reseau))

print("Fod :")
print(names(myconso_fod))
myconso_fod <- standardise_table(myconso_fod)
print(names(myconso_fod))



#test entire script
result <- run_energy_analysis(etat_path, occup_path, conso_path)

myconso_elec2   <- result$myconso_elec2
myconso_gaz2    <- result$myconso_gaz2
myconso_eau2    <- result$myconso_eau2
myconso_fod2    <- result$myconso_fod2
myconso_reseau2 <- result$myconso_reseau2

myconso_parbat           <- result$myconso_parbat
myconso_parunite         <- result$myconso_parunite
myconso_parbatunite      <- result$myconso_parbatunite

# Write Excel files
agg_path <- file.path(paste0(project.dir, "/Results/Test_2025_export_aggregation_energies_consommees.xlsx"))
stats_path <- file.path(paste0(project.dir, "/Results/Test_2025_export_stats_energies_consommees.xlsx"))

writexl::write_xlsx(list(
  Elect  = result$myconso_elec2,
  Gaz    = result$myconso_gaz2,
  Eau    = result$myconso_eau2,
  FOD    = result$myconso_fod2,
  Reseau = result$myconso_reseau2
), agg_path)

writexl::write_xlsx(list(
  `Par Batiments`          = result$myconso_parbat,
  `Par Unites`             = result$myconso_parunite,
  `Par batiment et unites` = result$myconso_parbatunite
), stats_path)



