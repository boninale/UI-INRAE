run_energy_analysis <- function(etat_path, occup_path, conso_path) {
  # Load required packages locally to ensure function independence
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readxl)
  library(DT)
  
  # Importation des données

  ## fichier Etat énergétique des batiments --- 'La Gaillarde.xlsx'

  # Import the first sheet
  myetat <- read_excel(
    path = etat_path,
    sheet = 1,
    na = c("ND", "NA")
  )
  
  colnames(myetat)<-c("Site","num_bat","nom_bat","Clos/couvert","securite",
                      "fonctionnement","etat_energie","confort d'usage adéquat aux besoins")
  
  ## Fichier Occupation des batiments par agents INRAE --- "2025-Enquete occupation-MISTEA.xlsx"

  # Feuille 2: Occupation

  myoccup <- read_excel(
    path = occup_path,
    sheet = 2,
    na = c("ND", "NA")
  ) |> 
    as.data.frame()
  
  myoccup<-myoccup[,-c(14,15)]
  
  colnames(myoccup)<-c("Site","num_bat","Local","Nom_usuel_local","Libell_fichiersource/DIE",
                       "Referencefichiersource/DIE","Surface_sol","Unite",
                       "Nom Tiers accueillis_31/03/2024","Nom_occupants",
                       "Nombre_unites","Surface_ajus","Typologie UMR")

  # Comptage du nombre de personnes par bureau à partir de la colonne "Nom_occupants"
  # attention au différent vocabulaire pour séparer qd plusieurs personnes
  # attention, à revoir car il y a les stagiaires et les personnels Institut Agro
  # Après je prendrais les infos de l'onglet "Effectifs" quand j'aurais l'info du batiment
  # + rajout de qqs règles d'execption
  myoccup<- myoccup %>%
    group_by(Site, num_bat) %>%
    mutate(count_pers1=str_count(Nom_occupants,",|/| et | - |\\+|;"),
           count_pers0=count_pers1 + 1,
           count_pers0=if_else(str_count(Nom_occupants,"Locaux") > 0,0,count_pers0),
           count_pers0=if_else(str_count(Nom_occupants,"véhicules|\\?|COMMUN") > 0,
                               0,count_pers0),
           count_pers0=if_else(str_count(Nom_occupants,"Salle|salle") > 0,
                               0,count_pers0),
           count_pers0=if_else(str_count(Nom_occupants,"4 personnes")> 0,4,count_pers0)
    ) %>%
    select(-count_pers1)
  
  # agrégation par batiment pour avoir l'occupation totale
  myoccup1<- myoccup %>%
    group_by(Site, num_bat) %>%
    summarize(metrage_batiment=sum(Surface_sol,na.rm=TRUE))
  
  # agrégation par batiment et unité pour avoir l'occupation totale + occupation INRAE
  myoccup2<- myoccup %>%
    group_by(Site, num_bat, Unite) %>%
    summarize(metrage_unite=sum(Surface_sol,na.rm=TRUE),
              metrage_unite_ajus=sum(Surface_ajus,na.rm=TRUE),
              pers_unite=sum(count_pers0,na.rm=TRUE))
  
  # jointure dans myoccup2
  myoccup2<- left_join(myoccup2,myoccup1,by=c("Site", "num_bat"))
  
  #-------------------------#
  # Feuille 3: Effectifs
  #-------------------------#
  myeffectif <- read_excel(
    path = occup_path,
    sheet = 3,
    na = c("ND", "NA")
  ) |> 
    as.data.frame()
  myeffectif<-myeffectif[,-c(9,10)]
  names(myeffectif)[1] <- "Unite"
  
  myeffectif<-myeffectif %>%
    mutate(num_bat=substr(Bat,2,5))
  
  count.eff<-myeffectif %>%
    group_by(num_bat,Unite) %>%
    mutate(num_bat=if_else(Unite == "Associé BAP","0070",num_bat)) %>%
    summarize(count_pers=n())
  
  sort(unique(myoccup2$Unite))
  
  # On ne conserve que les unités INRAE (i.e. on supprime les occurences du mot "IA")
  # On supprime le logement de fonction innocuppé dont le nom de batiment est INRAE
  # On passe tous les noms d'unité en majuscule pour cohérence d'orthographe
  myoccup2 <- myoccup2 %>%
    mutate(
      Unite = toupper(Unite),
      Unite=replace(Unite,which(Unite %in% c('SDAR MUTUALISÉ','SDAR Mutualisé')),values="SDAR"),
      Unite=replace(Unite,which(Unite == "LEPSE/IPSIM"),values="LEPSE/IPSiM")
    ) 
  myoccup3<-myoccup2[ !myoccup2$Unite %in% c("IA","IA COM","INRAE"),]
  
  # dataset d'occupation centre de la gaillarde
  myoccup.gaillarde<-myoccup3[grep("Gaillarde",myoccup2$Site),]
  
  # suppression des unite == NA
  myoccup.gaillarde<-na.omit(myoccup.gaillarde)
  
  myoccup.gaillarde<-myoccup.gaillarde %>%
    mutate(
      Unite=toupper(Unite),
      Unite=replace(Unite,which(Unite %in% c('SDAR MUTUALISÉ','SDAR Mutualisé')),values="SDAR"),
      Unite=replace(Unite,which(Unite == "LEPSE/IPSiM"),values="LEPSE/IPSIM")
    ) %>%
    left_join(count.eff,by=c("num_bat","Unite"))
  
  sort(unique(myoccup.gaillarde$Unite))

  # Calcul par unité du métrage ajusté
  resum.occup.gaillarde<-myoccup.gaillarde %>%
    mutate(
      Unite2=toupper(Unite),
      Unite2=replace(Unite2,which(Unite2 %in% c('SDAR MUTUALISÉ','SDAR Mutualisé')),values="SDAR"),
      Unite2=replace(Unite2,which(Unite2 == "LEPSE/IPSIM"),values="LEPSE/IPSiM")
    ) %>%
    group_by(Unite2) %>%
    summarize(sum_metrage=sum(metrage_unite),
              sum_metrage_ajus=sum(metrage_unite_ajus))
  
  sort(unique(resum.occup.gaillarde$Unite2))
  
  ## Fichier Consommations 2024 --- Import "Synthèse Consommations.xlsx"

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
  
  names(myconso_elec)<-c("batiment","2020_Conso_kWh","2021_Conso_kWh",
                         "2022_Conso_kWh","2023_Conso_kWh","2024_Conso_kWh",
                         "Evol_2023/2024p","Evol_2023/2024","Evol_2021/2024","Remarques")
  #Modifié elec car variables manquantes dans analyse
  names(myconso_fod)<-c("batiment","2023_Conso_kWh","2024_Conso_kWh",
                        "Evol_2023/2024","Evol_2023/2024p","Remarques")
  names(myconso_gaz)<-c("batiment","2023_Conso_kWh","2024_Conso_kWh",
                        "Evol_2023/2024","Evol_2023/2024p","Remarques")
  names(myconso_reseau)<-c("batiment","NA","2023_Conso","2024_Conso",
                           "Evol_2023/2024","Evol_2023/2024p","Remarques")
  names(myconso_eau)<-c("batiment","2020_Conso","2021_Conso","2022_Conso",
                        "2023_Conso","2024_Conso","Evol_2023/2024",
                        "Evol_2023/2024p","Remarques","V10","V11")
  
  myconso_reseau<-myconso_reseau[,-2]
  
  ### data-management des données de consommations
  
  # batiment avec fuel
  myconso_fod <- myconso_fod %>%
    mutate(num_bat=c("0290","0310","0030"))
  
  # batiment avec electricité
  myconso_elec$num_bat<-substr(myconso_elec[,1],5,8)
  
  # A REVOIR: somme de la ligne du batiment 0110 (restauration et hors restauration)
  myconso_elec<-myconso_elec %>%
    group_by(num_bat) %>%
    summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)))
  
  # batiment avec gaz
  myconso_gaz$num_bat<-substr(myconso_gaz[,1],5,8)
  
  # batiment avec geothermie
  myconso_reseau$num_bat<-substr(myconso_reseau[,1],5,8)
  
  # batiment avec eau
  myconso_eau$num_bat<-substr(myconso_eau[,1],5,8)
  
  # jointure consommation - état énergétique
  myconso_elec2<-left_join(myconso_elec,myetat,by="num_bat")
  myconso_gaz2<-left_join(myconso_gaz,myetat,by="num_bat")
  myconso_fod2<-left_join(myconso_fod,myetat,by="num_bat")
  myconso_reseau2<-left_join(myconso_reseau,myetat,by="num_bat")
  myconso_eau2<-left_join(myconso_eau,myetat,by="num_bat")
  
  # jointure consommation - occupation sols
  myconso_elec2<-left_join(myconso_elec2,myoccup.gaillarde,by="num_bat")
  myconso_gaz2<-left_join(myconso_gaz2,myoccup.gaillarde,by="num_bat")
  myconso_fod2<-left_join(myconso_fod2,myoccup.gaillarde,by="num_bat")
  myconso_reseau2<-left_join(myconso_reseau2,myoccup.gaillarde,by="num_bat",relationship = "many-to-many")
  myconso_eau2<-left_join(myconso_eau2,myoccup.gaillarde,by="num_bat",relationship = "many-to-many")
  
  # Transposition par année de consommation (pour les graphs)
  myconso_elec3<-pivot_longer(myconso_elec2,cols=2:6,names_to="Consommation")
  myconso_gaz3<-pivot_longer(myconso_gaz2,cols=2:3,names_to="Consommation")
  
  # Suppression irrigation et restauration
  myconso_elec2<-myconso_elec2[!myconso_elec2$num_bat %in% c("irag"),]
  myconso_eau2<-myconso_eau2[!myconso_eau2$num_bat %in% c("sage"),]
  
  ### -------------------
  # Plots Consommation au cours du temps
  ### -------------------
  
  # Calcul coût énergétique annuel global
  
  # fonction de calcul prix global
  myprix<-function(coef1,prix_kwh,input_col,input_pct){
    tp1<-sum(input_col * prix_kwh * coef1)
    tp2<-tp1 * input_pct
    return(c(tp1,tp2))
  }
  
  # Calcul prix total élec
  ct_edf<-0.09
  ct_gaz<-0.07
  ct_fod<-0.32
  ct_eau<-3.5
  ct_reseau<-0.07
  
  prix.elec  <-myprix(coef1=1.5,prix_kwh=ct_edf,input_col=myconso_elec$`2024_Conso_kWh`,input_pct=0.6)
  prix.gaz   <-myprix(coef1=1.5,prix_kwh=ct_gaz,input_col=myconso_gaz$`2024_Conso_kWh`,input_pct=0.6)
  prix.eau   <-myprix(coef1=1,  prix_kwh=ct_eau,input_col=myconso_eau$`2024_Conso`,input_pct=0.6)
  prix.fod   <-myprix(coef1=1.5,prix_kwh=ct_fod,input_col=myconso_fod$`2024_Conso_kWh`,input_pct=0.6)
  prix.reseau<-myprix(coef1=1.5,prix_kwh=ct_reseau,input_col=myconso_reseau$`2024_Conso`,input_pct=0.6)
  
  # # prix électricité
  # prix.elec
  # 
  # # prix gaz
  # prix.gaz
  # 
  # # prix eau
  # prix.eau
  # 
  # # prix fuel
  # prix.fod
  # 
  # # prix geothermie
  # prix.reseau
  # 
  # # prix total (avec géothermie)
  # prix.elec + prix.gaz + prix.eau + prix.fod + prix.reseau
  # 
  # # prix total (sans geothermie)
  # prix.elec + prix.gaz + prix.eau + prix.fod
  
  # Ventilation coût INRAE suivant occupation des sols
  
  myconso_elec2<-myconso_elec2 %>%
    mutate(ratio_metre=metrage_unite_ajus / metrage_batiment,
           conso_elec2024_kwh = `2024_Conso_kWh` * ratio_metre,
           cout_elec2024=conso_elec2024_kwh * ct_edf * 1.5,
           cout_par_m2=cout_elec2024 / metrage_unite_ajus,
           cout_par_pers=cout_elec2024 / count_pers)
  
  myconso_gaz2<-myconso_gaz2 %>%
    mutate(ratio_metre=metrage_unite_ajus / metrage_batiment,
           conso_gaz2024_kwh = `2024_Conso_kWh` * ratio_metre,
           cout_gaz2024=conso_gaz2024_kwh * ct_gaz * 1.5,
           cout_par_m2=cout_gaz2024 / metrage_unite_ajus,
           cout_par_pers=cout_gaz2024 / count_pers)
  
  myconso_eau2<-myconso_eau2 %>%
    mutate(ratio_metre=metrage_unite_ajus / metrage_batiment,
           conso_eau2024 = `2024_Conso` * ratio_metre,
           cout_eau2024=conso_eau2024 * ct_eau * 1,
           cout_par_m2=cout_eau2024 / metrage_unite_ajus,
           cout_par_pers=cout_eau2024 / count_pers)
  

  myconso_fod2<-myconso_fod2 %>%
    mutate(ratio_metre=metrage_unite_ajus / metrage_batiment,
           conso_fod2024 = `2024_Conso_kWh` * ratio_metre,
           cout_fod2024=conso_fod2024 * ct_fod * 1.5,
           cout_par_m2=cout_fod2024 / metrage_unite_ajus,
           cout_par_pers=cout_fod2024 / count_pers)
  
  myconso_reseau2<-myconso_reseau2 %>%
    mutate(ratio_metre=metrage_unite_ajus / metrage_batiment,
           conso_reseau2024 = `2024_Conso` * ratio_metre,
           cout_reseau2024=conso_reseau2024 * ct_reseau * 1.5,
           cout_par_m2=cout_reseau2024 / metrage_unite_ajus,
           cout_par_pers=cout_reseau2024 / count_pers)
  
  ### -------------------
  # Plots consommation par unité
  ### -------------------
  
  # Consommation par batiment /m²
  myconso_elec_bat<- myconso_elec2 %>%
    select(num_bat,`2024_Conso_kWh`,etat_energie,
           metrage_batiment) %>%
    distinct(num_bat,.keep_all = TRUE) %>%
    mutate(conso_elec_m2_kWh=`2024_Conso_kWh` / metrage_batiment,
           cout_elec=conso_elec_m2_kWh * ct_edf * 1.5)
  
  myconso_gaz_bat<- myconso_gaz2 %>%
    select(num_bat,`2024_Conso_kWh`,etat_energie,
           metrage_batiment) %>%
    distinct(num_bat,.keep_all = TRUE) %>%
    mutate(conso_gaz_m2_kWh=`2024_Conso_kWh` / metrage_batiment,
           cout_gaz=conso_gaz_m2_kWh * ct_gaz * 1.5)
  
  myconso_fod_bat<- myconso_fod2 %>%
    select(num_bat,`2024_Conso_kWh`,etat_energie,
           metrage_batiment) %>%
    distinct(num_bat,.keep_all = TRUE) %>%
    mutate(conso_fod_m2_kWh=`2024_Conso_kWh` / metrage_batiment,
           cout_fod=conso_fod_m2_kWh * ct_fod * 1.5)
  
  myconso_eau_bat<- myconso_eau2 %>%
    select(num_bat,`2024_Conso`,etat_energie,
           metrage_batiment) %>%
    distinct(num_bat,.keep_all = TRUE) %>%
    mutate(conso_eau_m2=`2024_Conso` / metrage_batiment,
           cout_eau=conso_eau_m2 * ct_eau * 1)
  
  myconso_reseau_bat<- myconso_reseau2 %>%
    select(num_bat,`2024_Conso`,etat_energie,
           metrage_batiment) %>%
    distinct(num_bat,.keep_all = TRUE) %>%
    mutate(conso_reseau_m2=`2024_Conso` / metrage_batiment,
           cout_reseau=conso_reseau_m2 * ct_reseau * 1.5)
  
  myconso_parbat <- full_join(select(myconso_elec_bat,num_bat,etat_energie,
                                     metrage_batiment,conso_elec_m2_kWh,cout_elec),
                              select(myconso_gaz_bat,num_bat,conso_gaz_m2_kWh,cout_gaz),by="num_bat")
  myconso_parbat <- full_join(myconso_parbat,select(myconso_eau_bat,num_bat,conso_eau_m2,cout_eau),by="num_bat")
  myconso_parbat <- full_join(myconso_parbat,select(myconso_fod_bat,num_bat,conso_fod_m2_kWh,cout_fod),by="num_bat")
  myconso_parbat <- full_join(myconso_parbat,select(myconso_reseau_bat,num_bat,conso_reseau_m2,cout_reseau),by="num_bat")
  
  # somme du cout de toutes les énergies par batiment
  myconso_parbat <- myconso_parbat %>%
    mutate(cout_global_bat=rowSums(across(starts_with("cout")),na.rm=TRUE))
  
  ### -------------------
  # Plots consommation selon état énergétique
  ### -------------------
  
  # Consommation par unité /m²
  myconso_elec_unite<- myconso_elec2 %>%
    select(num_bat,Unite,`2024_Conso_kWh`,etat_energie,
           metrage_unite_ajus,metrage_batiment,count_pers) %>%
    mutate(conso_elec_m2_kWh=`2024_Conso_kWh` * metrage_unite_ajus / metrage_batiment,
           cout_elec=conso_elec_m2_kWh * ct_edf * 1.5) %>%
    group_by(Unite) %>%
    summarise(conso_elec_m2_kWh=sum(conso_elec_m2_kWh),
              cout_elec=sum(cout_elec)) %>%
    na.omit()
  
  myconso_gaz_unite<- myconso_gaz2 %>%
    select(num_bat,Unite,`2024_Conso_kWh`,etat_energie,
           metrage_unite_ajus,metrage_batiment,count_pers) %>%
    mutate(conso_gaz_m2_kWh=`2024_Conso_kWh` * metrage_unite_ajus / metrage_batiment,
           cout_gaz=conso_gaz_m2_kWh * ct_gaz * 1.5) %>%
    group_by(Unite) %>%
    summarise(conso_gaz_m2_kWh=sum(conso_gaz_m2_kWh),
              cout_gaz=sum(cout_gaz)) %>%
    na.omit()
  
  myconso_fod_unite<- myconso_fod2 %>%
    select(num_bat,Unite,`2024_Conso_kWh`,etat_energie,
           metrage_unite_ajus,metrage_batiment,count_pers) %>%
    mutate(conso_fod_m2_kWh=`2024_Conso_kWh` * metrage_unite_ajus / metrage_batiment,
           cout_fod=conso_fod_m2_kWh * ct_fod * 1.5) %>%
    group_by(Unite) %>%
    summarise(conso_fod_m2_kWh=sum(conso_fod_m2_kWh),
              cout_fod=sum(cout_fod)) %>%
    na.omit() %>%
    na.omit()
  
  myconso_eau_unite<- myconso_eau2 %>%
    select(num_bat,Unite,`2024_Conso`,etat_energie,
           metrage_unite_ajus,metrage_batiment,count_pers) %>%
    mutate(conso_eau_m2=`2024_Conso` * metrage_unite_ajus / metrage_batiment,
           cout_eau=conso_eau_m2 * ct_eau * 1) %>%
    group_by(Unite) %>%
    summarise(conso_eau_m2=sum(conso_eau_m2),
              cout_eau=sum(cout_eau)) %>%
    na.omit()
  
  myconso_reseau_unite<- myconso_reseau2 %>%
    select(num_bat,Unite,`2024_Conso`,etat_energie,
           metrage_unite_ajus,metrage_batiment,count_pers) %>%
    mutate(conso_reseau_m2=`2024_Conso` * metrage_unite_ajus / metrage_batiment,
           cout_reseau=conso_reseau_m2 * ct_reseau * 1.5) %>%
    group_by(Unite) %>%
    summarise(conso_reseau_m2=sum(conso_reseau_m2),
              cout_reseau=sum(cout_reseau))
  
  myconso_parunite <- full_join(myconso_elec_unite,myconso_gaz_unite,by="Unite")
  myconso_parunite <- full_join(myconso_parunite,myconso_eau_unite,by="Unite")
  myconso_parunite <- full_join(myconso_parunite,myconso_fod_unite,by="Unite")
  myconso_parunite <- full_join(myconso_parunite,myconso_reseau_unite,by="Unite")
  
  # Redistribution des consommations des les lieux communs
  myconso_parunite<-myconso_parunite %>%
    mutate(countU=str_count(myconso_parunite$Unite,"/")+1) 
  myconso_parunite<-as.data.frame(myconso_parunite)
  
  vecUnite<-unique(myconso_parunite$Unite)
  vecUnite<-vecUnite[-grep(pattern="/",vecUnite)]
  
  conso_elec_m2_kWhb<-vector("numeric",length(vecUnite))
  conso_gaz_m2_kWhb<-vector("numeric",length(vecUnite))
  conso_eau_m2b<-vector("numeric",length(vecUnite))
  conso_fod_m2_kWhb<-vector("numeric",length(vecUnite))
  conso_reseau_m2b<-vector("numeric",length(vecUnite))
  
  for (i in seq_along(vecUnite)){
    for (j in 1:nrow(myconso_parunite)){
      if (length(grep(pattern=vecUnite[i],myconso_parunite[j,"Unite"])) != 0){
        conso_elec_m2_kWhb[i]<-conso_elec_m2_kWhb[i] + 
          myconso_parunite[j,"conso_elec_m2_kWh"] / myconso_parunite[j,"countU"]
        conso_gaz_m2_kWhb[i]<-conso_gaz_m2_kWhb[i] + 
          myconso_parunite[j,"conso_gaz_m2_kWh"] / myconso_parunite[j,"countU"]
        conso_eau_m2b[i]<-conso_eau_m2b[i] + 
          myconso_parunite[j,"conso_eau_m2"] / myconso_parunite[j,"countU"]
        conso_fod_m2_kWhb[i]<-conso_fod_m2_kWhb[i] + 
          myconso_parunite[j,"conso_fod_m2_kWh"] / myconso_parunite[j,"countU"]
        conso_reseau_m2b[i]<-conso_reseau_m2b[i] + 
          myconso_parunite[j,"conso_reseau_m2"] / myconso_parunite[j,"countU"]
      }
    } 
  }
  
  myconso_parunite2<-cbind.data.frame(vecUnite,conso_elec_m2_kWhb,
                                      conso_gaz_m2_kWhb,conso_eau_m2b,
                                      conso_fod_m2_kWhb,conso_reseau_m2b)
  
  # somme du cout de toutes les énergies par unite
  myconso_parunite <- myconso_parunite %>%
    mutate(cout_global_unite=rowSums(across(starts_with("cout")),na.rm=TRUE))
  
  # Consommation par batiment et unité /m²
  myconso_elec_batu<- myconso_elec2 %>%
    select(num_bat,Unite,`2024_Conso_kWh`,etat_energie,
           metrage_unite_ajus,metrage_batiment,count_pers) %>%
    mutate(conso_elec_m2_kWh=`2024_Conso_kWh` * metrage_unite_ajus / metrage_batiment,
           cout_elec=conso_elec_m2_kWh * ct_edf * 1.5) %>%
    group_by(num_bat,Unite) %>%
    summarise(conso_elec_m2_kWh=sum(conso_elec_m2_kWh),
              cout_elec=sum(cout_elec))
  
  myconso_gaz_batu<- myconso_gaz2 %>%
    select(num_bat,Unite,`2024_Conso_kWh`,etat_energie,
           metrage_unite_ajus,metrage_batiment,count_pers) %>%
    mutate(conso_gaz_m2_kWh=`2024_Conso_kWh` * metrage_unite_ajus / metrage_batiment,
           cout_gaz=conso_gaz_m2_kWh * ct_gaz * 1.5) %>%
    group_by(num_bat,Unite) %>%
    summarise(conso_gaz_m2_kWh=sum(conso_gaz_m2_kWh),
              cout_gaz=sum(cout_gaz))
  
  myconso_fod_batu<- myconso_fod2 %>%
    select(num_bat,Unite,`2024_Conso_kWh`,etat_energie,
           metrage_unite_ajus,metrage_batiment,count_pers) %>%
    mutate(conso_fod_m2_kWh=`2024_Conso_kWh` * metrage_unite_ajus / metrage_batiment,
           cout_fod=conso_fod_m2_kWh * ct_fod * 1.5) %>%
    group_by(num_bat,Unite) %>%
    summarise(conso_fod_m2_kWh=sum(conso_fod_m2_kWh),
              cout_fod=sum(cout_fod))
  
  myconso_eau_batu<- myconso_eau2 %>%
    select(num_bat,Unite,`2024_Conso`,etat_energie,
           metrage_unite_ajus,metrage_batiment,count_pers) %>%
    mutate(conso_eau_m2=`2024_Conso` * metrage_unite_ajus / metrage_batiment,
           cout_eau=conso_eau_m2 * ct_eau * 1) %>%
    group_by(num_bat,Unite) %>%
    summarise(conso_eau_m2=sum(conso_eau_m2),
              cout_eau=sum(cout_eau))
  
  myconso_reseau_batu<- myconso_reseau2 %>%
    select(num_bat,Unite,`2024_Conso`,etat_energie,
           metrage_unite_ajus,metrage_batiment,count_pers) %>%
    mutate(conso_reseau_m2=`2024_Conso` * metrage_unite_ajus / metrage_batiment,
           cout_reseau=conso_reseau_m2 * ct_reseau * 1.5) %>%
    group_by(num_bat,Unite) %>%
    summarise(conso_reseau_m2=sum(conso_reseau_m2),
              cout_reseau=sum(cout_reseau))
  
  myconso_parbatunite <- full_join(myconso_elec_batu,myconso_gaz_batu,by=c("num_bat","Unite"))
  myconso_parbatunite <- full_join(myconso_parbatunite,myconso_eau_batu,by=c("num_bat","Unite"))
  myconso_parbatunite <- full_join(myconso_parbatunite,myconso_fod_batu,by=c("num_bat","Unite"))
  myconso_parbatunite <- full_join(myconso_parbatunite,myconso_reseau_batu,by=c("num_bat","Unite"))
  # suppress ligne sans unité dans batiment
  myconso_parbatunite<-myconso_parbatunite[!is.na(myconso_parbatunite$Unite),]
  
  # Redistribution des consommations des lieux communs
  # et creation d'une clé batu == bat+unite
  myconso_parbatunite<-as.data.frame(myconso_parbatunite)
  myconso_parbatunite<-myconso_parbatunite %>%
    mutate(countU=str_count(myconso_parbatunite$Unite,"/") + 1,
           batu=paste(num_bat,Unite,sep="-"))
  
  vecBat<-unique(myconso_parbatunite$num_bat)
  vecUnite<-unique(myconso_parbatunite$Unite)
  vecUnite<-vecUnite[-grep(pattern="/",vecUnite)]
  vecBatu<-unique(myconso_parbatunite$batu)
  vecBatu<-vecBatu[-grep(pattern="/",vecBatu)]
  
  conso_elec_m2_kWhb<-vector("numeric",length(vecBatu))
  conso_gaz_m2_kWhb<-vector("numeric",length(vecBatu))
  conso_eau_m2b<-vector("numeric",length(vecBatu))
  conso_fod_m2_kWhb<-vector("numeric",length(vecBatu))
  conso_reseau_m2b<-vector("numeric",length(vecBatu))
  
  for (k in seq_along(vecBat)){
    for (i in seq_along(vecUnite)){
      for (j in 1:nrow(myconso_parbatunite)){
        if (length(grep(pattern=vecUnite[i],myconso_parbatunite[j,"batu"])) != 0 &
            length(grep(pattern=vecBat[k],myconso_parbatunite[j,"batu"])) != 0){
          key=paste(vecBat[k],vecUnite[i],sep="-")
          for (k2 in seq_along(vecBatu)){
            if (key == vecBatu[k2]){
              conso_elec_m2_kWhb[k2]<-conso_elec_m2_kWhb[k2] +
                myconso_parbatunite[j,"conso_elec_m2_kWh"] / myconso_parbatunite[j,"countU"]
              conso_gaz_m2_kWhb[k2]<-conso_gaz_m2_kWhb[k2] +
                myconso_parbatunite[j,"conso_gaz_m2_kWh"] / myconso_parbatunite[j,"countU"]
              conso_eau_m2b[k2]<-conso_eau_m2b[k2] +
                myconso_parbatunite[j,"conso_eau_m2"] / myconso_parbatunite[j,"countU"]
              conso_fod_m2_kWhb[k2]<-conso_fod_m2_kWhb[k2] +
                myconso_parbatunite[j,"conso_fod_m2_kWh"] / myconso_parbatunite[j,"countU"]
              conso_reseau_m2b[k2]<-conso_reseau_m2b[k2] +
                myconso_parbatunite[j,"conso_reseau_m2"] / myconso_parbatunite[j,"countU"]
            }
          }
        }
      }
    }
  }
  
  myconso_parbatunite2<-cbind.data.frame(vecBatu,conso_elec_m2_kWhb,
                                         conso_gaz_m2_kWhb,conso_eau_m2b,
                                         conso_fod_m2_kWhb,conso_reseau_m2b)
  
  # somme du cout de toutes les énergies par batiment et unite
  myconso_parbatunite <- myconso_parbatunite %>%
    mutate(cout_global_batunite=rowSums(across(starts_with("cout")),na.rm=TRUE))
  
  # Exports des fichiers avec les jointures
  return(
    list(
      myconso_elec2   = myconso_elec2,
      myconso_gaz2    = myconso_gaz2,
      myconso_eau2    = myconso_eau2,
      myconso_fod2    = myconso_fod2,
      myconso_reseau2 = myconso_reseau2,
      
      myconso_parbat           = myconso_parbat,
      myconso_parunite         = myconso_parunite,
      myconso_parbatunite      = myconso_parbatunite
      )
  )
}