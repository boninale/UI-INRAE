standardise_table <- function(df) {
  column_names <- names(df)
  out <- list()
  
  # 1. first column → batiment
  out$batiment <- df[[1]]
  
  # helper: extract all years in a string
  get_years <- function(x) {
    y <- gregexpr("20[0-9]{2}", x, perl = TRUE)
    regmatches(x, y)[[1]]
  }
  
  # 2–4. classify and rename remaining columns
  for (i in seq_along(column_names)[-1]) {
    colname <- column_names[i]
    years <- get_years(colname)
    
    if (length(years) == 1 && grepl("conso", colname, ignore.case = TRUE)) {
      newname <- paste0(years, "_conso")
      out[[newname]] <- df[[i]]
    }
    
    if (length(years) == 2 && !grepl("%", colname)) {
      newname <- paste0("Evol_", years[1], "-", years[2])
      out[[newname]] <- df[[i]]
    }
    
    if (length(years) == 2 && grepl("%", colname)) {
      newname <- paste0("Evol_", years[1], "-", years[2], "_p")
      out[[newname]] <- df[[i]]
    }
  }
  
  # 5. duplicate the latest year_conso column → derniere_conso
  conso_cols <- grep("^[0-9]{4}_conso$", names(out), value = TRUE)
  if (length(conso_cols) > 0) {
    yrs <- as.integer(sub("_conso$", "", conso_cols))
    latest <- conso_cols[which.max(yrs)]
    out$derniere_conso <- out[[latest]]
  }
  
  # return as data frame with ordered names
  out_df <- as.data.frame(out, check.names = FALSE)
  out_df
}
