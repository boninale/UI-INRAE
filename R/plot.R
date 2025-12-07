library(ggplot2)

# Function to plot consumption by building
generate_building_plot <- function(df, resource_col, unit) {
  df_building <- df |>
    dplyr::group_by(num_bat) |>
    dplyr::summarise(total_conso = sum(.data[[resource_col]], na.rm = TRUE), .groups = "drop")

  ggplot(df_building, aes(x = factor(num_bat), y = total_conso)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(x = "Bâtiment", y = "Consommation", title = paste0("Consommation par bâtiment (", unit, ")")) +
    theme_minimal()
}

# Function to plot consumption by unit
generate_unit_plot <- function(df, resource_col, unit) {
  df_unit <- df |>
    dplyr::group_by(Unite) |>
    dplyr::summarise(total_conso = sum(.data[[resource_col]], na.rm = TRUE), .groups = "drop")

  ggplot(df_unit, aes(x = factor(Unite), y = total_conso)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(x = "Unité", y = "Consommation", title = paste0("Consommation par unité (", unit, ")")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Function to plot total consumption over years
generate_yearly_plot <- function(df, year_cols, unit) {
  # Ensure all year columns are of numeric type
  df[year_cols] <- lapply(df[year_cols], function(col) {
    suppressWarnings(as.numeric(col))
  })

  df_long <- tidyr::pivot_longer(
    df,
    cols = all_of(year_cols),
    names_to = "year",
    values_to = "conso"
  )

  df_long$year <- sub("_conso$", "", df_long$year)
  df_year <- df_long |>
    dplyr::group_by(year) |>
    dplyr::summarise(total = sum(conso, na.rm = TRUE), .groups = "drop")

  df_year$year <- as.integer(as.character(df_year$year))

  ggplot(df_year, aes(x = year, y = total)) +
    geom_line(linewidth = 1, colour = "steelblue") +
    geom_point(size = 2, colour = "steelblue") +
    labs(x = "Année", y = "Consommation", title = paste0("Consommation totale (", unit, ")")) +
    theme_minimal()
}
