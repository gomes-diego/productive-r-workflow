create_scatterplot <- function(data, selected_species, color) {
  # Filter the data for the specified species and island
  filtered_data <- data %>%
    na.omit() %>%
    filter(species == selected_species)
  
  # Create the scatterplot
  plot <- ggplot(
    filtered_data,
    aes(x = bill_length_mm, y = bill_depth_mm)
  ) +
    geom_point(color=color) +
    labs(
      x = "Bill Length (mm)",
      y = "Bill Depth (mm)",
      title = selected_species
    ) +
    theme(legend.position = "none")
  
  return(plot)
}

diego_gomes_theme <- function() {
  theme_ipsum_pub() +
    theme(
      plot.title = element_text(color = "black", size = 18, face = "bold"),
      plot.subtitle = element_text(color = "#181818", size = 14),
      axis.text.x = element_text(size = 12, color = "#5C5C5C"),
      axis.text.y = element_text(size = 12, color = "#858585"),
      axis.title.x = element_text(hjust = 0.5, size = 12, color = "#5C5C5C"), # Center X axis label
      axis.title.y = element_text(hjust = 0.5, size = 12, color = "#5C5C5C"), # Center Y axis label
      panel.grid = element_blank(), # Remove all gridlines
      panel.grid.major.y = element_line(color = "grey80"), # Add major Y axis gridlines
      panel.grid.minor = element_blank(), # Ensure minor gridlines are removed
      panel.grid.major.x = element_blank(), # Ensure major X gridlines are removed
      axis.line.x = element_line(color = "#333333") # Add black line for X-axis
    ) 
}
