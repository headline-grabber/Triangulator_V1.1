overview_3_graph <- function(overview_3){
  
  overview_3_long <- overview_3 %>%
    mutate(origin = as.character(origin), origin_index = row_number()) %>%
    pivot_longer(cols = -c(origin, origin_index), names_to = "grouping", values_to = "value")
  
  total_rows <- nrow(overview_3)
  
  label_percentage <- 40
  
  if (label_percentage == 100) {breaks <- 1:total_rows} else {
    num_breaks <- ceiling(total_rows * (label_percentage / 100))
    breaks <- unique(c(1, seq(1, total_rows, length.out = num_breaks), total_rows))
  }
  
  p <- ggplot(overview_3_long, aes(x = origin_index, y = value, fill = grouping)) +
    geom_area() +
    scale_x_continuous(breaks = breaks, labels = overview_3$origin[breaks]) +
    scale_fill_viridis_d() +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom")
  
  final_graph <- ggplotly(p) %>% 
    layout(
      legend = list(
        orientation = "h",  # Horizontal legend
        x = 0.5,            # Center the legend horizontally
        xanchor = "center", # Anchor the legend in the center
        y = -0.3            # Move the legend further below the plot
      )
      #,margin = list(b = 100)
    )
    
    
  return(final_graph)
}
