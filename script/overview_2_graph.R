overview_2_graph <- function(overview_2){
  

long_data <- overview_2 %>% pivot_longer(-incurred_bucket, names_to = "grouping", values_to = "claims")

stacked_plot <- ggplot(long_data, aes(x = incurred_bucket, y = claims, fill = grouping)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  scale_fill_viridis_d() +
  labs(x = NULL,y = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

final_graph <- ggplotly(stacked_plot) %>% 
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