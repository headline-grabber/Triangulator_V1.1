triangulations_1_graph_basic <- function(data){

claims_data3 <- data %>%
  mutate(origin = factor(origin, levels = sort(unique(origin), decreasing = TRUE)),
         hover_text = paste0("Development: ", development, "<br>Origin: ", origin, "<br>Value: ", scales::comma(value)))

plot_1 <- ggplot(claims_data3, aes(x = development, y = origin, fill = value, text = hover_text)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "D", na.value = "grey50") +
  labs(x = "Development", y = "Origin", fill = "Value") +
  theme_minimal()

return(ggplotly(plot_1, tooltip = "text"))
}