triangulations_1_graph <- function(data){
  
  claims_data3 <- data %>%
    mutate(origin = factor(origin, levels = sort(unique(origin), decreasing = TRUE)),
           hover_text = paste0("Development: ", development, "<br>Origin: ", origin, "<br>Value: ", scales::comma(value))) %>%
    group_by(origin) %>%
    mutate(link_ratios = lead(value)/value) %>%
    ungroup() %>%
    group_by(development) %>%
    mutate(z_score = (value - mean(value, na.rm = TRUE)) / sd(value, na.rm = TRUE)) %>%
    ungroup()
  
  # -----------
  
  link_ratio_development_outliers <- claims_data3 %>%  filter(abs(z_score) > 3)
  
  # -----------
  
  #valid_data <- claims_data3 %>% filter(!is.na(link_ratios), !is.infinite(link_ratios), link_ratios > 0)
  
  #glm_link_ratio <- glm(link_ratios ~ development, family = gaussian(link = "log"), data = valid_data)
  
  #glm_link_ratio_residuals <- valid_data %>%
  #  mutate(residuals = residuals(glm_link_ratio)) %>%
  #  filter(abs(residuals) > 3 * sd(residuals, na.rm = TRUE))

  # -----------
  
  plot_1_outliers <- ggplot(claims_data3, aes(x = development, y = origin, fill = value, text = hover_text)) +
    geom_tile(color = "white") +
    geom_point(data = link_ratio_development_outliers, aes(x = development, y = origin), color = "red", size = 3) +
  #  geom_point(data = glm_link_ratio_residuals, aes(x = development, y = origin), color = "white", size = 1) +
    scale_fill_viridis_c(option = "D", na.value = "grey50") +
    labs(x = NULL, y = NULL, fill = "Value") +
    theme_minimal()
  
  return(ggplotly(plot_1_outliers, tooltip = "text"))
  }