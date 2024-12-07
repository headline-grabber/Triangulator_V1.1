overview_1_graph_2 <- function(overview_1){
  
  df <- overview_1
  
  grouping_columns <- names(df)[1:which(names(df) == "paid") - 1]
  
  df <- df %>%
    unite("label", all_of(grouping_columns), sep = "-", remove = FALSE)
  
  df_long <- df %>%
    pivot_longer(
      cols = c(average_settled_claim, average_open_claim, average_claim, sd_settled_claims,sd_open_claims,sd_all_claims),
      names_to = "metric",
      values_to = "value"
    ) %>%
    mutate(metric = factor(metric, levels = c("average_settled_claim", "average_open_claim", "average_claim", "sd_settled_claims", "sd_open_claims", "sd_all_claims")),
           titles = if_else(metric %in% c("average_settled_claim", "average_open_claim", "average_claim"),"averages","standard deviations"))
  
  
  graph <- ggplot(df_long, aes(x = label, y = value, fill = label)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +  # Avoid stacking with position = "dodge"
    facet_wrap(~ metric, ncol = 3, scales = "fixed") +               # Fix facets for `metric`
    scale_fill_viridis_d() +                                        # Optional for color
    labs(
      x = NULL,
      y = NULL,
      fill = "Groupings"
    ) +
    theme_minimal() +                                               # Cleaner theme
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),            # Rotate x-axis labels
      legend.position = "none",                                   # Move legend to bottom
      strip.text = element_text(size = 12)           # Bold facet labels face = "bold"
    )
  
  
  return(graph)
}