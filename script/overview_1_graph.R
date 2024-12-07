overview_1_graph <- function(overview_1){
  
df <- overview_1
  
grouping_columns <- names(df)[1:which(names(df) == "paid") - 1]

df <- df %>%
  unite("label", all_of(grouping_columns), sep = "-", remove = FALSE)

df_long <- df %>%
  pivot_longer(
    cols = c(settled_claims, open_claims, settled_numbers,open_numbers),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(metric = factor(metric, levels = c("settled_claims", "open_claims", "settled_numbers", "open_numbers")),
         titles = if_else(metric %in% c("settled_claims", "open_claims"),"Reported Amounts","Reported Counts"))


graph <- ggplot(df_long, aes(metric,value, fill = label)) +
  geom_col(position="stack") +
  facet_wrap(~ titles, scales = "free", ncol = 2) +
  scale_fill_viridis_d() +
  labs(x = NULL, y = NULL, fill = "Groupings") +
  theme_minimal() +
  theme(
    #axis.text = element_blank(),  # Remove axis text (the numbers)
    #axis.ticks = element_blank(),  # Remove axis tick marks
    legend.position = "none",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14,face = "bold"),
    strip.text = element_text(size = 12, face = "bold")
  )


#graph <- ggplot(df_long, aes(x = "", y = value, fill = label)) +
#  geom_bar(stat = "identity", width = 1) +
#  coord_polar(theta = "y") +
#  facet_wrap(~ metric, scales = "free", ncol = 3) +
#  scale_fill_viridis_d() +
#  labs(fill = "Groupings") +
#  theme_void() +
#  theme(
#    axis.text = element_blank(),  # Remove axis text (the numbers)
#    axis.ticks = element_blank(),  # Remove axis tick marks
#    legend.position = "bottom",
#    legend.title = element_text(size = 12),
#    legend.text = element_text(size = 10),
#    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
#    strip.text = element_text(size = 12)
#  )

return(graph)
}