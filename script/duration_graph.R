duration_graph <- function(duration_data,user_groupings,time){
  ggplot_list <- lapply(unique(duration_data$variable), function(var) {
    p <- duration_data %>%
      filter(variable == var) %>%
      unite("grouping", all_of(user_groupings), sep = " - ") %>%
      ggplot(aes(x = grouping, y = duration, fill = grouping)) +
      geom_bar(stat = "identity") +
      labs(title = var, x = NULL, y = NULL) +
      scale_y_continuous(limits = range(c(duration_data$duration,0), na.rm = TRUE)) +
      scale_fill_viridis_d() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) 
    
    if(var == "ITOT"){p <- p + labs(y = paste0("Duration (Years)"))} # (",time,")
    p
  })
  
  plot_combined <- wrap_plots(ggplot_list, ncol = 4)
  return(plot_combined)
}