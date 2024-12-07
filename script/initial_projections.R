
initial_projections <- function(time,user_claim_vars,triangles_,initial_reserves_,num_plots_,all_plots_){
  # ---------------------------------------------------------
  user_groupings <- user_claim_vars$user_groupings 
  assign(paste0("reporting_",time),triangles_[[paste0("reporting_", time)]]())
  assign(paste0("incident_",time),triangles_[[paste0("incident_", time)]]())
  assign(paste0("underwriting_",time),triangles_[[paste0("underwriting_", time)]]())
  
  
  diagonals <- function(triangle){
    triangle %>%
      group_by(!!!syms(user_groupings), variable,origin) %>%
      filter(development == max(development)) %>%
      ungroup() %>%
      select(-development) %>%
      rename(diagonal = "value")
  }
  
  columns <- function(triangle){
    triangle %>%
      group_by(!!!syms(user_groupings), variable,development) %>%
      summarise(
        origin = max(origin),
        column = sum(value)) %>%
      ungroup() %>%
      select(-development)
  }
  
  ultimates <- function(triangle){
    diagonals(triangle) %>%
      left_join(columns(triangle), by = c(user_groupings,"variable","origin")) %>%
      mutate(column_minus_diagonal = column - diagonal) %>%
      group_by(!!!syms(user_groupings), variable) %>%
      mutate(next_column = lag(column)) %>%
      ungroup() %>%
      mutate(link_ratio =  if_else(is.na(next_column / column_minus_diagonal), 1, next_column / column_minus_diagonal)) %>%
      group_by(!!!syms(user_groupings), variable) %>%
      mutate(link_to_ult = cumprod(if_else(origin <= origin, link_ratio, 1))) %>%
      ungroup() %>%
      mutate(ultimate = link_to_ult*diagonal,
             reserve = ultimate - diagonal) 
    
  }
  
  reserve_maker <- function(time){
    
    data_0 <- get(paste0("reporting_",time)) %>% 
      diagonals() %>%
      group_by(!!!syms(user_groupings),variable) %>%
      summarise(PAID = sum(diagonal)) %>%
      ungroup() %>%
      filter(variable == "PTOT") %>%
      select(-variable)
    
    data_1 <- get(paste0("reporting_",time)) %>% 
      diagonals() %>%
      group_by(!!!syms(user_groupings),variable) %>%
      summarise(INCURRED = sum(diagonal)) %>%
      ungroup()
    
    data_2 <- get(paste0("reporting_",time)) %>% 
      ultimates() %>%
      group_by(!!!syms(user_groupings),variable) %>%
      summarise(reporting_ultimate = sum(ultimate)) %>%
      ungroup()
    
    data_3 <- get(paste0("incident_",time)) %>% 
      ultimates() %>%
      group_by(!!!syms(user_groupings),variable) %>%
      summarise(incident_ultimate = sum(ultimate)) %>%
      ungroup()
    
    data_4 <- get(paste0("underwriting_",time)) %>% 
      ultimates() %>%
      group_by(!!!syms(user_groupings),variable) %>%
      summarise(underwriting_ultimate = sum(ultimate)) %>%
      ungroup()
    
    data_all <- data_1 %>%
      left_join(data_2, by = c(user_groupings,"variable")) %>%
      left_join(data_3, by = c(user_groupings,"variable")) %>%
      left_join(data_4, by = c(user_groupings,"variable")) %>%
      filter(variable == "ITOT") %>%
      left_join(data_0, by = user_groupings) %>%
      mutate(CASE = INCURRED - PAID,
             IBNER = reporting_ultimate - INCURRED,
             IBNR = incident_ultimate - reporting_ultimate,
             URR = underwriting_ultimate - incident_ultimate) %>%
      select(-variable,-INCURRED,-reporting_ultimate,-incident_ultimate,-underwriting_ultimate) %>%
      pivot_longer(
        cols = c(PAID, CASE, IBNER, IBNR, URR),
        names_to = "metric",
        values_to = "value"
      ) %>%
      mutate(metric = factor(metric, levels = rev(c("PAID" ,"CASE", "IBNER", "IBNR", "URR"))))
    
    return(data_all)
  }
  
  

  #-----------------------------------------------
  
  
  user_groupings_distinct <- get(paste0("reporting_",time)) %>% 
    select(!!!syms(user_groupings)) %>%
    distinct()
  
  num_plots_(nrow(user_groupings_distinct))
  
  plot_list <- list()
  
  for (i in 1:nrow(user_groupings_distinct)) {
    filter_conditions <- map2(user_groupings,user_groupings_distinct[i, ],~ expr(!!sym(.x) == !!.y))
    combined_filter <- reduce(filter_conditions, ~ quo(!!..1 & !!..2))
    selected_title <- paste(user_groupings_distinct[i, ], collapse = "_")
    # --- calculate reserve and make waterfall
    plot_temp <- reserve_maker(time) %>%
      filter(!!!combined_filter) %>%
      select(value, metric) %>%
      waterfall(calc_total = TRUE) +
      theme_minimal() +
      labs(title = selected_title,
           x = NULL,
           y = NULL) +
      theme(plot.title = element_text(size = 14,face = "bold",hjust = 0.5))
    # --- make data labels nicer
    plot_data <- ggplot_build(plot_temp)
    for (layer in c(12, 13, 14, 15, 16, 18)) {
      labels <- plot_data$data[[layer]]$label
      labels <- gsub("−", "-", labels)
      numeric_labels <- suppressWarnings(as.numeric(labels))
      plot_data$data[[layer]]$label <- comma(numeric_labels, accuracy = 1)
      # --- rebuild graph
      plot <- ggplot() +
        annotation_custom(grob = ggplot_gtable(plot_data)) +
        theme_minimal()}
    # --- assign graph
    plot_list[[selected_title]] <- plot}
  
  #final_plot <- wrap_plots(plot_list, ncol = 1) # Adjust ncol for layout
  
  all_plots_(plot_list)
  
  initial_reserves_(reserve_maker(time) %>%
                      pivot_wider(
                        names_from = all_of(user_groupings),
                        values_from = value
                      ) %>%
                      relocate(metric, .before = everything()))
  #return(final_plot)
}