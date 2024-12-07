triangle_calculator <- function(triangles,
                                user_groupings, # a character vector of column names in the triangles dataframe
                                variable_1, # i.e. a choice of the variables that are in the triangles already
                                variable_2, # i.e. a choice of the variables that are in the triangles already
                                operation) {  # should have "plus", "minus", "times", "divided_by"
  
  # Perform the requested operation dynamically
  triangles <- triangles %>%
    filter(variable %in% c(variable_1, variable_2)) %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    mutate(result_value = case_when(
      operation == "plus" ~ !!sym(variable_1) + !!sym(variable_2),
      operation == "minus" ~ !!sym(variable_1) - !!sym(variable_2),
      operation == "times" ~ !!sym(variable_1) * !!sym(variable_2),
      operation == "divided_by" ~ !!sym(variable_1) / !!sym(variable_2),
      TRUE ~ NA_real_
    )) %>%
    mutate(result_value = if_else(is.nan(result_value) | is.infinite(result_value), NA_real_, result_value)) %>%
    pivot_longer(cols = c(!!sym(variable_1), !!sym(variable_2), result_value), 
                 names_to = "variable", 
                 values_to = "value") %>%
    filter(variable == "result_value") %>%
    mutate(variable = paste(variable_1, operation, variable_2, sep = "_"))
  
  # Re-group and return the final result
  result <- triangles %>%
    group_by(!!!syms(user_groupings), origin, development) %>%
    ungroup() %>%
    select(!!!syms(user_groupings),variable, origin, development,value)
  
  return(result)
}