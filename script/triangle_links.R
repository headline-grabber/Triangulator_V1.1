triangle_links <- function(triangles,
                                user_groupings, # a character vector of column names in the triangles dataframe
                                variable_1) {  # this will be the variable you make your link ratios off
  
  result <- triangles %>%
    filter(variable %in% c(variable_1)) %>%
    mutate(variable = paste0(variable_1,"_links")) %>%
    group_by(!!!syms(user_groupings), variable,origin) %>%
    mutate(next_value = lead(value)) %>%
    ungroup() %>%
    mutate(link_ratio =  if_else(is.na(next_value / value), NA, next_value / value)) %>%
    mutate(link_ratio = if_else(is.nan(link_ratio) | is.infinite(link_ratio), NA, link_ratio)) %>%
    select(-value,-next_value) %>%
    rename(value = "link_ratio") %>%
    select(!!!syms(user_groupings),variable, origin, development,value)
  
  
  return(result)
}