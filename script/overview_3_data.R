overview_3_data <- function(triangles_,triangle_name,user_claim_vars,variable_select ){
  
triangles_[[triangle_name]]() %>%
    filter(variable == variable_select ) %>%
    group_by(!!!syms(user_claim_vars$user_groupings), origin) %>%
    filter(development == max(development)) %>%
    ungroup() %>%
    tidyr::pivot_wider(names_from = unlist(user_claim_vars$user_groupings), values_from = value) %>%
    mutate(origin = as.character(origin)) %>%
    select(-variable, -development)
  
}
  
  
  