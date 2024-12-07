movement_filter <- function(movement_,input,user_claim_vars,displayed_triangle_selections_){

movement_() %>% 
  filter(origin == input$filter_origin, development == input$filter_development) %>% 
  filter_at(vars(user_claim_vars$user_groupings), all_vars(. %in% unlist(displayed_triangle_selections_()))) %>%
  arrange(desc(ITOT_movement)) %>%
  select(policy_id,claim_id,PTOT_movement,ITOT_movement,STOT_movement,NTOT_movement)
}