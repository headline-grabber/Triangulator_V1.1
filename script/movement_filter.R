movement_filter <- function(movement_,input,user_claim_vars,displayed_triangle_selections_){

movement_() %>% 
  filter(origin == input$filter_origin, development == input$filter_development) %>% 
  filter_at(vars(user_claim_vars$user_groupings), all_vars(. %in% unlist(displayed_triangle_selections_()))) %>%
  arrange(desc(abs(ITOT_movement))) %>%
  mutate(  ITOT_movement_proportion = ITOT_movement / na_if(sum(ITOT_movement, na.rm = TRUE), 0),
           ITOT_movement_percent = ITOT_movement / na_if((ITOT - ITOT_movement), 0)) %>%
  select(policy_id,claim_id,ITOT_movement_proportion,ITOT_movement_percent,ITOT_movement,PTOT_movement,STOT_movement,NTOT_movement)
}