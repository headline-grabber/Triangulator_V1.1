create_triangles <- function(user_claims_database, user_claim_vars,
                             basis_ = "underwriting",                           #c("underwriting","incident","reporting")
                             time_ = "monthly",                                 #c("yearly","quarterly","monthly")
                             user_large_loss_definition = 999999999,
                             movement_,
                             update_movement_ = "no") {                               
  #basis_ = "incident"
  #time_ = "yearly"
  #user_large_loss_definition <- 20000
  
  claims_database <- user_claims_database()  
  user_policy_id <- user_claim_vars$user_policy_id  
  user_claim_id <- user_claim_vars$user_claim_id 
  user_groupings <- user_claim_vars$user_groupings  
  user_date_incident <- user_claim_vars$user_date_incident  
  user_date_stamp <- user_claim_vars$user_date_stamp
  user_date_issue <- user_claim_vars$user_date_issue
  user_date_reported <- user_claim_vars$user_date_reported
  user_paid <- user_claim_vars$user_paid
  user_case <- user_claim_vars$user_case  
  
  
  index_month <- seq(as.Date("2000-01-01"), as.Date("2099-12-01"), by = "month") %>% format("%Y%m") %>% data.frame(yyyymm = .) %>% rename(origin_month = "yyyymm") %>% mutate(index_month = row_number(), origin_month = as.integer(origin_month))
  index_quarter <- index_month %>% filter(index_month %% 3 == 0) %>% select(-index_month) %>% rename(origin_quarter = origin_month) %>% mutate(index_quarter = row_number())
  index_year <- index_month %>% filter(index_month %% 12 == 0) %>% select(-index_month) %>% rename(origin_year = origin_month) %>% mutate(index_year = row_number())
  
  
# .................SELECTION 1 - BASIS
#  "basis_select_1" 
#c("underwriting","incident","reporting")
#basis_ <- "underwriting"
if(basis_ == "underwriting"){basis <- "date_issue"
} else if(basis_ == "incident"){basis <- "date_incident"
} else if(basis_ == "reporting"){basis <- "date_reported"}
  


  claims_database_2 <- claims_database %>%                    
    # 0. rename standard columns
    rename(
      policy_id = !!user_policy_id,
      claim_id = !!user_claim_id,
      date_incident = !!user_date_incident,
      date_stamp = !!user_date_stamp,
      date_issue = !!user_date_issue,
      date_reported = !!user_date_reported,
      paid = !!user_paid,
      case = !!user_case) %>%
    # 1. arrange database
    arrange(claim_id,date_stamp) %>%    # data checks should have already checked if there are duplicates
    # 2. value definitions
    mutate(PTOT = paid,
           ITOT = paid + case,
           NTOT = 1,
           STOT = if_else(case == 0,1,0)) %>%
    # 3. format origin dates
    mutate(origin_month = as.integer(format(!!sym(basis), "%Y%m")),
           origin_quarter = as.integer(format(!!sym(basis), "%Y")) * 100 + 
             ((as.integer(format(!!sym(basis), "%m")) - 1) %/% 3 + 1) * 3,
           origin_year = as.integer(format(!!sym(basis), "%Y")) * 100 + 12) %>%
    # 4. format stamp dates
    mutate(stamp_month = as.integer(format(date_stamp, "%Y%m")),
           stamp_quarter = as.integer(format(date_stamp, "%Y")) * 100 + 
             ((as.integer(format(date_stamp, "%m")) - 1) %/% 3 + 1) * 3,
           stamp_year = as.integer(format(date_stamp, "%Y")) * 100 + 12) %>%
    # 5. Development From
    left_join(index_month,by = c("origin_month" = "origin_month")) %>%
    left_join(index_month,by = c("stamp_month" = "origin_month")) %>%
    mutate(development_month = index_month.y - index_month.x +1) %>%
    # --
    left_join(index_quarter,by = c("origin_quarter" = "origin_quarter")) %>%
    left_join(index_quarter,by = c("stamp_quarter" = "origin_quarter")) %>%
    mutate(development_quarter = index_quarter.y - index_quarter.x +1) %>%
    # --
    left_join(index_year,by = c("origin_year" = "origin_year")) %>%
    left_join(index_year,by = c("stamp_year" = "origin_year")) %>%
    mutate(development_year = index_year.y - index_year.x +1) #%>%
  
  # 6. Development To
  db_month <- claims_database_2 %>% group_by(claim_id, stamp_month) %>% filter(row_number() == n()) %>% ungroup() %>% group_by(claim_id) %>% mutate(development_month_to = coalesce(lead(development_month)-1, max(claims_database_2$index_month.x) - index_month.x +1)) %>% ungroup()
  db_quarter <- claims_database_2 %>% group_by(claim_id, stamp_quarter) %>% filter(row_number() == n()) %>% ungroup() %>% group_by(claim_id) %>% mutate(development_quarter_to = coalesce(lead(development_quarter)-1, max(claims_database_2$index_quarter.x) - index_quarter.x +1)) %>% ungroup()
  db_year <- claims_database_2 %>% group_by(claim_id, stamp_year) %>% filter(row_number() == n()) %>% ungroup() %>% group_by(claim_id) %>% mutate(development_year_to = coalesce(lead(development_year)-1, max(claims_database_2$index_year.x) - index_year.x +1)) %>% ungroup()
  
  
  
  # .................SELECTION 2 - TIME
  #  "time_select_1" 
  #c("yearly","quarterly","monthly")
  #time_ <- "monthly"
  if(time_ == "monthly"){
            origin  <- "origin_month"
            origin_index <- "index_month.x"
            development <- "development_month"
            development_to <- "development_month_to"
            db <- db_month 
            index <- index_month
  } else if(time_ == "quarterly"){
            origin  <- "origin_quarter"
            origin_index <- "index_quarter.x"
            development <- "development_quarter"
            development_to <- "development_quarter_to"
            db <- db_quarter 
            index <- index_quarter
  } else if(time_ == "yearly"){
            origin  <- "origin_year"
            origin_index <- "index_year.x"
            development <- "development_year"
            development_to <- "development_year_to"
            db <- db_year 
            index <- index_year}

  
  db <- db %>%
    rename(
      origin = !!sym(origin),
      origin_index = !!sym(origin_index),
      development = !!sym(development),
      development_to = !!sym(development_to)) 
  colnames(index) <- c("origin","index")
  db_long <- db %>% tidyr::pivot_longer(cols = c("PTOT", "ITOT", "NTOT", "STOT"), names_to = "variable", values_to = "value") 
  
 
  
  # Create Blank Triangles (to ensure no "gaps" in the created triangle)
  start_origin_index <- min(db$origin_index, na.rm = TRUE)
  number_of_origins <- max(db$origin_index, na.rm = TRUE) - start_origin_index + 1
  index <- index %>%
    filter(index >= start_origin_index & index <= (start_origin_index + number_of_origins - 1)) %>%
    mutate(index = index - min(index) + 1)
  blank_triangle <- expand.grid(origin = index$origin,development = 1:number_of_origins) %>%
    filter(development + (index$index[match(origin, index$origin)] - 1) <= number_of_origins) %>%
    mutate(value = 0)
  blank_triangles <- db_long %>%
    distinct(!!!syms(user_groupings), variable) %>%
    crossing(blank_triangle)
  
  
  
  # .................SELECTION 3 - Large Loss Threshold
  #  "user_large_loss_definition" 
  large_ids <- db %>% filter(ITOT > user_large_loss_definition) %>% select(claim_id) %>% distinct() %>% pull()
  
  
  # Calculate Triangles
  output <- as.data.frame(
    rbindlist( 
      lapply(1:nrow(index), function(x) {
        as.data.table(db_long %>% filter(!claim_id %in% large_ids))[
          development <= x & development_to >= x, 
          .(development = x, 
            value = sum(value)), 
          keyby = c(user_groupings, "variable", "origin")  # Use `user_groupings` directly here
        ]}))) %>%
    bind_rows(blank_triangles) %>%
    group_by(across(c(all_of(user_groupings), "variable", "origin", "development"))) %>%  # Use `all_of()` for dynamic column selection
    summarise(value = sum(value), .groups = "drop")
  
  
  calc_1 <- triangle_calculator(output,user_groupings,"ITOT","NTOT","divided_by")
  calc_2 <- triangle_calculator(output,user_groupings,"PTOT","STOT","divided_by")
  calc_3 <- triangle_calculator(output,user_groupings,"ITOT","PTOT","minus")
  calc_4 <- triangle_calculator(output,user_groupings,"NTOT","STOT","minus")
  calc_5 <- triangle_calculator(output,user_groupings,"PTOT","ITOT","divided_by")
  calc_6 <- triangle_calculator(output,user_groupings,"STOT","NTOT","divided_by")
  link_1 <- triangle_links(output,user_groupings,"ITOT")
  link_2 <- triangle_links(output,user_groupings,"PTOT")
  link_3 <- triangle_links(output,user_groupings,"NTOT")
  link_4 <- triangle_links(output,user_groupings,"STOT")
  
  final_result <- bind_rows(output, calc_1, calc_2, calc_3, calc_4, calc_5, calc_6,link_1,link_2,link_3,link_4)
  
  
  
  
  # --- update movements for Triangulations Tab
  if(update_movement_ == "yes"){
  db_movement <- db %>% 
    filter(!claim_id %in% large_ids) %>%
    group_by(claim_id) %>%
    mutate(PTOT_prev = lag(PTOT),
           ITOT_prev = lag(ITOT),
           NTOT_prev = lag(NTOT),
           STOT_prev = lag(STOT)) %>%
    ungroup() %>%
    mutate(across(ends_with("prev"), ~ replace_na(., 0))) %>%
    mutate(PTOT_movement = PTOT - PTOT_prev,
           ITOT_movement = ITOT - ITOT_prev,
           NTOT_movement = NTOT - NTOT_prev,
           STOT_movement = STOT - STOT_prev) %>%
    select(-PTOT_prev,-ITOT_prev,-STOT_prev,-NTOT_prev) 
  movement_(db_movement)}
  
  
  return(final_result)
}