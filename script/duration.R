duration <- function(triangles_,user_groupings,basis,time) {  
  
  triangle <- triangles_[[paste0(basis,"_",time)]]()
  
  duration_data <- triangle %>%
    filter(variable %in% c("ITOT","PTOT","NTOT","STOT")) %>%
    left_join(triangle %>%
                distinct(origin) %>%
                arrange(rev(origin)) %>%
                mutate(index = row_number()), by = "origin") %>%
    mutate(calendar = index - development + 1) %>%
    group_by(!!!syms(user_groupings), variable,calendar) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    mutate(calendar_mid = calendar - 0.5,
           value_times_calendar_mid = value*calendar_mid) %>%
    group_by(!!!syms(user_groupings), variable) %>%
    summarise(duration = sum(value_times_calendar_mid)/sum(value)) %>%
    ungroup() %>%
    mutate(duration = duration/if_else(time == "monthly",12,if_else(time == "quarterly",4,1)))
  
    return(duration_data)
  }