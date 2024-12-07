overview_2_data <- function(current_positions_, user_claim_vars, user_bucket_size) {
  # Generate all possible buckets based on ITOT max
  max_ITOT <- max(current_positions_()$ITOT, na.rm = TRUE)
  bucket_labels <- paste(
    seq(0, max_ITOT, by = user_bucket_size),
    seq(user_bucket_size, max_ITOT + user_bucket_size, by = user_bucket_size),
    sep = " - "
  )
  all_buckets <- tibble(incurred_bucket = factor(bucket_labels, levels = bucket_labels))
  
  # Process the current positions
  overview_2 <- current_positions_() %>%
    mutate(incurred_bucket = factor(
      cut(ITOT,
          breaks = seq(0, max_ITOT + user_bucket_size, by = user_bucket_size),
          labels = bucket_labels,
          include.lowest = TRUE),
      levels = bucket_labels
    )) %>%
    select(!!!syms(user_claim_vars$user_groupings), ITOT, NTOT, incurred_bucket) %>%
    group_by(incurred_bucket, !!!syms(user_claim_vars$user_groupings)) %>%
    summarise(NTOT = sum(NTOT, na.rm = TRUE), .groups = "drop") %>%
    ungroup()
  
  # Ensure all buckets are present
  complete_overview <- all_buckets %>%
    left_join(overview_2, by = "incurred_bucket") %>%
    replace_na(list(NTOT = 0)) # Replace missing claims with 0
  
  # Pivot to wide format
  result <- complete_overview %>%
    #filter(across(all_of(user_claim_vars$user_groupings), ~!is.na(.))) %>%
    tidyr::pivot_wider(names_from = user_claim_vars$user_groupings, values_from = NTOT) %>%
    mutate_all(~replace(., is.na(.), 0)) #%>% # Ensure no NA values remain
    
  if ("NA_NA" %in% colnames(result)) {result <- result %>% select(-`NA_NA`)}
  
  return(result)
}
