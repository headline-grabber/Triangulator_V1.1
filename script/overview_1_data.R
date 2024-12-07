overview_1_data <- function(current_positions_, user_claim_vars) {
  # Get the full dataset
  full_data <- current_positions_() %>%
    mutate(
      settled_indicator = if_else(STOT == 1, 1, 0),
      open_indicator = if_else(!is.na(STOT) & STOT == 0, 1, 0),
      settled = settled_indicator * ITOT,
      open = open_indicator * ITOT
    )
  
  # Grouped data for overall summaries
  summary_data <- full_data %>%
    group_by(!!!syms(user_claim_vars$user_groupings)) %>%
    summarise(
      paid = sum(PTOT, na.rm = TRUE),
      incurred = sum(ITOT, na.rm = TRUE),
      paid_over_incurred = paste0(round((paid / incurred) * 100, 1), "%"),
      #---
      number_settled = sum(STOT, na.rm = TRUE),
      number_reported = sum(NTOT, na.rm = TRUE),
      number_settled_over_reported = paste0(round((number_settled / number_reported) * 100, 1), "%"),
      #---
      case_est = incurred - paid,
      number_open = number_reported - number_settled,
      #---
      average_settled_claim = sum(settled, na.rm = TRUE) / sum(settled_indicator, na.rm = TRUE),
      average_open_claim = sum(open, na.rm = TRUE) / sum(open_indicator, na.rm = TRUE),
      average_claim = incurred / number_reported,
      sd_all_claims = sd(ITOT, na.rm = TRUE),
      #---
      settled_claims = sum(settled, na.rm = TRUE),
      open_claims = sum(open, na.rm = TRUE),
      settled_numbers = sum(settled_indicator, na.rm = TRUE),
      open_numbers = sum(open_indicator, na.rm = TRUE)
    )
  
  # Calculate standard deviation for settled claims
  sd_settled <- full_data %>%
    filter(settled_indicator == 1) %>%
    group_by(!!!syms(user_claim_vars$user_groupings)) %>%
    summarise(sd_settled_claims = sd(settled, na.rm = TRUE))

  # Calculate standard deviation for open claims
  sd_open <- full_data %>%
    filter(open_indicator == 1) %>%
    group_by(!!!syms(user_claim_vars$user_groupings)) %>%
    summarise(sd_open_claims = sd(open, na.rm = TRUE))

  # Left join the standard deviations back to the summary data
  summary_data <- summary_data %>%
    left_join(sd_settled, by = user_claim_vars$user_groupings) %>%
    left_join(sd_open, by = user_claim_vars$user_groupings)
  
  #return(full_data)
  return(summary_data)
}
