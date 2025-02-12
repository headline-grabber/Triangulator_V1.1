initial_data_checks <- function(claims_wide_) {
 
  data <- claims_wide_()
  
  rules <- validator(
    policyid_not_na = !is.na(policy_id),
    claimid_not_na = !is.na(claim_id),
    date_incident_not_na = !is.na(date_incident),
    date_stamp_not_na = !is.na(date_stamp),
    date_issue_not_na = !is.na(date_issue),
    date_issue_valid = date_issue <= date_stamp,
    date_reported_not_na = !is.na(date_reported),
    date_reported_after_incident = date_reported >= date_incident,
    paid_not_na = !is.na(paid),
    paid_positive = paid >= 0,
    case_not_na = !is.na(case),
    case_non_negative = case >= 0
  )
  results <- confront(data, rules)
  #summary(results) 
  #plot(results)
  return(results)
  
}