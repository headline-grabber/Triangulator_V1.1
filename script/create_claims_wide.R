create_claims_wide <- function(user_claims_database,user_claim_vars){
  
  user_claims_database() %>%
    rename(
      policy_id = !!user_claim_vars$user_policy_id,
      claim_id = !!user_claim_vars$user_claim_id,
      date_incident = !!user_claim_vars$user_date_incident,
      date_stamp = !!user_claim_vars$user_date_stamp,
      date_issue = !!user_claim_vars$user_date_issue,
      date_reported = !!user_claim_vars$user_date_reported,
      paid = !!user_claim_vars$user_paid,
      case = !!user_claim_vars$user_case) %>%
    arrange(claim_id,date_stamp) %>%
    mutate(PTOT = paid,
           ITOT = paid + case,
           NTOT = 1,
           STOT = if_else(case == 0,1,0))
}