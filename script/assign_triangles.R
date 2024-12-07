assign_triangles <- function(triangles_,user_claims_database,user_claim_vars){
  
  setProgress(value = 0 / 9, detail = "Monthly Triangles - Underwriting Basis")
  triangles_$underwriting_monthly(create_triangles(user_claims_database, user_claim_vars,basis_ = "underwriting",time_ = "monthly",user_large_loss_definition = 999999999,movement_,update_movement_ = "no"))
  setProgress(value = 1 / 9, detail = "Quarterly Triangles - Underwriting Basis")
  triangles_$underwriting_quarterly(create_triangles(user_claims_database, user_claim_vars,basis_ = "underwriting",time_ = "quarterly",user_large_loss_definition = 999999999,movement_,update_movement_ = "no"))
  setProgress(value = 2 / 9, detail = "Yearly Triangles - Underwriting Basis")
  triangles_$underwriting_yearly(create_triangles(user_claims_database, user_claim_vars,basis_ = "underwriting",time_ = "yearly",user_large_loss_definition = 999999999,movement_,update_movement_ = "no"))
  setProgress(value = 3 / 9, detail = "Monthly Triangles - Incident Basis")
  triangles_$incident_monthly(create_triangles(user_claims_database, user_claim_vars,basis_ = "incident",time_ = "monthly",user_large_loss_definition = 999999999,movement_,update_movement_ = "no"))
  setProgress(value = 4 / 9, detail = "Quarterly Triangles - Incident Basis")
  triangles_$incident_quarterly(create_triangles(user_claims_database, user_claim_vars,basis_ = "incident",time_ = "quarterly",user_large_loss_definition = 999999999,movement_,update_movement_ = "no"))
  setProgress(value = 5 / 9, detail = "Yearly Triangles - Incident Basis")
  triangles_$incident_yearly(create_triangles(user_claims_database, user_claim_vars,basis_ = "incident",time_ = "yearly",user_large_loss_definition = 999999999,movement_,update_movement_ = "no"))
  setProgress(value = 6 / 9, detail = "Monthly Triangles - Reporting Basis")
  triangles_$reporting_monthly(create_triangles(user_claims_database, user_claim_vars,basis_ = "reporting",time_ = "monthly",user_large_loss_definition = 999999999,movement_,update_movement_ = "no"))
  setProgress(value = 7 / 9, detail = "Quarterly Triangles - Reporting Basis")
  triangles_$reporting_quarterly(create_triangles(user_claims_database, user_claim_vars,basis_ = "reporting",time_ = "quarterly",user_large_loss_definition = 999999999,movement_,update_movement_ = "no"))
  setProgress(value = 8 / 9, detail = "Yearly Triangles - Reporting Basis")
  triangles_$reporting_yearly(create_triangles(user_claims_database, user_claim_vars,basis_ = "reporting",time_ = "yearly",user_large_loss_definition = 999999999,movement_,update_movement_ = "no"))
  
}