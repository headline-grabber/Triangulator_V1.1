format_dataframe <- function(df) {
  
  # Apply conditional formatting to the table using kableExtra
  df %>%
    mutate(origin = as.character(origin)) %>%
    #mutate(across(everything(), ~ replace(., is.na(.), ""))) %>%
    kable("html", escape = FALSE, col.names = names(df), digits = 0, format.args = list(big.mark = ",")) %>%
    kable_styling(full_width = FALSE, position = "center") %>%
    column_spec(which(sapply(df, is.character)), bold = TRUE, background = "#D3D3D3") %>%
    column_spec(which(names(df) == "origin"), bold = TRUE, background = "#D3D3D3") %>%
    column_spec(which(names(df) != "origin" & sapply(df, is.numeric)), background = "#FFFFFF")
     
}


format_overview_1 <- function(df) {
  num <- ncol(df)-6
  
  df %>%
    kable("html", escape = FALSE, col.names = names(df), digits = 0, format.args = list(big.mark = ",")) %>%
    kable_styling(full_width = FALSE, position = "center") %>%
    #column_spec(which(sapply(df, is.character)), bold = TRUE, background = "#D3D3D3") %>%
    column_spec(1:num, bold = TRUE, background = "#D3D3D3") %>%
    column_spec(which(names(df) %in% c("paid","incurred","percent_paid")), bold = TRUE, background = "#FFFFFF") %>%
    column_spec(which(names(df) %in% c("number_settled","number_reported","percent_settled")), bold = TRUE, background = "#D3D3D3") %>%
    column_spec(which(names(df) %in% c("average_cost_on_settled_claims","average_cost_on_open_claims","average_cost_on_all_claims")), bold = TRUE, background = "#FFFFFF")
}

format_overview_2 <- function(df) {
  df %>%
    kable("html", escape = FALSE, col.names = names(df), digits = 0, format.args = list(big.mark = ",")) %>%
    kable_styling(full_width = FALSE, position = "center") %>%
    column_spec(which(sapply(df, is.character)), bold = TRUE, background = "#D3D3D3") %>%
    column_spec(which(!(names(df) %in% c("incurred_bucket","paid_bucket"))), bold = TRUE, background = "#FFFFFF") 
}


format_overview_3 <- function(df) {
  tryCatch({
    # Validate that 'origin' exists and is of type character
    if (!"origin" %in% names(df)) {
      stop("The column 'origin' is missing in the dataframe.")
    }
    
    df <- df %>% mutate(origin = as.character(origin))
    
    # Apply formatting
    df %>%
      kable("html", escape = FALSE, col.names = names(df), digits = 0, format.args = list(big.mark = ",")) %>%
      kable_styling(full_width = FALSE, position = "center") %>%
      column_spec(which(sapply(df, is.character)), bold = TRUE, background = "#D3D3D3") %>%
      column_spec(which(names(df) != "origin" & sapply(df, is.numeric)), background = "#FFFFFF")
  }, error = function(e) {
    stop("Error in format_overview_3: ", e$message)
  })
}




format_explore_month <- function(df) {
  df %>%
    kable("html", escape = FALSE, col.names = names(df), digits = 0, format.args = list(big.mark = ",")) %>%
    kable_styling(full_width = FALSE, position = "center") %>%
    column_spec(which(sapply(df, is.character)), bold = TRUE, background = "#D3D3D3") %>%
    column_spec(which(!(names(df) %in% c("policy_id","claim_id"))), bold = TRUE, background = "#FFFFFF") 
}


format_initial_reserves <- function(df) {
  df %>%
    kable("html", escape = FALSE, col.names = names(df), digits = 0, format.args = list(big.mark = ",")) %>%
    kable_styling(full_width = FALSE, position = "center") %>%
    column_spec(1, bold = TRUE, background = "#D3D3D3") %>%
    column_spec(2:ncol(df), bold = TRUE, background = "#FFFFFF") 
}

format_duration <- function(df) {
  df %>%
    kable("html", escape = FALSE, col.names = names(df), digits = 2, format.args = list(big.mark = ",", nsmall = 2)) %>%
    kable_styling(full_width = FALSE, position = "center") %>%
    column_spec(1, bold = TRUE, background = "#D3D3D3") %>%
    column_spec(2:ncol(df), bold = TRUE, background = "#FFFFFF") 
}