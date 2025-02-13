for (file in list.files("script", pattern = "\\.R$", full.names = TRUE)) {source(file)}

ui <- fluidPage(
  title = "Triangulator",
  tags$head(tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Lobster&display=swap")),
  titlePanel(tags$div("Triangulator",style = title_style)),
  # --------------------------------------------------------------------
  useShinyjs(),
  sample_modal(button_style3, title_style2),
  tags$script(HTML("shinyjs.scrollToBottom = function() {window.scrollTo({ top: document.body.scrollHeight, behavior: 'smooth' });}")),
  # --------------------------------------------------------------------
  tabsetPanel(
    id = "main_tabs",
    # --------------------------------------------------------------------
    tabPanel("Import",
             div(style = position_style,
                 fluidRow(uiOutput("ui_0")),                                    #TITLE - step 1, and line-by-line upload 
                 fluidRow(uiOutput("ui_1a")),                                   #head of claims, and TITLE - step 2
                 fluidRow(                                      
                   column(width = 6,
                          uiOutput("ui_1b"),                                    #import variables (toggled in and out)
                          uiOutput("ui_1c"),                                    #manual selections (toggled in and out)
                          uiOutput("ui_1d"),                                    #button to assign variables
                          uiOutput("ui_2a")),                                   #download variables
                   column(width = 6,uiOutput("ui_1e"))),                        #assigned variables (and download)
                 fluidRow(uiOutput("conditional_line_a")),
                 fluidRow(uiOutput("ui_2b")),                                   #TITLE - step 3
                 fluidRow(uiOutput("ui_3a")),                                   #validation results
                 fluidRow(uiOutput("ui_2d")),                                   #line
                 fluidRow(uiOutput("ui_3b"))                                    #TITLE - step 4 
             )),
    # --------------------------------------------------------------------
    tabPanel("Dashboard",
             div(style = position_style,
                 uiOutput("conditional_title"),
                 uiOutput("conditional_line_1"),
                 fluidRow(
                   column(2,uiOutput("ui_4b_left")),                            #summary input
                   column(10,uiOutput("ui_4b_right"))),                             #summary output
                 uiOutput("conditional_line_2"),
                 fluidRow(
                   column(2,uiOutput("ui_4c_left")),                                 #bucket size input
                   column(10,uiOutput("ui_4c_right"))),                             #bucket size output
                 uiOutput("conditional_line_3"),
                 fluidRow(
                   column(2,uiOutput("ui_4d_left")),                                 #origin input
                   column(10,uiOutput("ui_4d_right"))),                               #origin output
                 uiOutput("conditional_line_4"),
                 fluidRow(
                   column(2,uiOutput("ui_4e_left")),                                           #dropdowns for triangles
                   column(10,
                          uiOutput("ui_4e_right_button"),
                          uiOutput("ui_4e_right"),                                           #explore input
                          uiOutput("ui_6a"),                                           #explore month output
                          uiOutput("ui_6b"),                                           #explore claim input
                          uiOutput("ui_7a")                                            #explore claim output
                   )),
                 uiOutput("conditional_line_5"),
                 #fluidRow(
                 #   column(2,uiOutput("duration_left")),                                           #dropdowns for triangles
                 #   column(10,uiOutput("duration_right"))),
                 # uiOutput("conditional_line_6"),
                 fluidRow(
                   column(2,uiOutput("initial_projections_left")),                                           #dropdowns for triangles
                   column(10,
                          uiOutput("initial_projections_right"))),
                 uiOutput("conditional_line_7")
             )),
    #--------------------------------------------------------
  ) # end of tabset panel
  #--------------------------------------------------------
) # end of UI




server <- function(input, output, session) {
  # ...........................................................................................
  # --- Reactive Values
  user_claims_database <- reactive({ req(input$file); withProgress(message = "Reading data, please wait ...", {read_excel(input$file$datapath)})})
  user_claim_vars <- reactiveValues()              # updated when variables assigned
  claims_wide_ <- reactiveVal()                    # updated when variables assigned
  current_positions_ <- reactiveVal()              # updated when variables assigned
  triangles_ <- reactiveValues(underwriting_monthly = reactiveVal(),underwriting_quarterly = reactiveVal(),underwriting_yearly = reactiveVal(),incident_monthly = reactiveVal(),incident_quarterly = reactiveVal(),incident_yearly = reactiveVal(),reporting_monthly = reactiveVal(),reporting_quarterly = reactiveVal(),reporting_yearly = reactiveVal())
  large_  <- reactiveVal()                         # optional: updated when filter_triangles clicked
  displayed_triangle_selections_ <- reactiveVal()  # updated when filter_triangles clicked
  movement_ <- reactiveVal()                       # optional: updated in the create triangles function
  button_clicked <- reactiveVal(FALSE)             # updated when "generate triangles" clicked (it renders the triangle automatically)
  show_title <- reactiveVal(TRUE)                  # updated when "generate triangles", message to user if go to dashboard before uploading data
  initial_reserves_ <- reactiveVal()
  initial_reserves_counter_ <- reactiveVal(0L)
  num_plots_ <- reactiveVal()
  notification_id <- reactiveVal(NULL)
  all_plots_ <- reactiveVal()
  find_claims_counter_ <- reactiveVal(0L)
  
  # ...........................................................................................
  # Line-by-Line Tab
  # --- (UI_0) upload interface for line-by-line claims
  output$ui_0 <- renderUI({
    tagList(
      tags$hr(style = line_style),
      tags$h3("Step 1: Upload Claims", style = title_style2), 
      tags$h6("Link up your line-by-line Claims database.", style = title_style3),
      div(style = centre_output_style2,fileInput("file",label = "", accept = c(".xlsx", ".xls"))))
  })
  
  
  # --- (UI_1) conditional UI once dataset uploaded
  observeEvent(user_claims_database(), {
    
    output$column_select_ui <- renderUI({
      df <- user_claims_database()
      tagList(
        div(style = blue_box,  
            fluidRow(
              column(6, selectInput("user_policy_id", "Policy Id:", choices = colnames(df))),
              column(6, selectInput("user_claim_id", "Claim Id:", choices = colnames(df)))),
            fluidRow(column(8, offset = 2, selectInput("user_groupings", "Groupings:", choices = colnames(df), multiple = TRUE))),
            fluidRow(
              column(6, selectInput("user_date_incident", "Incident Date:", choices = colnames(df))),
              column(6, selectInput("user_date_stamp", "Observation Date:", choices = colnames(df)))),
            fluidRow(
              column(6, selectInput("user_date_issue", "Policy Issue Date:", choices = colnames(df))),
              column(6, selectInput("user_date_reported", "Report Date:", choices = colnames(df)))),
            fluidRow(
              column(6, selectInput("user_paid", "Paid Claims:", choices = colnames(df))),
              column(6, selectInput("user_case", "Case Estimates:", selected = NULL, choices = colnames(df))))
        ))})
    
    head_data <- head(user_claims_database())

    output$ui_1a <- renderUI({
      tagList(
        div(style = centre_output_style, checkboxInput("ui_1a_button", label = "Show Data:", value = FALSE)),
        conditionalPanel(condition = "input.ui_1a_button == true",div(style = datatable_style, renderTable({head_data}))),
        tags$hr(style = line_style),
        tags$h3("Step 2: Define Variables", style = title_style2),
        tags$h6("You need to map the variable names in your database to the variable that Triangulator is expecting.", style = title_style3),
        div(style = centre_output_style, checkboxInput("ui_1_input_toggle_button", label = "First time selecting variables?", value = FALSE)) 
      )})
    
    output$ui_1b <- renderUI({
      tagList(
        div(id = "ui_1b",
            h3("Import Selections"),
            tags$h6("Import pre-made variables file:", style = title_style3),
            div(style = centre_output_style, fileInput("variable_selections", label = "", accept = c(".csv")))
        ))})
    
    output$ui_1c <- renderUI({
      tagList(
        div(id = "ui_1c", # Assign a unique ID here
            h3("Manual Selections"),
            tags$h6("Manually select variable names", style = title_style3),
            tags$h6("Note: download the variables file so you can re-use this via the import option:", style = title_style3),
            div(style = centre_output_style, uiOutput("column_select_ui"))
        ))})
    
    output$ui_1e <- renderUI({
      tagList(
        div(style = centre_output_style,h3("Assigned Selections")),
        tags$h6("These are your selected variable names.", style = title_style3),
        div(style = green_box,verbatimTextOutput("assigned_variables"))
      )})
    
    output$ui_1d <- renderUI({div(style = centre_output_style,actionButton("assign_variables", label = "Assign Variables", style = button_style))})
    
    output$conditional_line_a <- renderUI({tagList(tags$hr(style = line_style))})
    
    shinyjs::runjs("setTimeout(() => shinyjs.scrollToBottom(), 300);")
  })
  
  
  
  # --- (UI_2) conditional UI once you click assign variables
  observeEvent(input$assign_variables, {
    inputs_check <- c(input$user_policy_id,input$user_claim_id,input$user_groupings,input$user_date_incident,input$user_date_stamp,input$user_date_issue,input$user_date_reported,input$user_paid,input$user_case)
    if (length(inputs_check) != length(unique(inputs_check))) {shinyalert(title = "Warning", text = "Inputs must have unique values.", type = "info", confirmButtonText = "Understood",size = "l")
    } else {
      user_claim_vars$user_policy_id <- input$user_policy_id
      user_claim_vars$user_claim_id <- input$user_claim_id
      user_claim_vars$user_groupings <- input$user_groupings
      user_claim_vars$user_date_incident <- input$user_date_incident
      user_claim_vars$user_date_stamp <- input$user_date_stamp
      user_claim_vars$user_date_issue <- input$user_date_issue
      user_claim_vars$user_date_reported <- input$user_date_reported
      user_claim_vars$user_paid <- input$user_paid
      user_claim_vars$user_case <- input$user_case
      
      output$assigned_variables <- renderPrint({
        list(
          user_policy_id = user_claim_vars$user_policy_id,
          user_claim_id = user_claim_vars$user_claim_id,
          user_groupings = user_claim_vars$user_groupings,
          user_date_incident = user_claim_vars$user_date_incident,
          user_date_stamp = user_claim_vars$user_date_stamp,
          user_date_issue = user_claim_vars$user_date_issue,
          user_date_reported = user_claim_vars$user_date_reported,
          user_paid = user_claim_vars$user_paid,
          user_case = user_claim_vars$user_case
        )})
      
      output$ui_2a <- renderUI({
        tagList( 
          tags$hr(),
          div(style = centre_output_style,downloadButton("download_selections", "Download Selections", style = button_style3))
        )})
      
      output$ui_2b <- renderUI({
        tagList(
          tags$h3("Step 3: Data Checks & Validations", style = title_style2),
          div(style = centre_output_style,actionButton("perform_checks", label = "Perform Data Checks", style = button_style)),
          uiOutput("conditional_checkbox")
        )})
      
      output$ui_2d <- renderUI({tags$hr(style = line_style)})
      
      claims_wide_(create_claims_wide(user_claims_database,user_claim_vars))
      current_positions_(claims_wide_() %>% group_by(claim_id) %>% filter(date_stamp == max(date_stamp)) %>% ungroup())
    }
    shinyjs::runjs("setTimeout(() => shinyjs.scrollToBottom(), 200);")
  })
  
  
  
  # --- (UI_3) conditional UI once you click perform checks
  observeEvent(input$perform_checks, {
    
    issue_summary <- initial_data_checks(claims_wide_)
    
    validation_summary_table <- as.data.frame(summary(issue_summary)) %>% select(name,items,passes,fails,expression)
    
    output$validation_percent <- renderTable({as.data.frame(summary(issue_summary)) %>% summarise(Data_Score = (sum(passes)/sum(items))*100) %>% mutate(Data_Score = paste0(round(Data_Score, 1), "%")) })
    
    output$conditional_checkbox <- renderUI({
      tagList(
        div(style = datatable_style2, tableOutput("validation_percent")),  
        div(style = centre_output_style,checkboxInput("show_validation_results", label = "Show Detailed Results:", value = FALSE))
      )})
    
    output$ui_3a <- renderUI({
      req(input$show_validation_results)
      if(input$show_validation_results == TRUE){
        div(style = datatable_style, renderTable(validation_summary_table))
      }else{NULL}})
    
    shinyalert(title = "Data Checks", text = "Checks have been performed, please check the results before proceeding.", type = "info", confirmButtonText = "Understood",size = "l")
    
    output$ui_3b <- renderUI({
      tagList(
        tags$h3("Step 4: Click Button", style = title_style2),
        actionButton("generate_triangles", "Triangulate", style = button_style),
        tags$hr(style = line_style))})
    
    shinyjs::runjs("setTimeout(() => shinyjs.scrollToBottom(), 200);")
  })
  
  
  
  # --- (UI_4) conditional UI once you click triangulate
  observeEvent(input$generate_triangles, {
    withProgress(message = "", {
      
      shinyalert(
        title = "This will take a moment",
        text = "Please bear with us as we do some pre-processing.<br><br>Loading Now = Less Loading Later",
        type = "info",
        closeOnEsc = FALSE,
        closeOnClickOutside = FALSE,
        showConfirmButton = FALSE,
        size = "l",
        html = TRUE)
      # CHECK OUT THE ObserveEvent time_select_res WHICH CLOSES THE SHINYALERT
      
      # ............
      # Pre-processing
      assign_triangles(triangles_,user_claims_database,user_claim_vars)
      
      output$download_triangles_button <- renderUI({div(style = centre_output_style,downloadButton("download_triangles", "Download Triangles (all claims)", style = button_style3))})  
      
      # ............
      # Overviews
      output$ui_4b_left <- renderUI({
        tagList(
          tags$h3("Summary", style = title_style2),  
          tags$h6("Amount values are based on the incurred variable, at each claim_id's most recent position.", style = title_style3),
          tags$h6("Claims are categorised into open or closed based on the case estimates.", style = title_style3),
          div(style = centre_output_style, checkboxInput("overview_1_switch", label = "Show Data", value = FALSE))
        )})
      
      output$ui_4c_left <- renderUI({
        tagList(
          tags$h3("Number of Claims in Each Bucket", style = title_style2),
          tags$h6("This table shows how many claims are in each bucket.", style = title_style3),
          tags$h6("Values are based on the incurred variable, at each claim_id's most recent position.", style = title_style3),
          div(style = centre_output_style,selectInput(inputId = "user_bucket_size",label = "Select Bucket Size",choices = c(1000, 5000, 10000, 20000,50000,100000,500000),selected = 5000)),
          div(style = centre_output_style,checkboxInput("graph_switch_4b", label = "Show Data", value = FALSE))
        )})
      
      output$ui_4d_left <- renderUI({
        tagList(
          tags$h3("Origin Analysis", style = title_style2),  
          tags$h6("Each claim_id's most recent position is shown.", style = title_style3),
          div(style = centre_output_style,selectInput("basis_select_0", "Origin Basis", choices = c("underwriting","incident","reporting"), selected = "incident")),
          div(style = centre_output_style,selectInput("time_select_0", "Origin Time", choices = c("yearly","quarterly","monthly"), selected = "monthly")),
          div(style = centre_output_style,selectInput("variable_select_0", "Select Variable", choices = c("PTOT","ITOT","NTOT","STOT","ITOT_divided_by_NTOT","PTOT_divided_by_STOT","ITOT_minus_PTOT","NTOT_minus_STOT","PTOT_divided_by_ITOT","STOT_divided_by_NTOT"), selected = "ITOT_minus_PTOT")),
          div(style = centre_output_style,checkboxInput("graph_switch_4d", label = "Show Data", value = FALSE))
        )})
      
      # ............
      # Triangulations
      num_inputs <- length(user_claim_vars$user_groupings)
      input_list <- lapply(1:num_inputs, function(i) {
        grouping <- user_claim_vars$user_groupings[i]
        div(style = input_list_style,selectInput(inputId = paste0("grouping_select_", i),label = paste("Select", grouping),choices = isolate(unique(claims_wide_()[[grouping]]))))
      })
      
      output$ui_4e_left <- renderUI({
        elements <- tagList(
          tags$h3("Explore a Triangle", style = title_style2),
          tags$h6("Click the Explore Triangle button to refresh the triangle with your various selections.", style = title_style3),
          div(style = centre_output_style,selectInput("basis_select", "Origin Basis", choices = c("underwriting", "incident", "reporting"), selected = "incident")),
          div(style = centre_output_style,selectInput("time_select", "Origin Time", choices = c("yearly", "quarterly", "monthly"), selected = "monthly")),
          div(style = centre_output_style,selectInput("variable_select", "Select Variable", choices = c("PTOT", "ITOT", "STOT", "NTOT","ITOT_divided_by_NTOT","PTOT_divided_by_STOT","ITOT_minus_PTOT","NTOT_minus_STOT","PTOT_divided_by_ITOT","STOT_divided_by_NTOT","ITOT_links","PTOT_links","NTOT_links","STOT_links"), selected = "ITOT")))
        for (i in seq_along(user_claim_vars$user_groupings)) {
          grouping <- user_claim_vars$user_groupings[i]
          elements <- tagAppendChild(elements,div(style = centre_output_style,selectInput(inputId = paste0("grouping_select_", i),label = paste("Select", grouping),choices = isolate(unique(claims_wide_()[[grouping]])))))}
        elements <- tagAppendChild(elements,div(style = centre_output_style,checkboxInput("toggle", "Remove Large Losses? (if ever)", value = FALSE)))
        elements <- tagAppendChild(elements,conditionalPanel(condition = "input.toggle == true",div(style = centre_output_style,selectInput("user_large_loss_definition", "Large Loss Definition:", choices = c(1000, 5000, 10000, 20000, 50000, 100000, 500000), selected = 5000))))
        elements <- tagAppendChild(elements,div(style = centre_output_style,checkboxInput("show_heatmap", "Show Data", value = FALSE)))
        elements
      })
      
      output$ui_4e_right_button <- renderUI({div(style = centre_output_style,actionButton("filter_triangles", label = "Explore Triangle (Click to Refresh)", style = button_style))})
      
      
      # ............
      # Duration Calculation
      output$duration_left <- renderUI({
        tagList(
          tags$h3("Durations", style = title_style2),  
          tags$h6("Standard duration calculations have been performed on the triangles.", style = title_style3),
          tags$h6("Cashflows are assumed to occur mid way through the calendar time-period.", style = title_style3),
          tags$h6("Choose if you want the durations to be calculated from Monthly, Quarterly, or Yearly triangles below:", style = title_style3),
          div(style = centre_output_style,selectInput("time_select_dur", "Origin Time", choices = c("yearly","quarterly","monthly"), selected = "quarterly")),
          div(style = centre_output_style, checkboxInput("duration_switch", label = "Show Data:", value = FALSE))
        )})
      
      user_groupings <- user_claim_vars$user_groupings
      vector_of_groups <- claims_wide_() %>% select(!!!syms(user_groupings)) %>% distinct() %>% unite("groups", all_of(user_groupings), sep = "_", remove = TRUE) %>% pull()
      
      # ............
      # Initial Reserves
      output$initial_projections_left <- renderUI({
        tagList(
          tags$h3("Initial Reserves", style = title_style2),  
          tags$h6("Basic chain-ladders have been applied to estimate these initial reserves.", style = title_style3),
          tags$h6("The reserves may be very volatile as the chain-ladder is blindly used. Therefore please do not rely on these numbers for reserving purposes.", style = title_style3),
          tags$h6("Choose if you want the chain-ladder to run of Monthly, Quarterly, or Yearly triangles below:", style = title_style3),
          div(style = centre_output_style,selectInput("time_select_res", "Origin Time", choices = c("yearly","quarterly","monthly"), selected = "quarterly")),
          div(style = centre_output_style,selectInput("line_select_res", "Select Grouping:", 
                                                      choices = vector_of_groups, 
                                                      selected = vector_of_groups[1])),
          div(style = centre_output_style, checkboxInput("initial_projections_switch", label = "Show Data:", value = FALSE))
        )})
      
      # ............
      # Bring user to the Dashboard
      updateTabsetPanel(session, inputId = "main_tabs", selected = "Dashboard")
      
    })
    id <- showNotification("On the last leg now...", duration = NULL, type = "message")
    notification_id(id)
  })
  
  
  
  
  
  
  
  
  
  
  
  # ...........................................................................................
  # ...........................................................................................
  # Reactive AFTER Input Panel
  # ...........................................................................................
  # ...........................................................................................
  
  # ............
  # Sample Data Download Helpers
  observeEvent(input$trigger_sample_download, {shinyjs::click("download_sample")})
  observeEvent(input$trigger_mapping_download, {shinyjs::click("download_sample_mapping")})
  hide("download_sample")
  hide("download_sample_mapping")
  
  
  # ............
  # Toggles the manual variable selection/upload selection
  observeEvent(input$ui_1_input_toggle_button, {
    toggle("ui_1b") 
    toggle("ui_1c") 
  })
  shinyjs::hide("ui_1b")
  
  
  # ............
  # Clicks the filter_triangle button after generate_triangles button in order to render the triangle initially
  observeEvent(input$basis_select, {
    if (!button_clicked()) {  
      shinyjs::runjs("setTimeout(function() {document.getElementById('filter_triangles').click();}, 2000);")
      button_clicked(TRUE)  
    }})
  
  
  # ............
  # Overview 1
  observeEvent(input$overview_1_switch, {
    
    overview_1 <- overview_1_data(current_positions_,user_claim_vars)
    overview_1_plot <- overview_1_graph(overview_1)
    overview_1_plot_2 <- overview_1_graph_2(overview_1)
    
    output$ui_4b_right <- renderUI({
      if(input$overview_1_switch == TRUE){
        tagList(
          div(style = datatable_style, HTML(format_overview_1(overview_1)))) } else {
            tagList(
              div(style = centre_graph,renderPlotly({ggplotly(overview_1_plot)})),
              div(style = centre_graph,renderPlot({overview_1_plot_2}))
            )}})
  })
  
  
  # ............
  # Overview 2
  observeEvent(input$user_bucket_size, {
    
    user_bucket_size <- as.numeric(input$user_bucket_size)
    overview_2 <- overview_2_data(current_positions_,user_claim_vars,user_bucket_size)
    
    output$ui_4c_right <- renderUI({
      tagList(
        if(input$graph_switch_4b == FALSE){
          tagList(
            div(style = centre_graph,renderPlotly({overview_2_graph(overview_2)})))} else {
              tagList(
                div(style = datatable_style, HTML(format_overview_2(overview_2))))}
      )})
  })
  
  
  # ............
  # Overview 3
  observeEvent(list(input$basis_select_0, input$time_select_0, input$variable_select_0), {
    req(input$basis_select_0, input$time_select_0, input$variable_select_0)
    
    triangle_name <- paste(input$basis_select_0, input$time_select_0, sep = "_")
    variable_select <- input$variable_select_0
    overview_3 <- overview_3_data(triangles_,triangle_name,user_claim_vars,variable_select = variable_select)
    
    output$ui_4d_right <- renderUI({
      if(input$graph_switch_4d == FALSE){
        tagList(
          div(style = centre_graph,renderPlotly({overview_3_graph(overview_3)})))} else {
            tagList(  
              div(style = datatable_style, HTML(format_overview_3(overview_3))))}
    })
  })
  
  
  # ............
  # Triangulations 
  observeEvent(input$filter_triangles, {
    
    if(input$toggle == TRUE){
      withProgress(message = "Triangulating, please wait ...", {
        large_(input$user_large_loss_definition)
        data <- create_triangles(user_claims_database, user_claim_vars,basis_ = input$basis_select,time_ = input$time_select,user_large_loss_definition = large_(),movement_,update_movement_ = "no") })
    } else if(input$toggle == FALSE){
      large_(999999999)
      triangle_name <- paste(input$basis_select, input$time_select, sep = "_")
      data <- triangles_[[triangle_name]]()}
    
    displayed_triangle_selections_(setNames(lapply(seq_along(user_claim_vars$user_groupings), function(i) {input[[paste0("grouping_select_", i)]]}),user_claim_vars$user_groupings))
    displayed_triangle_data <- data %>% filter_at(vars(user_claim_vars$user_groupings), all_vars(. %in% unlist(displayed_triangle_selections_()))) %>% filter(variable == input$variable_select)
    displayed_triangle <- displayed_triangle_data %>% pivot_wider(names_from = development, values_from = value) %>% arrange(origin) %>% select(-all_of(user_claim_vars$user_groupings), -variable)
    
    output$ui_4e_right <- renderUI({
      tagList(
        if(input$show_heatmap == FALSE){
          div(style = centre_graph,renderPlotly({triangulations_1_graph(displayed_triangle_data)}))} else {
            div(style = datatable_style,HTML(format_dataframe(displayed_triangle)))},
        tags$h6("***A red circle indicates a possible link-ratio outlier.", style = title_style3),
        uiOutput("download_triangles_button"),
        tags$hr(style = line_style),
        tags$h3("Explore Triangle Movements", style = title_style2),
        div(style=blue_box,
            div(style = centre_output_style,selectInput("filter_origin", "Select Origin", choices = sort(unique(displayed_triangle_data$origin)))),
            div(style = centre_output_style,selectInput("filter_development", "Select Development", choices = (sort(unique(displayed_triangle_data$development)))))),
        tags$h3(),
        div(style = centre_output_style,actionButton("find_claims", label = "Explore Chunk", style = button_style)),
        tags$hr()
      )})
    
    output$ui_6a <- renderUI({tagList(renderTable({ NULL }))}) # month table
    output$ui_6b <- renderUI({tagList(renderTable({ NULL }))}) # individual input
    output$ui_7a <- renderUI({tagList(renderTable({ NULL }))}) # individual output
  })
  
  
  # ............
  # Explore Chunk 
  # conditional UI once you filter by origin / development
  observeEvent(input$find_claims, {
    withProgress(message = "Finding movements, please wait ...", {
      
      if(find_claims_counter_()>=0){
        shinyalert(
          title = "This will take a moment",
          text = "Please bear with us as we find the claims movements.",
          type = "info",
          closeOnEsc = FALSE,
          closeOnClickOutside = FALSE,
          showConfirmButton = FALSE,
          size = "l")
      }
      
      create_triangles(user_claims_database, user_claim_vars, basis_ = input$basis_select, time_ = input$time_select, user_large_loss_definition = large_(), movement_, update_movement_ = "yes") 
      
      ui_db <- movement_filter(movement_,input,user_claim_vars,displayed_triangle_selections_)
      
      ui_db_summary <- ui_db %>%
        select(-ITOT_movement_proportion,-ITOT_movement_percent) %>%
        summarise(across(where(is.numeric), sum, na.rm = TRUE))
      
      output$datatable_ui_db <- renderDT({
            datatable(ui_db, options = list(pageLength = 10,lengthMenu = c(10, 25, 50),scrollX = TRUE)) %>%
          formatStyle(
            c("ITOT_movement_proportion"),
            background = styleColorBar(range(ui_db$ITOT_movement_proportion, na.rm = TRUE), "lightpink"),
            backgroundSize = "90% 50%",
            backgroundRepeat = "no-repeat",
            backgroundPosition = "left"
          ) %>%
          formatStyle(
            c("ITOT_movement_percent"),
            background = styleColorBar(range(ui_db$ITOT_movement_percent, na.rm = TRUE), "lightpink"),
            backgroundSize = "90% 50%",
            backgroundRepeat = "no-repeat",
            backgroundPosition = "left"
          ) %>%
          #formatStyle('ITOT_movement_proportion',background=color_from_middle(ui_db$ITOT_movement_proportion,'lightblue','lightpink')) %>%
          #formatStyle('ITOT_movement_percent',background=color_from_middle(ui_db$ITOT_movement_percent,'lightblue','lightpink')) %>%
          formatPercentage(
            c("ITOT_movement_proportion", "ITOT_movement_percent"), # Columns to format
            digits = 2 # Number of decimal places
          ) 
      })
      
      
      
      output$ui_6a <- renderUI({
        tagList(
          tags$h3("Movement Summary", style = title_style4),
          div(style = centre_output_style, renderTable({ui_db_summary})),
          tags$h3("Individual Claim Movements", style = title_style4),
          div(style = datatable_style, DTOutput("datatable_ui_db")),
          tags$hr(style = line_style)
        )})
      
      output$ui_6b <- renderUI({
        tagList(
          tags$h3("Explore a Particular Claim", style = title_style2),
          div(style = blue_box, div(style = centre_output_style, selectizeInput("filter_claim_id", "Select Claim Id", choices = NULL, multiple = FALSE))),
          tags$h3(),
          div(style = centre_output_style, actionButton("find_individual_claim", label = "Explore Claim", style = button_style)),
          tags$hr()
        )})
      
      updateSelectizeInput(session, "filter_claim_id", choices = unique(ui_db$claim_id), server = TRUE)
      
      output$ui_7a <- renderUI({tagList(renderTable({ NULL }))})
      
      if(find_claims_counter_()>=0){runjs("swal.close();")}
      
      #---------
      
      find_claims_counter_(find_claims_counter_()+1)
      
      #---------
      
    })})
  
  
  # ............
  # Explore Claim 
  # conditional UI once you filter by individual claim id
  observeEvent(input$find_individual_claim, {
    
    ui_db2 <- claims_wide_() %>% filter(claim_id == input$filter_claim_id)
    
    ui_db2$date_issue <- as.Date(ui_db2$date_issue, origin = "1899-12-30")  # Excel uses 1900 as the origin
    ui_db2$date_stamp <- as.Date(ui_db2$date_stamp, origin = "1899-12-30")  # Excel uses 1900 as the origin
    ui_db2$date_incident <- as.Date(ui_db2$date_incident, origin = "1899-12-30")
    ui_db2$date_reported <- as.Date(ui_db2$date_reported, origin = "1899-12-30")
    
    # Format the Date objects to 'DD/MM/YYYY' for display
    ui_db2$date_issue <- format(ui_db2$date_issue, "%d/%m/%Y")
    ui_db2$date_stamp <- format(ui_db2$date_stamp, "%d/%m/%Y")
    ui_db2$date_incident <- format(ui_db2$date_incident, "%d/%m/%Y")
    ui_db2$date_reported <- format(ui_db2$date_reported, "%d/%m/%Y")
    
    output$ui_7a <- renderUI({div(style = datatable_style, renderTable({ ui_db2 }))})
  })
  
  # ............
  # Duration Calculation
  observeEvent(input$time_select_dur, {
    req(input$time_select_dur)
    
    time <- input$time_select_dur
    user_groupings <- user_claim_vars$user_groupings
    duration_data <- duration(triangles_,user_groupings,"incident",time)
    duration_plot <- duration_graph(duration_data,user_groupings,time)
    
    
    output$duration_right <- renderUI({
      if(input$duration_switch == FALSE){
        div(style = centre_output_style,renderPlot({duration_plot}))
      } else {
        div(style = datatable_style, HTML(format_duration(duration_data %>%
                                                            pivot_wider(
                                                              names_from = all_of(user_groupings),
                                                              values_from = duration))))}
    })
  })
  
  # ............
  # Initial Reserve Projections
  observeEvent(list(input$time_select_res), { #,input$line_select_res
    req(input$time_select_res,input$line_select_res)
    
    if(initial_reserves_counter_()>0){
      shinyalert(
        title = "This will take a moment",
        text = "Please bear with us as we calculate your initial reserves.",
        type = "info",
        closeOnEsc = FALSE,
        closeOnClickOutside = FALSE,
        showConfirmButton = FALSE,
        size = "l")
    }
    
    time <- input$time_select_res
    initial_plot <- initial_projections(time,user_claim_vars,triangles_,initial_reserves_,num_plots_,all_plots_)
    
    output$initial_projections_right <- renderUI({
      if(input$initial_projections_switch == FALSE){
        div(style = centre_output_style,renderPlot({all_plots_()[[input$line_select_res]]})) #, height = function() {num_plots_() * 400}
      } else {
        div(style = datatable_style, HTML(format_initial_reserves(initial_reserves_())))}
    })
    
    if(initial_reserves_counter_()>0){runjs("swal.close();")}
    
    
    #--------- THIS CLOSES THE INITIAL NON-MOVEABLE LOADING SCREEN AFTER CLICKING GENERATE TRIANGLES
    
    if(initial_reserves_counter_()==0){
      runjs("swal.close();")
      removeNotification(notification_id()) 
      shinyalert(
        title = "Triangulations Complete",
        text = "Welcome to the Dashboard.<br><br>In just a moment, you will see all your claims diagnostics.<br><br>If you are enjoying this app, consider helping with its development.<br><br>GitHub: <a href='https://github.com/headline-grabber/Triangulator_V1.1' target='_blank'>https://github.com/headline-grabber/Triangulator_V1.1</a>",
        type = "success",
        size = "l",
        html = TRUE)
    }
    
    #---------
    
    initial_reserves_counter_(initial_reserves_counter_()+1)
    
    #---------
    
    
  })
  
  
  
  # ............
  # Conditional formatting/title for the Dashboard panel BEFORE data is uploaded
  output$conditional_title <- renderUI({if (show_title()) {tags$h3("Upload Data to View the Dashboard", style = title_style2)} else {NULL}})
  output$conditional_line_1 <- renderUI({if (show_title()) {NULL} else {tags$hr(style = line_style)}})
  output$conditional_line_2 <- renderUI({if (show_title()) {NULL} else {tags$hr(style = line_style)}})
  output$conditional_line_3 <- renderUI({if (show_title()) {NULL} else {tags$hr(style = line_style)}})
  output$conditional_line_4 <- renderUI({if (show_title()) {NULL} else {tags$hr(style = line_style)}})
  output$conditional_line_5 <- renderUI({if (show_title()) {NULL} else {tags$hr(style = line_style)}})
  output$conditional_line_6 <- renderUI({if (show_title()) {NULL} else {tags$hr(style = line_style)}})
  output$conditional_line_7 <- renderUI({if (show_title()) {NULL} else {tags$hr(style = line_style)}})
  
  observeEvent(input$generate_triangles, {show_title(FALSE)})
  
  
  
  
  # ...........................................................................................
  # IMPORTS & EXPORTS
  
  # --- (import button) conditional once you upload assigned parameters - Updates selections when import csv
  observeEvent(input$variable_selections, {
    imported_data <- read.csv(input$variable_selections$datapath, stringsAsFactors = FALSE)
    updateSelectInput(session, "user_policy_id", selected = imported_data$user_policy_id[1])
    updateSelectInput(session, "user_claim_id", selected = imported_data$user_claim_id[1])
    updateSelectInput(session, "user_groupings", selected = unlist(strsplit(imported_data$user_groupings[1], ",")))
    updateSelectInput(session, "user_date_incident", selected = imported_data$user_date_incident[1])
    updateSelectInput(session, "user_date_stamp", selected = imported_data$user_date_stamp[1])
    updateSelectInput(session, "user_date_issue", selected = imported_data$user_date_issue[1])
    updateSelectInput(session, "user_date_reported", selected = imported_data$user_date_reported[1])
    updateSelectInput(session, "user_paid", selected = imported_data$user_paid[1])
    updateSelectInput(session, "user_case", selected = imported_data$user_case[1])
    
    shinyjs::runjs("setTimeout(() => shinyjs.scrollToBottom(), 200);")
  })
  
  # --- (export button) download variable_selections
  output$download_selections <- downloadHandler(
    filename = function() {"variable_selections.csv"},
    content = function(file) {
      selected_data <- data.frame(
        user_policy_id = input$user_policy_id,
        user_claim_id = input$user_claim_id,
        user_groupings = paste(input$user_groupings, collapse = ","),
        user_date_incident = input$user_date_incident,
        user_date_stamp = input$user_date_stamp,
        user_date_issue = input$user_date_issue,
        user_date_reported = input$user_date_reported,
        user_paid = input$user_paid,
        user_case = input$user_case,
        stringsAsFactors = FALSE)
      report_path <- file.path(tempdir(), "variable_selections.csv")
      write.csv(selected_data,report_path, row.names = FALSE)
      file.copy(report_path, file)})
  
  
  # --- (export button) download triangles
  output$download_triangles <- downloadHandler(
    filename = function() {"Triangles.xlsx"},
    content = function(file) {
      temp_file <- file.path(tempdir(), "Triangles.xlsx")
      wb <- createWorkbook()
      triangles_names <- c("underwriting_monthly", "underwriting_quarterly", "underwriting_yearly","incident_monthly", "incident_quarterly", "incident_yearly","reporting_monthly", "reporting_quarterly", "reporting_yearly")
      for (triangle_name in triangles_names) {
        triangle_data <- triangles_[[triangle_name]]()
        if (!is.null(triangle_data) && nrow(triangle_data) > 0) {
          addWorksheet(wb, sheetName = triangle_name)
          writeData(wb, sheet = triangle_name, x = triangle_data)}}
      saveWorkbook(wb, temp_file, overwrite = TRUE)
      file.copy(temp_file, file, overwrite = TRUE)
    })
  
  # --- (export button) download sample data
  output$download_sample <- downloadHandler(
    filename = function() {"sample_database.xlsx"},
    content = function(file) {file.copy("www/sample_database.xlsx", file)})
  
  # --- (export button) download sample variables
  output$download_sample_mapping <- downloadHandler(
    filename = function() {"variable_selections (for sample database).csv"},
    content = function(file) {file.copy("www/variable_selections (for sample database).csv", file)})
  
  
  # --------------------------------------------------------------------
} # end of server


shinyApp(ui, server)