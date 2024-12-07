sample_modal <- function(button_style3, title_style2) {
  tagList(
    tags$div(
      style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
      tags$a(href = "#", onclick = "$('#helpModal').modal('show');", "Documentation", style = button_style3),
      tags$a(href = "#", onclick = "$('#sampleModal').modal('show');", "Sample Data", style = button_style3)
    ),
    tags$div(id = "helpModal", class = "modal", tabindex = "-1", 
             tags$div(class = "modal-dialog", 
                      tags$div(class = "modal-content", 
                               tags$div(class = "modal-header", 
                                        tags$button(type = "button", class = "close", "data-dismiss" = "modal", "×"),
                                        tags$h4("Documentation")),
                               tags$div(class = "modal-body",
                                        tags$iframe(src = "documentation.pdf", height = 800, width = "100%")
                               )))),
    tags$div(id = "sampleModal", class = "modal", tabindex = "-1", style = "z-index: 1050;", `data-backdrop` = "static", `data-keyboard` = "false",
             tags$div(class = "modal-dialog", 
                      tags$div(class = "modal-content", 
                               tags$div(class = "modal-header", 
                                        tags$button(type = "button", class = "close", "data-dismiss" = "modal", "×"),
                                        tags$h4("Download Sample Data", style = title_style2)),
                               tags$div(style = "text-align: center;",
                                        class = "modal-body",
                                        tags$a(href = "#", onclick = "Shiny.setInputValue('trigger_sample_download', true);", 
                                               "Sample Data", style = button_style3),
                                        tags$a(href = "#", onclick = "Shiny.setInputValue('trigger_mapping_download', true);", 
                                               "Sample Mapping", style = button_style3)
                               )))),
    downloadButton("download_sample", "."),
    downloadButton("download_sample_mapping", ".")
  )
}
