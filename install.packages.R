install_if_missing <- function(packages) {
  missing <- packages[!packages %in% installed.packages()[, "Package"]]
  if (length(missing)) {
    install.packages(missing)
    message("Installed missing packages: ", paste(missing, collapse = ", "))
  } else {
    message("All packages are already installed.")
  }
}

# List of required packages
required_packages <- c(
  "shiny", "shinyalert", "shinyjs", "ggplot2", "dplyr", "DT", "readxl", 
  "openxlsx", "tidyr", "data.table", "validate", "lubridate", "rlang", 
  "rmarkdown", "knitr", "kableExtra", "plotly", "scales", "cowplot", 
  "later", "waterfalls", "patchwork", "purrr"
)

# Install missing packages
install_if_missing(required_packages)
