ui <- navbarPage(
  title = "HT-compare",
  collapsible = TRUE,
  source(file.path("ui", "ui_main.R"), local = TRUE)$value
  
)
