ui <- navbarPage(
  title = "HjerteTal2",
  collapsible = TRUE,
  source(file.path("ui", "ui_main.R"), local = TRUE)$value
  
)
