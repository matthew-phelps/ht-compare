shinyServer(function(input, output, session) {
  # source("global.R")
  # session$onSessionEnded(stopApp)
  source(file.path("server", "server_main.R"),
         encoding = "UTF-8",
         local = TRUE)$value

})
