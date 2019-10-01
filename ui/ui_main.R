tabPanel("Main",
         sidebarLayout(
           sidebarPanel( 
           selectizeInput(inputId = "outcome",
                          label = "Outcome",
                          choices = outcome_choices),
           selectizeInput(inputId = "var",
                          label = "Variable",
                          choices = outcome_choices)
         ),
         mainPanel()
         
         
         
         )
)