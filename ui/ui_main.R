tabPanel("Main",
         sidebarLayout(
           sidebarPanel( 
           selectizeInput(inputId = "outcome",
                          label = "Outcome",
                          choices = outcome_choices),
           selectizeInput(inputId = "var",
                          label = "Variable",
                          choices = variable_ui$var_dk),
           selectizeInput(inputId = "aggr_level",
                          label = "Aggregation",
                          choices = aggr_choices$name_ht,
                          selected = "national"),
           selectizeInput(inputId = "year",
                          label = "Year",
                          choices = 2006:2016,
                          selected = "2016")
         ),
         mainPanel(
           tabsetPanel(
             tabPanel("Raw", plotOutput(outputId = "raw_data")),
             tabPanel("Percentage", plotOutput(outputId = "diff_relative")),
             tabPanel("Absolute", plotOutput(outputId = "diff_absolute"))
             ),
           
           fluidRow(DTOutput("tables_t"))
         )
         
         
         
         )
)