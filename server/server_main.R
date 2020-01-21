outcomeCode <- reactive({
  # Connect the input in normal language to the hjertetal_code. This is so we
  # can change the description without having to rename allll the datasets.
  
  outcomes_all[name_dk == input$outcome, hjertetal_code]
})


makeDataObj <- reactive({
  var_selected <- variable_ui[var_dk == input$var, code_name]
  
  keep_vars <- c("sex", "year", "grouping", var_selected)
  
  x_new <-
    dat_new[[outcomeCode()]][[input$aggr_level]][, ..keep_vars]
  x_new[, year := as.character(year)]
  x_old <-
    dat_old[outcome == outcomeCode() &
              aggr == input$aggr_level & var == (var_selected), ]
  
  if (input$aggr_level == "national") {
    x_new[, grouping := year]
  } else {
    # If not national level, then we have to filter on years
    x_new[year == input$year]
    x_old <-
      x_old[year == input$year, ]
    
  }
  
  
  
  merge(x_new, x_old, by = c("sex", "year", "grouping"))
  
})

dataDifference <- reactive({
  var_selected <- variable_ui[var_dk == input$var, code_name]
  x <- makeDataObj()
  # Find the difference (with HT1 acting as the reference for the percentages)
  x[, diff_absolute := get(var_selected) - value]
  x[, diff_relative := (diff_absolute / get(var_selected)) * 100]
  x
  
})



output$diff_absolute <- renderPlot({
  x <- dataDifference()
  ggplot(x) +
    geom_line(aes(
      x = grouping,
      y = diff_absolute,
      group = sex,
      color = sex
    ),
    size = 1.5) +
    theme_classic() +
    geom_hline(aes(yintercept = 0), linetype = 2) +
    ylab("Difference (counts)") +
    ggtitle(paste0(input$var, " (absolute difference)")) +
    theme(plot.title = element_text(size = 22, face = "bold"))
  
  
})

output$diff_relative <- renderPlot({
  x <- dataDifference()
  
  ggplot(x) +
    geom_line(
      mapping = aes(
        x = grouping,
        y = diff_relative,
        group = sex,
        color = sex
      ),
      size = 1.5
    ) +
    geom_ribbon(aes(
      x = grouping,
      ymin = -5,
      ymax = 5,
      group = sex
    ),
    fill = "grey",
    alpha = 0.2) +
    theme_classic() +
    geom_hline(aes(yintercept = 0), linetype = 2) +
    ylab("Difference (%)") +
    coord_cartesian(ylim = c(-10, 10))
})




output$tables_t <- renderDT({
  x <- dataDifference()
  
  var_selected <- variable_ui[var_dk == input$var, code_name]
  keep_vars <-
    c("sex",
      "year",
      "grouping",
      var_selected,
      "value",
      "diff_absolute",
      "diff_relative")
  x <- x[, ..keep_vars]
  
  sum_cols <- c(var_selected, "value")
  x <-
    x[, lapply(.SD, sum), by = c("year", "grouping"), .SDcols = sum_cols]
  
  count_col <- grep("count", colnames(x), value = TRUE)
  x[, diff := get(count_col) - value]
  x[, diff_pct := round((diff / value) * 100, digits = 1)]
  
  setnames(x, c("year", "grouping", "my count", "SIF count", "difference", "difference (%)"))
  
  DT::datatable(x,
                extensions = "Buttons",
                options = list(dom = "B", buttons = c("excel"),
                               pageLength = 15)) %>%
    formatCurrency(c("my count", "SIF count"), currency = "", digits = 0)
  
  
})

output$tables_m <- renderDT({
  x <- dataDifference()
  browser()
  var_selected <- variable_ui[var_dk == input$var, code_name]
  keep_vars <-
    c("sex",
      "year",
      "grouping",
      var_selected,
      "value",
      "diff_absolute",
      "diff_relative")
  x <- x[, ..keep_vars]
  
})

output$tables_f <- renderDT({
  x <- dataDifference()
  browser()
  var_selected <- variable_ui[var_dk == input$var, code_name]
  keep_vars <-
    c("sex",
      "year",
      "grouping",
      var_selected,
      "value",
      "diff_absolute",
      "diff_relative")
  x <- x[, ..keep_vars]
  
})