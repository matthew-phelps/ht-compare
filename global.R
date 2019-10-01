library(data.table)
library(shiny)


# LANGUAGE UI ---------------------------------------------------------
lang = "dk"
if (lang == "dk") {
  thousands_sep <- "."
  dec_mark <- ","
} else {
  thousands_sep <- ","
  dec_mark <- "."
}


# OBJECTS ------------------------------------------------------------
data_path <- file.path(paste0("data/shiny_dat_", lang, ".rds"))
shiny_dat <- readRDS(file = data_path)
year_max <- 2016


ui_file_path <- file.path(paste0("ui/ui_", lang, ".R"))
source(ui_file_path, encoding = "UTF-8")
