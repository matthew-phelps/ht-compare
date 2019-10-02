library(data.table)
library(shiny)
library(magrittr)
library(readxl)

# LANGUAGE UI ---------------------------------------------------------
lang = "dk"
if (lang == "dk") {
  thousands_sep <- "."
  dec_mark <- ","
} else {
  thousands_sep <- ","
  dec_mark <- "."
}



# OLD HJERTETAL DATA ------------------------------------------------------
data_files <-
  list.files("data/old_ht",
             full.names = TRUE)
dat_tmp <- lapply(data_files, function(i) {
  read_xlsx(i) %>% setDT()
})


dat_old <- lapply(dat_tmp, function(x) {
  i <- copy(x)
  i[sex == "Mænd", sex := "male"]
  i[sex == "Kvinder", sex := "female"]
  i[is.na(sex), sex := "total"]

  # Clean national level
  if (nrow(i) == 33) {
    i[, aggr := "national"]
    i[, grouping := "national"]
    setcolorder(i,
                c("outcome", "sex", "year", "grouping", "var", "aggr", "value"))
  }
  
  # Clean age-level
  if (nrow(i) == 21) {
    i[, aggr := "age"]
    i[is.na(grouping), grouping := "total"]
    i[, grouping := gsub("-", " - ", grouping)]
    i[, grouping := gsub(" år", "", grouping)]
    setcolorder(i,
                c("outcome", "sex", "year", "grouping", "var", "aggr", "value"))
  }
  i
})


# OBJECTS -----------------------------------------------------------------
data_path <- file.path(paste0("data/shiny_dat_", lang, ".rds"))
dat_new <- readRDS(file = data_path)
year_max <- 2016


ui_file_path <- file.path(paste0("ui/ui_", lang, ".R"))
source(ui_file_path, encoding = "UTF-8")
