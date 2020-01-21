library(data.table)
library(magrittr)
# library(spdep)
edu <- fread("data/edu_description.csv", encoding = "UTF-8")
edu <- edu[, lapply(.SD, enc2native)]
# OUTCOME DATA ------------------------------------------------------------
data_files <-
  list.files(path = "data/",
             pattern = "export_diag",
             full.names = TRUE)
export_diag <- lapply(data_files, fread, encoding = "UTF-8") %>% rbindlist()
export_opr <- fread("data/export_opr.txt", encoding = "UTF-8")
export_med <- fread("data/export_med.txt", encoding = "UTF-8")
preProccess <- function(export_dat) {
  dat <- split(export_dat, by = "outcome") %>%
    lapply(., split, by = "aggr_level")
  
  lapply(dat, function(outcome) {
    out <- lapply(outcome, function(aggr_level) {
      aggr_level[, `:=` (outcome = NULL, aggr_level = NULL)]
      
      # !!!!! DO NOT CHANGE !!! unless you have checked with it does not
      # interfere with cbind operation in dtCast()
      setkey(aggr_level, sex, grouping, year)
    })
    
    # Change edu to factor to ensure correct ordering
    out$edu[, grouping := factor(grouping,
                                 levels = c(
                                   "Basic",
                                   "Secondary",
                                   "Tertiary",
                                   "Postgraduate",
                                   "Unknown"
                                 ))]
    out
  })
}

dat_opr <- preProccess(export_opr)
dat_med <-  preProccess(export_med)
dat_diag <-  preProccess(export_diag)
shiny_dat_en <- c(dat_opr, dat_med, dat_diag)
# shiny_dat_en <- (dat_diag)
cleanGeoData <- function(x) {
  # Remove unknowns from Region. and remove Christiansoe
  lapply(x, function(outcome) {
    outcome$region <- outcome$region[grouping != "Unknown", ]
    outcome$kom <- outcome$kom[grouping != "Christiansø", ]
    outcome
  })
  
}

setNAtoZero <- function(x) {
  lapply(x, function(outcome) {
    lapply(outcome, function(aggr_level) {
      data_vars <- grep("count|rate", colnames(aggr_level), value = TRUE)
      aggr_level[, (data_vars) := lapply(.SD, function(i) {
        i[is.na(i)] <- 0L
        i
      }),
      .SDcols = data_vars]
      
    })
  })
}


shiny_dat_en <- cleanGeoData(shiny_dat_en)
shiny_dat_en <- setNAtoZero(shiny_dat_en)
saveRDS(shiny_dat_en, file = "data/shiny_dat_en.rds")

# DANISH LANGUAGE SUPPORT -------------------------------------------------
makeDanish <- function(dat) {
  # Change english education labels to Danish labels
  lapply(dat, function(outcome) {
    outcome$edu <-
      merge(
        outcome$edu,
        edu[, .(edu_name_dk, edu_name_en)],
        by.x = "grouping",
        by.y = "edu_name_en",
        all.x = TRUE
      )
    outcome$edu[, `:=` (grouping = edu_name_dk)]
    outcome$edu[, `:=` (edu_name_dk = NULL)]
    
    # Turn DK edu into factor
    outcome$edu[, `:=` (grouping = factor(
      grouping,
      levels = c(
        edu[, edu_name_dk]
      )
    ))]
    setkey(outcome$edu, sex, grouping, year)
    outcome
  })
}

shiny_dat_dk <- makeDanish(shiny_dat_en)
saveRDS(shiny_dat_dk, file = "data/shiny_dat_dk.rds")