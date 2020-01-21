library(data.table)
library(shiny)
library(magrittr)
library(readxl)
library(ggplot2)
library(DT)
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

# Special formatting for admissions data

dat_tmp <- lapply(dat_tmp, function(x){
  col_names <- colnames(x)
  if(any(grepl("Filtreret", colnames(x)))){
    col_names <- as.matrix(x)[2, ]
    setnames(x, col_names)
    x <- x[3:nrow(x)]
  }
    x
})

dat_adm <- lapply(dat_tmp, function(x) {
  
  col_names <- tolower(colnames(x))
  if(any(grepl("indlagte patienter", col_names))){
    i <- copy(x)  
    if(any(grepl("alder", col_names))){
    setnames(i, c("sex","grouping","value1","value2", "tmp", "outcome"))  
    i[, tmp := NULL]
    i[, year := 2015]
    i <- NULL
    } else{
    setnames(i, c("sex","year","value1","value2", "outcome"))
    n_patients <- i[, .(sex, year, value1, outcome)][, var := "count_n_admissions_ppl"]
    setnames(n_patients, "value1", "value")
    n_adm <- i[, .(sex, year, value2, outcome)][, var := "count_n_admissions"]
    setnames(n_adm, "value2", "value")
    return(list(n_patients, n_adm))
    }
  }
})


dat_amb <- lapply(dat_tmp, function(x) {
  col_names <- tolower(colnames(x))
  
  if (any(grepl("antal ambulant", col_names))) {
    i <- copy(x)
    
    # if(any(grepl("alder", col_names))){
    #   setnames(i, c("sex","grouping","value1","value2", "tmp", "outcome"))
    #   i[, tmp := NULL]
    #   i[, year := 2015]
    #   i <- NULL
    # } else{
    
    setnames(i, c("sex", "year", "value1", "value2", "outcome"))
    n_patients <-
      i[, .(sex, year, value1, outcome)][, var := "count_n_ambulatory_ppl"]
    setnames(n_patients, "value1", "value")
    n_visits <-
      i[, .(sex, year, value2, outcome)][, var := "count_n_ambulatory"]
    setnames(n_visits, "value2", "value")
    return(list(n_patients, n_visits))
    # }
  }
})



# Remove NULL elements from list, then flatten it by one level
dat_adm[sapply(dat_adm, is.null)] <- NULL
dat_adm <- unlist(dat_adm, recursive = FALSE)

dat_amb[sapply(dat_amb, is.null)] <- NULL
dat_amb <- unlist(dat_amb, recursive = FALSE)


# Remove admissions
is.wrong <- function(x) {
  inx1 <- "Antal indlagte patienter"  %in% colnames(x)
  inx2 <- "Antal ambulante besøg"  %in% colnames(x)
  inx <- inx1 | inx2
  inx
}

lapply(dat_tmp, is.wrong)
dat_old <- copy(dat_tmp)
dat_old[sapply(dat_old, is.wrong)] <- NULL
dat_old <- c(dat_old, dat_adm)
dat_old <- c(dat_old, dat_amb)

dat_old <- lapply(dat_old, function(x) {
  
  i <- copy(x)
  
  # Temporarily delete 2017 data
  i <- i[year!=2017]
  
  if("Antal indlagte patienter"  %in% colnames(i)){
    # If this is triggered, something is wrong
    browser()
    }
  i[sex == "Mænd", sex := "male"]
  i[sex == "Kvinder", sex := "female"]
  i[is.na(sex), sex := "total"]
  # Clean national level
  # 

  if (nrow(i) == 33) {
    i[, aggr := "national"]
    i[, grouping := year]
    setcolorder(i,
                c("outcome", "sex", "year", "grouping", "var", "aggr", "value"))
  }

  # Clean age-level
  else if (nrow(i) == 21 && any(grepl("85", i$grouping))) {
    # browser()
    i[, aggr := "age"]
    i[is.na(grouping), grouping := "total"]
    i[, grouping := gsub("-", " - ", grouping)]
    i[, grouping := gsub(" år", "", grouping)]
    setcolorder(i,
                c("outcome", "sex", "year", "grouping", "var", "aggr", "value"))
  }
  
  else if (nrow(i) == 18 && any(grepl("Hove", i$grouping))) {

    i[, aggr := "region"]
    i[is.na(grouping), grouping := "total"]
    setcolorder(i,
                c("outcome", "sex", "year", "grouping", "var", "aggr", "value"))
  }
  
  
  
  return(i)
})
dat_old <- rbindlist(dat_old)

# Find meaningful min and max for each aggr-outcome-variable combo
# dat_split <- split(dat_old, by = c("outcome","var","aggr"))
# lapply(dat_split, function(i) range(i$value))


# OBJECTS -----------------------------------------------------------------
data_path <- file.path(paste0("data/shiny_dat_", lang, ".rds"))
dat_new <- readRDS(file = data_path)
year_max <- 2016


ui_file_path <- file.path(paste0("ui/ui_", lang, ".R"))
source(ui_file_path, encoding = "UTF-8")
