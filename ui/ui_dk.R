# INTRO -------------------------------------------------------------------
###########################
# Requires shiny_dat_* to be loaded - but will load this in global.R file before running this ui file
###########################

# Use hjertetal_code to merge names and descriptions of outcomes. This will be
# in seperate script run once - not on every launch.
outcome_descriptions <-
  fread(file = "data/outcome_descriptions.csv", encoding = "UTF-8")
# load(file = "data/variable_ui.Rdata")
variable_ui <-
  fread(file = "data/variable_ui.csv",
        encoding = "UTF-8",
        header = TRUE)
edu <- fread(file = "data/edu_description.csv", encoding = "UTF-8")


# Encode to native
outcome_descriptions <-
  outcome_descriptions[, lapply(.SD, enc2native)]
variable_ui <- variable_ui[, lapply(.SD, enc2native)]
edu <- edu[, lapply(.SD, enc2native)]


outcome_names_treatment <-
  merge(data.table(hjertetal_code = grep("b", names(dat_new), value = TRUE)),
        outcome_descriptions,
        by = "hjertetal_code")[, .(hjertetal_code, name_dk, name_en)]
colnames(outcome_names_treatment) <-
  c("hjertetal_code", "name_dk", "name_en")
outcome_names_med <-
  merge(data.table(hjertetal_code = grep("m", names(dat_new), value = TRUE)),
        outcome_descriptions,
        by = "hjertetal_code")[, .(hjertetal_code, name_dk, name_en)]
colnames(outcome_names_med) <-
  c("hjertetal_code", "name_dk", "name_en")
outcome_names_diag <-
  merge(data.table(hjertetal_code = grep("d", names(dat_new), value = TRUE)),
        outcome_descriptions,
        by = "hjertetal_code")[, .(hjertetal_code, name_dk, name_en)]
colnames(outcome_names_diag) <-
  c("hjertetal_code", "name_dk", "name_en")



outcomes_all <-
  rbind(outcome_names_diag,
        outcome_names_treatment,
        outcome_names_med)



outcome_choices <- c(list(
  "Sygdomme" = enc2utf8(outcome_names_diag$name_dk),
  "Behandling" = enc2utf8(outcome_names_treatment$name_dk),
  "Medicin" = enc2utf8(outcome_names_med$name_dk)
))


aggr_choices <-
  data.table(
    name_dk = c("Alder",
                "Uddannelse",
                "Region",
                "År"),
    name_dk_long = c("Aldersgruppe",
                     "Uddannelsesgruppe",
                     "Region",
                     "År"),
    name_ht = c("age", "edu", "region", "national")
  )
row.names(aggr_choices) <- aggr_choices$name_dk