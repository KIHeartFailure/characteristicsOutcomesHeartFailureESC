
ProjectTemplate::reload.project(data_loading = FALSE)

# Import data from SAS ----------------------------------------------------

pathesc <- "C:/Users/Lina/STATISTIK/Projects/ESC registry/data"

esc <- read_sas(paste0(pathesc, "/hf3_lt_fu_data_soladis_jan19.sas7bdat"), 
                paste0(pathesc, "/formats.sas7bcat"))


# Fix labels ect. ---------------------------------------------------------

esc <- esc %>%
  mutate_if(~ class(.) == "haven_labelled", ~ haven::as_factor(.)) %>%
  mutate_if(is.factor, ~ droplevels(., exclude = "Unknown")) %>%
  mutate_if(is.factor, factor) %>% ## Something odd with num_opRyth (two diff kinds of NA). This fixes it
  mutate(num_dmHepa = recode(num_dmHepa, "A" = "Yes", .default = levels(num_dmHepa))) # uses format for other hep variable. this fixes it

# Store as RData in /data folder ------------------------------------------

save(file = "./data/esc.RData", list = c("esc"))