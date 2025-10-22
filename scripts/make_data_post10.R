library(tidyverse); library(here); library(readr)

df <- read_delim(
  here::here("data", "post10_data_raw.csv"),
  delim = ";",
  locale = locale(encoding = "ISO-8859-1"),
  col_names = FALSE
)

# Rename descriptor columns
df <- df |> rename(sex = X1, age = X2, sector = X3, measure = X4)

# Rename year columns
year_labels <- c("2017","2018","2019","2020","2021","2022","2023","2024")
names(df)[5:12] <- year_labels

df <- df |> 
  filter(sex != "Køn i alt",
         age != "Alder i alt",
         sector != "Uanset sygehusvæsen")  

# Hand translate the easy ones

df <- df |> 
  mutate(sex = str_replace_all(sex,"Mænd","Men"),
         sex = str_replace_all(sex,"Kvinder","Women"),
         age = str_replace_all(age,"år","years"),
         age = str_replace_all(age,"og derover",""),
         age = str_replace_all(age,"60 years","60+ years"),
         sector = str_replace_all(sector,"Somatik","Somatic"),
         sector = str_replace_all(sector,"Psykiatri","Psychiatry"),
         sector = str_replace_all(sector,"Både somatik og psykiatri","Both somatic and psychiatry")
  ) |> 
  mutate(across(where(is.character), as.factor)) # make all characters factors

# GPT aided translation of the measure
# trim & collapse whitespace
df <- df  |> mutate(measure = str_squish(measure)
)

# Define a named lookup (DA -> EN)
da_en <- c(
  "Personer med ophold (antal)"                          = "Persons with stays (number)",
  "Personer med ophold (pct.)"                           = "Persons with stays (percent)",
  "Ophold per person (antal)"                            = "Stays per person (number)",
  "Personer med ophold på under 12 timer (antal)"        = "Persons with stays under 12 hours (number)",
  "Personer med ophold på under 12 timer (pct.)"         = "Persons with stays under 12 hours (percent)",
  "Ophold på under 12 timer per person (antal)"          = "Stays under 12 hours per person (number)",
  "Personer med ophold på 12 timer eller derover (antal)"= "Persons with stays of 12 hours or more (number)",
  "Personer med ophold på 12 timer eller derover (pct.)" = "Persons with stays of 12 hours or more (percent)",
  "Ophold på 12 timer eller derover per person (antal)"  = "Stays of 12 hours or more per person (number)"
)

# Translate (keep originals that don’t match)
df <- df  |> 
  mutate(measure = recode(measure, !!!da_en, .default = measure))

# Pivot longer
df <- df |> 
  pivot_longer(
    cols = 5:12,
    names_to = "year",
    values_to = "value"
  ) |> 
  mutate(year = as.integer(year)) 

saveRDS(df, "C:/Users/henri/Documents/publicdataprojects/data/post10_data.rds")
