# Classify diagnoses where frequency significantly increased
# 871_health_effects_reduced_care_covid


# README----
# produce a classification/grouping for diagnoses that increased post-pandemic
# initial classification was produced by Simon Bourne using 871_diagnosis_grouper_app
# see file "_20220304-1627.rds"


# packages----
library("tidyverse")
library("here")




# 1 read data-----
class_dat <- readRDS(here("raw_data", "39dad988da2dcd6bc843975708c50065_20220304-1627.rds"))




# 2 wrangle----
class_dat <- class_dat %>%
  map(~ as_tibble(.x)) %>%
  map2(names(.), ~ add_column(.x, grpnm = rep(.y, nrow(.x)))) %>%
  bind_rows()

# consolidated list of initial group names as described by Simon Bourne
grpnms <- c(
  "Direct impact of COVID/suspected COVID",
  "Lack of GP services leading to late presentation of illness",
  "Lack of follow up for chronic conditions from GP/specialists during COVID restrictions leading to an emergency presentation",
  "Lack of follow up for chronic conditions from GP/specialists during COVID restrictions",
  "Lack of face-to-face GP services for children available during COVID restrictions",
  "Reduced availability of emergency eye services",
  "Reduction in availability of antenatal services during COVID restrictions",
  "Lack of usual follow up after surgical procedures during COVID restrictions",
  "Increased alcohol use during COVID restrictions",
  "Increased isolation of vulnerable older people during COVID restrictions",
  "Increased sedentary lifestyles associated with COVID restrictions",
  "Increased domestic violence associated with COVID restrictions",
  "Mental health impact of restrictions associated with COVID"
)

# table(class_dat$grpnm)
# consolidate duplicate group names 
class_dat <- class_dat |>
  mutate(grpnm = str_remove(grpnm, "new_diagnosis_group_\\d+_"), grpnm = trimws(grpnm)) |>
  mutate(grpnm = case_when(
    grpnm %in% c("Severe COVID-related disease", "Severe COVID-related illness") ~ "Direct impact of COVID/suspected COVID",
    grpnm %in% c("Lack of acute GP services for children due to COVID restrictions", "Lack of GP services available for children during COVID restrictions") ~ "Lack of face-to-face GP services for children available during COVID restrictions",
    grpnm %in% c("Increase in sedentary behaviour associated with COVID") ~ "Increased sedentary lifestyles associated with COVID restrictions",
    grpnm %in% c("Direct COVID-related", "Direct impact of COVID/suspected COVID", "Impact of COVID/suspected COVID") ~ "Direct impact of COVID/suspected COVID",
    grpnm %in% c("Lack of follow up for chronic conditions from GP/specialists during COVID restrictions leading to acute illness") ~ "Lack of follow up for chronic conditions from GP/specialists during COVID restrictions leading to an emergency presentation",
    TRUE ~ grpnm
  ))

# lookup for abbreviated group names
grpnms_lookup <- tribble(
  ~ oldnm, ~ newnm,
  "Direct impact of COVID/suspected COVID",
  "covid-19",
  
  "Lack of GP services leading to late presentation of illness",
  "primary care late presentation",
  
  "Lack of follow up for chronic conditions from GP/specialists during COVID restrictions",
  "primary care fup ltc",
  
  "Lack of follow up for chronic conditions from GP/specialists during COVID restrictions leading to an emergency presentation",
  "primary care fup ltc - emergency",
  
  "Lack of face-to-face GP services for children available during COVID restrictions",
  "primary care children",
  
  "Reduced availability of emergency eye services",
  "emergency eye services",
  
  "Reduction in availability of antenatal services during COVID restrictions",
  "antenatal services",
  
  "Increased isolation of vulnerable older people during COVID restrictions",
  "isolation older people",
  
  "Lack of usual follow up after surgical procedures during COVID restrictions",
  "surgical fup",
  
  "Mental health impact of restrictions associated with COVID",
  "mental health",
  
  "Increased alcohol use during COVID restrictions",
  "alcohol",
  
  "Increased domestic violence associated with COVID restrictions",
  "domestic violence",
  
  "Increased sedentary lifestyles associated with COVID restrictions",
  "sedentary lifestyles"
)

# ordering for abbreviated group names 
grpnms_fct <- c(
  "covid-19",
  "primary care late presentation",
  "primary care fup ltc",
  "primary care fup ltc - emergency",
  "primary care children",
  "emergency eye services",
  "antenatal services",
  "isolation older people",
  "surgical fup",
  "mental health",
  "alcohol",
  "domestic violence",
  "sedentary lifestyles"
)

# remove duplicates
# class_dat[duplicated(class_dat), ] |> arrange(value)
# class_dat |> filter(!grpnm %in% grpnms)
class_dat <- distinct(class_dat) 

class_dat <- class_dat |>
  mutate(
    u = str_extract(value, "\\([^()]+\\)$"),
    u = as.double(str_extract(u, "(?<=^\\()[0-9\\.]*")),
    p2 = str_extract(value, "\\([^()]+\\)$"),
    p2 = str_extract(p2, "(?<=\\s)[0-9,]*"),
    pod = case_when(
      str_detect(value, "^([A-Z0-9]{4}:)") ~ "ip", TRUE ~ "ed"
    ),
    diagnm = str_remove(value, "[[:space:]]\\([^()]+\\)$")
  ) |>
  left_join(grpnms_lookup, by = c("grpnm" = "oldnm")) |>
  mutate(newnm = factor(newnm, levels = grpnms_fct)) |>
  select(pod, newnm, diagnm, u, p2) |>
  rename(grpnm = newnm) |>
  arrange(grpnm, pod, -u)




# 3 save----
saveRDS(class_dat, here("data", "initial_diagnosis_grping.rds"))

