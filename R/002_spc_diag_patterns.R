# SPC analysis of changes in diagnosis patterns
# 871_health_effects_reduced_care_covid


# README----
# examine changes in diagnosis patterns and identify those that significantly increased/decreased


# packages----
library("tidyverse")
library("here")
library("data.table")


# functions----
source(here("R", "fx_utilities.R"))


# parameters----
ed_n <- 400L
ip_n <- 100L




# 1 read data----
ed_dat <- fread(here("raw_data", "ed_diag_dat_20220719.csv"), na = c("NULL", "NA"))
ip_dat <- fread(here("raw_data", "ip_diag_dat_20220805.csv"), na = c("NULL", "NA"))

procd_th_plus <- readRDS(here("data", "sample_providers_code_above_threshold.rds"))




# 2 clean----
ed_dat <- clean_raw_dat(ed_dat, ed = TRUE) |> filter(isoyrwk != "2022-13")
ed_dat <- ed_dat |> filter(procd %in% procd_th_plus)

ip_dat <- clean_raw_dat(ip_dat) |> filter(isoyrwk != "2022-13")




# 3 spc analysis----
chg_ed <- spc_diagnosis_chg(ed_dat, diagnm = diagnm, sex = NULL, age = NULL, high = TRUE, n = ed_n)
# n = 147

chg_ip <- spc_diagnosis_chg(ip_dat, diagnm = diagl4nm, sex = NULL, age = NULL, high = TRUE, n = ip_n)

# n = 314
chg_ip <- chg_ip |>
  rename(diagnm = diagl4nm)

# subgroup analysis
# men
chg_ed_male <- spc_diagnosis_chg(ed_dat, diagnm = diagnm, sex = "m", age = "adult", high = TRUE, n = ed_n)
chg_ip_male <- spc_diagnosis_chg(ip_dat, diagnm = diagl4nm, sex = "m", age = "adult", high = TRUE, n = ip_n)
# women
chg_ed_female <- spc_diagnosis_chg(ed_dat, diagnm = diagnm, sex = "f", age = "adult", high = TRUE, n = ed_n)
chg_ip_female <- spc_diagnosis_chg(ip_dat, diagnm = diagl4nm, sex = "f", age = "adult", high = TRUE, n = ip_n)
# children
chg_ed_child <- spc_diagnosis_chg(ed_dat, diagnm = diagnm, sex = NULL, age = "child", high = TRUE, n = ed_n)
chg_ip_child <- spc_diagnosis_chg(ip_dat, diagnm = diagl4nm, sex = NULL, age = "child", high = TRUE, n = ip_n)




# 4 save----
saveRDS(chg_ed, here("data", "spc_ed_diags_increased.rds"))
saveRDS(chg_ip, here("data", "spc_ip_diags_increased.rds"))
