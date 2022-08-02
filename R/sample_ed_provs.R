# Select a sample of providers that code ED activity above a threshold
# 871_health_effects_reduced_care_covid


# README----
# select a sample of providers that code ED activity above a threshold
# use this sample for all analyses of changes in diagnosis patterns;
# only use the full population of ED providers for initial analysis of trends


# packages----
library("tidyverse")
library("here")
library("data.table")
library("lubridate")


# functions----
source(here("R", "fx_utilities.R"))


# parameters----
code_th <- .8  # coding threshold




# 1 read data----
ed_dat <- fread(here("raw_data", "ed_diag_dat_20220719.csv"), na = c("NULL", "NA"))




# 2 clean----
ed_dat <- clean_raw_dat(ed_dat, ed = TRUE) |> filter(isoyrwk != "2022-13")




# 3 plot uncoded----
# proportion of activity uncoded
ed_dat |>
  filter(dataset == "ecds") |>
  mutate(miss_diag = ifelse(is.na(diagcd), "miss", "notmiss")) |>
  group_by(isoyrwk, miss_diag) |>
  summarise(n_visits = sum(n_visits)) |>
  ungroup() |>
  pivot_wider(names_from = miss_diag, values_from = n_visits) |>
  mutate(p_miss = miss / (miss + notmiss)) |>
  ggplot(aes(x = isoyrwk, y = p_miss, group = 1L)) +
  geom_line() +
  scale_x_discrete() +
  scale_y_continuous()

# proportion of activity uncoded in study periods
ed_dat |>
  filter(!is.na(tmper)) |>
  mutate(miss_diag = ifelse(is.na(diagcd), "miss", "notmiss")) |>
  group_by(tmper, isowk, miss_diag) |>
  summarise(n_visits = sum(n_visits)) |>
  ungroup() |>
  pivot_wider(names_from = miss_diag, values_from = n_visits) |>
  mutate(p_miss = miss / (miss + notmiss)) |>
  ggplot(aes(x = isowk, y = p_miss, group = tmper, color = tmper)) +
  geom_line() +
  geom_point() +
  scale_x_continuous() +
  scale_y_continuous()

# using procode (3 characters) there are 143 providers in the full sample
ed_dat |> select(procd) |> distinct() |> tally()




# 4 select sample----
# select providers that on aggregate code above threshold in both study periods
procd_th_plus <- ed_dat |>
  filter(!is.na(tmper)) |>
  mutate(miss_diag = ifelse(is.na(diagcd), "miss", "notmiss")) |>
  group_by(tmper, procd, miss_diag) |>
  summarise(n_visits = sum(n_visits)) |>
  group_by(tmper, procd) |>
  mutate(freq = n_visits / sum(n_visits)) |>
  filter(miss_diag == "notmiss") |>
  select(-n_visits) |>
  group_by(procd) |>
  summarise(miss_min = min(freq)) |>
  arrange(desc(miss_min)) |>
  filter(miss_min >= code_th) |>
  pull(procd)

# 83 (n = 143) providers above threshold
length(procd_th_plus)

# trends appear similar in both study periods for providers above/below threshold
ed_dat |>
  filter(!is.na(tmper)) |>
  mutate(thold = ifelse(procd %in% procd_th_plus, "above_th", "below_th")) |>
  group_by(tmper, isowk, thold) |>
  summarise(n_visits = sum(n_visits)) |>
  ggplot(aes(x = isowk, y = n_visits, group = thold, color = thold)) +
  geom_line() +
  geom_point() +
  scale_x_continuous() +
  scale_y_continuous() +
  facet_wrap(vars(tmper))

# for providers above threshold >90% coded per week;
# for providers below threshold c.60% coded per week
ed_dat |>
  filter(!is.na(tmper)) |>
  mutate(thold = ifelse(procd %in% procd_th_plus, "above_th", "below_th")) |>
  mutate(miss_diag = ifelse(is.na(diagcd), "miss", "notmiss")) |>
  group_by(tmper, isowk, thold, miss_diag) |>
  summarise(n_visits = sum(n_visits)) |>
  group_by(tmper, isowk, thold) |>
  mutate(freq = n_visits / sum(n_visits)) |>
  filter(miss_diag == "notmiss") |>
  ggplot(aes(x = isowk, y = freq, group = thold, color = thold)) +
  geom_line() +
  geom_point() +
  scale_x_continuous() +
  scale_y_continuous() +
  facet_wrap(vars(tmper))

# c.58% of total activity is in sample of providers that code above threshold
ed_dat |>
  filter(!is.na(tmper)) |>
  mutate(thold = ifelse(procd %in% procd_th_plus, "above_th", "below_th")) |>
  group_by(tmper, thold) |>
  summarise(n_visits = sum(n_visits)) |> 
  mutate(share = n_visits / sum(n_visits))




# 5 save----
saveRDS(procd_th_plus, here("data", "sample_provs_code_above_threshold.rds"))

