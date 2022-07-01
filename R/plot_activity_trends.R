# Create plots showing trends in emergency care activity  
# 871_health_effects_reduced_care_covid


# README----
# create trend plots of ED visits and emergency admissions for report


# packages----
library("tidyverse")
library("here")
library("data.table")
library("readxl")
library("lubridate")


# functions ----
source(here("R", "fx_utilities.R"))
source(here("R", "plot_setup_and_parameters.R"))



# 1 read data----
ed_dat <- fread(
  here("raw_data", "ed_diag_dat_20220516.csv"),
  na = c("NULL", "NA"))

ip_dat <- fread(
  here("raw_data", "ip_diag_dat_20220517.csv"),
  na = c("NULL", "NA"))

beds_dat <- read_xlsx(
  here("raw_data", "covid_daily_bed_occ_series.xlsx"),
  sheet = "Sheet1",
  range = "A1:C609",
  col_types = c("text", "date", "numeric")
)

procd_th_plus <- readRDS(here("data", "sample_provs_code_above_threshold.rds"))




# 2 clean----
ed_dat <- clean_raw_dat(ed_dat, procd = TRUE) |> filter(isoyrwk != "2022-13")
ip_dat <- clean_raw_dat(ip_dat) |> filter(isoyrwk != "2022-13")

beds_dat <- beds_dat |>
  mutate(
    date = as.Date(date),
    beds = as.integer(beds),
    isoyr = isoyear(date),
    isowk = isoweek(date)
  ) |> 
  unite(isoyrwk, c(isoyr, isowk), sep = "-", remove = FALSE) |>
  mutate(isoyrwk = paste0(
    str_extract(isoyrwk, "^[[:digit:]]{4}-"),
    str_pad(str_extract(isoyrwk, "[[:digit:]]+$"), 2, pad = "0")
  )) |>
  group_by(isoyrwk) |>
  # take beds on last day of ISO week (better measure than mean?)
  filter(row_number() == n()) |> 
  ungroup() |> 
  select(beds, isoyrwk)

# pad beds series to match ED series
vec_date         <- seq(date("2018-01-01"), date("2022-03-31"), by = "weeks")
vec_date_iso     <- paste0(isoyear(vec_date), "-", isoweek(vec_date))
vec_date_iso_fmt <- paste0(
  str_extract(vec_date_iso, "^[[:digit:]]{4}-"),
  str_pad(str_extract(vec_date_iso, "[[:digit:]]+$"), 2, pad = "0"))

beds_dat <- tibble(isoyrwk = vec_date_iso_fmt) |>
  left_join(beds_dat, by = "isoyrwk")




# p1 trend visits and admis----
trend_ed <- ed_dat |>
  group_by(isoyrwk) |>
  summarise(n_visits = sum(n_visits)) |>
  ggplot(aes(x = isoyrwk, y = n_visits, group = 1)) +
  geom_line(size = .7, color = pal[2]) +
  scale_x_discrete(name = NULL, breaks = ts_breaks, labels = ts_labels, expand = c(0, 0)) +
  scale_y_continuous(name = NULL, limits = c(1e5, 4e5), labels = label_comma(scale = .001), expand = c(0, 0)) +
  labs(subtitle = "Weekly ED visits (thousands)")

trend_ip <- ip_dat |>
  group_by(isoyrwk) |>
  summarise(n_admi = sum(n_admi)) |>
  ggplot(aes(x = isoyrwk, y = n_admi, group = 1)) +
  geom_line(size = .7, color = pal[8]) +
  scale_x_discrete(name = NULL, breaks = ts_breaks, labels = ts_labels, expand = c(0, 0)) +
  scale_y_continuous(name = NULL, limits = c(.5e5, 1.50e5), labels = label_comma(scale = .001), expand = c(0, 0)) +
  labs(subtitle = "Weekly emergency admissions (thousands)")

trend_2 <- trend_ed + theme(plot.margin = margin(r = 4, unit = "mm")) + trend_ip
trend_2 <- trend_2 + plot_annotation(
  caption = "Source: SUS+, National Commissioning Data Repository.",
  title = "The COVID-19 pandemic caused major disruption to emergency\nhealthcare services"
)

ggsave(here("figures", "trend_visits_and_admis.png"), trend_2, width = 144, height = 100, units = c("mm"))




# p2 trend visits and bed occ----
trend_beds_occ <- beds_dat |> 
  filter(str_detect(isoyrwk, "^202")) |> 
  ggplot(aes(x = isoyrwk, y = beds, group = 1)) +
  geom_line(size = .7, color = pal[7]) +
  scale_x_discrete(name = NULL, breaks = ts_breaks, labels = ts_labels, expand = c(0, 0)) +
  scale_y_continuous(name = NULL, limits = c(0, 4e4), labels = label_comma(scale = .001), expand = c(0, 0)) +
  labs(subtitle = "Beds occ. COVID-19 patients (thousands)")

trend_ed_f2020 <- ed_dat |>
  filter(isoyr >= 2020) |>
  group_by(isoyrwk) |>
  summarise(n_visits = sum(n_visits)) |>
  ggplot(aes(x = isoyrwk, y = n_visits, group = 1)) +
  geom_line(size = .7, color = pal[2]) +
  scale_x_discrete(name = NULL, breaks = ts_breaks, labels = ts_labels, expand = c(0, 0)) +
  scale_y_continuous(name = NULL, limits = c(1e5, 4e5), labels = label_comma(scale = .001), expand = c(0, 0)) +
  labs(subtitle = "Weekly ED visits (thousands)")

trend_2 <- trend_ed_f2020 + theme(plot.margin = margin(r = 4, unit = "mm")) + trend_beds_occ
trend_2 <- trend_2 + plot_annotation(
  caption = "Source: SUS+, NCDR; COVID-19 daily situation reports, NHSE.",
  title = "The biggest falls in ED visits occured when Covid admissions were\nhighest"
)

ggsave(here("figures", "trend_visits_and_bed_occ.png"), trend_2, width = 144, height = 100, units = c("mm"))




# p3 trend visits w. study periods----
ref_index <- ref_index |>
  mutate(ymax = 2.6e5L)

lab_dat <- tribble(
  ~arrmode, ~isoyrwk, ~n, ~lab,
  "walkin", "2018-01", 2.4e5, "walk-in",
  "amb", "2018-01", 1.1e5, "ambulance"
)

ed_blocks <- ed_dat |>
  group_by(isoyrwk, arrmode) |>
  summarise(n_visits = sum(n_visits)) |>
  ggplot(aes(x = isoyrwk, y = n_visits, group = arrmode, color = arrmode)) +
  geom_rect(aes(xmin = wk_min, xmax = wk_max, ymin = ymin, ymax = ymax, group = tmper, fill = tmper),
    data = ref_index,
    alpha = .4, color = NA,
    show.legend = FALSE, inherit.aes = FALSE
  ) +
  geom_line(size = .7, show.legend = FALSE) +
  geom_text(aes(x = isoyrwk, y = n, group = arrmode, color = arrmode, label = lab),
            data = lab_dat,
            show.legend = FALSE,
            hjust = 0, vjust = .5) +
  scale_x_discrete(name = NULL, breaks = ts_breaks, labels = ts_labels, expand = c(0, 0)) +
  scale_y_continuous(name = NULL, limits = c(0, 2.6e5), labels = label_comma(scale = .001), expand = c(0, 0)) +
  scale_color_manual(values = pal[c(1, 3)]) +
  scale_fill_manual(values = pal[c(7, 8)]) +
  labs(
    caption = "Source: SUS+, National Commissioning Data Repository.",
    subtitle = "Weekly ED visits (thousands)",
    title = "By Summer 2021 ED visits had returned to pre-pandemic levels"
  )

ggsave(here("figures", "trend_visits_blocked.png"), ed_blocks, width = 144, height = 100, units = c("mm"))




# p4 visits cf. study periods----
ed_bar <- ed_dat |>
  filter(!is.na(tmper)) |>
  group_by(tmper) |>
  summarise(n_visits = sum(n_visits)) |>
  ggplot(aes(x = tmper, y = n_visits, fill = tmper)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_x_discrete(name = NULL, labels = c("w25\u201343 2019", "w25\u201343 2021")) +
  scale_y_continuous(name = NULL, limits = c(0, 7e6), expand = c(0, 0), labels = label_comma(scale = .000001)) +
  scale_fill_manual(values = pal[c(7, 8)]) +
  labs(subtitle = "ED visits (millions)")

lab_dat <- tribble(
  ~isoyr, ~isowk, ~n, ~lab,
  "2021", 24L, 3.45e5, "2021",
  "2019", 24L, 3.4e5, "2019"
  )

ed_ts <- ed_dat |>
  filter(!is.na(tmper)) |>
  group_by(isoyr, isowk) |>
  summarise(n_visits = sum(n_visits)) |> 
  mutate(isoyr = as.character(isoyr)) |>
  ggplot(aes(x = isowk, y = n_visits, group = isoyr, color = isoyr)) +
  geom_line(size = .7, show.legend = FALSE) +
  geom_text(aes(x = isowk, y = n, group = isoyr, color = isoyr, label = lab),
            data = lab_dat,
            show.legend = FALSE,
            hjust = 0, vjust = .5) +
  scale_x_continuous(name = NULL, expand = c(0, 1)) +
  scale_y_continuous(name = NULL, limits = c(2.7e5, 3.5e5), expand = c(0, 0), labels = label_comma(scale = .001)) +
  scale_color_manual(values = pal[c(7, 8)]) +
  labs(subtitle = "Weekly ED visits (thousands)")

ed_compare <- ed_bar + theme(plot.margin = margin(r = 5, unit = "mm")) + ed_ts
ed_compare <- ed_compare + plot_annotation(
  caption = "Source: SUS+, National Commissioning Data Repository.",
  title = "ED visit numbers in weeks 25\u201343 in 2021 were similar to their\npre-pandemic numbers"
)

ggsave(here("figures", "visits_cf_study_periods.png"), ed_compare, width = 144, height = 100, units = c("mm"))




# p5 trend admis w. study periods----
ref_index <- ref_index |> 
  mutate(ymin = .5e5L, ymax = 1.5e5L)

ip_blocks <- ip_dat |>
  group_by(isoyrwk) |>
  summarise(n_admi = sum(n_admi)) |>
  ggplot(aes(x = isoyrwk, y = n_admi, group = 1)) +
  geom_rect(aes(xmin = wk_min, xmax = wk_max, ymin = ymin, ymax = ymax, group = tmper, fill = tmper),
    data = ref_index,
    alpha = .4, color = NA,
    inherit.aes = FALSE, show.legend = FALSE
  ) +
  geom_line(size = .7, show.legend = FALSE, color = pal[3]) +
  scale_x_discrete(name = NULL, breaks = ts_breaks, labels = ts_labels, expand = c(0, 0)) +
  scale_y_continuous(name = NULL, limits = c(.5e5, 1.5e5), labels = label_comma(scale = .001), expand = c(0, 0)) +
  scale_fill_manual(values = pal[c(7, 8)]) +
  labs(
    caption = "Source: SUS+, National Commissioning Data Repository.",
    subtitle = "Weekly emergency admissions (thousands)",
    title = "By Summer 2021 emergency admissions had almost recovered\nto pre-pandemic levels"
  )

ggsave(here("figures", "trend_admis_blocked.png"), ip_blocks, width = 144, height = 100, units = c("mm"))




# p6 admis cf. study periods----
ip_bar <- ip_dat |>
  filter(!is.na(tmper)) |>
  group_by(tmper) |>
  summarise(n_admi = sum(n_admi)) |>
  ggplot(aes(x = tmper, y = n_admi, fill = tmper)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_x_discrete(name = NULL, labels = c("w25\u201343 2019", "w25\u201343 2021")) +
  scale_y_continuous(name = NULL, expand = c(0, 0), labels = label_comma(scale = .000001)) +
  scale_fill_manual(values = pal[c(7, 8)]) +
  labs(subtitle = "Emergency admissions (millions)")

lab_dat <- tribble(
  ~isoyr, ~isowk, ~n, ~lab,
  "2021", 24L, 1.32e5, "2021",
  "2019", 24L, 1.36e5, "2019"
)

ip_ts <- ip_dat |>
  filter(!is.na(tmper)) |>
  group_by(isoyr, isowk) |>
  summarise(n_admi = sum(n_admi)) |>
  mutate(isoyr = as.character(isoyr)) |>
  ggplot(aes(x = isowk, y = n_admi, group = isoyr, color = isoyr)) +
  geom_line(size = .7, show.legend = FALSE) +
  geom_text(aes(x = isowk, y = n, group = isoyr, color = isoyr, label = lab),
            data = lab_dat,
            show.legend = FALSE,
            hjust = 0, vjust = .5) +
  scale_x_continuous(name = NULL, expand = c(0, 1)) +
  scale_y_continuous(name = NULL, limits = c(1e5, 1.4e5), expand = c(0, 0), labels = label_comma(scale = .001)) +
  scale_color_manual(values = pal[c(7, 8)]) +
  labs(subtitle = "Emergency admissions (thousands)")

ip_compare <- ip_bar + theme(plot.margin = margin(r = 5, unit = "mm")) + ip_ts
ip_compare <- ip_compare + plot_annotation(
  caption = "Source: SUS+, National Commissioning Data Repository.",
  title = "Emergency admissions in weeks 25\u201343 in 2021 were 4.6% below\ntheir pre-pandemic level"
)

ggsave(here("figures", "admis_cf_study_periods.png"), ip_compare, width = 144, height = 100, units = c("mm"))




# p7 sample provs cf. study periods----
bar_dat <- ed_dat |>
  filter(!is.na(tmper)) |> 
  mutate(thold = ifelse(procd %in% procd_th_plus, "above_th", "below_th")) |>
  group_by(tmper, thold) |>
  summarise(n_visits = sum(n_visits)) |> 
  ungroup()

ed_bar <- bar_dat |> 
  ggplot(aes(x = tmper, y = n_visits, group = tmper, fill = tmper)) +
  geom_bar(aes(x = tmper, y = n_visits, group = thold, fill = thold),
           stat = "identity",
           position = "dodge",
           show.legend = FALSE) +
  scale_x_discrete(name = NULL, labels = c("w25\u201343 2019", "w25\u201343 2021")) +
  scale_y_continuous(name = NULL, limits = c(0, 4e6), expand = c(0, 0), labels = label_comma(scale = .000001)) +
  scale_fill_manual(values = pal[c(7, 8)]) +
  labs(subtitle = "ED visits (millions)")

lab_dat <- tribble(
  ~isoyr_th, ~isowk, ~n, ~lab,
  "2021_above_th", 24L, 2e5, "2021_above_th",
  "2019_above_th", 24L, 2e5, "2019_above_th",
  "2021_below_th", 24L, 1.4e5, "2021_below_th",
  "2019_below_th", 24L, 1.4e5, "2019_below_th"
)

ts_dat <- ed_dat |>
  filter(!is.na(tmper)) |> 
  mutate(thold = ifelse(procd %in% procd_th_plus, "above_th", "below_th")) |> 
  unite(isoyr_th, c(isoyr, thold), sep = "_", remove = FALSE) |> 
  group_by(isoyr_th, isoyr, isowk) |>
  summarise(n_visits = sum(n_visits)) |> 
  ungroup()

ed_ts <- ts_dat |>
  ggplot(aes(x = isowk, y = n_visits, group = isoyr_th, color = isoyr_th)) +
  geom_line(size = .7, show.legend = FALSE) +
  geom_text(aes(x = isowk, y = n, group = isoyr_th, color = isoyr_th, label = lab),
            data = lab_dat,
            show.legend = FALSE,
            hjust = 0, vjust = .5) +
  scale_x_continuous(name = NULL, expand = c(0, 1)) +
  scale_y_continuous(name = NULL, limits = c(.5e5, 2.2e5), expand = c(0, 0), labels = label_comma(scale = .001)) +
  scale_color_manual(values = pal[c(5:8)]) +
  labs(subtitle = "Weekly ED visits (thousands)")

ed_compare <- ed_bar + theme(plot.margin = margin(r = 5, unit = "mm")) + ed_ts
ed_compare <- ed_compare + plot_annotation(
  caption = "Source: SUS+, National Commissioning Data Repository.",
  title = "ED visit numbers in weeks 25\u201343 in 2021 were similar to their\npre-pandemic numbers"
)

ggsave(here("figures", "sample_provs_cf_study_periods.png"), ed_compare, width = 144, height = 100, units = c("mm"))

