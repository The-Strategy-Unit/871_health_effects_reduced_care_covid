# Create plots showing activity trends
# 871_health_effects_reduced_care_covid


# README----
# create plots showing activity trends for report


# packages----
library("tidyverse")
library("here")
library("data.table")
library("readxl")
library("lubridate")


# functions ----
source(here("R", "fx_utilities.R"))
source(here("R", "005_plot_setup.R"))



# 1 read data----
ed_dat <- fread(
  here("raw_data", "ed_diag_dat_20220719.csv"),
  na = c("NULL", "NA")
)

ip_dat <- fread(
  here("raw_data", "ip_diag_dat_20220805.csv"),
  na = c("NULL", "NA")
)

admigrp_dat <- fread(
  here("raw_data", "admigrp_dat.csv"),
  na = c("NULL", "NA")
)

beds_dat <- read_xlsx(
  here("raw_data", "covid_daily_bed_occ_series.xlsx"),
  sheet = "Sheet1",
  range = "A1:C609",
  col_types = c("text", "date", "numeric")
)

gp_dat <- read_xlsx(
  here("raw_data", "gp_appt_series.xlsx"),
  sheet = "dat",
  na = c("NULL", "NA", "-"),
  col_types = c("date", rep("numeric", 26L))
)

procd_th_plus <- readRDS(here("data", "sample_providers_code_above_threshold.rds"))




# 2 clean----
ed_dat <- clean_raw_dat(ed_dat, ed = TRUE) |> filter(isoyrwk != "2022-13")
ip_dat <- clean_raw_dat(ip_dat) |> filter(isoyrwk != "2022-13")
admigrp_dat <- admigrp_dat |>
  unite(isoyrwk, c(isoyr, isowk), sep = "-", remove = FALSE) |>
  mutate(isoyrwk = paste0(
    str_extract(isoyrwk, "^[[:digit:]]{4}-"),
    str_pad(str_extract(isoyrwk, "[[:digit:]]+$"), 2, pad = "0")
  )) |>
  filter(isoyrwk != "2022-13")

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
vec_date <- seq(date("2018-01-01"), date("2022-03-31"), by = "weeks")
vec_date_iso <- paste0(isoyear(vec_date), "-", isoweek(vec_date))
vec_date_iso_fmt <- paste0(
  str_extract(vec_date_iso, "^[[:digit:]]{4}-"),
  str_pad(str_extract(vec_date_iso, "[[:digit:]]+$"), 2, pad = "0")
)

beds_dat <- tibble(isoyrwk = vec_date_iso_fmt) |>
  left_join(beds_dat, by = "isoyrwk")

gp_dat <- gp_dat |>
  mutate(dt = as.character(as.Date(dt))) |>
  select(dt, apps, eng_apps, f2f, home, tel, video, mode_unk) |>
  filter(!dt %in% c("2017-11-01", "2017-12-01", "2022-04-01", "2022-05-01", "2022-06-01", "2022-07-01")) |>
  pivot_longer(cols = apps:mode_unk, names_to = "series", values_to = "apps")




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
  title = "Fig 3: Emergency care services were heavily disrupted during the\nfirst 18 months of the pandemic"
)

ggsave(here("figures", "trend_visits_and_admis.png"), trend_2, width = 144, height = 100, units = c("mm"))




# p2 trend bed occ----
trend_bed_occ <- beds_dat |>
  filter(str_detect(isoyrwk, "^202")) |>
  ggplot(aes(x = isoyrwk, y = beds, group = 1)) +
  geom_line(size = .7, color = pal[7]) +
  scale_x_discrete(name = NULL, breaks = ts_breaks, labels = ts_labels, expand = c(0, 0)) +
  scale_y_continuous(name = NULL, limits = c(0, 4e4), labels = label_comma(scale = .001), expand = c(0, 0)) +
  labs(subtitle = "Beds occ. Covid-19 patients (thousands)")

trend_2 <- trend_bed_occ + theme(plot.margin = margin(r = 4, unit = "mm")) + plot_spacer()
trend_2 <- trend_2 + plot_annotation(
  caption = "Source: COVID-19 daily situation reports, NHSE.",
  title = "Fig 4: By mid-2021, vaccines meant fewer beds occupied by Covid\npatients and more capacity for treating non-Covid patients"
)

ggsave(here("figures", "trend_bed_occ.png"), trend_2, width = 144, height = 100, units = c("mm"))




# p3 trend visits blocked----
ref_index <- ref_index |>
  mutate(ymax = 2.6e5L)

lab_dat <- tribble(
  ~arrmode, ~isoyrwk, ~n, ~lab,
  "walkin", "2018-01", 2.4e5, "walk-in",
  "amb", "2018-01", 1.1e5, "ambulance"
)

ed_block <- ed_dat |>
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
    hjust = 0, vjust = .5
  ) +
  annotate("text", x = "2019-34", y = 3 * max(ed_dat$n_visits), label = "w25\u201343", color = "#686f73", size = 8 / 2.8, hjust = .5) +
  annotate("text", x = "2021-34", y = 3 * max(ed_dat$n_visits), label = "w25\u201343", color = "#686f73", size = 8 / 2.8, hjust = .5) +
  scale_x_discrete(name = NULL, breaks = ts_breaks, labels = ts_labels, expand = c(0, 0)) +
  scale_y_continuous(name = NULL, limits = c(0, 2.6e5), labels = label_comma(scale = .001), expand = c(0, 0)) +
  scale_color_manual(values = pal[c(1, 2)]) +
  scale_fill_manual(values = pal[c(7, 8)])

ed_blocks <- ed_block +
  labs(
    caption = "Source: SUS+, National Commissioning Data Repository.",
    subtitle = "Weekly ED visits (thousands)",
    title = "Fig 5: By Summer 2021, ED visits had returned to their pre-pandemic\nlevel"
  )

ggsave(here("figures", "trend_visits_blocked.png"), ed_blocks, width = 144, height = 100, units = c("mm"))




# p4 visits compared----
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
    hjust = 0, vjust = .5
  ) +
  scale_x_continuous(name = NULL, expand = c(0, 1)) +
  scale_y_continuous(name = NULL, limits = c(2.7e5, 3.5e5), expand = c(0, 0), labels = label_comma(scale = .001)) +
  scale_color_manual(values = pal[c(7, 8)]) +
  labs(subtitle = "Weekly ED visits (thousands)")

ed_compare <- ed_bar + theme(plot.margin = margin(r = 5, unit = "mm")) + ed_ts
ed_compare <- ed_compare + plot_annotation(
  caption = "Source: SUS+, National Commissioning Data Repository.",
  title = "Fig 6: ED visit volumes in weeks 25\u201343 in 2021 were similar to the \nsame period in 2019"
)

ggsave(here("figures", "trend_visits_compared.png"), ed_compare, width = 144, height = 100, units = c("mm"))




# p5 trend admis blocked----
ref_index <- ref_index |>
  mutate(ymax = 1.5e5L)

ip_block <- ip_dat |>
  group_by(isoyrwk) |>
  summarise(n_admi = sum(n_admi)) |>
  ggplot(aes(x = isoyrwk, y = n_admi, group = 1)) +
  geom_rect(aes(xmin = wk_min, xmax = wk_max, ymin = ymin, ymax = ymax, group = tmper, fill = tmper),
    data = ref_index,
    alpha = .4, color = NA,
    inherit.aes = FALSE, show.legend = FALSE
  ) +
  geom_line(size = .7, show.legend = FALSE, color = pal[2]) +
  annotate("text", x = "2019-34", y = .8 * max(ip_dat$n_admi), label = "w25\u201343", color = "#686f73", size = 8 / 2.8, hjust = .5) +
  annotate("text", x = "2021-34", y = .8 * max(ip_dat$n_admi), label = "w25\u201343", color = "#686f73", size = 8 / 2.8, hjust = .5) +
  scale_x_discrete(name = NULL, breaks = ts_breaks, labels = ts_labels, expand = c(0, 0)) +
  scale_y_continuous(name = NULL, limits = c(0, 1.5e5), labels = label_comma(scale = .001), expand = c(0, 0)) +
  scale_fill_manual(values = pal[c(7, 8)])

ip_blocks <- ip_block +
  labs(
    caption = "Source: SUS+, National Commissioning Data Repository.",
    subtitle = "Weekly emergency admissions (thousands)",
    title = "Fig 7: By Summer 2021, emergency admissions had recovered close\nto their pre-pandemic level"
  )

ggsave(here("figures", "trend_admis_blocked.png"), ip_blocks, width = 144, height = 100, units = c("mm"))




# p6 admis compared----
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
    hjust = 0, vjust = .5
  ) +
  scale_x_continuous(name = NULL, expand = c(0, 1)) +
  scale_y_continuous(name = NULL, limits = c(1e5, 1.4e5), expand = c(0, 0), labels = label_comma(scale = .001)) +
  scale_color_manual(values = pal[c(7, 8)]) +
  labs(subtitle = "Emergency admissions (thousands)")

ip_compare <- ip_bar + theme(plot.margin = margin(r = 5, unit = "mm")) + ip_ts
ip_compare <- ip_compare + plot_annotation(
  caption = "Source: SUS+, National Commissioning Data Repository.",
  title = "Fig 8: Emergency admissions in weeks 25\u201343 in 2021 were 4.6%\nbelow their level in 2019"
)

ggsave(here("figures", "trend_admis_compared.png"), ip_compare, width = 144, height = 100, units = c("mm"))




# p7 sample provs compared----
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
    show.legend = FALSE
  ) +
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
    hjust = 0, vjust = .5
  ) +
  scale_x_continuous(name = NULL, expand = c(0, 1)) +
  scale_y_continuous(name = NULL, limits = c(.5e5, 2.2e5), expand = c(0, 0), labels = label_comma(scale = .001)) +
  scale_color_manual(values = pal[c(3, 7, 4, 8)]) +
  labs(subtitle = "Weekly ED visits (thousands)")

ed_compare <- ed_bar + theme(plot.margin = margin(r = 5, unit = "mm")) + ed_ts
ed_compare <- ed_compare + plot_annotation(
  caption = "Source: SUS+, National Commissioning Data Repository.",
  title = "ED visit numbers in weeks 25\u201343 in 2021 were similar to their\npre-pandemic numbers"
)

ggsave(here("figures", "trend_sample_providers_compared.png"), ed_compare, width = 144, height = 100, units = c("mm"))




# p8 trend elective admis ----
trend_ordelec <- admigrp_dat |>
  filter(admigrp %in% c("daycase", "ordelec", "paeds-elec")) |>
  mutate(admigrp = case_when(admigrp == "paeds-elec" ~ "ordelec", TRUE ~ admigrp)) |>
  group_by(isoyrwk, admigrp) |>
  summarise(n_admi = sum(n_admi)) |>
  filter(admigrp == "ordelec") |>
  ggplot(aes(x = isoyrwk, y = n_admi, group = 1)) +
  geom_line(size = .7, color = pal[2]) +
  scale_x_discrete(name = NULL, breaks = ts_breaks, labels = ts_labels, expand = c(0, 0)) +
  scale_y_continuous(name = NULL, limits = c(0e4, 3e4), labels = label_comma(scale = .001), expand = c(0, 0)) +
  labs(subtitle = "Weekly elective admissions (thousands)")

trend_dc <- admigrp_dat |>
  filter(admigrp %in% c("daycase", "ordelec", "paeds-elec")) |>
  mutate(admigrp = case_when(admigrp == "paeds-elec" ~ "ordelec", TRUE ~ admigrp)) |>
  group_by(isoyrwk, admigrp) |>
  summarise(n_admi = sum(n_admi)) |>
  filter(admigrp == "daycase") |>
  ggplot(aes(x = isoyrwk, y = n_admi, group = 1)) +
  geom_line(size = .7, color = pal[8]) +
  scale_x_discrete(name = NULL, breaks = ts_breaks, labels = ts_labels, expand = c(0, 0)) +
  scale_y_continuous(name = NULL, limits = c(0e4, 2e5), labels = label_comma(scale = .001), expand = c(0, 0)) +
  labs(subtitle = "Weekly daycase admissions (thousands)")

trend_2 <- trend_ordelec + theme(plot.margin = margin(r = 4, unit = "mm")) + trend_dc
trend_2 <- trend_2 + plot_annotation(
  caption = "Source: SUS+, National Commissioning Data Repository.",
  title = "Fig 1: Elective activity was reduced as part of a deliberate plan to\npreserve capacity for Covid patients, but has been slow to recover"
)

ggsave(here("figures", "trend_elec_and_daycase.png"), trend_2, width = 144, height = 100, units = c("mm"))




# p9 trend GP apps ----
ts_breaks <- str_c(seq(2018, 2022, 1L), "-01-01")
ts_labels <- str_c(seq(2018, 2022, 1L))

trend_gp <- gp_dat |> 
  filter(series %in% c("eng_apps", "f2f", "tel")) |> 
  mutate(
    dt = as.character(dt),
    slabel = case_when(series == "eng_apps" ~ "total", series == "f2f" ~ "f2f", series == "tel" ~ "tel", TRUE ~ series)
  ) |>
  ggplot(aes(x = dt, y = apps, group = series, color = series)) +
  geom_line(size = .7, show.legend = FALSE) +
  geom_dl(aes(label = slabel), method = list(dl.trans(y = y + .2), "first.points", hjust = 0)) +
  scale_x_discrete(name = NULL, breaks = ts_breaks, labels = ts_labels, expand = c(0, 0)) +
  scale_y_continuous(name = NULL, limits = c(0e6, 32e6), labels = label_comma(scale = .000001), expand = c(0, 0)) +
  scale_color_manual(values = pal[c(5, 2, 3)]) +
  labs(subtitle = "Monthly GP appointments (millions)")

f2f_gp <- gp_dat |> 
  filter(series %in% c("apps", "f2f", "home", "tel", "video", "mode_unk")) |> 
  pivot_wider(names_from = "series", values_from = "apps") |> 
  mutate(
    dt = as.character(dt),
    pct = f2f / (apps - mode_unk) * 100) |> 
  ggplot(aes(x = dt, y = pct, group = 1L)) +
  geom_line(size = .7, color = pal[2], show.legend = FALSE) +
  scale_x_discrete(name = NULL, breaks = ts_breaks, labels = ts_labels, expand = c(0, 0)) +
  scale_y_continuous(name = NULL, limits = c(0, 90), breaks = seq(0, 80, 20), expand = c(0, 0)) +
  labs(subtitle = "Share of apps. that were face-to-face (%)")

trend_gp_space <- trend_gp + theme(plot.margin = margin(r = 4, unit = "mm")) + f2f_gp
trend_gp_space <- trend_gp_space + plot_annotation(
  caption = str_c(
    str_wrap(
      "Note: NHS Digital estimates the total number of appointments in England if 
    all GP practices had submitted data. Home visit, video, and missing mode of delivery not shown. 
      Covid vaccination appointments are excluded.",
      width = 86
    ),
    "\nSource: Appointments in general practice, NHS Digital."
  ),
  title = "Fig 2: Appointment volumes in general practice recovered quickly,\nbut with a reduced share of face-to-face apps."
)

ggsave(here("figures", "trend_gp_appointments.png"), trend_gp_space, width = 144, height = 110, units = c("mm"))




# p10 for slide-deck ----
panel <- trend_ordelec +
  theme(plot.margin = margin(r = 4, unit = "mm")) +
  trend_dc +
  theme(plot.margin = margin(r = 4, unit = "mm")) +
  trend_gp

ggsave(here("figures", "deck_panel_ordelec_daycase_gp.png"), panel, width = 260, height = 110, units = c("mm"))

panel <- trend_ed +
  theme(plot.margin = margin(r = 4, unit = "mm")) +
  trend_ip +
  theme(plot.margin = margin(r = 4, unit = "mm")) +
  trend_bed_occ

ggsave(here("figures", "deck_panel_ed_admis_beds.png"), panel, width = 260, height = 110, units = c("mm"))

ed_p <- ed_block +
  labs(subtitle = "Weekly ED visits (thousands)")
ggsave(here("figures", "deck_ed_compared.png"), ed_p, width = 144, height = 100, units = c("mm"))

ip_p <- ip_block +
  labs(subtitle = "Weekly emergency admissions (thousands)")
ggsave(here("figures", "deck_admis_compared.png"), ip_p, width = 144, height = 100, units = c("mm"))
