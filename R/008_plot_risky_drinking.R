# Plot trend in risky drinking
# 871_health_effects_reduced_care_covid


# README----


# packages----
library("tidyverse")
library("here")
library("readxl")
library("lubridate")


# functions----
source(here("R", "005_plot_setup.R"))




# read data----
drink_dat <- read_xlsx(
  here("raw_data", "risky_drinking_series_ats.xlsx"),
  sheet = "dat",
  na = c("NULL", "NA", "-"),
  col_types = c("text", "date", "numeric")
)




# wrangle----
drink_dat <- drink_dat |> 
  mutate(tm = as.character(as.Date(tm))) |> 
  filter(!tm %in% c("2022-04-01", "2022-05-01", "2022-06-01", "2022-07-01")) |> 
  filter(str_detect(tm, "^201[4-7]", negate = TRUE)) |> 
  mutate(slabel = case_when(indicator == "risky drinking AUDIT" ~ "full questionnaire\n(AUDIT)", indicator == "risky drinking AUDIT-C" ~ "short questionnaire\n(AUDIT-C)", TRUE ~ as.character(indicator)))




# plot----
ts_breaks <- str_c(seq(2018, 2022, 1L), "-01-01")
ts_labels <- str_c(seq(2018, 2022, 1L))

# trend_risky <- ggplot(aes(x = tm, y = val, group = indicator, color = indicator), data = drink_dat) +
#   geom_line(size = .7, show.legend = FALSE) +
#   geom_dl(aes(label = slabel), method = list(dl.trans(y = y + .6), "first.points", hjust = 0)) +
#   scale_x_discrete(name = NULL, breaks = ts_breaks, labels = ts_labels, expand = c(0, 0)) +
#   scale_y_continuous(name = NULL, limits = c(0, 40), labels = function(x) str_c(x, "%"), expand = c(0, 0)) +
#   scale_color_manual(values = pal[c(1, 2)]) +
#   labs(subtitle = "Proportion of adults screening positive for increasing and higher risk drinking")

trend_risky <- ggplot(aes(x = tm, y = val, group = indicator, color = indicator), data = drink_dat |> 
                        filter(indicator == "risky drinking AUDIT-C")) +
  geom_line(size = .7, show.legend = FALSE) +
  scale_x_discrete(name = NULL, breaks = ts_breaks, labels = ts_labels, expand = c(0, 0)) +
  scale_y_continuous(name = NULL, limits = c(0, 40), labels = function(x) str_c(x, "%"), expand = c(0, 0)) +
  scale_color_manual(values = pal[c(2)]) +
  labs(subtitle = "Proportion of adults screening positive for increasing and higher risk drinking")

trend_risky <- trend_risky + plot_annotation(
  caption = "Note: Data for March 2020 is missing. Screening results from alcohol use disorders\nidentification test consumption (AUDIT C).\nSource: Alcohol Toolkit study.",
  title = "Fig 19: Since the start of the pandemic, there has been a sustained\nincrease in the prevalence of self-reported risky drinking"
)

ggsave(here("figures", "risky_drinkers.png"), trend_risky, width = 144, height = 100, units = c("mm"))

