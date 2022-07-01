# Dependencies and parameters for creating plots
# 871_health_effects_reduced_care_covid


# README----
# single source for plotting dependencies and parameters


# packages----
library("scales")
library("directlabels")
library("MetBrewer")
library("patchwork")
library("showtext")


# theme----
source(here("R", "theme_871.R"))
theme_set(theme_871())


# fonts----
font_add_google("Work Sans", "work")
showtext_auto()
showtext_opts(dpi = 320)


# parameters----
ts_breaks <- str_c(seq(2018, 2022, 1L), "-01")
ts_labels <- seq(2018, 2022, 1L)

p1 <- paste(2019, 25:43, sep = "-")
p2 <- str_replace_all(p1, "2019", "2021")

ref_index <- tibble(
  isoyrwk = list(p1, p2),
  tmper = c("p1", "p2")
) |>
  unnest(cols = c(isoyrwk)) |>
  arrange(isoyrwk) |>
  group_by(tmper) |>
  slice(c(1, n())) |>
  mutate(wk_min = first(isoyrwk), wk_max = last(isoyrwk), ymin = 0L, ymax = 1e5L) |>
  slice(1) |>
  ungroup()


# palette----
# met.brewer("VanGogh2")
pal <- MetPalettes$VanGogh2[[1]]

