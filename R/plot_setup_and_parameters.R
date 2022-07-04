# Dependencies and parameters for creating plots
# 871_health_effects_reduced_care_covid


# README----
# single source for plotting dependencies and parameters


# packages----
library("tidyverse")
library("here")
library("scales")
library("directlabels")
library("MetBrewer")
library("patchwork")
library("showtext")

# fonts----
library("ragg")
source(here("R", "fx_utilities.R"))
# systemfonts::system_fonts()
# systemfonts::system_fonts() %>% 
#   filter(family == "Fira Sans") %>% 
#   transmute(
#     family, style,
#     file = str_extract(path, "[\\w-]+\\.ttf$")
#   ) |> 
#   print(n = 25)
font_hoist("Fira Sans")
# systemfonts::registry_fonts() |> select(-path) |> print(n = 72)


# theme----
source(here("R", "theme_871.R"))
theme_set(theme_871())


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

