# Create profile plots for diagnosis groups
# 871_health_effects_reduced_care_covid


# README----
# change 123 to 1234 ...
# plot-1 subtitle - should it align with plot-3 subtitle or maintain
# spacing between subtitle and plot area with plot-2?
# this issue describes a hacky fix
# https://github.com/thomasp85/patchwork/issues/285
# stitched_plots_ls[[1]][[1]] <- stitched_plots_ls[[1]][[1]] + theme(plot.subtitle = element_text(vjust = -3.8))
# stitched_plots_ls[[1]]

# how to size geom_text
# https://wjschne.github.io/posts/making-text-labels-the-same-size-as-axis-labels-in-ggplot2/



# packages----
library("tidyverse")
library("here")
library("data.table")
library("fuzzyjoin")


# functions ----
source(here("R", "fx_utilities.R"))
source(here("R", "fx_plot_grp_profiles.R"))
source(here("R", "005_plot_setup.R"))




# 1 read data----
# information on admissions by age group comes from a SQL script that creates a
# separate csv file for each diagnosis group
gen_prof_plots <- map(
  list.files(here("raw_data"), pattern = "ip_discrete", full.names = TRUE),
  ~ fread(., na = c("NULL", "NA"))
)

# file names should be abbreviated grp names
short_nms <- str_extract(
  list.files(here("raw_data"), pattern = "ip_discrete"),
  "(?<=discrete_).+"
)

names(gen_prof_plots) <- short_nms

# only retain data up to and including iso week 12 2022
gen_prof_plots <- map(
  gen_prof_plots,
  ~ . |> filter(!(isoyr == 2022L & isowk > 12)))

ed_grp <- readRDS(here("data", "grouped_ed_diags.rds"))
ip_grp <- readRDS(here("data", "grouped_ip_diags.rds"))

# ed_diag_dat holds information on visits by age group
procd_th_plus <- readRDS(here("data", "sample_providers_code_above_threshold.rds"))

ed_dat <- fread(
  here("raw_data", "ed_diag_dat_20220719.csv"),
  na = c("NULL", "NA")
)

ed_dat <- clean_raw_dat(ed_dat, ed = TRUE) |> filter(isoyrwk != "2022-13")
ed_dat <- ed_dat |> filter(procd %in% procd_th_plus)

ed_grp_lkup <- ed_grp |> distinct(diagnm, grpnm)
ed_dat <- ed_dat |> left_join(ed_grp_lkup, by = "diagnm")

# remove males from complications of pregnancy group
ed_dat <- ed_dat |>
  filter(
    !(coalesce(grpnm, "x") == "Complications of pregnancy" & sex == "m"),
    !(coalesce(grpnm, "x") == "Complications of pregnancy" & agegrp == "65+")
  )

gen_prof_plots <- gen_prof_plots |>
  map_if(names(gen_prof_plots) == "pregnancy.csv", ~ . |>
    filter(sex != "m" & age < 65))




# 2 assign grp names----
# complete list of full grp names
full_nms <- na.omit(unique(c(ed_grp$grpnm, ip_grp$grpnm)))
full_nms <- full_nms |> as_tibble_col(column_name = "full_nm")

# create a grp name lookup from short name to full name
short_nms_df <- short_nms |>
  as_tibble_col(column_name = "short_nm") |>
  mutate(short_nm = str_remove(short_nm, ".csv"))

# fx for fuzzy join needs to be case insensitive
ci_str_detect <- function(x, y) {
  str_detect(x, regex(str_replace(y, "_", " "), ignore_case = TRUE))
}

nms_lookup <- full_nms |>
  fuzzy_left_join(short_nms_df, by = c("full_nm" = "short_nm"), match_fun = ci_str_detect)

# replace short names with full names
names(gen_prof_plots) <- nms_lookup$full_nm[match(short_nms_df$short_nm, nms_lookup$short_nm)]




# check numbers in text ----
ed_grp |> 
  group_by(grpnm) |> 
  summarise(across(c(p1:p2), sum)) |> 
  filter(!is.na(grpnm), grpnm != "Covid-19") |> 
  mutate(chg = p2 - p1, p = p2 / p1) |> 
  arrange(-p)

ip_grp |> 
  group_by(grpnm) |> 
  summarise(across(c(p1:p2), sum)) |> 
  filter(!is.na(grpnm), grpnm != "Covid-19") |> 
  mutate(chg = p2 - p1, p = p2 / p1) |> 
  arrange(-p)

gen_prof_plots$`Late presentation of chronic conditions` |> 
  group_by(isoyr) |> 
  summarise(n = sum(n_admi)) |> 
  filter(isoyr %in% c(2019, 2021)) |> 
  pivot_wider(names_from = isoyr, values_from = n, names_prefix = "yr_") |> 
  mutate(p = yr_2021 / yr_2019)

gen_prof_plots$`Exacerbation of chronic conditions` |> 
  group_by(isoyr) |> 
  summarise(n = sum(n_admi)) |> 
  filter(isoyr %in% c(2019, 2021)) |> 
  pivot_wider(names_from = isoyr, values_from = n, names_prefix = "yr_") |> 
  mutate(p = yr_2021 / yr_2019)

gen_prof_plots$`Spinal or back conditions` |> 
  group_by(isoyr, isowk) |> 
  summarise(n = sum(n_admi)) |> 
  ungroup() |> 
  group_by(isoyr) |> 
  summarise(mn = mean(n)) |> 
  filter(isoyr %in% c(2019, 2021))

ed_dat |> 
  filter(grpnm == "Eye conditions and injuries") |> 
  group_by(tmper) |> 
  summarise(n = sum(n_visits)) |> 
  filter(!is.na(tmper)) |> 
  pivot_wider(names_from = tmper, values_from = n) |> 
  mutate(p = p2 / p1)

ed_dat |> 
  filter(grpnm == "Spinal or back conditions") |> 
  group_by(tmper) |> 
  summarise(n = sum(n_visits)) |> 
  filter(!is.na(tmper)) |> 
  pivot_wider(names_from = tmper, values_from = n) |> 
  mutate(p = p2 / p1)

ed_dat |> 
  filter(diagnm %in% c("Lumbar radiculopathy", "Cervical radiculopathy")) |> 
  group_by(tmper) |> 
  summarise(n = sum(n_visits)) |> 
  filter(!is.na(tmper)) |> 
  pivot_wider(names_from = tmper, values_from = n) |> 
  mutate(p = p2 / p1)



# 3 profile plots----
# ensure consistent ordering before building plots
gen_prof_plots <- gen_prof_plots[order(names(gen_prof_plots))]
nms <- sort(na.omit(unique(ed_grp$grpnm)))

plot1_ls <- nms |>
  set_names() |>
  map(build_plot1, dat = ed_grp)
plot2_ls <- map(gen_prof_plots, ~ build_plot2(.))
plot3_ls <- nms |>
  set_names() |>
  map(build_plot3, dat = ed_dat)
plot4_ls <- map(gen_prof_plots, ~ build_plot4(.))
plot5_ls <- map(gen_prof_plots, ~ build_plot5(.)) # not used

# add labels to postoperative problems admissions age/sex plot
plot4_ls <- plot4_ls |>
  map_if(names(plot4_ls) == "Postoperative problems", ~ . +
    annotate("text", x = 1.25, y = 500, label = "male", color = pal[5], size = 8 / 2.8, hjust = 0) +
    annotate("text", x = 0.75, y = 500, label = "female", color = pal[4], size = 8 / 2.8, hjust = 0))

# update subtitles for postoperative problems profile plots
plot2_ls <- plot2_ls |>
  map_if(names(plot2_ls) == "Postoperative problems", ~ . +
  labs(subtitle = "Emergency admis., weeks 25\u201343")) 

plot4_ls <- plot4_ls |>
  map_if(names(plot4_ls) == "Postoperative problems", ~ . +
  labs(subtitle = "Emerg. admis. by age/sex, w25\u201343 2021"))


# 4 stitch profile plots----
# plot spacers required because some groups are specific to ED or inpatient activity
# 12 groups in total excl. COVID-19 (ED = 11; IP = 9) only 8 groups are common to both settings
# eye conditions, other childhood conditions, and effects of physical violence are ED only
plot2_ls <- append(plot2_ls, list("Eye conditions and injuries" = plot_spacer()), after = match("Exacerbation of chronic conditions", names(plot2_ls)))
plot2_ls <- append(plot2_ls, list("Other childhood conditions" = plot_spacer()), after = match("Late presentation of chronic conditions", names(plot2_ls)))
plot2_ls <- append(plot2_ls, list("Physical injuries" = plot_spacer()), after = match("Other childhood conditions", names(plot2_ls)))
plot4_ls <- append(plot4_ls, list("Eye conditions and injuries" = plot_spacer()), after = match("Exacerbation of chronic conditions", names(plot4_ls)))
plot4_ls <- append(plot4_ls, list("Other childhood conditions" = plot_spacer()), after = match("Late presentation of chronic conditions", names(plot4_ls)))
plot4_ls <- append(plot4_ls, list("Physical injuries" = plot_spacer()), after = match("Other childhood conditions", names(plot4_ls)))
plot5_ls <- append(plot5_ls, list("Eye conditions and injuries" = plot_spacer()), after = match("Exacerbation of chronic conditions", names(plot5_ls)))
plot5_ls <- append(plot5_ls, list("Other childhood conditions" = plot_spacer()), after = match("Late presentation of chronic conditions", names(plot5_ls)))
plot5_ls <- append(plot5_ls, list("Physical injuries" = plot_spacer()), after = match("Other childhood conditions", names(plot5_ls)))
# postoperative problems is inpatient only
plot1_ls <- append(plot1_ls, list("Postoperative problems" = plot_spacer()), after = match("Physical injuries", names(plot1_ls)))
plot3_ls <- append(plot3_ls, list("Postoperative problems" = plot_spacer()), after = match("Physical injuries", names(plot3_ls)))
# rm COVID-19
plot1_ls[["Covid-19"]] <- NULL
plot3_ls[["Covid-19"]] <- NULL

# check names and order match
a <- names(plot1_ls)
b <- names(plot2_ls)
c <- names(plot3_ls)
d <- names(plot4_ls)
e <- names(plot5_ls)

map_lgl(list(b, c, d, e), identical, a)
all(map_lgl(list(b, c, d, e), identical, a))

# profile plot titles
plot_titles_ls <- list(
  "Fig 9: Emergency care activity increased for many common infections of childhood", # 1;3.1
  "Fig 26: More women were treated in hospital for pregnancy complications", # 2;3.10
  "Fig 16: More young people are requiring emergency treatment for eating disorders", # 4;3.5
  "Fig 18: Visits and admissions for alcohol-related conditions increased", # 5;3.6
  "Fig 14: Disruption to routine care may have caused admissions for LTCs to increase", # 6;3.4
  "Fig 28: ED visits for minor eye conditions and injuries increased by 37% ", # 7;3.10
  "Fig 12: Delays in diagnosis may have contributed to an increase in late presentations", # 8;3.3
  "Fig 11: ED visits increased for a small number of non-viral childhood conditions", # 9;3.2
  "Fig 21: Injury presentations to ED increased", # 10;3.7
  "Fig 29: Admissions for postoperative problems increased, esp. in urology & gynaecology", # 11;3.12
  "Fig 22: Social isolation may have contributed to higher admissions among older people", # 12;3.8
  "Fig 24: ED visits for spinal or back conds. were 79% higher in w25\u201343 2021 than in 2019" # 13;3.9
)

# character limit to fit title on single line is c.90
# sort(map_dbl(plot_titles_ls, ~ nchar(.)))

stitch_plots_ls <- pmap(list(plot1_ls, plot2_ls, plot5_ls, plot_titles_ls), stitch_plotsA)
stitch_plots_ls <- pmap(list(plot1_ls, plot2_ls, plot3_ls, plot4_ls, plot_titles_ls), stitch_plotsB)




# 5 trend plots----
# trend plot titles
# names(gen_prof_plots)
plot_titles_ls <- list(
  # 1;3.1
  "Fig 10: The coronavirus pandemic disrupted the normal seasonal pattern for many common infections of childhood",
  # 2;3.10
  "Fig 27: More women are being admitted for pregnancy complications than before the pandemic",
  # 4;3.5
  "Fig 17: Eating disorder admissions started climbing from mid-2020 and have remained elevated",
  # 5;3.6
  "Fig 20: There has been a sustained increase in alcohol-related admissions",
  # 6;3.4
  "Fig 15: Admissions linked to exacerbation or complication of LTCs were 30% higher in 2021 compared with 2019",
  # 7;3.3
  "Fig 13: Admissions linked to late presentation of LTCs were 28% higher in 2021 compared with 2019",
  # 8;3.12
  "Fig 30: A step-change increase in admissions for postoperative problems occurred from early 2021",
  # 9;3.8
  "Fig 23: Admissions where social isolation may have been a factor were already trending upward before the pandemic hit",
  # 10;3.9
  "Fig 25: Average weekly admissions for spinal or back conditions climbed to 17 in 2021 compared with 13 in 2019"
)

# wrap titles
plot_titles_ls <- map(plot_titles_ls, ~ str_wrap(., width = 65))

# build plots
trend_plots <- map2(gen_prof_plots, plot_titles_ls, build_plot6)

names(gen_prof_plots)


# 6 save----
for (i in 1:length(stitch_plots_ls)) {
  nm <- str_replace_all(str_to_lower(names(stitch_plots_ls[i])), "[[:space:]]", "_")
  ggsave(here("figures", paste0(nm, "_profile", ".png")), stitch_plots_ls[[i]], width = 158, height = 60, units = c("mm"))
}

for (i in 1:length(trend_plots)) {
  nm <- str_replace_all(str_to_lower(names(trend_plots[i])), "[[:space:]]", "_")
  ggsave(here("figures", paste0(nm, "_ts", ".png")), trend_plots[[i]], width = 144, height = 100, units = c("mm"))
}

