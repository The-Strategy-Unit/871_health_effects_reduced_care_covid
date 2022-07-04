# Update diagnosis groups
# 871_health_effects_reduced_care_covid


# README----
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
source(here("R", "fx_plot_profiles_diagnosis_grps.R"))
source(here("R", "plot_setup_and_parameters.R"))




# 1 read data----
gen_prof_plots <- map(
  list.files(here("raw_data"), pattern = "ip_discrete", full.names = TRUE),
  ~ fread(., na = c("NULL", "NA")))

# file names should be abbreviated grp names
short_nms <- str_extract(
  list.files(here("raw_data"), pattern = "ip_discrete"),
  "(?<=discrete_).+")

names(gen_prof_plots) <- short_nms

ed_grp <- readRDS(here("data", "grouped_ed_diags.rds"))
ip_grp <- readRDS(here("data", "grouped_ip_diags.rds"))




# 2 assign grp names----
# complete list of full grp names
full_nms <- na.omit(unique(c(ed_grp$grpnm, ip_grp$grpnm)))
full_nms <- full_nms |> as_tibble_col(column_name = "full_nm")

# create a grp name lookup from short name to full name
short_nms_df <- short_nms |> 
  as_tibble_col(column_name = "short_nm") |> 
  mutate(short_nm = str_remove(short_nm, ".csv"))

# fx for fuzzy join needs to be case insensitive
ci_str_detect <- function(x, y) {str_detect(x, regex(str_replace(y, "_", " "), ignore_case = TRUE))}

nms_lookup <- full_nms |> 
  fuzzy_left_join(short_nms_df, by = c("full_nm" = "short_nm"), match_fun = ci_str_detect)

# replace short names with full names
names(gen_prof_plots) <- nms_lookup$full_nm[match(short_nms_df$short_nm, nms_lookup$short_nm)] 




# 3 profile plots----
plot1_ls <- map(gen_prof_plots, ~ build_plot1(.))

nms <- na.omit(unique(ed_grp$grpnm))
plot2_ls <- nms |> set_names() |> map(build_plot2, dat = ed_grp)

plot3_ls <- map(gen_prof_plots, ~ build_plot3(.))




# 4 stitch profile plots----
# 13 groups in total (ED = 12; IP = 11) only 10 groups are common to both settings
plot1_stitch_ls <- plot1_ls |> list_modify("Postoperative problems" = NULL)
plot3_stitch_ls <- plot3_ls |> list_modify("Postoperative problems" = NULL)
plot2_stitch_ls <- plot2_ls |> list_modify("Eye conditions and injuries" = NULL, "Other childhood conditions" = NULL)

stitched_plots_ls <- pmap(list(plot1_stitch_ls, plot2_stitch_ls, plot3_stitch_ls), stitch_plots)




# 5 trend plot----
trend_plots <- map(gen_prof_plots, ~ build_plot4(.))




# 6 save----
for (i in 1:length(stitched_plots_ls)) {
  nm <- str_replace_all(str_to_lower(names(stitched_plots_ls[i])), "[[:space:]]", "_")
  ggsave(here("figures", paste0("profile_", nm, ".png")), stitched_plots_ls[[i]], width = 158, height = 60, units = c("mm"))
}

for (i in 1:length(trend_plots)) {
  nm <- str_replace_all(str_to_lower(names(trend_plots[i])), "[[:space:]]", "_")
  ggsave(here("figures", paste0("trend_", nm, ".png")), trend_plots[[i]], width = 144, height = 100, units = c("mm"))
}




# check for invalid values
# table(gen_prof_plots$common.csv$sex)
# table(gen_prof_plots$common.csv$tmper)
# table(gen_prof_plots$common.csv$age)

