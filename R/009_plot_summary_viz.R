# Plot group summary visualization
# 871_health_effects_reduced_care_covid


# README----


# packages----
library("tidyverse")
library("here")

# functions----
source(here("R", "005_plot_setup.R"))




# read data----
ed_grp <- readRDS(here("data", "grouped_ed_diags.rds"))
ip_grp <- readRDS(here("data", "grouped_ip_diags.rds"))




# wrangle----
ed_grp <- ed_grp |> 
  filter(grpnm != "Covid-19") |> 
  group_by(grpnm) |> 
  summarise(p2 = sum(p2), u = sum(p2) / sum(p1))

ip_grp <- ip_grp |> 
  filter(grpnm != "Covid-19") |> 
  group_by(grpnm) |> 
  summarise(p2 = sum(p2), u = sum(p2) / sum(p1))

dat <- ed_grp |> 
  mutate(pod = "ed") |> 
  bind_rows(
    ip_grp |> 
      mutate(pod = "ip")
  ) |> 
  add_row(
    grpnm = c("Physical injuries", "Eye conditions and injuries", "Other childhood conditions", "Postoperative problems"),
    p2 = rep(NA_integer_, 4),
    u = rep(NA_real_, 4),
    pod = c("ip", "ip", "ip", "ed")
  )

# ordering
grpnm_lab <- tribble(
  ~ grpnm, ~ grpnm_lab,
  "Common infections of childhood", "2. Common infections of childhood",
  "Complications of pregnancy", "9. Complications of pregnancy",
  "Eating disorders", "1. Eating disorders",
  "Effects of alcohol misuse", "10. Effects of alcohol misuse",
  "Physical injuries", "6. Physical injuries",
  "Exacerbation of chronic conditions", "8. Exacerbation of chronic conditions",
  "Eye conditions and injuries", "7. Eye conditions and injuries",
  "Late presentation of chronic conditions", "5. Late presentation of chronic conditions",
  "Other childhood conditions", "4. Other childhood conditions",
  "Postoperative problems", "12. Postoperative problems",
  "Spinal or back conditions", "3. Spinal or back conditions",
  "Social isolation among older people", "11. Social isolation among older people"
)

grpnm_ord <- rev(c(
  "1. Eating disorders",
  "2. Common infections of childhood",
  "3. Spinal or back conditions",
  "4. Other childhood conditions",
  "5. Late presentation of chronic conditions",
  "6. Physical injuries",
  "7. Eye conditions and injuries",
  "8. Exacerbation of chronic conditions",
  "9. Complications of pregnancy",
  "10. Effects of alcohol misuse",
  "11. Social isolation among older people",
  "12. Postoperative problems"
))

pod_label <- c(
  ed = "ED visits",
  ip = "Emergency admissions"
)

plot_dat <- dat |> 
  left_join(grpnm_lab, by = "grpnm") |> 
  mutate(grpnm_lab = factor(grpnm_lab, levels = grpnm_ord))




# plot-----
ann_text <- tribble(
  ~ grpnm_lab, ~ pod, ~ txt, ~x,
  "12. Postoperative problems", "ed", "n/a", 0.12,
  "4. Other childhood conditions", "ip", "n/a", .12,
  "6. Physical injuries", "ip", "n/a", .12,
  "7. Eye conditions and injuries", "ip", "n/a", .12
)

p1 <- ggplot(aes(x = u - 1, y = grpnm_lab, group = 1), data = plot_dat) +
  geom_vline(xintercept = 0, size = .5, color = "#d3d3d3") +
  geom_col(width = .8, show.legend = FALSE, fill = pal[3], color = pal[3]) +
  facet_wrap(vars(pod), labeller = labeller(pod = pod_label)) +
  scale_x_continuous(name = NULL, expand = c(0, 0), breaks = 0:2, limits = c(0, 2.6), labels = function(x) str_c("x", x + 1)) +
  scale_y_discrete(name = NULL, labels = function(x) str_wrap(x, width = 24)) +
  geom_text(data = ann_text, aes(x = x,  y = grpnm_lab, label = txt), colour = "#686f73", size = .36 * 8, family = "Fira Sans") +
  labs(
    caption = str_c(
      str_wrap("Note: No ED visits included in postoperative problems group; no admissions incl. in other childhood conditions, physical injuries, or eye conditions and injuries groups", width = 110),
      "\nSource: Strategy Unit analysis; SUS+, National Commissioning Data Repository."),
    title = "Fig A: Increased emergency care use by patients in these groups suggests\nthey were adversely affected by fallout from the coronavirus pandemic",
    subtitle = "scale of increase, x2 = a doubling"
    ) +
  theme(
    strip.background = element_rect(fill = "#ffffff", colour = "#ffffff"),
    strip.text = element_text(size = rel(1), hjust = 0, vjust = .5, margin = margin(t = 4, b = 4, l = 2), family = "Fira Sans Medium"),
    axis.text.y = element_text(hjust = 0, size = rel(.9), family = "Fira Sans"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(size = .2, color = "#d3d3d3"),
    plot.subtitle = element_text(size = rel(.9)),
    plot.caption = element_text(lineheight = 1.2, size = rel(.9))
  )

p1
ggsave(here("figures", "group_summary_viz.png"), p1, width = 160, height = 150, units = c("mm"))




# plot slide-deck----
p2 <- ggplot(aes(x = u - 1, y = grpnm_lab, group = 1), data = plot_dat) +
  geom_vline(xintercept = 0, size = .5, color = "#d3d3d3") +
  geom_col(width = .8, show.legend = FALSE, fill = pal[3], color = pal[3]) +
  facet_wrap(vars(pod), labeller = labeller(pod = pod_label)) +
  scale_x_continuous(name = NULL, expand = c(0, 0), breaks = 0:2, limits = c(0, 2.6), labels = function(x) str_c("x", x + 1)) +
  scale_y_discrete(name = NULL, labels = function(x) str_wrap(x, width = 24)) +
  labs(subtitle = "scale of increase, x2 = a doubling") +
  theme(
    strip.background = element_rect(fill = NA, colour = NA),
    strip.text = element_text(size = rel(1), hjust = 0, vjust = .5, margin = margin(t = 4, b = 4, l = 2), family = "Fira Sans Medium"),
    axis.text.y = element_text(hjust = 0, size = rel(.9), family = "Fira Sans Medium"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(size = .2, color = "#d3d3d3"),
    plot.subtitle = element_text(size = rel(.9)),
    plot.caption = element_text(lineheight = 1.2, size = rel(.9))
  )

p2
ggsave(here("figures", "deck_group_summary_viz.png"), p2, width = 160, height = 130, units = c("mm"))

