
cut_short_scale <- function(space = FALSE) {
  out <- c(0, k = 1e3, M = 1e6, B = 1e9, T = 1e12)
  if (space) {
    names(out) <- paste0(" ", names(out))
  }
  out
}


# bar plot visits by year
build_plot1 <- function(dat, grpnm = "Common infections of childhood") {
  
  plot_dat <- dat |> 
    group_by(grpnm) |> 
    summarise(across(c(p1, p2), sum)) |> 
    pivot_longer(cols = c(p1:p2), names_to = "tmper", values_to = "n") |> 
    filter(grpnm == {{grpnm}})
  
  plot_dat |> 
    ggplot(aes(x = tmper, y = n, group = tmper, fill = tmper)) +
    geom_bar(stat = "identity", show.legend = FALSE) + 
    scale_x_discrete(name = NULL, labels = c("2019", "2021")) + 
    scale_y_continuous(name = NULL, expand = expansion(mult = c(0, 0.05)), limits = c(0, NA),
                       labels = label_number(scale_cut = cut_short_scale())) + 
    scale_fill_manual(values = pal[c(7, 8)]) + 
    coord_flip() + 
    labs(subtitle = "ED visits, weeks 25\u201343") +
    theme_871(base_size = 8) +
    theme(panel.grid.major.y = element_blank())
}

# bar plot admissions by year
build_plot2 <- function(dat) {
  
  plot_dat <- dat |> 
    filter(!is.na(tmper)) |> 
    group_by(tmper) |> 
    summarise(n = sum(n_admi))
  
  plot_dat |> 
    ggplot(aes(x = tmper, y = n, group = tmper, fill = tmper)) +
    geom_bar(stat = "identity", show.legend = FALSE) + 
    scale_x_discrete(name = NULL, labels = c("2019", "2021")) + 
    scale_y_continuous(name = NULL, expand = expansion(mult = c(0, 0.05)), limits = c(0, NA),
                       labels = label_number(scale_cut = cut_short_scale())) + 
    scale_fill_manual(values = pal[c(7, 8)]) + 
    coord_flip() + 
    labs(subtitle = "Emergency admis.") +
    theme_871(base_size = 8) +
    theme(panel.grid.major.y = element_blank())
}

# grouped bar plot visits, by age & sex
build_plot3 <- function(dat, grpnm = "Common infections of childhood") {
  
  plot_dat <- dat |> 
    filter(if_all(c(tmper, sex, agegrp), ~ !is.na(.x)), grpnm == {{grpnm}}) |> 
    group_by(tmper, sex, agegrp) |> 
    summarise(n = sum(n_visits)) |> 
    ungroup()
  
  p <- plot_dat |> 
    ggplot(aes(x = agegrp, y = n, group = sex, fill = sex)) +
    geom_bar(stat = "identity", position = "dodge", show.legend = FALSE) +
    coord_flip() +
    scale_x_discrete(name = NULL) +
    scale_y_continuous(name = NULL, expand = expansion(mult = c(0, 0.05)), limits = c(0, NA),
                       labels = label_number(scale_cut = cut_short_scale())) + 
    scale_fill_manual(values = pal[c(4,5)]) +
    labs(subtitle = "ED visits by age/sex, w25\u201343 2021") +
    theme_871(base_size = 8) +
    theme(strip.background = element_rect(fill = NA))
    
    {
      if (grpnm %in% c(
        "Late presentation of chronic conditions",
        "Exacerbation of chronic conditions",
        "Social isolation among older people"
        )) {
        p +
        annotate("text", x = 1.25, y = .7 * max(plot_dat$n), label = "male", color = pal[5], size = 8 / 2.8, hjust = 0) +
        annotate("text", x = 0.75, y = .7 * max(plot_dat$n), label = "female", color = pal[4], size = 8 / 2.8, hjust = 0)
      } else if (grpnm == "Physical injuries") {
        p +
        annotate("text", x = 3.25, y = .7 * max(plot_dat$n), label = "male", color = pal[5], size = 8 / 2.8, hjust = 0) +
        annotate("text", x = 2.75, y = .7 * max(plot_dat$n), label = "female", color = pal[4], size = 8 / 2.8, hjust = 0)
      } else if (grpnm == "Complications of pregnancy") {
        p
        } else {
        p +
        annotate("text", x = 4.25, y = .7 * max(plot_dat$n), label = "male", color = pal[5], size = 8 / 2.8, hjust = 0) +
        annotate("text", x = 3.75, y = .7 * max(plot_dat$n), label = "female", color = pal[4], size = 8 / 2.8, hjust = 0)
      }
    }

}

# grouped bar plot admissions, by age & sex
build_plot4 <- function(dat) {
  
  plot_dat <- dat |> 
    filter(!is.na(tmper), age <= 110L, !is.na(sex)) |> 
    mutate(agegrp = cut(age, c(0, 15, 44, 64, 110), c("0\u201315", "16\u201344", "45\u201364", "65+"), include.lowest = TRUE)) |>
    group_by(tmper, sex, agegrp) |>
    summarise(n = sum(n_admi))
  
  p <- plot_dat |> 
    ggplot(aes(x = agegrp, y = n, group = sex, fill = sex)) +
    geom_bar(stat = "identity", position = "dodge", show.legend = FALSE) +
    coord_flip() +
    scale_x_discrete(name = NULL, labels = NULL) +
    scale_y_continuous(name = NULL, expand = expansion(mult = c(0, 0.05)), limits = c(0, NA),
                       labels = label_number(scale_cut = cut_short_scale())) + 
    scale_fill_manual(values = pal[c(4,5)]) +
    labs(subtitle = "Emergency admis.") +
    theme_871(base_size = 8) +
    theme(strip.background = element_rect(fill = NA))
  
}

# facet grouped bar plot admissions, by year, by age & sex
build_plot5 <- function(dat) {

  plot_dat <- dat |>
    filter(!is.na(tmper), age <= 110L, !is.na(sex)) |>
    mutate(agegrp = cut(age, c(0, 15, 44, 64, 110), c("0\u201315", "16\u201344", "45\u201364", "65+"), include.lowest = TRUE)) |>
    group_by(tmper, sex, agegrp) |>
    summarise(n = sum(n_admi))

  tmper_labs <- c(
    `p1` = "2019, weeks 25\u201343",
    `p2` = "2021, weeks 25\u201343"
  )

  plot_dat |>
    ggplot(aes(x = agegrp, y = n, group = sex, fill = sex)) +
    geom_bar(stat = "identity", position = "dodge", show.legend = FALSE) +
    facet_wrap(vars(tmper), labeller = as_labeller(tmper_labs)) +
    coord_flip() +
    scale_x_discrete(name = NULL, expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
    scale_y_continuous(name = NULL, labels = label_comma(scale = .001)) +
    scale_fill_manual(values = pal[c(4,5)]) +
    annotate("text", x = 4.25, y = .7 * max(plot_dat$n), label = "male", color = pal[5], size = 8 / 2.8, hjust = 0) +
    annotate("text", x = 3.75, y = .7 * max(plot_dat$n), label = "female", color = pal[4], size = 8 / 2.8, hjust = 0) +
    labs(subtitle = "Emergency admissions by age & sex (thous.)") +
    theme_871(base_size = 8) +
    theme(strip.background = element_rect(fill = NA))
}

# stitch plots 1+2+5 above together
# this method of composing the layout (passing design argument to plot_layout) creates 3 patch areas
# alternative method shown below only creates 2 patch areas
# use plot(layout) to view layout
stitch_plotsA <- function(
    p1, p2, p3,
    title = "Plot title") {
  
  layout <- c(
    area(t = 1, l = 1, b = 1, r = 1),
    area(t = 2, l = 1, b = 2, r = 1),
    area(t = 1, l = 2, b = 2, r = 2)
  )
  
  p <- p1 +
    theme(plot.margin = margin(b = 2, unit = "mm")) + p2 +
    theme(plot.margin = margin(r = 2, unit = "mm")) + p3 +
    plot_layout(design = layout) +
    plot_annotation(
      caption = "Sources: Strategy Unit analysis; SUS+, National Commissioning Data Repository.",
      title = {{title}},
      theme = theme(plot.title = element_text(size = 11), plot.caption = element_text(size = 8))
      )
} 

# this method of composing the layout only creates 2 patch areas
# p <- (p1) / theme(plot.margin = margin(b = 4, unit = "mm")) / (p2) +
#   theme(plot.margin = margin(r = 4, unit = "mm")) | (p3) +
#   plot_annotation(
#     caption = "Source(s): Strategy Unit analysis; SUS+, National Commissioning Data Repository.",
#     title = "Common childhood infections...",
#     theme = theme(
#       plot.caption = element_text(size = 8),
#       plot.title = element_text(size = 8 * 1.3)
#       )
#     )

# stitch plots 1+2+3+4 above together
stitch_plotsB <- function(
    p1, p2, p3, p4,
    title = "Plot title") {
  
  layout <- c(
    area(t = 1, l = 1, b = 1, r = 1),
    area(t = 2, l = 1, b = 2, r = 1),
    area(t = 1, l = 2, b = 2, r = 2),
    area(t = 1, l = 3, b = 2, r = 3)
  )
  
  p <- p1 +
    theme(plot.margin = margin(b = 2, unit = "mm")) + p2 +
    theme(plot.margin = margin(r = 2, unit = "mm")) + p3 +
    theme(plot.margin = margin(b = 2, unit = "mm")) + p4 +
    plot_layout(design = layout) +
    plot_annotation(
      caption = "Sources: Strategy Unit analysis; SUS+, National Commissioning Data Repository.",
      title = {{title}},
      theme = theme(plot.title = element_text(size = 11), plot.caption = element_text(size = 8))
    )
}

# timeseries plot admissions
build_plot6 <- function(dat, title = "Plot title") {
  
  plot_dat <- dat |> 
    unite(isoyrwk, c(isoyr, isowk), sep = "-", remove = FALSE) |>
    mutate(
      isoyrwk = paste0(
        str_extract(isoyrwk, "^[[:digit:]]{4}-"),
        str_pad(str_extract(isoyrwk, "[[:digit:]]+$"), 2, pad = "0")
      )) |> 
    group_by(isoyrwk) |> 
    summarise(n = sum(n_admi))

  ref_index$ymax = max(pretty(plot_dat$n))

  plot_dat |>   
    ggplot(aes(x = isoyrwk, y = n, group = 1L)) +
    geom_rect(aes(xmin = wk_min, xmax = wk_max, ymin = ymin, ymax = ymax, group = tmper, fill = tmper),
              data = ref_index,
              alpha = .4, color = NA,
              show.legend = FALSE, inherit.aes = FALSE
    ) +
    geom_line(linewidth = .7, show.legend = FALSE, color = pal[2]) +
    annotate("text", x = "2019-34", y = .05 * max(plot_dat$n), label = "w25\u201343", color = "#686f73", size = 8 / 2.8, hjust = .5) +
    annotate("text", x = "2021-34", y = .05 * max(plot_dat$n), label = "w25\u201343", color = "#686f73", size = 8 / 2.8, hjust = .5) +
    scale_x_discrete(name = NULL, breaks = ts_breaks, labels = ts_labels, expand = c(0, 0)) +
    scale_y_continuous(name = NULL, expand = c(0, 0)) +
    scale_fill_manual(values = pal[c(7, 8)]) +
    labs(
      caption = "Sources: Strategy Unit analysis; SUS+, National Commissioning Data Repository.",
      subtitle = "Weekly emergency admissions",
      title = {{title}})
}

