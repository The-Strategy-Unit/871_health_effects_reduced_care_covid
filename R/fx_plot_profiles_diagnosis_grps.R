
# bar plot admissions by year
build_plot1 <- function(dat) {
  
  plot_dat <- dat |> 
    filter(!is.na(tmper)) |> 
    group_by(tmper) |> 
    summarise(n = sum(n_admi))
  
  plot_dat |> 
    ggplot(aes(x = tmper, y = n, group = tmper, fill = tmper)) +
    geom_bar(stat = "identity", show.legend = FALSE) + 
    scale_x_discrete(name = NULL, labels = c("2019", "2021")) + 
    scale_y_continuous(name = NULL, expand = expansion(mult = c(0, 0.05)), limits = c(0, NA),
                       labels = label_comma(scale = .001)) + 
    scale_fill_manual(values = pal[c(7, 8)]) + 
    coord_flip() + 
    labs(subtitle = "Emergency admissions (thousands), weeks 25\u201343") +
    theme_871(
      base_size = 8,
    ) +
    theme(
      panel.grid.major.y = element_blank()
    )
  
}

# bar plot visits by year
build_plot2 <- function(dat, grpnm = "Common infections of childhood") {
  
  plot_dat <- dat |> 
    group_by(grpnm) |> 
    summarise(across(c(p1, p2), sum)) |> 
    pivot_longer(cols = c(p1:p2), names_to = "tmper", values_to = "n") |> 
    filter(grpnm == {{grpnm}})
  
  plot_dat |> 
    ggplot(aes(x = tmper, y = n, group = tmper, fill = tmper)) +
    geom_bar(stat = "identity", show.legend = FALSE) + 
    scale_x_discrete(name = NULL, labels = c("2019", "2021")) + 
    scale_y_continuous(name = NULL, expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)
                       , labels = label_comma(scale = .001)) + 
    scale_fill_manual(values = pal[c(7, 8)]) + 
    coord_flip() + 
    labs(subtitle = "ED visits (thousands), weeks 25\u201343") +
    theme_871(
      base_size = 8,
    ) +
    theme(
      panel.grid.major.y = element_blank()
    )
  
}

# facet grouped bar plot admissions by year, by age & sex
build_plot3 <- function(dat) {
  
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
    scale_x_discrete(name = NULL) +
    scale_y_continuous(name = NULL, labels = label_comma(scale = .001)) +
    scale_fill_manual(values = pal[c(4,5)]) +
    annotate("text", x = 4.25, y = .8 * max(plot_dat$n), label = "male", color = pal[5], size = 8 / 2.8, hjust = 0) +
    annotate("text", x = 3.75, y = .8 * max(plot_dat$n), label = "female", color = pal[4], size = 8 / 2.8, hjust = 0) +
    labs(subtitle = "Emergency admissions by age & sex (thousands)") +
    theme_871(
      base_size = 8,
    ) +
    theme(
      strip.background = element_rect(fill = NA)
    )
  
}

# stitch 3 plots above together
# this method of composing the layout (passing design argument to plot_layout) creates 3 patch areas
# alternative method shown below only creates 2 patch areas 
stitch_plots <- function(
    p1, p2, p3,
    title = "Emergency care activity for many common infections of childhood was much higher in 2021") {
  
  layout <- c(
    area(t = 1, l = 1, b = 1, r = 1),
    area(t = 2, l = 1, b = 2, r = 1),
    area(t = 1, l = 2, b = 2, r = 2)
  )
  
  p4 <- p1 +
    theme(plot.margin = margin(b = 2, unit = "mm")) + p2 +
    theme(plot.margin = margin(r = 2, unit = "mm")) + p3 +
    plot_layout(design = layout) +
    plot_annotation(
      caption = "Source(s): Strategy Unit analysis; SUS+, National Commissioning Data Repository.",
      title = {{title}},
      theme = theme(
        plot.caption = element_text(size = 8),
        plot.title = element_text(size = 8 * 1.3)
      )
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

# timeseries plot admissions
build_plot4 <- function(dat) {
  
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
    geom_line(size = .7, show.legend = FALSE, color = pal[2]) +
    scale_x_discrete(name = NULL, breaks = ts_breaks, labels = ts_labels, expand = c(0, 0)) +
    scale_y_continuous(name = NULL, expand = c(0, 0)) +
    scale_fill_manual(values = pal[c(7, 8)]) +
    labs(
      caption = "Source(s): Strategy Unit analysis; SUS+, National Commissioning Data Repository.",
      subtitle = "Weekly emergency admissions",
      title = "The coronavirus pandemic distorted the normal seasonal pattern for\nmany common infections of childhood") +
    theme_871(
      base_size = 8,
    )
}

