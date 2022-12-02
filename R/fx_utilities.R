# Utility functions
# 871_health_effects_reduced_care_covid


# README----
# utility functions


# https://yjunechoe.github.io/posts/2021-06-24-setting-up-and-debugging-custom-fonts/
# https://github.com/yjunechoe/junebug/
# register all font styles for use by ragg package
font_hoist <- function(family, silent = FALSE) {
  font_specs <- systemfonts::system_fonts() %>%
    dplyr::filter(family == .env[["family"]]) %>%
    dplyr::mutate(family = paste(.data[["family"]], .data[["style"]])) %>%
    dplyr::select(plain = .data[["path"]], name = .data[["family"]])
  
  purrr::pwalk(as.list(font_specs), systemfonts::register_font)
  
  if (!silent)  message(paste0("Hoisted ", nrow(font_specs), " variants:\n", paste(font_specs$name, collapse = "\n")))
}


# clean raw data
clean_raw_dat <- function(df, ed = FALSE) {
  df <- df |>
    unite(isoyrwk, c(isoyr, isowk), sep = "-", remove = FALSE) |>
    mutate(
      isoyrwk = paste0(
        str_extract(isoyrwk, "^[[:digit:]]{4}-"),
        str_pad(str_extract(isoyrwk, "[[:digit:]]+$"), 2, pad = "0")
      ),
      tmper = case_when(tmper == "ref" ~ "p1", TRUE ~ tmper)
    ) %>%
    {
      if (ed) { 
        mutate(., procd = substr(procd, 1, 3)) |> 
        mutate(diagnm = str_remove(diagnm, "\\s\\(disorder\\)|\\s\\(finding\\)|\\s\\(procedure\\)"))
        } else .
    }
}


# exec SPC on change in diagnosis frequency
spc_diagnosis_chg <- function(df, diagnm = diagl4nm, sex = NULL, age = NULL, high = TRUE, n = 400) {
  df <- df |>
    filter(!is.na(tmper)) %>%
    {
      if (is.null(sex)) filter(., TRUE) else filter(., sex == {{ sex }})
    } %>%
    {
      if (is.null(age)) filter(., TRUE) else filter(., age == {{ age }})
    } %>%
    group_by(tmper, {{ diagnm }}) |>
    summarise(n = sum(across(.cols = (starts_with("n_"))))) |>
    pivot_wider(names_from = tmper, values_from = n, values_fill = 0) |>
    mutate(
      chg = p2 - p1,
      u = p2 / p1,
      ubar = sum(p2) / sum(p1),
      lcl = ubar - 3 * sqrt(ubar / p1),
      ucl = ubar + 3 * sqrt(ubar / p1)
    )
  if (isTRUE(high)) {
    df <- df |>
      filter(u > ucl | is.infinite(u), p2 >= n) |> 
      arrange(-u)
  } else {
    df <- df |>
      filter(u < lcl, p2 >= n) |> 
      arrange(-u)
  }
}

