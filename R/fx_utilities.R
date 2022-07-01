# Utility functions
# 871_health_effects_reduced_care_covid


# README----
# utility functions




# clean raw data
clean_raw_dat <- function(df, procd = FALSE) {
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
      if (procd) mutate(., procd = substr(procd, 1, 3)) else .
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

