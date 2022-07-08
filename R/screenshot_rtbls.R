# Take screenshots of HTML tables
# 871_health_effects_reduced_care_covid


# README----
# save reactable tables as PNG files for report


# packages----
library("here")
library("webshot2")





# screenshots----
# group profile tables
tbls_html <- rep(c(
  here("rtbl_sedentary_lifestyles.html"),
  here("rtbl_social_isolation_among_older_people.html"),
  here("rtbl_effects_of_alcohol_misuse.html"),
  here("rtbl_exacerbation_of_chronic_condition.html"),
  here("rtbl_eating_disorders.html"),
  here("rtbl_common_infections_of_childhood.html"),
  here("rtbl_late_presentation_of_chronic_condition.html"),
  here("rtbl_complications_of_pregnancy.html"),
  here("rtbl_postoperative_problems.html"),
  here("rtbl_eye_conditions_and_injuries.html"),
  here("rtbl_other_childhood_conditions.html"),
  here("rtbl_effects_of_physical_violence.html"),
  here("rtbl_covid-19.html")
), each = 2L)

selectors <- as.list(
  rep_len(c(".rtbl-grp-ed", ".rtbl-grp-ip"), length.out = length(tbls_html))
)

file_nms <- paste0(
  rep(c(
    here("figures", "rtbl_sedentary_lifestyles"),
    here("figures", "rtbl_social_isolation_among_older_people"),
    here("figures", "rtbl_effects_of_alcohol_misuse"),
    here("figures", "rtbl_exacerbation_of_chronic_condition"),
    here("figures", "rtbl_eating_disorders"),
    here("figures", "rtbl_common_infections_of_childhood"),
    here("figures", "rtbl_late_presentation_of_chronic_condition"),
    here("figures", "rtbl_complications_of_pregnancy"),
    here("figures", "rtbl_postoperative_problems"),
    here("figures", "rtbl_eye_conditions_and_injuries"),
    here("figures", "rtbl_other_childhood_conditions"),
    here("figures", "rtbl_effects_of_physical_violence"),
    here("figures", "rtbl_covid-19")
  ), each = 2L),
  c("_ed.png", "_ip.png")
)

webshot(
  tbls_html,
  selector = selectors,
  zoom = 2,
  expand = c(0, 0, 0, 0),
  file = file_nms
)

# top_n tables
webshot(c(
  here("./", "rtbl_topn_diagnosis_increased.html"),
  here("./", "rtbl_topn_diagnosis_increased.html")
  ),
  selector = list(
  ".rtbl-topn-ed",
  ".rtbl-topn-ip"
  ),
  zoom = 2,
  expand = c(0, 0, 0, 0),
  file = c(
    here("figures", "rtbl_topn_diagnosis_increased_ed.png"),
    here("figures", "rtbl_topn_diagnosis_increased_ip.png")
    )
)

