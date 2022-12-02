# Take screenshots of HTML tables
# 871_health_effects_reduced_care_covid


# README----
# save reactable tables as PNG files for report


# packages----
library("here")
library("webshot2")




# 1 group profile tables----
tbls_html <- rep(c(
  here("rtbl_common_infections_of_childhood.html"),
  here("rtbl_complications_of_pregnancy.html"),
  here("rtbl_covid-19.html"),
  here("rtbl_eating_disorders.html"),
  here("rtbl_effects_of_alcohol_misuse.html"),
  here("rtbl_exacerbation_of_chronic_conditions.html"),
  here("rtbl_eye_conditions_and_injuries.html"),
  here("rtbl_late_presentation_of_chronic_conditions.html"),
  here("rtbl_other_childhood_conditions.html"),
  here("rtbl_physical_injuries.html"),
  here("rtbl_postoperative_problems.html"),
  here("rtbl_social_isolation_among_older_people.html"),
  here("rtbl_spinal_or_back_conditions.html")
), each = 2L)

selectors <- as.list(
  rep_len(c(".rtbl-grp-ed", ".rtbl-grp-ip"), length.out = length(tbls_html))
)

file_nms <- paste0(
  rep(c(
    here("figures", "rtbl_common_infections_of_childhood.html"),
    here("figures", "rtbl_complications_of_pregnancy.html"),
    here("figures", "rtbl_covid-19.html"),
    here("figures", "rtbl_eating_disorders.html"),
    here("figures", "rtbl_effects_of_alcohol_misuse.html"),
    here("figures", "rtbl_exacerbation_of_chronic_conditions.html"),
    here("figures", "rtbl_eye_conditions_and_injuries.html"),
    here("figures", "rtbl_late_presentation_of_chronic_conditions.html"),
    here("figures", "rtbl_other_childhood_conditions.html"),
    here("figures", "rtbl_physical_injuries.html"),
    here("figures", "rtbl_postoperative_problems.html"),
    here("figures", "rtbl_social_isolation_among_older_people.html"),
    here("figures", "rtbl_spinal_or_back_conditions.html")
  ), each = 2L),
  c("_ed.png", "_ip.png")
)


webshot(
  tbls_html,
  selector = selectors,
  zoom = 2,
  # strange behaviour
  expand = c(
    list(c(0, 14, 0, -12)),  #1
    list(c(0, 14, 0, -12)),  #1
    list(c(0, 1, 0, 1)),  #2
    list(c(0, 1, 0, 1)),  #2
    list(c(0, 1, 0, 1)),  #3
    list(c(0, 1, 0, 1)),  #3
    list(c(0, 1, 0, 1)),  #4
    list(c(0, 1, 0, 1)),  #4
    list(c(0, 1, 0, 1)),  #5
    list(c(0, 1, 0, 1)),  #5
    list(c(0, 14, 0, -12)),  #6
    list(c(0, 14, 0, -12)),  #6
    list(c(0, 1, 0, 1)),  #7
    list(c(0, 1, 0, 1)),  #7
    list(c(0, 1, 0, 1)),  #8
    list(c(0, 1, 0, 1)),  #8
    list(c(0, 1, 0, 1)),  #9
    list(c(0, 1, 0, 1)),  #9
    list(c(0, 1, 0, 1)),  #10
    list(c(0, 1, 0, 1)),  #10
    list(c(0, 1, 0, 1)),  #11
    list(c(0, 1, 0, 1)),  #11
    list(c(0, 1, 0, 1)),  #12
    list(c(0, 1, 0, 1)),  #12
    list(c(0, 1, 0, 1)),  #13
    list(c(0, 1, 0, 1))  #13
    ),
  file = file_nms
)

# 2 top_n tables----
webshot(c(
  here("./", "rtbl_topn_diagnosis_increased.html"),
  here("./", "rtbl_topn_diagnosis_increased.html")
),
selector = list(
  ".rtbl-topn-ed",
  ".rtbl-topn-ip"
),
zoom = 2,
# strange behaviour
expand = c(4, 14, 4, -12),
file = c(
  here("figures", "rtbl_topn_diagnosis_increased_ed.png"),
  here("figures", "rtbl_topn_diagnosis_increased_ip.png")
)
)

