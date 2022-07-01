# Take screenshots of HTML tables
# 871_health_effects_reduced_care_covid


# README----
# save reactable tables as image files for report


# packages----
library("here")
library("webshot2")




# screenshots----
webshot(c(
  here("./", "common_infections_of_childhood_table.html"),
  here("./", "common_infections_of_childhood_table.html")
  ),
  selector = list(
    ".childhood-infections-ip-rtbl",
    ".childhood-infections-ed-rtbl"
    ),
  zoom = 2,
  expand = c(0, 12, 0, -12),
  file = c(
    here("figures", "common_infections_of_childhood_ip_tbl.png"),
    here("figures", "common_infections_of_childhood_ed_tbl.png")
    )
)

webshot(c(
  here("./", "topn_diagnosis_increased_table.html"),
  here("./", "topn_diagnosis_increased_table.html")
  ),
  selector = list(
  ".topn-ip-rtbl",
  ".topn-ed-rtbl"
  ),
  zoom = 2,
  expand = c(0, 12, 0, -12),
  file = c(
    here("figures", "topn_diagnosis_increased_ip_tbl.png"),
    here("figures", "topn_diagnosis_increased_ed_tbl.png")
    )
)

