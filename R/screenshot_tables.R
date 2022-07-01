# Take screenshots of HTML tables
# 871_health_effects_reduced_care_covid


# README----
# save reactable tables as image files for report


# packages----
library("here")
library("webshot2")




# 1 webshots----
webshot(c(
  here("./", "common_infections_of_childhood_table.html"),
  here("./", "common_infections_of_childhood_table.html")
  ),
  selector = list(
    ".childhood-infections-ip-tbl",
    ".childhood-infections-ed-tbl"
    ),
  zoom = 2,
  expand = c(0, 12, 0, -12),
  file = c(
    here("figures", "common_infections_of_childhood_ip_tbl.png"),
    here("figures", "common_infections_of_childhood_ed_tbl.png")
    )
)





webshot(
  "diagnosis_increased_tables.html",
  selector = ".diag-tbl-ed",
  zoom = 2,
  expand = c(0, 0, 0, -10),
  file = here("figures", "diag_tbl_ed_increased.png")
)

webshot(
  "diagnosis_increased_tables.html",
  selector = ".diag-tbl-ip",
  zoom = 2,
  expand = c(0, 0, 0, -10),
  file = here("figures", "diag_tbl_ip_increased.png")
)

webshot(
  "group_profile_table.html",
  selector = ".profile",
  zoom = 2,
  expand = c(0, 0, 0, 0),
  file = here("figures", "profile_groups_table.png")
)




