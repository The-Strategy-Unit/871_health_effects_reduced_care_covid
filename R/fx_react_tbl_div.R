

# outputs html required to place react table in div container
react_tbl_div <- function(rtbl = tbl, selector = "rtbl", title = "Table title") {
  tagList(
    tags$div(
      class = paste0(selector),
      div(
        class = "rtbl-title",
        paste0(title)
      ), htmlwidgets::appendContent(
        rtbl,
        htmltools::tags$span(
          class = "rtbl-src",
          p("Source(s): Strategy Unit analysis; National Commissioning Dataset Repository.")
        )
      )
    )
  )
}

# with h2 header
react_tbl_div_h2 <- function(rtbl = tbl, selector = "rtbl", h2title = "H2-title", title = "Table title") {
  tagList(
    tags$div(
      class = paste0(selector),
      div(
        class = "rtbl-title",
        tags$h2(paste0(h2title)),
        paste0(title), 
      ), htmlwidgets::appendContent(
        rtbl,
        htmltools::tags$span(
          class = "rtbl-src",
          p("Source(s): Strategy Unit analysis; National Commissioning Dataset Repository.")
        )
      )
    )
  )
}

# source Google fonts
g_fonts <- tagList(
  htmltools::tags$link(
    href = "https://fonts.googleapis.com/css?family=Work+Sans:400,700|Fira+Mono&display=fallback",
    rel = "stylesheet"
    )
  )

