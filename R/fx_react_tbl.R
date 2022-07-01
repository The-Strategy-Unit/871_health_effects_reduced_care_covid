

# palette----
library("MetBrewer")
# met.brewer("VanGogh2")
pal <- MetPalettes$VanGogh2[[1]]


# enables omission of extreme values when scaling relative width of bars
# returns width of bar based on cell value, used by build_react_tbl
scale_value <- function(value, limit = 100, omit = 0, dat = dat) {
  if (value > limit) {
    width <- "100%"
  } else {
    width <- paste0(value * 100 / sort(dat$u, partial = nrow(dat) - omit)[nrow(dat) - omit], "%")
  }
}

# builds custom table showing increase in diagnosis frequency using reactable 
build_react_tbl <- function(dat, units = "Admis.", colwidth = c(300, 200, 200)) {
  
  bar_chart <- function(label, width = "100%", height = "14px", fill = "#000000", background = NULL) {
    bar <- div(style = list(background = fill, width = width, height = height))
    chart <- div(style = list(flexGrow = 1, marginLeft = "6px", background = background), bar)
    div(style = list(display = "flex", alignItems = "center"), label, chart)
  }

  reactable(
    dat,
    pagination = FALSE,
    defaultColGroup = colGroup(headerClass = "group-header"),
    defaultColDef = colDef(class = "cell", headerClass = "header"),
    defaultSorted = list(u = "desc"),
    columns = list(
      diagnm = colDef(
        name = "Diagnosis",
        align = "left",
        width = colwidth[1]
      ),
      p2 = colDef(
        name = paste(units, "w25\u2013w43 2022"),
        defaultSortOrder = "desc",
        align = "left",
        width = colwidth[2],
        style = list(fontFamily = "Fira Mono, sans serif", whiteSpace = "pre"),
        cell = function(value) {
          width <- paste0(value * 100 / max(dat$p2), "%")
          value <- prettyNum(value, big.mark = ",")
          value <- format(value, width = 6, justify = "right")
          bar_chart(value, width = width, fill = pal[1])
        },
      ),
      u = colDef(
        name = "Increase from 2021",
        defaultSortOrder = "desc",
        align = "left",
        width = colwidth[3],
        style = list(fontFamily = "Fira Mono, sans serif", whiteSpace = "pre"),
        cell = function(value) {
          width <- scale_value(value, dat = dat)
          value <- sprintf("%.3g", value)
          value <- format(value, width = 4, justify = "right")
          bar_chart(value, width = width, fill = pal[3], background = "#e1e1e1")
        }
      )
    ),
    class = "build-react-tbl",
  )
}

