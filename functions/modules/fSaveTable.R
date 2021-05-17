fSaveTable <- function(filename = filename, data = x, caption = caption, myFactor = f, latexLabel = l, firstCol = "") {
  myvar <- sym(myFactor)
  tab <- data %>%
    mutate(
      n = fPrettyNum(n, 0),
      np = fPrettyNum(np),
      middle = fPrettyNum(middle),
      ci = paste0("(", fPrettyNum(lower), " - ", fPrettyNum(upper), ")")
    ) %>%
    select(
      {{ myvar }}, n, np, middle, ci
    ) %>%
    knitr::kable(
      "latex",
      caption = caption,
      label = latexLabel,
      booktabs = T,
      escape = F,
      linesep = "", # https://stackoverflow.com/questions/45409750/get-rid-of-addlinespace-in-kable
      col.names = c(firstCol, "Imkereien [\\textit{n}]", "[\\%]", "Verlustrate [\\%]", "95\\% CI [\\%]"),
      align = c("l", rep("r", 4))
    ) %>%
    kable_styling(latex_options = "HOLD_position", font_size = 10)

  begin <- 1
  for (i in unique(data$year)) {
    end <- begin + nrow(data %>% filter(year == i)) - 1
    tab <- tab %>% pack_rows(i, begin, end)
    begin <- end + 1
  }

  # small bugfix as pack_rows introduces an linespaceing which breaks midrule
  tab <- sub("midrule{}", "midrule", tab, fixed = T)

  tab %>% save_kable(paste0("output/tables/", filename, ".tex"))

  invisible(tab)
}