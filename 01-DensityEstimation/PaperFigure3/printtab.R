library(xtable)
tab1 <- read_csv("summary.table.csv") |>
  mutate(across(is.numeric, ~ sprintf("%.3f", .x)))

print(xtable(tab1[,-2], caption="..."), include.rownames = F)

