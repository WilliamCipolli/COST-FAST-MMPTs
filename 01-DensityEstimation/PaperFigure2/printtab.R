library(xtable)
tab1 <- read_csv("summary.table.csv") |>
  mutate(across(is.numeric, ~ sprintf("%.3f", .x)))
tab2 <- read_csv("summary.table.csv") |>
  mutate(across(is.numeric, ~ sprintf("%.2e", .x)))

print(xtable(tab1[,-2], caption="..."), include.rownames = F)
print(xtable(tab2[,-2], caption="..."), include.rownames = F)
