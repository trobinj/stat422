library(tidyverse)
library(survey)
library(SDaA)
library(mice)
library(mitools)

set.seed(111)

myotters <- otters |> 
  mutate(habtype = case_when(
    habitat == 1 ~ "cliffs",
    habitat == 2 ~ "agricultural",
    habitat == 3 ~ "peat",
    habitat == 4 ~ "notpeat")) |>
  mutate(N = case_when(
    habtype == "cliffs" ~ 89,
    habtype == "agricultural" ~ 61,
    habtype == "peat" ~ 40,
    habtype == "notpeat" ~ 47)) |>
  mutate(area = round(rnorm(n(), 5.5, 0.5), 2)) |> 
  select(-habitat)

myotters.missing <- myotters |> 
  mutate(habtype = ifelse(runif(n()) < 0.25, NA, habtype)) |>
  mutate(area = ifelse(runif(n()) < 0.25, NA, area)) |> 
  mutate(habtype = factor(habtype))

pmat <- matrix(0, 5, 5)
pmat[3,2] <- 1
pmat[3,5] <- 1
pmat[5,2] <- 1
pmat[5,3] <- 1

myotters.imputed <- mice(myotters.missing, predictorMatrix = pmat, m = 5)
myotters.imputed <- imputationList(complete(myotters.imputed, action = "all"))

head(myotters.missing)
for (i in 1:5) {
  print(head(myotters.imputed$imputations[[i]]))
}

for (i in 1:5) {
  myotters.imputed$imputations[[i]] <- myotters.imputed$imputations[[i]] |> 
    mutate(N = case_when(
      habtype == "cliffs" ~ 89,
      habtype == "agricultural" ~ 61,
      habtype == "peat" ~ 40,
      habtype == "notpeat" ~ 47))
}

mydesign <- svydesign(id = ~1, strata = ~habtype, fpc = ~N, data = myotters)
svytotal(x = ~holts, design = mydesign)

mydesign.imputed <- svydesign(id = ~1, strata = ~habtype, fpc = ~N, data = myotters.imputed)
total.each <- with(mydesign.imputed, svytotal(x = ~holts))
MIcombine(total.each)


