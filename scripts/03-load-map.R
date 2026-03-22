install.packages("RCzechia")
install.packages("sf")

library(RCzechia)
library(sf)
library(dplyr)

obce <- obce_polygony()
isv <- readRDS("data-processed/isv.rds")

obce_isv <- obce |>
  mutate(KOD_OBEC = as.character(KOD_OBEC)) |>
  left_join(
    isv |> mutate(obec_kod = as.character(obec_kod)),
    by = c("KOD_OBEC" = "obec_kod")
  )

nrow(obce_isv)
sum(is.na(obce_isv$obec))
names(obce_isv)