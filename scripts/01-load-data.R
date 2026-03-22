install.packages("readxl")
install.packages("RCzechia")
install.packages("sf")
install.packages("dplyr")
install.packages("rmapshaper")

library(readxl)
library(RCzechia)
library(sf)
library(dplyr)
library(rmapshaper)

isv_raw <- read_excel("data-raw/isv.xlsx")
obce <- obce_polygony()

isv <- obce |>
  mutate(KOD_OBEC = as.character(KOD_OBEC)) |>
  left_join(
    isv_raw |> mutate(obec_kod = as.character(obec_kod)),
    by = c("KOD_OBEC" = "obec_kod")
  )

# Vytvoření proměnných k celkovému ISV
isv <- isv |>
  mutate(
    `18_isv` = rowSums(across(all_of(c(
      "18_EXEK_abs", "18_EXEK_rel", "18_EXEK_lq",
      "18_PNB_abs", "18_PNB_rel", "18_PNB_lq",
      "18_PNZ_abs", "18_PNZ_rel", "18_PNZ_lq",
      "18_UOZ_abs", "18_UOZ_rel", "18_UOZ_lq",
      "18_ODCHOD_abs", "18_ODCHOD_rel", "18_ODCHOD_lq"
    ))), na.rm = TRUE),
    
    `24_isv` = rowSums(across(all_of(c(
      "24_EXEK_abs", "24_EXEK_rel", "24_EXEK_lq",
      "24_PNB_abs", "24_PNB_rel", "24_PNB_lq",
      "24_PNZ_abs", "24_PNZ_rel", "24_PNZ_lq",
      "24_UOZ_abs", "24_UOZ_rel", "24_UOZ_lq",
      "24_ODCHOD_abs", "24_ODCHOD_rel", "24_ODCHOD_lq"
    ))), na.rm = TRUE)
  )

saveRDS(isv, "data-processed/isv.rds")

isv_map_18 <- isv |>
  select(NAZ_OBEC, `18_isv`, geometry) |>
  st_transform(4326) |>
  ms_simplify(keep = 0.005, keep_shapes = TRUE)

saveRDS(isv_map_18, "data-processed/isv-map-18.rds")




