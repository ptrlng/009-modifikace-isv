install.packages("readxl")
install.packages("RCzechia")
install.packages("sf")
install.packages("dplyr")
install.packages("rmapshaper")
install.packages("writexl")

library(readxl)
library(RCzechia)
library(sf)
library(dplyr)
library(rmapshaper)
library(writexl)

isv_raw <- read_excel("data-raw/isv.xlsx")
obce <- obce_polygony()

isv <- obce |>
  mutate(KOD_OBEC = as.character(KOD_OBEC)) |>
  left_join(
    isv_raw |> mutate(obec_kod = as.character(obec_kod)),
    by = c("KOD_OBEC" = "obec_kod")
  )

# názvy výsledných proměnných
isv_vars <- c(
  "18_orig",
  "18_bez_abs",
  "18_bez_lq",
  "18_bez_abs_a_lq",
  "18_bez_pnz_a_pnb",
  "18_bez_pnz_a_pnb_2krat_exek_a_uoz",
  "18_bez_lq_a_bez_pnz_a_pnb",
  "24_orig",
  "24_bez_abs",
  "24_bez_lq",
  "24_bez_abs_a_lq",
  "24_bez_pnz_a_pnb",
  "24_bez_pnz_a_pnb_2krat_exek_a_uoz",
  "24_bez_lq_a_bez_pnz_a_pnb"
)

# Vytvoření proměnných k celkovému ISV
isv <- isv |>
  mutate(
    `18_orig` = rowSums(across(all_of(c(
      "18_EXEK_abs", "18_EXEK_rel", "18_EXEK_lq",
      "18_PNB_abs", "18_PNB_rel", "18_PNB_lq",
      "18_PNZ_abs", "18_PNZ_rel", "18_PNZ_lq",
      "18_UOZ_abs", "18_UOZ_rel", "18_UOZ_lq",
      "18_ODCHOD_abs", "18_ODCHOD_rel", "18_ODCHOD_lq"
    ))), na.rm = TRUE),
    
    `24_orig` = rowSums(across(all_of(c(
      "24_EXEK_abs", "24_EXEK_rel", "24_EXEK_lq",
      "24_PNB_abs", "24_PNB_rel", "24_PNB_lq",
      "24_PNZ_abs", "24_PNZ_rel", "24_PNZ_lq",
      "24_UOZ_abs", "24_UOZ_rel", "24_UOZ_lq",
      "24_ODCHOD_abs", "24_ODCHOD_rel", "24_ODCHOD_lq"
    ))), na.rm = TRUE),
    
    `18_bez_pnz_a_pnb` = rowSums(across(all_of(c(
      "18_EXEK_abs", "18_EXEK_rel", "18_EXEK_lq",
      "18_UOZ_abs", "18_UOZ_rel", "18_UOZ_lq",
      "18_ODCHOD_abs", "18_ODCHOD_rel", "18_ODCHOD_lq"
    ))), na.rm = TRUE) * 30 / 18,
    
    `24_bez_pnz_a_pnb` = rowSums(across(all_of(c(
      "24_EXEK_abs", "24_EXEK_rel", "24_EXEK_lq",
      "24_UOZ_abs", "24_UOZ_rel", "24_UOZ_lq",
      "24_ODCHOD_abs", "24_ODCHOD_rel", "24_ODCHOD_lq"
    ))), na.rm = TRUE) * 30 / 18,
    
    `18_bez_pnz_a_pnb_2krat_exek_a_uoz` =
      2 * rowSums(across(all_of(c(
        "18_EXEK_abs", "18_EXEK_rel", "18_EXEK_lq"
      ))), na.rm = TRUE) +
      2 * rowSums(across(all_of(c(
        "18_UOZ_abs", "18_UOZ_rel", "18_UOZ_lq"
      ))), na.rm = TRUE) +
      rowSums(across(all_of(c(
        "18_ODCHOD_abs", "18_ODCHOD_rel", "18_ODCHOD_lq"
      ))), na.rm = TRUE),
    
    `24_bez_pnz_a_pnb_2krat_exek_a_uoz` =
      2 * rowSums(across(all_of(c(
        "24_EXEK_abs", "24_EXEK_rel", "24_EXEK_lq"
      ))), na.rm = TRUE) +
      2 * rowSums(across(all_of(c(
        "24_UOZ_abs", "24_UOZ_rel", "24_UOZ_lq"
      ))), na.rm = TRUE) +
      rowSums(across(all_of(c(
        "24_ODCHOD_abs", "24_ODCHOD_rel", "24_ODCHOD_lq"
      ))), na.rm = TRUE),
    
    `18_bez_lq` = rowSums(across(all_of(c(
      "18_EXEK_abs", "18_EXEK_rel",
      "18_PNB_abs", "18_PNB_rel",
      "18_PNZ_abs", "18_PNZ_rel",
      "18_UOZ_abs", "18_UOZ_rel",
      "18_ODCHOD_abs", "18_ODCHOD_rel"
    ))), na.rm = TRUE) * 30 / 20,
    
    `24_bez_lq` = rowSums(across(all_of(c(
      "24_EXEK_abs", "24_EXEK_rel",
      "24_PNB_abs", "24_PNB_rel",
      "24_PNZ_abs", "24_PNZ_rel",
      "24_UOZ_abs", "24_UOZ_rel",
      "24_ODCHOD_abs", "24_ODCHOD_rel"
    ))), na.rm = TRUE) * 30 / 20,
    
    `18_bez_abs` = rowSums(across(all_of(c(
      "18_EXEK_rel", "18_EXEK_lq",
      "18_PNB_rel", "18_PNB_lq",
      "18_PNZ_rel", "18_PNZ_lq",
      "18_UOZ_rel", "18_UOZ_lq",
      "18_ODCHOD_rel", "18_ODCHOD_lq"
    ))), na.rm = TRUE) * 30 / 20,
    
    `24_bez_abs` = rowSums(across(all_of(c(
      "24_EXEK_rel", "24_EXEK_lq",
      "24_PNB_rel", "24_PNB_lq",
      "24_PNZ_rel", "24_PNZ_lq",
      "24_UOZ_rel", "24_UOZ_lq",
      "24_ODCHOD_rel", "24_ODCHOD_lq"
    ))), na.rm = TRUE) * 30 / 20,
    
    `18_bez_abs_a_lq` = rowSums(across(all_of(c(
      "18_EXEK_rel",
      "18_PNB_rel",
      "18_PNZ_rel",
      "18_UOZ_rel",
      "18_ODCHOD_rel"
    ))), na.rm = TRUE) * 30 / 10,
    
    `24_bez_abs_a_lq` = rowSums(across(all_of(c(
      "24_EXEK_rel",
      "24_PNB_rel",
      "24_PNZ_rel",
      "24_UOZ_rel",
      "24_ODCHOD_rel"
    ))), na.rm = TRUE) * 30 / 10,
    
    `18_bez_lq_a_bez_pnz_a_pnb` = rowSums(across(all_of(c(
      "18_EXEK_abs", "18_EXEK_rel",
      "18_UOZ_abs", "18_UOZ_rel",
      "18_ODCHOD_abs", "18_ODCHOD_rel"
    ))), na.rm = TRUE) * 30 / 12,
    
    `24_bez_lq_a_bez_pnz_a_pnb` = rowSums(across(all_of(c(
      "24_EXEK_abs", "24_EXEK_rel",
      "24_UOZ_abs", "24_UOZ_rel",
      "24_ODCHOD_abs", "24_ODCHOD_rel"
    ))), na.rm = TRUE) * 30 / 12
  )

saveRDS(isv, "data-processed/isv.rds")

isv_map <- isv |>
  select(NAZ_OBEC, all_of(isv_vars), geometry) |>
  st_transform(4326) |>
  ms_simplify(keep = 0.005, keep_shapes = TRUE)

saveRDS(isv_map, "data-processed/isv-map.rds")

# Vytvoření souboru ke stažení
isv_download <- isv |>
  st_drop_geometry() |>
  select(
    KOD_OBEC,
    NAZ_OBEC,
    NAZ_ORP,
    NAZ_CZNUTS3,
    all_of(isv_vars)
  ) |>
  rename(
    NAZ_KRAJ = NAZ_CZNUTS3
  )

write_xlsx(isv_download, "data-download/isv_download.xlsx")