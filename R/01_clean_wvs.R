#Cleaning and merging the data
library(haven)
library(dplyr)


# helper: legge il primo .dta che trova in una cartella
read_first_dta <- function(folder) {
  f <- list.files(folder, pattern = "\\.dta$", full.names = TRUE)
  if (length(f) == 0) stop("No .dta found in: ", folder)
  message("Reading: ", f[1])
  read_dta(f[1])
}

# carica le 3 waves
wv5 <- read_first_dta("data/raw/wv5") %>% mutate(wave = 5)
wv6 <- read_first_dta("data/raw/wv6") %>% mutate(wave = 6)
wv7 <- read_first_dta("data/raw/wv7") %>% mutate(wave = 7)

#Have a look at the data
glimpse(wv5)
glimpse(wv6)
glimpse(wv7)

dim(wv5)
dim(wv6)
dim(wv7)


#Renaming, merging and harmonising data
map_wv5 <- c(
  polselfplacement = "V114",
  pref_for_equality = "V116",
  pref_privatisation = "V117",
  pref_redistribution = "V118",
  pref_competition = "V119",
  happiness = "V10",
  life_satisfaction = "V22",
  HH_finacialsatisfaction = "V68",
  Trust_people = "V23",
  Trust_GOV = "V138",
  Trust_politicalParties = "V139",
  Trust_BigCompanies = "V142",
  Trust_LaborUnions = "V135",
  meritocracy = "V120",
  importance_democracy = "V162",
  sex = "V235",
  age = "V237",
  Respondent_education_level = "V238",
  Social_class = "V252",
  Income_group = "V253",
  Trust_police = "V136",
  Trust_JusticeSystem = "V137",
  Trust_Parliament = "V140",
  Trust_Civil_Service = "V141",
  country_codeISO = "V2",
  InterviewNumber = "V3"
)


map_wv6 <- c(
  polselfplacement = "V95",
  pref_for_equality = "V96",
  pref_privatisation = "V97",
  pref_redistribution = "V98",
  pref_competition = "V99",
  happiness = "V10",
  life_satisfaction = "V23",
  HH_finacialsatisfaction = "V59",
  Trust_people = "V24",
  Trust_GOV = "V115",
  Trust_politicalParties = "V116",
  Trust_BigCompanies = "V120",
  Trust_Banks = "V121",
  Trust_LaborUnions = "V112",
  meritocracy = "V100",
  importance_democracy = "V140",
  sex = "V240",
  age = "V242",
  Respondent_education_level = "V248",
  Social_class = "V238",
  Income_group = "V239",
  Trust_police = "V113",
  Trust_JusticeSystem = "V114",
  Trust_Parliament = "V117",
  Trust_Civil_Service = "V118",
  country_codeISO = "V2",
  InterviewNumber = "V3"
)


map_wv7 <- c(
  polselfplacement = "Q240",
  pref_for_equality = "Q106",
  pref_privatisation = "Q107",
  pref_redistribution = "Q108",
  pref_competition = "Q109",
  happiness = "Q46",
  life_satisfaction = "Q49",
  HH_finacialsatisfaction = "Q50",
  Trust_people = "Q57",
  Trust_GOV = "Q71",
  Trust_politicalParties = "Q72",
  Trust_BigCompanies = "Q77",
  Trust_Banks = "Q78",
  Trust_LaborUnions = "Q68",
  meritocracy = "Q110",
  importance_democracy = "Q250",
  sex = "Q260",
  age = "Q262",
  Respondent_education_level = "Q275",
  Mother_education_level = "Q277",
  Father_education_level = "Q278",
  Social_class = "Q287",
  Income_group = "Q288",
  Trust_police = "Q69",
  Trust_JusticeSystem = "Q70",
  Trust_Parliament = "Q73",
  Trust_Civil_Service = "Q74",
  Trust_Elections = "Q76",
  country_codeISO = "B_COUNTRY",
  InterviewNumber = "D_INTERVIEW"
)

#Harmonize within-wave (rename, add missing cols, build uid)
harmonize_wave <- function(df, mapping) {
  mapping_present <- mapping[mapping %in% names(df)]
  # rename: new_name = old_name
  out <- df %>%
    rename(!!!mapping_present) %>%
    select(wave, any_of(names(mapping)))
  
  out
}

wv5_core <- harmonize_wave(wv5, map_wv5)
wv6_core <- harmonize_wave(wv6, map_wv6)
wv7_core <- harmonize_wave(wv7, map_wv7)

#put NA for variables non recorded in some waves
needed_cols <- unique(c("wave", names(map_wv5), names(map_wv6), names(map_wv7)))

add_missing_cols <- function(df, cols) {
  miss <- setdiff(cols, names(df))
  if (length(miss) > 0) df[miss] <- NA
  df
}

wv5_core <- add_missing_cols(wv5_core, needed_cols)
wv6_core <- add_missing_cols(wv6_core, needed_cols)
wv7_core <- add_missing_cols(wv7_core, needed_cols)

#convert lables to numeric to avoid warnings
wv5_core <- wv5_core %>%
  mutate(across(where(haven::is.labelled), as.numeric))

wv6_core <- wv6_core %>%
  mutate(across(where(haven::is.labelled), as.numeric))

wv7_core <- wv7_core %>%
  mutate(across(where(haven::is.labelled), as.numeric))

#Stack waves
wvs_567_core <- bind_rows(wv5_core, wv6_core, wv7_core)

# Clean WVS special missing codes
to_na_wvs <- function(x) {
  x <- na_if(x, -1)
  x <- na_if(x, -2)
  x <- na_if(x, -3)
  x <- na_if(x, -4)
  x <- na_if(x, -5)
  x
}

wvs_567_core <- wvs_567_core %>%
  mutate(across(where(is.numeric), to_na_wvs))

#Create stable unique ID
wvs_567_core <- wvs_567_core %>%
  mutate(
    row_id = row_number(),
    uid = paste0("w", wave, "_", row_id)
  )

anyDuplicated(wvs_567_core$uid)
stopifnot(anyDuplicated(wvs_567_core$uid) == 0)

#Education harmonization
wvs_567_core <- wvs_567_core %>%
  mutate(
    edu_raw = as.numeric(Respondent_education_level),
    edu_3 = case_when(
      wave %in% c(5, 6) & edu_raw %in% 1:3 ~ "low",
      wave %in% c(5, 6) & edu_raw %in% 4:7 ~ "medium",
      wave %in% c(5, 6) & edu_raw %in% 8:9 ~ "high",
      wave == 7 & edu_raw %in% 0:1 ~ "low",
      wave == 7 & edu_raw %in% 2:4 ~ "medium",
      wave == 7 & edu_raw %in% 5:8 ~ "high",
      TRUE ~ NA_character_
    ),
    edu_3 = factor(edu_3, levels = c("low", "medium", "high"))
  )


#Save database
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(wvs_567_core, "data/processed/wvs_567_core.rds")








