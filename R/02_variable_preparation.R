library(dplyr)

#Load database
wvs_567_core <- readRDS("data/processed/wvs_567_core.rds")

#making the scales of the varibales consistent, i.e. higher values menans higher support for the variabele
#so this means inverting some scales

wvs <- wvs_567_core %>%
  mutate(
    # 1–10 scales: reverse so higher = more pro-... (as you want)
    privatisation = 11 - pref_privatisation,
    redistribution = 11 - pref_redistribution,
    equality = 11 - pref_for_equality,
    competition = 11 - pref_competition,
    meritocracy = 11 - meritocracy,
    
    # happiness is 1..4 (1=very happy, 4=not at all) -> reverse so higher = happier
    happiness_rev = 5 - happiness,
    
    # social class is 1..5 (1=upper, 5=lower) -> reverse so higher = higher class
    social_class_rev = 6 - Social_class,
    
    # trust in institutions is 1..4 (1=a great deal, 4=none) -> reverse so higher = more trust
    trust_gov = 5 - Trust_GOV,
    trust_parties = 5 - Trust_politicalParties,
    trust_companies = 5 - Trust_BigCompanies,
    trust_banks = 5 - Trust_Banks,
    trust_labUnions = 5 - Trust_LaborUnions,
    trust_police = 5 - Trust_police,
    trust_justice = 5 - Trust_JusticeSystem,
    trust_parliament = 5 - Trust_Parliament,
    trust_civil_service = 5 - Trust_Civil_Service,
    trust_elections = 5 - Trust_Elections,
    
    # generalized trust (binary): 1=trusted, 0=careful
    trust_ppl = case_when(
      Trust_people == 1 ~ 1,
      Trust_people == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # sex dummy: 1=female, 0=male
    female = case_when(
      sex == 2 ~ 1,
      sex == 1 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  # Keep originals that are already in the “right direction”
  # and keep only the NEW corrected versions for the others
  select(
    uid, row_id, wave, country_codeISO, InterviewNumber,
    
    # keep “already OK” variables
    polselfplacement,
    life_satisfaction,
    HH_finacialsatisfaction,
    importance_democracy,
    age,
    Income_group,
    edu_3,
    Mother_education_level,
    Father_education_level,
    
    # keep corrected versions (use these in analysis)
    privatisation, redistribution, equality, competition,
    meritocracy,
    happiness_rev,
    social_class_rev,
    trust_gov, trust_parties, trust_companies, trust_banks, trust_labUnions,
    trust_police, trust_justice, trust_parliament, trust_civil_service, trust_elections,
    trust_ppl,
    female
  )


# uid is a substantive unique identifier constructed from wave, country, and interview number,
# and uniquely identifies each respondent across datasets.
# row_id is only a technical row counter indicating the position of an observation in the dataset;
# it changes if the data are sorted or filtered and does not identify individuals.

names(wvs)

#Create categorical var for ideology
wvs <- wvs %>%
  mutate(
    ideology = case_when(
      polselfplacement >= 1 & polselfplacement <= 3 ~ "Left",
      polselfplacement >= 4 & polselfplacement <= 6 ~ "Center",
      polselfplacement >= 7 & polselfplacement <= 10 ~ "Right",
      TRUE ~ NA_character_
    ),
    ideology = factor(ideology, levels = c("Left", "Center", "Right"))
  )


#alternative coding for ideology
wvs <- wvs %>%
  mutate(
    ideology_alt = case_when(
      polselfplacement >= 1 & polselfplacement <= 4 ~ "Left",
      polselfplacement >= 5 & polselfplacement <= 6 ~ "Center",
      polselfplacement >= 7 & polselfplacement <= 10 ~ "Right",
      TRUE ~ NA_character_
    ),
    ideology_alt = factor(ideology_alt, levels = c("Left", "Center", "Right"))
  )



#constructing trust variables
wvs <- wvs %>%
  mutate(
    # Trust in Market Actors = average trust in banks and big companies
    #PROBEM: FOR WAVE 5 THE VARIABLE IS NOT RECORDED!
    trust_market = rowMeans(
      cbind(trust_banks, trust_companies),
      na.rm = TRUE
    ),
    
    # Trust in Public Actors = average trust in government, parliament, civil service, justice system, police
    trust_public = rowMeans(
      cbind(trust_gov, trust_parliament, trust_civil_service, trust_justice, trust_police),
      na.rm = TRUE
    ),
    
    # If a respondent has all components missing, rowMeans returns NaN -> convert to NA
    trust_market = ifelse(is.nan(trust_market), NA_real_, trust_market),
    trust_public = ifelse(is.nan(trust_public), NA_real_, trust_public)
  )


#creating relative trust index
wvs <- wvs %>%
  mutate(
    # Relative trust index:
    # Positive values = more trust in market actors than in public actors
    # Negative values = more trust in public actors than in market actors
    trust_market_relative = trust_market - trust_public
  )

names(wvs)



saveRDS(wvs, "data/final/wvs.rds")

View(wvs)
dim(wvs)
