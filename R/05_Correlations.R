library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)

wvs <- readRDS("data/final/wvs.rds")

# Prelimiary test for reframing the paper--------------------------------------------------------------------
#1) Corr(polselfplacement, Meritocracy), Corr(polselfplacement, Inequality aversion), Corr(polselfplacement, TrustCompanies), Corr(polselfplacement, Trust in public institutions)
  #remember that trust_market is defined also for wave 5 (even if wafe 5 does not have the variable trust_banks recorded) see 02_variable_preparation.R row 115
selected_countries <- c(
  32,   # Argentina
  50,   # Bangladesh
  76,   # Brazil
  818,  # Egypt
  231,  # Ethiopia
  276,  # Germany
  356,  # India
  360,  # Indonesia
  380,  # Italy
  392,  # Japan
  484,  # Mexico
  566,  # Nigeria
  586,  # Pakistan
  616,  # Poland
  643,  # Russia
  710,  # South Africa
  410,  # South Korea
  724,  # Spain
  752,  # Sweden
  826,  # UK
  840   # USA
)

add_stars <- function(p) {
  ifelse(p < 0.001, "***",
         ifelse(p < 0.01,  "**",
                ifelse(p < 0.05,  "*", "")))
}

compute_corr <- function(data, y) {
  ok <- complete.cases(data$polselfplacement, data[[y]])
  if (sum(ok) < 3) {
    return(tibble(r = NA_real_, p_value = NA_real_, n_pair = sum(ok)))
  }
  ct <- cor.test(data$polselfplacement[ok], data[[y]][ok], method = "pearson")
  tibble(r = unname(ct$estimate), p_value = ct$p.value, n_pair = sum(ok))
}

# --- Build country-by-row table (with r + stars + N in the same cell)
corr_table <- wvs %>%
  filter(country_codeISO %in% selected_countries) %>%
  group_by(country_codeISO) %>%
  group_modify(~{
    counts <- tibble(
      n_total        = nrow(.x),
      n_polself      = sum(!is.na(.x$polselfplacement)),
      n_meritocracy  = sum(!is.na(.x$meritocracy)),
      n_equality     = sum(!is.na(.x$equality)),
      n_trust_market = sum(!is.na(.x$trust_market)),
      n_trust_public = sum(!is.na(.x$trust_public))
    )
    
    corrs <- bind_rows(
      compute_corr(.x, "meritocracy")  %>% mutate(var = "meritocracy"),
      compute_corr(.x, "equality")     %>% mutate(var = "equality"),
      compute_corr(.x, "trust_market") %>% mutate(var = "trust_market"),
      compute_corr(.x, "trust_public") %>% mutate(var = "trust_public")
    ) %>%
      mutate(
        stars    = add_stars(p_value),
        r_star   = sprintf("%.3f%s", r, stars),
        r_star_n = paste0(r_star, " (N=", n_pair, ")"),
        p_fmt    = ifelse(is.na(p_value), NA_character_,
                          format.pval(p_value, digits = 3, eps = 0.001))
      ) %>%
      select(var, r_star_n, p_fmt) %>%
      pivot_wider(
        names_from  = var,
        values_from = c(r_star_n, p_fmt),
        names_sep   = "_"
      )
    
    bind_cols(counts, corrs)
  }) %>%
  ungroup() %>%
  # reorder (update names to r_star_n_*)
  select(
    country_codeISO,
    n_total,
    n_polself,
    n_meritocracy,
    n_equality,
    n_trust_market,
    n_trust_public,
    r_star_n_meritocracy,
    r_star_n_equality,
    r_star_n_trust_market,
    r_star_n_trust_public,
    p_fmt_meritocracy,
    p_fmt_equality,
    p_fmt_trust_market,
    p_fmt_trust_public
  ) %>%
  arrange(country_codeISO)

# --- Add country names
country_labels <- c(
  `32`  = "Argentina",
  `50`  = "Bangladesh",
  `76`  = "Brazil",
  `818` = "Egypt",
  `231` = "Ethiopia",
  `276` = "Germany",
  `356` = "India",
  `360` = "Indonesia",
  `380` = "Italy",
  `392` = "Japan",
  `484` = "Mexico",
  `566` = "Nigeria",
  `586` = "Pakistan",
  `616` = "Poland",
  `643` = "Russia",
  `710` = "South Africa",
  `410` = "South Korea",
  `724` = "Spain",
  `752` = "Sweden",
  `826` = "UK",
  `840` = "USA"
)

corr_table <- corr_table %>%
  mutate(country = country_labels[as.character(country_codeISO)]) %>%
  select(country_codeISO, country, everything())

View(corr_table)

#transposed table
corr_table_wide_countries <- corr_table %>%
  select(-country_codeISO) %>%   # optional (keep just country names)
  column_to_rownames("country") %>%
  t() %>%
  as.data.frame()
View(corr_table_wide_countries)


#2)Computing EIC index

compute_r <- function(data, y) {
  ok <- complete.cases(data$polselfplacement, data[[y]])
  if (sum(ok) < 3) return(NA_real_)
  cor(data$polselfplacement[ok], data[[y]][ok])
}

EIC_table <- wvs %>%
  filter(country_codeISO %in% selected_countries) %>%
  group_by(country_codeISO) %>%
  summarise(
    r_meritocracy      = compute_r(cur_data(), "meritocracy"),
    r_equality         = compute_r(cur_data(), "equality"),
    r_trust_market  = compute_r(cur_data(), "trust_market"),
    r_trust_public     = compute_r(cur_data(), "trust_public"),
    EIC = mean(abs(c(
      r_meritocracy,
      r_equality,
      r_trust_market,
      r_trust_public
    )), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(country_codeISO)

#insert country names in the table
EIC_table <- EIC_table %>%
  mutate(country = country_labels[as.character(country_codeISO)]) %>%
  relocate(country, .after = country_codeISO)

EIC_table


#3) Comupte correlatoin with privatisation
compute_r <- function(data, y) {
  ok <- complete.cases(data$polselfplacement, data[[y]])
  if (sum(ok) < 3) return(NA_real_)
  cor(data$polselfplacement[ok], data[[y]][ok], method = "pearson")
}

Y_table <- wvs %>%
  filter(country_codeISO %in% selected_countries) %>%
  group_by(country_codeISO) %>%
  summarise(
    Yc = compute_r(cur_data(), "privatisation"),
    n_pair_privatisation = sum(complete.cases(polselfplacement, privatisation)),
    .groups = "drop"
  ) %>%
  arrange(country_codeISO) %>%
  mutate(country = country_labels[as.character(country_codeISO)]) %>%
  relocate(country, .after = country_codeISO)
#Merge Yc with the EIC table
regression_data <- Y_table %>%
  left_join(EIC_table %>% select(country_codeISO, EIC), by = "country_codeISO")
print(regression_data)


#3)Cross-country regression and visualize the relationship in a scatter plot

#Individual-level regression: privatisation_ic=α+βEIC_c
# wvs_with_EIC <- wvs %>%
#   left_join(EIC_table %>% select(country_codeISO, EIC),
#             by = "country_codeISO")
# 
# model <- lm(privatisation ~ EIC, data = wvs_with_EIC)
# 
# library(sandwich)
# library(lmtest)
# 
# coeftest(model, vcov = vcovCL(model, cluster = ~country_codeISO))


#Country-level regression: Yc= Corr(Ideology, Privatization) =α+βEIC_c
# Mean privatisation per country

model_country <- lm(Yc ~ EIC, data = regression_data)
summary(model_country)

#Preparing for the graph: acronyms
country_acronyms <- c(
  "Argentina" = "ARG",
  "Bangladesh" = "BGD",
  "Brazil" = "BRA",
  "Egypt" = "EGY",
  "Ethiopia" = "ETH",
  "Germany" = "DEU",
  "India" = "IND",
  "Indonesia" = "IDN",
  "Italy" = "ITA",
  "Japan" = "JPN",
  "Mexico" = "MEX",
  "Nigeria" = "NGA",
  "Pakistan" = "PAK",
  "Poland" = "POL",
  "Russia" = "RUS",
  "South Africa" = "ZAF",
  "South Korea" = "KOR",
  "Spain" = "ESP",
  "Sweden" = "SWE",
  "UK" = "GBR",
  "USA" = "USA"
)

regression_data <- regression_data %>%
  mutate(country_code = country_acronyms[country])


#scatterplot
library(ggrepel)

ggplot(regression_data, aes(x = EIC, y = Yc, label = country_code)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_text_repel(size = 4) +
  labs(
    x = "EIC index",
    y = "Corr(Ideology, Privatisation)",
    title = "Country-level relationship between EIC and ideological correlation with privatisation"
  ) +
  theme_minimal()

getwd()

