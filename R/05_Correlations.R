wvs <- readRDS("data/final/wvs.rds")

library(dplyr)
library(tidyr)


#COMMENTA QUI E TUTTO QUELLO PRIMA
selected_countries <- c(32, 818, 231, 276, 380, 724, 752, 826, 840)

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

corr_table <- wvs %>%
  filter(country_codeISO %in% selected_countries) %>%
  group_by(country_codeISO) %>%
  group_modify(~{
    # counts (non-missing) + total per country
    counts <- tibble(
      n_total       = nrow(.x),
      n_polself     = sum(!is.na(.x$polselfplacement)),
      n_meritocracy = sum(!is.na(.x$meritocracy)),
      n_equality    = sum(!is.na(.x$equality)),
      n_trust_companies= sum(!is.na(.x$trust_companies)),
      n_trust_public= sum(!is.na(.x$trust_public))
    )
    
    # correlations (pairwise)
    corrs <- bind_rows(
      compute_corr(.x, "meritocracy")  %>% mutate(var = "meritocracy"),
      compute_corr(.x, "equality")     %>% mutate(var = "equality"),
      compute_corr(.x, "trust_companies") %>% mutate(var = "trust_companies"),
      compute_corr(.x, "trust_public") %>% mutate(var = "trust_public")
    ) %>%
      mutate(
        stars = add_stars(p_value),
        r_star = sprintf("%.3f%s", r, stars),
        p_fmt  = ifelse(is.na(p_value), NA_character_,
                        format.pval(p_value, digits = 3, eps = 0.001))
      ) %>%
      select(var, r_star, p_fmt, n_pair) %>%
      pivot_wider(
        names_from = var,
        values_from = c(r_star, p_fmt, n_pair),
        names_sep = "_"
      )
    
    bind_cols(counts, corrs)
  }) %>%
  ungroup() %>%
  
  # reorder exactly as requested
  select(
    country_codeISO,
    n_total,
    n_polself,
    n_meritocracy,
    n_equality,
    n_trust_companies,
    n_trust_public,
    r_star_meritocracy,
    r_star_equality,
    r_star_trust_companies,
    r_star_trust_public,
    p_fmt_meritocracy,
    p_fmt_equality,
    p_fmt_trust_companies,
    p_fmt_trust_public
  ) %>%
  arrange(country_codeISO)
  
  country_labels <- c(
    `32`  = "Argentina",
    `818` = "Egypt",
    `231` = "Ethiopia",
    `276` = "Germany",
    `380` = "Italy",
    `724` = "Spain",
    `752` = "Sweden",
    `826` = "United Kingdom",
    `840` = "United States"
  )
  
  corr_table <- corr_table %>%
    mutate(country = country_labels[as.character(country_codeISO)]) %>%
    select(
      country_codeISO,
      country,
      everything()
    )
  
View(corr_table)

names(wvs)




