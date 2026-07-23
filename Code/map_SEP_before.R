library(progressr)
library(tidyverse)
library(readxl)
library(writexl)
library(rio)
library(xtable)
library(here)
library(gtsummary)
library(glue)
library(scales)
library(patchwork)
library(stargazer)
library(sandwich)
library(lmtest)
library(AER)
library(car)
library(haven)
library(fixest) 
library(sf)
library(did)
library(rdrobust)
library(TwoWayFEWeights)
library(Synth)
library(fredr)
library(plm)
library(openalexR)
library(purrr)
library(np)
library(furrr)
library(countrycode)
library(WDI)
library(typstable)
library(mgcv)
library(FactoMineR)
library(factoextra)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)
library(MatchIt)
library(cobalt)
library(ggh4x)
library(rvest)
library(purrr)
library(httr2)
library(jsonlite)
library(stringr)

df <- read_dta('Data/Data_SEP_FSA.dta')

df_sep <- df %>%
  filter(SEP == 1) %>%
  arrange(ID)

# Monthly evolution of SEP AND non-SEP actions ---- 

df <- df %>%
  group_by(year, month) %>%
  mutate(
    n_actions_sep = n_distinct(ID[SEP == 1]),
    n_actions_nsep = n_distinct(ID[SEP == 0])
  ) %>%
  ungroup()

plot_sep_nsep_month <- ggplot(data = df, aes(x = Date)) +
  geom_line(
    data = df,
    aes(y = n_actions_sep), 
    linewidth = 1.2, 
    color = '#003A70'
  ) + 
    geom_line(
      data = df, 
      aes(y = n_actions_nsep),
      linewidth = 1.2,
      color = '#6BAED6'
  ) + 
    scale_x_date(
      date_breaks = '3 months',
      date_labels = '%b\n%Y'
    ) +
    theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey85"),
    panel.grid.minor.y = element_blank()
  )

plot_sep_nsep_month

ggsave(
  'Output/plot_sep_nsep_month.jpeg',
  plot_sep_nsep_month, 
  width = 12,
  height = 7,
  dpi = 300
)

# Share of SEP cases among all UPC actions ----

quarterly_juris <- df %>%
  mutate(
    jurisdiction = sub(".*[-]\\s*", "", Courtdivision),
    quarter = floor_date(Date, "quarter")
  ) %>%
  group_by(jurisdiction, quarter) %>%
  summarise(
    n_sep = n_distinct(ID[SEP == 1]),
    n_total = n_distinct(ID),
    share_sep = n_sep / n_total,
    .groups = "drop"
  ) %>%
  mutate(
    highlight = share_sep == 1
  )

plot_share_juris_sup0 <- ggplot(
  quarterly_juris %>% 
    group_by(jurisdiction) %>%
    filter(any(n_sep > 0)),
  aes(x = quarter, y = share_sep, fill = highlight)
) +
  geom_col(
  ) +
  geom_text(
    aes(label = ifelse(
      share_sep > 0 & share_sep < 1,
      round(share_sep * 100, 0),
      ""
    )),
    vjust = -0.8, 
    size = 2.5,
    fontface = 'bold'
  ) + 
  scale_x_date(
  date_breaks = "3 months",
  labels = function(x) {
    ifelse(
      lubridate::quarter(x) == 1,
      paste0("Q1\n", lubridate::year(x)),
      paste0("Q", lubridate::quarter(x))
    )
  }
) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    labels = scales::percent
  ) +
  scale_fill_manual(
    values = c(
      'TRUE' = '#003A70',
      'FALSE' = '#003A70'
    )
  ) + 
  facet_wrap(~ jurisdiction,
            axes = 'all') +
  theme_minimal() +
  theme(
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.grid.major.y = element_line(colour = "grey85"),
  panel.grid.minor.y = element_blank(),
  legend.position = 'none',
  axis.title.x = element_blank(),
  axis.title.y = element_blank()
) 

plot_share_juris_sup0

ggsave(
  'Output/plot_share_juris_sup0.jpeg',
  plot_share_juris_sup0,
  width = 12, 
  height = 7, 
  dpi = 501
)

# Top and Bottom SEP Firms infringements (Claimants) ----

df_firms <- df %>%
  group_by(Claimants) %>%
  summarise(
    n_by_firms = n_distinct(ID[SEP == 1])
  )

# Top 10 firms in SEP actions count (Claimants) ---- 

top_5_firms <- df_firms %>%
  arrange(desc(n_by_firms)) %>%
  slice_max(n_by_firms, n = 10)

plot_bar_top5_sep_firms <- ggplot(top_5_firms, aes(x = reorder(Claimants, -n_by_firms), y = n_by_firms)) +
  geom_col(
    fill = '#003A70',
    width = 0.4
  ) + 
  theme_minimal() + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

plot_bar_top5_sep_firms

# Top and Bottom SEP Firms infringements (Defendants) ----

df_firms <- df %>%
  group_by(Defendants) %>%
  summarise(
    n_by_firms_def = n_distinct(ID[SEP == 1])
  )

# Top 10 firms in SEP actions count (Defendants) ---- 

top_5_firms <- df_firms %>%
  arrange(desc(n_by_firms_def)) %>%
  slice_max(n_by_firms_def, n = 10)

plot_bar_top5_sep_firms_def <- ggplot(top_5_firms, aes(x = reorder(Defendants, -n_by_firms_def), y = n_by_firms_def)) +
  geom_col(
    fill = '#003A70',
    width = 0.4
  ) + 
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

plot_bar_top5_sep_firms_def

# Final plot : claim + def ---- 

plot_bar_top5_sep_firms <- plot_bar_top5_sep_firms + 
  geom_text(
    aes(
      label = n_by_firms
    ),
    vjust = 2.5,
    size = 2,
    color = 'white',
    fontface = 'bold'
  ) + 
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      fill = 'white'
    )
  ) 

plot_bar_top5_sep_firms_def <- plot_bar_top5_sep_firms_def + 
  theme(
    axis.text.x = element_text(
      angle = 45, 
      hjust = 1
    )
  ) + 
  geom_text(
    aes(
      label = n_by_firms_def
    ),
    vjust = 2.5,
    size = 2,
    color = 'white',
    fontface = 'bold'
  ) 

plot_bar_top5_sep_def_claim <- plot_bar_top5_sep_firms | plot_bar_top5_sep_firms_def 

plot_bar_top5_sep_def_claim

ggsave(
  'Output/top5_sep_claim_def.jpeg',
  plot_bar_top5_sep_def_claim, 
  width = 12,
  height = 7,
  dpi = 500
)

# Geo Distribution of SEP ----

country_plot_df <- df %>%
  group_by(court, Country_Claimants, Country_Defendants) %>%
  filter(any(SEP == 1, na.rm = TRUE)) %>%
  ungroup() %>%
  select(
    ID,
    court,
    Country_Claimants,
    Country_Defendants
  ) %>%
  pivot_longer(
    cols = c(Country_Claimants, Country_Defendants),
    names_to = "party",
    values_to = "country"
  ) %>%
  mutate(
    party = dplyr::recode(
      party,
      Country_Claimants  = "Claimant",
      Country_Defendants = "Defendant"
    )
  ) %>%
  filter(
    !is.na(court),
    !is.na(country),
    country != ""
  ) %>%
  distinct(ID, court, party, country) %>%
  count(court, country, party, name = "n") %>%
  complete(
    court,
    country,
    party = c("Claimant", "Defendant"),
    fill = list(n = 0)
  ) %>%
  mutate(
    value = if_else(party == "Claimant", -n, n)
  )

map_country_plot <- ggplot(
  country_plot_df,
  aes(
    x = value,
    y = fct_reorder(country, abs(value), .fun = sum),
    fill = party
  )
) +
  geom_col(
    width = 0.75,
    color = "black",
    linewidth = 0.2
  ) +
  geom_vline(
    xintercept = 0,
    linewidth = 0.5
  ) +
  facet_wrap(
    ~ court,
    scales = "free_y",
    ncol = 3
  ) +
  scale_x_continuous(
    labels = abs,
    breaks = breaks_pretty(n = 5),
    expand = expansion(mult = c(0.08, 0.08))
  ) +
  geom_text(
  aes(
    x = ifelse(value < 0, value - 1, value + 1),
    label = ifelse(n == 0, "", n)
  ),
  hjust = ifelse(country_plot_df$value < 0, 1, 0),
  size = 3
) +
  scale_fill_manual(
    values = c(
      "Claimant"  = "grey75",
      "Defendant" = "grey35"
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    strip.background = element_rect(fill = "grey92"),
    strip.text = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = grid::unit(1, "lines"),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    plot.subtitle = element_text(hjust = 0.5)
  )

ggsave('Output/geo_dist_countries_sep.jpeg', map_country_plot, width = 12, height = 7, dpi = 500)

# Outcome distribution of SEP actions ----

outcome_plot_df <- df %>%
  filter(
    SEP == 1,
    !is.na(court),
    !is.na(Outcome),
    Outcome != ""
  ) %>%
  distinct(ID, court, Outcome) %>%
  count(court, Outcome, name = "n")

outcome_order <- outcome_plot_df %>%
  group_by(Outcome) %>%
  summarise(total = sum(n), .groups = "drop") %>%
  arrange(total) %>%
  pull(Outcome)

outcome_plot_df <- outcome_plot_df %>%
  mutate(
    outcome = factor(Outcome, levels = outcome_order)
  )

outcome_plot_court <- ggplot(
  outcome_plot_df,
  aes(
    x = n,
    y = outcome
  )
) +
  geom_col(
    width = 0.7,
    fill = "grey55",
    color = "black",
    linewidth = 0.2
  ) +
  geom_text(
    aes(label = n),
    hjust = -0.2,
    size = 3
  ) +
  facet_wrap(
    ~ court,
    ncol = 3,
    scales = "free_y"
  ) +
  scale_x_continuous(
    breaks = scales::breaks_pretty(n = 4),
    expand = expansion(mult = c(0, 0.15))
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 11) +
  theme(
    strip.background = element_rect(fill = "grey92"),
    strip.text = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = grid::unit(1, "lines"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

ggsave('Output/outcome_plot_court.jpeg', outcome_plot_court, width = 12, height = 7, dpi = 500)

# Firm characteristics of SEP claimants and defendants (and N-SEP) by SECTOR ----

sector_vars <- c("CHEMISTRY", "MECHANICAL", "ICT", "Instruments")

plot_df_sector <- df %>%
  group_by(court) %>%
  filter(any(SEP == 1)) %>%
  ungroup() %>%
  pivot_longer(
    cols = all_of(sector_vars),
    names_to = "Sector",
    values_to = "Present"
  ) %>%
  filter(Present == 1) %>%
  mutate(
    SEP_status = ifelse(SEP == 1, "SEP", "Non-SEP")
  ) %>%
  group_by(court, Sector, SEP_status) %>%
  summarise(
    n = n_distinct(ID),
    .groups = "drop"
  )

plot_df_sep_sector <- ggplot(plot_df_sector,
       aes(x = n,
           y = Sector,
           fill = SEP_status)) +
  geom_col(position = position_dodge(width = 0.8),
           width = 0.7) +
  facet_wrap(~court, scales = "free_y") +
  scale_fill_manual(values = c("SEP" = "forestgreen",
                               "Non-SEP" = "red")) +
  labs(
    x = "Number of cases",
    y = NULL,
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    strip.background = element_rect(fill = "grey95"),
    panel.grid.major.y = element_blank()
  )

ggsave('Output/plot_df_sep_sector.jpeg', plot_df_sep_sector, width = 12, height = 7, dpi = 500)

# Firm characteristic of SEP by QUALITY ---- 

heatmap_df <- df %>%
  filter(
    !is.na(court),
    !is.na(Type),
    !is.na(SEP)
  ) %>%
  mutate(
    Firm_type = case_when(
      Type == "UPSTREAM"   ~ "Upstream",
      Type == "DOWNSTREAM" ~ "Downstream",
      Type == "UNKNOWN"    ~ "Unknown",
      Type == "PAE"        ~ "PAE",
      TRUE                 ~ NA_character_
    ),

    SEP_status = factor(
      SEP,
      levels = c(0, 1),
      labels = c("Non-SEP", "SEP")
    )
  ) %>%
  filter(!is.na(Firm_type)) %>%
  count(court, Firm_type, SEP_status, name = "n") %>%
  complete(
    court,
    Firm_type,
    SEP_status,
    fill = list(n = 0)
  ) %>%
  unite(
    "Firm_SEP",
    Firm_type,
    SEP_status,
    sep = " — "
  )

heatmap_quality <- ggplot(
  heatmap_df %>%
    separate(
      Firm_SEP,
      into = c("Firm_type", "SEP_status"),
      sep = " — "
    ),
  aes(
    x = court,
    y = Firm_type,
    fill = n
  )
) +
  geom_tile(
    color = "white",
    linewidth = 0.5
  ) +
  geom_text(
    aes(label = ifelse(n == 0, "", n)),
    size = 3.5
  ) +
  facet_wrap(
    ~ SEP_status,
    ncol = 1
  ) +
  scale_fill_gradient(
    low = "white",
    high = "grey20"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    strip.text = element_text(face = "bold"),
    panel.grid = element_blank(),
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    )
  )

ggsave('Output/heatmap_sector.jpeg', heatmap_quality, width = 12, height = 7, dpi = 500)

df2 <- read_dta('Data/Data_SEP_FSA2.dta')

df_sep <- df2 %>%
  filter(SEP_REVIEW == 1)

df2_sdo <- df2 %>%
  filter(SEP == 1, !is.na(SDO)) %>%
  distinct(ID, SDO) %>%
  count(SDO, sort = TRUE) 

# Bar plot of SDO categories (aggregate) ---- 

plot_sdo <- ggplot(df2_sdo, aes(x = n, y = reorder(SDO, n))) +
            geom_col() +
            theme_minimal()

ggsave('Output/plot_sdo_sep.jpeg', width = 12, height = 7, dpi = 500)

# Bar plot of SDO categories (across jurisdictions)

plot_sdo <- df2 %>%
  filter(SEP == 1, !is.na(SDO)) %>%   
  distinct(ID, court, SDO) %>%     
  count(court, SDO)

sdo_plot <- ggplot(
  plot_sdo,
  aes(
    x = n,
    y = reorder(SDO, n)
  )
) +
  geom_col(fill = "grey35") +
  facet_wrap(~court) +
  labs(
    x = "Number of SEP cases",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "grey95"),
    strip.text = element_text(face = "plain"),
    legend.position = "none"
  )

sdo_plot

ggsave('Output/plot_df_sep_sdo.jpeg', sdo_plot, width = 12, height = 7, dpi = 500)

df %>%
  filter(court %in% c("Munich","Mannheim","Düsseldorf")) %>%
  summarise(
    sep_share = 100 * sum(ID[SEP == 1]) / sum(ID)
  )

df %>%
  group_by(SEP) %>%
  summarise(
    pae_share = 100 * n_distinct(ID[Type == 'PAE']) / n_distinct(ID)
  )
