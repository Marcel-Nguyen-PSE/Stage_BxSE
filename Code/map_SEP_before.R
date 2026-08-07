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
library(treemapify)
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

df_sep_unique <- df_sep %>%
  distinct(Patentnumber)

# Monthly evolution of SEP AND non-SEP actions ---- 

df <- df %>%
  group_by(year, month) %>%
  mutate(
    n_actions_sep = n_distinct(ID[SEP == 1]),
    n_actions_nsep = n_distinct(ID[SEP == 0])
  ) %>%
  ungroup()

df_quarter <- df %>%
  mutate(
    Quarter = floor_date(Date, unit = "quarter"),
    Quarter_label = paste0("Q", quarter(Date), " ", year(Date))
  ) %>%
  group_by(Quarter, Quarter_label) %>%
  summarise(
    SEP = sum(n_actions_sep, na.rm = TRUE),
    `N-SEP` = sum(n_actions_nsep, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(SEP, `N-SEP`),
    names_to = "Type",
    values_to = "n"
  )

plot_sep_nsep_quarter <- ggplot(
  df_quarter,
  aes(x = Quarter, y = n, fill = Type)
) +
  geom_col(
    position = position_dodge(width = 70),
    width = 60
  ) +
  geom_text(
    aes(label = n),
    position = position_dodge(width = 70),
    vjust = -0.4,
    size = 3
  ) +
  scale_fill_manual(
    values = c(
      "SEP" = "#003A70",
      "N-SEP" = "#6BAED6"
    )
  ) +
  scale_x_date(
    breaks = unique(df_quarter$Quarter),
    labels = unique(df_quarter$Quarter_label)
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1))
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey85"),
    panel.grid.minor.y = element_blank(),
    legend.title = element_blank()
  ) +
labs(
  caption = "Note: N = 911 patent cases before the United Patent Court (UPC) from Jun 2023 to May 2026 aggregated by quarter. \n Low levels in Q2 2023 and Q2 2026 are due to limited data availability."
) + 
  theme(
  plot.caption = element_text(
    hjust = 0,
    color = "grey50",
    size = 9
  ),
  legend.position = 'bottom'
)

plot_sep_nsep_quarter

ggsave(
  "Output/plot_sep_nsep_quarter.jpeg",
  plot_sep_nsep_quarter,
  width = 12,
  height = 7,
  dpi = 500
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

npe_firms <- df %>%
  filter(NPE == 1) %>%
  distinct(Claimants) %>%
  pull(Claimants)

df_firms_claim <- df %>%
  filter(SEP == 1) %>%
  group_by(Claimants) %>%
  summarise(
    n_by_firms = n_distinct(ID),
    .groups = "drop"
  ) %>%
  mutate(
    NPE_firm = Claimants %in% npe_firms
  )

top_10_claim <- df_firms_claim %>%
  slice_max(n_by_firms, n = 10, with_ties = FALSE)

plot_bar_top10_sep_firms <- ggplot(
  top_10_claim,
  aes(
    x = reorder(Claimants, -n_by_firms),
    y = n_by_firms,
    fill = NPE_firm
  )
) +
  geom_col(width = 0.4) +
  geom_text(
    aes(label = n_by_firms),
    vjust = 2.5,
    size = 2,
    color = "white",
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      `FALSE` = "#003A70",
      `TRUE`  = "#0B5CAB"
    ),
    breaks = c(FALSE, TRUE),
    labels = c(
      `FALSE` = "Operating company",
      `TRUE`  = "NPE"
    ),
    drop = FALSE,
    name = NULL
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

df_firms_def <- df %>%
  filter(SEP == 1) %>%
  group_by(Defendants) %>%
  summarise(
    n_by_firms_def = n_distinct(ID),
    .groups = "drop"
  ) %>%
  mutate(
    NPE_firm = Defendants %in% npe_firms
  )

top_10_def <- df_firms_def %>%
  slice_max(n_by_firms_def, n = 10, with_ties = FALSE)

plot_bar_top10_sep_firms_def <- ggplot(
  top_10_def,
  aes(
    x = reorder(Defendants, -n_by_firms_def),
    y = n_by_firms_def,
    fill = NPE_firm
  )
) +
  geom_col(width = 0.4) +
  geom_text(
    aes(label = n_by_firms_def),
    vjust = 2.5,
    size = 2,
    color = "white",
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      `FALSE` = "#003A70",
      `TRUE`  = "#0B5CAB"
    ),
    breaks = c(FALSE, TRUE),
    labels = c(
      `FALSE` = "Operating company",
      `TRUE`  = "NPE"
    ),
    drop = FALSE,
    name = NULL
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

plot_bar_top10_sep_def_claim <-
  (plot_bar_top10_sep_firms | plot_bar_top10_sep_firms_def) +
  plot_layout(guides = "collect") +
  plot_annotation(
    theme = theme(
      legend.position = "bottom",
      legend.justification = "center"
    )
  )

ggsave(
  "Output/top10_sep_claim_def.jpeg",
  plot_bar_top10_sep_def_claim,
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
      "Claimant"  = "#003A70",
      "Defendant" = "#0B5CAB"
    ),
    name = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
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
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y.left = element_blank()
  ) +
labs(
  caption = "Note: Jurisdictions with none SEP indexed cases or without court specified have been omitted from the plot. This includes Milan, Nordic-Baltic, Brussels and Vienna, and 6 empty cases."
) + 
  theme(
  plot.caption = element_text(
    hjust = 0,
    color = "grey50",
    size = 9
  ),
  legend.position = 'bottom'
)

ggsave('Output/geo_dist_countries_sep.jpeg', map_country_plot, width = 12, height = 7, dpi = 500)

# Firm characteristics of SEP claimants and defendants (and N-SEP) by SECTOR ----

sector_vars <- c("CHEMISTRY", "MECHANICAL", "ICT", "Instruments")

sector_share <- df %>%
  pivot_longer(
    cols = all_of(sector_vars),
    names_to = "Sector",
    values_to = "Present"
  ) %>%
  filter(Present == 1) %>%
  group_by(Sector) %>%
  summarise(n = n_distinct(ID), .groups = "drop") %>%
  mutate(
    share = n / sum(n),
    label = paste0(Sector, "\n", round(100 * share, 1), "%")
  )

ict_share <- df %>%
  filter(ICT == 1) %>%
  mutate(Type = ifelse(SEP == 1, "SEP", "Non-SEP")) %>%
  group_by(Type) %>%
  summarise(n = n_distinct(ID), .groups = "drop") %>%
  mutate(
    share = n / sum(n),
    label = paste0(Type, "\n", round(100 * share, 1), "%")
  )

plot_ict_sep_share_small <-
  plot_spacer() /
  plot_ict_sep_share /
  plot_spacer() +
  plot_layout(heights = c(0.2, 0.6, 0.2))

plot_sector_treemap <-
  plot_sector_share +
  plot_spacer() +
  plot_ict_sep_share_small +
  plot_layout(widths = c(1, 0.08, 0.65)) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = "white", color = NA)
    )
  )

plot_sector_treemap <- plot_sector_treemap +
  inset_element(
    ggplot() +
      geom_segment(
        aes(x = 0, y = 1, xend = 1, yend = 0.8),
        colour = "grey50",
        linewidth = 0.6
      ) +
      geom_segment(
        aes(x = 0, y = 0, xend = 1, yend = 0.2),
        colour = "grey50",
        linewidth = 0.6
      ) +
      xlim(0, 1) +
      ylim(0, 1) +
      theme_void(),
    left = 0.55,
    bottom = 0,
    right = 0.72,
    top = 1,
    align_to = "full"
  )

plot_sector_treemap

base_plot <-
  plot_sector_share +
  plot_spacer() +
  plot_ict_sep_share_small +
  plot_layout(widths = c(1, 0.08, 0.65))

plot_sector_treemap <- ggdraw(base_plot) +
  draw_line(
    x = c(0.561, 0.627),
    y = c(0.93, 0.71),
    color = "grey50",
    linewidth = 0.7
  ) +
  draw_line(
    x = c(0.561, 0.627),
    y = c(0.03, 0.21),
    color = "grey50",
    linewidth = 0.7
  )

ggsave(
  "Output/plot_sector_treemap.jpeg",
  plot_sector_treemap,
  width = 12,
  height = 6,
  dpi = 500
)

# SDO Categories ----

df2 <- read_dta('Data/Data_SEP_FSA2.dta')

df_sep <- df2 %>%
  filter(SEP_REVIEW == 1)

df2_sdo <- df2 %>%
  filter(SEP == 1, !is.na(SDO)) %>%
  distinct(ID, SDO) %>%
  count(SDO, sort = TRUE) 

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

# Summary Statistics ---- 

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

df %>%
  filter(SEP == 1) %>%
  summarise(
    total_cases = n(),
    judgments = sum(Outcome %in% c("Claimant", "Defendant")),
    judgment_share = 100 * judgments / total_cases
  )

df2 %>%
  filter(SEP == 1, !is.na(SDO)) %>%
  distinct(ID, SDO) %>%
  summarise(
    etsi_share = 100 * mean(SDO == "ETSI")
  )

# SDO / Firm Categories ----

df_cat <- read_dta('Data/Data_SEP_FSA2_V2.dta')

df_share <- df_cat %>%
  filter(
    !is.na(Type),
    !is.na(Category),
    !is.na(SEP),
    Type != 'UNKNOWN',
    Category != ''
  ) %>%
  count(SEP, Type, Category, name = "n") %>%
  group_by(SEP, Type) %>%
  mutate(
    share = n / sum(n),
    SEP_status = if_else(SEP == 1, "SEP", "Non-SEP")
  ) %>%
  ungroup()

df_share <- df_share %>%
  mutate(
    Type = factor(
      Type,
      levels = c("DOWNSTREAM", "HYBRID", "UPSTREAM", "PAE")
    )
  )

plot_category_claim_def <- ggplot(
  df_share,
  aes(
    x = Type,
    y = share,
    fill = Category
  )
) +
  geom_col(width = 0.7) +
  facet_wrap(~ SEP_status) +
  scale_fill_manual(
  values = c(
    "Component supplier" = "#003A70",
    "Distributor / retailer" = "#0B5CAB",
    "Product manufacturer (OEM)" = "#6BAED6",
    "Service provider / end user" = "#D9EAF7"
  )
) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    x = "Claimant category",
    y = "Share of defendant categories",
    fill = "Defendant category"
  ) +
  theme_minimal() + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y.left = element_blank(),
    legend.position = 'bottom'
  ) +
labs(
  caption = "Note: 22 cases with empty defendant categories and 15 with empty claimants categories are omitted in this plot. Numeric values are not shown for shares inferior to 5%."
) + 
  theme(
  plot.caption = element_text(
    hjust = 0,
    color = "grey50",
    size = 9
  ),
  legend.position = 'bottom'
) +
  geom_text(
  aes(
    label = ifelse(share > 0.06, round(share * 100, 0), "")
  ),
  position = position_stack(vjust = 0.5),
  color = "white",
  size = 3
) 

plot_category_claim_def 

ggsave('Output/plot_category_claim_def.jpeg', plot_category_claim_def, width = 12, height = 7, dpi = 500)

# SEP SDO Patent Categories ---- 

df_sdo <- read_xlsx('Data/SEP_complete_with_standard_category - 25072026.xlsx')

sdo_share <- df_sdo %>%
  filter(!is.na(SDO)) %>%
  group_by(SDO) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    share = n / sum(n),
    label = paste0(SDO, "\n", round(100 * share, 1), "%"),
    color_rank = rank(-n, ties.method = "first")
  )

etsi_technology_share <- df_sdo %>%
  filter(
    SDO == "ETSI",
    !is.na(`Technology Category`)
  ) %>%
  group_by(`Technology Category`) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    share = n / sum(n),
    label = paste0(`Technology Category`, "\n", round(100 * share, 1), "%"),
    color_rank = rank(-n, ties.method = "first")
  )

cellular_standard_share <- df_sdo %>%
  filter(
    SDO == "ETSI",
    `Technology Category` == "Cellular",
    !is.na(Standard)
  ) %>%
  group_by(Standard) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    share = n / sum(n),
    label = paste0(Standard, "\n", round(100 * share, 1), "%"),
    color_rank = rank(-n, ties.method = "first")
  )

plot_sdo <- ggplot(
  sdo_share,
  aes(
    area = n,
    fill = factor(color_rank),
    label = label
  )
) +
  geom_treemap(color = "white", linewidth = 1) +
  geom_treemap_text(
    colour = "white",
    place = "topleft",
    grow = FALSE,
    reflow = TRUE,
    min.size = 4,
    fontsize = 6
  ) +
  labs(title = "SDO distribution") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) + 
  scale_fill_manual(
    values = c(
      "1" = "#003A70",
      "2" = "#0B5CAB",
      "3" = "#6BAED6",
      "4" = "#D9EAF7",
      "5" = "#4A4A4A",
      "6" = "#8C8C8C"
    )
  ) 

plot_technology <- ggplot(
  etsi_technology_share,
  aes(
    area = n,
    fill = factor(color_rank),
    label = label
  )
) +
  geom_treemap(color = "white", linewidth = 1) +
  geom_treemap_text(
    colour = "white",
    place = "topleft",
    grow = FALSE,
    reflow = TRUE,
    min.size = 4,
    fontsize = 6
  ) +
  labs(title = "Technology within ETSI") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) + 
  scale_fill_manual(
    values = c(
      "1" = "#003A70",
      "2" = "#0B5CAB",
      "3" = "#6BAED6",
      "4" = "#D9EAF7",
      "5" = "#4A4A4A",
      "6" = "#8C8C8C"
    )
  ) 

plot_standard <- ggplot(
  cellular_standard_share,
  aes(
    area = n,
    fill = factor(color_rank),
    label = label
  )
) +
  geom_treemap(color = "white", linewidth = 1) +
  geom_treemap_text(
    colour = "white",
    place = "topleft",
    grow = FALSE,
    reflow = TRUE,
    min.size = 4,
    fontsize = 6
  ) +
  labs(title = "Standards within cellular") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) + 
  scale_fill_manual(
    values = c(
      "1" = "#003A70",
      "2" = "#0B5CAB",
      "3" = "#6BAED6",
      "4" = "#D9EAF7",
      "5" = "#4A4A4A",
      "6" = "#8C8C8C"
    )
  ) 

plot_sdo_treemap <-
  plot_sdo +
  plot_spacer() +
  plot_technology +
  plot_spacer() +
  plot_standard +
  plot_layout(widths = c(1, 0.05, 0.8, 0.05, 0.6))

plot_sdo_treemap

ggsave('Output/plot_sdo_treemap.jpeg', plot_sdo_treemap, height = 7, width = 12, dpi = 500)


sdo_share <- sdo_share %>%
  mutate(color_rank = if_else(SDO == "ETSI", 1, color_rank))

etsi_technology_share <- etsi_technology_share %>%
  mutate(color_rank = if_else(`Technology Category` == "Cellular", 1, color_rank))

cellular_standard_share <- cellular_standard_share %>%
  mutate(color_rank = if_else(Standard == "5G NR", 1, color_rank))


plot_sdo <- ggplot(
  sdo_share,
  aes(
    area = n,
    fill = factor(color_rank),
    label = label
  )
) +
  geom_treemap(color = "white", linewidth = 1, start = "bottomright") +
  geom_treemap_text(
    colour = "white",
    place = "topleft",
    grow = FALSE,
    reflow = TRUE,
    min.size = 4,
    fontsize = 6,
    start = "bottomright"
  ) +
  labs(title = "SDO distribution") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  scale_fill_manual(
    values = c(
      "1" = "#003A70",
      "2" = "#0B5CAB",
      "3" = "#6BAED6",
      "4" = "#D9EAF7",
      "5" = "#4A4A4A",
      "6" = "#8C8C8C"
    )
  )

plot_technology <- ggplot(
  etsi_technology_share,
  aes(
    area = n,
    fill = factor(color_rank),
    label = label
  )
) +
  geom_treemap(color = "white", linewidth = 1, start = "bottomright") +
  geom_treemap_text(
    colour = "white",
    place = "topleft",
    grow = FALSE,
    reflow = TRUE,
    min.size = 4,
    fontsize = 6,
    start = "bottomright"
  ) +
  labs(title = "Technology within ETSI") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  scale_fill_manual(
    values = c(
      "1" = "#003A70",
      "2" = "#0B5CAB",
      "3" = "#6BAED6",
      "4" = "#D9EAF7",
      "5" = "#4A4A4A",
      "6" = "#8C8C8C"
    )
  )

plot_standard <- ggplot(
  cellular_standard_share,
  aes(
    area = n,
    fill = factor(color_rank),
    label = label
  )
) +
  geom_treemap(color = "white", linewidth = 1, start = "bottomright") +
  geom_treemap_text(
    colour = "white",
    place = "topleft",
    grow = FALSE,
    reflow = TRUE,
    min.size = 4,
    fontsize = 6,
    start = "bottomright"
  ) +
  labs(title = "Standards within cellular") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  scale_fill_manual(
    values = c(
      "1" = "#003A70",
      "2" = "#0B5CAB",
      "3" = "#6BAED6",
      "4" = "#D9EAF7",
      "5" = "#4A4A4A",
      "6" = "#8C8C8C"
    )
  )

plot_technology_small <-
  plot_spacer() /
  plot_technology /
  plot_spacer() +
  plot_layout(heights = c(0.2, 0.6, 0.2))

plot_standard_small <-
  plot_spacer() /
  plot_standard /
  plot_spacer() +
  plot_layout(heights = c(0.3, 0.4, 0.3))

plot_sdo_treemap <-
  plot_sdo +
  plot_spacer() +
  plot_technology_small +
  plot_spacer() +
  plot_standard_small +
  plot_layout(widths = c(1, 0.03, 0.9, 0.03, 0.8))

plot_sdo_treemap

ggsave(
  'Output/plot_sdo_treemap.jpeg',
  plot_sdo_treemap,
  height = 7,
  width = 12,
  dpi = 500
)

