library(progressr)
library(tidyverse)
library(readxl)
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

df <- read_dta('Data/Data_SEP_FSA.dta')

df_sep <- df %>%
  filter(SEP == 1) %>%
  arrange(ID)

# Monthly evolution of SEP actions ---- 

df_sep <- df_sep %>%
  group_by(year, month) %>%
  mutate(n_actions = n_distinct(ID)) %>%
  ungroup()

plot_sep_month <- ggplot(
  data = df_sep, mapping = aes(x = Date, y = n_actions)
) + 
  geom_line(
    linewidth = 1.2,
    color = 'red'
  ) + 
  scale_x_date(
    date_breaks = '3 months',
    date_labels = '%b\n%Y'
  ) + 
  theme_minimal() 

plot_sep_month

ggsave(
  'Output/plot_sep_month.jpeg',
  plot_sep_month, 
  width = 12,
  height = 7,
  dpi = 300
)

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
      color = '#0B5CAB'
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

# Across jurisdictions ---- 

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

plot_share_juris <- ggplot(
  quarterly_juris,
  aes(x = quarter, y = share_sep)
) +
  geom_col() +
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
  facet_wrap(~ jurisdiction,
            axes = 'all') +
  theme_minimal() + 
  theme(
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.grid.major.y = element_line(colour = "grey85"),
  panel.grid.minor.y = element_blank()
)


plot_share_juris

ggsave(
  'Output/plot_share_juris.jpeg',
  plot_share_juris, 
  width = 12, 
  height = 7,
  dpi = 500
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
      'TRUE' = '#0B5CAB',
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
  dpi = 500
)

# Top and Bottom SEP Firms infringements (Claimants) ----

df_firms <- df %>%
  group_by(Claimants) %>%
  summarise(
    n_by_firms = n_distinct(ID[SEP == 1])
  )

# Histogram of frequencies SEP actions ----

histogram_sep_firms <- ggplot(df_firms, aes(x = n_by_firms)) + 
  geom_histogram(
    binwidth = 1, 
    boundary = 0.5, 
    fill = 'red',
    color = 'white'
  ) + 
  scale_x_continuous(
    breaks = seq(min(df_firms$n_by_firms), max(df_firms$n_by_firms), by = 1)
  ) +
  theme_minimal()

histogram_sep_firms

ggsave(
  'Output/plot_density_sep_firms_claimants.jpeg',
  histogram_sep_firms,
  width = 12,
  height = 7,
  dpi = 500
)

# Top 5 firms in SEP actions count (Claimants) ---- 

top_5_firms <- df_firms %>%
  arrange(desc(n_by_firms)) %>%
  slice_max(n_by_firms, n = 5)

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

ggsave(
  'Output/plot_bar_top5_sep_firms_claimants.jpeg',
  plot_bar_top5_sep_firms,
  width = 12,
  height = 7,
  dpi = 500
)

# Top 5 SEP actions count by sector (Claimants)

top_5_sectors <- df %>%
  group_by(Technology_35_classes) %>%
  summarise(
    n_by_sector = n_distinct(ID[SEP == 1])
  ) %>%
  slice_max(n_by_sector, n = 5)

plot_bar_top5_sep_sector <- ggplot(top_5_sectors, aes(x = reorder(Technology_35_classes, -n_by_sector), y = n_by_sector)) + 
  geom_col(
    fill = '#003A70',
    width = 0.4
  ) + 
  theme_minimal()

plot_bar_top5_sep_sector

ggsave(
  'Output/plot_bar_top5_sep_sector.jpeg',
  plot_bar_top5_sep_sector,
  width = 12,
  height = 7,
  dpi = 500 
)

# Top and Bottom SEP Firms infringements (Defendants) ----

df_firms <- df %>%
  group_by(Defendants) %>%
  summarise(
    n_by_firms_def = n_distinct(ID[SEP == 1])
  )

# Histogram of frequencies SEP actions (Defendants) ----

histogram_sep_firms_def <- ggplot(df_firms, aes(x = n_by_firms_def)) + 
  geom_histogram(
    binwidth = 1, 
    boundary = 0.5, 
    fill = 'red',
    color = 'white'
  ) + 
  scale_x_continuous(
    breaks = seq(min(df_firms$n_by_firms_def), max(df_firms$n_by_firms_def), by = 1)
  ) +
  theme_minimal()

histogram_sep_firms_def

ggsave(
  'Output/plot_density_sep_firms_defendants.jpeg',
  histogram_sep_firms_def,
  width = 12,
  height = 7,
  dpi = 500
)

# Top 5 firms in SEP actions count (Defendants) ---- 

top_5_firms <- df_firms %>%
  arrange(desc(n_by_firms_def)) %>%
  slice_max(n_by_firms_def, n = 5)

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

ggsave(
  'Output/plot_bar_top5_sep_firms_defendants.jpeg',
  plot_bar_top5_sep_firms_def,
  width = 12,
  height = 7,
  dpi = 500
)

# Final plot : claim + def ---- 

plot_bar_top5_sep_firms <- plot_bar_top5_sep_firms + 
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  ) 

plot_bar_top5_sep_firms_def <- plot_bar_top5_sep_firms_def + 
  theme(
    axis.text.x = element_text(
      angle = 45, 
      hjust = 1
    )
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

# Geographic distribution of SEP

sep_cases <- df %>%
  filter(SEP == 1) %>%
  mutate(
    Date = as.Date(Date),
    year = factor(
      format(Date, "%Y"),
      levels = c("2023", "2024", "2025", "2026")
    )
  ) %>%
  distinct(
    ID,
    year,
    Country_Claimants,
    Country_Defendants,
    court
  ) %>%
  filter(
    !is.na(year),
    !is.na(Country_Claimants),
    !is.na(Country_Defendants),
    !is.na(court)
  )

claimant_edges <- sep_cases %>%
  group_by(
    year,
    Country_Claimants,
    court
  ) %>%
  summarise(
    n_cases = n_distinct(ID),
    .groups = "drop"
  ) %>%
  transmute(
    year,
    origin = Country_Claimants,
    court,
    party_type = "Claimant",
    n_cases
  )

defendant_edges <- sep_cases %>%
  group_by(
    year,
    Country_Defendants,
    court
  ) %>%
  summarise(
    n_cases = n_distinct(ID),
    .groups = "drop"
  ) %>%
  transmute(
    year,
    origin = Country_Defendants,
    court,
    party_type = "Defendant",
    n_cases
  )

edges <- bind_rows(
  claimant_edges,
  defendant_edges
)

claimant_nodes <- edges %>%
  filter(party_type == "Claimant") %>%
  distinct(origin) %>%
  arrange(origin) %>%
  mutate(
    node_id = paste0("claimant_", origin),
    label = origin,
    node_type = "Claimant country",
    x = 0,
    y = seq_len(n()) * 2
  )

court_nodes <- edges %>%
  distinct(court) %>%
  arrange(court) %>%
  mutate(
    node_id = paste0("court_", court),
    label = court,
    node_type = "UPC jurisdiction",
    x = 1,
    y = seq(
      from = 2,
      to = max(c(
        claimant_nodes$y,
        seq_len(
          edges %>%
            filter(party_type == "Defendant") %>%
            distinct(origin) %>%
            nrow()
        ) * 2
      )),
      length.out = n()
    )
  )

defendant_nodes <- edges %>%
  filter(party_type == "Defendant") %>%
  distinct(origin) %>%
  arrange(origin) %>%
  mutate(
    node_id = paste0("defendant_", origin),
    label = origin,
    node_type = "Defendant country",
    x = 2,
    y = seq_len(n()) * 2
  )

nodes <- bind_rows(
  claimant_nodes,
  court_nodes,
  defendant_nodes
)

edges_plot <- edges %>%
  mutate(
    origin_id = if_else(
      party_type == "Claimant",
      paste0("claimant_", origin),
      paste0("defendant_", origin)
    ),
    court_id = paste0("court_", court)
  ) %>%
  left_join(
    nodes %>%
      select(
        origin_id = node_id,
        x_origin = x,
        y_origin = y
      ),
    by = "origin_id"
  ) %>%
  left_join(
    nodes %>%
      select(
        court_id = node_id,
        x_court = x,
        y_court = y
      ),
    by = "court_id"
  ) %>%
  mutate(
    edge_colour = ifelse(
      court %in% top3_jurisdictions,
      party_type,
      'Other'
    )
  )

years <- tibble(
  year = factor(
    c("2023", "2024", "2025", "2026"),
    levels = c("2023", "2024", "2025", "2026")
  )
)

top3_jurisdictions <- edges_plot %>%
  group_by(court) %>%
  summarise(
    n_cases = sum(n_cases),
    .groups = "drop"
  ) %>%
  slice_max(
    order_by = n_cases,
    n = 3,
    with_ties = FALSE
  ) %>%
  pull(court)

nodes_by_year <- nodes_by_year %>%
  mutate(
    node_colour = case_when(
      node_type != "UPC jurisdiction" ~ node_type,
      label %in% top3_jurisdictions   ~ "Top 3 jurisdiction",
      TRUE                            ~ "Other jurisdiction"
    )
  )

flow_network <- ggplot() +
  geom_curve(
    data = edges_plot,
    aes(
      x = x_origin,
      y = y_origin,
      xend = x_court,
      yend = y_court,
      colour = edge_colour,
      linewidth = n_cases
    ),
    curvature = 0.10,
    alpha = 0.65,
    lineend = "round",
    arrow = grid::arrow(
      type = "closed",
      length = grid::unit(0.055, "inches")
    )
  ) +
  geom_point(
    data = nodes_by_year,
    aes(
      x = x,
      y = y,
      fill = node_colour
    ),
    shape = 21,
    size = 4,
    stroke = 0.8,
    colour = "grey20"
  ) +
  geom_text(
    data = nodes_by_year %>%
      filter(node_type == "Claimant country"),
    aes(
      x = x,
      y = y,
      label = label
    ),
    hjust = 1,
    nudge_x = -0.12,
    family = "sans",
    size = 2.8
  ) +
  geom_text(
    data = nodes_by_year %>%
      filter(node_type == "UPC jurisdiction"),
    aes(
      x = x,
      y = y,
      label = label
    ),
    hjust = 0,
    nudge_x = 0.1,
    family = "sans",
    size = 2,
    fontface = "bold"
  ) +
  geom_text(
    data = nodes_by_year %>%
      filter(node_type == "Defendant country"),
    aes(
      x = x,
      y = y,
      label = label
    ),
    hjust = 0,
    nudge_x = 0.12,    # increased distance
    family = "sans",
    size = 2.8
  ) +
  facet_wrap(
    ~year,
    ncol = 2,
    axes = "all",
    axis.labels = "all"
  ) +
  scale_x_continuous(
    breaks = c(0, 1, 2),
    labels = c(
      "Claimant countries",
      "UPC jurisdictions",
      "Defendant countries"
    ),
    limits = c(-0.45, 2.45)
  ) +
  scale_y_continuous(
    limits = c(0, max(nodes$y) + 2),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_colour_manual(
    values = c(
      "Claimant" = "#003A70",
      "Defendant" = "#E69F00"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Claimant country"   = "white",
      "Defendant country"  = "white",
      "Top 3 jurisdiction" = "#0B5CAB",
      "Other jurisdiction" = "grey70"
    )
  ) +
  scale_linewidth_continuous(
    range = c(0.4, 2.5),
    breaks = scales::pretty_breaks(n = 4)
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(
      face = "bold",
      size = 10,
      margin = margin(t = 8)
    ),
    strip.text = element_text(
      face = "bold",
      size = 12
    ),
    panel.spacing = grid::unit(1.5, "lines"),
    legend.position = "none"
  )

flow_network

ggsave('Output/map_geo_sep.jpeg',
        flow_network,
        width = 12,
        height = 7,
        dpi = 500
)

# Outcome distribution of SEP actions ----

df_outcome <- df %>%
  filter(SEP == 1) %>%
  group_by(Outcome) %>%
  summarise(
    n_outcome = n_distinct(ID)
  )

plot_bar_outcome_sep <- ggplot(df_outcome, aes(x = reorder(Outcome, -n_outcome), y = n_outcome)) + 
  geom_col(
    fill = 'blue',
    width = 0.7
  ) + 
  theme_minimal()

plot_bar_outcome_sep

ggsave('Output/plot_bar_outcome_sep.jpeg', plot_bar_outcome_sep, width = 12, height = 7, dpi = 500)

