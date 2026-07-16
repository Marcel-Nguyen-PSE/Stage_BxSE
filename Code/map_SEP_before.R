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
    color = 'green'
  ) + 
    geom_line(
      data = df, 
      aes(y = n_actions_nsep),
      linewidth = 1.2,
      color = 'red'
  ) + 
    scale_x_date(
      date_breaks = '3 months',
      date_labels = '%b\n%Y'
    ) +
    theme_minimal()

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
  theme_minimal()

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
  theme_minimal()

plot_share_juris_sup0

ggsave(
  'Output/plot_share_juris_sup0.jpeg',
  plot_share_juris_sup0,
  width = 12, 
  height = 7, 
  dpi = 500
)



