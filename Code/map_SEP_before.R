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
