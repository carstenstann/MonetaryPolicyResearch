library(readxl)
library(tidyverse)
library(lubridate)
library(TTR)
library(skimr)
library(dynlm)
library(broom)
library(lmtest)
library(sandwich)
library(stargazer)

source("./R/CEE/01_import_and_clean.R")

# portfolio rebalancing tests -----------------------------------------------------------
ez_10Y <- transformed %>% 
  select(date, country, Bond_10Y) %>% 
  filter(country %in% c("Germany", "Italy", "France", "Spain")) %>% 
  spread(country, Bond_10Y)

FX_data <- transformed %>% 
  select(date, country, FX) %>% 
  filter(country %in% c("Czech", "Hungary", "Poland", "Romania", "Serbia")) %>% 
  left_join(ez_10Y, by = "date") %>% 
  left_join(event_dummy, by = "date")
  
FX_DE <- lm(FX ~ Purchases.1 * Germany, data = FX_data)
FX_FR <- lm(FX ~ Purchases.1 * France, data = FX_data)
FX_IT <- lm(FX ~ Purchases.1 * Italy, data = FX_data)
FX_ES <- lm(FX ~ Purchases.1 * Spain, data = FX_data)

# produce latex table
stargazer(
  FX_DE, FX_FR, FX_IT, FX_ES, 
  dep.var.labels = "FX",
  dep.var.labels.include = TRUE,
  column.labels = c("Germany", "France", "Italy", "Spain"),
  dep.var.caption = "",
  covariate.labels = c("Constant", 
                       "Asset Purchases", 
                       "Germany", 
                       "Asset Purchases:Germany", 
                       "France",
                       "Asset Purchases:France",
                       "Italy", 
                       "Asset Purchases:Italy",
                       "Spain",
                       "Asset Purchases:Spain"),
  df = FALSE, 
  align = TRUE,
  float = TRUE,
  font.size = "scriptsize",
  column.sep.width = "-10pt",
  header = FALSE,
  model.numbers = FALSE,
  intercept.bottom = FALSE,
  digits = 2,
  #title = "confidence channel test: CEE Stock Market Indices vs. Europes Stock Market Index",
  type = "latex",
  style = "aer",
  notes.align = "r",
  out = paste0(getwd(), "/latex/tables/portfoliorebalancing.tex")
)

# Confidence Channel Testing ------------------------------------------------------------
MSCI <- nested %>% 
  filter(variable == "Stock") %>% 
  mutate(
    MSCI = map(data, ~lm(value ~ Europe * Purchases.1, data = .)
  )) 

# produce latex regression tables
stargazer(
  MSCI$MSCI,
  dep.var.labels = "Stock Market Index",
  dep.var.labels.include = TRUE,
  column.labels = MSCI$country,
  dep.var.caption = "",
  covariate.labels = c("Europe Stock Market Index", "Asset Purchases", "Interaction term", "Constant"),
  min.max = FALSE,
  df = FALSE, 
  align = TRUE,
  float = TRUE,
  font.size = "scriptsize",
  column.sep.width = "-10pt",
  header = FALSE,
  model.numbers = FALSE,
  digits = 2,
  title = "confidence channel test: CEE Stock Market Indices vs. Europes Stock Market Index",
  type = "latex",
  style = "aer",
  notes.align = "r",
  out = paste0(getwd(), "/latex/tables/confidencechannel.tex")
)
 

# correlation plots 
transformed %>%  
  filter(!country %in% c("France", "Germany", "Italy", "Spain") & date > ymd("2008-12-31") ) %>% 
ggplot(aes(x = Europe, y = Stock)) + 
  geom_point(alpha = 0.7, size = 0.5) +
  facet_wrap(~country, scales = "free_y") +
  geom_smooth(method = 'lm', formula= y~x, col = "red", size = .9, se = FALSE) +
  labs(
    y = "Change in country MSCI Index (pp)",
    x = "Change in Europe MSCI Index (pp)"
  ) + 
  theme_linedraw() + 
  theme(strip.background = element_rect(fill = "transparent", colour = "black"),
        strip.text = element_text(colour = "black"))
  

