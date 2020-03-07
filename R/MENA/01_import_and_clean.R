#########################################################################################
## Project: MonetaryPolicySpillovers
## Script purpose: import and clean MENA data
## Date: 03.10.2019
## Author: Carsten Stann
#########################################################################################

library(readxl)
library(tidyverse)
library(janitor)
library(lubridate)
library(TTR)
library(naniar)
library(visdat)
library(cowplot)
library(stargazer)

# set paths -----------------------------------------------------------------------------

mena_path <- "./data/MENA/2019_09_24_eikon_data.xlsx"
symbols_path <- "./data/MENA/2019_09_02_eikon_symbols.xlsx"

datastream_path <- "./data/MENA/datastream.xlsx"
event_dummy_path <- "./data/MENA/EventWindows.xlsx"

# import and clean eikon data ---------------------------------------------------------------------

# MENA data
mena_import <- read_xlsx(path = mena_path, sheet = "Daily", col_type = c("date", rep("numeric", 75)))[-1,]

# table of eikon codes for MENA data
mena_symbols_import <- read_xlsx(path = symbols_path, sheet = "Tabelle1")

# ECB policy rates
ECB_import <- read_xlsx(path = datastream_path, sheet = "ECB", skip = 1) %>% 
   mutate(date = ymd(Date), Date = NULL)

# European stock market index / VSTOXX
indexes_import <- read_xlsx(path = datastream_path, sheet = "Stock", skip = 1) %>% 
   select(Date, Europe, VSTOXX, VIX) %>% 
   mutate(date = ymd(Date),Date = NULL) 

# ECB policy announcement dummy variables
event_dummy_import <- map(excel_sheets(event_dummy_path), 
                          read_xlsx, 
                          path = event_dummy_path, 
                          col_names = TRUE,
                          col_types = c("date", rep("numeric", 19)))
# name list
names(event_dummy_import) <- excel_sheets(event_dummy_path)

# collapse list to dataframe
event_dummy <- flatten_dfr(event_dummy_import) %>% 
   mutate(date = ymd(Date), Date = NULL) 

# set NAs to 0
event_dummy[is.na(event_dummy)] <- 0
   
# clean and join data -------------------------------------------------------------------

# clean table with eikon symbols for each country and indicator
mena_symbols <- mena_symbols_import %>% 
   select(country = "Data availabilty", everything(), -Note) %>% 
   pivot_longer(names_to = "indicator", values_to = "eikon_code", cols = -country)

# clean 
mena_data <- mena_import %>% 
   mutate(Date = ymd(Date)) %>% 
   pivot_longer(names_to = "eikon_code", values_to = "value", cols = -Date) %>% 
   left_join(mena_symbols, by = c("eikon_code" = "eikon_code")) %>%
   filter(!country %in% c("Iraq", "Iran", "Libya")) %>% 
   select(-eikon_code) %>% 
   pivot_wider(names_from = indicator, values_from = value) %>% 
   clean_names() %>% 
   rename(cds_5y = credit_default_swap_5y,
          cds_10y = credit_default_swap_10y,
          fx = exchange_rate_to_usd,
          interbank_3m = interbank_rate_3m,
          sovereign_3y = sovereign_bond_yield_3y,
          sovereign_10y = sovereign_bond_yield_10y,
          stock_index = stock_market_index_benchmark_if_available) %>% 
   mutate(country = str_replace(country, "United Arab Emirates", "UAE")) %>% 
   filter(date >= as.Date("2009-12-29") & date <= as.Date("2017-12-29"))

# join ECB policy rates, Europe stock index, VSTOXX, and event dummy variables
joined <- mena_data %>% 
   left_join(ECB_import, by = "date") %>% 
   left_join(indexes_import, by = "date") %>% 
   left_join(event_dummy, by = "date") %>% 
   select(country, date, everything()) %>% 
   arrange(country, date)

# transform financial variables ---------------------------------------------------------

# transformations
diff_vars <- c("sovereign_3y", "sovereign_10y", "Marginal.Lending.Rate", "MRO.Rate.Marginal")
perc_diff_vars <- c("cds_10y", "cds_5y", "fx", "stock_index","interbank_3m", "Europe", "VSTOXX", "VIX")

transformed <- joined %>% 
   group_by(country) %>% 
   arrange(date) %>% 
   mutate_at(
      vars(one_of(diff_vars)), 
      ~(. - lag(.)) * 100
   ) %>% 
   mutate_at(
      vars(one_of(perc_diff_vars)),
      ~ROC(., type = "discrete")
   ) %>% 
   ungroup()

# filter for balanced panel sets for each variable of interest --------------------------

gather_vars <- c("sovereign_3y", "sovereign_10y", "interbank_3m", "cds_10y", "cds_5y", "fx", "stock_index")

filtered_mena <- transformed %>% 
   pivot_longer(names_to = "indicator", values_to = "value", cols = gather_vars) %>% 
   filter(
      (country == "Bahrain" & (indicator != "sovereign_3y" & indicator != "sovereign_10y" 
                               & indicator != "cds_5y" & indicator != "cds_10y")) |
      (country == "Egypt" & (indicator != "sovereign_3y" & indicator != "sovereign_10y" 
                             & indicator != "cds_5y" & indicator != "cds_10y")) |
      country == "Israel" |
      (country == "Jordan" & (indicator == "interbank_3m" | indicator == "stock_index")) |
      (country == "Kuwait" & indicator == "interbank_3m") |
      (country == "Lebanon" & indicator == "fx") |
      (country == "Morocco" & (indicator == "stock_index" | indicator == "cds_10y" | indicator == "cds_5y")) |
      (country == "Oman" & (indicator == "fx" | indicator == "stock_index")) |
      (country == "Qatar" & (indicator == "fx" | indicator == "cds_10y" | indicator == "cds_5y" | indicator == "stock_index")) |
      (country == "Saudi Arabia" & (indicator == "interbank_3m" | indicator == "fx")) |
      (country == "Syria" & indicator == "fx") |
      (country == "Tunisia" & indicator == "stock_index") |
      (country == "Turkey" & (indicator != "sovereign_3y" & indicator != "sovereign_10y" 
                              & indicator != "cds_10y")) |
      (country == "UAE" & (indicator != "sovereign_3y" & indicator != "sovereign_10y" 
                           & indicator != "cds_5y" & indicator != "cds_10y")) |
      (country == "Yemen" & indicator == "fx")
   ) %>% 
   filter(date > as.Date("2008-01-01")) %>% 
   select(country, date, value, everything()) %>% 
   arrange(country, indicator, date)

# check missingness of filtered data (should contain no missing values)
vis_miss(select(filtered_mena, date, value))

nested_indicator <- filtered_mena %>%
   select(country, date, everything()) %>% 
   arrange(country, indicator, date) %>% 
   filter(indicator != "sovereign_10y" & indicator != "sovereign_3y") %>% 
   group_by(indicator) %>% 
   nest()

nested_country_indicator <- filtered_mena %>%
   select(country, date, indicator, everything()) %>% 
   arrange(country, indicator, date) %>% 
   group_by(country, indicator) %>% 
   nest() %>% 
   ungroup()

# table of eikon codes
symbols <- mena_symbols %>% 
   pivot_wider(names_from =indicator, values_from = eikon_code) %>% 
   clean_names() %>% 
   select(-country_code_for_bond_search) %>% 
   rename(`CDS 5Y` = credit_default_swap_5y,
          `CDS 10Y` = credit_default_swap_10y,
          FX = exchange_rate_to_usd,
          `Interbank 3M` = interbank_rate_3m,
          `Sovereign 3Y` = sovereign_bond_yield_3y,
          `Sovereign 10Y` = sovereign_bond_yield_10y,
          `Stock Index` = stock_market_index_benchmark_if_available) 

# check missingness 
mena_data %>% 
   group_by(country) %>% 
   arrange(country, date) %>% 
   select(-date) %>% 
   gg_miss_var(facet = country) 

ggsave("missingness.png", device = "png", path = "./latex/MENA/plots", width = 7, height = 5, dpi = 300)

