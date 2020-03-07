library(tidyverse)
library(readxl)
library(lubridate)
library(xts)
library(tsbox)

ecb <- read_excel("./data/CEE/datastream.xlsx", sheet = "ECB") %>% 
  mutate(Date = ymd(Date))

ecb_monthly <- ts_data.frame(ecb) %>% 
  ts_frequency(to = "month", aggregate = "mean", na.rm = TRUE) %>% 
  select(Date, everything())

daily <- read_excel("./data/CEE/policy_rates.xlsx", sheet = "daily", col_types = c("date", "numeric", "numeric", "numeric", "numeric")) %>% 
  mutate(Date = ymd(Date))

monthly <- read_excel("./data/CEE/policy_rates.xlsx", sheet = "monthly") %>% 
  mutate(Date = dmy(Date))

daily_rates <- left_join(ecb, daily, by = "Date") 

monthly_rates <- left_join(ecb_monthly, monthly, by = "Date") 

# calculate correlations ----------------------------------------------------------------
mro_cor <- tibble(
  ECB_Rate = "Main Refinancing Operations",
  Bulgaria = cor(drop_na(select(monthly_rates, MRO.Rate.Marginal, Bulgaria)), method = "pearson")[1,2], 
  Croatia = cor(drop_na(select(monthly_rates, MRO.Rate.Marginal, Croatia)), method = "pearson")[1,2], 
  Czechia = cor(drop_na(select(monthly_rates, MRO.Rate.Marginal, Czechia)), method = "pearson")[1,2], 
  Hungary = cor(drop_na(select(daily_rates, MRO.Rate.Marginal, Hungary)), method = "pearson")[1,2],
  Latvia = cor(drop_na(select(daily_rates, MRO.Rate.Marginal, Latvia)), method = "pearson")[1,2],
  Poland = cor(drop_na(select(monthly_rates, MRO.Rate.Marginal, Poland)), method = "pearson")[1,2], 
  Romania = cor(drop_na(select(monthly_rates, MRO.Rate.Marginal, Romania)), method = "pearson")[1,2],
  Russia = cor(drop_na(select(daily_rates, MRO.Rate.Marginal, Russia)), method = "pearson")[1,2],
  Serbia = cor(drop_na(select(daily_rates, MRO.Rate.Marginal, Serbia)), method = "pearson")[1,2]
)

mlr_cor <- tibble(
  ECB_Rate = "Marginal Lending Facility",
  Bulgaria = cor(drop_na(select(monthly_rates, Marginal.Lending.Rate, Bulgaria)), method = "pearson")[1,2], 
  Croatia = cor(drop_na(select(monthly_rates, Marginal.Lending.Rate, Croatia)), method = "pearson")[1,2], 
  Czechia = cor(drop_na(select(monthly_rates, Marginal.Lending.Rate, Czechia)), method = "pearson")[1,2], 
  Hungary = cor(drop_na(select(daily_rates, Marginal.Lending.Rate, Hungary)), method = "pearson")[1,2],
  Latvia = cor(drop_na(select(daily_rates, Marginal.Lending.Rate, Latvia)), method = "pearson")[1,2],
  Poland = cor(drop_na(select(monthly_rates, Marginal.Lending.Rate, Poland)), method = "pearson")[1,2], 
  Romania = cor(drop_na(select(monthly_rates, Marginal.Lending.Rate, Romania)), method = "pearson")[1,2],
  Russia = cor(drop_na(select(daily_rates, Marginal.Lending.Rate, Russia)), method = "pearson")[1,2],
  Serbia = cor(drop_na(select(daily_rates, Marginal.Lending.Rate, Serbia)), method = "pearson")[1,2]
)
  

correlations <- bind_rows(mro_cor, mlr_cor) 

correlations
