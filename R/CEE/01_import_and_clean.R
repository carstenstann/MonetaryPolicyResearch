#library(readxl)
#library(tidyverse)
#library(lubridate)
#library(TTR)
#library(skimr)

# set paths -----------------------------------------------------------------------------

datastream_path <- "./data/CEE/datastream.xlsx"
event_dummy_path <- "./data/CEE/events.xlsx"

# import financial data -----------------------------------------------------------------

financial_import <- map(excel_sheets(datastream_path), 
                        read_xlsx, 
                        path = datastream_path , 
                        skip = 1,
                        col_names = TRUE)

# name list
names(financial_import) <- excel_sheets(datastream_path)

# collapse list to dataframe
financial_data <- financial_import[!excel_sheets(datastream_path) %in% c("ECB")] %>% 
  map(gather, key = "country", value = "value", -Date) %>%
  map_df(bind_rows, .id = "id") %>% 
  filter(!country %in% c("Marginal.Lending.Rate", "MRO.Rate.Marginal", "VIX", "VSTOXX", "Europe")) %>% 
  mutate(date = ymd(Date),
         Date = NULL) %>% 
  spread(id, value) %>% 
  select(date, everything())

# extract ECB rates from import
ECB <- pluck(financial_import, "ECB") %>% 
  mutate(date = ymd(Date),
         Date = NULL) %>% 
  select(date, everything())

# extract Europe and VIX and VSTOXX columns
indexes <-  pluck(financial_import, "Stock") %>% 
  select(date = Date, Europe, VIX, VSTOXX) %>% 
  mutate(date = ymd(date)) %>% 
  select(date, everything())

# import dummy variables ----------------------------------------------------------------

event_dummy_import <- map(excel_sheets(event_dummy_path), 
                          read_xlsx, 
                          path = event_dummy_path, 
                          col_names = TRUE,
                          col_types = c("date", rep("numeric", 19)))

# name list
names(event_dummy_import) <- excel_sheets(event_dummy_path)

# collapse list to dataframe
event_dummy <- flatten_dfr(event_dummy_import) %>% 
  mutate(date = ymd(Date),
         Date = NULL) %>% 
  select(date, everything())

# set NAs to 0
event_dummy[is.na(event_dummy)] <- 0

# join financial_data, indexes, ECB dataframes ------------------------------------------

joined <- financial_data %>% 
  left_join(indexes, by = "date") %>% 
  left_join(ECB, by = "date") %>% 
  filter(date >= ymd("2008-12-31") & date <= ymd("2017-12-31")) %>% 
  select(date, country, Bond_3Y, Bond_10Y, CDS_5Y, CDS_10Y, FX, Interbank, Stock, Europe, VIX, VSTOXX, Marginal.Lending.Rate, MRO.Rate.Marginal)

joined

# scale data ---------------------------------------------------------------------------
 
joined <- joined %>% 
  # convert to from percentage points to basis points
  mutate(
    Bond_3Y = Bond_3Y * 100,
    Bond_10Y = Bond_10Y * 100,
    Interbank = Interbank * 100,
    Marginal.Lending.Rate = Marginal.Lending.Rate * 100, 
    MRO.Rate.Marginal = MRO.Rate.Marginal * 100
  )

# examine missing data ------------------------------------------------------------------
#
#skim(joined)
#
#joined %>% 
#  gg_miss_var(facet = country)
#
#joined %>% 
#  group_by(country) %>%
#  miss_var_summary() %>% 
#  filter(n_miss > 0)
#
# transform data ------------------------------------------------------------------------

diff_vars <- c("Bond_3Y", "Bond_10Y", "Interbank", "Marginal.Lending.Rate", "MRO.Rate.Marginal")
perc_diff_vars <- c("CDS_10Y", "CDS_5Y", "FX", "Stock", "Europe", "VIX", "VSTOXX")

transformed <- joined %>% 
  group_by(country) %>% 
  arrange(country, date) %>% 
  mutate_at(
    vars(one_of(diff_vars)), 
    ~ (. - lag(.))
  ) %>% 
  mutate_at(
    vars(one_of(perc_diff_vars)),
    ~(.x - lag(.x, n = 1)) / lag(.x, n = 1) * 100,
    #~ROC(., type = "discrete") * 100
  ) %>% 
  filter(date != ymd("2008-12-31")) %>% 
  ungroup()

nested <- transformed %>% 
  filter(!country %in% c("Germany", "Italy", "Spain", "France")) %>% 
  pivot_longer(
    cols = c(Bond_3Y, Bond_10Y, CDS_5Y, CDS_10Y, FX, Interbank, Stock), 
    names_to = "variable", 
    values_to = "value"
  )  %>% 
  select(date, country, variable, value, everything()) %>% 
  left_join(event_dummy, by = "date") %>% 
  filter(!(country == "Bulgaria" & variable == "Bond_3Y" & date <= ymd("2009-05-26")),
         !(country == "Croatia" & variable == "Bond_3Y" & date <= ymd("2010-01-18")),
         !(country == "Ukraine" & variable == "Bond_3Y" & date <= ymd("2011-08-26"))
  ) %>%
  group_by(country, variable) %>% 
  mutate(value.lag = lag(value, n = 1)) %>% 
  select(date, country, variable, value, value.lag, everything()) %>% 
  ungroup() %>% 
  group_by(country, variable) %>% 
  nest() %>% 
  filter(
    !((variable == "Bond_10Y") & country == "Ukraine")
  ) %>% 
  arrange(country, variable) %>% 
  ungroup()

rm(ECB, event_dummy_import, financial_import, financial_data, indexes, datastream_path, event_dummy_path,
   diff_vars, perc_diff_vars)
