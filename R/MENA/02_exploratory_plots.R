#########################################################################################
## Project: MonetaryPolicySpillovers
## Script purpose: Visualize data
## Date: 08.10.2019
## Author: Carsten Stann
#########################################################################################

source("./R/MENA/01_import_and_clean.R")

# untransformed raw data ----------------------------------------------------------------

indexed_vars <- mena_data %>% 
   select(date, country, fx, stock_index) %>% 
   filter(date >= ymd("2008-01-01")) %>% 
   group_by(country) %>% 
   arrange(country, date) %>% 
   mutate(
      fx = fx/first(fx) * 100,
      stock_index = stock_index / first(stock_index) * 100
   ) %>% 
   drop_na(fx, stock_index)

fx_indexed <- ggplot(indexed_vars, aes(x = date, y = fx, col = country, group = country)) + 
   geom_line() +
   labs(title = "Spot exchange rate vs. USD",
        subtitle = "(index; 01/01/2008 = 100)",
        y = NULL,
        x = NULL,
        col = "Country") +
   theme_minimal() +
   theme(title = element_text(size = 8),
         axis.text = element_text(size = 6),
         legend.text = element_text(size = 7),
         legend.title = element_text(size = 7),
         legend.key.size = unit(0.75, 'lines'))

stock_market_indexed <- indexed_vars %>% 
   ggplot(aes(x = date, y = stock_index, col = country, group = country)) +
      geom_line() +
      labs(title = "Stock market index",
           subtitle = "(index; 01/01/2008 = 100)",
           y = NULL,
           x = NULL,
           col = "Country") +
      theme_minimal() +
      theme(title = element_text(size = 8),
            axis.text = element_text(size = 6),
            legend.text = element_text(size = 7),
            legend.title = element_text(size = 7),
            legend.key.size = unit(0.75, 'lines'))

cds_10y_bps <- mena_data %>% 
   filter(date >= ymd("2008-01-01"),
          !country %in% c("Algeria", "Tunisia")) %>% 
   drop_na(cds_10y) %>% 
   ggplot(aes(x = date, y = cds_10y, col = country, group = country)) +
      geom_line() +
      labs(title = "10-year CDS spreads",
           subtitle = "(basis points)",
           x = NULL,
           y = NULL,
           col = "Country") +
      theme_minimal() +
      theme(title = element_text(size = 8),
            axis.text = element_text(size = 6),
            legend.text = element_text(size = 7),
            legend.title = element_text(size = 7),
            legend.key.size = unit(0.75, 'lines'))

cds_5y_bps <- mena_data %>% 
   filter(date >= ymd("2008-01-01"),
          !country %in% c("Algeria", "Saudi Arabia", "Tunisia")) %>% 
   drop_na(cds_5y) %>% 
   ggplot(aes(x = date, y = cds_5y, col = country, group = country)) +
      geom_line() +
      labs(title = "5-year CDS spreads",
           subtitle = "(basis points)",
           x = NULL,
           y = NULL,
           col = "Country") +
      theme_minimal() +
      theme(title = element_text(size = 8),
            axis.text = element_text(size = 6),
            legend.text = element_text(size = 7),
            legend.title = element_text(size = 7),
            legend.key.size = unit(0.75, 'lines'))

bond_10y_pct <- mena_data %>% 
   filter(date >= ymd("2008-01-01"),
          country != "Jordan") %>% 
   drop_na(sovereign_10y) %>% 
   ggplot(aes(x = date, y = sovereign_10y, col = country, group = country)) +
      geom_line() +
      labs(title = "10-year sovereign bond yields",
           subtitle = "(in pct)",
           x = NULL,
           y = NULL,
           col = "Country") +
      theme_minimal() +
      theme(title = element_text(size = 8),
            axis.text = element_text(size = 6),
            legend.text = element_text(size = 7),
            legend.title = element_text(size = 7),
            legend.key.size = unit(0.75, 'lines'))

bond_3y_pct <- mena_data %>% 
   filter(date >= ymd("2008-01-01"),
          country != "Jordan") %>% 
   drop_na(sovereign_3y) %>% 
   ggplot(aes(x = date, y = sovereign_3y, col = country, group = country)) +
      geom_line() +
      labs(title = "3-year sovereign bond yields",
           subtitle = "(in pct)",
           x = NULL,
           y = NULL,
           col = "Country") +
      theme_minimal() +
      theme(title = element_text(size = 8),
            axis.text = element_text(size = 6),
            legend.text = element_text(size = 7),
            legend.title = element_text(size = 7),
            legend.key.size = unit(0.75, 'lines'))

plot_grid(fx_indexed, stock_market_indexed, cds_10y_bps,
          cds_5y_bps, bond_10y_pct, bond_3y_pct, ncol = 2)

ggsave("input_data_plots.pdf", device = "pdf", path = "./latex/MENA/plots", 
       width = 6, height = 7.5, dpi = 300)

# transformed data ----------------------------------------------------------------------
# transformations
# diff_vars <- c("sovereign_3y", "sovereign_10y", "Marginal.Lending.Rate", "MRO.Rate.Marginal")
# perc_diff_vars <- c("cds_10y", "cds_5y", "fx", "stock_index","interbank_3m", "Europe", "VSTOXX", "VIX")

filter(filtered_mena, indicator == "fx",
       country != "Syria") %>% 
   ggplot(aes(x = date, y = value)) +
      geom_line() + 
      facet_grid(~country) +
      labs(y = "daily percent change",
           x = NULL) +
      theme_minimal() +
      theme(axis.title = element_text(size = 6),
            axis.text.x = element_text(angle = 90),
            axis.text = element_text(size = 5),
            legend.text = element_text(size = 7),
            legend.title = element_text(size = 8),
            strip.text = element_text(size = 5))

ggsave("fx_transformed.pdf", device = "pdf", path = "./latex/MENA/plots", width = 6, height = 3.5, dpi = 300)

filter(filtered_mena, indicator == "stock_index") %>%  
   ggplot(aes(x = date, y = value)) +
      geom_line() +
      facet_grid(~country) +
      labs(y = "daily percent change",
           x = NULL) +
      theme_minimal() +
      theme(axis.title = element_text(size = 6),
            axis.text.x = element_text(angle = 90),
            axis.text = element_text(size = 5),
            legend.text = element_text(size = 7),
            legend.title = element_text(size = 8),
            strip.text = element_text(size = 5))

ggsave("stock_index_transformed.pdf", device = "pdf", path = "./latex/MENA/plots", width = 6, height = 3.5, dpi = 300)

filter(filtered_mena, indicator == "interbank_3m") %>%  
   ggplot(aes(x = date, y = value)) +
      geom_line() +
      facet_grid(~country) +
      labs(y = "daily percent change",
           x = NULL) +
      theme_minimal() +
      theme(axis.title = element_text(size = 6),
            axis.text.x = element_text(angle = 90),
            axis.text = element_text(size = 5),
            legend.text = element_text(size = 7),
            legend.title = element_text(size = 8),
            strip.text = element_text(size = 5))       

ggsave("interbank_transformed.pdf", device = "pdf", path = "./latex/MENA/plots", width = 6, height = 3.5, dpi = 300)

filter(filtered_mena, indicator %in% c("cds_5y", "cds_10y")) %>%  
   mutate(indicator = str_replace(indicator, "_", " ") %>% toupper()) %>% 
   ggplot(aes(x = date, y = value)) +
      geom_line() +
      facet_grid(indicator~country) +
      labs(y = "daily percent change",
           x = NULL) +
      theme_minimal() +
      theme(axis.title = element_text(size = 6),
            axis.text.x = element_text(angle = 90),
            axis.text = element_text(size = 5),
            legend.text = element_text(size = 7),
            legend.title = element_text(size = 8),
            strip.text = element_text(size = 5)) 

ggsave("cds_transformed.pdf", device = "pdf", path = "./latex/MENA/plots", width = 6, height = 3.5, dpi = 300)

filter(filtered_mena, indicator %in% c("sovereign_10y", "sovereign_3y")) %>% 
   mutate(indicator = str_replace(indicator, "sovereign_10y", "Sovereign Yield 10Y") %>% 
             str_replace("sovereign_3y", "Sovereign Yield 3Y")) %>% 
   ggplot(aes(x = date, y = value)) +
      geom_line() +
      facet_grid(indicator~country) +
      labs(y = "daily percent change",
           x = NULL) +
      theme_minimal() +
      theme(axis.title = element_text(size = 6),
            axis.text.x = element_text(angle = 90),
            axis.text = element_text(size = 5),
            legend.text = element_text(size = 7),
            legend.title = element_text(size = 8),
            strip.text = element_text(size = 5)) 

ggsave("sovereign_yield_transformed.pdf", device = "pdf", path = "./latex/MENA/plots", width = 6, height = 3.5, dpi = 300)
