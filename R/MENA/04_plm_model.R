#########################################################################################
## Project: MonetaryPolicySpillovers
## Script purpose: Estimate panel regressions and produce latex tables
## Date: 05.10.2019
## Author: Carsten Stann
#########################################################################################

source("./R/MENA/01_import_and_clean.R")

library(plm)
library(stargazer)

# estimate Pooled OLS using 1 day esimation windows (sample period: 2008-01-03 - 2017-12-29)
pooled_models <- nested_indicator %>% 
   ungroup() %>% 
   mutate(
      Liquidity = map(data, ~plm(value ~ lag(value, 1) + Liquidity.1 + VIX,
                                 method = "pooling",
                                 data = .)),
      Purchases = map(data, ~plm(value ~ lag(value, 1) + Purchases.1 + VIX,
                                 method = "pooling",
                                 data = .)),
      SMP = map(data, ~plm(value ~ lag(value, 1) + SMP.1 + VIX,
                                 method = "pooling",
                                 data = .)),
      OMT = map(data, ~plm(value ~ lag(value, 1) + OMT.1 + VIX,
                                  method = "pooling",
                                  data = .)),
      PSPP = map(data, ~plm(value ~ lag(value, 1) + PSPP.1 + VIX,
                            method = "pooling",
                            data = .)),
      CBPP = map(data, ~plm(value ~ lag(value, 1) + CBPP.1 + VIX,
                             method = "pooling",
                             data = .)),
      CBPP1 = map(data, ~plm(value ~ lag(value, 1) + CBPP1.1 + VIX,
                             method = "pooling",
                             data = .)),
      CBPP2 = map(data, ~plm(value ~ lag(value, 1) + CBPP2.1 + VIX,
                             method = "pooling",
                             data = .)),
      CBPP3 = map(data, ~plm(value ~ lag(value, 1) + CBPP3.1 + VIX,
                               method = "pooling",
                               data = .)),
      ABSPP = map(data, ~plm(value ~ lag(value, 1) + ABSPP.1 + VIX,
                             method = "pooling",
                             data = .)),
      CSPP = map(data, ~plm(value ~ lag(value, 1) + CSPP.1 + VIX,
                            method = "pooling",
                            data = .)),
      LTRO = map(data, ~plm(value ~ lag(value, 1) + LTRO.1 + VIX,
                              method = "pooling",
                              data = .))
   ) %>% 
   mutate(indicator = recode(indicator, cds_10y = "CDS 10Y", 
                             cds_5y = "CDS 5Y",
                             fx = "FX",
                             interbank_3m = "Interbank Rate",
                             stock_index = "Stock Index"))

# generate LaTeX Regression Summary Tables ----------------------------------------------
stargazer(pooled_models$Liquidity, 
          type = "latex",
          title = "Liquidity-Providing Policy Announcements",
          style = "aer",
          column.labels = pooled_models$indicator,
          dep.var.labels.include = FALSE,
          dep.var.caption = "",
          df = FALSE, 
          align = TRUE,
          order = c(2,3,1),
          float = TRUE,
          font.size = "small",
          header = FALSE,
          out = "./latex/MENA/tables/plm_liquidity.tex")

stargazer(pooled_models$Purchases, 
          type = "latex",
          title = "Asset-Purchasing Policy Announcements",
          style = "aer",
          column.labels = pooled_models$indicator,
          dep.var.labels.include = FALSE,
          dep.var.caption = "",
          df = FALSE, 
          align = TRUE,
          order = c(2,3,1),
          float = TRUE,
          font.size = "small",
          header = FALSE,
          out = "./latex/MENA/tables/plm_purchases.tex")

stargazer(pooled_models$SMP, 
          type = "latex",
          title = "SMP Policy Announcements",
          style = "aer",
          column.labels = pooled_models$indicator,
          dep.var.labels.include = FALSE,
          dep.var.caption = "",
          df = FALSE, 
          align = TRUE,
          order = c(2,3,1),
          float = TRUE,
          font.size = "small",
          header = FALSE,
          out = "./latex/MENA/tables/plm_smp.tex")

stargazer(pooled_models$OMT, 
          type = "latex",
          title = "OMT Policy Announcements",
          style = "aer",
          column.labels = pooled_models$indicator,
          dep.var.labels.include = FALSE,
          dep.var.caption = "",
          df = FALSE, 
          align = TRUE,
          order = c(2,3,1),
          float = TRUE,
          font.size = "small",
          header = FALSE,
          out = "./latex/MENA/tables/plm_omt.tex")

stargazer(pooled_models$PSPP, 
          type = "latex",
          title = "PSPP Policy Announcements",
          style = "aer",
          column.labels = pooled_models$indicator,
          dep.var.labels.include = FALSE,
          dep.var.caption = "",
          df = FALSE, 
          align = TRUE,
          order = c(2,3,1),
          float = TRUE,
          font.size = "small",
          header = FALSE,
          out = "./latex/MENA/tables/plm_pspp.tex")

stargazer(pooled_models$CBPP, 
          type = "latex",
          title = "CBPP Policy Announcements",
          style = "aer",
          column.labels = pooled_models$indicator,
          dep.var.labels.include = FALSE,
          dep.var.caption = "",
          df = FALSE, 
          align = TRUE,
          order = c(2,3,1),
          float = TRUE,
          font.size = "small",
          header = FALSE,
          out = "./latex/MENA/tables/plm_cbpp.tex")

stargazer(pooled_models$ABSPP, 
          type = "latex",
          title = "ABSPP Policy Announcements",
          style = "aer",
          column.labels = pooled_models$indicator,
          dep.var.labels.include = FALSE,
          dep.var.caption = "",
          df = FALSE, 
          align = TRUE,
          order = c(2,3,1),
          float = TRUE,
          font.size = "small",
          header = FALSE,
          out = "./latex/MENA/tables/plm_abspp.tex")

stargazer(pooled_models$CSPP, 
          type = "latex",
          title = "CSPP Policy Announcements",
          style = "aer",
          column.labels = pooled_models$indicator,
          dep.var.labels.include = FALSE,
          dep.var.caption = "",
          df = FALSE, 
          align = TRUE,
          order = c(2,3,1),
          float = TRUE,
          font.size = "small",
          header = FALSE,
          out = "./latex/MENA/tables/plm_cspp.tex")

stargazer(pooled_models$LTRO, 
          type = "latex",
          title = "LTRO Policy Announcements",
          style = "aer",
          column.labels = pooled_models$indicator,
          dep.var.labels.include = FALSE,
          dep.var.caption = "",
          df = FALSE, 
          align = TRUE,
          order = c(2,3,1),
          float = TRUE,
          font.size = "small",
          header = FALSE,
          out = "./latex/MENA/tables/plm_ltro.tex")
