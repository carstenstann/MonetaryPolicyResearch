#########################################################################################
## Project: MonetaryPolicySpillovers
## Script purpose: Estimate country specific models
## Date: 08.10.2019
## Author: Carsten Stann
#########################################################################################

source("./R/MENA/01_import_and_clean.R")

library(dynlm)
library(stargazer)

dynlm_models <- nested_country_indicator %>% 
   mutate(
      Liquidity = map(data, ~dynlm(value ~ Liquidity.1 + VIX + Marginal.Lending.Rate + L(value,1), data = .)),
      Purchases = map(data, ~dynlm(value ~ Purchases.1 + VIX + Marginal.Lending.Rate + L(value,1), data = .)),
      SMP = map(data, ~dynlm(value ~ SMP.1 + VIX + Marginal.Lending.Rate + L(value,1), data = .)),
      OMT = map(data, ~dynlm(value ~ OMT.1 + VIX + Marginal.Lending.Rate + L(value,1), data = .)),
      PSPP = map(data, ~dynlm(value ~ PSPP.1 + VIX + Marginal.Lending.Rate + L(value,1), data = .)),
      CBPP = map(data, ~dynlm(value ~ CBPP.1 + VIX + Marginal.Lending.Rate + L(value,1), data = .)),
      CSPP = map(data, ~dynlm(value ~ CSPP.1 + VIX + Marginal.Lending.Rate + L(value,1), data = .)),
      LTRO = map(data, ~dynlm(value ~ LTRO.1 + VIX + Marginal.Lending.Rate + L(value,1), data = .))
   ) %>% 
   mutate(indicator = recode(indicator, cds_10y = "CDS 10Y", 
                             cds_5y = "CDS 5Y",
                             fx = "FX",
                             interbank_3m = "Interbank Rate",
                             stock_index = "Stock Index"))
dynlm_models

# generate LaTeX Regression Summary Tables ----------------------------------------------
dynlm_models %>% 
   filter(indicator == "FX") %>% 
   pull(SMP) %>% 
stargazer(type = "text",
          title = "SMP Policy Announcements",
          style = "aer",
          column.labels = filter(dynlm_models, indicator == "FX") %>% 
             pull(country),
          dep.var.labels.include = FALSE,
          dep.var.caption = "",
          df = FALSE, 
          align = TRUE,
          float = TRUE,
          font.size = "small",
          header = FALSE)
          #out = "./latex/tables/dynlm_liquidity.tex")


