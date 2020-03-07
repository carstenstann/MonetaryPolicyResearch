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

# prepare samples for modeling: 2009-01-01 to 2017-12-31, 2009-01-01 to 2012-07-25, 2012-07-26 to 2017-12-31

model_data <- nested %>% 
  mutate(
    full_sample = map(data, ~as_tibble(.)),
    pre_omt = map(data, ~filter(., date < ymd("2012-07-26"))),
    post_omt = map(data, ~filter(., date >= ymd("2012-07-26"))),
    data = NULL
  ) %>% 
  gather(key = sample, value = data, -c(country, variable))

# model data ----------------------------------------------------------------------------          

names(event_dummy)

models <- model_data %>%
  mutate(
    pooled = map(data, ~dynlm(value ~ Cumulative.1 + Fed.1 + EFSF_ESM.1 + VSTOXX + Marginal.Lending.Rate + 
                              value.lag, data = .)
    ),
    liquidity = map(data, ~dynlm(value ~ Liquidity.1 + Fed.1 + EFSF_ESM.1 + VSTOXX + Marginal.Lending.Rate + 
                                 value.lag, data = .)
    ),
    purchases = map(data, ~dynlm(value ~ Purchases.1 + Fed.1 + EFSF_ESM.1 + VSTOXX + Marginal.Lending.Rate + 
                                 value.lag, data = .)
    ),
    LTRO = map(data, ~dynlm(value ~ LTRO.1 + Fed.1 + EFSF_ESM.1 + VSTOXX + Marginal.Lending.Rate + 
                            value.lag, data = .)
    ),
    FRFA = map(data, ~dynlm(value ~ FRTFA.1 + Fed.1 + EFSF_ESM.1 + VSTOXX + Marginal.Lending.Rate + 
                            value.lag, data = .)
    ),
    COLL = map(data, ~dynlm(value ~ COLL.1 + Fed.1 + EFSF_ESM.1 + VSTOXX + Marginal.Lending.Rate + 
                            value.lag, data = .)
    ),
    FOR = map(data, ~dynlm(value ~ FOR.1 + Fed.1 + EFSF_ESM.1 + VSTOXX + Marginal.Lending.Rate + 
                           value.lag, data = .)
    ),
    SMP = map(data, ~dynlm(value ~ SMP.1 + Fed.1 + EFSF_ESM.1 + VSTOXX + Marginal.Lending.Rate + 
                           value.lag, data = .)
    ),
    OMT = map(data, ~dynlm(value ~ OMT.1 + Fed.1 + EFSF_ESM.1 + VSTOXX + Marginal.Lending.Rate + 
                           value.lag, data = .)
    ),
    PSPP = map(data, ~dynlm(value ~ PSPP.1 + Fed.1 + EFSF_ESM.1 + VSTOXX + Marginal.Lending.Rate + 
                            value.lag, data = .)
    ),
    CBPP1 = map(data, ~dynlm(value ~ CBPP1.1 + Fed.1 + EFSF_ESM.1 + VSTOXX + Marginal.Lending.Rate +
                             value.lag, data = .)
    ),
    CBPP2 = map(data, ~dynlm(value ~ CBPP2.1 + Fed.1 + EFSF_ESM.1 + VSTOXX + Marginal.Lending.Rate +
                             value.lag, data = .)
    ),
    CBPP3 = map(data, ~dynlm(value ~ CBPP3.1 + Fed.1 + EFSF_ESM.1 + VSTOXX + Marginal.Lending.Rate +
                             value.lag, data = .)
    ),
    CBPP_all = map(data, ~dynlm(value ~ CBPP.1 + Fed.1 + EFSF_ESM.1 + VSTOXX + Marginal.Lending.Rate +
                                value.lag, data = .)
    ),
    ABSPP = map(data, ~dynlm(value ~ ABSPP.1 + Fed.1 + EFSF_ESM.1 + VSTOXX + Marginal.Lending.Rate +
                             value.lag, data = .)
    ),
    CSPP = map(data, ~dynlm(value ~ CSPP.1 + Fed.1 + EFSF_ESM.1 + VSTOXX + Marginal.Lending.Rate +
                            value.lag, data = .)
    ),
    FED = map(data, ~dynlm(value ~ Fed.1 + EFSF_ESM.1 + VSTOXX + Marginal.Lending.Rate +
                              value.lag, data = .)
    )
  ) %>% 
  # gather all models into single column
  gather(key = program, value = model, pooled:FED)  %>% 
  # compute HAC standard errors and t-statistics
  mutate(
    HACt_test = map(model, ~coeftest(.x, vcov = vcovHAC))
  )

models

# HAC errors ----------------------------------------------------------------------------

HAC <- models %>% 
  mutate(
    Estimate = map_dbl(HACt_test, ~.x[2, 1]),
    HACevent_se = map_dbl(HACt_test, ~.x[2, 2]),
    HACevent_pvalue = map_dbl(HACt_test, ~.x[2, 4])
  ) %>% 
  select(program, country, variable, sample, Estimate, HACevent_se, HACevent_pvalue) %>% 
  gather(key = "statistic", value = "value", 5:7) %>% 
  spread(key = variable, value = value) %>% 
  arrange(program, country, sample)
  
HAC

write_csv(HAC, path = "./HACerrors.csv")

# regression tables ---------------------------------------------------------------------

regtable <- function(.x, .var, .prog, .samp) {
  
  title <- paste(str_replace(.var, "_", " "), str_replace(.prog, "_", " "), str_replace(.samp, "_", " "))
  
  data <- .x %>% 
    filter(sample == .samp & program == .prog & variable == .var)
  
  stargazer(
    data$model,
    se = map(data$HACt_test[1:5], ~.x[,2]),
    p = map(data$HACt_test[1:5], ~.x[,4]),
    dep.var.labels = str_replace(.var, "_", " "),
    dep.var.labels.include = TRUE,
    column.labels = data$country,
    dep.var.caption = "",
    covariate.labels = c("ECB", "FED", "EFSF", "VSTOXX", "ECB Rate", "Lag", "Constant"),
    omit.stat = "ser",
    min.max = FALSE,
    df = FALSE, 
    align = TRUE,
    float = TRUE,
    font.size = "scriptsize",
    column.sep.width = "-15pt",
    header = FALSE,
    model.numbers = FALSE,
    digits = 2,
    title = title,
    label = paste0(.var, "_", .prog, "_", .samp),
    type = "latex",
    style = "aer",
    notes.align = "r",
    out = paste0(getwd(), "/latex/tables/", .prog, "_", .var, "_", .samp, ".tex")
  )
}

for(p in unique(models$program)){
  for(i in unique(models$variable)){
    regtable(
      .x = models, 
      .var = i,
      .prog = p,
      .samp = "full_sample"
    )
  }
}




