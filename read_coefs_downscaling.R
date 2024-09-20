# Taking average coefficients across all models, per country
# only focus on pop-weighted coefficients

coefs <- read_parquet(here("data", "coefficients.parq")) %>% 
  group_by(iso3) %>% 
  summarise(
    intercept = mean(Intercept_coef, na.rm = T),
    beta = mean(gmt_coef, na.rm = T)
  )

