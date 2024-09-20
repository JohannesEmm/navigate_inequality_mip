# load(here("data", "inequalit_mip_full.Rdata"))

#### Compute global emissions by Scenario-Model-Year ####

# Imaclim: "WLD"
# GEM_E3: "WORLD"
# NICE: "WORLD"
# REMIND: no world region, must aggregate macro-regions
# AIM/PHI: same as above
# E3ME: no world, only has energy & industry emissions
# RICE50+: no world region, must aggregate macro-regions
# WITCH: "world"

### I will create manually a subset for each model and compute global emissions 

df_imaclim <- mip_data %>%
  filter(Model == "Imaclim") %>% 
  filter(Variable == "Emissions|CO2") %>% 
  filter(Region == "World") %>%
  pivot_wider(names_from = "Variable", 
              values_from = "value") %>% 
  group_by(Scenario, Model, Year) %>% 
  summarise(
    global_emissions = sum(`Emissions|CO2`)
  )

df_geme3 <- mip_data %>%
  filter(Model == "GEM-E3") %>% 
  filter(Variable == "Emissions|CO2") %>% 
  filter(Region == "WORLD") %>%
  pivot_wider(names_from = "Variable", 
              values_from = "value") %>% 
  group_by(Scenario, Model, Year) %>% 
  summarise(
    global_emissions = sum(`Emissions|CO2`)
  )

df_nice <- mip_data %>%
  filter(Model == "NICE") %>% 
  filter(Variable == "Emissions|CO2") %>% 
  filter(Region == "WORLD") %>%
  pivot_wider(names_from = "Variable", 
              values_from = "value") %>% 
  group_by(Scenario, Model, Year) %>% 
  summarise(
    global_emissions = sum(`Emissions|CO2`)
  )

df_remind <- mip_data %>%
  filter(Model == "ReMIND") %>% 
  filter(Variable == "Emissions|CO2") %>% 
  pivot_wider(names_from = "Variable", 
              values_from = "value") %>% 
  group_by(Scenario, Model, Year) %>% 
  summarise(
    global_emissions = sum(`Emissions|CO2`)
  )

df_aim <- mip_data %>%
  filter(Model == "AIM") %>% 
  filter(Variable == "Emissions|CO2") %>% 
  pivot_wider(names_from = "Variable", 
              values_from = "value") %>% 
  group_by(Scenario, Model, Year) %>% 
  summarise(
    global_emissions = sum(`Emissions|CO2`)
  )

df_e3me <- mip_data %>% 
  filter(Model == "E3ME") %>% 
  filter(Variable == "Emissions|CO2") %>% 
  # filter(Variable == "Emissions|CO2|Energy and Industrial Processes") %>% 
  pivot_wider(names_from = "Variable", 
              values_from = "value") %>% 
  group_by(Scenario, Model, Year) %>% 
  summarise(
    global_emissions = sum(`Emissions|CO2`)
    # global_emissions = sum(`Emissions|CO2|Energy and Industrial Processes`)
  )

df_rice <- mip_data %>%
  filter(Model == "RICE50+") %>% 
  filter(Variable == "Emissions|CO2") %>% 
  pivot_wider(names_from = "Variable", 
              values_from = "value") %>% 
  group_by(Scenario, Model, Year) %>% 
  summarise(
    global_emissions = sum(`Emissions|CO2`)
  )

df_witch <- mip_data %>%
  filter(Model == "WITCH") %>% 
  filter(Variable == "Emissions|CO2") %>%
  filter(Region == "world") %>%
  pivot_wider(names_from = "Variable", 
              values_from = "value") %>% 
  group_by(Scenario, Model, Year) %>% 
  summarise(
    global_emissions = sum(`Emissions|CO2`)
  )

ref_emissions <- rbind(df_aim, df_e3me, df_geme3, df_imaclim, df_nice, df_remind, df_rice, df_witch)

rm(list=ls(pattern="^df_"))

#### Now compute emissions 

ref_emissions <- ref_emissions %>%
  mutate(
    lag_emi = lag(global_emissions, n=1),
    for_cum = ifelse(is.na(lag_emi), 0, lag_emi),
    for_cum_5 = for_cum*5) %>% 
  select(-lag_emi)

# Multiply 2100 emissions by 2.5 factor rather than 5
ref_emissions$for_cum_5[ref_emissions$Year==2100] <- 2.5*ref_emissions$for_cum[ref_emissions$Year==2100]


ref_emissions <- ref_emissions %>% 
  filter(Year >= 2015) %>% 
  group_by(Scenario, Model) %>% 
  mutate(
cum_global_emissions = cumsum(for_cum_5)) %>% 
  select(-for_cum, -for_cum_5)

#make cumulative emissions start at 0 in 2020
ref_emissions <- ref_emissions %>% group_by(Scenario,Model) %>% mutate(cum_global_emissions=cum_global_emissions-mean(cum_global_emissions[Year==2020]))

# Stop E3ME cumulative emissions in 2050
ref_emissions$cum_global_emissions[ref_emissions$Model=="E3ME" & ref_emissions$Year > 2050] <- NA


#replace processed by ""
ref_emissions$Scenario <- gsub("_post_process", "", ref_emissions$Scenario)


## Create scenario type for better plots
ref_emissions <- ref_emissions %>% 
  mutate(Scenario_type = case_when(
    Scenario == "REF" | Scenario == "REF_impact" ~ "Reference",
    Scenario == "Paris" | Scenario == "Paris_impact" | Scenario == "Paris_impact_redist" | Scenario == "Paris_redist" ~ "Paris",
    Scenario == "1150" | Scenario == "1150_impact" | Scenario == "1150_impact_redist" | Scenario == "1150_redist" ~ "1150",
    TRUE ~ as.character(Scenario)
  ))

## Create factor variable for budget
ref_emissions <- ref_emissions %>% 
  mutate(budget = case_when (
    Scenario == "Paris" | Scenario == "Paris_impact" | Scenario == "Paris_impact_redist" | Scenario == "Paris_redist" ~ 650,
    Scenario == "1150" | Scenario == "1150_impact" | Scenario == "1150_impact_redist" | Scenario == "1150_redist" ~ 1150,
  ))

# Annual emissions, by scenario type
p_fl <- ggplot(ref_emissions %>% filter(!str_detect(Scenario, "1150")) %>% mutate(Scenario=gsub("redist", "epc", Scenario)) %>% 
                 mutate(Scenario_type = factor(Scenario_type, levels = c("Reference", "Paris", "1150")))) +
  geom_line(aes(x = Year, y = global_emissions/1000,
                color = Model, linetype = Scenario)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlim(c(2020, 2100)) +
  facet_wrap(~ Scenario_type) +
  labs(y = "Gt CO2/yr", title = "Flow: annual emissions") +
  theme_bw() 

# Cumulative emissions, by scenario type
p_st <- ggplot(ref_emissions %>% filter(!str_detect(Scenario, "1150")) %>% mutate(Scenario=gsub("redist", "epc", Scenario)) %>% 
                 mutate(Scenario_type = factor(Scenario_type, levels = c("Reference", "Paris", "1150")))) +
  geom_line(aes(x = Year, y = cum_global_emissions/1000,
                color = Model, linetype = Scenario)) +
  geom_hline(aes(yintercept = budget), linetype = "dashed") +
  xlim(c(2020, 2100)) +
  facet_wrap(~ Scenario_type) +
  labs(y = "Gt CO2", title = "Stock: cumulative emissions") +
  theme_bw()


#### Compute global temperature anomaly ####

TCRE <- 0.44 # increase in °C per 1000 GtCO2 (Matthews et al., 2021)

ref_emissions <- ref_emissions %>% 
  mutate(
    dT_global = TCRE*(cum_global_emissions/(1000^2)) # convert to GtCO2 and then in per 1000
  )

p_t <- ggplot(ref_emissions %>% filter(!str_detect(Scenario, "1150")) %>% mutate(Scenario=gsub("redist", "epc", Scenario)) %>% 
         mutate(Scenario_type = factor(Scenario_type, levels = c("Reference", "Paris", "1150")))) +
  geom_line(aes(x = Year, y = dT_global + 1.1,
                color = Model, linetype = Scenario)) +
  xlim(c(2020, 2100)) +
  labs(y = "Degrees Celsius", title = "Temperature anomaly") +
  facet_wrap(~ Scenario_type) +
  theme_bw()

ggarrange(p_fl, p_st, p_t, nrow = 3, ncol = 1,
          common.legend = T, legend = "right")

ggsave(filename = "Emissions_Temperature_over_scenarios_global.png",
       width = 9, height = 6, path = graphdir) 

