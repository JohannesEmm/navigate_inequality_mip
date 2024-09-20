#check if master.R has been run before)
if(!exists("measure_inequality")){
measure_inequality <- "Consumption"
graphdir <- paste0("graphs_", measure_inequality)
mip_data <- readRDS(here("inequality_mip_full.Rdata"))
}


graphdir <- paste0("graphs_", measure_inequality)
#use the function saveplot to save the graphs in the relative folders 
figure_format <- "png"
convert_pdftopng <- F #converts all created pdfs to png for better quality (needs pdftopng.exe in your PATH. Download from http://www.xpdfreader.com/download.html)
saveplot <- function(plotname, text_size=12, width=12, height=8, plot_title = T, plot_theme=theme_bw()){
  if(!dir.exists(file.path(graphdir))){dir.create(file.path(graphdir))}
  print(last_plot() + plot_theme)
  ggsave(filename=file.path(graphdir, paste0(as.character(gsub(" ", "_", plotname)),".", figure_format)), plot = if(plot_title){last_plot()}else{last_plot()} + theme(text = element_text(size=text_size)), width=width, height=height, dpi = "print")
  if(figure_format=="pdf" & convert_pdftopng) shell(str_glue('pdftopng.exe {file.path(graphdir, paste0(as.character(gsub(" ", "_", plotname)),".", figure_format))} - > {file.path(graphdir, paste0(as.character(gsub(" ", "_", plotname)),".", "png"))}'))
}


theme_set(theme_minimal(base_size = 12))

# mip_data <- iiasadb_data

# Decile incomes data
mip_income_d <- subset(mip_data, grepl(str_glue("{measure_inequality}\\|D"), mip_data$Variable)) %>% 
  pivot_wider(names_from = "Variable",
              values_from = "value")

#for simplicity rename to always keep Income as name
names(mip_income_d) <- gsub(measure_inequality, "Income", names(mip_income_d))

## "Income| deciles" variables are actually shares
## merging with total GDP

pop <- mip_data %>% 
  filter(Variable == "Population") %>% 
  pivot_wider(names_from = "Variable",
              values_from = "value")

gdp_ppp <- mip_data %>% 
  filter(Variable == "GDP|PPP") %>% 
  pivot_wider(names_from = "Variable",
              values_from = "value") %>% 
  full_join(pop, by = c("Scenario", "Model", "Region", "Year")) %>% 
  mutate(
    gdp_ppp_dollars = `GDP|PPP`*10^9, # convert to $ (like WB)
    pop_total = Population*10^6, # convert from millions (like WB)
    gdp_pc = gdp_ppp_dollars/pop_total,
    lgdp_pc = log(gdp_ppp_dollars/pop_total) 
  )

# Decile columns are list, so first convert them to numeric columns
# then append back

conv_list = function (x) {
  pippo <- x %>% 
    set_names(seq(1, nrow(mip_income_d), by = 1)) %>% 
    bind_rows()
  
  pippo <- pippo[1,]
  
  pippo <- data.frame(x = t(pippo))  
}

prova_list <- list()

prova_list <- lapply(mip_income_d[,5:14],
                     conv_list)

decile_cols <- prova_list %>% bind_cols() 

names(decile_cols) <- names(prova_list)

mip_income_d <- mip_income_d %>% select(Scenario:Year) %>% cbind(decile_cols)


# compute income levels by decile
mip_income_d <- mip_income_d %>% 
  full_join(gdp_ppp, by = c("Scenario", "Model", "Region", "Year")) %>% 
    mutate_at(vars(`Income|D1`:`Income|D9`), funs(level = gdp_pc*./10))
  
source("read_coefs_downscaling.R")

# Select only the 10 common regions
country_naming <- read.csv(here("data", "country_naming.csv"))

mip_income_d$Region <- stringi::stri_replace_all_regex(mip_income_d$Region,
                                                       pattern=country_naming$original,
                                                       replacement=country_naming$new, vectorize=FALSE)

mip_income_d <- mip_income_d %>% 
  filter(Region %in% unique(country_naming$new)) %>% 
  filter(Region != "World") %>% 
  mutate(iso3 = countrycode(Region,
                            origin = 'country.name', destination = 'iso3c'))


# Merge to find regional temperature anomaly, following Fra's work
source("ref_emissions.R")

coefs_mip <- coefs %>% 
  filter(iso3 %in% unique(mip_income_d$iso3))

mip_income_d <- mip_income_d %>% 
  full_join(ref_emissions, by = c("Scenario", "Model", "Year")) %>% 
  full_join(coefs_mip, by = "iso3") %>% 
  mutate(
    temp_regional = intercept + beta*dT_global
  )

# Pivot longer to get decile-level income in column (counterfactual)
mip_income_d <- mip_income_d %>%
  rename(temp_start = intercept) %>% 
  select(Scenario, Scenario_type, Model, Region, iso3, Year,
         `GDP|PPP`, gdp_pc, lgdp_pc, temp_start, temp_regional,
         `Income|D1_level`:`Income|D9_level`) %>% 
  pivot_longer(`Income|D1_level`:`Income|D9_level`,
               names_to = "Decile",
               values_to = "Decile_income") %>% 
  mutate(
    Decile = substr(Decile, start = 8, stop = nchar(Decile)),
    Decile = gsub('_level', '', Decile)
  ) %>% 
  filter(Year >= 2020)






#### Compute income elasticity of policy impacts ####

policy_df <- mip_income_d %>% 
  filter(Scenario == "REF" | Scenario == "Paris") %>% 
  select(Scenario, Model, Region, Year, Decile, Decile_income) %>% 
  # mutate(Decile_income = log(Decile_income)) %>%
  pivot_wider(
    names_from = Scenario,
    values_from = Decile_income
  ) %>% 
  mutate(
    delta_income_policy = Paris - REF, # choose whether relative or absolute
    delta_income_pol_rel = (Paris - REF)/REF
  ) %>% 
  group_by(Model, Region, Year) %>% 
  mutate(
    country_y_ref = mean(REF, na.rm = T),
    REFrel = REF/country_y_ref
  ) %>% 
  filter(Year >= 2020 & Year <= 2050)

#visual
mod_letters <- c("A", "E", "G", "I", "N", "r", "R", "W")

mod_letters_utf <- unlist(lapply(mod_letters, utf8ToInt))


#add model type to delta_income_policy
policy_df <- policy_df %>% mutate(Type=case_when(Model %in% c("AIM", "GEM-E3", "Imaclim") ~ "CGE", Model %in% c("ReMIND", "WITCH") ~ "DP-IAM", Model %in% c("NICE", "RICE50+") ~ "CB-IAM", Model %in% c("E3ME") ~ "Macroeconometric", TRUE ~ "Other"))

# Regressing difference in decile-level income due to policy on income levels under REF
policy_impact_reg <- lm(log(delta_income_policy) ~ log(REF) + Model + Region +
                          factor(Year),
                        data = policy_df %>% 
                          filter(delta_income_policy < 0) %>% 
                          mutate(delta_income_policy = -1*delta_income_policy) %>% mutate(Model=as.factor(Model), Model=relevel(Model, ref="AIM"), Region=as.factor(Region), Region=relevel(Region, ref="United States")))

stargazer(policy_impact_reg,
          type = "latex",
          dep.var.labels = "Change in decile income, from policy",
          model.names = FALSE,
          header=F,
          float=T,
          single.row = T,
          out = paste0(graphdir, "/policy_impact_elast.tex"))

hutils::replace_pattern_in("log\\(REF\\)", "Deciles under Reference scenario", file_pattern="*.tex", basedir = graphdir)
hutils::replace_pattern_in("Model|Region|Type","", file_pattern="*.tex", basedir = graphdir)
hutils::replace_pattern_in("factor\\(Year\\)","", file_pattern="*.tex", basedir = graphdir)
# reg_policy_obs <- cbind(policy_df, predict(object = policy_impact_reg, newdata = policy_df)) %>% filter(!is.na(...10))
# table(reg_policy_obs$Model, reg_policy_obs$Region)


### Plot policy impact elasticity across regions

# function to compute policy elasticity in each region
policy_elast = function(r) {
  
  df_tmp <- policy_df %>% mutate(Model=as.factor(Model), Model=relevel(Model, ref="AIM")) %>% 
    filter(Region == r) %>% filter(!is.na(delta_income_policy))
  if(length(unique(df_tmp$Model)) > 1){
      reg_tmp <- lm(log(delta_income_policy) ~ log(REF) + Model +
                    factor(Year),
                  data = df_tmp %>% 
                    filter(delta_income_policy < 0) %>% 
                    mutate(delta_income_policy = -1*delta_income_policy))
  } else {
    reg_tmp <- lm(log(delta_income_policy) ~ log(REF) + factor(Year),
                  data = df_tmp %>% 
                    filter(delta_income_policy < 0) %>% 
                    mutate(delta_income_policy = -1*delta_income_policy))
  }
      
    df_tmp$policy_elast <- coefficients(reg_tmp)[2]
    
  
  if(F){
    stargazer(reg_tmp,
            type = "latex",
            dep.var.labels = "Change in decile income, from policy",
            model.names = FALSE,
            header=F,
            float=T,
            single.row = T,
            out = paste0(graphdir, "/", r, "_", "policy_impact_elast.tex"))
  
  hutils::replace_pattern_in("log(REF)", "Deciles under Reference scenario", file_pattern="*.tex", basedir = graphdir)
  hutils::replace_pattern_in("Model|Region|Type","", file_pattern="*.tex", basedir = graphdir)
  hutils::replace_pattern_in("factor(Year)","", file_pattern="*.tex", basedir = graphdir)
  }
  print(paste(r, coefficients(reg_tmp)[2]))
  return(df_tmp)
}

policy_elast_df_plot <- list()
policy_elast_df_plot <- lapply(unique(policy_df$Region), policy_elast) %>% 
  bind_rows()

# merge with 2020 GDP per capita, in PPP $
gdp_pc <- mip_income_d %>% 
  filter(Year == 2020) %>% 
  select(Region, gdp_pc) %>% 
  group_by(Region) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  select(Region, gdp_pc)

### for revision: add elasticity estimation for all models
model_avg_elasticity <- policy_df  %>% filter(delta_income_policy < 0) %>% mutate(delta_income_policy = -1*delta_income_policy) %>% filter(Model!="E3ME") %>% group_by(Model) %>% nest() %>% 
  mutate(lm_model = map(data, ~lm(log(delta_income_policy) ~ log(REF) + Region + factor(Year), data = .))) %>% mutate(tidy = map(lm_model, broom::tidy),
                                                                                                                      glance = map(lm_model, broom::glance),
                                                                                                                      augment = map(lm_model, broom::augment),
                                                                                                                      rsq = glance %>% map_dbl('r.squared'),
                                                                                                                      average_elasticity = tidy %>% map_dbl(function(x) x$estimate[2])) 
#slope per model
slope_per_model <- policy_df  %>% filter(delta_income_policy < 0) %>% mutate(delta_income_policy = -1*delta_income_policy) %>% filter(Model!="E3ME") %>% group_by(Model, Region) %>% nest() %>% 
  mutate(lm_model = map(data, ~lm(log(delta_income_policy) ~ log(REF) + factor(Year), data = .))) %>% mutate(tidy = map(lm_model, broom::tidy),
                                                                                                             glance = map(lm_model, broom::glance),
                                                                                                             augment = map(lm_model, broom::augment),
                                                                                                             rsq = glance %>% map_dbl('r.squared'),
                                                                                                             slope = tidy %>% map_dbl(function(x) x$estimate[2])) %>%
  left_join(gdp_pc) 
slope_per_model <- slope_per_model %>% ungroup() %>% group_by(Model) %>% nest() %>% mutate(lm_model = map(data, ~lm(slope ~ gdp_pc, data = .))) %>% mutate(tidy = map(lm_model, broom::tidy),
                                                                                                                                                           glance = map(lm_model, broom::glance),
                                                                                                                                                           augment = map(lm_model, broom::augment),
                                                                                                                                                           rsq = glance %>% map_dbl('r.squared'),
                                                                                                                                                           slope = tidy %>% map_dbl(function(x) x$estimate[2]*1e3),
                                                                                                                                                           slope_significance = tidy %>% map_dbl(function(x) x$p.value[2]))

model_elasticity_table <- model_avg_elasticity %>% select(Model, average_elasticity) %>% left_join(slope_per_model %>% select(Model, slope, slope_significance)) %>% mutate(slope_significance=paste0(sprintf("%01.2f",slope_significance),gtools::stars.pval(slope_significance)), average_elasticity=round(average_elasticity, 2), slope=round(slope, 4)) %>% arrange(Model) 
print(model_elasticity_table)
write_xlsx(model_elasticity_table, paste0(graphdir, "/model_elasticity_table.xlsx"))

#now pooled regression without region dummy (within and between)
lm(log(delta_income_policy) ~ log(REF) + Model +
     factor(Year),
   data = policy_df %>% 
     filter(delta_income_policy < 0) %>% 
     mutate(delta_income_policy = -1*delta_income_policy) %>% mutate(Model=as.factor(Model), Model=relevel(Model, ref="AIM"), Region=as.factor(Region), Region=relevel(Region, ref="United States")))
#now pooled regression WITH region dummy (only within variation)
lm(log(delta_income_policy) ~ log(REF) + Model + Region +
     factor(Year),
   data = policy_df %>% 
     filter(delta_income_policy < 0) %>% 
     mutate(delta_income_policy = -1*delta_income_policy) %>% mutate(Model=as.factor(Model), Model=relevel(Model, ref="AIM"), Region=as.factor(Region), Region=relevel(Region, ref="United States")))

#simple regression of regional mean elasticity estimates against gdppc for figure
summary(lm(policy_elast ~ gdp_pc, data = policy_elast_df_plot %>% 
  full_join(gdp_pc, by = "Region") %>% 
  group_by(Region) %>% 
  slice_head(n = 1) %>% mutate(gdp_pc=gdp_pc*1e-3)))


###



ggplot(policy_elast_df_plot %>% 
         full_join(gdp_pc, by = "Region") %>% 
         group_by(Region) %>% 
         slice_head(n = 1),
       aes(x = gdp_pc, y = policy_elast)) +
  geom_hline(yintercept = 1, linetype = "solid", color = "grey50") +
  geom_point(aes(color = Region))   + # Show dots
  geom_label_repel(aes(color = Region, label = Region), box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  guides(label = "none", color="none") +
  labs(x = "GDP per capita in 2020 (PPP)",
       y = "Climate Policy Income Elasticity") +
  scale_x_continuous(labels = scales::dollar) + 
geom_smooth(method="lm", formula = y ~ x, colour="black") +
  ggpmisc::stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label..)),  parse = TRUE, size = 4, col = "black") +
  geom_text(x = 15000, y = 1.12, label = 'Slope: -0.0054/k$', size = 6, color="black")
saveplot("Policy Elasticity by Country Income", width = 8, height = 6)


#### Compute damages: region-level ####


# Load coefficients for decile impact functions
## BHM
coefs_bhm <- read.csv(here("data", "coefs_bhm.csv"))

b1_bhm <- coefs_bhm$temperature_mean[11]

b2_bhm <- coefs_bhm$temperature_mean2[11]


## Adaptation
coefs_ada <- read.csv(here("data", "coefs_ada.csv"))

b1 <- coefs_ada[11, 2]

b2 <- coefs_ada[11, 3]

a1 <- coefs_ada[11, 4]

a2 <- coefs_ada[11, 5]


# First, create a "t" variable with 2015 ==> t=0
mip_income_d <- mip_income_d %>% 
  filter(Year >= 2020) %>% # change here if you want to start in 2015
  mutate(
    t = (Year - 2015)/5 # +1 
  )


# counterfactual growth rates of regional GDP
## creating a region-level dataframe (re-merge later)
mip_income_reg <- mip_income_d %>% 
  select(Scenario:temp_regional, t) %>% 
  distinct() %>% 
  group_by(Scenario, Model, Region) %>% 
  mutate(
    growth_counter = gdp_pc/lag(gdp_pc) - 1
  )

mip_income_reg$growth_counter[is.na(mip_income_reg$growth_counter)] <- 0

# set initial value of regional GDP under impacts
gdp_start <- mip_income_reg %>% 
  select(Scenario, Model, Region, Year, gdp_pc) %>% 
  group_by(Scenario, Model, Region) %>% 
  slice_head(n=1) %>% 
  rename(gdp_start = gdp_pc)

mip_income_reg <- mip_income_reg %>% 
  full_join(gdp_start, by = c("Scenario", "Model", "Region", "Year"))


# damage factor under BHM specification
mip_income_reg <- mip_income_reg %>% 
  group_by(Scenario, Model, Region) %>% 
  mutate(
    d = (temp_regional - temp_start)*b1_bhm + (temp_regional^2 - temp_start^2)*b2_bhm
  )

# Separate models that go until 2050 from the others (until 2100)
mip_income_2050 <- mip_income_reg %>% 
  filter(Model == "E3ME")


mip_income_reg <- mip_income_reg %>% 
  filter(Model != "E3ME")


# Loops for growth effect on regional GDP

time_length <- as.numeric(length(unique(mip_income_reg$t)))

### under BHM specification

# For models with results until 2100
mip_income_reg$gdp_with_impact_bhm[mip_income_reg$t==1] = mip_income_reg$gdp_start[mip_income_reg$t==1]

for (i in 2:time_length) {
  mip_income_reg$gdp_with_impact_bhm[mip_income_reg$t==i] = (1 + mip_income_reg$growth_counter[mip_income_reg$t==i]
                                                             + mip_income_reg$d[mip_income_reg$t==i])*mip_income_reg$gdp_with_impact_bhm[mip_income_reg$t==i-1]
}


# For models with results until 2050
mip_income_2050$gdp_with_impact_bhm[mip_income_2050$t==1] = mip_income_2050$gdp_start[mip_income_2050$t==1]

for (i in 2:7) {
  mip_income_2050$gdp_with_impact_bhm[mip_income_2050$t==i] = (1 + mip_income_2050$growth_counter[mip_income_2050$t==i]
                                                               + mip_income_2050$d[mip_income_2050$t==i])*mip_income_2050$gdp_with_impact_bhm[mip_income_2050$t==i-1]
}


### under Adaptation specification

# For models until 2100
mip_income_reg$gdp_with_impact_ada[mip_income_reg$t==1] = mip_income_reg$gdp_start[mip_income_reg$t==1]

# loop for growth rates, includes d factor inside
for (i in 2:time_length) {
  
  mip_income_reg$gdp_with_impact_ada[mip_income_reg$t==i] =
    # growth rates without impacts, counterfactual
    (1 + mip_income_reg$growth_counter[mip_income_reg$t==i] +
       # damage factor
       (mip_income_reg$temp_regional[mip_income_reg$t==i] - mip_income_reg$temp_start[mip_income_reg$t==i])*(b1 + a1*log(mip_income_reg$gdp_with_impact_ada[mip_income_reg$t==i-1])) +
       ((mip_income_reg$temp_regional[mip_income_reg$t==i])^2 - (mip_income_reg$temp_start[mip_income_reg$t==i])^2)*(b2 + a2*log(mip_income_reg$gdp_with_impact_ada[mip_income_reg$t==i-1]))
    )*
    mip_income_reg$gdp_with_impact_ada[mip_income_reg$t==i-1]
}

# For models until 2050
mip_income_2050$gdp_with_impact_ada[mip_income_2050$t==1] = mip_income_2050$gdp_start[mip_income_2050$t==1]

# loop for growth rates, includes d factor inside
for (i in 2:time_length) {
  
  mip_income_2050$gdp_with_impact_ada[mip_income_2050$t==i] =
    # growth rates without impacts, counterfactual
    (1 + mip_income_2050$growth_counter[mip_income_2050$t==i] +
       # damage factor
       (mip_income_2050$temp_regional[mip_income_2050$t==i] - mip_income_2050$temp_start[mip_income_2050$t==i])*(b1 + a1*log(mip_income_2050$gdp_with_impact_ada[mip_income_2050$t==i-1])) +
       ((mip_income_2050$temp_regional[mip_income_2050$t==i])^2 - (mip_income_2050$temp_start[mip_income_2050$t==i])^2)*(b2 + a2*log(mip_income_2050$gdp_with_impact_ada[mip_income_2050$t==i-1]))
    )*
    mip_income_2050$gdp_with_impact_ada[mip_income_2050$t==i-1]
}

mip_income_reg <- rbind(mip_income_reg, mip_income_2050) # append them back together

mip_income_reg <- mip_income_reg %>% 
  group_by(Scenario, Model, Region) %>% 
  mutate(
    damages_bhm = gdp_pc - gdp_with_impact_bhm,
    damages_ada = gdp_pc - gdp_with_impact_ada,
    dam_frac_bhm = (gdp_pc - gdp_with_impact_bhm)/gdp_pc,
    dam_frac_ada = (gdp_pc - gdp_with_impact_ada)/gdp_pc
  )

# ggplot(mip_income_reg %>% filter(Scenario == "REF"),
#        aes(x = Year, y = dam_frac_bhm, color = Model)) +
#   geom_line() +
#   facet_wrap(~ Region, ncol = 5) +
#   theme_bw()
# 
# ggplot(mip_income_reg %>% filter(Scenario == "REF"),
#        aes(x = Year, y = dam_frac_ada, color = Model)) +
#   geom_line() +
#   facet_wrap(~ Region, ncol = 5) +
#   theme_bw()

#### Compute damages: decile-level ####

# Counterfactual growth rates for deciles
mip_income_d <- mip_income_d %>% 
  group_by(Scenario, Model, Region, Decile) %>% 
  mutate(
    dist_growth_counter = Decile_income/lag(Decile_income, n=1) - 1
  )

# set growth in initial period to 0
mip_income_d$dist_growth_counter[is.na(mip_income_d$dist_growth_counter)] <- 0


# Compute impacts on deciles with growth effect

## Create a "Decile_income_start" variable to initialize loop
decile_start <- mip_income_d %>% 
  select(Scenario, Model, Region, Decile, Decile_income) %>% 
  group_by(Scenario, Model, Region, Decile) %>% 
  slice_head(n=1) %>% 
  rename(Decile_income_start = Decile_income)

mip_income_d <- mip_income_d %>% 
  full_join(decile_start, by = c("Scenario", "Model", "Region", "Decile"))

rm(list = ls(pattern = "_start$"))

## numeric identifier of deciles, useful for loop
mip_income_d$dist_num <- as.numeric(gsub(".*?([0-9]+).*", "\\1", 
                                         mip_income_d$Decile))


##### BHM specification

# Compute damage factor on growth rates of decile income
for(i in 1:10) {
  mip_income_d$d_dist_bhm[mip_income_d$dist_num == i] <-  (mip_income_d$temp_regional[mip_income_d$dist_num == i]
                                                           - mip_income_d$temp_start[mip_income_d$dist_num == i])*coefs_bhm[i,4] +
    (mip_income_d$temp_regional[mip_income_d$dist_num == i]^2 -
       mip_income_d$temp_start[mip_income_d$dist_num == i]^2)*coefs_bhm[i,5]
  
}

## Initial value of impacted decile income
for(j in 1:10) {
  mip_income_d$Dec_with_impact_bhm[mip_income_d$t==1 & mip_income_d$dist_num==j] = mip_income_d$Decile_income_start[mip_income_d$t==1 & mip_income_d$dist_num==j]
}

# Compute impacted decile income for every period

### Separately for models that go until 2050 and the others (until 2100)
mip_income_d2050 <- mip_income_d %>% 
  filter(Model == "E3ME")

mip_income_d <- mip_income_d %>% 
  filter(Model != "E3ME")

### For models until 2100

for (i in 2:17) {
  for (j in 1:10) {
    mip_income_d$Dec_with_impact_bhm[mip_income_d$t==i & mip_income_d$dist_num==j] = (1 + mip_income_d$dist_growth_counter[mip_income_d$t==i & mip_income_d$dist_num==j] +
                                                                                        mip_income_d$d_dist_bhm[mip_income_d$t==i & mip_income_d$dist_num==j])*mip_income_d$Dec_with_impact_bhm[mip_income_d$t==i-1 & mip_income_d$dist_num==j]
  }
}

### For models until 2050
for (i in 2:7) {
  for (j in 1:10) {
    mip_income_d2050$Dec_with_impact_bhm[mip_income_d2050$t==i & mip_income_d2050$dist_num==j] = (1 + mip_income_d2050$dist_growth_counter[mip_income_d2050$t==i & mip_income_d2050$dist_num==j] +
                                                                                                    mip_income_d2050$d_dist_bhm[mip_income_d2050$t==i & mip_income_d2050$dist_num==j])*mip_income_d2050$Dec_with_impact_bhm[mip_income_d2050$t==i-1 & mip_income_d2050$dist_num==j]
  }
}

# Compute damages, absolute and relative
mip_income_d <- rbind(mip_income_d, mip_income_d2050) %>% 
  group_by(Scenario, Model, Region, Decile) %>% 
  mutate(
    damages_dist_bhm = Decile_income - Dec_with_impact_bhm,
    dam_dist_frac_bhm = (Decile_income - Dec_with_impact_bhm)/Decile_income 
  )

# Compute region-level damages (sum across deciles)
mip_income_d <- mip_income_d %>% 
  group_by(Scenario, Model, Region, Year) %>% 
  mutate(
    Reg_damages_bhm_frac = sum(damages_dist_bhm)/sum(Decile_income), # damages
    `GDP|PPP_with_impact_bhm` = (1 - Reg_damages_bhm_frac)*`GDP|PPP` # GDP reg. with impacts
  )

# Compute Gini, both counterfactual and under impacts
mip_income_d <- mip_income_d %>% 
  group_by(Scenario, Model, Region, Year) %>% 
  mutate(
    gini_counter = reldist::gini(Decile_income),
    gini_impact_bhm = reldist::gini(Dec_with_impact_bhm),
    delta_gini_bhm = gini_impact_bhm - gini_counter
  )


##### Adaptation specification

# merge with total GDP under impacts with Adaptation spec
mip_income_d <- mip_income_d %>% 
  full_join(mip_income_reg %>% select(Scenario, Model, Region,Year,
                                      gdp_with_impact_ada),
            by = c("Scenario", "Model", "Region", "Year"))

## Initial value of impacted decile income
for(j in 1:10) {
  mip_income_d$Dec_with_impact_ada[mip_income_d$t==1 & mip_income_d$dist_num==j] = mip_income_d$Decile_income_start[mip_income_d$t==1 & mip_income_d$dist_num==j]
}

# Compute impacted decile income for every period, with d factor inside loop

### Separately for models that go until 2050 and the others (until 2100)
mip_income_d2050 <- mip_income_d %>% 
  filter(Model == "E3ME")

for (i in 2:time_length) {
  for (j in 1:10) {
    mip_income_d2050$Dec_with_impact_ada[mip_income_d2050$t==i & mip_income_d2050$dist_num==j] =
      (1 + 
         # counterfactual growth rate
         mip_income_d2050$dist_growth_counter[mip_income_d2050$t==i & mip_income_d2050$dist_num==j] +
         # damage factor
         (mip_income_d2050$temp_regional[mip_income_d2050$t==i & mip_income_d2050$dist_num==j] - mip_income_d2050$temp_start[mip_income_d2050$t==i & mip_income_d2050$dist_num==j])*
         (coefs_ada[j,2] + coefs_ada[j,4]*log(mip_income_d2050$gdp_with_impact_ada[mip_income_d2050$t==i-1 & mip_income_d2050$dist_num==j])) +
         (mip_income_d2050$temp_regional[mip_income_d2050$t==i & mip_income_d2050$dist_num==j]^2 - mip_income_d2050$temp_start[mip_income_d2050$t==i & mip_income_d2050$dist_num==j]^2)*
         (coefs_ada[j,3] + coefs_ada[j,5]*log(mip_income_d2050$gdp_with_impact_ada[mip_income_d2050$t==i-1 & mip_income_d2050$dist_num==j]))
      )*
      mip_income_d2050$Dec_with_impact_ada[mip_income_d2050$t==i-1 & mip_income_d2050$dist_num==j]
  }
}


mip_income_d <- mip_income_d %>% 
  filter(Model != "E3ME")

for (i in 2:time_length) {
  for (j in 1:10) {
    mip_income_d$Dec_with_impact_ada[mip_income_d$t==i & mip_income_d$dist_num==j] =
      (1 + 
         # counterfactual growth rate
         mip_income_d$dist_growth_counter[mip_income_d$t==i & mip_income_d$dist_num==j] +
         # damage factor
         (mip_income_d$temp_regional[mip_income_d$t==i & mip_income_d$dist_num==j] - mip_income_d$temp_start[mip_income_d$t==i & mip_income_d$dist_num==j])*
         (coefs_ada[j,2] + coefs_ada[j,4]*log(mip_income_d$gdp_with_impact_ada[mip_income_d$t==i-1 & mip_income_d$dist_num==j])) +
         (mip_income_d$temp_regional[mip_income_d$t==i & mip_income_d$dist_num==j]^2 - mip_income_d$temp_start[mip_income_d$t==i & mip_income_d$dist_num==j]^2)*
         (coefs_ada[j,3] + coefs_ada[j,5]*log(mip_income_d$gdp_with_impact_ada[mip_income_d$t==i-1 & mip_income_d$dist_num==j]))
      )*
      mip_income_d$Dec_with_impact_ada[mip_income_d$t==i-1 & mip_income_d$dist_num==j]
  }
}

# Compute damages, absolute and relative
mip_income_d <- rbind(mip_income_d, mip_income_d2050) %>% 
  group_by(Scenario, Model, Region, Decile) %>% 
  mutate(
    damages_dist_ada = Decile_income - Dec_with_impact_ada,
    dam_dist_frac_ada = (Decile_income - Dec_with_impact_ada)/Decile_income 
  )

# Compute region-level damages (sum across deciles)
mip_income_d <- mip_income_d %>% 
  group_by(Scenario, Model, Region, Year) %>% 
  mutate(
    Reg_damages_ada_frac = sum(damages_dist_ada)/sum(Decile_income), # damages
    `GDP|PPP_with_impact_ada` = (1 - Reg_damages_ada_frac)*`GDP|PPP` # GDP reg. with impacts
  )

# Compute Gini, both counterfactual and under impacts
mip_income_d <- mip_income_d %>% 
  group_by(Scenario, Model, Region, Year) %>% 
  mutate(
    # gini_counter = reldist::gini(Decile_income),
    gini_impact_ada = reldist::gini(Dec_with_impact_ada),
    delta_gini_ada = gini_impact_ada - gini_counter
  )

# ggplot(mip_income_d %>% filter(Scenario == "REF"),
#        aes(x = Year, y = Reg_damages_bhm_frac, color = Model)) +
#   geom_line() +
#   facet_wrap(~ Region, ncol = 5) +
#   theme_bw()
# 
# 
# ggplot(mip_income_d %>% filter(Scenario == "REF"),
#        aes(x = Year, y = delta_gini_ada, color = Model)) +
#   geom_line() +
#   facet_wrap(~ Region, ncol = 5) +
#   theme_bw()

#### Create dataframe with post-processed impacts, for all models and scenarios ####

### 2 Variables to add to mip_data, per spec = 4 total 

### (Dec_with_impact_*, then compute GDP|PPP_with_impact_* as sum of Dec_with_impact)

# BHM Spec

## Decile impacts
### double pivot to get consistent column names in long format
df_impact_dist_bhm <- mip_income_d %>% 
  group_by(Scenario, Model, Region, Year) %>% 
  mutate(Reg_income = sum(Dec_with_impact_bhm),
         D_share_with_impact_bhm = (Dec_with_impact_bhm/Reg_income)*100) %>% 
  select(Scenario, Model, Region, Year,
         Decile, D_share_with_impact_bhm) %>%
  rename(Dec_with_impact_bhm = D_share_with_impact_bhm) %>% 
  pivot_wider(
    names_from = Decile,
    names_prefix = "Dec_with_impact_bhm|",
    values_from = Dec_with_impact_bhm
  ) %>% 
  pivot_longer(
    cols = `Dec_with_impact_bhm|D1`:`Dec_with_impact_bhm|D9`,
    names_to = "Variable",
    values_to = "value"
  )


## Regional impacts
df_impact_reg_bhm <- mip_income_d %>% 
  select(Scenario, Model, Region, Year, `GDP|PPP_with_impact_bhm`) %>% 
  pivot_longer(cols = `GDP|PPP_with_impact_bhm`,
               names_to = "Variable",
               values_to = "value") %>% 
  group_by(Scenario, Model, Region, Year) %>% 
  slice_head(n = 1) # the GDP variable is the same for all deciles in a country


# Adaptation spec

## Decile impacts
### double pivot to get consistent column names in long format
df_impact_dist_ada <- mip_income_d %>% 
  group_by(Scenario, Model, Region, Year) %>% 
  mutate(Reg_income = sum(Dec_with_impact_ada),
         D_share_with_impact_ada = (Dec_with_impact_ada/Reg_income)*100) %>% 
  select(Scenario, Model, Region, Year,
         Decile, D_share_with_impact_ada) %>%
  rename(Dec_with_impact_ada = D_share_with_impact_ada) %>% 
  pivot_wider(
    names_from = Decile,
    names_prefix = "Dec_with_impact_ada|",
    values_from = Dec_with_impact_ada
  ) %>% 
  pivot_longer(
    cols = `Dec_with_impact_ada|D1`:`Dec_with_impact_ada|D9`,
    names_to = "Variable",
    values_to = "value"
  )


## Regional impacts
df_impact_reg_ada <- mip_income_d %>% 
  select(Scenario, Model, Region, Year, `GDP|PPP_with_impact_ada`) %>% 
  pivot_longer(cols = `GDP|PPP_with_impact_ada`,
               names_to = "Variable",
               values_to = "value") %>% 
  group_by(Scenario, Model, Region, Year) %>% 
  slice_head(n = 1) # the GDP variable is the same for all deciles in a country


# Create df with impact variables
df_impacts_all <- rbind(df_impact_dist_bhm, df_impact_dist_ada,
                        df_impact_reg_ada, df_impact_reg_bhm)



#  Append to original data
mip_data <- rbind(mip_data, df_impacts_all)

# Now I want to: set to NA the impacts for scenarios that have them already from the models (those that have *impacts in the scenario name)
## I create a new set of scenarios with different names and saving the post-processed impacts for those
### Then I put everything together

df_old_scen_names <- mip_data %>% 
  filter(str_detect(Scenario, "impact"))

df_new_scen_names <- df_old_scen_names %>% 
  mutate(
    Scenarios_new_name = paste(Scenario, "post_process", sep = "_")
  ) %>%
  ungroup() %>% 
  select(-Scenario) %>% 
  rename(Scenario = Scenarios_new_name) %>% 
  select(Scenario, Variable, everything())

df_old_scen_names <- df_old_scen_names %>% 
  mutate(
    value = case_when(str_detect(Variable, "impact") ~ NA_real_,
                      TRUE ~ value)
  )

df_no_impacts_scen <- mip_data %>% 
  filter(str_detect(Scenario, "impact", negate = T))

## all together, the rest has stayed the same
mip_data <- rbind(df_no_impacts_scen, df_old_scen_names, df_new_scen_names)

rm(list = ls(pattern = "^df_"))

save(mip_data, file = "prova_mip.Rdata")


#### Make all the plots for impacts ####
source("plot_impacts.R")

