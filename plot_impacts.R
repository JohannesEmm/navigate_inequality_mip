#### Prepare data for plots ####

## custom function
`%nin%` = Negate(`%in%`)

# Save temperature data
mip_50_final <- mip_income_d %>% 
  filter(Model == "E3ME" | Model == "AIM") %>% 
  # filter(Year == 2050) %>% 
  group_by(Model, Region) %>% 
  mutate(
    temp_final = temp_regional,
    delta_temp = temp_final - temp_start
  ) %>% 
  select(Scenario, Model, Region, Year, delta_temp) %>% 
  distinct()

mip_100_final <- mip_income_d %>% 
  filter(Model != "E3ME" & Model != "AIM") %>% 
  # filter(Year == 2100) %>% 
  group_by(Model, Region) %>% 
  mutate(
    temp_final = temp_regional,
    delta_temp = temp_final - temp_start
  ) %>% 
  select(Scenario, Model, Region, Year, delta_temp) %>% 
  distinct()

# Filter out the models that already have their own impacts
models_with_impacts <- c("NICE", "ReMIND", "RICE50+")

## Impacts under REF
prova_ref <- mip_income_d  %>%
  filter(Model %in% models_with_impacts) %>% 
  filter(str_detect(Scenario, "REF")) %>% 
  select(
    Scenario, Model, Region, Decile, Year, Decile_income
  ) %>% 
  pivot_wider(
    names_from = Scenario,
    values_from = Decile_income
  ) %>% 
  mutate(
    damages_dist_bhm = REF - REF_impact,
    damages_dist_ada = damages_dist_bhm, # these impacts don't depend on Spec.
    dam_dist_frac_bhm = (REF - REF_impact)/REF,
    dam_dist_frac_ada = dam_dist_frac_bhm
  ) %>% 
  group_by(Model, Region, Year) %>% 
  mutate(
    Reg_impact = sum(REF, na.rm = T) - sum(REF_impact, na.rm = T),
    Reg_damages_bhm_frac = Reg_impact/sum(REF, na.rm = T),
    Reg_damages_ada_frac = Reg_damages_bhm_frac, # same as above
    gini_counter = reldist::gini(REF),
    gini_impact_bhm = reldist::gini(REF_impact),
    delta_gini_bhm = gini_impact_bhm - gini_counter,
    delta_gini_ada = delta_gini_bhm # same as above
  ) %>% 
  mutate(
    Scenario = "REF"
  ) %>% 
  select(Scenario, Model, Region, Decile, Year, everything(),
         -REF, -REF_impact, -Reg_impact)

## Impacts under 650
prova_650 <- mip_income_d  %>%
  filter(Model %in% models_with_impacts) %>% 
  filter(Scenario == "650" | Scenario == "650_impact") %>% 
  select(
    Scenario, Model, Region, Decile, Year, Decile_income
  ) %>% 
  pivot_wider(
    names_from = Scenario,
    values_from = Decile_income
  ) %>% 
  mutate(
    damages_dist_bhm = `650` - `650_impact`,
    damages_dist_ada = damages_dist_bhm, # these impacts don't depend on Spec.
    dam_dist_frac_bhm = (`650` - `650_impact`)/`650`,
    dam_dist_frac_ada = dam_dist_frac_bhm
  ) %>% 
  group_by(Model, Region, Year) %>% 
  mutate(
    Reg_impact = sum(`650`, na.rm = T) - sum(`650_impact`, na.rm = T),
    Reg_damages_bhm_frac = Reg_impact/sum(`650`, na.rm = T),
    Reg_damages_ada_frac = Reg_damages_bhm_frac, # same as above
    gini_counter = reldist::gini(`650`),
    gini_impact_bhm = reldist::gini(`650_impact`),
    delta_gini_bhm = gini_impact_bhm - gini_counter,
    delta_gini_ada = delta_gini_bhm # same as above
  )  %>% 
  mutate(
    Scenario = "650"
  )  %>% 
  select(Scenario, Model, Region, Decile, Year, everything(),
         -`650`, -`650_impact`, -Reg_impact)

# Re-append models with impacts from models to those with post-processed impacts
mip_income_d <- mip_income_d %>% 
  filter(Model %nin% models_with_impacts) %>% 
  select(names(prova_ref)) %>% 
  rbind(., prova_ref, prova_650)

rm(list = ls(pattern = "^prova_"))

# set colors for deciles in plots
colors_deciles <- c("purple", rev(colorspace::diverging_hcl(9, "Cyan-Magenta")))

#### GDP damages ####
reg_dam_plot = function(spec, dir) {
  
  theme_set(theme_bw(base_size = 12))
  
  if(spec == "BHM") {
    
    # country-level damages
    title <- expression(atop(bold("Damages on GDP, across countries"),
                             scriptstyle("Without adaptation")))
    
    p_reg_dam <- ggplot(mip_income_d %>% 
                          na.omit() %>% 
                          filter(Scenario == "REF" | Scenario == "650"),
                        aes(x = Year, y = Reg_damages_bhm_frac*100,
                            linetype = Scenario, color = Model)) +
      geom_line(linewidth = 1) +
      labs(y = "Damages, %",
           title = title) +
      xlim(c(2020, 2100)) +
      facet_wrap(~ Region, ncol = 5) +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "bottom",
            panel.spacing.x = unit(6, "mm"))
    
    # avoided country-level damages by going from REF to 650
    title_2 <- expression(atop(bold("Avoided damages from REF to 650, across countries"),
                             scriptstyle("Without adaptation")))
    
    p_reg_avoided_dam <- ggplot(mip_income_d %>% 
                          na.omit() %>% 
                          filter(Scenario == "REF" | Scenario == "650") %>% 
                            select(Scenario:Year, Reg_damages_bhm_frac) %>% 
                            distinct() %>% 
                            mutate(
                              Reg_damages_bhm_frac = Reg_damages_bhm_frac*100
                            ) %>% 
                            pivot_wider(
                              names_from = Scenario,
                              values_from = Reg_damages_bhm_frac
                            ) %>% 
                            mutate(
                              avoided_reg_dam_bhm = REF - `650`
                            ),
                        aes(x = Year, y = avoided_reg_dam_bhm,
                            color = Model)) +
      geom_line(linewidth = 1) +
      labs(y = "Avoided damages, %",
           title = title_2) +
      xlim(c(2020, 2100)) +
      facet_wrap(~ Region, ncol = 5) +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "bottom",
            panel.spacing.x = unit(6, "mm"))
    
    
  }
  
  if(spec == "Adaptation") {
    
    # Country-level damages
    title <- expression(atop(bold("Damages on GDP, across countries")))
  
    
    p_reg_dam <- ggplot(mip_income_d %>% 
                          na.omit() %>% 
                          filter(Scenario == "REF" | Scenario == "650"),
                        aes(x = Year, y = Reg_damages_ada_frac*100,
                            linetype = Scenario, color = Model)) +
      geom_line(linewidth = 1) +
      labs(y = "Damages, %",
           title = title) +
      xlim(c(2020, 2100)) +
      facet_wrap(~ Region, ncol = 5) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "bottom")
    
    # avoided country-level damages by going from REF to 650
    title_2 <- expression(atop(bold("Avoided damages from REF to 650, across countries")))
    
    p_reg_avoided_dam <- ggplot(mip_income_d %>% 
                                  na.omit() %>% 
                                  filter(Scenario == "REF" | Scenario == "650") %>% 
                                  select(Scenario:Year, Reg_damages_ada_frac) %>% 
                                  distinct() %>% 
                                  mutate(
                                    Reg_damages_ada_frac = Reg_damages_ada_frac*100
                                  ) %>% 
                                  pivot_wider(
                                    names_from = Scenario,
                                    values_from = Reg_damages_ada_frac
                                  ) %>% 
                                  mutate(
                                    avoided_reg_dam_ada = REF - `650`
                                  ),
                                aes(x = Year, y = avoided_reg_dam_ada,
                                    color = Model)) +
      geom_line(linewidth = 1) +
      labs(y = "Avoided damages, %",
           title = title_2) +
      xlim(c(2020, 2100)) +
      facet_wrap(~ Region, ncol = 5) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "bottom")
  }
  
  ggsave(filename = paste0(spec, "_region_damages_all.png"),
         plot = p_reg_dam,
         width = 15, height = 7, path = dir)

  ggsave(filename = paste0(spec, "_region_avoid_damages_all.png"),
         plot = p_reg_avoided_dam,
         width = 15, height = 7, path = dir)
  
  
  return(list(p_reg_dam, p_reg_avoided_dam))
  
}

reg_dam_plot(spec = "BHM", dir = graphdir)
reg_dam_plot(spec = "Adaptation", dir = graphdir)

#### GINI impacts ####
gini_plot = function(spec, dir) {
  
  theme_set(theme_bw(base_size = 12))
  
  if(spec == "BHM") {
    
    # Country-level impacts on Gini
    title <- expression(atop(bold("Impacts on Gini index, across countries"),
                             scriptstyle("Without adaptation")))
    
    p_gini <- ggplot(mip_income_d %>% 
                          na.omit() %>% 
                          filter(Scenario == "REF" | Scenario == "650"),
                        aes(x = Year, y = delta_gini_bhm*100,
                            linetype = Scenario, color = Model)) +
      geom_line(linewidth = 1) +
      labs(y = "\u0394 Gini",
           title = title) +
      xlim(c(2020, 2100)) +
      facet_wrap(~ Region, ncol = 5) +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "bottom",
            panel.spacing.x = unit(6, "mm"))
    
    # Avoided Gini impacts from REF to 650 scenario
    title_2 <- expression(atop(bold("Avoided impacts on Gini index from REF to 650, across countries"),
                               scriptstyle("Without adaptation")))
    
    p_avoided_gini <- ggplot(mip_income_d %>% 
                                  na.omit() %>% 
                                  filter(Scenario == "REF" | Scenario == "650") %>% 
                                  select(Scenario:Year, delta_gini_bhm) %>% 
                                  distinct() %>% 
                                  mutate(
                                    delta_gini_bhm = delta_gini_bhm*100
                                  ) %>% 
                                  pivot_wider(
                                    names_from = Scenario,
                                    values_from = delta_gini_bhm
                                  ) %>% 
                                  mutate(
                                    avoided_gini_bhm = REF - `650`
                                  ),
                                aes(x = Year, y = avoided_gini_bhm,
                                    color = Model)) +
      geom_line(linewidth = 1) +
      labs(y = "Avoided \u0394 Gini",
           title = title_2) +
      xlim(c(2020, 2100)) +
      facet_wrap(~ Region, ncol = 5) +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "bottom",
            panel.spacing.x = unit(6, "mm"))
    
  }
  
  if(spec == "Adaptation") {
    
    # Country-level impacts on Gini index
    title <- expression(atop(bold("Impacts on Gini index, across countries")))
    
    
    p_gini <- ggplot(mip_income_d %>% 
                       na.omit() %>% 
                       filter(Scenario == "REF" | Scenario == "650"),
                     aes(x = Year, y = delta_gini_ada*100,
                         linetype = Scenario, color = Model)) +
      geom_line(linewidth = 1) +
      labs(y = "\u0394 Gini",
           title = title) +
      xlim(c(2020, 2100)) +
      facet_wrap(~ Region, ncol = 5) +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "bottom",
            panel.spacing.x = unit(6, "mm"))
    
    # Avoided Gini impacts from REF to 650 scenario
    title_2 <- expression(atop(bold("Avoided impacts on Gini index from REF to 650, across countries")))
    
    p_avoided_gini <- ggplot(mip_income_d %>% 
                                  na.omit() %>% 
                                  filter(Scenario == "REF" | Scenario == "650") %>% 
                                  select(Scenario:Year, delta_gini_ada) %>% 
                                  distinct() %>% 
                                  mutate(
                                    delta_gini_ada = delta_gini_ada*100
                                  ) %>% 
                                  pivot_wider(
                                    names_from = Scenario,
                                    values_from = delta_gini_ada
                                  ) %>% 
                                  mutate(
                                    avoided_gini_ada = REF - `650`
                                  ),
                                aes(x = Year, y = avoided_gini_ada,
                                    color = Model)) +
      geom_line(linewidth = 1) +
      labs(y = "Avoided \u0394 Gini",
           title = title_2) +
      xlim(c(2020, 2100)) +
      facet_wrap(~ Region, ncol = 5) +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "bottom",
            panel.spacing.x = unit(6, "mm"))
  }
  
  ggsave(filename = paste0(spec, "_gini_impacts_all.png"),
         plot = p_gini,
         width = 15, height = 7, path = dir)
  
  ggsave(filename = paste0(spec, "_gini_avoided_impacts_all.png"),
         plot = p_avoided_gini,
         width = 15, height = 7, path = dir)
  
  return(list(p_gini, p_avoided_gini))
  
}

gini_plot(spec = "BHM", dir = graphdir)
gini_plot(spec = "Adaptation", dir = graphdir)

# produces regression table of Delta Gini on Delta regional temperature

gini_temp_reg = function(spec, dir) {

  require(stargazer)
  
  mip_final <- rbind(mip_50_final, mip_100_final)
  
  mip_income_reg <- mip_income_d %>% 
    select(Scenario, Model, Region, Year, delta_gini_bhm, delta_gini_ada) %>% 
    distinct() %>% 
    full_join(mip_final,
              by = c("Scenario", "Model", "Region", "Year")) %>% 
    rename(deltatemp = delta_temp)
    
  
if(spec == "BHM") {
  
  gini_temp_reg <- lm(delta_gini_bhm*100 ~ deltatemp +
                        Model -1, data = mip_income_reg)
}

if(spec == "Adaptation") {
  
  gini_temp_reg <- lm(delta_gini_ada*100 ~ deltatemp +
                        Model -1, data = mip_income_reg)
}

stargazer(gini_temp_reg,
          type = "latex",
          dep.var.labels = "Gini impact  [points]",
          model.names = FALSE,
          header=F,
          float=T,
          single.row = T,
          out = paste0(graphdir, "/", spec, "_gini_temp_reg",".tex")
)

graphdir = graphdir
hutils::replace_pattern_in("Model|Region","", file_pattern="*.tex", basedir = graphdir)
hutils::replace_pattern_in("deltatemp","Change in temperature", file_pattern="*.tex", basedir = graphdir)


}

gini_temp_reg(spec = "BHM")
gini_temp_reg(spec = "Adaptation")


#### Decile damages ####
decile_plot = function (spec, dir) {
  
  mod_letters <- c("A", "E", "G", "I", "N", "r", "R", "W")
  
  mod_letters_utf <- unlist(lapply(mod_letters, utf8ToInt))
  
  
  theme_set(theme_bw(base_size = 12))
  
  
  if(spec == "BHM") {
    
    # Decile-level impacts
    title <- expression(atop(bold("Damages at decile-level, across countries"),
                             scriptstyle("Without adaptation")))
  
   
  p_dec_dam <-  ggplot(mip_income_d %>% 
                         filter(Scenario == "REF" | Scenario == "650"
                         ) %>% 
                         mutate(Decile = factor(Decile,
                                                levels = c("D1", "D2", "D3", "D4", "D5",
                                                           "D6", "D7", "D8", "D9", "D10"))) ,
                       aes(x = Year, y = dam_dist_frac_bhm*100,
                           color = Decile, linetype = Scenario, shape = Model)) +
    geom_line(alpha = 0.5) +
    geom_point() +
    labs(x = "Year", y = "Damages, %", title = title) + ### checking whether common axis in ggarrange works
    scale_color_manual(name = "Income deciles", values = colors_deciles) +
    scale_shape_manual(name = "Models", values = mod_letters_utf) +
    facet_wrap(~ Region, ncol = 5) +
    scale_x_continuous(limits = c(2020, 2100), breaks = c(2020, 2060, 2100)) +
    theme(panel.spacing.x = unit(6, "mm"),
          legend.position="bottom",
          plot.title = element_text(hjust = 0.5))
  
  
  # Avoided decile-level impacts from REF to 650
  title_2 <- expression(atop(bold("Avoided damages at decile-level from REF to 650")),
  scriptstyle("Without adaptation"))
  
  p_dec_avoided_dam <-  ggplot(mip_income_d %>% 
    na.omit() %>% 
    filter(Scenario == "REF" | Scenario == "650") %>% 
    select(Scenario:Year, Decile, dam_dist_frac_bhm) %>% 
    distinct() %>% 
    mutate(
      dam_dist_frac_bhm = dam_dist_frac_bhm*100,
      Decile = factor(Decile,
                      levels = c("D1", "D2", "D3", "D4", "D5",
                                 "D6", "D7", "D8", "D9", "D10"))
    ) %>% 
    pivot_wider(
      names_from = Scenario,
      values_from = dam_dist_frac_bhm
    ) %>% 
    mutate(avoided_reg_dam_bhm = REF - `650`) ,
                       aes(x = Year, y = avoided_reg_dam_bhm,
                           color = Decile, shape = Model)) +
    geom_line(alpha = 0.5) +
    geom_point() +
    labs(x = "Year", y = "Avoided damages, %", title = title_2) +
    scale_color_manual(name = "Income deciles", values = colors_deciles) +
    scale_shape_manual(name = "Models", values = mod_letters_utf) +
    facet_wrap(~ Region, ncol = 5) +
    scale_x_continuous(limits = c(2020, 2100), breaks = c(2020, 2060, 2100)) +
    theme(panel.spacing.x = unit(6, "mm"),
          legend.position="bottom",
          plot.title = element_text(hjust = 0.5))
   }
  
  if(spec == "Adaptation") {
    
    # Decile-level impacts
    title <- expression(atop(bold("Damages at decile-level, across countries")))

    
    p_dec_dam <-  ggplot(mip_income_d %>% 
                           filter(Scenario == "REF" | Scenario == "650"
                           ) %>% 
                           mutate(Decile = factor(Decile,
                                                  levels = c("D1", "D2", "D3", "D4", "D5",
                                                             "D6", "D7", "D8", "D9", "D10"))) ,
                         aes(x = Year, y = dam_dist_frac_ada*100,
                             color = Decile, linetype = Scenario, shape = Model)) +
      geom_line(alpha = 0.5) +
      geom_point() +
      labs(x = "Year", y = "Damages, %", title = title) + ### checking whether common axis in ggarrange works
      scale_color_manual(name = "Income deciles", values = colors_deciles) +
      scale_shape_manual(name = "Models", values = mod_letters_utf) +
      facet_wrap(~ Region, ncol = 5) +
      scale_x_continuous(limits = c(2020, 2100), breaks = c(2020, 2060, 2100)) +
      theme(panel.spacing.x = unit(6, "mm"),
            legend.position="bottom",
            plot.title = element_text(hjust = 0.5))
    
    # Avoided decile-level impacts from REF to 650
    title_2 <- expression(atop(bold("Avoided damages at decile-level from REF to 650")))
    
    p_dec_avoided_dam <-  ggplot(mip_income_d %>% 
                                   na.omit() %>% 
                                   filter(Scenario == "REF" | Scenario == "650") %>% 
                                   select(Scenario:Year, Decile, dam_dist_frac_ada) %>% 
                                   distinct() %>% 
                                   mutate(
                                     dam_dist_frac_ada = dam_dist_frac_ada*100,
                                     Decile = factor(Decile,
                                                     levels = c("D1", "D2", "D3", "D4", "D5",
                                                                "D6", "D7", "D8", "D9", "D10"))
                                   ) %>% 
                                   pivot_wider(
                                     names_from = Scenario,
                                     values_from = dam_dist_frac_ada
                                   ) %>% 
                                   mutate(avoided_reg_dam_ada = REF - `650`) ,
                                 aes(x = Year, y = avoided_reg_dam_ada,
                                     color = Decile, shape = Model)) +
      geom_line(alpha = 0.5) +
      geom_point() +
      labs(x = "Year", y = "Avoided damages, %", title = title_2) +
      scale_color_manual(name = "Income deciles", values = colors_deciles) +
      scale_shape_manual(name = "Models", values = mod_letters_utf) +
      facet_wrap(~ Region, ncol = 5) +
      scale_x_continuous(limits = c(2020, 2100), breaks = c(2020, 2060, 2100)) +
      theme(panel.spacing.x = unit(6, "mm"),
            legend.position="bottom",
            plot.title = element_text(hjust = 0.5))
  }
  
  ggsave(filename = paste0(spec, "_decile_damages.png"),
         plot = p_dec_dam,
         width = 15, height = 7, path = dir)
  
  ggsave(filename = paste0(spec, "_decile_avoided_damages.png"),
         plot = p_dec_avoided_dam,
         width = 15, height = 7, path = dir)
  
  
  return(list(p_dec_dam, p_dec_avoided_dam))
  
}

decile_plot(spec = "BHM", dir = graphdir)
decile_plot(spec = "Adaptation", dir = graphdir)

decile_plot_sel_years = function(spec, dir) {
  
  require(ggpattern)
  
  if(spec == "BHM") {
    
    # Decile-level impacts in 2100, both Scenarios
    p_dec_sel_100 <-  ggplot(mip_income_d %>% 
                               na.omit() %>% 
                               filter(Scenario == "REF" | Scenario == "650"
                               ) %>% 
                               filter(Year == 2100) %>%
                               mutate(Decile = factor(Decile,
                                                      levels = c("D1", "D2", "D3", "D4", "D5",
                                                                 "D6", "D7", "D8", "D9", "D10"))) ,
                             aes(x = Model, y = dam_dist_frac_bhm*100)) +
      geom_bar_pattern(aes(pattern = Scenario,
                           group = Decile, fill = Decile),
                       stat = "identity", position = position_dodge(0.9), width = 2) +
      labs(x = "Model", y = "Damages, %",
           title = "Damages at decile level in 2100",
           subtitle = "Without adaptation") + 
      scale_fill_manual(name = "Income deciles", values = colors_deciles) +
      scale_pattern_manual(name = "Scenario", values = c("plasma", "stripe")) +
      facet_wrap(~ Region, ncol = 5) +
      scale_x_discrete(guide = guide_axis(n.dodge=2)) +
      theme_bw() +
      theme(panel.spacing.x = unit(6, "mm"),
            legend.position="bottom",
            plot.title = element_text(hjust = 0.5))
    
    
    # Avoided decile-level impacts in 2100, from 650 relative to REF
    p_dec_avoided_100 <- ggplot(mip_income_d %>% 
                                  na.omit() %>% 
                                  filter(Scenario == "REF" | Scenario == "650") %>% 
                                  filter(Year == 2100) %>%
                                  select(Scenario:Year, Decile, dam_dist_frac_bhm) %>% 
                                  distinct() %>% 
                                  mutate(
                                    dam_dist_frac_bhm = dam_dist_frac_bhm*100,
                                    Decile = factor(Decile,
                                                    levels = c("D1", "D2", "D3", "D4", "D5",
                                                               "D6", "D7", "D8", "D9", "D10"))
                                  ) %>% 
                                  pivot_wider(
                                    names_from = Scenario,
                                    values_from = dam_dist_frac_bhm
                                  ) %>% 
                                  mutate(avoided_reg_dam_bhm = REF - `650`),
                                aes(x = Model, y = avoided_reg_dam_bhm)) +
      geom_bar(aes(group = Decile, fill = Decile),
               stat = "identity", position = position_dodge(0.9), width = 2) +
      labs(x = "Model", y = "Avoided damages, %",
           title = "Avoided damages at decile level in 2100",
           subtitle = "Without adaptation") + 
      scale_fill_manual(name = "Income deciles", values = colors_deciles) +
      facet_wrap(~ Region, ncol = 5) +
      scale_x_discrete(guide = guide_axis(n.dodge=2)) +
      theme_bw() +
      theme(panel.spacing.x = unit(6, "mm"),
            legend.position="bottom",
            plot.title = element_text(hjust = 0.5))
    
  }
  
  if(spec == "Adaptation") {
    
    p_dec_sel_100 <-  ggplot(mip_income_d %>% 
                               na.omit() %>% 
                               filter(Scenario == "REF" | Scenario == "650"
                               ) %>% 
                               filter(Year == 2100) %>%
                               mutate(Decile = factor(Decile,
                                                      levels = c("D1", "D2", "D3", "D4", "D5",
                                                                 "D6", "D7", "D8", "D9", "D10"))) ,
                             aes(x = Model, y = dam_dist_frac_ada*100)) +
      geom_bar_pattern(aes(pattern = Scenario,
                           group = Decile, fill = Decile),
                       stat = "identity", position = position_dodge(0.9), width = 2) +
      labs(x = "Model", y = "Damages, %",
           title = "Damages at decile level in 2100") + 
      scale_fill_manual(name = "Income deciles", values = colors_deciles) +
      scale_pattern_manual(name = "Scenario", values = c("plasma", "stripe")) +
      facet_wrap(~ Region, ncol = 5) +
      scale_x_discrete(guide = guide_axis(n.dodge=2)) +
      theme_bw() +
      theme(panel.spacing.x = unit(6, "mm"),
            legend.position="bottom",
            plot.title = element_text(hjust = 0.5))
    
    # Avoided decile-level impacts in 2100, from 650 relative to REF
    p_dec_avoided_100 <- ggplot(mip_income_d %>% 
                                  na.omit() %>% 
                                  filter(Scenario == "REF" | Scenario == "650") %>% 
                                  filter(Year == 2100) %>%
                                  select(Scenario:Year, Decile, dam_dist_frac_ada) %>% 
                                  distinct() %>% 
                                  mutate(
                                    dam_dist_frac_ada = dam_dist_frac_ada*100,
                                    Decile = factor(Decile,
                                                    levels = c("D1", "D2", "D3", "D4", "D5",
                                                               "D6", "D7", "D8", "D9", "D10"))
                                  ) %>% 
                                  pivot_wider(
                                    names_from = Scenario,
                                    values_from = dam_dist_frac_ada
                                  ) %>% 
                                  mutate(avoided_reg_dam_ada = REF - `650`),
                                aes(x = Model, y = avoided_reg_dam_ada)) +
      geom_bar(aes(group = Decile, fill = Decile),
               stat = "identity", position = position_dodge(0.9), width = 2) +
      labs(x = "Model", y = "Avoided damages, %",
           title = "Avoided damages at decile level in 2100") + 
      scale_fill_manual(name = "Income deciles", values = colors_deciles) +
      facet_wrap(~ Region, ncol = 5) +
      scale_x_discrete(guide = guide_axis(n.dodge=2)) +
      theme_bw() +
      theme(panel.spacing.x = unit(6, "mm"),
            legend.position="bottom",
            plot.title = element_text(hjust = 0.5))
    
    
  }
  
  ggsave(filename = paste0(spec, "_decile_damages_2100.png"),
         plot = p_dec_sel_100,
         width = 15, height = 7, path = dir)
  
  ggsave(filename = paste0(spec, "_decile_avoided_damages_2100.png"),
         plot = p_dec_avoided_100,
         width = 15, height = 7, path = dir)
  
  return(list(p_dec_sel_100, p_dec_avoided_100))
  
}

decile_plot_sel_years(spec = "BHM", dir = graphdir)
decile_plot_sel_years(spec = "Adaptation", dir = graphdir)
