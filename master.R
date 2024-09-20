#Consumption or Income?
measure_inequality <- "Consumption"


library(tidyverse)
library(data.table)
library(readxl)
library(writexl)
library(gtools)
library(zoo)
library(reldist)
library(here)
library(stargazer)
library(arrow)
library(ggrepel)
library(ggbrace)
library(countrycode)
library(ggpubr)
library(ggpattern)
library(sf)
library(rnaturalearth)
library(ggh4x)


graphdir <- paste0("graphs_", measure_inequality)
#use the function saveplot to save the graphs in the relative folders 
figure_format <- "pdf"
convert_pdftopng <- T #converts all created pdfs to png for better quality (needs pdftopng.exe in your PATH. Download from http://www.xpdfreader.com/download.html)
saveplot <- function(plotname, text_size=18, width=12, height=8, plot_title = T, plot_theme=theme_bw()){
  if(!dir.exists(file.path(graphdir))){dir.create(file.path(graphdir))}
  print(last_plot() + plot_theme)
  ggsave(filename=file.path(graphdir, paste0(as.character(gsub(" ", "_", plotname)),".", figure_format)), plot = last_plot() + theme(text = element_text(size=text_size)), width=width, height=height)
  if(figure_format=="pdf" & convert_pdftopng) shell(str_glue('pdftopng.exe {file.path(graphdir, paste0(as.character(gsub(" ", "_", plotname)),".", figure_format))} - > {file.path(graphdir, paste0(as.character(gsub(" ", "_", plotname)),".", "png"))}'))
}

upload2iiasa <- function(filename){
  manual_xlsx <- readxl::read_excel(file.path("model_results", filename))
  manual_xlsx <- manual_xlsx %>%
    pivot_longer(cols = starts_with("2"), names_to = "Year") %>% mutate(Year=as.integer(Year)) %>% 
    filter(!is.na(value))
    return(manual_xlsx)
}

##################### END OF FUNCTIONS ################################



#manually convert upload files to iiasadb format

#REMIND
iiasadb_data <- upload2iiasa("REMIND_WP4_ICMP_Nov2023.xlsx")
#manually combine Energy plus Industrial Processes
iiasadb_data <- rbind(iiasadb_data, upload2iiasa("REMIND_WP4_ICMP_Nov2023.xlsx") %>% filter(Variable=="Emissions|CO2|Energy"|Variable=="Emissions|CO2|Industrial Processes") %>% mutate(Variable="Emissions|CO2|Energy and Industrial Processes") %>% group_by(Scenario,Variable,Model,Region,Unit,Year) %>% summarise(value=sum(value)))
#WITCH
iiasadb_data <- rbind(iiasadb_data, upload2iiasa("WITCH-WP4-v6.xlsx") %>% mutate(Region=tolower(Region), Region=gsub("india", "India", Region)))
#RICE
iiasadb_data <- rbind(iiasadb_data, upload2iiasa("RICE50x_v2.xlsx"))
#NICE
iiasadb_data <- rbind(iiasadb_data, rbind(upload2iiasa("NICE_WP4_sent_191222.xlsx"), upload2iiasa("NICE_WP4_sent_191222.xlsx") %>% filter(Variable=="Emissions|CO2|Energy and Industrial Processes" & Region!="WORLD") %>% mutate(Variable="Emissions|CO2")) )
#IMACLIM
#iiasadb_data <- rbind(iiasadb_data, upload2iiasa("WP4_IMACLIM_sent_080523.xlsx"))
iiasadb_data <- rbind(iiasadb_data, upload2iiasa("WP4_IMACLIM_sent_041023.xlsx"))

#Ad E3ME from v2 protocol
iiasadb_data <- rbind(iiasadb_data, rbind(upload2iiasa("E3ME-FTT data for NAVIGATE Task 4.2_V5.xlsx"), upload2iiasa("E3ME-FTT data for NAVIGATE Task 4.2_V5.xlsx") %>% filter(Variable=="Emissions|CO2|Energy and Industrial Processes") %>% mutate(Variable="Emissions|CO2")) %>% mutate(Variable=gsub("MER", "PPP", Variable))) # use MER as PPP

#GEM-E3
iiasadb_data <- rbind(iiasadb_data, upload2iiasa("Results_WP4_GEM-E3_cleaned.xlsx") %>% filter(value!="n/a") %>% mutate(value=as.numeric(value)) %>% mutate(Variable=gsub("MER", "PPP", Variable))) # use MER as PPP

#AIM:
iiasadb_data <- rbind(iiasadb_data, upload2iiasa("NAVIGATE_template_inequality_variables_v4_AIM_20231228.xlsx") %>% mutate(Scenario=gsub("WP4_", "", Scenario), Variable=gsub(" Decile", "", Variable)))
iiasadb_data <- iiasadb_data %>% mutate(value = as.numeric(value)) %>% mutate(Variable=gsub("Expenditure Decile", "Consumption", Variable))


#For REMIND and AIM based on a constant savings rate use same deciles as consumption for income
#ISSUE: E3ME has only income, so take consumption as now also as income!
iiasadb_data <- rbind(iiasadb_data, iiasadb_data %>% filter(str_detect(Variable, "Consumption\\|") & Model %in% c("REMIND 3.0", "AIM")) %>% mutate(Variable=gsub("Consumption", "Income", Variable)), iiasadb_data %>% filter(str_detect(Variable, "Income\\|") & Model %in% c("E3ME-FTT")) %>% mutate(Variable=gsub("Income", "Consumption", Variable)))

#change 650 to Paris in Scenarios
iiasadb_data <- iiasadb_data %>% mutate(Scenario=gsub("650", "Paris", Scenario))


####### Clean Data ##########

#convert income quintiles to deciles
iiasadb_data <- rbind(iiasadb_data %>% filter(!str_detect(Variable, "\\|Q[0-9]")), rbind(iiasadb_data %>% filter(str_detect(Variable, "\\|Q[0-9]")) %>% mutate(value=value/2, Variable=paste0(substr(Variable, 1, nchar(Variable)-2), "D", as.numeric(substr(Variable, nchar(Variable),nchar(Variable)))*2-1)), iiasadb_data %>% filter(str_detect(Variable, "\\|Q[0-9]")) %>% mutate(Variable=paste0(Variable,".5")) %>% mutate(value=value/2, Variable=paste0(substr(Variable, 1, nchar(Variable)-4), "D", as.numeric(substr(Variable, nchar(Variable)-2,nchar(Variable)))*2-1)))
)

#List of variables and units
#print(unique(iiasadb_data$Unit))
#drop units to avoid errors for now
iiasadb_data$Unit <- NULL

#some report 45,55 years other 40,50: so complete and interpolate time:
iiasadb_data <- iiasadb_data %>% filter(Year!=2005)
iiasadb_data <- rbind(iiasadb_data %>% filter(str_detect(Model, "E3ME")) %>% group_by(Scenario, Variable, Model, Region) %>% complete(Year=seq(2010,2050,5)) %>% mutate(value=approxfun(Year, value)(Year)), iiasadb_data %>% filter(!str_detect(Model, "E3ME")) %>% group_by(Scenario, Variable, Model, Region) %>% complete(Year=seq(2010,2100,5)) %>% mutate(value=approxfun(Year, value)(Year)))

#three models report quantiles in % (0-1), convert to 0-100
iiasadb_data <- iiasadb_data %>% mutate(value=ifelse((str_detect(Variable, "Consumption\\|D") | str_detect(Variable, "Income\\|D")) & str_detect(Model, "GEM|REMIND|E3ME"), value*100, value))

#NEGATIVE OR UNRELAISTICALLY LOW DECILES SET TO 0.1
iiasadb_data <- iiasadb_data %>% mutate(value=ifelse((str_detect(Variable, "Consumption\\|D") | str_detect(Variable, "Income\\|D")) & value < 0.1, 0.1, value))



######### Nice names and orders and variables ##########

#nice model and scenario names
iiasadb_data <- iiasadb_data %>%
  mutate(Scenario=gsub("WP4_", "", Scenario), Scenario=gsub("distON_", "", Scenario)) %>%
mutate(Model=str_split(Model, " |-", simplify = T)[,1]) %>%
  mutate(Model=gsub("GEM", "GEM-E3", Model), Model=gsub("REMIND", "ReMIND", Model))

#order scenarios
print(cat(paste0('"', unique(iiasadb_data$Scenario), '"'), sep = ", "))
iiasadb_data <- iiasadb_data %>%
  mutate(Scenario=as.factor(Scenario), Scenario=fct_relevel(Scenario, c("REF", "1150", "1150_redist", "Paris", "Paris_redist", "REF_impact", "1150_impact", "1150_impact_redist", "Paris_impact", "Paris_impact_redist"))) 



#Model Scenario Matrix
ggplot(iiasadb_data %>% group_by(Model, Scenario) %>% summarize(regions=length(unique(Region)), variables=length(unique(Variable))), aes(Model,Scenario, fill = variables*regions)) + geom_tile() + geom_text(aes(label=str_glue("Var:{variables}, N:{regions}"))) + theme_minimal() + scale_fill_gradient2(low = "white", mid = "yellow", high = "darkgreen") + xlab("") + ylab("") + ggtitle(str_glue("Scenario submissions ({nrow(iiasadb_data %>% ungroup() %>% group_by(Model, Scenario) %>% summarize(n=1))} as of {format(Sys.time(), '%d %b %Y')})")) + guides(fill="none")
saveplot("Scenario matrix")



saveRDS(iiasadb_data, file = "inequality_mip_full.Rdata")
mip_data <- iiasadb_data
source("mip_script.R")   #>> creates mip_data






###########################################################################
#import impacts postprocessed!
models_with_impacts <- c("NICE", "ReMIND", "RICE50+")
load("prova_mip.Rdata")
iiasadb_impacts_postprocessed <- mip_data %>% filter(!(Model %in% models_with_impacts)) %>% filter(str_detect(Variable, "with_impact_ada")) %>% mutate(Variable=gsub("Dec_with_impact_ada", "Consumption", Variable), Variable=gsub("GDP\\|PPP_with_impact_ada", "GDP|PPP", Variable)) %>% mutate(Scenario = paste0(Scenario, "_impact")) %>% mutate(Scenario=gsub("redist_impact", "impact_redist", Scenario))
iiasadb_data <- rbind(iiasadb_data, iiasadb_impacts_postprocessed)
##########################################################################



#Global Variables before dealing with regions
global_sums <- iiasadb_data %>% filter(Variable == "Emissions|CO2|Energy and Industrial Processes") %>% filter(!str_detect(Region, "orld") & !str_detect(Region, "r5")) %>% group_by(Scenario, Variable, Model, Year) %>% summarize(value=sum(value, na.rm = T)) %>% mutate(Region="World")


#################################### REGIONS #############################
#show all regions per model
iiasadb_data %>% group_by(Model) %>% summarize(regions=unique(Region)) %>% mutate(value=1) %>% pivot_wider(id_cols = regions, names_from = Model) %>% replace(is.na(.), 0) %>% as.data.frame()
#without native regions
iiasadb_data %>% group_by(Model) %>% summarize(regions=unique(Region)) %>% mutate(value=1) %>% pivot_wider(id_cols = regions, names_from = Model) %>% replace(is.na(.), 0) %>% as.data.frame() %>% filter(!str_detect(regions, "\\|"))

ggplot(iiasadb_data %>% group_by(Model, Scenario) %>% filter(!str_detect(Region, "\\|")) %>% summarize(Region=unique(Region)) %>% ungroup() %>% group_by(Model, Region) %>% summarize(Scenarios=length(Scenario)), aes(Region, Model, fill=Scenarios)) + geom_tile() + theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + geom_text(aes(label=Scenarios))  + scale_fill_gradient2(low = "white", mid = "yellow", high = "darkgreen") + scale_x_discrete(labels = function(x) str_wrap(x, width = 50))
#saveplot("Regions")





######### Now clean regions and only use common big countries #############
countries_reported <- c("Japan", "India", "United States", "China")
countries_reported_max <- c("France", "India", "Brazil", "Mexico", "United States", "Canada", "China", "Russia", "Japan", "South Africa")

countries_reported <- countries_reported_max

country_naming <- fread("data/country_naming.csv")
iiasadb_data$Region <- stringi::stri_replace_all_regex(iiasadb_data$Region, pattern=country_naming$original, replacement=country_naming$new, vectorize=FALSE)

iiasadb_data <- iiasadb_data %>% filter(Region %in% c("France", "India", "Brazil", "Mexico", "United States", "Canada", "China", "Russia", "Japan", "South Africa"))

ggplot(read_rds("inequality_mip_full.Rdata") %>% filter(Variable=="Consumption|D1") %>% group_by(Model, Scenario) %>% filter(!str_detect(Region, "\\|")) %>% summarize(Region=unique(Region)) %>% ungroup() %>% group_by(Model, Region) %>% summarize(Scenarios=length(Scenario)) %>% mutate(Region=stringi::stri_replace_all_regex(Region, pattern=country_naming$original, replacement=country_naming$new, vectorize=FALSE)) %>% filter(Region %in% countries_reported_max) %>% mutate(Region = factor(Region, levels = c("United States", "Canada", "France", "Japan", "Russia", "Mexico", "China", "Brazil", "South Africa", "India"))), aes(Region, Model, fill=Scenarios)) + geom_tile() + theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + geom_text(aes(label=Scenarios))  + scale_fill_gradient2(low = "white", mid = "yellow", high = "darkgreen") + scale_x_discrete(labels = function(x) str_wrap(x, width = 50))
saveplot("Regions cleaned")


#order dataframe by country by GDP per capita
iiasadb_data <- iiasadb_data %>% mutate(Region = factor(Region, levels = c("United States", "Canada", "France", "Japan", "Russia", "Mexico", "China", "Brazil", "South Africa", "India")))

################################# VARIABLES ###########################################

#we don't use consumption SHARE variables
iiasadb_data <- iiasadb_data %>% filter(!str_detect(Variable, "[s|S]hare"))

print(iiasadb_data %>% group_by(Model) %>% summarize(regions=unique(Variable)) %>% mutate(value=1) %>% pivot_wider(id_cols = regions, names_from = Model) %>% replace(is.na(.), 0) %>% as.data.frame())

#ggplot(iiasadb_data %>% group_by(Model, Scenario, Region) %>% filter(!str_detect(Region, "\\|")) %>% summarize(Variable=unique(Variable)) %>% ungroup() %>% group_by(Variable, Model) %>% summarize(RegScen=length(Variable)), aes(Variable, Model, fill=RegScen)) + geom_tile() + theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + geom_text(aes(label=RegScen))  + scale_fill_gradient2(low = "white", mid = "yellow", high = "darkgreen") + scale_x_discrete(labels = function(x) str_wrap(x, width = 20))
#saveplot("Variables")





################################# PLOTS ###############################################
iamc_lineplot <- function(reg="World", var="Emissions|CO2", cumulative=F){
  .data <- iiasadb_data %>% filter(Region %in% reg) %>% filter(Variable==var) 
  if(cumulative) .data <- .data %>% group_by(Model, Region, Scenario, Variable) %>% arrange(Year) %>% mutate(value_cum=value*ifelse(is.na(lead(Year)), Year-lag(Year), lead(Year)-Year), value_cum=cumsum(value_cum)) %>% mutate(value=value_cum * 1e-3)
  .data %>% ggplot() + geom_line(aes(Year, value, color=Model, linetype=Scenario)) + ylab(var) + xlab("") + ggtitle(str_glue("{var}")) + facet_wrap(Region ~ ., scales = "free_y", nrow = 2) + theme(legend.position = "bottom")
  saveplot(gsub("\\|","-", str_glue("{var}")))
}
iamc_lineplot(reg=countries_reported, var="Emissions|CO2")



#Quantile plot
iiasadb_data %>% filter(str_detect(Variable, str_glue("{measure_inequality}\\|D"))) %>% filter(Year==2050 & Scenario %in% c("REF")) %>% mutate(dist=as.numeric(gsub(str_glue("{measure_inequality}\\|D"), "", Variable)))  %>% filter(!str_detect(Region, "\\|")) %>% ggplot() + geom_line(aes(dist, value, color=Model, linetype=Scenario), size=1) + xlab("Decile") + facet_wrap(Region ~ ., scales = "free_x", nrow = 2) + scale_x_continuous(breaks=seq(1,10)) + theme(legend.position = "bottom") + ylab("Share of total consumption for each decile [%]")
saveplot("Decile Plots for 2050", width=8, height = 5)





#Incicende from two scenarios
iamc_incidence_curve <- function(scen0 = "REF", scen1 = "Paris", year = 2050, aggregate_use=measure_inequality, Regions="all"){
  iiasadb_incidence <- iiasadb_data %>% filter(str_detect(Variable, paste0(aggregate_use, "\\|D"))) %>% filter(Year==year) %>% mutate(dist=as.numeric(gsub(paste0(aggregate_use, "\\|D"), "", Variable)))  %>% filter(!str_detect(Region, "\\|")) %>% filter(Scenario %in% c(scen0, scen1)) %>% pivot_wider(id_cols = c(Model, Region, dist, Year), names_from = Scenario) %>% mutate(Incidence_decile=get(scen1)/get(scen0)-1)
  if(Regions[1]!="all") iiasadb_incidence <- iiasadb_incidence %>% filter(Region %in% Regions)
  # quantiles need to be combined with GDP|PPP to get the total imapct of inequality and macro GDP level
  iiasadb_incidence_gdp <- iiasadb_data %>% filter(str_detect(Variable, "GDP\\|PPP")) %>% filter(Year==year) %>% filter(!str_detect(Region, "\\|")) %>% filter(Scenario %in% c(scen0, scen1)) %>% mutate(Scenario=paste0("GDP_", Scenario)) %>% pivot_wider(id_cols = c(Model, Region, Year), names_from = Scenario) %>% mutate(Incidence_macro=get(paste0("GDP_",scen1))/get(paste0("GDP_",scen0))-1)
  iiasadb_incidence <- iiasadb_incidence %>% left_join(iiasadb_incidence_gdp) %>% mutate(Incidence_total=(get(scen1)*get(paste0("GDP_",scen1)))/(get(scen0)*get(paste0("GDP_",scen0)))-1)
   ggplot(iiasadb_incidence) + geom_line(aes(dist, Incidence_total, color=Model, linetype="Decile"), size=1) + xlab("Decile") + ylab(paste0(aggregate_use, " change [%]")) + labs(linetype=NULL) + facet_wrap(Region ~ ., scales = "free_y", nrow = 2) + scale_y_continuous(labels = scales::percent) + scale_x_continuous(breaks=seq(1,10)) + geom_hline(yintercept = 0) + ggtitle(str_glue("Incidence from {scen0} to {scen1} in {year}")) + geom_line(aes(dist, Incidence_macro, color=Model, linetype="Average"), size=1) + theme(legend.position = "bottom") # + geom_line(aes(dist, Incidence_decile, color=Model, linetype="Decile"), size=1)
   saveplot(str_glue("Incidence from {scen0} to {scen1} in {year}"))
    }
iamc_incidence_curve(scen0 = "Paris", scen1 = "Paris_redist", year = 2050)
#Incidence curve for Deliverable
#iamc_incidence_curve(scen0 = "REF", scen1 = "Paris", year = 2050, aggregate_use = measure_inequality, Regions = countries_reported)



# Gini index
#iamc_lineplot(reg=countries_reported, var="Inequality index|Gini")
#recompute for missing models
gini_recomputed <- iiasadb_data %>% group_by(Model, Region, Scenario, Year) %>% filter(str_detect(Variable, str_glue("{measure_inequality}\\|D"))) %>% summarize(value=reldist::gini(value)) %>% mutate(Variable="Gini_recomputed")

use_original_gini <- F
#(A) Add variable with reported or recomputed "Gini_full"
if(use_original_gini){
  all_ginis <- rbind(iiasadb_data %>% filter(Variable=="Inequality index|Gini"), gini_recomputed) %>% pivot_wider(id_cols = c(Model, Region, Scenario, Year), names_from = Variable) %>% mutate(Gini_full=ifelse(is.na(`Inequality index|Gini`), Gini_recomputed, `Inequality index|Gini`)) %>% pivot_longer(cols = c(`Inequality index|Gini`, Gini_recomputed, Gini_full), names_to = "Variable") 
  iiasadb_data <- rbind(iiasadb_data %>% filter(!str_detect(Variable, "Gini")), all_ginis)
  iamc_lineplot(reg=countries_reported_max, var="Gini_recomputed")
}else{
#(B) always recompute
  iiasadb_data <- rbind(iiasadb_data %>% filter(!str_detect(Variable, "Gini")), gini_recomputed %>% mutate(Variable="Gini_full"))
}
#add decile ratios
iiasadb_data <- rbind(iiasadb_data, 
  iiasadb_data %>% group_by(Model, Region, Scenario, Year) %>% filter(str_detect(Variable, str_glue("{measure_inequality}\\|D"))) %>% summarize(value=mean(value[Variable==str_glue("{measure_inequality}|D10")])/mean(value[Variable==str_glue("{measure_inequality}|D1")])) %>% mutate(Variable="D10D1"),
  iiasadb_data %>% group_by(Model, Region, Scenario, Year) %>% filter(str_detect(Variable, str_glue("{measure_inequality}\\|D"))) %>% summarize(value=(mean(value[Variable==str_glue("{measure_inequality}|D10")])+mean(value[Variable==str_glue("{measure_inequality}|D9")]))/(mean(value[Variable==str_glue("{measure_inequality}|D1")])+mean(value[Variable==str_glue("{measure_inequality}|D2")]))) %>% mutate(Variable="D8D2"),
  iiasadb_data %>% group_by(Model, Region, Scenario, Year) %>% filter(str_detect(Variable, str_glue("{measure_inequality}\\|D"))) %>% summarize(value=1 - prod(value)^(1/length(value))/mean(value)) %>% mutate(Variable="Atkinson_1")
)


#Sen-Welfare effect decomposition Figure
#data_welfare_effect <- iiasadb_data %>% filter(Variable=="GDP|PPP" | Variable=="Gini_full") %>% mutate(value=ifelse(Variable=="Gini_full", 1-value, value), Variable = gsub("Gini_full", "Equality_index", Variable))
#now keep also Atkinson and decile rations (remember, Equality_index = 1-Gini)
data_welfare_effect <- iiasadb_data %>% filter(Variable=="GDP|PPP" | Variable=="Gini_full" | Variable=="D10D1" | Variable=="D8D2" | Variable=="Atkinson_1") %>% mutate(value=ifelse(Variable=="Gini_full", 1-value, value), Variable = gsub("Gini_full", "Equality_index", Variable))

#make sure to remove country model combinations without data
data_welfare_effect <- data_welfare_effect %>% filter(!is.na(value)) %>% filter(!(Model=="WITCH" & Region=="Canada") & !(Model=="GEM-E3" & Region=="Japan") & !(Model=="E3ME" & Region=="China") & !(Model=="E3ME" & Region=="Japan"))
#compute scenario pair wise values and differens (x= from y = to scenario)
data_welfare_effect <- transform(merge(data_welfare_effect, data_welfare_effect, by = c('Model', 'Region', 'Year')), relchange = value.y/value.x-1)
data_welfare_effect <- data_welfare_effect %>% filter(Variable.x==Variable.y) %>% dplyr::rename(Variable=Variable.x) %>% select(-Variable.y)
#As wide data frame for plotting
data_welfare_effect <- data_welfare_effect %>% group_by(Model, Region, Year) %>% pivot_wider(id_cols = c(Model, Region, Year, Scenario.x, Scenario.y), names_from = Variable, values_from = c(relchange, value.x, value.y))
#set NAs to zero as zero change for non uploaded scenarios
data_welfare_effect[is.na(data_welfare_effect)] <- 0


#fpr revision reordering, keep always REF as reference, hence don't do any changes
data_welfare_effect_revision <- data_welfare_effect %>% mutate(Scenario.x=as.character(Scenario.x), Scenario.y=as.character(Scenario.y))
#for impact scenarios: use avoided impacts (different from wb2c to impact etc.
data_welfare_effect <- data_welfare_effect %>% mutate(Scenario.x=as.character(Scenario.x), Scenario.y=as.character(Scenario.y)) %>% mutate(Scenario.x=ifelse(str_detect(Scenario.y, "impact") & Scenario.x=="REF", "REF_original", ifelse(str_detect(Scenario.y, "impact") & Scenario.x=="REF_impact", "REF", Scenario.x)))




sdn <- function(x) ifelse(length(x)==1, 0, sd(x))
data_welfare_effect_mod_mean <- data_welfare_effect  %>% group_by(Region, Year, Scenario.x, Scenario.y) %>% summarize(`MEAN_relchange_GDP|PPP`= mean(`relchange_GDP|PPP`, na.rm = T), MEAN_relchange_Equality_index = mean(relchange_Equality_index), MEAN_abschange_Gini=mean(-(`value.y_Equality_index`-`value.x_Equality_index`)), `SD_relchange_GDP|PPP`=sdn(`relchange_GDP|PPP`), SD_relchange_Equality_index = sdn(relchange_Equality_index), SD_abschange_Gini=sdn(-(`value.y_Equality_index`-`value.x_Equality_index`)))




















swfrange = .1; xscale=1;  
#changes across two scenarios
scen_switches <- list(one=c("REF", "Paris"), two=c("REF", "Paris_redist"), three=c("REF_impact", "Paris_impact_redist"), four=c("REF_impact", "Paris_impact"))
if(FALSE){
for(comp in scen_switches) {
  ggplot(data_welfare_effect %>% filter(Year %in% c(2030, 2050, 2100) & Region %in% countries_reported) %>% filter(Scenario.x==comp[1] & Scenario.y==comp[2])) + geom_point(aes(x = relchange_Equality_index, y = `relchange_GDP|PPP`, color=Model, shape=as.character(Year)), size=3) + scale_x_continuous(labels=scales::percent, limits = xscale*c(-swfrange, +swfrange)) + scale_y_continuous(labels=scales::percent, limits = c(-swfrange, +swfrange)) + theme_minimal() + labs(x="Equality change", y="GDP change", title="Welfare impact", caption=str_glue("{comp[1]} to {comp[2]}")) + geom_segment(aes(x = -swfrange, y = 0, xend = +swfrange, yend = 0),arrow = arrow(length = unit(0.2, "cm"))) + geom_segment(aes(y = -swfrange, x = 0, yend = +swfrange, xend = 0),arrow = arrow(length = unit(0.2, "cm")))
  saveplot(str_glue("Welfare change {comp[1]} to {comp[2]}"))
}
}


data_welfare_effect <- data_welfare_effect %>% mutate(Models=ifelse(Model=="RICE50+", "+", substr(Model,1,1)))
#changes from REF for 3 years all models and all Paris scenarios
swfrange = 0.10

#Inequality change across scenarios, model ranges from left to right
data_welfare_effect_reordered <- data_welfare_effect %>% mutate(Scenario.y=as.factor(Scenario.y), Scenario.y=fct_relevel(Scenario.y, c("REF", "1150", "Paris", "1150_redist", "Paris_redist", "REF_impact", "1150_impact", "1150_impact_redist", "Paris_impact", "Paris_impact_redist"))) 
data_welfare_effect_reordered <- data_welfare_effect_reordered %>% mutate(scenarioclass=case_when(str_detect(Scenario.y, "impact") ~ "EPC with avoided Impacts", str_detect(Scenario.y, "redist$") ~ "EPC redistribution", TRUE ~ "Climate Policy"))




#Emissions, carbon price, and transfers for EPC anaysis
transfer_data <- iiasadb_data %>% filter(Variable %in% c("Price|Carbon", "Emissions|CO2", "Gini_recomputed", "Population")) %>% pivot_wider(id_cols = c(Scenario, Model, Region, Year),names_from = Variable)
ggplot(transfer_data %>% filter(Scenario=="Paris_redist" & Year <= 2050)) + geom_line(aes(Year, `Emissions|CO2`*`Price|Carbon` / Population, color=Model)) + ylab("Carbon Revenues per capita [USD/cap]") + facet_wrap(Region ~ ., scales = "free_x", nrow = 2) + theme(legend.position = "bottom")
saveplot("Carbon Revenues")
#carbon prices
ggplot(transfer_data %>% filter(Scenario=="Paris_redist" & Year <= 2050)) + geom_line(aes(Year, `Price|Carbon`, color=Model)) + ylab("Carbon Price [USD/tCO2eq]") + facet_wrap(Region ~ ., scales = "free_y", nrow = 2) + theme(legend.position = "bottom")
saveplot("Carbon Prices")




data_welfare_effect_reordered <- data_welfare_effect_reordered %>% mutate(Type=case_when(Model %in% c("AIM", "GEM-E3", "Imaclim") ~ "CGE", Model %in% c("ReMIND", "WITCH") ~ "DP-IAM", Model %in% c("NICE", "RICE50+") ~ "CB-IAM", Model %in% c("E3ME") ~ "Macroeconometric", TRUE ~ "Other"))
reg_carbrev <- lm(data_welfare_effect_reordered %>% left_join(transfer_data %>% rename(Scenario.y=Scenario)) %>% filter(Scenario.y=="Paris_redist" &  Scenario.x=="Paris" & Year <= 2050 & Year >= 2020) %>% mutate(gini_change=-100*(value.y_Equality_index - value.x_Equality_index), carbon_revenue_capita=`Emissions|CO2`*`Price|Carbon` / Population *1e-3) %>% mutate(Model=as.factor(Model), Model=relevel(Model, ref="AIM"), Region=as.factor(Region), Region=relevel(Region, ref="United States")), formula = "gini_change ~ carbon_revenue_capita + Region + Model")
summary(reg_carbrev)
stargazer::stargazer(reg_carbrev, type = "latex", single.row = T, out = paste0(graphdir, "/reg.tex"), dep.var.labels = "Gini impact  [points]")
hutils::replace_pattern_in("Model|Region|Type","", file_pattern="*.tex", basedir = graphdir)
hutils::replace_pattern_in("carbon(.*)capita","Carbon revenue per capita (1,000 $)", file_pattern="*.tex", basedir = graphdir)


#carbon revenue regression separately
carb_rev_effect_separate <- data_welfare_effect_reordered %>% left_join(transfer_data %>% rename(Scenario.y=Scenario)) %>% filter(Scenario.y=="Paris_redist" &  Scenario.x=="Paris" & Year <= 2050 & Year >= 2020) %>% mutate(gini_change=-100*(value.y_Equality_index - value.x_Equality_index), carbon_revenue_capita=`Emissions|CO2`*`Price|Carbon` / Population *1e-3) %>% mutate(Model=as.factor(Model), Model=relevel(Model, ref="AIM"), Region=as.factor(Region), Region=relevel(Region, ref="United States")) %>% group_by(Model, Region) %>% nest() %>% 
  mutate(lm_model = map(data, ~lm(gini_change ~ carbon_revenue_capita, data = .))) %>% mutate(tidy = map(lm_model, broom::tidy),glance = map(lm_model, broom::glance),augment = map(lm_model, broom::augment),rsq = glance %>% map_dbl('r.squared'),average_effect = tidy %>% map_dbl(function(x) x$estimate[2]))
quantile(carb_rev_effect_separate$average_effect)



source("main_plots.R")
