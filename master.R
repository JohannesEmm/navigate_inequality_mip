#Consumption or Income?
measure_inequality <- "Consumption"


require(tidyverse)
require(reticulate)
require(data.table)
require(readxl)
require(writexl)
require(yaml)

update_from_iiasadb <- F


if(update_from_iiasadb){
pyam <- import("pyam", convert = FALSE)
iiasa_credentials <- yaml::read_yaml("iiasa_credentials.yml") # two lines with username: and password:
pyam$iiasa$set_config(iiasa_credentials$username, iiasa_credentials$password)
df = pyam$read_iiasa('navigate_internal', scenario='WP4_*')
# any other operations, like filtering, plotting, etc.
df$to_excel("WP4_snapshot.xlsx")
}





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
iiasadb_data <- rbind(iiasadb_data, upload2iiasa("WITCH-WP4-v5.xlsx") %>% mutate(Region=tolower(Region), Region=gsub("india", "India", Region)))
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
ggplot(global_sums) + geom_line(aes(Year, value, color=Scenario, linetype=Model))


#################################### REGIONS #############################
#show all regions per model
iiasadb_data %>% group_by(Model) %>% summarize(regions=unique(Region)) %>% mutate(value=1) %>% pivot_wider(id_cols = regions, names_from = Model) %>% replace(is.na(.), 0) %>% as.data.frame()
#without native regions
iiasadb_data %>% group_by(Model) %>% summarize(regions=unique(Region)) %>% mutate(value=1) %>% pivot_wider(id_cols = regions, names_from = Model) %>% replace(is.na(.), 0) %>% as.data.frame() %>% filter(!str_detect(regions, "\\|"))

ggplot(iiasadb_data %>% group_by(Model, Scenario) %>% filter(!str_detect(Region, "\\|")) %>% summarize(Region=unique(Region)) %>% ungroup() %>% group_by(Model, Region) %>% summarize(Scenarios=length(Scenario)), aes(Region, Model, fill=Scenarios)) + geom_tile() + theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + geom_text(aes(label=Scenarios))  + scale_fill_gradient2(low = "white", mid = "yellow", high = "darkgreen") + scale_x_discrete(labels = function(x) str_wrap(x, width = 50))
saveplot("Regions")





######### Now clean regions and only use common big countries #############
countries_reported <- c("Japan", "India", "United States", "China")
countries_reported_max <- c("France", "India", "Brazil", "Mexico", "United States", "Canada", "China", "Russia", "Japan", "South Africa")

countries_reported <- countries_reported_max

country_naming <- fread("country_naming.csv")
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

ggplot(iiasadb_data %>% group_by(Model, Scenario, Region) %>% filter(!str_detect(Region, "\\|")) %>% summarize(Variable=unique(Variable)) %>% ungroup() %>% group_by(Variable, Model) %>% summarize(RegScen=length(Variable)), aes(Variable, Model, fill=RegScen)) + geom_tile() + theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + geom_text(aes(label=RegScen))  + scale_fill_gradient2(low = "white", mid = "yellow", high = "darkgreen") + scale_x_discrete(labels = function(x) str_wrap(x, width = 20))
saveplot("Variables")



























################################# PLOTS ###############################################
iamc_lineplot <- function(reg="World", var="Emissions|CO2", cumulative=F){
  .data <- iiasadb_data %>% filter(Region %in% reg) %>% filter(Variable==var) 
  if(cumulative) .data <- .data %>% group_by(Model, Region, Scenario, Variable) %>% arrange(Year) %>% mutate(value_cum=value*ifelse(is.na(lead(Year)), Year-lag(Year), lead(Year)-Year), value_cum=cumsum(value_cum)) %>% mutate(value=value_cum * 1e-3)
  .data %>% ggplot() + geom_line(aes(Year, value, color=Model, linetype=Scenario)) + ylab(var) + xlab("") + ggtitle(str_glue("{var}")) + facet_wrap(Region ~ ., scales = "free_y", nrow = 2) + theme(legend.position = "bottom")
  saveplot(gsub("\\|","-", str_glue("{var}")))
}
iamc_lineplot(reg=countries_reported, var="Emissions|CO2")
iamc_lineplot(reg=countries_reported, var="GDP|PPP")
iamc_lineplot(reg=countries_reported, var="Population")
iamc_lineplot(reg=countries_reported, var="Emissions|CO2|Energy and Industrial Processes", cumulative = F)




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

iamc_incidence_curve(scen0 = "REF", scen1 = "Paris", year = 2050)
iamc_incidence_curve(scen0 = "Paris", scen1 = "Paris_redist", year = 2050)
iamc_incidence_curve(scen0 = "REF", scen1 = "Paris_redist", year = 2050)
iamc_incidence_curve(scen0 = "REF", scen1 = "REF_impact", year = 2050)
#Incidence curve for Deliverable
iamc_incidence_curve(scen0 = "REF", scen1 = "Paris", year = 2050, aggregate_use = measure_inequality, Regions = countries_reported)






#for now take Gini based on Consumption!!!!

# Gini index
iamc_lineplot(reg=countries_reported, var="Inequality index|Gini")
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


#Sen-Welfare effect decomposition Figure
data_welfare_effect <- iiasadb_data %>% filter(Variable=="GDP|PPP" | Variable=="Gini_full") %>% mutate(value=ifelse(Variable=="Gini_full", 1-value, value), Variable = gsub("Gini_full", "Equality_index", Variable))
#make sure to remove country model combination swithout data
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
#relative changes across all scenarios
#ggplot(data_welfare_effect %>% filter(Year %in% c(2030, 2050, 2100) & Region %in% countries_reported)) + geom_point(aes(x = relchange_Equality_index, y = `relchange_GDP|PPP`, color=Model), size=3) + scale_x_continuous(labels=scales::percent, limits = xscale*c(-swfrange, +swfrange)) + scale_y_continuous(labels=scales::percent, limits = c(-swfrange, +swfrange)) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_minimal() + labs(x="Equality change", y="GDP change", title="Welfare impact", caption="Scenx to Sceny")
#saveplot("Welfare impact all scenarios")

#changes across two scenarios
scen_switches <- list(one=c("REF", "Paris"), two=c("REF", "Paris_redist"), three=c("REF_impact", "Paris_impact_redist"), four=c("REF_impact", "Paris_impact"))
for(comp in scen_switches) {
  ggplot(data_welfare_effect %>% filter(Year %in% c(2030, 2050, 2100) & Region %in% countries_reported) %>% filter(Scenario.x==comp[1] & Scenario.y==comp[2])) + geom_point(aes(x = relchange_Equality_index, y = `relchange_GDP|PPP`, color=Model, shape=as.character(Year)), size=3) + scale_x_continuous(labels=scales::percent, limits = xscale*c(-swfrange, +swfrange)) + scale_y_continuous(labels=scales::percent, limits = c(-swfrange, +swfrange)) + theme_minimal() + labs(x="Equality change", y="GDP change", title="Welfare impact", caption=str_glue("{comp[1]} to {comp[2]}")) + geom_segment(aes(x = -swfrange, y = 0, xend = +swfrange, yend = 0),arrow = arrow(length = unit(0.2, "cm"))) + geom_segment(aes(y = -swfrange, x = 0, yend = +swfrange, xend = 0),arrow = arrow(length = unit(0.2, "cm")))
  saveplot(str_glue("Welfare change {comp[1]} to {comp[2]}"))
}







#Arrows from REF to WB2C and redist
#ggplot() + theme_minimal() + labs(x="Equality", y="GDP", title="Welfare impact", caption="REF to WB2C to WB2C_redist")  + scale_y_log10() + geom_segment(data = data_welfare_effect %>% filter(Year %in% c(2050, 2100) & Region=="India") %>% filter(Scenario.x=="REF" & Scenario.y=="Paris"), aes(x = value.x_Equality_index, y = `value.x_GDP|PPP`, xend = value.y_Equality_index, yend = `value.y_GDP|PPP`, color=Model), size=1, arrow = arrow(type = "closed", length = unit(1, "mm"))) + geom_segment(data = data_welfare_effect %>% filter(Year %in% c(2050, 2100) & Region=="India") %>% filter(Scenario.x=="Paris" & Scenario.y=="Paris_redist"), aes(x = value.x_Equality_index, y = `value.x_GDP|PPP`, xend = value.y_Equality_index, yend = `value.y_GDP|PPP`, color=Model), size=1, arrow = arrow(type = "open", length = unit(1, "mm"))) + facet_wrap(Year ~  Model, scales = "free")

#All scenarios per model in LEVELs
ggplot(data_welfare_effect %>% filter(Year %in% c(2050) & Region=="India" & Scenario.x==Scenario.y & !str_detect(Scenario.x, "OFF"))) + geom_point(aes(x = value.x_Equality_index, y =  `value.x_GDP|PPP`, color=Scenario.x), size=3) + theme_minimal() + labs(x="Equality", y="GDP", title="Welfare in 2050") + facet_wrap(Year ~ Model, scales = "fixed")
saveplot("Welfare levels")



data_welfare_effect <- data_welfare_effect %>% mutate(Models=ifelse(Model=="RICE50+", "+", substr(Model,1,1)))
#changes from REF for 3 years all models and all Paris scenarios
swfrange = 0.10
#ggplot(data_welfare_effect %>% filter(Year %in% c(2030, 2050, 2100) & Region %in% countries_reported) %>% filter(Scenario.x=="REF" & Scenario.y %in% c("Paris", "Paris_redist", "Paris_impact_redist"))) + geom_point(aes(x = relchange_Equality_index, y = `relchange_GDP|PPP`, color=Model, size=as.character(Year), shape=Scenario.y)) + scale_size_manual(values = c("2030"=3, "2050"=2, "2100"=1)) + 
#  scale_x_continuous(labels=scales::percent, limits = xscale*c(-swfrange, +swfrange)) + scale_y_continuous(labels=scales::percent, limits = c(-swfrange, +swfrange)) +
#  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_minimal() + labs(x="Equality change", y="GDP change", title="Welfare impact") + facet_wrap(Region ~ ., scales = "free") + labs(size="Year", shape="Scenario", color="Model", fill="Scenario") + ggalt::geom_encircle(aes(x = relchange_Equality_index, y = `relchange_GDP|PPP`, fill=Scenario.y, s_shape = 0.7, expand = 0.05), alpha = 0.22)
#saveplot("Change in GDP and inequality across scenarios Paris")
#ggplot(data_welfare_effect %>% filter(Year %in% c(2030, 2050, 2100) & Region %in% countries_reported) %>% filter(Scenario.x=="REF" & Scenario.y %in% c("1150", "1150_redist", "1150_impact_redist"))) + geom_point(aes(x = relchange_Equality_index, y = `relchange_GDP|PPP`, color=Model, size=as.character(Year), shape=Scenario.y)) + scale_size_manual(values = c("2030"=3, "2050"=2, "2100"=1)) + 
#  scale_x_continuous(labels=scales::percent, limits = xscale*c(-swfrange, +swfrange)) + scale_y_continuous(labels=scales::percent, limits = c(-swfrange, +swfrange)) +
#  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_minimal() + labs(x="Equality change", y="GDP change", title="Welfare impact") + facet_wrap(Region ~ ., scales = "free") + labs(size="Year", shape="Scenario", color="Model", fill="Scenario") + #ggalt::geom_encircle(aes(x = relchange_Equality_index, y = `relchange_GDP|PPP`, fill=Scenario.y, s_shape = 0.7, expand = 0.05), alpha = 0.22)
#saveplot("Change in GDP and inequality across scenarios 1150")

#for better readability, model as shape, scenario as color
ggplot(data_welfare_effect %>% filter(Year %in% c(2030, 2050, 2100) & Region %in% countries_reported_max) %>% filter(Scenario.x=="REF" & Scenario.y %in% c("Paris", "Paris_redist", "Paris_impact_redist")) %>% mutate(scenarioclass=case_when(str_detect(Scenario.y, "impact") ~ "EPC with avoided Impacts", str_detect(Scenario.y, "redist$") ~ "EPC redistribution", TRUE ~ "Climate Policy"))) + geom_point(aes(x = relchange_Equality_index, y = `relchange_GDP|PPP`, color=scenarioclass, size=as.character(Year), shape=Models)) + scale_size_manual(values = c("2030"=3, "2050"=2, "2100"=1)) + 
  scale_x_continuous(labels=scales::percent, limits = xscale*c(-swfrange, +swfrange)) + scale_y_continuous(labels=scales::percent, limits = c(-swfrange, +swfrange)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_minimal() + labs(x="Equality change", y="GDP change", title="Welfare impact") + facet_wrap(Region ~ ., scales = "free", nrow=3) + labs(size="Year", shape="Model", color="Scenario", fill="Scenario") + ggalt::geom_encircle(aes(x = relchange_Equality_index, y = `relchange_GDP|PPP`, fill=scenarioclass, s_shape = 0.7, expand = 0.05), alpha = 0.22) + scale_shape_identity() + scale_fill_manual(values = c("Climate Policy" = "red", "EPC redistribution"="blue", "EPC with avoided Impacts"="darkgreen")) + theme(legend.position =c(0.80, 0.15))

#now Gini on x axis
ggplot(data_welfare_effect %>% filter(Year %in% c(2030, 2050, 2100) & Region %in% countries_reported_max) %>% filter(Scenario.x=="REF" & Scenario.y %in% c("Paris", "Paris_redist", "Paris_impact_redist")) %>% mutate(scenarioclass=case_when(str_detect(Scenario.y, "impact") ~ "EPC with avoided Impacts", str_detect(Scenario.y, "redist$") ~ "EPC redistribution", TRUE ~ "Climate Policy"))) + geom_point(aes(x = -100*(value.y_Equality_index-value.x_Equality_index), y = `relchange_GDP|PPP`, color=scenarioclass, size=as.character(Year), shape=Models)) + scale_size_manual(values = c("2030"=3, "2050"=2, "2100"=1))  +   scale_x_continuous(limits = c(-8,+5)) + scale_y_continuous(labels=scales::percent, limits = c(-swfrange, +swfrange)) +   geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_minimal() + labs(x="Change in the Gini index [points]", y="GDP change", title="Welfare impact") + facet_wrap(Region ~ ., scales = "free", nrow=3) + labs(size="Year", shape="Model", color="Scenario", fill="Scenario") + ggalt::geom_encircle(aes(x = -100*(value.y_Equality_index-value.x_Equality_index), y = `relchange_GDP|PPP`, fill=scenarioclass, s_shape = 0.7, expand = 0.05), alpha = 0.22) + scale_shape_identity() + scale_fill_manual(values = c("Climate Policy" = "red", "EPC redistribution"="blue", "EPC with avoided Impacts"="darkgreen")) + theme(legend.position =c(0.80, 0.15))
saveplot("Change in GDP and inequality across scenarios Paris", width = 12, height = 10, plot_theme = NULL)


#now showing model mean only 
#now with errorbars
numse=1
ggplot(data_welfare_effect_mod_mean %>% filter(Year %in% c(2030, 2050, 2100) & Region %in% countries_reported_max) %>% filter(Scenario.x=="REF" & Scenario.y %in% c("Paris", "Paris_redist", "Paris_impact_redist")) %>% mutate(scenarioclass=case_when(str_detect(Scenario.y, "impact") ~ "EPC with avoided Impacts", str_detect(Scenario.y, "redist$") ~ "EPC redistribution", TRUE ~ "Climate Policy"))) + geom_point(aes(x = 100*MEAN_abschange_Gini, y = `MEAN_relchange_GDP|PPP`, color=scenarioclass, shape=as.character(Year)), size = 3)  +   scale_x_continuous(limits = c(-10,+10)) + scale_y_continuous(labels=scales::percent, limits = c(-swfrange, +swfrange)) +   geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_minimal() + labs(x="Change in the Gini index [points]", y="GDP change", title="Welfare impact") + facet_wrap(Region ~ ., scales = "free", nrow=3) + labs(shape="Year", color="Scenario") + scale_fill_manual(values = c("Climate Policy" = "red", "EPC redistribution"="blue", "EPC with avoided Impacts"="darkgreen")) + theme(legend.position =c(0.80, 0.15)) + geom_errorbarh(aes(x = 100*MEAN_abschange_Gini, y = `MEAN_relchange_GDP|PPP`, color=scenarioclass, shape=as.character(Year), xmin=100*(MEAN_abschange_Gini-numse*SD_abschange_Gini), xmax=100*(MEAN_abschange_Gini+numse*SD_abschange_Gini))) + geom_errorbar(aes(x = 100*MEAN_abschange_Gini, y = `MEAN_relchange_GDP|PPP`, color=scenarioclass, shape=as.character(Year), ymin=(`MEAN_relchange_GDP|PPP`-numse*`SD_relchange_GDP|PPP`), ymax=(`MEAN_relchange_GDP|PPP`+numse*`SD_relchange_GDP|PPP`)))
saveplot("Change in GDP and inequality across scenarios Paris (model mean)", width = 12, height = 10, plot_theme = NULL)





#new idea for welfare plots
ggplot(data_welfare_effect %>% filter(Region %in% countries_reported_max) %>% filter(Scenario.x=="REF" & Scenario.y %in% c("Paris", "Paris_redist", "Paris_impact_redist")) %>% mutate(scenarioclass=case_when(str_detect(Scenario.y, "impact") ~ "EPC with avoided Impacts", str_detect(Scenario.y, "redist$") ~ "EPC redistribution", TRUE ~ "Climate Policy"))) + geom_line(aes(x = -100*(value.y_Equality_index-value.x_Equality_index), y = `relchange_GDP|PPP`, color=scenarioclass, size=as.character(Year), shape=Models)) + scale_size_manual(values = c("2030"=3, "2050"=2, "2100"=1))  +   scale_x_continuous(limits = c(-8,+5)) + scale_y_continuous(labels=scales::percent, limits = c(-swfrange, +swfrange)) +   geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_minimal() + labs(x="Change in the Gini index [points]", y="GDP change", title="Welfare impact") + facet_wrap(Region ~ ., scales = "free", nrow=3) + labs(size="Year", shape="Model", color="Scenario", fill="Scenario") + ggalt::geom_encircle(aes(x = -100*(value.y_Equality_index-value.x_Equality_index), y = `relchange_GDP|PPP`, fill=scenarioclass, s_shape = 0.7, expand = 0.05), alpha = 0.22) + scale_shape_identity() + scale_fill_manual(values = c("Climate Policy" = "red", "EPC redistribution"="blue", "EPC with avoided Impacts"="darkgreen")) + theme(legend.position =c(0.80, 0.15))





ggplot(data_welfare_effect_mod_mean %>% filter(Year >=2025 & Year <=2100 & Region %in% countries_reported_max) %>% filter(Scenario.x=="REF" & Scenario.y %in% c("Paris", "Paris_redist", "Paris_impact_redist")) %>% mutate(scenarioclass=case_when(str_detect(Scenario.y, "impact") ~ "EPC with avoided Impacts", str_detect(Scenario.y, "redist$") ~ "EPC redistribution", TRUE ~ "Climate Policy"))) + geom_line(aes(x = Year, y = `MEAN_relchange_GDP|PPP`, color=scenarioclass), size = 1) + facet_wrap(Region ~ ., scales = "fixed", nrow=3) + geom_hline(yintercept = 0) + theme_minimal() + scale_y_continuous(labels=scales::percent) 
saveplot("Change only GDP")
ggplot(data_welfare_effect_mod_mean %>% filter(Year >=2025 & Year <=2100 & Region %in% countries_reported_max) %>% filter(Scenario.x=="REF" & Scenario.y %in% c("Paris", "Paris_redist", "Paris_impact_redist")) %>% mutate(scenarioclass=case_when(str_detect(Scenario.y, "impact") ~ "EPC with avoided Impacts", str_detect(Scenario.y, "redist$") ~ "EPC redistribution", TRUE ~ "Climate Policy"))) + geom_line(aes(x = Year, y = MEAN_abschange_Gini, color=scenarioclass), size = 1) + facet_wrap(Region ~ ., scales = "fixed", nrow=3) + geom_hline(yintercept = 0) + theme_minimal() + scale_y_continuous(labels=scales::percent) 
saveplot("Change only Gini")
ggplot(data_welfare_effect_mod_mean %>% filter(Year >=2025 & Year <=2100 & Region %in% countries_reported_max) %>% filter(Scenario.x=="REF" & Scenario.y %in% c("Paris", "Paris_redist", "Paris_impact_redist")) %>% mutate(scenarioclass=case_when(str_detect(Scenario.y, "impact") ~ "EPC with avoided Impacts", str_detect(Scenario.y, "redist$") ~ "EPC redistribution", TRUE ~ "Climate Policy")) %>% pivot_longer(cols = 5:10, names_to = "variable") %>% filter(variable %in% c("MEAN_abschange_Gini", "MEAN_relchange_GDP|PPP"))) + geom_line(aes(x = Year, y = value, color=scenarioclass), size = 1) + facet_grid(Region ~ variable) + geom_hline(yintercept = 0) + theme_minimal() + scale_y_continuous(labels=scales::percent) + theme(legend.position = "bottom")
saveplot("Change GDP and Gini")
#for all models
ggplot(data_welfare_effect %>% filter(Year >=2025 & Year <=2100 & Region %in% countries_reported_max) %>% filter(Scenario.x=="REF" & Scenario.y %in% c("Paris", "Paris_redist", "Paris_impact_redist")) %>% mutate(scenarioclass=case_when(str_detect(Scenario.y, "impact") ~ "EPC with avoided Impacts", str_detect(Scenario.y, "redist$") ~ "EPC redistribution", TRUE ~ "Climate Policy")) %>% mutate(MEAN_abschange_Gini=-(value.y_Equality_index-value.x_Equality_index),  `MEAN_relchange_GDP|PPP` = `relchange_GDP|PPP`) %>% pivot_longer(cols = 14:15, names_to = "variable") %>% filter(variable %in% c("MEAN_abschange_Gini", "MEAN_relchange_GDP|PPP"))) + geom_line(aes(x = Year, y = value, color=scenarioclass, group=interaction(scenarioclass, Model)), size = 1) + facet_grid(Region ~ variable) + geom_hline(yintercept = 0) + theme_minimal() + scale_y_continuous(labels=scales::percent) + theme(legend.position = "bottom") + ylim(-swfrange,+swfrange)





#is modmoean correct?
#writexl::write_xlsx(data_welfare_effect %>% filter(Region=="India" & Year==2050 & Scenario.y=="Paris" & Scenario.x == "REF") %>% as.data.frame(), path = "test_india2.xlsx")
#writexl::write_xlsx(data_welfare_effect_mod_mean %>% filter(Region=="India" & Year==2050 & Scenario.y=="Paris" & Scenario.x == "REF") %>% as.data.frame(), path = "test_india.xlsx")







ggplot(data_welfare_effect %>% filter(Year %in% c(2030, 2050, 2100) & Region %in% countries_reported_max) %>% filter(Scenario.x=="REF" & Scenario.y %in% c("1150", "1150_redist", "1150_impact_redist")) %>% mutate(scenarioclass=case_when(str_detect(Scenario.y, "impact") ~ "EPC with avoided Impacts", str_detect(Scenario.y, "redist$") ~ "EPC redistribution", TRUE ~ "Climate Policy"))) + geom_point(aes(x = relchange_Equality_index, y = `relchange_GDP|PPP`, color=scenarioclass, size=as.character(Year), shape=Models)) + scale_size_manual(values = c("2030"=3, "2050"=2, "2100"=1)) + 
  scale_x_continuous(labels=scales::percent, limits = xscale*c(-swfrange, +swfrange)) + scale_y_continuous(labels=scales::percent, limits = c(-swfrange, +swfrange)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_minimal() + labs(x="Equality change", y="GDP change", title="Welfare impact") + facet_wrap(Region ~ ., scales = "free", nrow = 3) + labs(size="Year", shape="Model", color="Scenario", fill="Scenario") + ggalt::geom_encircle(aes(x = relchange_Equality_index, y = `relchange_GDP|PPP`, fill=scenarioclass, s_shape = 0.7, expand = 0.05), alpha = 0.22) + scale_shape_identity() + scale_fill_manual(values = c("Climate Policy" = "red", "EPC redistribution"="blue", "EPC with avoided Impacts"="darkgreen")) + theme(legend.position =c(0.80, 0.15))
#now Gini on x axis
ggplot(data_welfare_effect %>% filter(Year %in% c(2030, 2050, 2100) & Region %in% countries_reported_max) %>% filter(Scenario.x=="REF" & Scenario.y %in% c("1150", "1150_redist", "1150_impact_redist")) %>% mutate(scenarioclass=case_when(str_detect(Scenario.y, "impact") ~ "EPC with avoided Impacts", str_detect(Scenario.y, "redist$") ~ "EPC redistribution", TRUE ~ "Climate Policy"))) + geom_point(aes(x = -100*(value.y_Equality_index-value.x_Equality_index), y = `relchange_GDP|PPP`, color=scenarioclass, size=as.character(Year), shape=Models)) + scale_size_manual(values = c("2030"=3, "2050"=2, "2100"=1))  +   scale_x_continuous(limits = c(-8,+5)) + scale_y_continuous(labels=scales::percent, limits = c(-swfrange, +swfrange)) +   geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_minimal() + labs(x="Change in the Gini index [points]", y="GDP change", title="Welfare impact") + facet_wrap(Region ~ ., scales = "free", nrow=3) + labs(size="Year", shape="Model", color="Scenario", fill="Scenario") + ggalt::geom_encircle(aes(x = -100*(value.y_Equality_index-value.x_Equality_index), y = `relchange_GDP|PPP`, fill=scenarioclass, s_shape = 0.7, expand = 0.05), alpha = 0.22) + scale_shape_identity() + scale_fill_manual(values = c("Climate Policy" = "red", "EPC redistribution"="blue", "EPC with avoided Impacts"="darkgreen")) + theme(legend.position =c(0.80, 0.15))
saveplot("Change in GDP and inequality across scenarios 1150", width = 12, height = 10, plot_theme = NULL)







#Inequality change across scenarios, model ranges from left to right
data_welfare_effect_reordered <- data_welfare_effect %>% mutate(Scenario.y=as.factor(Scenario.y), Scenario.y=fct_relevel(Scenario.y, c("REF", "1150", "Paris", "1150_redist", "Paris_redist", "REF_impact", "1150_impact", "1150_impact_redist", "Paris_impact", "Paris_impact_redist"))) 
data_welfare_effect_reordered <- data_welfare_effect_reordered %>% mutate(scenarioclass=case_when(str_detect(Scenario.y, "impact") ~ "EPC with avoided Impacts", str_detect(Scenario.y, "redist$") ~ "EPC redistribution", TRUE ~ "Climate Policy"))
#ggplot(data_welfare_effect_reordered %>% filter(Year %in% c(2030, 2050, 2100) & Region %in% countries_reported) %>% filter(Scenario.x=="REF" & Scenario.y %in% c("1150", "Paris", "1150_redist", "Paris_redist", "1150_impact_redist", "Paris_impact_redist")) %>% mutate(Scenario.y=factor(Scenario.y)) %>% mutate(Scenario.y.start = as.numeric(Scenario.y) - 0.5, Scenario.y.end = as.numeric(Scenario.y) + 0.5)) + geom_point(aes(x = Scenario.y, y = 100*(value.x_Equality_index-value.y_Equality_index), color=Model)) + facet_grid(Region ~ Year)  + theme_minimal() + geom_hline(yintercept = 0) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + labs(y="Change in Gini [points]", x="Scenario", title="Impact on the Gini index")  + geom_rect(aes(xmin=Scenario.y.start, xmax=Scenario.y.end, ymin=-5, ymax=5, fill=scenarioclass), alpha=0.05)
#saveplot("Gini impact over scenarios")


#version with boxplot for presentation
#ggplot(data_welfare_effect_reordered %>% filter(Year %in% c(2030, 2050, 2100) & Region %in% countries_reported) %>% filter(Scenario.x=="REF" & Scenario.y %in% c("1150", "Paris", "1150_redist", "Paris_redist", "1150_impact_redist", "Paris_impact_redist")) %>% mutate(Scenario.y=factor(Scenario.y)) %>% mutate(Scenario.y.start = as.numeric(Scenario.y) - 0.5, Scenario.y.end = as.numeric(Scenario.y) + 0.5), cbudget=gsub("[a-z_]","", Scenario.y)) + geom_point(aes(x = Scenario.y, y = 100*(value.x_Equality_index-value.y_Equality_index), color=Model)) + facet_grid(Region ~ Year, scales = "free_y")  + theme_minimal() + geom_hline(yintercept = 0) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + labs(y="Change in Gini [points]", x="Scenario", title="Impact on the Gini index")  + geom_rect(aes(xmin=Scenario.y.start, xmax=Scenario.y.end, ymin=-Inf, ymax=+Inf, fill=scenarioclass), alpha=0.05) + geom_boxplot(aes(x = Scenario.y, y = 100*(value.x_Equality_index-value.y_Equality_index)), alpha=0.3, color="grey50") + labs(fill="Scenario", x="Carbon budget") + scale_x_discrete(labels=rep(c("1150", "Paris"), 3)) 
#saveplot("Gini impact over scenarios v2")



#10 countries, only Paris
ggplot(data_welfare_effect_reordered %>% filter(Year %in% c(2030, 2050, 2100) & Region %in% countries_reported_max) %>% filter(Scenario.x=="REF" & Scenario.y %in% c("1150", "Paris", "1150_redist", "Paris_redist", "1150_impact_redist", "Paris_impact_redist")) %>% filter(str_detect(Scenario.y, "Paris")) %>% mutate(Scenario.y=factor(Scenario.y)) %>% mutate(Scenario.y.start = as.numeric(Scenario.y) - 0.5, Scenario.y.end = as.numeric(Scenario.y) + 0.5), cbudget=gsub("[a-z_]","", Scenario.y)) + geom_point(aes(x = Scenario.y, y = 100*(value.x_Equality_index-value.y_Equality_index), color=Model)) + facet_grid(Region ~ Year, scales = "free_y")  + theme_minimal() + geom_hline(yintercept = 0) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + labs(y="Change in Gini [points]", x="Scenario", title="Impact on the Gini index")  + geom_rect(aes(xmin=Scenario.y.start, xmax=Scenario.y.end, ymin=-Inf, ymax=+Inf, fill=scenarioclass), alpha=0.05) + geom_boxplot(aes(x = Scenario.y, y = 100*(value.x_Equality_index-value.y_Equality_index)), alpha=0.3, color="grey50") + labs(fill="Scenario", x="") + scale_x_discrete(labels=rep(c("Climate Policy", " + EPC", "+ Residual Impacts"),3)) + scale_fill_manual(values = c("Climate Policy" = "red", "EPC redistribution"="blue", "EPC with avoided Impacts"="darkgreen")) # + ylim(-10,+10)
saveplot("Gini impact over scenarios all countries", width = 8, height = 12, plot_theme = NULL, text_size = 24)






#Emissions, carbon price, and transfers for EPC anaysis
transfer_data <- iiasadb_data %>% filter(Variable %in% c("Price|Carbon", "Emissions|CO2", "Gini_recomputed", "Population")) %>% pivot_wider(id_cols = c(Scenario, Model, Region, Year),names_from = Variable)
ggplot(transfer_data %>% filter(Scenario=="Paris_redist" & Year <= 2050)) + geom_line(aes(Year, `Emissions|CO2`*`Price|Carbon` / Population, color=Model)) + ylab("Carbon Revenues per capita [USD/cap]") + facet_wrap(Region ~ ., scales = "free_x", nrow = 2) + theme(legend.position = "bottom")
saveplot("Carbon Revenues")
#carbon prices
ggplot(transfer_data %>% filter(Scenario=="Paris_redist" & Year <= 2050)) + geom_line(aes(Year, `Price|Carbon`, color=Model)) + ylab("Carbon Price [USD/tCO2eq]") + facet_wrap(Region ~ ., scales = "free_y", nrow = 2) + theme(legend.position = "bottom")
saveplot("Carbon Prices")

#Correlation of Transper per capita and Gini
#global relationship
ggplot(data_welfare_effect_reordered %>% left_join(transfer_data %>% rename(Scenario.y=Scenario)) %>% filter(Scenario.y=="Paris_redist" &  Scenario.x=="Paris" & Year <= 2050)) + geom_point(aes(`Emissions|CO2`*`Price|Carbon` / Population, -100*(value.y_Equality_index - value.x_Equality_index), color=Model, alpha=Year, shape=Region)) + facet_wrap(. ~ ., scales = "free", nrow = 2) + theme(legend.position = "bottom") + geom_smooth(aes(`Emissions|CO2`*`Price|Carbon` / Population, -100*(value.y_Equality_index - value.x_Equality_index)), method="lm") + labs(x="Carbon Revenues per capita [USD/cap]", y = "Change in the Gini with EPC in the Paris senario [points]") + geom_hline(yintercept = 0)
#by country
ggplot(data_welfare_effect_reordered %>% left_join(transfer_data %>% rename(Scenario.y=Scenario)) %>% filter(Scenario.y=="Paris_redist" &  Scenario.x=="Paris" & Year <= 2050)) + geom_point(aes(`Emissions|CO2`*`Price|Carbon` / Population, -100*(value.y_Equality_index - value.x_Equality_index), color=Model, alpha=Year)) + facet_wrap(Region ~ ., scales = "free", nrow = 2) + theme(legend.position = "bottom") + geom_smooth(aes(`Emissions|CO2`*`Price|Carbon` / Population, -100*(value.y_Equality_index - value.x_Equality_index)), method="lm") + labs(x="Carbon Revenues per capita [USD/cap]", y = "Change in the Gini with EPC in the Paris senario [points]") + geom_hline(yintercept = 0)

#now avoid negative transfers (remove obs)
ggplot(data_welfare_effect_reordered %>% left_join(transfer_data %>% rename(Scenario.y=Scenario)) %>% filter(Scenario.y=="Paris_redist" &  Scenario.x=="Paris" & Year <= 2050) %>% filter(`Emissions|CO2`>=0)) + geom_point(aes(`Emissions|CO2`*`Price|Carbon` / Population, -100*(value.y_Equality_index - value.x_Equality_index), color=Model, alpha=Year)) + facet_wrap(Region ~ ., scales = "free", nrow = 2) + theme(legend.position = "bottom") + geom_smooth(aes(`Emissions|CO2`*`Price|Carbon` / Population, -100*(value.y_Equality_index - value.x_Equality_index)), method="lm") + labs(x="Carbon Revenues per capita [USD/cap]", y = "Change in the Gini with EPC in the Paris senario [points]") + geom_hline(yintercept = 0)
saveplot("Carbon Revenes and Gini impact")

#and fix start at zero
ggplot(data_welfare_effect_reordered %>% left_join(transfer_data %>% rename(Scenario.y=Scenario)) %>% filter(Scenario.y=="Paris_redist" &  Scenario.x=="Paris" & Year <= 2050) %>% filter(`Emissions|CO2`>=0)) + geom_point(aes(`Emissions|CO2`*`Price|Carbon` / Population, -100*(value.y_Equality_index - value.x_Equality_index), color=Model, alpha=Year)) + facet_wrap(Region ~ ., scales = "free", nrow = 2) + theme(legend.position = "bottom") + geom_smooth(aes(`Emissions|CO2`*`Price|Carbon` / Population, -100*(value.y_Equality_index - value.x_Equality_index)), method="lm", formula = y ~ 0 + x) + labs(x="Carbon Revenues per capita [USD/cap]", y = "Change in the Gini with EPC in the Paris senario [points]") + geom_hline(yintercept = 0)

reg_carbrev <- lm(data_welfare_effect_reordered %>% left_join(transfer_data %>% rename(Scenario.y=Scenario)) %>% filter(Scenario.y=="Paris_redist" &  Scenario.x=="Paris" & Year <= 2050 & Year >= 2020) %>% mutate(gini_change=-100*(value.y_Equality_index - value.x_Equality_index), carbon_revenue_capita=`Emissions|CO2`*`Price|Carbon` / Population *1e-3) %>% mutate(Model=as.factor(Model), Model=relevel(Model, ref="AIM"), Region=as.factor(Region), Region=relevel(Region, ref="United States")), formula = "gini_change ~ carbon_revenue_capita + Region + Model")
summary(reg_carbrev)
stargazer::stargazer(reg_carbrev, type = "latex", single.row = T, out = paste0(graphdir, "/reg.tex"), dep.var.labels = "Gini impact  [points]")
hutils::replace_pattern_in("Model|Region","", file_pattern="*.tex", basedir = graphdir)
hutils::replace_pattern_in("carbon(.*)capita","Carbon revenue per capita (1,000 $)", file_pattern="*.tex", basedir = graphdir)

#reg_epc_obs <- cbind(data_welfare_effect_reordered %>% left_join(transfer_data %>% rename(Scenario.y=Scenario)) %>% filter(Scenario.y=="Paris_redist" &  Scenario.x=="Paris" & Year <= 2050 & Year >= 2020) %>% mutate(gini_change=-100*(value.y_Equality_index - value.x_Equality_index), carbon_revenue_capita=`Emissions|CO2`*`Price|Carbon` / Population), predict(object = reg_carbrev, newdata = data_welfare_effect_reordered %>% left_join(transfer_data %>% rename(Scenario.y=Scenario)) %>% filter(Scenario.y=="Paris_redist" &  Scenario.x=="Paris" & Year <= 2050 & Year >= 2020) %>% mutate(gini_change=-100*(value.y_Equality_index - value.x_Equality_index), carbon_revenue_capita=`Emissions|CO2`*`Price|Carbon` / Population)))
#table(reg_epc_obs$Model, reg_epc_obs$Region)





source("new_plots_reorder.R")
