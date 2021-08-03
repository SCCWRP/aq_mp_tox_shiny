#### Aquatic Microplastics Toxicology Shiny App
#### File created: September 23, 2020
#### Code contributors: Heili Lowman, Leah Thornton Hampton, Scott Coffin, Emily Darin



#### Setup ####

# Load packages
library(tidyverse) #General everything
library(data.table) #Faster functions for loading data set
library(RColorBrewer) #color palette
library(ggplot2) #plotting
library(ggrepel) #For adding text labels that repel away from data points
library(calecopal) #Color palette
library(shiny) #Runs shiny
library(shinythemes) #Shiny theme for the page
library(shinyWidgets) #Widgets
library(scales) #SSD - Use the percent format
library(reshape2) #Overview tab - melts bars together
library(ssdtools) #SSD package
library(DT) #Build HTML data tables
library(plotly) #Make plots interactive
library(viridis) #Colors
library(scales) #To use "percent" function
library(shinyjs) #Exploration tab - reset button
library(tigerstats) #turns things into percents
library(ggbeeswarm) #plot all points
library(fitdistrplus) #alt SSD 
library(ggdark) #dark mode ggplot
library(ggsci) #color palettes
# library(bslib) #better themes. required for dark mode
# library(thematic) #complete control over themes (including plots) required for dark mode
library(collapsibleTree) #plot type for endpoint category tree
library(hrbrthemes) #theme for screening plot
library(shinydashboard)

# Load finalized dataset.
aoc <- fread("AquaticOrganisms_Clean_test.csv") 

#### Introduction Setup ####

# All text inputs below.

#### Overview AO Setup ####

polydf<-rowPerc(xtabs( ~polymer +effect, aoc)) #pulls polymers by effect 
polyf<-as.data.frame(polydf)%>% #Makes data frame 
  filter(effect %in% c("Yes","No"))%>% #Sorts into Yes and No
  mutate(polymer = case_when(
    polymer == "BIO" ~ "Biopolymer",
    polymer == "EVA" ~ "Polyethylene Vinyl Acetate",
    polymer == "LTX" ~ "Latex",
    polymer == "PA" ~ "Polyamide",
    polymer == "PE" ~ "Polyethylene",
    polymer == "PC" ~ "Polycarbonate",
    polymer == "PET" ~ "Polyethylene Terephthalate",
    polymer == "PI" ~ "Polyisoprene",
    polymer == "PMMA" ~ "Polymethylmethacrylate",
    polymer == "PP" ~ "Polypropylene",
    polymer == "PS" ~ "Polystyrene",
    polymer == "PUR" ~ "Polyurethane",
    polymer == "PVC" ~ "Polyvinylchloride",
    polymer == "PLA" ~ "Polylactic Acid"))%>%
  mutate_if(is.numeric, round,0) #rounds percents 
Endpoints<-xtabs(~polymer +effect ,aoc) #Pulls all study obs. for polymer from dataset
polyfinal<- data.frame(cbind(polyf, Endpoints))%>% #adds it as a column
  rename(Endpoints='Freq.1')%>% #renames column
  mutate(logEndpoints = log(Endpoints))%>%
  rename(Percent = Freq)#renames column

sizedf<-rowPerc(xtabs(~size.category +effect, aoc))
sizef<-as.data.frame(sizedf)%>%
  filter(effect %in% c("Yes","No"))%>%
  mutate(size.category = case_when(
    size.category == 1 ~ "1nm < 100nm",
    size.category == 2 ~ "100nm < 1µm",
    size.category == 3 ~ "1µm < 100µm",
    size.category == 4 ~ "100µm < 1mm",
    size.category == 5 ~ "1mm < 5mm",
    size.category == 0 ~ "Not Reported"))%>% 
  rename(Type = "size.category")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Size")
study_s<-xtabs(~size.category +effect ,aoc)
sizefinal<- data.frame(cbind(sizef, study_s))%>% 
  rename(Endpoints='Freq.1')%>%
  rename(category='size.category')%>%
  mutate(logEndpoints = log(Endpoints))%>%
  rename(Percent = Freq)#renames column

      
shapedf<-rowPerc(xtabs(~shape + effect, aoc))
shapef<-as.data.frame(shapedf)%>%
  filter(effect %in% c("Yes","No"))%>%
  rename(Type="shape")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Shape")%>%
  mutate(Type = case_when(
    Type == "cube" ~ "Cube",
    Type == "sphere" ~ "Sphere",
    Type == "fragment" ~ "Fragment",
    Type == "fiber" ~ "Fiber"))
study_sh<-xtabs(~shape + effect,aoc)
shapefinal<- data.frame(cbind(shapef, study_sh))%>% 
  rename(Endpoints='Freq.1')%>%
  rename(category='shape')%>%
  mutate(logEndpoints = log(Endpoints))%>%
  rename(Percent = Freq)#renames column

taxdf<-rowPerc(xtabs(~organism.group +effect, aoc))
taxf<-as.data.frame(taxdf)%>%
  filter(effect %in% c("Yes","No"))%>%
  rename(Type= "organism.group")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Organism")
study_t<-xtabs(~organism.group +effect,aoc)
taxfinal<- data.frame(cbind(taxf, study_t))%>% 
  rename(Endpoints='Freq.1')%>%
  rename(category='organism.group')%>%
  mutate(logEndpoints = log(Endpoints))%>%
  rename(Percent = Freq)#renames column

lvl1df<-rowPerc(xtabs(~lvl1 +effect, aoc))
lvl1f<-as.data.frame(lvl1df)%>%
  filter(effect %in% c("Yes","No"))%>%
  rename(Type= "lvl1")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Lvl1")%>%
  mutate(Type = case_when(
    Type == "alimentary.excretory" ~ "Alimentary, Excretory",
    Type == "behavioral.sense.neuro" ~ "Behavioral, Sensory, Neurological",
    Type == "circulatory.respiratory" ~ "Circulatory, Respiratory",
    Type == "community" ~ "Community",
    Type == "fitness" ~ "Fitness",
    Type == "immune" ~ "Immune",
    Type == "metabolism" ~ "Metabolism",
    Type == "microbiome" ~ "Microbiome",
    Type == "stress" ~ "Stress")) 
study_l<-xtabs(~lvl1 +effect,aoc)
lvl1final<- data.frame(cbind(lvl1f, study_l))%>% 
  rename(Endpoints='Freq.1')%>%
  rename(category='lvl1')%>%
  mutate(logEndpoints = log(Endpoints))%>%
  rename(Percent = Freq)#renames column
  
lifedf<-rowPerc(xtabs(~life.stage +effect, aoc))
lifef<-as.data.frame(lifedf)%>%
  filter(effect %in% c("Yes","No"))%>%
  rename(Type= "life.stage")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Life.stage")
studyli<-xtabs(~life.stage +effect ,aoc)
lifefinal<- data.frame(cbind(lifef, studyli))%>% 
  rename(Endpoints='Freq.1')%>%
  rename(category='life.stage')%>%
  mutate(logEndpoints = log(Endpoints))%>%
  rename(Percent = Freq)#renames column

vivodf<-rowPerc(xtabs(~invitro.invivo +effect, aoc))
vivof<-as.data.frame(vivodf)%>%
  filter(effect %in% c("Yes","No"))%>%
  rename(Type= "invitro.invivo")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Invivo.invivo")%>%
  mutate(Type = case_when(
    Type=="invivo"~"In Vivo",
    Type=="invitro"~"In Vitro"))
study_v<-xtabs(~invitro.invivo +effect,aoc)
vivofinal<- data.frame(cbind(vivof, study_v))%>% 
  rename(Endpoints='Freq.1')%>%
  rename(category='invitro.invivo')%>%
  mutate(logEndpoints = log(Endpoints))%>%
  rename(Percent = Freq)#renames column

routedf<-rowPerc(xtabs(~exposure.route +effect, aoc))
routef<-as.data.frame(routedf)%>%
  filter(effect %in% c("Yes","No"))%>%
  rename(Type= "exposure.route")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Exposure.route")%>%
  mutate(Type = case_when(
    Type == "coparental.exposure" ~"Co-Parental Exposure",
    Type == "paternal.exposure" ~ "Paternal Exposure",
    Type == "maternal.exposure" ~ "Maternal Exposure",
    Type == "food" ~ "Food",
    Type == "water" ~ "Water",
    Type == "sediment" ~ "Sediment",
    Type == "media" ~ "Media"))
study_r<-xtabs(~exposure.route +effect,aoc)
routefinal<- data.frame(cbind(routef, study_r))%>% 
  rename(Endpoints='Freq.1')%>%
  rename(category='exposure.route')%>%
  mutate(logEndpoints = log(Endpoints))%>%
  rename(Percent = Freq)#renames column
  
  
#### Exploration AO Setup ####

aoc_setup <- aoc %>% 
  
  #Factors
  mutate(effect_f = as.factor(effect)) %>% 
  
  mutate(size_f = as.factor(size.category)) %>% 
  
  mutate(shape_f = as.factor(shape)) %>% 
  
  mutate(poly_f = as.factor(polymer)) %>% 
  
  mutate(org_f = as.factor(organism.group)) %>% 
  
  mutate(lvl1_f = as.factor(lvl1)) %>% 
  
  mutate(lvl2_f = as.factor(lvl2)) %>% 
  
  mutate(lvl3_f = as.factor(lvl3)) %>% 
  
  mutate(bio_f = as.factor(bio.org)) %>% 
  
  mutate(vivo_f = as.factor(invitro.invivo)) %>% 
  
  mutate(life_f = as.factor(life.stage)) %>% 
  
  mutate(env_f = as.factor(environment)) %>% 
  
  mutate(weather.biofoul_f = as.factor(weather.biofoul)) %>% 
  
  mutate(species_f = as.factor(paste(genus,species))) %>% 
  
  mutate(dose.mg.L.master.converted.reported = factor(dose.mg.L.master.converted.reported)) %>%
  
  mutate(dose.particles.mL.master.converted.reported = factor(dose.particles.mL.master.converted.reported)) %>% 
  
  mutate(effect.metric = factor(effect.metric)) %>% 
  
  mutate(af.time_noNA = replace_na(af.time, "Unavailable")) %>%
  
  mutate(acute.chronic_f = factor(acute.chronic)) %>%
  
  mutate(tier_zero_tech_f = factor(tech.tier.zero)) %>%
  
  mutate(tier_zero_risk_f = factor(risk.tier.zero)) %>%
  
  mutate(exp_type_f = factor(experiment.type)) %>% 
  
  mutate(max.size.ingest.mm = ifelse(is.na(max.size.ingest.mm), 
                                     10^(0.9341 * log10(body.length.cm) - 1.1200) * 10,  #(Jamm et al 2020 Nature paper)correction for cm to mm
                                     max.size.ingest.mm)) %>%  # if already present, just use that
  mutate(max.size.ingest.um = 1000 * max.size.ingest.mm) %>%  #makes it less confusing below
  #calcualte monodisperse unaligned effect concentration for surface area
  mutate(dose.um2.mL.master = as.numeric(particle.surface.area.um2) * dose.particles.mL.master) %>% 
  #calculate monodisperse unaligned  specific surface area dose
  mutate(dose.um2.ug.mL.master = dose.um2.mL.master / (mass.per.particle.mg / 1000)) #correct mg to ug

#### Ecologically Relevant Metric calculations ####

###function to derive correction factor (CF) from Koelmans et al (equation 2)
CFfnx = function(a, #default alpha from Koelmans et al (2020)
                 x2D, #set detault values to convert ranges to (1-5,000 um) #5mm is upper defuault 
                 x1D, #1 um is lower default size
                 x2M, x1M){
  CF = (x2D^(1-a)-x1D^(1-a))/(x2M^(1-a)-x1M^(1-a)) 
  return(CF)}

#### equations for mu_x_poly (note that there are three depending on certain alphas for limits of equation)
###### if alpha does not equal 2 #####
mux.polyfnx = function(a.x, 
                       x_UL, 
                       x_LL){
  mux.poly = ((1-a.x)/(2-a.x)) * ((x_UL^(2-a.x) - x_LL^(2-a.x))/(x_UL^(1-a.x) - x_LL^(1-a.x)))
  return(mux.poly)}

##### If alpha does equal 2 #####
mux.polyfnx.2 = function(x_UL,x_LL){
  mux.poly = (log(x_UL/x_LL))/(x_LL^(-1) - x_UL^-1)
  return(mux.poly)}

### Calculating max ingestible parameters ###
## function to calcualte min and max ingestible surface area ##
SAfnx = function(a = a, # a = 0.5 * length
                 b = b, # b = 0.5 * width
                 c = c # c = 0.5 * height (note that height is 0.67 * width)
){
  SA = 4*pi*(((a*b)^1.6 + (a*c)^1.6 + (b*c)^1.6) / 3)^(1/1.6)
  return(SA)}

## max ingestible volume ##
volumefnx = function(R, L){
  volume = 0.111667 * pi * R^2 * L^3 #assumes height = 0.67 * Width, and Width:Length ratio is 'R' (compartment-specific)
  return(volume)}

#max ingestible mass
massfnx = function(R, L, p){
  mass = p * #density (g/cm^3)
    0.111667 * pi * R^2 * L^3 # volume (um^3): assumes height = 0.67 * Width, and Width:Length ratio is 'R' (compartment-specific)
    1/1e12 * 1e6 #correction factor
  return(mass)}

#max ingestible specific surface area
SSAfnx = function(sa, #surface area, calcaulted elsewhere
                  m){ #mass, calculated elsewhere
  SSA = sa/m
  return(SSA)}

## parametrization ##
# Define params for correction #
alpha = 2.07 #table s4 for marine surface water. length
x2D_set = 5000 #upper size length (default)
x1D_set = 1 #lower size range (default)
x1M_set = 1 #lower size range for measured

# define parameters for power law coefficients
a.sa = 1.5 #marine surface area power law
a.v = 1.48 #a_V for marine surface water volume
a.m = 1.32 # upper limit fora_m for mass for marine surface water in table S4 
a.ssa = 1.98 # A_SSA for marine surface water

#define additional parameters for calculations based on averages in the environment
R.ave = 0.77 #average width to length ratio for microplastics in marine enviornment
p.ave = 1.10 #average density in marine surface water

# calculate ERM for each species
aoc_ERM_default <- aoc_setup  %>%
   
  # define upper size WIDTH for ingestion (based on average width:length ratio)
  mutate(x2M = case_when(is.na(max.size.ingest.um) ~ (1/R.ave) * x2D_set, #all calculations below occur for length. Width is R.ave * length, so correcting here makes width the max size ingest below
                         (max.size.ingest.um * (1/R.ave)) < x2D_set ~ ((1/R.ave) * max.size.ingest.um),
                         (max.size.ingest.um * (1/R.ave)) > x2D_set ~ (x2D_set * (1/R.ave)))) %>% #set to 10um for upper limit or max size ingest, whichever is smaller
  # calculate effect threshold for particles
  mutate(EC_mono_p.particles.mL = dose.particles.mL.master) %>% 
  mutate(mu.p.mono = 1) %>% #mu_x_mono is always 1 for particles to particles
  mutate(mu.p.poly = mux.polyfnx(a.x = alpha, #alpha for particles
                                 x_UL= x2M, #upper ingestible size limit
                                 x_LL = x1M_set)) %>% 
  # polydisperse effect threshold for particles
  mutate(EC_poly_p.particles.mL = (EC_mono_p.particles.mL * mu.p.mono)/mu.p.poly) %>% 
  #calculate CF_bio for all conversions
  mutate(CF_bio = CFfnx(x1M = x1M_set,#lower size bin
                        x2M = x2M, #upper ingestible
                        x1D = x1D_set, #default
                        x2D = x2D_set,  #default
                        a = alpha)) %>%  
  ## Calculate environmentally relevant effect threshold for particles
  mutate(EC_env_p.particles.mL = EC_poly_p.particles.mL * CF_bio) %>%  #aligned particle effect concentraiton (1-5000 um)
  
  #### Surface area ERM ####
mutate(mu.sa.mono = as.numeric(particle.surface.area.um2)) %>% #define mu_x_mono for alignment to ERM
  #calculate lower ingestible surface area
  mutate(x_LL_sa = SAfnx(a = 0.5 * x1D_set, 
                         b = 0.5 * R.ave * x1D_set, 
                         c = 0.5 * R.ave * 0.67 * x1D_set)) %>%  
  #calculate upper ingestible surface area
  mutate(x_UL_sa = SAfnx(a = 0.5 * x2M, 
                         b = 0.5 * R.ave *x2M, 
                         c = 0.5 * R.ave * 0.67 * x2M)) %>%  
  #calculate mu_x_poly for surface area
  mutate(mu.sa.poly = if(a.sa == 2){mux.polyfnx.2(x_UL_sa, x_LL_sa)} else if (a.sa != 2){mux.polyfnx(a.sa, x_UL_sa, x_LL_sa)}) %>% 
  #calculate polydisperse effect concentration for surface area (particles/mL)
  mutate(EC_poly_sa.particles.mL = (EC_mono_p.particles.mL * mu.sa.mono)/mu.sa.poly) %>%  
  #calculate environmentally realistic effect threshold
  mutate(EC_env_sa.particles.mL = EC_poly_sa.particles.mL * CF_bio) %>% 
  #### volume ERM ####
#define mu_x_mono for alignment to ERM
mutate(mu.v.mono = as.numeric(particle.volume.um3)) %>% 
  #calculate lower ingestible volume 
  mutate(x_LL_v = volumefnx(R = R.ave,
                            L = x1D_set)) %>%
  #calculate maximum ingestible volume 
  mutate(x_UL_v = volumefnx(R = R.ave,
                            L = x2M)) %>%  
  # calculate mu.v.poly
  mutate(mu.v.poly = if(a.v == 2){mux.polyfnx.2(x_UL_v, x_LL_v)} else if (a.v != 2){mux.polyfnx(a.v, x_UL_v, x_LL_v)}) %>% 
  #calculate polydisperse effect concentration for volume (particles/mL)
  mutate(EC_poly_v.particles.mL = (EC_mono_p.particles.mL * mu.v.mono)/mu.v.poly) %>%  
  #calculate environmentally realistic effect threshold
  mutate(EC_env_v.particles.mL = EC_poly_v.particles.mL * CF_bio) %>% 
  
  #### mass ERM ###
  #define mu_x_mono for alignment to ERM (ug)
  mutate(mu.m.mono = mass.per.particle.mg * 1000) %>% 
  #calculate lower ingestible mass
  mutate(x_LL_m = massfnx(R = R.ave, L = x1D_set, p = p.ave)) %>%  
  #calculate upper ingestible mass
  mutate(x_UL_m = massfnx(R = R.ave, L = x2M, p = p.ave)) %>%  
  # calculate mu.m.poly
  mutate(mu.m.poly = if(a.m == 2){mux.polyfnx.2(x_UL_m, x_LL_m)} else if (a.m != 2){mux.polyfnx(a.m, x_UL_m, x_LL_m)}) %>% 
  #calculate polydisperse effect concentration for volume (particles/mL)
  mutate(EC_poly_m.particles.mL = (EC_env_p.particles.mL * mu.m.mono)/mu.m.poly) %>%
  #calculate environmentally realistic effect threshold
  mutate(EC_env_m.particles.mL = EC_poly_m.particles.mL * CF_bio) %>% 
  
  ##### specific surface area ERM ####
mutate(mu.ssa.mono = mu.sa.mono/mu.m.mono) %>% #define mu_x_mono for alignment to ERM (um^2/ug)
  #calculate lower ingestible SSA
  mutate(x_LL_ssa = SSAfnx(sa = x_LL_sa, #surface area
                           m = x_LL_m) #mass
  ) %>% 
  #calculate upper ingestible SSA  (um^2/ug)
  mutate(x_UL_ssa = SSAfnx(sa = x_UL_sa, #surface area
                           m = x_UL_m) #mass
  ) %>% 
  #calculate mu_x_poly for specific surface area
  mutate(mu.ssa.poly = if(a.ssa == 2){mux.polyfnx.2(x_UL_ssa, x_LL_ssa)} else if (a.ssa != 2){mux.polyfnx(a.ssa, x_UL_ssa, x_LL_ssa)}) %>% 
  #calculate polydisperse effect concentration for specific surface area (particles/mL)
  mutate(EC_poly_ssa.particles.mL = (EC_env_p.particles.mL * mu.ssa.mono)/mu.ssa.poly) %>% 
  #calculate environmentally realistic effect threshold
  mutate(EC_env_ssa.particles.mL = EC_poly_ssa.particles.mL * CF_bio)


#### SSD Setup ####

# Master dataset for SSDs
aoc_z <- aoc_setup %>% # start with Heili's altered dataset (no filtration for terrestrial data)
  # environment category data tidying.
  mutate(environment.noNA = replace_na(environment, "Not Reported")) %>% # replaces NA to better relabel.
  mutate(env_f = factor(environment.noNA, levels = c("Marine", "Freshwater", "Terrestrial", "Not Reported"))) 
 
# final cleanup and factoring  

aoc_z$Species <- as.factor(paste(aoc_setup$genus,aoc_setup$species)) #must make value 'Species" (uppercase)
aoc_z$Group <- as.factor(aoc_z$organism.group) #must make value "Group"
aoc_z$Group <- fct_explicit_na(aoc_z$Group) #makes sure that species get counted even if they're missing a group

#### Endpoint Category Setup ####

aoc_endpoint <- aoc_setup %>% 
  group_by(lvl1_f,lvl2_f,lvl3_f,bio_f) %>% 
  summarise()

#### User Interface ####

#custom themes (EXPERIMENTAL, COMMENT OUT UNTIL SHINY UPDATES BUGS)
# light <- bs_theme() # DARK MODE SWITCH
# dark <- bs_theme(bg = "black", fg = "white", primary = "purple") # DARK MODE SWITCH

ui <- dashboardPage(

  dashboardHeader(title = "Toxicity of Microplastics Explorer", titleWidth = 400),
                # #### DARK MODE SWITCH ####
                #   div(class = "custom-control custom-switch",
                #     tags$input(id = "dark_mode", type = "checkbox", class = "custom-control-input",
                #       onclick = HTML("Shiny.setInputValue('dark_mode', document.getElementById('dark_mode').value);")),
                #     tags$label("Dark mode", `for` = "dark_mode", class = "custom-control-label")
                #     ),
                 ####

  dashboardSidebar(width = 175,
                   
                   sidebarMenu(
                     
                     #Logo image
                     br(),
                     tags$img(src="main_logo_drop.png", width = "100%", height = "100%", style = 'display: block; margin-left: auto; margin-right: auto;'),
                     tags$div("Logo created by J.C. Leapman.", align = 'center', style = 'font-size: 10px; display: block; margin-left: auto; margin-right: auto;'), 
                     br(),         
                     #List of tabs
                     menuItem("Welcome", tabName = "Welcome", icon = icon("home")),
                     menuItem("Overview", tabName = "Overview", icon = icon("globe")),
                     menuItem("Search", tabName = "Search", icon = icon("search")),
                     menuItem("Exploration", tabName = "Exploration", icon = icon("chart-bar")),
                     menuItem("SSD", tabName = "SSD", icon = icon("fish")),
                     menuItem("Study Screening", tabName = "Screening", icon = icon("check-circle")),
                     menuItem("Resources", tabName = "Resources", icon = icon("question-circle")),
                     menuItem("Contact", tabName = "Contact", icon = icon("envelope")),
                     br(),
                     br(),
                     #Twitter icon
                     menuItem("Follow Us on Twitter!", href = "https://twitter.com/ToMExApp", icon = icon("twitter")))
  
                   ), #End dashboard sidebar

  dashboardBody(
    
    #extends background color automatically
    tags$head(tags$style(HTML('.content-wrapper { overflow: auto; }'))),
    
    tabItems(
      
#### Welcome UI ####        
      tabItem(tabName = "Welcome",
               
              #Header     
              h1("Welcome to the Toxicity of Microplastics Explorer,",br(),"Aquatic Organisms Database!", align = 'center'),
              br(),
              
              
              box(status = "primary", width = 12,
                    
                    #top right box
                    column(width = 7, 
                    h3("What is the Microplastics Toxicity Database?", align = "center"), 
                    
                    strong(p("This database is a repository for microplastics 
                      toxicity data for the California Microplastics Health Effects Workshop.")), 
                    
                    p("This web application allows users to explore toxicity 
                    data using an intuitive interface while retaining the diversity and complexity inherent 
                    to microplastics. Data is extracted from existing, peer-reviewed manuscripts containing 
                    toxicity data pertaining to microplastics."),
                  
                    p("Use the side panel on the left of the page to navigate to each section. Each section provides different information or data visualization options. 
                      More specific instructions may be found within each section.")),
                  
                    #top left box 
                    column(width = 5, 
                           p(tags$img(src="welcome.png", width = "100%", height = "100%")))),
              
                    
                    #bottom left box  
                    box(status = "primary", width = 12, 
                    h3("Why was the Microplastics Toxicity Database and Web Application created?", align = "center"),
                    
                    p("The database and application tools have been created for use by the participants of the ", a(href = "https://www.sccwrp.org/about/
                      research-areas/additional-research-areas/
                      trash-pollution/microplastics-health-effects-webinar-series/", 'Microplastics Health Effects Workshop', 
                      .noWS = "outside"),".The purpose of this workshop is to identify the most sensitive and biologically critical endpoints associated with microplastics exposure, 
                      prioritize which microplastics characteristics (e.g., size, shape, polymer) that are of greatest biological concern, and identify 
                      critical thresholds for each at which those biological effects become pronounced. Workshop participants will also make reccomendations for future
                      research investments. Workshop findings will be published in a special issue of ", a(href ="https://microplastics.springeropen.com/", 'Microplastics and Nanoplastics', .noOWs = "outside"),". 
                      These findings will be used directly by the state of California to fulfill ", a(href = "https://www.sccwrp.org/about/research-areas/
                      additional-research-areas/trash-pollution/microplastics-health-effects-webinar-series/history-california-microplastics-legislation/", 'legislative mandates', 
                      .noWS = "outside")," regarding the management of microplastics in drinking water and the aquatic environment.")),
                   
                    #bottom right box  
                    box(status = "primary", width = 12, 
                    h3("Contributors", align = "center"), 
                 
                    p(align = "center", a(href = "https://www.sccwrp.org/about/staff/leah-thornton-hampton/", 'Dr. Leah Thornton Hampton'),", Southern California Coastal Water Research Project ", 
                      tags$a(href="https://twitter.com/DrLeahTH", icon("twitter")), tags$a(href="https://github.com/leahth", icon("github"))),
                    p(align = "center", a(href = "https://agency.calepa.ca.gov/staffdirectory/detail.asp?UID=69294&BDO=7&VW=DET&SL=S", 'Dr. Scott Coffin'),", California State Water Resources Control Board", 
                      tags$a(href="https://twitter.com/DrSCoffin", icon("twitter")), tags$a(href="https://github.com/ScottCoffin", icon("github"))),
                    p(align = "center", "Heili Lowman, Southern California Coastal Water Research Project ",
                      tags$a(href="https://twitter.com/heili_lowman", icon("twitter")), tags$a(href="https://github.com/hlowman", icon("github"))), 
                    p(align = "center", a(href = "https://www.sfei.org/users/liz-miller", 'Dr. Ezra Miller'),", Aquatic Science Center"),
                    p(align = "center", a(href = "https://rochmanlab.com/people/", 'Dr. Ludovic Hermabessiere'),", University of Toronto", 
                      tags$a(href="https://twitter.com/HermabessiereL", icon("twitter"))),
                    p(align = "center", a(href = "https://rochmanlab.com/people/", 'Hannah De Frond'),", University of Toronto", 
                        tags$a(href="https://twitter.com/HanDefrond", icon("twitter"))),
                    p(align = "center", "Emily Darin, Southern California Coastal Water Research Project",
                      tags$a(href="https://github.com/EmilyDarin", icon("github"))),
                    p(align = "center", "Syd Kotar, Southern California Coastal Water Research Project"),
                    p(align = "center", "Sarah Khan, Southern California Coastal Water Research Project"),
                    p(align = "center", a(href = "https://www.wur.nl/en/Persons/Bart-prof.dr.-AA-Bart-Koelmans.htm", 'Dr. Bart Koelmans'),", Wageningen University",
                     tags$a(href="https://twitter.com/MicroplasticLab", icon("twitter"))),
                    p(align = "center", a(href = "https://rochmanlab.com/", 'Dr. Chelsea Rochman'),", University of Toronto",
                      tags$a(href="https://twitter.com/ChelseaRochman", icon("twitter"))),
                    p(align = "center", a(href = "https://www.sccwrp.org/about/staff/alvina-mehinto/", 'Dr. Alvina Mehinto'),", Southern California Coastal Water Research Project"), 
                    p(align = "center", a(href = "https://www.sccwrp.org/about/staff/steve-weisberg/", 'Dr. Steve Weisberg'),", Southern California Coastal Water Research Project")), 
                  
                    #Logos with links to organizations
              box(status = "primary", width = 12, align = "center",  
                  splitLayout(align = "center", 
                  tags$a(href="https://www.waterboards.ca.gov", tags$img(src="waterboard.png", width = "100%", height = "100%")),
                  tags$a(href="https://www.sccwrp.org", tags$img(src="sccwrp.png", width = "100%", height = "100%")),
                  tags$a(href="https://www.utoronto.ca", tags$img(src="toronto.png", width = "100%", height = "100%")),
                  tags$a(href="https://www.sfei.org/", tags$img(src="sfei.png", width = "100%", height = "100%")))),
                   
                    
                  ),
                  
#### Overview UI ####

tabItem(tabName = "Overview", 
         
        box(title = "Database Overview", status = "primary", width = 12, collapsible = TRUE,
        
        p("Select tabs below to explore the database. Each bar displays the total number of measured endpoints where a 
          statistically signifcant effect was detected (Y) or where a measurement was made but a significant effect was not detected (N)."),
        
        br(),
        
        fluidRow(
          tabBox(width = 12,
            tabPanel("Organism Group", 
                     plotOutput(outputId = "tax_plot"),
                     ),
            
            tabPanel(div(HTML("<i>In vitro</i> vs <i>In vivo</i>")),
                     plotOutput(outputId = "vivo_plot"),
                     ),
            
            tabPanel("Life Stage",
                     plotOutput(outputId = "life_plot"),
                     ),
            
            tabPanel("Exposure Route",
                     plotOutput(outputId = "exposure_plot"),
                     ),
            
            tabPanel("Polymer Type",
                     plotOutput(outputId = "polymer_plot"),
                     ),
            
            tabPanel("Particle Morphology",
                     plotOutput(outputId = "shape_plot"),
                     ),
            
            tabPanel("Particle Size",
                     plotOutput(outputId = "size_plot"),
                     )),
         
            ), #close fluid row
            ), #close box
        
        box(title = "Biological Endpoint Catgorization", status = "primary", width = 12, collapsible = TRUE,
        
        br(),
        p("This plot displays the categorization of measured endpoints in the database. Nodes correspond to the Broad Endpoint Category (blue), 
        the Specific Endpoint Category (green), Endpoints (pink) and the level of biological organization (purple). Alternatively, the widget 
        below may be used to select endpoints at various Biological Levels of Organization. Click nodes to expand and collapse the plot."),
        br(),
            
        fluidRow(
          
          column(width = 12,
                 
                 column(width = 3,
                        pickerInput(inputId = "bio_check_endpoint", # bio org checklist
                                    label = "Level of Biological Organization",
                                    choices = levels(aoc_endpoint$bio_f),
                                    selected = levels(aoc_endpoint$bio_f),
                                    options = list(`actions-box` = TRUE),
                                    multiple = TRUE)),
          ), #closes out column
          
          column(width = 12,
                 
                 #Go button
                 column(width = 3,
                        actionButton("go_endpoint", "Update Filters", class = "btn-success")), # adds update action button
                 
          ), #closes out column
          
          column(width = 12,
          #collapsible tree plot
          collapsibleTreeOutput("plot", height = "400px"),
          
          ), #closes out column
          
        ), #close fluid row
        ), #close box
      
), #close tab


#### Search UI ####

tabItem(tabName = "Search",
        
         box(title = "Search Database", status = "primary", width = 12,
             
             
             dataTableOutput("databaseDataTable", height = "200px")   
             
             
         ), #close box
        
),#close search tab

#### Study Screening UI ####

tabItem(tabName = "Screening",
        
        box(title = "Data Selection", status = "primary", width = 12, collapsible = TRUE,
            
            p("This plot displays scores from the quality screening exercise developed by", a(href ="https://pubs.acs.org/doi/abs/10.1021/acs.est.0c03057", 'de Ruijter et al. (2020)', .noOWs = "outside"), "with some modification. 
            For more information, including the scoring rubric used, see Resources."),
            
            fluidRow(
              tabBox(width = 12, height = "200px",
                     
                     tabPanel("Data Type",
                              
                     "Only 'Particle Only' data are included in the study screening dataset."          
                              
                     ), #close tabpanel
                     
                     tabPanel("Effect", 
                              
                              #Broad endpoint selection
                              column(width = 4,
                                     pickerInput(inputId = "lvl1_quality", # endpoint checklist
                                                 label = "Broad Endpoint Category:",
                                                 choices = levels(aoc_setup$lvl1_f),
                                                 selected = levels(aoc_setup$lvl1_f),
                                                 options = list(`actions-box` = TRUE), # option to de/select all
                                                 multiple = TRUE)), # allows for multiple inputs
                              
                              #Specific endpoint selection
                              column(width = 4, #Specific endpoint selection
                                     pickerInput(inputId = "lvl2_quality", 
                                                  label = "Specific Endpoint Category:", 
                                                  choices = levels(aoc_setup$lvl2_f),
                                                  selected = levels(aoc_setup$lvl2_f),
                                                  options = list(`actions-box` = TRUE),
                                                  multiple = TRUE)),
                              
                              #Effect y/n selection
                              column(width = 4,
                                     pickerInput(inputId = "effect_quality", 
                                                 label = "Effect:",
                                                 choices = levels(aoc_setup$effect_f),
                                                 selected = levels(aoc_setup$effect_f),
                                                 options = list(`actions-box` = TRUE),
                                                 multiple = TRUE)),
                              
                     ), #close tabpanel
                     
                     tabPanel("Biology", 
                              
                              #organism group selection
                              column(width = 4,
                                     pickerInput(inputId = "organism_quality",
                                                 label = "Organisms:",
                                                 choices = levels(aoc_setup$org_f),
                                                 selected = levels(aoc_setup$org_f),
                                                 options = list(`actions-box` = TRUE),
                                                 multiple = TRUE),
                                     
                                     #environment selection
                                     pickerInput(inputId = "env_quality", 
                                                 label = "Environment:",
                                                 choices = levels(aoc_setup$env_f),
                                                 selected = levels(aoc_setup$env_f),
                                                 options = list(`actions-box` = TRUE),
                                                 multiple = TRUE)),
                              
                              
                             #species selection
                             column(width = 4,
                                    pickerInput(inputId = "species_quality", 
                                                label = "Species:", 
                                                choices = levels(aoc_setup$species_f),
                                                selected = levels(aoc_setup$species_f),
                                                options = list(`actions-box` = TRUE),
                                                multiple = TRUE), 
                                     
                                     #biological organization selection
                                     pickerInput(inputId = "bio_quality", 
                                                 label = "Biological Organization:",
                                                 choices = levels(aoc_setup$bio_f),
                                                 selected = levels(aoc_setup$bio_f),
                                                 options = list(`actions-box` = TRUE),
                                                 multiple = TRUE)),
                              
                              #life stage selection
                              column(width = 4,
                                     pickerInput(inputId = "life_quality", 
                                                 label = "Life Stages:",
                                                 choices = levels(aoc_setup$life_f),
                                                 selected = levels(aoc_setup$life_f),
                                                 options = list(`actions-box` = TRUE),
                                                 multiple = TRUE),     
                                     
                                     #exposure duration
                                     pickerInput(inputId = "acute.chronic_quality", 
                                                 label = "Exposure Duration:",
                                                 choices = levels(aoc_setup$acute.chronic_f),
                                                 selected = levels(aoc_setup$acute.chronic_f),
                                                 options = list(`actions-box` = TRUE),
                                                 multiple = TRUE)),
                              
                     ), #close tabpanel
                     
                     tabPanel("Particles", 
                              
                              #polymer selection
                              column(width = 4,
                                     pickerInput(inputId = "poly_quality", 
                                                 label = "Polymer:",
                                                 choices = levels(aoc_setup$poly_f),
                                                 selected = levels(aoc_setup$poly_f),
                                                 options = list(`actions-box` = TRUE),
                                                 multiple = TRUE)),
                              
                              #shape selection
                              column(width = 4,
                                     pickerInput(inputId = "shape_quality", 
                                                 label = "Shape:",
                                                 choices = levels(aoc_setup$shape_f),
                                                 selected = levels(aoc_setup$shape_f),
                                                 options = list(`actions-box` = TRUE),
                                                 multiple = TRUE)),
                              
                              #size category selection
                              column(width = 4,
                                     pickerInput(inputId = "size_quality", 
                                                 label = "Size Category:",
                                                 choices = levels(aoc_setup$size_f),
                                                 selected = levels(aoc_setup$size_f),
                                                 options = list(`actions-box` = TRUE),
                                                 multiple = TRUE)),
                              
                     ), #close tabpanel
                     
                     tabPanel("Study Screening", 
                              
                              #technical quality selection
                              column(width = 4,
                                     pickerInput(inputId = "tech_tier_zero_quality", 
                                                 label = "Technical Criteria:",
                                                 choices = levels(aoc_setup$tier_zero_tech_f),
                                                 selected = levels(aoc_setup$tier_zero_tech_f),
                                                 options = list(`actions-box` = TRUE),
                                                 multiple = TRUE)),
                              
                              #risk assessment quality selection
                              column(width = 4,
                                     pickerInput(inputId = "risk_tier_zero_quality", 
                                                 label = "Risk Assessment Criteria:",
                                                 choices = levels(aoc_setup$tier_zero_risk_f),
                                                 selected = levels(aoc_setup$tier_zero_risk_f),
                                                 options = list(`actions-box` = TRUE),
                                                 multiple = TRUE)),
                              
                     ) #close tabpanel
                     
              ), #close tab box
            ), #close fluid row
            
            column(width = 3,
                   actionButton("go_quality", "Plot Current Selection", icon("rocket"), style="color: #fff; background-color:  #117a65; border-color:  #0e6655")),
            
            column(width = 3,
                   actionButton("reset_quality", "Reset Filters", icon("redo"), style="color: #fff; background-color: #f39c12; border-color: #d68910")), 
            
        ), #close box

        box(title = "Visualize Data", status = "primary", width = 12, height = "1600px",
            
            p("Use the cursor to zoom and hover over the plot to view additional information about each study. Plot make take several minutes to appear."),
            br(),
            
            plotlyOutput("quality_plot", height = "1500px")  
            
            
        ), #close box
        
), #closes out tab


#### Exploration UI ####
  
tabItem(tabName = "Exploration",
            
         box(title = "Data Selection", status = "primary", width = 12, collapsible = TRUE,
            
             shinyjs::useShinyjs(), # requires package for "reset" button, DO NOT DELETE - make sure to add any new widget to the reset_input in the server
             id = "heili-tab", # adds ID for resetting filters
             
         fluidRow(
           tabBox(width = 12, height = "200px",
             
             tabPanel("Data Type",
                      
                      #Data type selection
                      column(width = 4,
                             pickerInput(inputId = "exp_type_check", 
                             label = "Data Type:",
                             choices = levels(aoc_setup$exp_type_f),
                             selected = levels(aoc_setup$exp_type_f),
                             options = list(`actions-box` = TRUE), 
                             multiple = TRUE)), 
                                 
                      ), #close tabpanel
             
             tabPanel("Effect", 
                      
                      #Broad endpoint selection
                      column(width = 4,
                        pickerInput(inputId = "lvl1_check", # endpoint checklist
                        label = "Broad Endpoint Category:",
                        choices = levels(aoc_setup$lvl1_f),
                        selected = levels(aoc_setup$lvl1_f),
                        options = list(`actions-box` = TRUE), # option to de/select all
                        multiple = TRUE)), # allows for multiple inputs
                      
                      #Specific endpoint selection
                      column(width = 4,
                        pickerInput(inputId = "lvl2_check", 
                        label = "Specific Endpoint Category:", 
                        choices = levels(aoc_setup$lvl2_f),
                        selected = levels(aoc_setup$lvl2_f),
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE)),
                      
                      #Effect y/n selection
                      column(width = 4,
                        pickerInput(inputId = "effect_check", 
                        label = "Effect:",
                        choices = levels(aoc_setup$effect_f),
                        selected = levels(aoc_setup$effect_f),
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE)),
                      
                      ), #close tabpanel
             
             tabPanel("Biology", 
                      
                      #organism group selection
                      column(width = 4,
                        pickerInput(inputId = "organism_check",
                        label = "Organisms:",
                        choices = levels(aoc_setup$org_f),
                        selected = levels(aoc_setup$org_f),
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE),
                      
                      #environment selection
                        pickerInput(inputId = "env_check", 
                        label = "Environment:",
                        choices = levels(aoc_setup$env_f),
                        selected = levels(aoc_setup$env_f),
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE)),
                      
                      #species selection
                      column(width = 4,
                      pickerInput(inputId = "species_check", 
                                  label = "Species:", 
                                  choices = levels(aoc_setup$species_f),
                                  selected = levels(aoc_setup$species_f),
                                  options = list(`actions-box` = TRUE),
                                  multiple = TRUE),
                        
                      #biological organization selection
                        pickerInput(inputId = "bio_check", 
                        label = "Biological Organization:",
                        choices = levels(aoc_setup$bio_f),
                        selected = levels(aoc_setup$bio_f),
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE)),
                        
                      #life stage selection
                      column(width = 4,
                         pickerInput(inputId = "life_check", 
                         label = "Life Stages:",
                         choices = levels(aoc_setup$life_f),
                         selected = levels(aoc_setup$life_f),
                         options = list(`actions-box` = TRUE),
                         multiple = TRUE),     
                      
                      #exposure duration
                             pickerInput(inputId = "acute.chronic_check", 
                             label = "Exposure Duration:",
                             choices = levels(aoc_setup$acute.chronic_f),
                             selected = levels(aoc_setup$acute.chronic_f),
                             options = list(`actions-box` = TRUE),
                             multiple = TRUE)),
                      
                      ), #close tabpanel
             
             tabPanel("Particles", 
                      
                      #polymer selection
                      column(width = 4,
                        pickerInput(inputId = "poly_check", 
                        label = "Polymer:",
                        choices = levels(aoc_setup$poly_f),
                        selected = levels(aoc_setup$poly_f),
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE)),
                        
                      #shape selection
                      column(width = 4,
                        pickerInput(inputId = "shape_check", 
                        label = "Shape:",
                        choices = levels(aoc_setup$shape_f),
                        selected = levels(aoc_setup$shape_f),
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE)),
                      
                      #size category selection
                      column(width = 4,
                        pickerInput(inputId = "size_check", 
                        label = "Size Category:",
                        choices = levels(aoc_setup$size_f),
                        selected = levels(aoc_setup$size_f),
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE)),

                      ), #close tabpanel
           
             tabPanel("Study Screening", 
                      
                      #technical quality selection
                      column(width = 4,
                        pickerInput(inputId = "tech_tier_zero_check", 
                        label = "Technical Criteria:",
                        choices = levels(aoc_setup$tier_zero_tech_f),
                        selected = levels(aoc_setup$tier_zero_tech_f),
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE)),
                      
                      #risk assessment quality selection
                      column(width = 4,
                            pickerInput(inputId = "risk_tier_zero_check", 
                            label = "Risk Assessment Criteria:",
                            choices = levels(aoc_setup$tier_zero_risk_f),
                            selected = levels(aoc_setup$tier_zero_risk_f),
                            options = list(`actions-box` = TRUE),
                            multiple = TRUE)),
                      
                      ), #close tabpanel
             
             tabPanel("Dose Metric",
                      
                       column(width = 4,

                              radioButtons(inputId = "dose_check", 
                              label = "Dose Metric:",
                              choices = c("Particles/mL", "mg/L", "µm3/mL"),
                              selected = "mg/L")),
                      
                       column(width = 8,

                              radioButtons(inputId = "Rep_Con_rad",
                              label = "Do you want to use just the reported, just the converted, or all exposure concentrations?",
                              choices = c("reported", "converted", "all"),
                              selected = "all")),
                      
                      p(strong("Advanced Option:"),"Select 'particles/mL' for the option to align data as described in", a(href = "https://pubs.acs.org/doi/abs/10.1021/acs.est.0c02982",'Koelmans et al. 2020'), 
                        br(),
                        "Continue with default settings if this option is not desired."),
                      
                      #Ecologically Relevant Metric Row
                      column(width = 12,
                             
                      conditionalPanel(condition = "input.dose_check == 'Particles/mL'",
                              br(),
                              h4("Ecologically Relevant Metric Alignment"),
                              p("A monodisperse effect concentration (e.g. 5 micron spheres) may be re-scaled to a default size range (e.g. 1 - 5,000 microns) using methods described in Kooi et al (2021). Re-scaling to a default size range allows direct comparison to exposure concentrations for a default size range (which may also be re-scaled). The following radio buttons apply corrections for bioavailability (i.e. limiting available particles to max ingestable size), and a further correction for the ecologically relevant metric (ERM). For a given ERM, the threshold may be related to both mono- or polydisperse particles interchangeably so long as the total magnitude of ERM remains the same (Koelmans et al, 2020). If, for example, 'volume' is chosen below as an ERM, the monodisperse effect concentration is first corrected for bioavailability and aligned to whichever default size range the user chooses below. This aligned threshold (in particles/mL) is then multiplied by a correction for polydisperse volume based on the average volumes for the given range of microplastics in the environment.",
                                strong("This analysis will only include 'Particle Only' data.")),
                              
                              #ERM Checkbox
                              column(width = 3,
                                     radioButtons(inputId = "ERM_check", # ERM (particle, surface area, mass, volume, specific surface area)
                                                  label = "Ecologically Relevant Metric:",
                                                  choices = c("Unaligned","Particles", "Surface Area", "Volume", "Mass", "Specific Surface Area"),
                                                  selected = "Unaligned")),
                              #Alpha checkbox
                              # column(width = 3,
                              #        numericInput(inputId = "alpha_exploration",
                              #                     label = "Length Alpha Value (default of 2.07 for marine surface water)",
                              #                     value = 2.07)),
                              
                              # lower length input
                              column(width = 3,
                                     numericInput(inputId = "lower_length_exploration",
                                                  label = "Lower Length for Default Size Range (µm)",
                                                  value = 1)),
                              # upper length input
                              column(width = 3,
                                     numericInput(inputId = "upper_length_exploration",
                                                  label = "Upper Length for Default Size Range (µm)",
                                                  value = 5000)),
                                       
                      )), #end of conditional Panel
                      
                      ), #close tabpanel
             
             tabPanel("Aesthetics", 
                      
                       column(width = 4,
                            selectInput(inputId = "plot.type", "Plot Type:",
                            list(boxplot = "boxplot", violin = "violin", beeswarm = "beeswarm")),
                            checkboxInput(inputId = "show.points", "Show All Points", FALSE)),
                      
                      column(width = 4,
                             selectInput(inputId = "theme.type_exp", "Dark or Light Mode:",
                             list(light = "light", dark = "dark"))),
                             
                      column(width = 4,
                      selectInput(inputId = "color.type_exp", "Color Theme:",
                      list(default = "default", viridis = "viridis", brewer = "brewer", tron = "tron", locusZoom = "locusZoom", d3 = "d3", Nature = "Nature", JAMA = "JAMA"))),
                         
                      ) #close tabpanel

         ), #close tab box
         ), #close fluid row
         
         column(width = 3,
                actionButton("go", "Plot Current Selection", icon("rocket"), style="color: #fff; background-color:  #117a65; border-color:  #0e6655")),
         
         column(width = 3,
                actionButton("reset_input", "Reset Filters", icon("redo"), style="color: #fff; background-color: #f39c12; border-color: #d68910")), 
         
         column(width = 3,
                downloadButton("downloadData", "Download Data (Excel File)", icon("download"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
         
         ), #close box
        
        box(title = "Data Visualization", status = "primary", width = 12,
            
            fluidRow(
              tabBox(width = 12, height = "700px",
                     
                     tabPanel("Organism Group",
                        column(width = 12,      
                        plotOutput(outputId = "organism_plot_react", height = "600px")),
                        
                        column(width = 3,
                               downloadButton("downloadexploration_org", "Download Plot", icon("download"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                     
                     ),#closes tab panel
                     
                     tabPanel("Broad Endpoint Category",
                        column(width = 12,      
                        plotOutput(outputId = "lvl_plot_react", height = "600px")),
                              
                     ),#closes tab panel
                     
                     tabPanel("Specific Endpoint Category", 
                        column(width = 12,      
                        plotOutput(outputId = "lvl2_plot_react", height = "600px")),
                              
                     ),#closes tab panel
                     
                     tabPanel("Size",
                        column(width = 12,      
                        plotOutput(outputId = "size_plot_react", height = "600px")),
                              
                     ),#closes tab panel
                     
                     tabPanel("Shape",
                        column(width = 12,      
                        plotOutput(outputId = "shape_plot_react", height = "600px")),
                              
                     ),#closes tab panel
                     
                     tabPanel("Polymer",
                        column(width = 12,      
                        plotOutput(outputId = "poly_plot_react", height = "600px")),
                              
                     ),#closes tab panel
                     
              p("Data labels on the far right of each plot represent the number of measurements and studies, respectively.")

            ), #closes tab box
            ), #closes fluid tab
            ), #closes box
              
        ), #closes out exploration tab

#### SSD UI ####

tabItem(tabName = "SSD", 
        
        box(title = "Data Selection", status = "primary", width = 12, collapsible = TRUE,
           
            p("Species sensitivity distributions (SSDs) are cumulative probability distributions that estimate the percent of species affected by a given concentration of exposure using Maximum Likelihood and model averaging. A useful metric often used for setting risk-based thresholds is the concentration that affects 5% of the species, the 5% Hazard Concentration (HC). For more information on SSDs, refer to", a(href = "https://bit.ly/2Hy4q10", 'Posthuma, Suter II, and Traas (2001).')),
            br(),
             
            fluidRow(
              tabBox(width = 12, height = "310px",
                     
                     tabPanel("Data Type",
                              
                          #Data type selection
                          column(width = 4,
                                 pickerInput(inputId = "exp_type_check_ssd", 
                                 label = "Data Type:",
                                 choices = levels(aoc_z$exp_type_f),
                                 selected = levels(aoc_z$exp_type_f),
                                 options = list(`actions-box` = TRUE), 
                                 multiple = TRUE)), 
                              
                     ), #close tabpanel  
                     
                     tabPanel("Effect",
                              
                           column(width = 4,
                                  #Broad endpoint category selection
                                  pickerInput(inputId = "lvl1_check_ssd", 
                                  label = "Broad Endpoint Category:",
                                  choices = levels(aoc_z$lvl1_f),
                                  selected = levels(aoc_z$lvl1_f),
                                  options = list(`actions-box` = TRUE), 
                                  multiple = TRUE)),  
                           
                           column(width = 4,
                                  #Specific endpoint category selection
                                  pickerInput(inputId = "lvl2_check_ssd", 
                                  label = "Broad Endpoint Category:",
                                  choices = levels(aoc_z$lvl2_f),
                                  selected = levels(aoc_z$lvl2_f),
                                  options = list(`actions-box` = TRUE), 
                                  multiple = TRUE)), 
                              
                     ), #close tabpanel 
                     
                     tabPanel("Biology",
                              
                           column(width = 4,
                                  #Organism group selection
                                  pickerInput(inputId = "Group_check_ssd", 
                                  label = "Organism Group:",
                                  choices = levels(aoc_z$Group),
                                  selected = levels(aoc_z$Group),
                                  options = list(`actions-box` = TRUE), 
                                  multiple = TRUE),
                                  
                                  #Environment selection
                                  pickerInput(inputId = "env_check_ssd", 
                                  label = "Environment:",
                                  choices = levels(aoc_z$env_f),
                                  selected = levels(aoc_z$env_f),
                                  options = list(`actions-box` = TRUE), 
                                  multiple = TRUE)), 

                           
                           column(width = 4,
                                  #Species selection - reactive to environment
                                  pickerInput(inputId = "Species_check_ssd", 
                                  label = "Species:",
                                  choices = levels(aoc_z$Species),
                                  selected = levels(aoc_z$Species),
                                  options = list(`actions-box` = TRUE), 
                                  multiple = TRUE),

                                  #level of biological organization selection - reactive to environment, group, and species
                                 pickerInput(inputId = "bio_check_ssd", 
                                 label = "Biological Organization:",
                                 choices = levels(aoc_z$bio_f),
                                 selected = levels(aoc_z$bio_f),
                                 options = list(`actions-box` = TRUE), 
                                 multiple = TRUE)),

                           
                           column(width = 4,
                                  #acute/chronic selection
                                  pickerInput(inputId = "acute.chronic_check_ssd", 
                                  label = "Exposure Duration Type:",
                                  choices = levels(aoc_z$acute.chronic_f),
                                  selected = levels(aoc_z$acute.chronic_f),
                                  options = list(`actions-box` = TRUE),
                                  multiple = TRUE)),
                              
                     ), #close tabpanel 
                     
                     tabPanel("Particles",
                              
                          column(width = 4,
                                 #polymer selection
                                 pickerInput(inputId = "poly_check_ssd", 
                                 label = "Biological Organization:",
                                 choices = levels(aoc_z$poly_f),
                                 selected = levels(aoc_z$poly_f),
                                 options = list(`actions-box` = TRUE), 
                                 multiple = TRUE)),
                              
                          column(width = 4,
                                  #shape selection
                                 pickerInput(inputId = "shape_check_ssd", 
                                 label = "Biological Organization:",
                                 choices = levels(aoc_z$shape_f),
                                 selected = levels(aoc_z$shape_f),
                                 options = list(`actions-box` = TRUE), 
                                 multiple = TRUE)), 
                          
                          column(width = 4,
                                  #particle size selection
                                  pickerInput(inputId = "size_check_ssd", 
                                  label = "Size Category:",
                                  choices = levels(aoc_z$size_f),
                                  selected = levels(aoc_z$size_f),
                                  options = list(`actions-box` = TRUE), 
                                  multiple = TRUE)), 
                              
                     ), #close tabpanel 
                     
                     tabPanel("Study Screening",
                              
                          column(width = 4,    
                                  #technical criteria selection
                                  pickerInput(inputId = "tech_tier_zero_check_ssd", 
                                  label = "Technical Criteria:",
                                  choices = levels(aoc_z$tier_zero_tech_f),
                                  selected = levels(aoc_z$tier_zero_tech_f),
                                  options = list(`actions-box` = TRUE),
                                  multiple = TRUE)),
                          
                          column(width = 4,
                                 #technical criteria selection
                                 pickerInput(inputId = "risk_tier_zero_check_ssd", 
                                 label = "Risk Assessment Criteria:",
                                 choices = levels(aoc_z$tier_zero_risk_f),
                                 selected = levels(aoc_z$tier_zero_risk_f),
                                 options = list(`actions-box` = TRUE),
                                 multiple = TRUE)),

                     ), #close tabpanel 
                     
                     tabPanel("Dose Metric",
                              
                          column(width = 4,
                                 radioButtons(inputId = "dose_check_ssd", 
                                 label = "Dose Metric:",
                                 choices = c("Particles/mL", "µg/mL", "µm3/mL", "µm2/mL", "µm2/µg/mL"),
                                 selected = "µg/mL")),
                          
                          column(width = 8,
                                 radioButtons(
                                 inputId = "Reported_Converted_rad",
                                 label = "Do you want to use just the reported, just the converted, or all exposure concentrations?",
                                 choices = list("reported", "converted", "all"),
                                 selected = "all")),
                     ), #close tabPanel
                    
                     tabPanel("Ecologically Relevant Metric Alignment",
                          
                                           p("A monodisperse effect concentration (e.g. 5 micron spheres) may be re-scaled to a default size range (e.g. 1 - 5,000 microns) using methods described in", a(href = "https://www.sciencedirect.com/science/article/pii/S0043135421006278", "Kooi et al (2021)"), "Re-scaling to a default size range allows direct comparison to exposure concentrations for a default size range (which may also be re-scaled). The following radio buttons apply corrections for bioavailability (i.e. limiting available particles to max ingestable size), and a further correction for the ecologically relevant metric (ERM). For a given ERM, the threshold may be related to both mono- or polydisperse particles interchangeably so long as the total magnitude of ERM remains the same (Koelmans et al, 2020). If, for example, 'volume' is chosen below as an ERM, the monodisperse effect concentration is first corrected for bioavailability and aligned to whichever default size range the user chooses below. This aligned threshold (in particles/mL) is then multiplied by a correction for polydisperse volume based on the average volumes for the given range of microplastics in the environment."),
                                           br(),
                                           
                                                  #ERM Checkbox
                                                  column(width = 12,
                                                         radioButtons(inputId = "ERM_check_ssd", # ERM (particle, surface area, mass, volume, specific surface area)
                                                                      label = "Ecologically Relevant Metric:",
                                                                      choices = c("Unaligned","Particles", "Surface Area", "Volume", "Mass", "Specific Surface Area"),
                                                                      selected = "Unaligned")),
                                                  column(width = 12,
                                                  strong("Starting alpha values are for marine surface water reported in (citation here).")),
                                                  br(),
                                           
                                                  #Alpha checkbox
                                                  column(width = 4,
                                                         numericInput(inputId = "alpha_ssd",
                                                                      label = "Length Alpha Value",
                                                                      value = 2.07,
                                                                      step = 0.01)),
                                                  
                                                  #Alpha surface area input
                                                  column(width = 4,
                                                         numericInput(inputId = "a.sa_ssd",
                                                                      label = "Surface Area Alpha Value",
                                                                      value = 1.50,
                                                                      step = 0.01)),
                                                  
                                                  #Alpha volume input
                                                  column(width = 4,
                                                         numericInput(inputId = "a.v_ssd",
                                                                      label = "Volume Alpha Value",
                                                                      value = 1.48,
                                                                      step = 0.01)),
                                                  
                                                  #Alpha mass input
                                                  column(width = 4,
                                                         numericInput(inputId = "a.m_ssd",
                                                                      label = "Mass Alpha Value",
                                                                      value = 1.32,
                                                                      step = 0.01)),
                                                  
                                                  #Alpha ssa input
                                                  column(width = 4,
                                                         numericInput(inputId = "a.ssa_ssd",
                                                                      label = "Specific Surface Area Alpha Value",
                                                                      value = 1.98,
                                                                      step = 0.01)),
                                                  
                                                  #average width to length ratio
                                                  column(width = 4,
                                                         numericInput(inputId = "R.ave_ssd",
                                                                      label = "Average Particle Width to Length Ratio",
                                                                      value = 0.77,
                                                                      step = 0.01)),
                                                  
                                                  #average density
                                                  column(width = 4,
                                                         numericInput(inputId = "p.ave_ssd",
                                                                      label = "Average Particle Density (g/cm^3)",
                                                                      value = 1.10,
                                                                      step = 0.01)),
                                                  
                                                  # lower length input
                                                  column(width = 4,
                                                         numericInput(inputId = "lower_length_ssd",
                                                                      label = "Lower Length for Default Size Range (µm)",
                                                                      value = 1)),
                                                  # upper length input
                                                  column(width = 4,
                                                         numericInput(inputId = "upper_length_ssd",
                                                                      label = "Upper Length for Default Size Range (µm)",
                                                                      value = 5000))
                          
                     ), #close tabpanel  
                     
                     tabPanel("SSD Options (Advanced)",
                          
                          p("The choice of effect metrics (e.g., NOEC, LOEC, HONEC, ECXX and LCXX) should be carefully considered. Assessment factors are available for converting acute exposures to chronic exposure and estimating NOECs from other effect metrics (e.g. LOEC's), according to the methods described in ", a(href = "https://setac.onlinelibrary.wiley.com/doi/epdf/10.1002/ieam.4214", 'Wigger et al (2020).'), "In brief, an assessment factor of 10 is applied to convert LC/EC25-50 to NOEC, 2 to convert EC/LC20, LOEC, or MIC to NOEC. LC10, EC10 and HONEC are considered equivalent to LOEC. An assessment factor of 10 is applied to convert acute-to-chronic, with determinations of such categories dependent on taxa, as defined in the reference."),
                               
                          #Effect metric widget
                          column(width = 6,
                                 pickerInput(inputId = "effect.metric_rad_ssd", 
                                 label = "Effect Metric:",
                                 choices = levels(aoc_z$effect.metric),
                                 selected = c("LOEC", "NOEC"),
                                 options = list(`actions-box` = TRUE),
                                 multiple = TRUE), 
                          
                          #Assessment factor - time
                                 pickerInput(inputId = "AF.time_rad_ssd", 
                                 label = "Apply Assessment Factor for acute and sub-chronic to chronic?",
                                 choices = c("Yes", "No"),
                                 options = list(`actions-box` = TRUE),
                                 selected = "No")),
                          
                          #Assessment factor - noec conversion
                          column(width = 6,
                                 pickerInput(inputId = "AF.noec_rad_ssd", # noec/loc assessment factor
                                 label = "Apply Assessment Factor to convert effect metrics to NOECs?",
                                 choices = c("Yes", "No"),
                                 options = list(`actions-box` = TRUE),
                                 selected = "No"),
                          
                          #concentration selector (minimum, lower 95% CI, median, mean)
                                 pickerInput(
                                 inputId = "conc.select.rad",
                                 label = "What summary statistic should be used for each species?",
                                 choices = list("Minimum", "Lower 95% CI", "1st Quartile", "Median", "Mean", "3rd Quartile", "Upper 95% CI", "Maximum"),
                                 selected = "Mean")),
                              
            ) #close tabpanel  
            ), #closes out tabbox
            ), #closes out fluidrow
            
            column(width = 3,
                   actionButton("SSDgo", "Submit Current Selection", icon("rocket"), style="color: #fff; background-color:  #117a65; border-color:  #0e6655")),
            
        ), #closes out box #1

        box(title = "Selected Data Summary", status = "primary", width = 12, collapsible = TRUE,
        
            fluidRow(
            
            column(width = 12,
            DT::dataTableOutput(outputId = "aoc_filter_ssd_table")),
            
            ), #closes out fluidrow
            
        ), #closes out box #2
        
        box(title = "SSD Results: Plot", status = "primary", width = 12, collapsible = TRUE,
            
            column(width = 3,
                   actionButton("ssdPred", "Predict SSD", icon("rocket"), style="color: #fff; background-color:  #117a65; border-color:  #0e6655")),
                br(),
            
            fluidRow(
            
              column(width = 12,
                
                plotOutput(outputId = "aoc_ssd_ggplot", height = "500px", hover = hoverOpts(id = "plot_hover")), verbatimTextOutput("info"),
                br(),
                # column(width = 12,
                #        column(width = 4,
                #                pickerInput(inputId = "user_width",
                #                            label = "Plot Width (in.)",
                #                            value = 8,
                #                            min = 2,
                #                            max = 18,
                #                            step = 1)),
                #        column(width = 4,
                #               pickerInput(inputId = "user_heigth",
                #                           label = "Plot Height (in.)",
                #                           value =10,
                #                           min = 2,
                #                           max = 18,
                #                           step = 1)),
            
              p("The model-averaged 95% confidence interval is indicated by the shaded band and the model-averaged Hazard Concentration by the dotted line.")),
              br(),  
            
            column(width = 12,
            column(width = 3,
                   selectInput(inputId = "theme.type", "Dark or Light Mode:",
                               list(light = "light", dark = "dark"))),
            
            column(width = 3,
                   selectInput(inputId = "color.type", "Color Theme:",
                               list(viridis = "viridis", brewer = "brewer", tron = "tron", locusZoom = "locusZoom", d3 = "d3", Nature = "Nature", JAMA = "JAMA")))),
            column(width = 12,
            column(width = 3,
                   downloadButton("downloadSsdPlot", "Download Plot", icon("download"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                   
            ),#closes out fluidrow    
            
        ), #closes out box #3
        
        box(title = "SSD Results: Table", status = "primary", width = 12, collapsible = TRUE,
          fluidRow(
            
            column(width = 12,
            DT::dataTableOutput(outputId = "ssd_pred_table", height = "500px")),
            
          ),#closes out fluidrow    
          
        ), #closes out box #4   
            
        box(title = "Model Selections (Advanced)", status = "primary", width = 12, collapsible = TRUE, collapsed = TRUE,
          
            p("The figure below displays minimum observed effect concentrations for a range of species along with three common distributions."),
            br(),
            plotOutput(outputId = "autoplot_dists_react"),
            p("Different distributions can be fit to the data. Below are some common distributions (llogis = log-logistic; lnorm = log-normal; lgumbel = log-Gumbel)."),
            br(),
            DT::dataTableOutput(outputId = "table_gof_react"), #using DT package provides better functionality
            br(),
            p("The best fitting model is that with the smallest Information Criteria value. Note that several informaiton criteria are listed ", a(href ="http://a100.gov.bc.ca/appsdata/acat/documents/r57400/2_1568399094009_8398900200.pdf", 'Schwarz and Tillmanns (2019)', .noOWs = "outside"),"."),
            br(),
            p("Following ", a(href ="https://books.google.com/books?id=c45qtw7tDrsC&lpg=PA113&ots=Zn9Neau5aM&dq=burnham%20and%20anderson%20(2002)%20species%20sensitivity&lr&pg=PA113#v=onepage&q&f=false", 'Burnham and Anderson (2002)', .noOWs = "outside"),", the aicc is recommended for model selection (for which the lowest value is the best fitting model), and is the default information criteria used to predict confidence intervals (unless otherwise specified below). Options inlcude aicc (Akaike's Information Criteria Corrected for sample size; default), aic (Akaike's Information Criteria), or bic (Bayseian Information Criteria). Choose the information criteria used to estimate confidence intervals below:"),
            br(),
            column(width = 4,
                   pickerInput(inputId = "pred_ic_ssd", # prediction model averaging checklist
                               label = "Information Criteria:",
                               choices = c("aicc", "aic", "bic"), #tells the model which information criteria to use to select best fit
                               selected = "aicc",
                               options = list(`actions-box` = FALSE), # option to de/select all
                               multiple = FALSE)),
            br(),
            column(width = 12,
            p("Understanding that other distributions may fit the data almost as well as the 'best' distribution (as evidenced by delta values <2), it is recommended to average such fits based on the relative aicc weights of the distributions (indicated by the weight column in the goodness of fit table) ", a(href ="https://books.google.com/books?id=c45qtw7tDrsC&lpg=PA113&ots=Zn9Neau5aM&dq=burnham%20and%20anderson%20(2002)%20species%20sensitivity&lr&pg=PA113#v=onepage&q&f=false", 'Burnham and Anderson (2002)', .noOWs = "outside"),". Below, choose whether or not multiple distributions should be averaged (weighted according to above table) or if a single distribution should be used.")),
            br(),
            column(width = 4,
                   pickerInput(inputId = "pred_ave_ssd", # prediction model averaging checklist
                               label = "Averaging:",
                               choices = c("TRUE", "FALSE"), #tells the model to average or not
                               selected = NULL,
                               options = list(`actions-box` = FALSE), # option to de/select all
                               multiple = FALSE)),
            br(),
            column(width = 12,
            conditionalPanel("input.pred_ave_ssd == 'FALSE'",
                             p("Choose which distribution will be plotted (llogis = log-logistic; lnorm = log-normal; lgumbel = log-Gumbel):"),
                             pickerInput(inputId = "dist",
                                         label = "Distribution:",
                                         choices = c("weibull", "llogis", "lnorm", "gamma", "lgumbel"),
                                         selected = NULL,
                                         options = list(`actions-box` = FALSE), # option to de/select all
                                         multiple = FALSE))),
            br(),
            
            column(width = 4,
            numericInput(inputId = "pred_hc_ssd", #hazard concentration input
                         label = "Hazard Concentration (%)",
                         value = 5,
                         min = 0.1,
                         step = 1,
                         max = 0.99)),
            
            column(width = 4,
            numericInput(inputId = "nbootInput", #hazard concentration input
                         label = "Bootstrap Iterations (n)",
                         value = 10,
                         min = 10,
                         step = 10,
                         max = 10000)),
            
        ), #closes out box #4
        
        box(title = "Additional Diagnostics (Advanced)", status = "primary", width = 12, collapsible = TRUE, collapsed = TRUE,
        
            fluidRow(
              tabBox(width = 12, height = "650px",
                     
                     tabPanel("Cullen and Frey Graph",
                              
                       column(width = 12,
                              plotOutput(outputId = "ssd_CF_plot", height = "600px")),
                              
                     ), #close tabpanel
            
              tabPanel("Q-Q Plot",
                              
                       column(width = 12,
                              plotOutput(outputId = "ssd_qq_plot", height = "600px")),
                              
                     ), #close tabpanel
              
              tabPanel("P-P Plot",
                       
                       column(width = 12,
                              plotOutput(outputId = "ssd_pp_plot", height = "600px")),
                       
              ), #close tabpanel
              
              tabPanel("Histogram & Theoretical Densities",
                       
                       column(width = 12,
                              plotOutput(outputId = "ssd_dens_plot", height = "600px")),
                       
              ) #close tabpanel
              ), #close tab box
            ), #close fluidrow
           
            h4(align = "center", "Credits"),
            p(align = "center", style = "font-size: 12px;", "This app is built using the R package ", a(href = "https://github.com/bcgov/ssdtools", 'ssdtools', .noWS = "outside"), " version 0.3.2 and share the same functionality."),
            p(align = "center", style = "font-size: 12px;", "Citation: Thorley, J. and Schwarz C., (2018). ssdtools An R package to fit species Sensitivity Distributions. Journal of Open Source Software, 3(31), 1082. https://doi.org/10.21105/joss.01082."),
            
        ), #closes out box #5
        
        ), #closes out SSD tab
  
#### Resources UI ####

tabItem(tabName = "Resources", 
         
        
         box(title = "Resources", width = 6, status = "primary",     
         p(align = "center",a(href = "https://sccwrp-my.sharepoint.com/:b:/g/personal/leahth_sccwrp_org/EYUFX1dOfSdGuHSfrUDcnewBxgttfTCOwom90hrt5nx1FA?e=jFXEyQ", 'Data Category Descriptions')),
         br(),
         p(align = "center",a(href = "https://sccwrp-my.sharepoint.com/:b:/g/personal/leahth_sccwrp_org/EVd-oEZ-xxtJnWdOCC7KHfoBIOO3ByJz7omFoeruD0W6Sw?e=a3weoV", 'Assessment Factor Descriptions')),
         br(),
         p(align = "center",a(href = "https://sccwrp-my.sharepoint.com/:b:/g/personal/leahth_sccwrp_org/EWZha9ygAihPi5sHlOpgR1UBNS9BEoLGZW6TqwCInf0bwg?e=yn8Iim", 'Study Screening Rubric')),
         br(),
         p(align = "center",a(href = "https://sccwrp-my.sharepoint.com/:b:/g/personal/leahth_sccwrp_org/ETy8vDCXe_pAq88Ky0Xob1gBmCdAXYCsEwDFqCfDTL-DNA?e=e7Ic21", 'Aquatic Organisms Study List')),
         br(),
         p(align = "center",a(href = "https://sccwrp-my.sharepoint.com/:b:/g/personal/leahth_sccwrp_org/EXf0crCKDPVHo5xBEdw4PQwBxA8cnu0x4WY477CuEzZcPw?e=qs00V3", 'Dose Conversion Methods'))),
         
        ), #close tab

#### Contact UI ####

tabItem(tabName = "Contact", 
         
        box(title = "Contact", width = 6, status = "primary",
         p("For scientific questions, please contact Dr. Leah Thornton Hampton (leahth@sccwrp.org)."),
         br(),
         p("If you encounter technical problems with the web application, please contact Emily Darin (emilyd@sccwrp.org).")),
         
         )#closes tab

#following three parentheses close out UI. Do not delete. 
        )))   
     

#### Server ####
server <- function (input, output){  #dark mode: #(input, output, session) {

  # # Theme Switch (comment out until Shiny updates)
  # observe({session$setCurrentTheme(
  #   if (isTRUE(input$dark_mode)) dark else light)
  #   })

#### Welcome S ####

  # Welcome does not have any reactive features.
  
#### Overview S ####
  
  #Box #1
  
   output$polymer_plot <- renderPlot({
    
    # generate plot
     ggplot(polyfinal,aes(fill=effect, y= logEndpoints, x= polymer, Percent=Percent)) +
       geom_bar(position="stack", stat="identity") +
       geom_text(aes(label= paste0(Endpoints)), position = position_stack(vjust = 0.5),colour="black", size = 5) +
       scale_fill_manual(values = cal_palette("seagrass"))+
       theme_classic() +
       ylab("Number of Endpoints Measured") +
       labs(fill="Effect") +
       guides(x = guide_axis(angle = 45))+
       theme(text = element_text(size=17), plot.title = element_text(hjust = 0.5, face="bold",size=20))+
       theme(legend.position = "right",
             
             axis.ticks= element_blank(),
             axis.text.x = element_text(),
             axis.text.y = element_blank(),
             axis.title.x = element_blank())
    })
  
   output$vivo_plot <- renderPlot({
     
     # generate plot
     ggplot(vivofinal,aes(fill=effect, y= logEndpoints, x= Type, Percent=Percent)) +
       geom_bar(position="stack", stat="identity") +
       geom_text(aes(label= paste0(Endpoints)), position = position_stack(vjust = 0.5),colour="black", size = 5) +
       scale_fill_manual(values = cal_palette("lupinus"))+
       theme_classic() +
       ylab("Number of Endpoints Measured") +
       labs(fill="Effect") +
       guides(x = guide_axis(angle = 45))+
       theme(text = element_text(size=17),plot.title = element_text(hjust = 0.5, face="bold"))+
       theme(legend.position = "right",
             axis.ticks= element_blank(),
             axis.text.x = element_text(),
             axis.text.y = element_blank(),
             axis.title.x = element_blank())
   })
   
   output$size_plot <- renderPlot({
     
     # generate plot
     ggplot(sizefinal,aes(fill=effect, y= logEndpoints, x= Type, Percent=Percent)) +
       geom_bar(position="stack", stat="identity") +
       geom_text(aes(label= paste0(Endpoints)), position = position_stack(vjust = 0.5),colour="black", size = 5) +
       scale_fill_manual(values = cal_palette("bigsur2"))+
       theme_classic() +
       ylab("Number of Endpoints Measured") +
       labs(fill="Effect") +
       guides(x = guide_axis(angle = 45))+
       theme(text = element_text(size=17),plot.title = element_text(hjust = 0.5, face="bold"))+
       theme(legend.position = "right",
             axis.ticks= element_blank(),
             axis.text.x = element_text(),
             axis.text.y = element_blank(),
             axis.title.x = element_blank())
   })
   
   output$shape_plot <- renderPlot({
     
     # generate plot
     ggplot(shapefinal,aes(fill=effect, y= logEndpoints, x= Type, Percent=Percent)) +
       geom_bar(position="stack", stat="identity") +
       geom_text(aes(label= paste0(Endpoints)), position = position_stack(vjust = 0.5),colour="black", size = 5) +
       scale_fill_manual(values = cal_palette("vermillion"))+
       theme_classic() +
       ylab("Number of Endpoints Measured") +
       labs(fill="Effect") +
       guides(x = guide_axis(angle = 45))+
       theme(text = element_text(size=17),plot.title = element_text(hjust = 0.5, face="bold"))+
       theme(legend.position = "right",
             axis.ticks= element_blank(),
             axis.text.x = element_text(),
             axis.text.y = element_blank(),
             axis.title.x = element_blank())
   })
   
   output$life_plot <- renderPlot({
     
     # generate plot
     ggplot(lifefinal,aes(fill=effect, y= logEndpoints, x= Type, Percent=Percent)) +
       geom_bar(position="stack", stat="identity") +
       geom_text(aes(label= paste0(Endpoints)), position = position_stack(vjust = 0.5),colour="black", size = 5) +
       scale_fill_manual(values = cal_palette("lake"))+
       theme_classic() +
       ylab("Number of Endpoints Measured") +
       labs(fill="Effect") +
       guides(x = guide_axis(angle = 45))+
       theme(text = element_text(size=17),plot.title = element_text(hjust = 0.5, face="bold"))+
       theme(legend.position = "right",
             axis.ticks= element_blank(),
             axis.text.x = element_text(),
             axis.text.y = element_blank(),
             axis.title.x = element_blank())
   })
   
   output$tax_plot <- renderPlot({
     
     # generate plot
     ggplot(taxfinal,aes(fill=effect, y= logEndpoints, x= Type, Percent=Percent)) +
       geom_bar(position="stack", stat="identity") +
       geom_text(aes(label= paste0(Endpoints)), position = position_stack(vjust = 0.5),colour="black", size = 5) +
       scale_fill_manual(values = cal_palette("superbloom2"))+
       theme_classic() +
       ylab("Number of Endpoints Measured") +
       labs(fill="Effect") +
       guides(x = guide_axis(angle = 45))+
       theme(text = element_text(size=17),plot.title = element_text(hjust = 0.5, face="bold"))+
       theme(legend.position = "right",
             axis.ticks= element_blank(),
             axis.text.x = element_text(),
             axis.text.y = element_blank(),
             axis.title.x = element_blank())
   })
   
   output$exposure_plot <- renderPlot({
     
     # generate plot
     ggplot(routefinal,aes(fill=effect, y= logEndpoints, x= Type, Percent=Percent)) +
       geom_bar(position="stack", stat="identity") +
       geom_text(aes(label= paste0(Endpoints)), position = position_stack(vjust = 0.5),colour="black", size = 5) +
       scale_fill_manual(values = cal_palette("wetland"))+
       theme_classic() +
       ylab("Number of Endpoints Measured") +
       labs(fill="Effect") +
       guides(x = guide_axis(angle = 45))+
       theme(text = element_text(size=17),plot.title = element_text(hjust = 0.5, face="bold"))+
       theme(legend.position = "right",
             axis.ticks= element_blank(),
             axis.text.x = element_text(),
             axis.text.y = element_blank(),
             axis.title.x = element_blank())
   })
   
   #Box #2
   
   aoc_filter_endpoint <- eventReactive(list(input$go_endpoint),{
     
     # biological organization widget
     bio_c_endpoint <- input$bio_check_endpoint # assign bio values to "bio_c"
     
     aoc_endpoint %>% # take original dataset
       filter(bio_f %in% bio_c_endpoint) #filter by bio organization
     
   })
   
   output$plot <- renderCollapsibleTree({
     
     collapsibleTree(aoc_filter_endpoint(), root = "Aquatic Organisms Database", hierarchy = c("lvl1_f", "lvl2_f", "lvl3_f", "bio_f"),
                     fontSize = 12, zoomable = FALSE,    
                     fill = c(
                       # The root
                       "seashell",
                       # lvl1
                       rep("turquoise", length(unique(aoc_filter_endpoint()$lvl1_f))),
                       # lvl2
                       rep("palegreen", length(unique(paste(aoc_filter_endpoint()$lvl1_f, aoc_filter_endpoint()$lvl2_f)))),
                       # lvl3
                       rep("hotpink", length(unique(paste(aoc_filter_endpoint()$lvl1_f, aoc_filter_endpoint()$lvl2_f, aoc_filter_endpoint()$lvl3_f)))),
                       # bio org
                       rep("orchid", length(unique(paste(aoc_filter_endpoint()$lvl1_f, aoc_filter_endpoint()$lvl2_f, aoc_filter_endpoint()$lvl3_f, aoc_filter_endpoint()$bio_f))))))
     
   }) 
   

#### Search S ####
   
   output$databaseDataTable <- renderDataTable(
     aoc_setup[,c("doi", "authors", "year","exp_type_f", "env_f", "org_f", "species_f", "life_f", "acute.chronic_f", "size_f", 
                  "size.length.um.used.for.conversions", "shape_f", "poly_f", "weather.biofoul_f", "lvl1_f", "lvl2_f", "lvl3_f", "effect_f")],
     filter = "top",
     extensions = c('Buttons'),
     options = list(
       dom = 'Brtip',
       buttons = list(I('colvis'), c('copy', 'csv', 'excel')),
       scrollY = 400,
       scrollH = TRUE,
       sScrollX = TRUE),
     colnames = c('DOI', 'Authors', 'Year', 'Experiment Type', 'Environment', 'Organism Group', 'Species', 'Life Stage', 'Exposure Duration',
                  'Size Category', 'Mean/Median Particle Size', 'Shape', 'Polymer', 'Weathering/Biofoul', 'Broad Endpoint Category',
                  'Specific Endpoint Category', 'Endpoint', 'Effect'))
   
   #### Screening S ####
   
   #Filter data based on widget selections
   quality_filtered <- eventReactive(list(input$go_quality),{
     
     #Assign every widget selection to a variable
     lvl1_c <- input$lvl1_quality 
     lvl2_c <- input$lvl2_quality 
     bio_c <- input$bio_quality 
     effect_c <- input$effect_quality 
     life_c <- input$life_quality 
     poly_c <- input$poly_quality 
     shape_c <- input$shape_quality 
     size_c <- input$size_quality 
     species_c<-input$species_quality
     env_c<-input$env_quality
     org_c<-input$organism_quality
     acute_chronic_c<-input$acute.chronic_quality
     tech_c<-input$tech_tier_zero_quality
     risk_c<-input$risk_tier_zero_quality
     
     #Create summary data set based on widget filters
     aoc_setup %>%
       filter(lvl1_f %in% lvl1_c) %>% # filter by level inputs
       filter(lvl2_f %in% lvl2_c) %>% #filter by level 2 inputs
       filter(bio_f %in% bio_c) %>% #filter by bio organization
       filter(effect_f %in% effect_c) %>% #filter by effect
       filter(life_f %in% life_c) %>% #filter by life stage
       filter(poly_f %in% poly_c) %>% #filter by polymer
       filter(shape_f %in% shape_c) %>% #filter by shape
       filter(size_f %in% size_c) %>% #filter by size class
       filter(species_f %in% species_c) %>%   #filter by species
       filter(env_f %in% env_c) %>%
       filter(org_f %in% org_c) %>%
       filter(acute.chronic_f %in% acute_chronic_c) %>%
       filter(tier_zero_tech_f %in% tech_c) %>% 
       filter(tier_zero_risk_f %in% risk_c) %>%
       mutate(Study = paste0(authors, " (", year,")")) %>%
       distinct(Study, doi, treatment_range, tech.a1, tech.a2, tech.a3, tech.a4, tech.a5, tech.a6, tech.1, tech.2, tech.3, tech.4, tech.5,
                tech.6, tech.7, tech.8, tech.9, tech.10, tech.11, tech.12, risk.13, risk.14, risk.15, risk.16, risk.17, risk.18, risk.19, risk.20) %>%   
       drop_na() %>% 
       pivot_longer(!c(Study, doi, treatment_range),
                    names_to ="Criteria", 
                    values_to ="Score") %>%
       #Assign descriptions to numerical scores
       mutate(Score_f = factor(case_when(Score == 0 ~ "Inadequate",
                                         Score == 1 ~ "Adequate with Restrictions",
                                         Score == 2 ~ "Adequate"))) %>%
       #Assign each criteria to appropriate category
       mutate(Category = case_when(Criteria == "tech.a1" ~ "Technical Criteria",
                                   Criteria == "tech.a2" ~ "Technical Criteria",
                                   Criteria == "tech.a3" ~ "Technical Criteria",
                                   Criteria == "tech.a4" ~ "Technical Criteria",
                                   Criteria == "tech.a5" ~ "Technical Criteria",
                                   Criteria == "tech.a6" ~ "Technical Criteria",
                                   Criteria == "tech.1" ~ "Technical Criteria",
                                   Criteria == "tech.2" ~ "Technical Criteria",
                                   Criteria == "tech.3" ~ "Technical Criteria",
                                   Criteria == "tech.4" ~ "Technical Criteria",
                                   Criteria == "tech.5" ~ "Technical Criteria",
                                   Criteria == "tech.6" ~ "Technical Criteria",
                                   Criteria == "tech.7" ~ "Technical Criteria",
                                   Criteria == "tech.8" ~ "Technical Criteria",
                                   Criteria == "tech.9" ~ "Technical Criteria",
                                   Criteria == "tech.10" ~ "Technical Criteria",
                                   Criteria == "tech.11" ~ "Technical Criteria",
                                   Criteria == "tech.12" ~ "Technical Criteria",
                                   Criteria == "risk.13" ~ "Risk Assessment",
                                   Criteria == "risk.13" ~ "Risk Assessment",
                                   Criteria == "risk.14" ~ "Risk Assessment",
                                   Criteria == "risk.15" ~ "Risk Assessment",
                                   Criteria == "risk.16" ~ "Risk Assessment",
                                   Criteria == "risk.17" ~ "Risk Assessment",
                                   Criteria == "risk.18" ~ "Risk Assessment",
                                   Criteria == "risk.19" ~ "Risk Assessment",
                                   Criteria == "risk.20" ~ "Risk Assessment")) %>%
       #Set order of categories so they plot in correct order
       mutate(Category_f = factor(Category, levels = c("Technical Criteria", "Risk Assessment"))) %>% 
       #Assign descriptions to each criteria
       mutate(Criteria = case_when(Criteria == "tech.a1" ~ "Test Medium Reported*",
                                   Criteria == "tech.a2" ~ "Administration Route Reported*",
                                   Criteria == "tech.a3" ~ "Test Species Reported*",
                                   Criteria == "tech.a4" ~ "Sample Size Reported*",
                                   Criteria == "tech.a5" ~ "Control Group Reported*",
                                   Criteria == "tech.a6" ~ "Exposure Duration Reported*",
                                   Criteria == "tech.1" ~ "Particle Size*",
                                   Criteria == "tech.2" ~ "Particle Shape*",
                                   Criteria == "tech.3" ~ "Polymer Type*",
                                   Criteria == "tech.4" ~ "Source of Microplastics*",
                                   Criteria == "tech.5" ~ "Data Reporting*",
                                   Criteria == "tech.6" ~ "Chemical Purity",
                                   Criteria == "tech.7" ~ "Laboratory Preparation",
                                   Criteria == "tech.8" ~ "Background Contamination",
                                   Criteria == "tech.9" ~ "Exposure Verification",
                                   Criteria == "tech.10" ~ "Exposure Homogeneity",
                                   Criteria == "tech.11" ~ "Exposure Assessment",
                                   Criteria == "tech.12" ~ "Replication",
                                   Criteria == "risk.13" ~ "Endpoints",
                                   Criteria == "risk.14" ~ "Food Availability",
                                   Criteria == "risk.15" ~ "Effect Thresholds",
                                   Criteria == "risk.16" ~ "Dose Response",
                                   Criteria == "risk.17" ~ "Concentration Range",
                                   Criteria == "risk.18" ~ "Aging and Biofouling",
                                   Criteria == "risk.18" ~ "Risk Assessment",
                                   Criteria == "risk.19" ~ "Microplastic Diversity",
                                   Criteria == "risk.20" ~ "Exposure Time")) %>% 
       #Create factor for criteria and set order - need to be in reverse order here to plot correctly
       mutate(Criteria_f = factor(Criteria, levels = c("Exposure Time", "Microplastic Diversity", "Risk Assessment", "Aging and Biofouling", "Concentration Range", "Dose Response",
                                                       "Effect Thresholds", "Food Availability", "Endpoints", "Replication", "Exposure Assessment", "Exposure Homogeneity",
                                                       "Exposure Verification", "Background Contamination", "Laboratory Preparation","Chemical Purity","Data Reporting*",
                                                       "Source of Microplastics*","Polymer Type*","Particle Shape*","Particle Size*","Exposure Duration Reported*","Control Group Reported*",
                                                       "Sample Size Reported*", "Test Species Reported*", "Administration Route Reported*","Test Medium Reported*"))) 
     
   })
   
   #Create plot for quality screening scores from quality_filtered data
   quality_plotly <- eventReactive(list(input$go_quality),{
     
     quality_filtered() %>%   
       ggplot(aes(Study, Criteria_f)) + 
       geom_tile(aes(fill = Score_f,
                     #Define information for hover over
                     text = paste("Study:", Study, "\n",
                                  "Criteria:", Criteria_f, "\n",
                                  "Category:", Category_f, "\n",
                                  "Score:", Score_f, "\n",
                                  "Number of Treatments", treatment_range, "\n",
                                  "DOI:", paste0(doi), "\n")),
                 color = "white", size = 0.25) +
       theme_ipsum() +
       scale_fill_manual(name = "Score",
                         values = c("dodgerblue4","deepskyblue1","#ebcccd")) +
       labs(title = "Screening & Prioritization Scores (Chemical Effect Data Excluded, Red Criteria Indicated with (*))",
            subtitle = "Red Criteria are indicated with an askterisk") +
       coord_cartesian(clip = "off") + #Keeps labels from disappearing
       theme_minimal(base_size = 14) +
       scale_y_discrete(labels = label_wrap(30)) +
       facet_grid(Category_f ~ ., scales = "free", space = "free") + #Adds criteria category labels
       theme(axis.title.x = element_blank(),
             axis.title.y = element_blank(),
             panel.grid.minor=element_blank(),
             panel.grid.major=element_blank(),
             axis.text.x = element_text(size = 7, angle = 90, vjust = 0.5, hjust = .5),
             plot.title = element_text(hjust = 0.5)) %>% 
       req(nrow(quality_filtered()) > 0) #Suppresses warning message text before submit button is clicked
     
   })
   
   #Render plotly
   output$quality_plot <- renderPlotly({
     ggplotly(quality_plotly(), tooltip = c("text")) %>% 
       layout(legend = list(orientation = "h", #Displays legend horizontally
                            xanchor = "center", #Use the center of the legend as an anchor
                            x = 0.5, #Center the legend on the x axis
                            y = 1.025)) #Places the legend at the top of the plot
   })
   
   # Create "reset" button to revert all filters back to what they began as
   # Need to call all widgets individually by their ids
   # See https://stackoverflow.com/questions/44779775/reset-inputs-with-reactive-app-in-shiny for more information.
   observeEvent(input$reset_quality, {
     shinyjs::reset("lvl1_quality")
     shinyjs::reset("lvl2_quality")
     shinyjs::reset("bio_quality")
     shinyjs::reset("effect_quality")
     shinyjs::reset("life_quality")
     shinyjs::reset("poly_quality")
     shinyjs::reset("shape_quality")
     shinyjs::reset("size_quality")
     shinyjs::reset("species_quality")
     shinyjs::reset("env_quality")
     shinyjs::reset("organism_quality")
     shinyjs::reset("acute.chronic_quality")
     shinyjs::reset("tech_tier_zero_quality")
     shinyjs::reset("risk_tier_zero_quality")
     
   }) 
   
   #### Exploration S ####

  # Create new dataset based on widget filtering and adjusted to reflect the presence of the "update" button.
  aoc_filter <- eventReactive(list(input$go),{
    # eventReactive explicitly delays activity until you press the button
    # use the inputs to create a new dataset that will be fed into the renderPlot calls below

    # every selection widget should be represented as a new variable below
    org_c <- input$organism_check # assign organism input values to "org_c"
    lvl1_c <- input$lvl1_check # assign level values to "lvl1_c"
    lvl2_c <- input$lvl2_check # assign lvl2 values to "lvl2_c"
    bio_c <- input$bio_check # assign bio values to "bio_c"
    effect_c <- input$effect_check # assign effect values to "effect_c"
    life_c <- input$life_check #assign values to "life_check"
    env_c <- input$env_check #assign values to "env_c"
    poly_c <- input$poly_check # assign values to "poly_c"
    shape_c <- input$shape_check # assign values to "shape_c" 
    size_c <- input$size_check # assign values to "size_c"
    species_c <- input$species_check #assign values to "species_c"
    tech_tier_zero_c<-input$tech_tier_zero_check #assign values to "design_tier_zero_c"
    risk_tier_zero_c<-input$risk_tier_zero_check #assign values to "risk_tier_zero_c"
    range_n <- input$range # assign values to "range_n"
    dose_check <- input$dose_check #renames selection from radio button
    ERM_check <- input$ERM_check #chooses aligned dose metric by ecologically relevant metric
    Rep_Con_rad <- input$Rep_Con_rad #use nominal or calculated exposure concentrations. Options are TRUE (calculated) or FALSE (reported)
    acute.chronic.c <- input$acute.chronic_check #acute chronic checkbox
    exp_type_c <- input$exp_type_check #experiment type
  
    ## ERM parametrization ##
    # Define params for correction #
    alpha = 2.07#input$alpha_exploration #table s4 for marine surface water. length
    x2D_set = input$upper_length_exploration #upper size range (default)
    x1D_set = input$lower_length_exploration #lower size range (default)
    x1M_set = 1 #lower size range for ingestible plastic
    
    # define parameters for power law coefficients
    a.sa = 1.5 #marine surface area power law
    a.v = 1.48 #a_V for marine surface water volume
    a.m = 1.32 # upper limit fora_m for mass for marine surface water in table S4 
    a.ssa = 1.98 # A_SSA for marine surface water
    
    #define additional parameters for calculations based on averages in the environment
    R.ave = 0.77 #average width to length ratio for microplastics in marine enviornment
    p.ave = 1.10 #average density in marine surface water
    
    # calculate ERM for each species
    aoc_setup <- aoc_setup %>% 
      #filter the data to only include particle only data
      # dplyr::filter(exp_type_f == "Particle Only") %>%
      # define upper size length for ingestion
      mutate(x2M = max.size.ingest.mm * 1000) %>% #max size ingest in um
      # calculate effect threshold for particles
      mutate(EC_mono_p.particles.mL = dose.particles.mL.master) %>% 
      mutate(mu.p.mono = 1) %>% #mu_x_mono is always 1 for particles to particles
      mutate(mu.p.poly = mux.polyfnx(a.x = alpha, x_UL= x2M, x_LL = x1M_set)) %>% 
      # polydisperse effect threshold for particles
      mutate(EC_poly_p.particles.mL = (EC_mono_p.particles.mL * mu.p.mono)/mu.p.poly) %>% 
      #calculate CF_bio for all conversions
      mutate(CF_bio = CFfnx(x1M = x1M_set, x2M = x2M, x1D = x1D_set, x2D = x2D_set, a = alpha)) %>%  
      ## Calculate environmentally relevant effect threshold for particles
      mutate(EC_env_p.particles.mL = EC_poly_p.particles.mL * CF_bio) %>%  #aligned particle effect concentraiton (1-5000 um)
      #### Surface area ERM ####
    mutate(mu.sa.mono = as.numeric(particle.surface.area.um2)) %>% #define mu_x_mono for alignment to ERM
      #calculate lower ingestible surface area
      mutate(x_LL_sa = SAfnx(a = 0.5 * x1D_set, b = 0.5 * R.ave * x1D_set, c = 0.5 * R.ave * 0.67 * x1D_set)) %>%  
      #calculate upper ingestible surface area
      mutate(x_UL_sa = SAfnx(a = 0.5 * x2M, b = 0.5 * R.ave * x2M, c = 0.5 * R.ave * 0.67 * x2M)) %>%  
      #calculate mu_x_poly for surface area
      mutate(mu.sa.poly = if(a.sa == 2){mux.polyfnx.2(x_UL_sa, x_LL_sa)} else if (a.sa != 2){mux.polyfnx(a.sa, x_UL_sa, x_LL_sa)}) %>% 
      #calculate polydisperse effect concentration for surface area (particles/mL)
      mutate(EC_poly_sa.particles.mL = (EC_mono_p.particles.mL * mu.sa.mono)/mu.sa.poly) %>%  
      #calculate environmentally realistic effect threshold
      mutate(EC_env_sa.particles.mL = EC_poly_sa.particles.mL * CF_bio) %>% 
      #### volume ERM ####
    #define mu_x_mono for alignment to ERM
    mutate(mu.v.mono = as.numeric(particle.volume.um3)) %>% 
      #calculate lower ingestible volume 
      mutate(x_LL_v = volumefnx(R = R.ave, L = x1D_set)) %>%
      #calculate maximum ingestible volume 
      mutate(x_UL_v = volumefnx(R = R.ave,L = x2M)) %>%  
      # calculate mu.v.poly
      mutate(mu.v.poly = if(a.v == 2){mux.polyfnx.2(x_UL_v, x_LL_v)} else if (a.v != 2){mux.polyfnx(a.v, x_UL_v, x_LL_v)}) %>% 
      #calculate polydisperse effect concentration for volume (particles/mL)
      mutate(EC_poly_v.particles.mL = (EC_mono_p.particles.mL * mu.v.mono)/mu.v.poly) %>%  
      #calculate environmentally realistic effect threshold
      mutate(EC_env_v.particles.mL = EC_poly_v.particles.mL * CF_bio) %>% 
      #### mass ERM ###
      #define mu_x_mono for alignment to ERM (ug)
      mutate(mu.m.mono = mass.per.particle.mg * 1000) %>% 
      #calculate lower ingestible mass
      mutate(x_LL_m = massfnx(R = R.ave, L = x1D_set, p = p.ave)) %>%  
      #calculate upper ingestible mass
      mutate(x_UL_m = massfnx(R = R.ave, L = x2M, p = p.ave)) %>%  
      # calculate mu.m.poly
      mutate(mu.m.poly = if(a.m == 2){mux.polyfnx.2(x_UL_m, x_LL_m)} else if (a.m != 2){mux.polyfnx(a.m, x_UL_m, x_LL_m)}) %>% 
      #calculate polydisperse effect concentration for volume (particles/mL)
      mutate(EC_poly_m.particles.mL = (EC_env_p.particles.mL * mu.m.mono)/mu.m.poly) %>%
      #calculate environmentally realistic effect threshold
      mutate(EC_env_m.particles.mL = EC_poly_m.particles.mL * CF_bio) %>% 
      ##### specific surface area ERM ####
    mutate(mu.ssa.mono = mu.sa.mono/mu.m.mono) %>% #define mu_x_mono for alignment to ERM (um^2/ug)
      #calculate lower ingestible SSA
      mutate(x_LL_ssa = SSAfnx(sa = x_LL_sa,m = x_LL_m)) %>% 
      #calculate upper ingestible SSA  (um^2/ug)
      mutate(x_UL_ssa = SSAfnx(sa = x_UL_sa, m = x_UL_m)) %>% 
      #calculate mu_x_poly for specific surface area
      mutate(mu.ssa.poly = if(a.ssa == 2){mux.polyfnx.2(x_UL_ssa, x_LL_ssa)} else if (a.ssa != 2){mux.polyfnx(a.ssa, x_UL_ssa, x_LL_ssa)}) %>% 
      #calculate polydisperse effect concentration for specific surface area (particles/mL)
      mutate(EC_poly_ssa.particles.mL = (EC_env_p.particles.mL * mu.ssa.mono)/mu.ssa.poly) %>% 
      #calculate environmentally realistic effect threshold in particles/mL
      mutate(EC_env_ssa.particles.mL = EC_poly_ssa.particles.mL * CF_bio)
      
      
    
    #filter out reported, calcualted, or all based on checkbox and make new variable based on mg/L or particles/mL
    if(Rep_Con_rad == "reported" & dose_check == "ug/mL"){
      aoc_setup <- aoc_setup %>% 
        filter(dose.mg.L.master.converted.reported == "reported") %>% 
        mutate(dose_new = dose.mg.L.master)}
    
    if(Rep_Con_rad == "converted" & dose_check == "ug/mL"){
      aoc_setup <- aoc_setup %>%
        filter(dose.mg.L.master.converted.reported == "converted") %>% 
        mutate(dose_new = dose.mg.L.master)}
    
    if(Rep_Con_rad == "all" & dose_check == "ug/mL"){
      aoc_setup <- aoc_setup %>%
        mutate(dose_new = dose.mg.L.master)}
    
    #repeat for particles (unaligned)
    if(Rep_Con_rad == "reported" & dose_check == "Particles/mL" & ERM_check == "Unaligned"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>% 
        mutate(dose_new = dose.particles.mL.master)}
    
    if(Rep_Con_rad == "converted" & dose_check == "Particles/mL" & ERM_check == "Unaligned"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>% 
        mutate(dose_new = dose.particles.mL.master)} 
    
    if(Rep_Con_rad == "all" & dose_check == "Particles/mL" & ERM_check == "Unaligned"){
      aoc_setup <- aoc_setup %>%
        mutate(dose_new = dose.particles.mL.master)}
    
    #repeat for volume
    if(Rep_Con_rad == "reported" & dose_check == "um3/mL"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = dose.um3.mL.master)}

    if(Rep_Con_rad == "converted" & dose_check == "um3/mL"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = dose.um3.mL.master)}

    if(Rep_Con_rad == "all" & dose_check == "um3/mL"){
      aoc_setup <- aoc_setup %>%
        mutate(dose_new = dose.um3.mL.master)}
    
    ## ERM ## NOTE: ERM doses reported ONLY in particles/mL
    #repeat for particles with ERM = Particles
    if(Rep_Con_rad == "reported" & dose_check == "Particles/mL" & ERM_check == "Particles"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_p.particles.mL)}
    
    if(Rep_Con_rad == "converted" & dose_check == "Particles/mL" & ERM_check == "Particles"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_p.particles.mL)}
    
    if(Rep_Con_rad == "all" & dose_check == "Particles/mL" & ERM_check == "Particles"){
      aoc_setup <- aoc_setup %>%
        mutate(dose_new = EC_env_p.particles.mL)}
    
    #repeat for particles with ERM = Surface Area
    if(Rep_Con_rad == "reported" & dose_check == "Particles/mL" & ERM_check == "Surface Area"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new =EC_env_sa.particles.mL)}
    
    if(Rep_Con_rad == "converted" & dose_check == "Particles/mL" & ERM_check == "Surface Area"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_sa.particles.mL)}
    
    if(Rep_Con_rad == "all" & dose_check == "Particles/mL" & ERM_check == "Surface Area"){
      aoc_setup <- aoc_setup %>%
        mutate(dose_new = EC_env_sa.particles.mL)}
    
    #repeat for particles with ERM = Volume
    if(Rep_Con_rad == "reported" & dose_check == "Particles/mL" & ERM_check == "Volume"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_v.particles.mL)}
    
    if(Rep_Con_rad == "converted" & dose_check == "Particles/mL" & ERM_check == "Volume"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_v.particles.mL)}
    
    if(Rep_Con_rad == "all" & dose_check == "Particles/mL" & ERM_check == "Volume"){
      aoc_setup <- aoc_setup %>%
        mutate(dose_new = EC_env_v.particles.mL)}
    
    #repeat for particles with ERM = Mass
    if(Rep_Con_rad == "reported" & dose_check == "Particles/mL" & ERM_check == "Mass"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_m.particles.mL)}
    
    if(Rep_Con_rad == "converted" & dose_check == "Particles/mL" & ERM_check == "Mass"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_m.particles.mL)}
    
    if(Rep_Con_rad == "all" & dose_check == "Particles/mL" & ERM_check == "Mass"){
      aoc_setup <- aoc_setup %>%
        mutate(dose_new = EC_env_m.particles.mL)}
    
    #repeat for particles with ERM = Specific Surface Area
    if(Rep_Con_rad == "reported" & dose_check == "Particles/mL" & ERM_check == "Specific Surface Area"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_ssa.particles.mL)}
    
    if(Rep_Con_rad == "converted" & dose_check == "Particles/mL" & ERM_check == "Specific Surface Area"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_ssa.particles.mL)}
    
    if(Rep_Con_rad == "all" & dose_check == "Particles/mL" & ERM_check == "Specific Surface Area"){
      aoc_setup <- aoc_setup %>%
        mutate(dose_new = EC_env_ssa.particles.mL)}
  
    # new dataset based on filtering
    aoc_setup %>% # take original dataset
      filter(exp_type_f %in% exp_type_c) %>% #filter by experiment type
      filter(org_f %in% org_c) %>% # filter by organism inputs
      filter(lvl1_f %in% lvl1_c) %>% # filter by level inputs
      filter(lvl2_f %in% lvl2_c) %>% #filter by level 2 inputs 
      filter(bio_f %in% bio_c) %>% #filter by bio organization
      filter(effect_f %in% effect_c) %>% #filter by effect
      filter(life_f %in% life_c) %>% #filter by life stage
      filter(poly_f %in% poly_c) %>% #filter by polymer
      filter(size_f %in% size_c) %>% #filter by size class
      filter(shape_f %in% shape_c) %>% #filter by shape
      filter(species_f %in% species_c) %>%  #filter by species
      filter(env_f %in% env_c) %>% #filter by environment
      filter(acute.chronic_f %in% acute.chronic.c) %>%  #acute/chronic
      filter(tier_zero_tech_f %in% tech_tier_zero_c) %>% #technical quality
      filter(tier_zero_risk_f %in% risk_tier_zero_c)   #risk assessment quality
      #filter(size.length.um.used.for.conversions <= range_n) #For size slider widget - currently commented out
    
  })

#caption ouput       
  output$caption<-renderText({ #rename plot types in UI
    switch(input$plot.type,
           "boxplot" 	= 	"Boxplot",
           "violin" = "Violin Plot",
           "beeswarm" = "Beeswarm",
           "bar" 		=	"Bar graph")
  })
  
  # Use newly created dataset from above to generate plots for size, shape, polymer, and endpoint plots on four different rows.
  
  #Organism plot
  
  organism_plot_react <- reactive({
    
    #plot types
    plot.type<-switch(input$plot.type,
                      "boxplot" 	= geom_boxplot(alpha = 0.8, aes(color = effect_f)),
                      "violin" = geom_violin(alpha = 0.8, aes(color = effect_f)),
                      "beeswarm" = geom_quasirandom(alpha = 0.8, aes(color = effect_f), 
                                                    method = "smiley", groupOnX = FALSE, cex = 2)) #groupOnX specifies groups on y axis
    
    #Theme type
    theme.type<-switch(input$theme.type_exp,
                       "light" 	= theme_classic(),
                       "dark" = dark_theme_bw()) 
    #color selection
    fill.type <- switch(input$color.type_exp,
                        "default" =  scale_fill_manual(values = c("#FD8D3C", "#7F2704")),
                        "viridis" = scale_fill_viridis(discrete = TRUE),
                        "brewer" =  scale_fill_brewer(palette = "Paired"),
                        "tron" = scale_fill_tron(),
                        "locusZoom" = scale_fill_locuszoom(),
                        "d3" = scale_fill_d3(),
                        "Nature" = scale_fill_npg(),
                        "JAMA" = scale_fill_jama())
    #color selection
    color.type <- switch(input$color.type_exp,
                         "default" =  scale_color_manual(values = c("#FD8D3C", "#7F2704")),
                         "viridis" = scale_color_viridis(discrete = TRUE),
                         "brewer" =  scale_color_brewer(palette = "Paired"),
                         "tron" = scale_color_tron(),
                         "locusZoom" = scale_color_locuszoom(),
                         "d3" = scale_color_d3(),
                         "Nature" = scale_color_npg(),
                         "JAMA" = scale_color_jama())
    
    #Mini data set for measurement and study labels
    aoc_org1 <- aoc_filter() %>%
      drop_na(dose_new) %>%
      group_by(org_f, effect_f) %>% # need to include so there's a recognized "y"
      summarize(dose_new = quantile(dose_new, .1), # need for recognized "x"
                measurements = n(),
                studies = n_distinct(article))
   
    p <- ggplot(aoc_filter(), aes(x = dose_new, y = org_f, fill = effect_f)) +
      plot.type + 
      coord_trans(x = "log10") +
      scale_x_continuous(breaks = scales::trans_breaks("log10", function(x) 10^x, n = 10),
                         labels = trans_format("log10", scales::math_format(10^.x))) +
      scale_y_discrete(limits=rev)+
      color.type +
      fill.type +
      geom_text(data = aoc_org1, 
                aes(label = paste("(",measurements,",",studies,")"),
                  y = org_f,
                  x = Inf,
                  hjust = 1),
                position = position_dodge(.9),
                size = 4.5)+
      theme.type +
      theme(text = element_text(size=18), 
            legend.position = "right") +
      labs(x = input$dose_check,
           y = "Organism",
           color = "Effect?",
           fill = "Effect?",
           caption = (input$Rep_Con_rad))%>%
        req(nrow(aoc_filter()) > 0)
    
    if(input$show.points==TRUE & (input$plot.type == "boxplot" || input$plot.type == "violin")){
      p<-p+geom_point(aes(color = effect_f), alpha=0.8, position = 'jitter')
    }
    
    else {
      p
    }
      # print(p)
   
  })
  
  output$organism_plot_react <- renderPlot({
    
    organism_plot_react()
    
  })
  
  # Size Plot
  
  output$size_plot_react <- renderPlot({

    
    #plot types
    plot.type<-switch(input$plot.type,
                      "boxplot" 	= geom_boxplot(alpha = 0.8, aes(color = effect_f)),
                      "violin" = geom_violin(alpha = 0.8, aes(color = effect_f)),
                      "beeswarm" = geom_quasirandom(alpha = 0.8, aes(color = effect_f), 
                                                    method = "smiley", groupOnX = FALSE, cex = 2)) #groupOnX specifies groups on y axis
    
    #Theme type
    theme.type<-switch(input$theme.type_exp,
                       "light" 	= theme_classic(),
                       "dark" = dark_theme_bw()) 
    #color selection
    fill.type <- switch(input$color.type_exp,
                        "default" =  scale_fill_manual(values = c("#A1CAF6", "#4C6FA1")),
                        "viridis" = scale_fill_viridis(discrete = TRUE),
                        "brewer" =  scale_fill_brewer(palette = "Paired"),
                        "tron" = scale_fill_tron(),
                        "locusZoom" = scale_fill_locuszoom(),
                        "d3" = scale_fill_d3(),
                        "Nature" = scale_fill_npg(),
                        "JAMA" = scale_fill_jama())
    #color selection
    color.type <- switch(input$color.type_exp,
                         "default" =  scale_color_manual(values = c("#A1CAF6", "#4C6FA1")),
                         "viridis" = scale_color_viridis(discrete = TRUE),
                         "brewer" =  scale_color_brewer(palette = "Paired"),
                         "tron" = scale_color_tron(),
                         "locusZoom" = scale_color_locuszoom(),
                         "d3" = scale_color_d3(),
                         "Nature" = scale_color_npg(),
                         "JAMA" = scale_color_jama())
    
    #Mini data set for measurement and study labels
    aoc_size1 <- aoc_filter() %>%
      drop_na(dose_new) %>%
      group_by(size_f, effect_f) %>% # need to include so there's a recognized "y"
      summarize(dose_new = quantile(dose_new, .1), # need for recognized "x"
                measurements = n(),
                studies = n_distinct(article))
    
    p <- ggplot(aoc_filter(), aes(x = dose_new, y = size_f, fill = effect_f)) +
      plot.type + 
      coord_trans(x = "log10") +
      scale_x_continuous(breaks = scales::trans_breaks("log10", function(x) 10^x, n = 10),
                         labels = trans_format("log10", scales::math_format(10^.x))) +
      scale_y_discrete(limits=rev)+
      fill.type +  
      color.type + 
      geom_text(data = aoc_size1, 
                aes(label = paste("(",measurements,",",studies,")"),
                    y = size_f,
                    x = Inf,
                    hjust = 1),
                position = position_dodge(.9),
                size = 4.5)+
      theme.type + 
      theme(text = element_text(size=18), 
        legend.position = "right") +
      labs(x = input$dose_check,
        y = "Size",
        color = "Effect?",
        fill = "Effect?",
        caption = (input$Rep_Con_rad))%>%
      req(nrow(aoc_filter()) > 0)

    if(input$show.points==TRUE & (input$plot.type == "boxplot" || input$plot.type == "violin")){
      p<-p+geom_point(aes(color = effect_f), alpha=0.8, position = 'jitter')
    }
    
    else {
      p
    }
    print(p)
    
  })
  
  # Shape Plot
  
  output$shape_plot_react <- renderPlot({
    
    #plot types
    plot.type<-switch(input$plot.type,
                      "boxplot" 	= geom_boxplot(alpha = 0.8, aes(color = effect_f)),
                      "violin" = geom_violin(alpha = 0.8, aes(color = effect_f)),
                      "beeswarm" = geom_quasirandom(alpha = 0.8, aes(color = effect_f), 
                                                    method = "smiley", groupOnX = FALSE, cex = 2)) #groupOnX specifies groups on y axis
    #Theme type
    theme.type<-switch(input$theme.type_exp,
                       "light" 	= theme_classic(),
                       "dark" = dark_theme_bw()) 
    #color selection
    fill.type <- switch(input$color.type_exp,
                        "default" =  scale_fill_manual(values = c("#C7EAE5","#35978F")),
                        "viridis" = scale_fill_viridis(discrete = TRUE),
                        "brewer" =  scale_fill_brewer(palette = "Paired"),
                        "tron" = scale_fill_tron(),
                        "locusZoom" = scale_fill_locuszoom(),
                        "d3" = scale_fill_d3(),
                        "Nature" = scale_fill_npg(),
                        "JAMA" = scale_fill_jama())
    #color selection
    color.type <- switch(input$color.type_exp,
                         "default" =  scale_color_manual(values = c("#C7EAE5","#35978F")),
                         "viridis" = scale_color_viridis(discrete = TRUE),
                         "brewer" =  scale_color_brewer(palette = "Paired"),
                         "tron" = scale_color_tron(),
                         "locusZoom" = scale_color_locuszoom(),
                         "d3" = scale_color_d3(),
                         "Nature" = scale_color_npg(),
                         "JAMA" = scale_color_jama())
    
    #Mini data set for measurement and study labels
    aoc_shape1 <- aoc_filter() %>%
      drop_na(dose_new) %>%
      group_by(shape_f, effect_f) %>% # need to include so there's a recognized "y"
      summarize(dose_new = quantile(dose_new, .1), # need for recognized "x"
                measurements = n(),
                studies = n_distinct(article))
    
    p <- ggplot(aoc_filter(), aes(x = dose_new, y = shape_f, fill = effect_f)) +
      coord_trans(x = "log10") +
      scale_x_continuous(breaks = scales::trans_breaks("log10", function(x) 10^x, n = 10),
                         labels = trans_format("log10", scales::math_format(10^.x))) +
      scale_y_discrete(limits=rev)+
      plot.type + 
      fill.type + 
      color.type + 
      geom_text(data = aoc_shape1, 
                aes(label = paste("(",measurements,",",studies,")"),
                    y = shape_f,
                    x = Inf,
                    hjust = 1),
                position = position_dodge(.9),
                size = 4.5)+
      theme.type + 
      theme(text = element_text(size=18), 
        legend.position = "right") +
      labs(x = input$dose_check,
        y = "Shape",
        color = "Effect?",
        fill = "Effect?",
        caption = (input$Rep_Con_rad))%>%
      req(nrow(aoc_filter()) > 0)
    
    if(input$show.points==TRUE & (input$plot.type == "boxplot" || input$plot.type == "violin")){
      p<-p+geom_point(aes(color = effect_f), alpha=0.8, position = 'jitter')
    }
    
    else {
      p
    }
    print(p)
  })
  
  # Polymer Plot
  
  output$poly_plot_react <- renderPlot({
    
    #plot types
    plot.type<-switch(input$plot.type,
                      "boxplot" 	= geom_boxplot(alpha = 0.8, aes(color = effect_f)),
                      "violin" = geom_violin(alpha = 0.8, aes(color = effect_f)),
                      "beeswarm" = geom_quasirandom(alpha = 0.8, aes(color = effect_f), 
                                                    method = "smiley", groupOnX = FALSE, cex = 2)) #groupOnX specifies groups on y axis
    
    #Theme type
    theme.type<-switch(input$theme.type_exp,
                       "light" 	= theme_classic(),
                       "dark" = dark_theme_bw()) 
    #color selection
    fill.type <- switch(input$color.type_exp,
                        "default" =  scale_fill_manual(values = c("#FAB455", "#A5683C")),
                        "viridis" = scale_fill_viridis(discrete = TRUE),
                        "brewer" =  scale_fill_brewer(palette = "Paired"),
                        "tron" = scale_fill_tron(),
                        "locusZoom" = scale_fill_locuszoom(),
                        "d3" = scale_fill_d3(),
                        "Nature" = scale_fill_npg(),
                        "JAMA" = scale_fill_jama())
    #color selection
    color.type <- switch(input$color.type_exp,
                         "default" =  scale_color_manual(values = c("#FAB455", "#A5683C")),
                         "viridis" = scale_color_viridis(discrete = TRUE),
                         "brewer" =  scale_color_brewer(palette = "Paired"),
                         "tron" = scale_color_tron(),
                         "locusZoom" = scale_color_locuszoom(),
                         "d3" = scale_color_d3(),
                         "Nature" = scale_color_npg(),
                         "JAMA" = scale_color_jama())
    
    #Mini data set for measurement and study labels
    aoc_poly1 <- aoc_filter() %>%
      drop_na(dose_new) %>%
      group_by(poly_f, effect_f) %>% # need to include so there's a recognized "y"
      summarize(dose_new = quantile(dose_new, .1), # need for recognized "x"
                measurements = n(),
                studies = n_distinct(article))
    
    p <- ggplot(aoc_filter(), aes(x = dose_new, y = poly_f, fill = effect_f)) +
      coord_trans(x = "log10") +
      scale_x_continuous(breaks = scales::trans_breaks("log10", function(x) 10^x, n = 10),
                         labels = trans_format("log10", scales::math_format(10^.x))) +
      scale_y_discrete(limits=rev)+
      plot.type + 
      color.type +
      fill.type +
      geom_text(data = aoc_poly1, 
                aes(label = paste("(",measurements,",",studies,")"),
                    y = poly_f,
                    x = Inf,
                    hjust = 1),
                position = position_dodge(.9),
                size = 4.5)+
     theme.type +
      theme(text = element_text(size=18),
        legend.position = "right") +
      labs(x = input$dose_check,
        y = "Polymer",
        color = "Effect?",
        fill = "Effect?",
        caption = (input$Rep_Con_rad))%>%
      req(nrow(aoc_filter()) > 0)
    
    if(input$show.points==TRUE & (input$plot.type == "boxplot" || input$plot.type == "violin")){
      p<-p+geom_point(aes(color = effect_f), alpha=0.8, position = 'jitter')
    }
    
    else {
      p
    }
    print(p)
    
  })
  
  # Endpoint Plot
  
  output$lvl_plot_react <- renderPlot({
    
    #plot types
    plot.type<-switch(input$plot.type,
                      "boxplot" 	= geom_boxplot(alpha = 0.8, aes(color = effect_f)),
                      "violin" = geom_violin(alpha = 0.8, aes(color = effect_f)),
                      "beeswarm" = geom_quasirandom(alpha = 0.8, aes(color = effect_f), 
                                                    method = "smiley", groupOnX = FALSE, cex = 2)) #groupOnX specifies groups on y axis
    
    #Theme type
    theme.type<-switch(input$theme.type_exp,
                       "light" 	= theme_classic(),
                       "dark" = dark_theme_bw()) 
    #color selection
    fill.type <- switch(input$color.type_exp,
                        "default" =  scale_fill_manual(values = c("#A99CD9", "#6C568C")),
                        "viridis" = scale_fill_viridis(discrete = TRUE),
                        "brewer" =  scale_fill_brewer(palette = "Paired"),
                        "tron" = scale_fill_tron(),
                        "locusZoom" = scale_fill_locuszoom(),
                        "d3" = scale_fill_d3(),
                        "Nature" = scale_fill_npg(),
                        "JAMA" = scale_fill_jama())
    #color selection
    color.type <- switch(input$color.type_exp,
                         "default" =  scale_color_manual(values = c("#A99CD9", "#6C568C")),
                         "viridis" = scale_color_viridis(discrete = TRUE),
                         "brewer" =  scale_color_brewer(palette = "Paired"),
                         "tron" = scale_color_tron(),
                         "locusZoom" = scale_color_locuszoom(),
                         "d3" = scale_color_d3(),
                         "Nature" = scale_color_npg(),
                         "JAMA" = scale_color_jama())
    
    #Mini data set for measurement and study labels
    aoc_lvl1_1 <- aoc_filter() %>%
      drop_na(dose_new) %>%
      group_by(lvl1_f, effect_f) %>% # need to include so there's a recognized "y"
      summarize(dose_new = quantile(dose_new, .1), # need for recognized "x"
                measurements = n(),
                studies = n_distinct(article))
    
    p <- ggplot(aoc_filter(), aes(x = dose_new, y = lvl1_f, fill = effect_f)) +
      coord_trans(x = "log10") +
      scale_x_continuous(breaks = scales::trans_breaks("log10", function(x) 10^x, n = 10),
                         labels = trans_format("log10", scales::math_format(10^.x))) +
      scale_y_discrete(limits=rev)+
      plot.type + 
      color.type +
      fill.type +
      geom_text(data = aoc_lvl1_1, 
                aes(label = paste("(",measurements,",",studies,")"),
                    y = lvl1_f,
                    x = Inf,
                    hjust = 1),
                position = position_dodge(.9),
                size = 4.5)+
      theme.type +
      theme(text = element_text(size=18),
        legend.position = "right") +
      labs(x = input$dose_check,
        y = "Endpoint",
        color = "Effect?",
        fill = "Effect?",
        caption = (input$Rep_Con_rad))%>%
      req(nrow(aoc_filter()) > 0)
    
    if(input$show.points==TRUE & (input$plot.type == "boxplot" || input$plot.type == "violin")){
      p<-p+geom_point(aes(color = effect_f), alpha=0.8, position = 'jitter')
    }
    
    else {
      p
    }
    print(p)
    
  })
  
  #Lvl2 Plot 
  
  output$lvl2_plot_react <- renderPlot({
    
    #plot types
    plot.type<-switch(input$plot.type,
                      "boxplot" 	= geom_boxplot(alpha = 0.8, aes(color = effect_f)),
                      "violin" = geom_violin(alpha = 0.8, aes(color = effect_f)),
                      "beeswarm" = geom_quasirandom(alpha = 0.8, aes(color = effect_f), 
                                                    method = "smiley", groupOnX = FALSE, cex = 2)) #groupOnX specifies groups on y axis
    
    #Theme type
    theme.type<-switch(input$theme.type_exp,
                       "light" 	= theme_classic(),
                       "dark" = dark_theme_bw()) 
    #color selection
    fill.type <- switch(input$color.type_exp,
                        "default" =  scale_fill_manual(values = c("#A99CD9", "#6C568C")),
                        "viridis" = scale_fill_viridis(discrete = TRUE),
                        "brewer" =  scale_fill_brewer(palette = "Paired"),
                        "tron" = scale_fill_tron(),
                        "locusZoom" = scale_fill_locuszoom(),
                        "d3" = scale_fill_d3(),
                        "Nature" = scale_fill_npg(),
                        "JAMA" = scale_fill_jama())
    #color selection
    color.type <- switch(input$color.type_exp,
                         "default" =  scale_color_manual(values = c("#A99CD9", "#6C568C")),
                         "viridis" = scale_color_viridis(discrete = TRUE),
                         "brewer" =  scale_color_brewer(palette = "Paired"),
                         "tron" = scale_color_tron(),
                         "locusZoom" = scale_color_locuszoom(),
                         "d3" = scale_color_d3(),
                         "Nature" = scale_color_npg(),
                         "JAMA" = scale_color_jama())
    
    #Mini data set for measurement and study labels
    aoc_lvl2_1 <- aoc_filter() %>%
      drop_na(dose_new) %>%
      group_by(lvl2_f, effect_f) %>% # need to include so there's a recognized "y"
      summarize(dose_new = quantile(dose_new, .1), # need for recognized "x"
                measurements = n(),
                studies = n_distinct(article))
    
  p <- ggplot(aoc_filter(), aes(x = dose_new, y = lvl2_f, fill = effect_f)) +
    coord_trans(x = "log10") +
    scale_x_continuous(breaks = scales::trans_breaks("log10", function(x) 10^x, n = 10),
                       labels = trans_format("log10", scales::math_format(10^.x))) +
    scale_y_discrete(limits=rev)+
    plot.type +
    color.type +
    fill.type +
    geom_text(data = aoc_lvl2_1, 
              aes(label = paste("(",measurements,",",studies,")"),
                  y = lvl2_f,
                  x = Inf,
                  hjust = 1),
              position = position_dodge(.9),
              size = 4.5)+
    theme.type +
      theme(text = element_text(size=18),
            legend.position = "right") +
      labs(x = input$dose_check,
           y = "Specific Endpoint",
           color = "Effect?",
           fill = "Effect?",
           caption = (input$Rep_Con_rad))%>%
      req(nrow(aoc_filter()) > 0)
  
  if(input$show.points==TRUE & (input$plot.type == "boxplot" || input$plot.type == "violin")){
    p<-p+geom_point(aes(color = effect_f), alpha=0.8, position = 'jitter')
  }
  
  else {
    p
  }
  print(p)
 
  },
  
  # Create downloadable png organism group plot
  output$downloadexploration_org <- downloadHandler(
    
    filename = function() {
      paste('Organism_Group', Sys.Date(), '.png', sep='')
    },
    content = function(file) {
      ggsave(file, plot = organism_plot_react(), width = 12, height = 8, device = 'png')
    })
  
  )
  
  # Create downloadable csv of filtered dataset.
  # Removed columns created above so the dataset matches Leah's original dataset.
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(aoc_filter() %>%
          dplyr::select(-c(effect_f, size_f, shape_f, poly_f, org_f, lvl1_f, lvl2_f, bio_f, vivo_f, life_f, env_f)), 
        file, row.names = FALSE)
    }
  )
  
  # Create "reset" button to revert all filters back to what they began as.
  # Need to call all widgets individually by their ids.
  # See https://stackoverflow.com/questions/44779775/reset-inputs-with-reactive-app-in-shiny for more information.
  observeEvent(input$reset_input, {
    shinyjs::reset("exp_type_check")
    shinyjs::reset("lvl1_check")
    shinyjs::reset("lvl2_check")
    shinyjs::reset("poly_check")
    shinyjs::reset("organism_check")
    shinyjs::reset("shape_check")
    shinyjs::reset("env_check")
    shinyjs::reset("effect_check")
    shinyjs::reset("size_check")
    shinyjs::reset("life_check")
    shinyjs::reset("bio_check")
    shinyjs::reset("species_check")
    shinyjs::reset("tech_tier_zero_check")
    shinyjs::reset("risk_tier_zero_check")
  }) #If we add more widgets, make sure they get added here. 

#### SSD S ####

  # #Create dependent dropdown checklists: select Group by environment
  # output$GroupSelection <- renderUI({
  #   
  #   #Assign user inputs to variables for this reactive
  #   env_c_ssd <- input$env_check_ssd #assign environments
  #   
  #   #filter based on user input
  #   aoc_new <- aoc_z %>% # take original dataset
  #     filter(env_f %in% env_c_ssd) %>% #filter by environment inputs
  #     mutate(Group_new = factor(as.character(Group))) # new subset of factors
  #   
  #   pickerInput(inputId = "Group_check_ssd", 
  #               label = "Organism Group:", 
  #               choices = levels(aoc_new$Group_new),
  #               selected = levels(aoc_new$Group_new),
  #               options = list(`actions-box` = TRUE),
  #               multiple = TRUE)})
  # 
  # #Create dependent dropdown checklists: select Species by env and group
  # output$SpeciesSelection <- renderUI({
  #   
  #   #Assign user inputs to variables for this reactive
  #   env_c_ssd <- input$env_check_ssd #assign environments
  #   Group_c_ssd <- input$Group_check_ssd # assign organism input values to "org_c"
  #   
  #   #filter based on user input
  #   aoc_new <- aoc_z %>% # take original dataset
  #     filter(env_f %in% env_c_ssd) %>% #filter by environment inputs
  #     filter(Group %in% Group_c_ssd) %>% # filter by organism inputs
  #     mutate(Species_new = factor(as.character(Species))) # new subset of factors
  #   
  #   pickerInput(inputId = "Species_check_ssd", 
  #               label = "Species:", 
  #               choices = levels(aoc_new$Species_new),
  #               selected = levels(aoc_new$Species_new),
  #               options = list(`actions-box` = TRUE),
  #               multiple = TRUE)})
  # 
  # #Create dependent dropdown checklists: select biological organization by env, group, and species
  # output$BiologicalSelection <- renderUI({
  #   
  #   #Assign user inputs to variables for this reactive
  #   env_c_ssd <- input$env_check_ssd #assign environments
  #   Group_c_ssd <- input$Group_check_ssd # assign organism input values to "org_c"
  #   Species_c_ssd <- input$Species_check_ssd #species select
  #   
  #   #filter based on user input
  #   aoc_new <- aoc_z %>% # take original dataset
  #     filter(env_f %in% env_c_ssd) %>% #filter by environment inputs
  #     filter(Group %in% Group_c_ssd) %>% # filter by organism inputs
  #     filter(Species %in% Species_c_ssd) %>% # filter by organism inputs
  #     mutate(bio_f_new = factor(as.character(bio_f))) # new subset of factors
  #   
  #   pickerInput(inputId = "bio_check_ssd", 
  #               label = "Biological Organization:", 
  #               choices = levels(aoc_new$bio_f_new),
  #               selected = levels(aoc_new$bio_f_new),
  #               options = list(`actions-box` = TRUE),
  #               multiple = TRUE)})
  # 
  # #Create dependent dropdown checklists: select shape by above input
  # output$shapeSelection <- renderUI({
  #   #Assign user inputs to variables for this reactive
  #   exp_type_c_ssd <- input$exp_type_check_ssd
  #   env_c_ssd <- input$env_check_ssd #assign environments
  #   Group_c_ssd <- input$Group_check_ssd # assign organism input values to "org_c"
  #   Species_c_ssd <- input$Species_check_ssd #assign species input
  #   size_c_ssd <- input$size_check_ssd #assign sizes input
  #   poly_c_ssd <- input$poly_check_ssd #assign polymer input
  #   bio_c_ssd <- input$bio_check_ssd #assign bio org input
  #   tech_tier_zero_c_ssd<-input$tech_tier_zero_check_ssd #assign values to "design_tier_zero_c"
  #   risk_tier_zero_c_ssd<-input$risk_tier_zero_check_ssd #assign values to "risk_tier_zero_c"
  #   
  #   #filter based on user input
  #   aoc_new <- aoc_z %>% # take original dataset
  #     filter(exp_type_f %in% exp_type_c_ssd) %>% 
  #     filter(env_f %in% env_c_ssd) %>% #filter by environment inputs
  #     filter(Group %in% Group_c_ssd) %>% # filter by organism inputs
  #     filter(Species %in% Species_c_ssd) %>% #filter by species inputs
  #     filter(size_f %in% size_c_ssd) %>% #filter by size inputs
  #     filter(poly_f %in% poly_c_ssd) %>%  # filter polymers from other checkbox
  #     filter(bio_f %in% bio_c_ssd) %>%  # filter bio org from other checkbox
  #     filter(tier_zero_tech_f %in% tech_tier_zero_c_ssd) %>% #technical quality
  #     filter(tier_zero_risk_f %in% risk_tier_zero_c_ssd) %>%  #risk assessment quality
  #   mutate(shape_f_new = factor(as.character(shape_f))) # new subset of factors
  #   
  #   #populate picker choices based on available factors
  #   pickerInput(inputId = "shape_check_ssd", 
  #               label = "Shape:", 
  #               choices = levels(aoc_new$shape_f_new),
  #               selected = levels(aoc_new$shape_f_new),
  #               options = list(`actions-box` = TRUE),
  #               multiple = TRUE)})
  # 
  # 
  # #Create dependent dropdown checklists: select lvl1 by above input
  # output$lvl1Selection <- renderUI({
  #   #Assign user inputs to variables for this reactive
  #   exp_type_c_ssd <- input$exp_type_check_ssd
  #   env_c_ssd <- input$env_check_ssd #assign environments
  #   Group_c_ssd <- input$Group_check_ssd # assign organism input values to "org_c"
  #   Species_c_ssd <- input$Species_check_ssd #assign species input
  #   size_c_ssd <- input$size_check_ssd #assign sizes input
  #   bio_c_ssd <- input$bio_check_ssd #assign bio org input
  #   tech_tier_zero_c_ssd<-input$tech_tier_zero_check_ssd #assign values to "design_tier_zero_c"
  #   risk_tier_zero_c_ssd<-input$risk_tier_zero_check_ssd #assign values to "risk_tier_zero_c"
  #   
  #   #filter based on user input
  #   aoc_new <- aoc_z %>% # take original dataset
  #     filter(exp_type_f %in% exp_type_c_ssd) %>%
  #     filter(env_f %in% env_c_ssd) %>% #filter by environment inputs
  #     filter(Group %in% Group_c_ssd) %>% # filter by organism inputs
  #     filter(Species %in% Species_c_ssd) %>% #filter by species inputs
  #     filter(size_f %in% size_c_ssd) %>% #filter by size inputs
  #     filter(bio_f %in% bio_c_ssd) %>%  # filter bio org from other checkbox
  #     filter(tier_zero_tech_f %in% tech_tier_zero_c_ssd) %>% #technical quality
  #     filter(tier_zero_risk_f %in% risk_tier_zero_c_ssd) %>%  #risk assessment quality
  #     mutate(lvl1_f_new = factor(as.character(lvl1_f))) # new subset of factors
  #   #populate picker choices based on available factors
  #   pickerInput(inputId = "lvl1_check_ssd", 
  #               label = "Broad Endpoint Category:", 
  #               choices = levels(aoc_new$lvl1_f_new),
  #               selected = levels(aoc_new$lvl1_f_new),
  #               options = list(`actions-box` = TRUE),
  #               multiple = TRUE)})
  # 
  # #Create dependent dropdown checklists: select lvl2 by lvl1 input and Species
  # output$lvl2Selection <- renderUI({
  #   #Assign user inputs to variables for this reactive
  #   exp_type_c_ssd <- input$exp_type_check_ssd
  #   lvl1_c_ssd <- input$lvl1_check_ssd #assign endpoints
  #   Species_c_ssd <- input$Species_check_ssd #assign species input
  #   size_c_ssd <- input$size_check_ssd #assign sizes input
  #   bio_c_ssd <- input$bio_check_ssd #assign bio org input
  #   tech_tier_zero_c_ssd<-input$tech_tier_zero_check_ssd #assign values to "design_tier_zero_c"
  #   risk_tier_zero_c_ssd<-input$risk_tier_zero_check_ssd #assign values to "risk_tier_zero_c"
  #   
  #   #filter based on user input
  #   aoc_new <- aoc_z %>% # take original dataset
  #     filter(exp_type_f %in% exp_type_c_ssd) %>%
  #     filter(Species %in% Species_c_ssd) %>% #filter by species inputs
  #     filter(lvl1_f %in% lvl1_c_ssd) %>% # filter by level inputs
  #     filter(size_f %in% size_c_ssd) %>% #filter by size inputs
  #     filter(bio_f %in% bio_c_ssd) %>%  # filter bio org from other checkbox
  #     filter(tier_zero_tech_f %in% tech_tier_zero_c_ssd) %>% #technical quality
  #     filter(tier_zero_risk_f %in% risk_tier_zero_c_ssd) %>%  #risk assessment quality
  #     mutate(lvl2_f_new = factor(as.character(lvl2_f))) # new subset of factors
  #   #populate picker choices based on available factors
  #   pickerInput(inputId = "lvl2_check_ssd", 
  #               label = "Specific Endpoint Category:", 
  #               choices = levels(aoc_new$lvl2_f_new),
  #               selected = levels(aoc_new$lvl2_f_new),
  #               options = list(`actions-box` = TRUE),
  #               multiple = TRUE)})
  # 
  # #Create dependent dropdown checklists: select lvl2 by all other input
  # output$polySelection <- renderUI({
  #   #Assign user inputs to variables for this reactive
  #   exp_type_c_ssd <- input$exp_type_check_ssd
  #   env_c_ssd <- input$env_check_ssd #assign environments
  #   Group_c_ssd <- input$Group_check_ssd # assign organism input values to "org_c"
  #   Species_c_ssd <- input$Species_check_ssd #assign species input
  #   size_c_ssd <- input$size_check_ssd #assign sizes input
  #   bio_c_ssd <- input$bio_check_ssd #assign bio org input
  #   lvl2_c_ssd <- input$lvl2_check_ssd #assign endpoints
  #   tech_tier_zero_c_ssd<-input$tech_tier_zero_check_ssd #assign values to "design_tier_zero_c"
  #   risk_tier_zero_c_ssd<-input$risk_tier_zero_check_ssd #assign values to "risk_tier_zero_c"
  #   
  #   #filter based on user input
  #   aoc_new <- aoc_z %>% # take original dataset
  #     filter(exp_type_f %in% exp_type_c_ssd) %>%
  #     filter(env_f %in% env_c_ssd) %>% #filter by environment inputs
  #     filter(Group %in% Group_c_ssd) %>% # filter by organism inputs
  #     filter(Species %in% Species_c_ssd) %>% #filter by species inputs
  #     filter(size_f %in% size_c_ssd) %>% #filter by size inputs
  #     filter(bio_f %in% bio_c_ssd) %>%  # filter bio org from other checkbox
  #     filter(lvl2_f %in% lvl2_c_ssd) %>%  #filter by second level endpoints
  #     filter(tier_zero_tech_f %in% tech_tier_zero_c_ssd) %>% #technical quality
  #     filter(tier_zero_risk_f %in% risk_tier_zero_c_ssd) %>%  #risk assessment quality
  #     mutate(poly_f_new = factor(as.character(poly_f))) # new subset of factors
  #   #populate picker choices based on available factors
  #   pickerInput(inputId = "poly_check_ssd", 
  #               label = "Polymer:", 
  #               choices = levels(aoc_new$poly_f_new),
  #               selected = levels(aoc_new$poly_f_new),
  #               options = list(`actions-box` = TRUE),
  #               multiple = TRUE)}) 

    
  # Create new all tested dataset based on widget filtering and adjusted to reflect the presence of the "update" button.
  aoc_z_L <- eventReactive(list(input$SSDgo),{
    # eventReactive explicitly delays activity until you press the button
    # here we'll use the inputs to create a new dataset that will be fed into the renderPlot calls below
    exp_type_c_ssd <- input$exp_type_check_ssd
    env_c_ssd <- input$env_check_ssd #assign environments
    Group_c_ssd <- input$Group_check_ssd # assign organism input values to "org_c"
    Species_c_ssd <- input$Species_check_ssd #assign species input
    size_c_ssd <- input$size_check_ssd #assign sizes input
    lvl1_c_ssd <- input$lvl1_check_ssd #assign broad endpoints
    lvl2_c_ssd <- input$lvl2_check_ssd #assign specific endpoints
    poly_c_ssd <- input$poly_check_ssd #assign polymers
    shape_c_ssd <- input$shape_check_ssd #assign shapes
    bio_c_ssd <- input$bio_check_ssd #assign bio org input
    acute.chronic.c_ssd <- input$acute.chronic_check_ssd #acute chronic checkbox
    AF.time_r_ssd <- input$AF.time_rad_ssd #yes/no apply assessment factor for acute -> chronic
    AF.noec_r_ssd <- input$AF.noec_rad_ssd #yes/no apply assessment factor for LOEC/ECXX -> NOEC
    Rep_Con_rad <- input$Reported_Converted_rad #use nominal or calculated exposure concentrations. Options are TRUE (calculated) or FALSE (reported)
    ERM_check <- input$ERM_check_ssd #ERM
    dose_check <- input$dose_check_ssd #rename variable
    tech_tier_zero_c_ssd<-input$tech_tier_zero_check_ssd #assign values to "design_tier_zero_c"
    risk_tier_zero_c_ssd<-input$risk_tier_zero_check_ssd #assign values to "risk_tier_zero_c"
    
    ## ERM parametrization ##
    # Define params for correction #
    alpha = input$alpha_ssd #2.07 #table s4 for marine surface water. length
    x2D_set = input$upper_length_ssd #upper size range (default)
    x1D_set = input$lower_length_ssd #lower size range (default)
    x1M_set = 1 #lower size range for ingestible plastic
    
    # define parameters for power law coefficients
    a.sa = input$a.sa_ssd #1.5 #marine surface area power law
    a.v = input$a.v_ssd#1.48 #a_V for marine surface water volume
    a.m = input$a.m_ssd#1.32 # upper limit fora_m for mass for marine surface water in table S4 
    a.ssa = input$a.ssa_ssd #1.98 # A_SSA for marine surface water
    
    #define additional parameters for calculations based on averages in the environment
    R.ave = input$R.ave_ssd #0.77 #average width to length ratio for microplastics in marine enviornment
    p.ave = input$p.ave_ssd#1.10 #average density in marine surface water
    
    # calculate ERM for each species
    aoc_z <- aoc_z %>%
      #filter the data to only include particle only data
      # dplyr::filter(exp_type_f == "Particle Only") %>%
      # define upper size WIDTH for ingestion (based on average width:length ratio)
      mutate(x2M = case_when(is.na(max.size.ingest.um) ~ (1/R.ave) * x2D_set, #all calculations below occur for length. Width is R.ave * length, so correcting here makes width the max size ingest below
                             (max.size.ingest.um * (1/R.ave)) < x2D_set ~ ((1/R.ave) * max.size.ingest.um),
                             (max.size.ingest.um * (1/R.ave)) > x2D_set ~ (x2D_set * (1/R.ave)))) %>% #set to 10um for upper limit or max size ingest, whichever is smaller
      # calculate effect threshold for particles
      mutate(EC_mono_p.particles.mL = dose.particles.mL.master) %>% 
      mutate(mu.p.mono = 1) %>% #mu_x_mono is always 1 for particles to particles
      mutate(mu.p.poly = mux.polyfnx(a.x = alpha, x_UL= x2M, x_LL = x1M_set)) %>% 
      # polydisperse effect threshold for particles
      mutate(EC_poly_p.particles.mL = (EC_mono_p.particles.mL * mu.p.mono)/mu.p.poly) %>% 
      #calculate CF_bio for all conversions
      mutate(CF_bio = CFfnx(x1M = x1M_set, x2M = x2M, x1D = x1D_set, x2D = x2D_set, a = alpha)) %>%  
      ## Calculate environmentally relevant effect threshold for particles
      mutate(EC_env_p.particles.mL = EC_poly_p.particles.mL * CF_bio) %>%  #aligned particle effect concentraiton (1-5000 um)
      # Surface area ERM ##
    mutate(mu.sa.mono = as.numeric(particle.surface.area.um2)) %>% #define mu_x_mono for alignment to ERM
      #calculate lower ingestible surface area
      mutate(x_LL_sa = SAfnx(a = 0.5 * x1D_set, b = 0.5 * R.ave * x1D_set, c = 0.5 * R.ave * 0.67 * x1D_set)) %>%  
      #calculate upper ingestible surface area
      mutate(x_UL_sa = SAfnx(a = 0.5 * x2M, b = 0.5 * R.ave * x2M, c = 0.5 * R.ave * 0.67 * x2M)) %>%  
      #calculate mu_x_poly for surface area
      mutate(mu.sa.poly = if(a.sa == 2){mux.polyfnx.2(x_UL_sa, x_LL_sa)} else if (a.sa != 2){mux.polyfnx(a.sa, x_UL_sa, x_LL_sa)}) %>% 
      #calculate polydisperse effect concentration for surface area (particles/mL)
      mutate(EC_poly_sa.particles.mL = (EC_mono_p.particles.mL * mu.sa.mono)/mu.sa.poly) %>%  
      #calculate environmentally realistic effect threshold
      mutate(EC_env_sa.particles.mL = EC_poly_sa.particles.mL * CF_bio) %>% 
      # volume ERM ##
    #define mu_x_mono for alignment to ERM
    mutate(mu.v.mono = as.numeric(particle.volume.um3)) %>% 
      #calculate lower ingestible volume 
      mutate(x_LL_v = volumefnx(R = R.ave, L = x1D_set)) %>%
      #calculate maximum ingestible volume 
      mutate(x_UL_v = volumefnx(R = R.ave,L = x2M)) %>%  
      # calculate mu.v.poly
      mutate(mu.v.poly = if(a.v == 2){mux.polyfnx.2(x_UL_v, x_LL_v)} else if (a.v != 2){mux.polyfnx(a.v, x_UL_v, x_LL_v)}) %>% 
      #calculate polydisperse effect concentration for volume (particles/mL)
      mutate(EC_poly_v.particles.mL = (EC_mono_p.particles.mL * mu.v.mono)/mu.v.poly) %>%  
      #calculate environmentally realistic effect threshold
      mutate(EC_env_v.particles.mL = EC_poly_v.particles.mL * CF_bio) %>% 
      #### mass ERM ###
      #define mu_x_mono for alignment to ERM (ug)
      mutate(mu.m.mono = mass.per.particle.mg * 1000) %>% 
      #calculate lower ingestible mass
      mutate(x_LL_m = massfnx(R = R.ave, L = x1D_set, p = p.ave)) %>%  
      #calculate upper ingestible mass
      mutate(x_UL_m = massfnx(R = R.ave, L = x2M, p = p.ave)) %>%  
      # calculate mu.m.poly
      mutate(mu.m.poly = if(a.m == 2){mux.polyfnx.2(x_UL_m, x_LL_m)} else if (a.m != 2){mux.polyfnx(a.m, x_UL_m, x_LL_m)}) %>% 
      #calculate polydisperse effect concentration for volume (particles/mL)
      mutate(EC_poly_m.particles.mL = (EC_env_p.particles.mL * mu.m.mono)/mu.m.poly) %>%
      #calculate environmentally realistic effect threshold
      mutate(EC_env_m.particles.mL = EC_poly_m.particles.mL * CF_bio) %>% 
      ## specific surface area ERM ##
    mutate(mu.ssa.mono = mu.sa.mono/mu.m.mono) %>% #define mu_x_mono for alignment to ERM (um^2/ug)
      #calculate lower ingestible SSA
      mutate(x_LL_ssa = SSAfnx(sa = x_LL_sa,m = x_LL_m)) %>% 
      #calculate upper ingestible SSA  (um^2/ug)
      mutate(x_UL_ssa = SSAfnx(sa = x_UL_sa, m = x_UL_m)) %>% 
      #calculate mu_x_poly for specific surface area
      mutate(mu.ssa.poly = if(a.ssa == 2){mux.polyfnx.2(x_UL_ssa, x_LL_ssa)} else if (a.ssa != 2){mux.polyfnx(a.ssa, x_UL_ssa, x_LL_ssa)}) %>% 
      #calculate polydisperse effect concentration for specific surface area (particles/mL)
      mutate(EC_poly_ssa.particles.mL = (EC_env_p.particles.mL * mu.ssa.mono)/mu.ssa.poly) %>% 
      #calculate environmentally realistic effect threshold
      mutate(EC_env_ssa.particles.mL = EC_poly_ssa.particles.mL * CF_bio) %>% 
      ### Convert to Metrics other than particles/mL ###
      
      ## convert all environmentally realistic thresholds to surface area ##
      # particle count to surface area #
      mutate(EC_env_p.um2.mL =  EC_env_p.particles.mL * mux.polyfnx(a.x = a.sa, x_UL = x2D_set, x_LL = x1D_set)) %>% 
      # surface area to surface area #
      mutate(EC_env_sa.um2.mL =  EC_env_sa.particles.mL * mux.polyfnx(a.x = a.sa, x_UL = x2D_set, x_LL = x1D_set)) %>% 
      # volume to surface area #
      mutate(EC_env_v.um2.mL =  EC_env_v.particles.mL * mux.polyfnx(a.x = a.sa, x_UL = x2D_set, x_LL = x1D_set)) %>%
      # mass to surface area #
      mutate(EC_env_m.um2.mL =  EC_env_m.particles.mL * mux.polyfnx(a.x = a.sa, x_UL = x2D_set, x_LL = x1D_set)) %>% 
      # specific surface area to surface area #
      mutate(EC_env_ssa.um2.mL =  EC_env_ssa.particles.mL * mux.polyfnx(a.x = a.sa, x_UL = x2D_set, x_LL = x1D_set)) %>% 
      
      ## convert all environmentally realistic thresholds to volume ##
      # particle count to volume #
      mutate(EC_env_p.um3.mL =  EC_env_p.particles.mL * mux.polyfnx(a.x = a.v, x_UL = x2D_set, x_LL = x1D_set)) %>% 
      # surface area to volume #
      mutate(EC_env_sa.um3.mL =  EC_env_sa.particles.mL * mux.polyfnx(a.x = a.v, x_UL = x2D_set, x_LL = x1D_set)) %>% 
      # volume to volume #
      mutate(EC_env_v.um3.mL =  EC_env_v.particles.mL * mux.polyfnx(a.x = a.v, x_UL = x2D_set, x_LL = x1D_set)) %>%
      # mass to volume #
      mutate(EC_env_m.um3.mL =  EC_env_m.particles.mL * mux.polyfnx(a.x = a.v, x_UL = x2D_set, x_LL = x1D_set)) %>% 
      # specific surface area to volume #
      mutate(EC_env_ssa.um3.mL =  EC_env_ssa.particles.mL * mux.polyfnx(a.x = a.v, x_UL = x2D_set, x_LL = x1D_set)) %>% 
      
      ## convert all environmentally realistic thresholds to mass ##
      # particle count to mass #
      mutate(EC_env_p.ug.mL =  EC_env_p.particles.mL * mux.polyfnx(a.x = a.m, x_UL = x2D_set, x_LL = x1D_set)) %>% 
      # surface area to mass #
      mutate(EC_env_sa.ug.mL =  EC_env_sa.particles.mL * mux.polyfnx(a.x = a.m, x_UL = x2D_set, x_LL = x1D_set)) %>% 
      # volume to mass #
      mutate(EC_env_v.ug.mL =  EC_env_v.particles.mL * mux.polyfnx(a.x = a.m, x_UL = x2D_set, x_LL = x1D_set)) %>%
      # mass to mass #
      mutate(EC_env_m.ug.mL =  EC_env_m.particles.mL * mux.polyfnx(a.x = a.m, x_UL = x2D_set, x_LL = x1D_set)) %>% 
      # specific surface area to mass #
      mutate(EC_env_ssa.ug.mL =  EC_env_ssa.particles.mL * mux.polyfnx(a.x = a.m, x_UL = x2D_set, x_LL = x1D_set)) %>% 
      
      ## convert all environmentally realistic thresholds to specific surface area ##
      # particle count to specific surface area #
      mutate(EC_env_p.um2.ug.mL =  EC_env_p.particles.mL * mux.polyfnx(a.x = a.ssa, x_UL = x2D_set, x_LL = x1D_set)) %>% 
      # surface area to specific surface area #
      mutate(EC_env_sa.um2.ug.mL =  EC_env_sa.particles.mL * mux.polyfnx(a.x = a.ssa, x_UL = x2D_set, x_LL = x1D_set)) %>% 
      # volume to specific surface area #
      mutate(EC_env_v.um2.ug.mL =  EC_env_v.particles.mL * mux.polyfnx(a.x = a.ssa, x_UL = x2D_set, x_LL = x1D_set)) %>%
      # mass to specific surface area #
      mutate(EC_env_m.um2.ug.mL =  EC_env_m.particles.mL * mux.polyfnx(a.x = a.ssa, x_UL = x2D_set, x_LL = x1D_set)) %>% 
      # specific surface area to specific surface area #
      mutate(EC_env_ssa.um2.ug.mL =  EC_env_ssa.particles.mL * mux.polyfnx(a.x = a.ssa, x_UL = x2D_set, x_LL = x1D_set))
    
    ## ERM ## 
    #Note: dose_check reports what dose metric to report in, ERM_check reports ERM of interest ##
    # note this is iteratered for each ERM (unaligned, particles, surface area, volume, mass, specific surface area), so there are 6 iterations below (numbere) #
    
    ## 0 DOSE METRIC = unaligned ###
    
    #filter out reported, calcualted, or all based on checkbox and make new variable based on µg/mL or particles/mL
    if(Rep_Con_rad == "reported" & dose_check == "µg/mL" & ERM_check == "Unaligned"){
      aoc_z <- aoc_z %>% 
        filter(dose.mg.L.master.converted.reported == "reported") %>% 
        mutate(dose_new = dose.mg.L.master)}
    
    if(Rep_Con_rad == "converted" & dose_check == "µg/mL" & ERM_check == "Unaligned"){
      aoc_z <- aoc_z %>%
        filter(dose.mg.L.master.converted.reported == "converted") %>% 
        mutate(dose_new = dose.mg.L.master)}
    
    if(Rep_Con_rad == "all" & dose_check == "µg/mL" & ERM_check == "Unaligned"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = dose.mg.L.master)}
    
    #repeat for particles (unaligned)
    if(Rep_Con_rad == "reported" & dose_check == "Particles/mL" & ERM_check == "Unaligned"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>% 
        mutate(dose_new = dose.particles.mL.master)}
    
    if(Rep_Con_rad == "converted" & dose_check == "Particles/mL" & ERM_check == "Unaligned"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>% 
        mutate(dose_new = dose.particles.mL.master)} 
    
    if(Rep_Con_rad == "all" & dose_check == "Particles/mL" & ERM_check == "Unaligned"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = dose.particles.mL.master)}
    
    #repeat for volume
    if(Rep_Con_rad == "reported" & dose_check == "µm3/mL" & ERM_check == "Unaligned"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = dose.um3.mL.master)}
    
    if(Rep_Con_rad == "converted" & dose_check == "µm3/mL" & ERM_check == "Unaligned"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = dose.um3.mL.master)}
    
    if(Rep_Con_rad == "all" & dose_check == "µm3/mL" & ERM_check == "Unaligned"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = dose.um3.mL.master)}
    
    #repeat for surface area
    if(Rep_Con_rad == "reported" & dose_check == "µm2/mL" & ERM_check == "Unaligned"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = dose.um2.mL.master)}
    
    if(Rep_Con_rad == "converted" & dose_check == "µm2/mL" & ERM_check == "Unaligned"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = dose.um2.mL.master)}
    
    if(Rep_Con_rad == "all" & dose_check == "µm2/mL" & ERM_check == "Unaligned"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = dose.um2.mL.master)}
    
    #repeat for specific surface area
    if(Rep_Con_rad == "reported" & dose_check == "µm2/µg/mL" & ERM_check == "Unaligned"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = dose.um2.ug.mL.master)}
    
    if(Rep_Con_rad == "converted" & dose_check == "µm2/µg/mL" & ERM_check == "Unaligned"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = dose.um2.ug.mL.master)}
    
    if(Rep_Con_rad == "all" & dose_check == "µm2/µg/mL" & ERM_check == "Unaligned"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = dose.um2.ug.mL.master)}
    

    
    ## 1 DOSE METRIC = PARTICLES ###
    if(Rep_Con_rad == "reported" & dose_check == "Particles/mL" & ERM_check == "Particles"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_p.particles.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "Particles/mL" & ERM_check == "Particles"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_p.particles.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "Particles/mL" & ERM_check == "Particles"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_p.particles.mL)}
    
    #repeat for particles with ERM = Surface Area
    if(Rep_Con_rad == "reported" & dose_check == "Particles/mL" & ERM_check == "Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new =EC_env_sa.particles.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "Particles/mL" & ERM_check == "Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_sa.particles.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "Particles/mL" & ERM_check == "Surface Area"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_sa.particles.mL)}
    
    #repeat for particles with ERM = Volume
    if(Rep_Con_rad == "reported" & dose_check == "Particles/mL" & ERM_check == "Volume"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_v.particles.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "Particles/mL" & ERM_check == "Volume"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_v.particles.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "Particles/mL" & ERM_check == "Volume"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_v.particles.mL)}
    
    #repeat for particles with ERM = Mass
    if(Rep_Con_rad == "reported" & dose_check == "Particles/mL" & ERM_check == "Mass"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_m.particles.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "Particles/mL" & ERM_check == "Mass"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_m.particles.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "Particles/mL" & ERM_check == "Mass"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_m.particles.mL)}
    
    #repeat for particles with ERM = Specific Surface Area
    if(Rep_Con_rad == "reported" & dose_check == "Particles/mL" & ERM_check == "Specific Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_ssa.particles.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "Particles/mL" & ERM_check == "Specific Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_ssa.particles.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "Particles/mL" & ERM_check == "Specific Surface Area"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_ssa.particles.mL)}
    
    # 2 DOSE METRIC = Surface Area ###
    if(Rep_Con_rad == "reported" & dose_check == "µm2/mL" & ERM_check == "Particles"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_p.um2.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm2/mL" & ERM_check == "Particles"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_p.um2.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm2/mL" & ERM_check == "Particles"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_p.um2.mL)}
    
    #repeat for particles with ERM = Surface Area
    if(Rep_Con_rad == "reported" & dose_check == "µm2/mL" & ERM_check == "Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_sa.um2.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm2/mL" & ERM_check == "Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_sa.um2.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm2/mL" & ERM_check == "Surface Area"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_sa.um2.mL)}
    
    #repeat for particles with ERM = Volume
    if(Rep_Con_rad == "reported" & dose_check == "µm2/mL" & ERM_check == "Volume"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_v.um2.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm2/mL" & ERM_check == "Volume"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_v.um2.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm2/mL" & ERM_check == "Volume"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_v.um2.mL)}
    
    #repeat for particles with ERM = Mass
    if(Rep_Con_rad == "reported" & dose_check == "µm2/mL" & ERM_check == "Mass"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_m.um2.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm2/mL" & ERM_check == "Mass"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_m.um2.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm2/mL" & ERM_check == "Mass"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_m.um2.mL)}
    
    #repeat for particles with ERM = Specific Surface Area
    if(Rep_Con_rad == "reported" & dose_check == "µm2/mL" & ERM_check == "Specific Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_ssa.um2.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm2/mL" & ERM_check == "Specific Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_ssa.um2.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm2/mL" & ERM_check == "Specific Surface Area"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_ssa.um2.mL)}
    
    # 3 DOSE METRIC = mass ###
    if(Rep_Con_rad == "reported" & dose_check == "µg/mL" & ERM_check == "Particles"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_p.ug.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µg/mL" & ERM_check == "Particles"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_p.ug.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µg/mL" & ERM_check == "Particles"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_p.ug.mL)}
    
    #repeat for particles with ERM = Surface Area
    if(Rep_Con_rad == "reported" & dose_check == "µg/mL" & ERM_check == "Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_sa.ug.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µg/mL" & ERM_check == "Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_sa.ug.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µg/mL" & ERM_check == "Surface Area"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_sa.ug.mL)}
    
    #repeat for particles with ERM = Volume
    if(Rep_Con_rad == "reported" & dose_check == "µg/mL" & ERM_check == "Volume"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_v.ug.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µg/mL" & ERM_check == "Volume"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_v.ug.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µg/mL" & ERM_check == "Volume"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_v.ug.mL)}
    
    #repeat for particles with ERM = Mass
    if(Rep_Con_rad == "reported" & dose_check == "µg/mL" & ERM_check == "Mass"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_m.ug.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µg/mL" & ERM_check == "Mass"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_m.ug.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µg/mL" & ERM_check == "Mass"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_m.ug.mL)}
    
    #repeat for particles with ERM = Specific Surface Area
    if(Rep_Con_rad == "reported" & dose_check == "µg/mL" & ERM_check == "Specific Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_ssa.ug.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µg/mL" & ERM_check == "Specific Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_ssa.ug.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µg/mL" & ERM_check == "Specific Surface Area"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_ssa.ug.mL)}
    
    # 4 DOSE METRIC = volume ###
    if(Rep_Con_rad == "reported" & dose_check == "µm3/mL" & ERM_check == "Particles"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_p.um3.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm3/mL" & ERM_check == "Particles"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_p.um3.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm3/mL" & ERM_check == "Particles"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_p.um3.mL)}
    
    #repeat for particles with ERM = Surface Area
    if(Rep_Con_rad == "reported" & dose_check == "µm3/mL" & ERM_check == "Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_sa.um3.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm3/mL" & ERM_check == "Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_sa.um3.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm3/mL" & ERM_check == "Surface Area"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_sa.um3.mL)}
    
    #repeat for particles with ERM = Volume
    if(Rep_Con_rad == "reported" & dose_check == "µm3/mL" & ERM_check == "Volume"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_v.um3.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm3/mL" & ERM_check == "Volume"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_v.um3.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm3/mL" & ERM_check == "Volume"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_v.um3.mL)}
    
    #repeat for particles with ERM = Mass
    if(Rep_Con_rad == "reported" & dose_check == "µm3/mL" & ERM_check == "Mass"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_m.um3.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm3/mL" & ERM_check == "Mass"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_m.um3.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm3/mL" & ERM_check == "Mass"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_m.um3.mL)}
    
    #repeat for particles with ERM = Specific Surface Area
    if(Rep_Con_rad == "reported" & dose_check == "µm3/mL" & ERM_check == "Specific Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_ssa.um3.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm3/mL" & ERM_check == "Specific Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_ssa.um3.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm3/mL" & ERM_check == "Specific Surface Area"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_ssa.um3.mL)}
    
    # 5 DOSE METRIC = specific surface are ###
    if(Rep_Con_rad == "reported" & dose_check == "µm2/µg/mL" & ERM_check == "Particles"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_p.um2.ug.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm2/µg/mL" & ERM_check == "Particles"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_p.um2.ug.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm2/µg/mL" & ERM_check == "Particles"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_p.um2.ug.mL)}
    
    #repeat for particles with ERM = Surface Area
    if(Rep_Con_rad == "reported" & dose_check == "µm2/µg/mL" & ERM_check == "Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_sa.um2.ug.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm2/µg/mL" & ERM_check == "Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_sa.um2.ug.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm2/µg/mL" & ERM_check == "Surface Area"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_sa.um2.ug.mL)}
    
    #repeat for particles with ERM = Volume
    if(Rep_Con_rad == "reported" & dose_check == "µm2/µg/mL" & ERM_check == "Volume"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_v.um2.ug.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm2/µg/mL" & ERM_check == "Volume"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_v.um2.ug.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm2/µg/mL" & ERM_check == "Volume"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_v.um2.ug.mL)}
    
    #repeat for particles with ERM = Mass
    if(Rep_Con_rad == "reported" & dose_check == "µm2/µg/mL" & ERM_check == "Mass"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_m.um2.ug.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm2/µg/mL" & ERM_check == "Mass"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_m.um2.ug.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm2/µg/mL" & ERM_check == "Mass"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_m.um2.ug.mL)}
    
    #repeat for particles with ERM = Specific Surface Area
    if(Rep_Con_rad == "reported" & dose_check == "µm2/µg/mL" & ERM_check == "Specific Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_ssa.um2.ug.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm2/µg/mL" & ERM_check == "Specific Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_ssa.um2.ug.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm2/µg/mL" & ERM_check == "Specific Surface Area"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_ssa.um2.ug.mL)}
    
    #left-hand table of all data considered
    aoc_z %>% # take original dataset
      mutate(dose_new = case_when(AF.time_r_ssd == "Yes" & AF.noec_r_ssd == "Yes" ~ dose_new / (af.time * af.noec), #composite assessment factors
                                  AF.time_r_ssd == "Yes" & AF.noec_r_ssd == "No" ~ dose_new / af.time,
                                  AF.time_r_ssd == "No" & AF.noec_r_ssd == "Yes" ~ dose_new / af.noec,
                                  AF.time_r_ssd == "No" & AF.noec_r_ssd == "No" ~ dose_new)) %>% # adjust for assessment factors based on user input
      dplyr::filter(exp_type_f %in% exp_type_c_ssd) %>%
      dplyr::filter(env_f %in% env_c_ssd) %>% #filter by environment inputs
      dplyr::filter(Group %in% Group_c_ssd) %>% # filter by organism inputs
      dplyr::filter(Species %in% Species_c_ssd) %>% #filter by species inputs
      dplyr::filter(bio_f %in% bio_c_ssd) %>% #filter by species inputs
      dplyr::filter(size_f %in% size_c_ssd) %>% #filter by size inputs
      dplyr::filter(lvl1_f %in% lvl1_c_ssd) %>% # filter by broad inputs
      dplyr::filter(lvl2_f %in% lvl2_c_ssd) %>% # filter by level inputs
      dplyr::filter(poly_f %in% poly_c_ssd) %>% #filter by polymer inputs
      dplyr::filter(shape_f %in% shape_c_ssd) %>% #filter by shape inputs
      dplyr::filter(bio_f %in% bio_c_ssd) %>% #filter by bio org inputs
      dplyr::filter(tier_zero_tech_f %in% tech_tier_zero_c_ssd) %>% #technical quality
      dplyr::filter(tier_zero_risk_f %in% risk_tier_zero_c_ssd) %>%  #risk assessment quality
      dplyr::filter(dose_new > 0) %>% #clean out no dose data
      dplyr::filter(acute.chronic_f %in% acute.chronic.c_ssd) %>%  #acute chronic filter
      group_by(Species) %>% 
      drop_na(dose_new) %>% 
            summarise(MinConcTested = min(dose_new), MaxConcTested = max(dose_new), CountTotal = n()) %>%   #summary data for whole database
      mutate_if(is.numeric, ~ signif(., 4))
        })
  
  # Create new effect dataset based on widget filtering and adjusted to reflect the presence of the "update" button.
  aoc_z_R <- eventReactive(list(input$SSDgo),{
    # eventReactive explicitly delays activity until you press the button
    # here we'll use the inputs to create a new dataset that will be fed into the renderPlot calls below
    exp_type_c_ssd <- input$exp_type_check_ssd
    env_c_ssd <- input$env_check_ssd #assign environments
    Group_c_ssd <- input$Group_check_ssd # assign organism input values to "org_c"
    bio_c_ssd <- input$bio_check_ssd #level of biological organization
    Species_c_ssd <- input$Species_check_ssd #assign species input
    size_c_ssd <- input$size_check_ssd #assign sizes input
    lvl1_c_ssd <- input$lvl1_check_ssd #assign general endpoints
    lvl2_c_ssd <- input$lvl2_check_ssd #assign specific endpoints
    poly_c_ssd <- input$poly_check_ssd #assign polymers
    shape_c_ssd <- input$shape_check_ssd #assign shapes
    effect_metric_rad <- input$effect.metric_rad_ssd #effect metric filtering
    AF.time_r_ssd <- input$AF.time_rad_ssd #yes/no apply assessment factor for acute -> chronic
    AF.noec_r_ssd <- input$AF.noec_rad_ssd #yes/no apply assessment factor for LOEC/ECXX -> NOEC
    Rep_Con_rad <- input$Reported_Converted_rad #use nominal or calculated exposure concentrations. Options are TRUE (calculated) or FALSE (reported)
    ERM_check <- input$ERM_check_ssd #ERM
    dose_check <- input$dose_check_ssd #rename variable
    acute.chronic.c_ssd <- input$acute.chronic_check_ssd #acute chronic checkbox
    conc.select.r <- input$conc.select.rad #concentration selector ("minimum", "lower 95% CI", "1st Quartile", "median", "mean", "3rd Quartile", "upper 95% CI", "maximum")
    tech_tier_zero_c_ssd<-input$tech_tier_zero_check_ssd #assign values to "design_tier_zero_c"
    risk_tier_zero_c_ssd<-input$risk_tier_zero_check_ssd #assign values to "risk_tier_zero_c"
    
    ## ERM parametrization ##
    # Define params for correction #
    alpha = input$alpha_ssd #2.07 #table s4 for marine surface water. length
    x2D_set = input$upper_length_ssd #upper size range (default)
    x1D_set = input$lower_length_ssd #lower size range (default)
    x1M_set = 1 #lower size range for ingestible plastic
    
    # define parameters for power law coefficients
    a.sa = input$a.sa_ssd #1.5 #marine surface area power law
    a.v = input$a.v_ssd#1.48 #a_V for marine surface water volume
    a.m = input$a.m_ssd#1.32 # upper limit fora_m for mass for marine surface water in table S4 
    a.ssa = input$a.ssa_ssd #1.98 # A_SSA for marine surface water
    
    #define additional parameters for calculations based on averages in the environment
    R.ave = input$R.ave_ssd #0.77 #average width to length ratio for microplastics in marine enviornment
    p.ave = input$p.ave_ssd#1.10 #average density in marine surface water
  
    
    # calculate ERM for each species
    aoc_z <- aoc_z %>% 
      #filter the data to only include particle only data
      # dplyr::filter(exp_type_f == "Particle Only") %>%
      # define upper size WIDTH for ingestion (based on average width:length ratio)
      mutate(x2M = case_when(is.na(max.size.ingest.um) ~ (1/R.ave) * x2D_set, #all calculations below occur for length. Width is R.ave * length, so correcting here makes width the max size ingest below
                             (max.size.ingest.um * (1/R.ave)) < x2D_set ~ ((1/R.ave) * max.size.ingest.um),
                             (max.size.ingest.um * (1/R.ave)) > x2D_set ~ (x2D_set * (1/R.ave)))) %>% #set to 10um for upper limit or max size ingest, whichever is smaller
      # calculate effect threshold for particles
      mutate(EC_mono_p.particles.mL = dose.particles.mL.master) %>% 
      mutate(mu.p.mono = 1) %>% #mu_x_mono is always 1 for particles to particles
      mutate(mu.p.poly = mux.polyfnx(a.x = alpha, x_UL= x2M, x_LL = x1M_set)) %>% 
      # polydisperse effect threshold for particles
      mutate(EC_poly_p.particles.mL = (EC_mono_p.particles.mL * mu.p.mono)/mu.p.poly) %>% 
      #calculate CF_bio for all conversions
      mutate(CF_bio = CFfnx(x1M = x1M_set, x2M = x2M, x1D = x1D_set, x2D = x2D_set, a = alpha)) %>%  
      ## Calculate environmentally relevant effect threshold for particles
      mutate(EC_env_p.particles.mL = EC_poly_p.particles.mL * CF_bio) %>%  #aligned particle effect concentraiton (1-5000 um)
      # Surface area ERM ##
    mutate(mu.sa.mono = as.numeric(particle.surface.area.um2)) %>% #define mu_x_mono for alignment to ERM
      #calculate lower ingestible surface area
      mutate(x_LL_sa = SAfnx(a = 0.5 * x1D_set, b = 0.5 * R.ave * x1D_set, c = 0.5 * R.ave * 0.67 * x1D_set)) %>%  
      #calculate upper ingestible surface area
      mutate(x_UL_sa = SAfnx(a = 0.5 * x2M, b = 0.5 * R.ave * x2M, c = 0.5 * R.ave * 0.67 * x2M)) %>%  
      #calculate mu_x_poly for surface area
      mutate(mu.sa.poly = if(a.sa == 2){mux.polyfnx.2(x_UL_sa, x_LL_sa)} else if (a.sa != 2){mux.polyfnx(a.sa, x_UL_sa, x_LL_sa)}) %>% 
      #calculate polydisperse effect concentration for surface area (particles/mL)
      mutate(EC_poly_sa.particles.mL = (EC_mono_p.particles.mL * mu.sa.mono)/mu.sa.poly) %>%  
      #calculate environmentally realistic effect threshold
      mutate(EC_env_sa.particles.mL = EC_poly_sa.particles.mL * CF_bio) %>% 
      # volume ERM ##
    #define mu_x_mono for alignment to ERM
    mutate(mu.v.mono = as.numeric(particle.volume.um3)) %>% 
      #calculate lower ingestible volume 
      mutate(x_LL_v = volumefnx(R = R.ave, L = x1D_set)) %>%
      #calculate maximum ingestible volume 
      mutate(x_UL_v = volumefnx(R = R.ave,L = x2M)) %>%  
      # calculate mu.v.poly
      mutate(mu.v.poly = if(a.v == 2){mux.polyfnx.2(x_UL_v, x_LL_v)} else if (a.v != 2){mux.polyfnx(a.v, x_UL_v, x_LL_v)}) %>% 
      #calculate polydisperse effect concentration for volume (particles/mL)
      mutate(EC_poly_v.particles.mL = (EC_mono_p.particles.mL * mu.v.mono)/mu.v.poly) %>%  
      #calculate environmentally realistic effect threshold
      mutate(EC_env_v.particles.mL = EC_poly_v.particles.mL * CF_bio) %>% 
      #### mass ERM ###
      #define mu_x_mono for alignment to ERM (ug)
      mutate(mu.m.mono = mass.per.particle.mg * 1000) %>% 
      #calculate lower ingestible mass
      mutate(x_LL_m = massfnx(R = R.ave, L = x1D_set, p = p.ave)) %>%  
      #calculate upper ingestible mass
      mutate(x_UL_m = massfnx(R = R.ave, L = x2M, p = p.ave)) %>%  
      # calculate mu.m.poly
      mutate(mu.m.poly = if(a.m == 2){mux.polyfnx.2(x_UL_m, x_LL_m)} else if (a.m != 2){mux.polyfnx(a.m, x_UL_m, x_LL_m)}) %>% 
      #calculate polydisperse effect concentration for volume (particles/mL)
      mutate(EC_poly_m.particles.mL = (EC_env_p.particles.mL * mu.m.mono)/mu.m.poly) %>%
      #calculate environmentally realistic effect threshold
      mutate(EC_env_m.particles.mL = EC_poly_m.particles.mL * CF_bio) %>% 
      ## specific surface area ERM ###
    mutate(mu.ssa.mono = mu.sa.mono/mu.m.mono) %>% #define mu_x_mono for alignment to ERM (um^2/ug)
      #calculate lower ingestible SSA
      mutate(x_LL_ssa = SSAfnx(sa = x_LL_sa,m = x_LL_m)) %>% 
      #calculate upper ingestible SSA  (um^2/ug)
      mutate(x_UL_ssa = SSAfnx(sa = x_UL_sa, m = x_UL_m)) %>% 
      #calculate mu_x_poly for specific surface area
      mutate(mu.ssa.poly = if(a.ssa == 2){mux.polyfnx.2(x_UL_ssa, x_LL_ssa)} else if (a.ssa != 2){mux.polyfnx(a.ssa, x_UL_ssa, x_LL_ssa)}) %>% 
      #calculate polydisperse effect concentration for specific surface area (particles/mL)
      mutate(EC_poly_ssa.particles.mL = (EC_env_p.particles.mL * mu.ssa.mono)/mu.ssa.poly) %>% 
      #calculate environmentally realistic effect threshold
      mutate(EC_env_ssa.particles.mL = EC_poly_ssa.particles.mL * CF_bio) %>% 
      ### Convert to Metrics other than particles/mL ###
      
      ## convert all environmentally realistic thresholds to surface area ##
      # particle count to surface area #
      mutate(EC_env_p.um2.mL =  EC_env_p.particles.mL * mux.polyfnx(a.x = a.sa, x_UL = x2D_set, x_LL = x1D_set)) %>% 
      # surface area to surface area #
      mutate(EC_env_sa.um2.mL =  EC_env_sa.particles.mL * mux.polyfnx(a.x = a.sa, x_UL = x2D_set, x_LL = x1D_set)) %>% 
      # volume to surface area #
      mutate(EC_env_v.um2.mL =  EC_env_v.particles.mL * mux.polyfnx(a.x = a.sa, x_UL = x2D_set, x_LL = x1D_set)) %>%
      # mass to surface area #
      mutate(EC_env_m.um2.mL =  EC_env_m.particles.mL * mux.polyfnx(a.x = a.sa, x_UL = x2D_set, x_LL = x1D_set)) %>% 
      # specific surface area to surface area #
      mutate(EC_env_ssa.um2.mL =  EC_env_ssa.particles.mL * mux.polyfnx(a.x = a.sa, x_UL = x2D_set, x_LL = x1D_set)) %>% 
      
      ## convert all environmentally realistic thresholds to volume ##
      # particle count to volume #
      mutate(EC_env_p.um3.mL =  EC_env_p.particles.mL * mux.polyfnx(a.x = a.v, x_UL = x2D_set, x_LL = x1D_set)) %>% 
      # surface area to volume #
      mutate(EC_env_sa.um3.mL =  EC_env_sa.particles.mL * mux.polyfnx(a.x = a.v, x_UL = x2D_set, x_LL = x1D_set)) %>% 
      # volume to volume #
      mutate(EC_env_v.um3.mL =  EC_env_v.particles.mL * mux.polyfnx(a.x = a.v, x_UL = x2D_set, x_LL = x1D_set)) %>%
      # mass to volume #
      mutate(EC_env_m.um3.mL =  EC_env_m.particles.mL * mux.polyfnx(a.x = a.v, x_UL = x2D_set, x_LL = x1D_set)) %>% 
      # specific surface area to volume #
      mutate(EC_env_ssa.um3.mL =  EC_env_ssa.particles.mL * mux.polyfnx(a.x = a.v, x_UL = x2D_set, x_LL = x1D_set)) %>% 
     
       ## convert all environmentally realistic thresholds to mass ##
      # particle count to mass #
      mutate(EC_env_p.ug.mL =  EC_env_p.particles.mL * mux.polyfnx(a.x = a.m, x_UL = x2D_set, x_LL = x1D_set)) %>% 
      # surface area to mass #
      mutate(EC_env_sa.ug.mL =  EC_env_sa.particles.mL * mux.polyfnx(a.x = a.m, x_UL = x2D_set, x_LL = x1D_set)) %>% 
      # volume to mass #
      mutate(EC_env_v.ug.mL =  EC_env_v.particles.mL * mux.polyfnx(a.x = a.m, x_UL = x2D_set, x_LL = x1D_set)) %>%
      # mass to mass #
      mutate(EC_env_m.ug.mL =  EC_env_m.particles.mL * mux.polyfnx(a.x = a.m, x_UL = x2D_set, x_LL = x1D_set)) %>% 
      # specific surface area to mass #
      mutate(EC_env_ssa.ug.mL =  EC_env_ssa.particles.mL * mux.polyfnx(a.x = a.m, x_UL = x2D_set, x_LL = x1D_set)) %>% 
      
      ## convert all environmentally realistic thresholds to specific surface area ##
      # particle count to specific surface area #
      mutate(EC_env_p.um2.ug.mL =  EC_env_p.particles.mL * mux.polyfnx(a.x = a.ssa, x_UL = x2D_set, x_LL = x1D_set)) %>% 
      # surface area to specific surface area #
      mutate(EC_env_sa.um2.ug.mL =  EC_env_sa.particles.mL * mux.polyfnx(a.x = a.ssa, x_UL = x2D_set, x_LL = x1D_set)) %>% 
      # volume to specific surface area #
      mutate(EC_env_v.um2.ug.mL =  EC_env_v.particles.mL * mux.polyfnx(a.x = a.ssa, x_UL = x2D_set, x_LL = x1D_set)) %>%
      # mass to specific surface area #
      mutate(EC_env_m.um2.ug.mL =  EC_env_m.particles.mL * mux.polyfnx(a.x = a.ssa, x_UL = x2D_set, x_LL = x1D_set)) %>% 
      # specific surface area to specific surface area #
      mutate(EC_env_ssa.um2.ug.mL =  EC_env_ssa.particles.mL * mux.polyfnx(a.x = a.ssa, x_UL = x2D_set, x_LL = x1D_set))
      
    
    ## ERM ## 
    #Note: dose_check reports what dose metric to report in, ERM_check reports ERM of interest ##
    # note this is iteratered for each ERM (unaligned, particles, surface area, volume, mass, specific surface area), so there are 6 iterations below (numbere) #
    
    ## 0 DOSE METRIC = unaligned ###
    
    #filter out reported, calcualted, or all based on checkbox and make new variable based on µg/mL or particles/mL
    if(Rep_Con_rad == "reported" & dose_check == "µg/mL" & ERM_check == "Unaligned"){
      aoc_z <- aoc_z %>% 
        filter(dose.mg.L.master.converted.reported == "reported") %>% 
        mutate(dose_new = dose.mg.L.master)}
    
    if(Rep_Con_rad == "converted" & dose_check == "µg/mL" & ERM_check == "Unaligned"){
      aoc_z <- aoc_z %>%
        filter(dose.mg.L.master.converted.reported == "converted") %>% 
        mutate(dose_new = dose.mg.L.master)}
    
    if(Rep_Con_rad == "all" & dose_check == "µg/mL" & ERM_check == "Unaligned"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = dose.mg.L.master)}
    
    #repeat for particles (unaligned)
    if(Rep_Con_rad == "reported" & dose_check == "Particles/mL" & ERM_check == "Unaligned"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>% 
        mutate(dose_new = dose.particles.mL.master)}
    
    if(Rep_Con_rad == "converted" & dose_check == "Particles/mL" & ERM_check == "Unaligned"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>% 
        mutate(dose_new = dose.particles.mL.master)} 
    
    if(Rep_Con_rad == "all" & dose_check == "Particles/mL" & ERM_check == "Unaligned"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = dose.particles.mL.master)}
    
    #repeat for volume
    if(Rep_Con_rad == "reported" & dose_check == "µm3/mL" & ERM_check == "Unaligned"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = dose.um3.mL.master)}
    
    if(Rep_Con_rad == "converted" & dose_check == "µm3/mL" & ERM_check == "Unaligned"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = dose.um3.mL.master)}
    
    if(Rep_Con_rad == "all" & dose_check == "µm3/mL" & ERM_check == "Unaligned"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = dose.um3.mL.master)}
    
    #repeat for surface area
    if(Rep_Con_rad == "reported" & dose_check == "µm2/mL" & ERM_check == "Unaligned"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = dose.um2.mL.master)}
    
    if(Rep_Con_rad == "converted" & dose_check == "µm2/mL" & ERM_check == "Unaligned"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = dose.um2.mL.master)}
    
    if(Rep_Con_rad == "all" & dose_check == "µm2/mL" & ERM_check == "Unaligned"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = dose.um2.mL.master)}
    
    #repeat for specific surface area
    if(Rep_Con_rad == "reported" & dose_check == "µm2/µg/mL" & ERM_check == "Unaligned"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = dose.um2.ug.mL.master)}
    
    if(Rep_Con_rad == "converted" & dose_check == "µm2/µg/mL" & ERM_check == "Unaligned"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = dose.um2.ug.mL.master)}
    
    if(Rep_Con_rad == "all" & dose_check == "µm2/µg/mL" & ERM_check == "Unaligned"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = dose.um2.ug.mL.master)}
    
    
    ## 1 DOSE METRIC = PARTICLES ###
    if(Rep_Con_rad == "reported" & dose_check == "Particles/mL" & ERM_check == "Particles"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_p.particles.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "Particles/mL" & ERM_check == "Particles"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_p.particles.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "Particles/mL" & ERM_check == "Particles"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_p.particles.mL)}
    
    #repeat for particles with ERM = Surface Area
    if(Rep_Con_rad == "reported" & dose_check == "Particles/mL" & ERM_check == "Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new =EC_env_sa.particles.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "Particles/mL" & ERM_check == "Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_sa.particles.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "Particles/mL" & ERM_check == "Surface Area"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_sa.particles.mL)}
    
    #repeat for particles with ERM = Volume
    if(Rep_Con_rad == "reported" & dose_check == "Particles/mL" & ERM_check == "Volume"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_v.particles.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "Particles/mL" & ERM_check == "Volume"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_v.particles.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "Particles/mL" & ERM_check == "Volume"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_v.particles.mL)}
    
    #repeat for particles with ERM = Mass
    if(Rep_Con_rad == "reported" & dose_check == "Particles/mL" & ERM_check == "Mass"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_m.particles.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "Particles/mL" & ERM_check == "Mass"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_m.particles.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "Particles/mL" & ERM_check == "Mass"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_m.particles.mL)}
    
    #repeat for particles with ERM = Specific Surface Area
    if(Rep_Con_rad == "reported" & dose_check == "Particles/mL" & ERM_check == "Specific Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_ssa.particles.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "Particles/mL" & ERM_check == "Specific Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_ssa.particles.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "Particles/mL" & ERM_check == "Specific Surface Area"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_ssa.particles.mL)}
    
    # 2 DOSE METRIC = Surface Area ###
    if(Rep_Con_rad == "reported" & dose_check == "µm2/mL" & ERM_check == "Particles"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_p.um2.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm2/mL" & ERM_check == "Particles"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_p.um2.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm2/mL" & ERM_check == "Particles"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_p.um2.mL)}
    
    #repeat for particles with ERM = Surface Area
    if(Rep_Con_rad == "reported" & dose_check == "µm2/mL" & ERM_check == "Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_sa.um2.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm2/mL" & ERM_check == "Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_sa.um2.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm2/mL" & ERM_check == "Surface Area"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_sa.um2.mL)}
    
    #repeat for particles with ERM = Volume
    if(Rep_Con_rad == "reported" & dose_check == "µm2/mL" & ERM_check == "Volume"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_v.um2.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm2/mL" & ERM_check == "Volume"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_v.um2.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm2/mL" & ERM_check == "Volume"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_v.um2.mL)}
    
    #repeat for particles with ERM = Mass
    if(Rep_Con_rad == "reported" & dose_check == "µm2/mL" & ERM_check == "Mass"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_m.um2.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm2/mL" & ERM_check == "Mass"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_m.um2.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm2/mL" & ERM_check == "Mass"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_m.um2.mL)}
    
    #repeat for particles with ERM = Specific Surface Area
    if(Rep_Con_rad == "reported" & dose_check == "µm2/mL" & ERM_check == "Specific Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_ssa.um2.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm2/mL" & ERM_check == "Specific Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_ssa.um2.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm2/mL" & ERM_check == "Specific Surface Area"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_ssa.um2.mL)}
    
    # 3 DOSE METRIC = mass ###
    if(Rep_Con_rad == "reported" & dose_check == "µg/mL" & ERM_check == "Particles"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_p.ug.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µg/mL" & ERM_check == "Particles"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_p.ug.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µg/mL" & ERM_check == "Particles"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_p.ug.mL)}
    
    #repeat for particles with ERM = Surface Area
    if(Rep_Con_rad == "reported" & dose_check == "µg/mL" & ERM_check == "Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_sa.ug.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µg/mL" & ERM_check == "Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_sa.ug.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µg/mL" & ERM_check == "Surface Area"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_sa.ug.mL)}
    
    #repeat for particles with ERM = Volume
    if(Rep_Con_rad == "reported" & dose_check == "µg/mL" & ERM_check == "Volume"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_v.ug.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µg/mL" & ERM_check == "Volume"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_v.ug.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µg/mL" & ERM_check == "Volume"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_v.ug.mL)}
    
    #repeat for particles with ERM = Mass
    if(Rep_Con_rad == "reported" & dose_check == "µg/mL" & ERM_check == "Mass"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_m.ug.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µg/mL" & ERM_check == "Mass"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_m.ug.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µg/mL" & ERM_check == "Mass"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_m.ug.mL)}
    
    #repeat for particles with ERM = Specific Surface Area
    if(Rep_Con_rad == "reported" & dose_check == "µg/mL" & ERM_check == "Specific Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_ssa.ug.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µg/mL" & ERM_check == "Specific Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_ssa.ug.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µg/mL" & ERM_check == "Specific Surface Area"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_ssa.ug.mL)}
    
    # 4 DOSE METRIC = volume ###
    if(Rep_Con_rad == "reported" & dose_check == "µm3/mL" & ERM_check == "Particles"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_p.um3.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm3/mL" & ERM_check == "Particles"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_p.um3.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm3/mL" & ERM_check == "Particles"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_p.um3.mL)}
    
    #repeat for particles with ERM = Surface Area
    if(Rep_Con_rad == "reported" & dose_check == "µm3/mL" & ERM_check == "Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_sa.um3.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm3/mL" & ERM_check == "Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_sa.um3.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm3/mL" & ERM_check == "Surface Area"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_sa.um3.mL)}
    
    #repeat for particles with ERM = Volume
    if(Rep_Con_rad == "reported" & dose_check == "µm3/mL" & ERM_check == "Volume"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_v.um3.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm3/mL" & ERM_check == "Volume"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_v.um3.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm3/mL" & ERM_check == "Volume"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_v.um3.mL)}
    
    #repeat for particles with ERM = Mass
    if(Rep_Con_rad == "reported" & dose_check == "µm3/mL" & ERM_check == "Mass"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_m.um3.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm3/mL" & ERM_check == "Mass"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_m.um3.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm3/mL" & ERM_check == "Mass"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_m.um3.mL)}
    
    #repeat for particles with ERM = Specific Surface Area
    if(Rep_Con_rad == "reported" & dose_check == "µm3/mL" & ERM_check == "Specific Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_ssa.um3.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm3/mL" & ERM_check == "Specific Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_ssa.um3.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm3/mL" & ERM_check == "Specific Surface Area"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_ssa.um3.mL)}
    
    # 5 DOSE METRIC = specific surface are ###
    if(Rep_Con_rad == "reported" & dose_check == "µm2/µg/mL" & ERM_check == "Particles"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_p.um2.ug.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm2/µg/mL" & ERM_check == "Particles"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_p.um2.ug.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm2/µg/mL" & ERM_check == "Particles"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_p.um2.ug.mL)}
    
    #repeat for particles with ERM = Surface Area
    if(Rep_Con_rad == "reported" & dose_check == "µm2/µg/mL" & ERM_check == "Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_sa.um2.ug.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm2/µg/mL" & ERM_check == "Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_sa.um2.ug.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm2/µg/mL" & ERM_check == "Surface Area"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_sa.um2.ug.mL)}
    
    #repeat for particles with ERM = Volume
    if(Rep_Con_rad == "reported" & dose_check == "µm2/µg/mL" & ERM_check == "Volume"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_v.um2.ug.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm2/µg/mL" & ERM_check == "Volume"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_v.um2.ug.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm2/µg/mL" & ERM_check == "Volume"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_v.um2.ug.mL)}
    
    #repeat for particles with ERM = Mass
    if(Rep_Con_rad == "reported" & dose_check == "µm2/µg/mL" & ERM_check == "Mass"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_m.um2.ug.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm2/µg/mL" & ERM_check == "Mass"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_m.um2.ug.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm2/µg/mL" & ERM_check == "Mass"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_m.um2.ug.mL)}
    
    #repeat for particles with ERM = Specific Surface Area
    if(Rep_Con_rad == "reported" & dose_check == "µm2/µg/mL" & ERM_check == "Specific Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_ssa.um2.ug.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm2/µg/mL" & ERM_check == "Specific Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_ssa.um2.ug.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm2/µg/mL" & ERM_check == "Specific Surface Area"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_ssa.um2.ug.mL)}
    
    
    
    #right-hand table of just effect data
    aoc_ssd <- aoc_z %>% 
      mutate(dose_new = case_when(AF.time_r_ssd == "Yes" & AF.noec_r_ssd == "Yes" ~ dose_new / (af.time * af.noec),
                                  AF.time_r_ssd == "Yes" & AF.noec_r_ssd == "No" ~ dose_new / (af.time),
                                  AF.time_r_ssd == "No" & AF.noec_r_ssd == "Yes" ~ dose_new / (af.noec),
                                  AF.time_r_ssd == "No" & AF.noec_r_ssd == "No" ~ dose_new)) %>% # adjust for assessment factors based on user input
      dplyr::filter(exp_type_f %in% exp_type_c_ssd) %>%
      dplyr::filter(env_f %in% env_c_ssd) %>% #filter by environment inputs
      dplyr::filter(Group %in% Group_c_ssd) %>% # filter by organism inputs
      dplyr::filter(bio_f %in% bio_c_ssd) %>% #filter by species inputs
      dplyr::filter(Species %in% Species_c_ssd) %>% #filter by species inputs
      dplyr::filter(size_f %in% size_c_ssd) %>% #filter by size inputs
      dplyr::filter(lvl1_f %in% lvl1_c_ssd) %>% # filter by generic endpoints inputs
      dplyr::filter(lvl2_f %in% lvl2_c_ssd) %>% # filter by specific endpoints inputs
      dplyr::filter(poly_f %in% poly_c_ssd) %>% #filter by polymer inputs
      dplyr::filter(shape_f %in% shape_c_ssd) %>% #filter by shape inputs
      dplyr::filter(bio_f %in% bio_c_ssd) %>% #filter by bio org inputs
      dplyr::filter(effect.metric %in% effect_metric_rad) %>%  #filter for effect metric
      dplyr::filter(acute.chronic_f %in% acute.chronic.c_ssd) %>%  #acute chronic filter
      dplyr::filter(tier_zero_tech_f %in% tech_tier_zero_c_ssd) %>% #technical quality
      dplyr::filter(tier_zero_risk_f %in% risk_tier_zero_c_ssd) %>%  #risk assessment quality
      drop_na(dose_new) %>%  #must drop NAs or else nothing will work
      group_by(Species, Group) %>%
      summarise(minConcEffect = min(dose_new), meanConcEffect = mean(dose_new), medianConcEffect = median(dose_new), SDConcEffect = sd(dose_new),MaxConcEffect = max(dose_new), CI95_LCL = meanConcEffect - 1.96 * SDConcEffect/sqrt(n()), firstQuartileConcEffect = quantile(dose_new, 0.25), CI95_UCL = meanConcEffect + 1.96 * SDConcEffect/sqrt(n()), thirdQuartileConcEffect = quantile(dose_new, 0.75), CountEffect = n(), MinEffectType = lvl1[which.min(dose_new)], Minlvl2EffectType = lvl2[which.min(dose_new)], MinEnvironment = environment[which.min(dose_new)], MinDoi = doi[which.min(dose_new)], MinLifeStage = life.stage[which.min(dose_new)], Mininvitro.invivo = invitro.invivo[which.min(dose_new)]) %>%  #set concentration to minimum observed effect
      mutate_if(is.numeric, ~ signif(., 3))
   
    #dynamically change concentrations used based on user input
    ###concentration selector ("minimum", "lower 95% CI", "1st Quartile", "median", "mean", "3rd Quartile", "upper 95% CI", "maximum")###
    if(conc.select.r == "Minimum"){
      aoc_ssd <- aoc_ssd %>% 
        mutate(Conc = minConcEffect)
    } 
    if(conc.select.r == "Lower 95% CI"){
      aoc_ssd <- aoc_ssd %>% 
        mutate(Conc = CI95_LCL)
    }
    if(conc.select.r == "1st Quartile"){
      aoc_ssd <- aoc_ssd %>% 
        mutate(Conc = firstQuartileConcEffect)
    }
    if(conc.select.r == "Mean"){
      aoc_ssd <- aoc_ssd %>% 
        mutate(Conc = meanConcEffect)
    }
    if(conc.select.r == "Median"){
      aoc_ssd <- aoc_ssd %>% 
        mutate(Conc = medianConcEffect)
    }
    if(conc.select.r == "3rd Quartile"){
      aoc_ssd <- aoc_ssd %>% 
        mutate(Conc = thirdQuartileConcEffect)
    }
    if(conc.select.r == "Upper 95% CI"){
      aoc_ssd <- aoc_ssd %>% 
        mutate(Conc = CI95_UCL)
    }
    if(conc.select.r == "Maximum"){
      aoc_ssd <- aoc_ssd %>% 
        mutate(Conc = MaxConcEffect)
    }
    
    #final table
    aoc_ssd
     })
  
  #Join
  aoc_filter_ssd <- reactive({
    req(aoc_z_L)
    req(aoc_z_R)
    
    #join datasets (final)
    aoc_z_join <- right_join(aoc_z_L(), aoc_z_R(), by = "Species") 
    #order list
    col_order <- c("Group", "Species", "Conc", "MinEffectType", "Minlvl2EffectType", "MinEnvironment", "MinDoi", "minConcEffect", "CI95_LCL", "firstQuartileConcEffect", "meanConcEffect", "medianConcEffect", "thirdQuartileConcEffect", "CI95_UCL", "MaxConcEffect", "SDConcEffect", "CountEffect", "MinConcTested", "MaxConcTested", "CountTotal")
    #reorder
    aoc_z_join_order <- aoc_z_join[, col_order]
    
    #'print'
    aoc_z_join_order
  })
  
  
  #print summarize filtered data in data table
  output$aoc_filter_ssd_table <- DT::renderDataTable(server = FALSE,{ #server= FALSE prints ALL data, not just what's shown
    dose_check_ssd <- input$dose_check_ssd
    req(input$SSDgo)
    
    datatable(aoc_filter_ssd(),
              extensions = c('Buttons'),
              options = list(
                dom = 'Brtip',
                buttons = list(I('colvis'), c('copy', 'csv', 'excel')),
                scrollY = 400,
                scrollH = TRUE,
                sScrollX = TRUE,
                columnDefs = list(list(width = '50px, targets = "_all'))),#only display the table and nothing else
              colnames = c("Group", "Species", paste0("Most Sensitive Concentration ",  dose_check_ssd), "Min Conc. Broad Endpoint", "Min Conc. Specfic Endpoint", "Min Environment", "DOI", "Minimum Effect Concentration", "95% Lower CI Effect Concentration", "1st Quartile Effect Concentration", "Average Effect Concentration", "Median Effect Concentration", "3rd Quartile Effect Concentration", "95% Upper CI Concentration", "Maximum Observed Effect Concentration", "Std Dev Effect Concentration", "Number of doses with Effects", "Min Concentration Tested (with or without effects)", "Max Concentration Tested (with or without effects)", "Total # Doses Considered"),
              caption = "Filtered Data") %>% 
      formatStyle(
        "Conc",
        backgroundColor = '#a9d6d6')
  })

  # Use newly created dataset from above to generate SSD
 # ** Prediction ---- 
  #create distribution based on newly created dataset
  fit_dists <- reactive({
    req(input$SSDgo) #won't run unless submit button is pressed
    aoc_ssd <- aoc_filter_ssd() #static
    
    ssd_fit_dists(aoc_filter_ssd(), #data frame
                  left = "Conc", #string of the column in data with the concentrations
                  # right = left, #string of the column with the right concentration values. If different from left, then the data are considerd to be censored
                 dists = c("weibull", "llogis", "lnorm", "gamma", "lgumbel"), #char vector of distribution anmes
                 computable = FALSE, #flag specifying whether to only return fits with numerically computable standard errors
                silent = FALSE) #flag indicating whether fits should fail silently
  }) 
  
  #create an autoplot of the distributions
  output$autoplot_dists_react <- renderPlot({
    req(input$SSDgo) #won't run unless submit button is pressed
    
    autoplot(fit_dists())
  })
  
  #back end create goodness of fit table
  gof <- reactive({
    req(input$SSDgo) #won't run unless submit button is pressed
    
    ssd_gof(fit_dists()) %>%
      mutate_if(is.numeric, ~ signif(., 3)) %>%
      arrange(delta) #orders by delta of fit
  }) 
  
  #Render table for goodness of fit
  output$table_gof_react <- DT::renderDataTable(server= FALSE,{  #prints ALL data, not just what's shown 
    req(gof())
    gof <- gof() %>% 
      mutate_if(is.numeric, ~ signif(., 3))
    
     datatable(gof,
              extensions = 'Buttons',
              options = list(
                dom = 'Brt', #buttons, processing display element, table
                 buttons = c('copy', 'csv', 'excel')
                 ),
              class = "compact",
              colnames = c("Distribution", "Anderson-Darling","Kolmogorv Smirnov", "Cramer-Von Mises", "Akaike's Information Criteria", "Akaike's Information Criteria (Corrected for sample size)", "Bayesian Information Criteria", "delta", "weight"),
              caption = "Distributions and their according fit paramaters are displayed",
              selection = list(c(6), target = 'column')
              )
  })
  
  #SLOW STEP: Make a dataframe (aoc_pred) of the estimated concentration (est) with standard error (se) and lower (lcl) and upper (ucl) 95% confidence limits by percent of species affected (percent). The confidence limits are estimated using parametric bootstrapping.
    aoc_pred <- eventReactive(list(input$ssdPred),{
      # eventReactive explicitly delays activity until you press the button
      # here we'll use the inputs to create a new dataset that will be fed into the prediction below
      
    pred_c_ave_ssd <- as.logical(input$pred_ave_ssd) #assign prediction averaging choice
    pred_c_ic_ssd <- input$pred_ic_ssd #assign prediction information criteria choice
    nbootNum <- as.numeric(input$nbootInput) #assign  number of bootsrap samples
    dist_c <- input$dist #assign input to selected distribution
    
    if(pred_c_ave_ssd == TRUE){
    set.seed(99)
    stats::predict(fit_dists(), #Predict fitdist. 
            average = pred_c_ave_ssd, #flag tells whether or not to average models from user input
            ic = pred_c_ic_ssd, #tells which information criteria to use - user input
            nboot = nbootNum, #number of bootstrap samples to use to estimate SE and CL
            ci= TRUE) #estimates confidence intervals
    }
    
    else{
      set.seed(99)
      predict(fit_dists(), #Predict fitdist. 
                     average = pred_c_ave_ssd, #flag tells whether or not to average models from user input
                     ic = pred_c_ic_ssd, #tells which information criteria to use - user input
                     nboot = nbootNum, #number of bootstrap samples to use to estimate SE and CL
                     ci= TRUE) %>%  #estimates confidence intervals
        as.data.frame() %>% 
        filter(dist == dist_c)
    }
    
  }) 
 
# **SSD Plot ----
#Create the plot for species sensitivity distribution
SSD_plot_react <- reactive({
    req(aoc_pred()) #won't start until prediction is complete
    pred_c_hc_ssd <- as.numeric(input$pred_hc_ssd) #assign hazard concentration from numeric input
    #determine if particles of mass will be used
    dose_check_ssd <- input$dose_check_ssd #assign whether or not to use particles/mL or mass/mL
  
    aoc_ssd <- aoc_filter_ssd() %>% arrange(Conc) #static
    
    aoc_ssd$frac <- ppoints(aoc_ssd$Conc, 0.5)
    #convert hazard concentration to sig digits
    aochc <- aoc_hc()
    aochc$est_format <-format(aochc$est, digits = 3, scientific = TRUE)
    
    ## generate plot from prediction ##
  ssd_plot(
     aoc_ssd, #data
     aoc_pred(), #prediction
     color = "Group",
     label = "Species",
     xlab = dose_check_ssd,
     ci = TRUE, #confidence interval plotting
     ribbon = TRUE,
     hc = pred_c_hc_ssd) + #percent hazard concentration
     scale_fill_viridis_d() + #make colors more differentiable 
     scale_colour_viridis_d() +  #make colors more differentiable 
     expand_limits(x = c(0.000000000001,5000)) + # to ensure the species labels fit
    geom_text(data = aochc, aes(x = est, y = 0, label = paste0(percent, "% Hazard Confidence Level")), color = "red", size = 4) + #label for hazard conc
    geom_text(data = aochc, aes(x = est, y = -0.05, label = est_format), color = "red") #label for hazard conc
    
      })
  
# print the SSD plot    
output$SSD_plot <- renderPlot({
  SSD_plot_react()
  })
    
    
# Create downloadable png of ssd plot
output$downloadSsdPlot <- downloadHandler(
  
  filename = function() {
    paste('SSD_plot', Sys.Date(), '.png', sep='')
  },
  content = function(file) {
    # #define user inputs
     # width <- isolate(input$user_width)
     # height <- isolate(input$user_height)
    device <- function(..., width, height) {
      grDevices::png(..., width = 10, height = 8, res = 250, units = "in")
    }
    ggsave(file, plot = ssd_ggplot(), device = device)
  })

  # ***Sub-plots ----
  #Determine Hazard Concentration
  
  #Estimate hazard concentration
  aoc_hc <- eventReactive(list(input$ssdPred),{
    
    #user inputs
    pred_c_ave_ssd <- as.logical(input$pred_ave_ssd) #assign prediction averaging choice
    pred_c_ic_ssd <- input$pred_ic_ssd #assign prediction information criteria choice
    pred_c_hc_ssd <- as.numeric(input$pred_hc_ssd) #assign hazard concentration from numeric input
    nbootNum <- as.numeric(input$nbootInput) #assign  number of bootsrap samples
    dist_c <- input$dist
  
      if(pred_c_ave_ssd == TRUE){
    set.seed(99)
    ssd_hc(fit_dists(), #dataset
           percent = pred_c_hc_ssd, #numeric threshold input by user (default is 0.05)
           nboot = nbootNum, # number of bootstrap predictions to make. 10 is minimum, 1,000 is default
           average = pred_c_ave_ssd, #tells whether or not the average models
           ic = pred_c_ic_ssd, #tells which information criteria to use
           ci = TRUE) #flag to estimate confidence intervals using parametric bootstrapping
      }
    
    #create hc based on user choice of distribution
    else{
      ssd_hc(fit_dists(), #dataset
             percent = pred_c_hc_ssd, #numeric threshold input by user (default is 0.05)
             nboot = nbootNum, # number of bootstrap predictions to make. 10 is minimum, 1,000 is default
             average = pred_c_ave_ssd, #tells whether or not the average models
             ic = pred_c_ic_ssd, #tells which information criteria to use
             ci = TRUE) %>%  #flag to estimate confidence intervals using parametric bootstrapping
        as.data.frame() %>% 
        filter(dist == dist_c)
        }
  })
  
#Plot SSD data with ggplot
   ssd_ggplot <- reactive({
     
     req(input$ssdPred) #won't start until button is pressed for prediction
     
     #Theme type
     theme.type<-switch(input$theme.type,
                       "light" 	= theme_gray(base_size = 15),
                       "dark" = dark_theme_bw(base_size = 15)) 
     #color selection
     fill.type <- switch(input$color.type,
                         "viridis" = scale_fill_viridis(discrete = TRUE),
                         "brewer" =  scale_fill_brewer(palette = "Paired"),
                         "tron" = scale_fill_tron(),
                         "locusZoom" = scale_fill_locuszoom(),
                         "d3" = scale_fill_d3(),
                         "Nature" = scale_fill_npg(),
                         "JAMA" = scale_fill_jama())
     #color selection
     color.type <- switch(input$color.type,
                         "viridis" = scale_color_viridis(discrete = TRUE),
                         "brewer" =  scale_color_brewer(palette = "Paired"),
                         "tron" = scale_color_tron(),
                         "locusZoom" = scale_color_locuszoom(),
                         "d3" = scale_color_d3(),
                         "Nature" = scale_color_npg(),
                         "JAMA" = scale_color_jama())

     
     dose_check_ssd <- input$dose_check_ssd #assign whether or not to use particles/mL or mass/mL
     aoc_ssd <- aoc_filter_ssd() %>% arrange(Conc) #static
    
     #calcualte fraction
    aoc_ssd$frac <- ppoints(aoc_ssd$Conc, 0.5)
    
    #convert hazard concentration to sig digits
    aochc <- aoc_hc()
    
    aochc$est_format <-format(aochc$est, digits = 3, scientific = TRUE)
    
    #build ggplot
    ggplot(aoc_pred(),aes_string(x = "est")) +
      geom_xribbon(aes_string(xmin = "lcl", xmax = "ucl", y = "percent/100"), alpha = 0.2, color = "grey") +
      geom_line(aes_string(y = "percent/100"), color = "gray") +
      geom_point(data = aoc_ssd,aes(x = Conc, y =frac, color = Group)) + 
      geom_text_repel(data = aoc_ssd, aes(x = Conc, y = frac, label = Species, color = Group), nudge_x = 0.2, size = 4, segment.alpha = 0.5) + #species labels
      scale_y_continuous("Species Affected (%)", labels = scales::percent, limits = c(0,1)) +
      #expand_limits(x = c(0.000000001, 100000)) + #ensure species labels fit
      # reactive x axis based on alignment
      xlab(ifelse(input$ERM_check_ssd == "Unaligned", dose_check_ssd,
           paste0(dose_check_ssd, " (",input$lower_length_ssd, " to ",input$upper_length_ssd, " um)"))
           )+
      labs(
        title = "Microplastics Species Sensitivity Distribution",
             subtitle = paste("(ERM = ",input$ERM_check_ssd,")")) +
      coord_trans(x = "log10") +
      scale_x_continuous(breaks = scales::trans_breaks("log10", function(x) 10^x, n = 15),
                         labels = trans_format("log10", scales::math_format(10^.x))) + #comma_signif)+
      geom_segment(data = aochc,aes(x = est, y = percent/100, xend = est, yend = est), linetype = 'dashed', color = "red", size = 1) + #hazard conc line vertical
      geom_segment(data = aochc,aes(x = lcl, y = percent/100, xend = est, yend = percent/100), linetype = 'dashed', color = "red", size = 1) + #hazard conc line horizontal
      geom_text(data = aochc, aes(x = est, y = 0.15, label = paste0(percent, "% Hazard Confidence Level")), color = "red", size = 5) + #label for hazard conc
      geom_text(data = aochc, aes(x = est, y = 0.10, label = paste0(est_format, " ", dose_check_ssd)), color = "red", size = 5) + #label for hazard conc
      geom_label(data = aoc_pred(), aes(x = 100000, y = -0.05, label = paste0("distribution:", dist)), color = "darkcyan", size = 5) + #label for distribution
      fill.type + #user-selected
      color.type + #user-selected
      theme.type + #user theme
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))
  })
  
  
  output$aoc_ssd_ggplot <- renderPlot({
    ssd_ggplot()
  })
  
  
  #hover text for info
  output$info <- renderText({
    dose_check_ssd <- input$dose_check_ssd #assign whether or not to use particles/mL or mass/mL
    
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0(dose_check_ssd, " = ", format(e$x,scientific = TRUE), " percent =", percent(e$y), "\n")
    }
    
    paste0(
      "", xy_str(input$plot_hover)
    )
  })
  
  # SSD Table

  output$ssd_pred_table <- DT::renderDataTable(server = FALSE,{ #server= FALSE prints ALL data, not just what's shown
    dose_check_ssd <- input$dose_check_ssd #assign whether or not to use particles/mL or mass/mL
     req(aoc_pred())
    aoc_pred <- aoc_pred() %>% 
      mutate_if(is.numeric, ~ signif(., 3))
    
      datatable(aoc_pred,
                rownames = FALSE,
                extensions = c('Buttons', 'Scroller'),
                options = list(
                  dom = 'Brtip',
                  scrollY = 400,
                  scroller = TRUE,
                  buttons = c('copy', 'csv', 'excel')
                ), 
                class = "compact",
                colnames = c("Percent", paste0("Estimated Mean Concentration ",  dose_check_ssd), paste0("Standard Error ",  dose_check_ssd), "Lower 95% Confidence Limit", "Upper 95% Confidence Limit", "Distribution"),
                caption = "Predicted species sensitivity distribution concentrations with uncertanties."
                )
  })

  # Cullen and Frey Graph
  output$ssd_CF_plot <- renderPlot({
    req(nrow(aoc_filter_ssd())>0)
    #reactive to static
    aoc_SSD <- aoc_filter_ssd()
    
    #log10 transform vector
    logConc <- log10(subset(aoc_SSD)$Conc) 
    
    #print plot
    descdist(logConc,boot=1000)
  })
  
  #alt fits with fitdistrplus package
  aocSSDFitLNorm <- reactive({
    #reactive to static
    aocSSD <- aoc_filter_ssd()
    #subset and define variable
    Conc <- aocSSD$Conc
    
    #fit log-normal distribution
    fitdist(Conc, "lnorm")
  })
  
  #alt fits with fitdistrplus package
  aocSSDFitLlogis <- reactive({
    #reactive to static
    aocSSD <- aoc_filter_ssd()
    #subset and define variable
    Conc <- aocSSD$Conc
    
    #fit logistic distribution
    fitdist(Conc, "logis")
  })
  
  #alt fits with fitdistrplus package
  aocSSDFitWeibull <- reactive({
    #reactive to static
    aocSSD <- aoc_filter_ssd()
    #subset and define variable
    Conc <- aocSSD$Conc
    
    #fit logistic distribution
    fitdist(Conc, "weibull")
  })
  
  #QQ plot
  output$ssd_qq_plot <- renderPlot({
    #req
    req(nrow(aoc_filter_ssd())>0)
    
    #reactive to static
    aocFitLNorm <- aocSSDFitLNorm()
   # aocFitLogis <- aocSSDFitLogis() #not working right now
    aocFitWeibull <- aocSSDFitWeibull()
    
    #plot
    qqcomp(list(aocFitLNorm,
      #aocFitLogis,
      aocFitWeibull),
           legendtext=c("log-normal", "Weibull"))
  })
  
  #pp plot
  output$ssd_pp_plot <- renderPlot({
    #req
    req(nrow(aoc_filter_ssd())>0)
    
    #reactive to static
    aocFitLNorm <- aocSSDFitLNorm()
    # aocFitLogis <- aocSSDFitLogis()
    aocFitWeibull <- aocSSDFitWeibull()
    #plot
  ppcomp(list(aocFitLNorm, 
              #aocFitLogis, 
              aocFitWeibull),
         legendtext=c("log-normal",
                      #"logistic",
                      "weibull"))
  })
  
  #Histogram
  output$ssd_dens_plot <- renderPlot({
    #req
    req(nrow(aoc_filter_ssd())>0)
    
    #reactive report x-axis
    dose_check_ssd <- input$dose_check_ssd
    
    #reactive to static
    aocSSD <- aoc_filter_ssd()
    #subset and define variable
    logConc <- log10(aocSSD$Conc)
    
    #fit  distributions to log10 data
    aocFitNorm <- fitdist(logConc, "norm")
    #aocFitlogis <- fitdist(logConc, "logis")
    
    
    #plot densities
  denscomp(list(aocFitNorm),#, aocFitLogis),
           legendtext=c("log-normal"),#,"log-logistic"),
           xlab = paste0("log10 ",dose_check_ssd))
  })

 
  
  
  } #Server end

#### Full App ####
shinyApp(ui = ui, server = server)


# End of R Shiny app script.
