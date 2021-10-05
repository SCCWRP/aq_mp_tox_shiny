#### Aquatic Microplastics Toxicology Shiny App
#### File created: September 23, 2020
#### Code contributors: Heili Lowman, Leah Thornton Hampton, Scott Coffin, Emily Darin

#### Setup ####

# Load packages

# Load packages
library(tidyverse) #General everything
library(shinydashboard)
library(RColorBrewer) #color palette
library(ggplot2) #plotting
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
library(shinyjs) #Exploration tab - reset button
library(tigerstats) #turns things into percents
library(ggbeeswarm) #plot all points
library(plotly)
library(ggdark) #dark mode ggplot
library(ggsci) #color palettes
library(collapsibleTree) #plot type for endpoint category tree
library(hrbrthemes) #theme for screening plot
library(ggrepel)

# Load finalized dataset.
aoc <- read_csv("AquaticOrganisms_Clean_final.csv", guess_max = 10000)

#### Welcome Setup ####

# All text inputs below.

#### Overview Setup ####

polydf<-rowPerc(xtabs( ~polymer +effect, aoc)) #pulls polymers by effect 
polyf<-as.data.frame(polydf)%>% #Makes data frame
  mutate(effect = case_when(effect == "Y" ~ "Yes",
                            effect == "N" ~ "No")) %>% 
  filter(effect %in% c("Yes","No"))%>% #Sorts into Yes and No
  mutate(polymer = factor(case_when(
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
    polymer == "PLA" ~ "Polylactic Acid",
    polymer == "Not Reported" ~ "Not Reported"))) %>%
  mutate_if(is.numeric, round,0) #rounds percents 
Endpoints<-xtabs(~polymer +effect ,aoc) #Pulls all study obs. for polymer from dataset
polyfinal<- data.frame(cbind(polyf, Endpoints))%>% #adds it as a column
  rename(Endpoints='Freq.1')%>% #renames column
  mutate(logEndpoints = log(Endpoints))%>%
  rename(Percent = Freq)#renames column

sizedf<-rowPerc(xtabs(~size.category +effect, aoc))
sizef<-as.data.frame(sizedf)%>%
  mutate(effect = case_when(effect == "Y" ~ "Yes",
                            effect == "N" ~ "No")) %>% 
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
  mutate(effect = case_when(effect == "Y" ~ "Yes",
                            effect == "N" ~ "No")) %>% 
  filter(effect %in% c("Yes","No"))%>%
  rename(Type="shape")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Shape")%>%
  mutate(Type = case_when(
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
  mutate(effect = case_when(effect == "Y" ~ "Yes",
                            effect == "N" ~ "No")) %>% 
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
  mutate(effect = case_when(effect == "Y" ~ "Yes",
                            effect == "N" ~ "No")) %>% 
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
  mutate(effect = case_when(effect == "Y" ~ "Yes",
                            effect == "N" ~ "No")) %>% 
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
  mutate(effect = case_when(effect == "Y" ~ "Yes",
                            effect == "N" ~ "No")) %>% 
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
  mutate(effect = case_when(effect == "Y" ~ "Yes",
                            effect == "N" ~ "No")) %>% 
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

#### Exploration/General Setup ####

#### Particle Characteristics Equations ####
#surface area equation for elongated spheres (fragments)
SAfnx = function(a, # length
                 b, # width
                 c){ # height
  SA = 4*pi*(((a*b)^1.6 + (a*c)^1.6 + (b*c)^1.6) / 3)^(1/1.6)
  return(SA)}

# equation for volume
volumefnx_poly = function(width, length){
  height = width
  volume = (4/3) * pi * (length/2) * (width/2) * (height/2) 
  return(volume)}

massfnx_poly = function(width, length, p){
  height = width
  volume = (4/3) * pi * (length/2) * (width/2) * (height/2)  
  mass = p * #density (g/cm^3)
    volume * # volume (um^3): assumes height = 0.67 * Width, and Width:Length ratio is 'R' (compartment-specific)
    1/1e12 * 1e6 #correction factor
  return(mass)}

#### Ecologically Relevant Metric Functions (used in reactives with user-input params) ####

###function to derive correction factor (CF) from Koelmans et al (equation 2)
CFfnx = function(a, #default alpha from Koelmans et al (2020)
                 x2D, #set detault values to convert ranges to (1-5,000 um) #5mm is upper defuault 
                 x1D, #1 um is lower default size
                 x2M, x1M){
  CF = (x2D^(1-a)-x1D^(1-a))/(x2M^(1-a)-x1M^(1-a)) 
  return(CF)}

#### equations for mu_x_poly (note that there are three depending on certain alphas for limits of equation)
##### if alpha does not equal 2 #####
mux.polyfnx = function(a.x, 
                       x_UL, 
                       x_LL){
  mux.poly = ((1-a.x)/(2-a.x)) * ((x_UL^(2-a.x) - x_LL^(2-a.x))/(x_UL^(1-a.x) - x_LL^(1-a.x)))
  return(mux.poly)}

##### If alpha does equal 2 #####
mux.polyfnx.2 = function(x_UL,x_LL){
  mux.poly = (log(x_UL/x_LL))/(x_LL^(-1) - x_UL^-1)
  return(mux.poly)}

#max ingestible specific surface area
SSA.inversefnx = function(sa, #surface area, calcaulted elsewhere
                          m){ #mass, calculated elsewhere
  SSA.inverse = m / sa
  return(SSA.inverse)}


# Master dataset for scatterplots - for Heili's tab.
aoc_v1 <- aoc %>% # start with original dataset
  #Remove 26C temperature treatment data from Jaimukar et al. 2018 (necessary to get same thresholds as in manuscript)
  filter(!(article == 42 & media.temp == 26)) %>% 
  # full dataset filters.
  mutate(effect_f = factor(case_when(effect == "Y" ~ "Yes",
                                     effect == "N" ~ "No"),
                           levels = c("No", "Yes"))) %>%
  # removing NAs to make data set nicer
  replace_na(list(size.category = 0, shape = "Not Reported", polymer = "Not Reported", life.stage = "Not Reported", chem.exp.typ.nominal = "Particle Only",
                  tech.tier.zero = "Scoring Not Applicable", risk.tier.zero = "Scoring Not Applicable"))

aoc_setup <- aoc_v1 %>% # start with original dataset
  mutate(size_f = factor(case_when(
    size.category == 1 ~ "1nm < 100nm",
    size.category == 2 ~ "100nm < 1µm",
    size.category == 3 ~ "1µm < 100µm",
    size.category == 4 ~ "100µm < 1mm",
    size.category == 5 ~ "1mm < 5mm",
    size.category == 0 ~ "Not Reported"),
    levels = c("1nm < 100nm", "100nm < 1µm", "1µm < 100µm", "100µm < 1mm", "1mm < 5mm", "Not Reported"))) %>% # creates new column with nicer names and order by size levels.
  # shape category data tidying.
  mutate(shape_f = factor(case_when(
    shape == "fiber" ~ "Fiber",
    shape == "fragment" ~ "Fragment",
    shape == "sphere" ~ "Sphere",
    shape == "Not Reported" ~ "Not Reported"),
    levels = c("Fiber", "Fragment", "Sphere", "Not Reported"))) %>% # order our different shapes.
  # polymer category data tidying.
  mutate(poly_f = factor(case_when(
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
    polymer == "PLA" ~ "Polylactic Acid",
    polymer == "Not Reported" ~ "Not Reported"))) %>%
  # taxonomic category data tidying.
  mutate(org_f = factor(organism.group, levels = c("Algae", "Annelida", "Bacterium", "Cnidaria", "Crustacea",
                                                   "Echinoderm", "Fish", "Insect", "Mollusca", "Nematoda", "Plant", "Rotifera", "Mixed"))) %>% # order our different organisms.
  mutate(lvl1_f = factor(case_when(lvl1 == "alimentary.excretory" ~ "Alimentary, Excretory",
                                   lvl1 == "behavioral.sense.neuro" ~ "Behavioral, Sensory, Neurological",
                                   lvl1 == "circulatory.respiratory" ~ "Circulatory, Respiratory",
                                   lvl1 == "community" ~ "Community",
                                   lvl1 == "fitness" ~ "Fitness",
                                   lvl1 == "immune" ~ "Immune",
                                   lvl1 == "metabolism" ~ "Metabolism",
                                   lvl1 == "microbiome" ~ "Microbiome",
                                   lvl1 == "stress" ~ "Stress"))) %>% # creates new column with nicer names.
  # Level 2 Data tidying
  mutate(lvl2_f = factor(case_when(lvl2 == "abundance"~"Abundance",
                                   lvl2 == "actinobacteria" ~ "Actinobacteria",
                                   lvl2 == "aggressivity"~"Agressivity",
                                   lvl2 == "ammonia.excretion" ~ "Ammonia Excretion",
                                   lvl2 == "bacteroidetes"~ "Bacteriodetes",
                                   lvl2 == "blood"~"Blood",
                                   lvl2 == "body.condition"~"Body Condition",
                                   lvl2 == "boldness"~"Boldness",
                                   lvl2 == "brain.histo"~"Brain Histological Abnormalities",
                                   lvl2 == "burrowing"~"Burrowing",
                                   lvl2 == "carb.metabolism"~"Carb Metabolism",
                                   lvl2 == "chemokines.cytokines"~"Chemokines",
                                   lvl2 == "circulatory"~"Circulatory",
                                   lvl2 == "detoxification"~"Detoxification",
                                   lvl2 == "development"~"Development",
                                   lvl2 == "digestion"~"Digestion",
                                   lvl2 == "digestive.enzymes"~"Digestive Enzymes",
                                   lvl2 == "digestive.tract.histo"~"Digestive Tract Histological Abnormalities",
                                   lvl2 == "diversity"~ "Diversity",
                                   lvl2 == "feeding"~ "Feeding",
                                   lvl2 == "firmicutes"~ "Firmicutes",
                                   lvl2 == "gall.bladder.histo" ~ "Gall Bladder Histological Abnormalities",
                                   lvl2 == "gen.metabolism"~ "General Metabolism",
                                   lvl2 == "gill.histo"~ "Gill Histological Abnormalities",
                                   lvl2 == "gonad.histo"~"Gonad Histological Abnormalities",
                                   lvl2 == "growth"~ "Growth",
                                   lvl2 == "immune.cells"~"Immune Cells",
                                   lvl2 == "immune.other"~"Immune Other ",
                                   lvl2 == "intestinal.permeability"~"Intestinal Permeability",
                                   lvl2 == "kidney.histo"~"Kidney Histological abnormalities",
                                   lvl2 == "lipid.metabolism"~"Lipid Metabolism",
                                   lvl2 == "liver.histo"~"Liver Histological Abnormalities",
                                   lvl2 == "liver.kidney.products" ~ "Liver and Kidney Products",
                                   lvl2 == "locomotion"~"Locomotion",
                                   lvl2 == "mortality"~"Mortality",
                                   lvl2 == "nervous.system"~"Nervous System",
                                   lvl2 == "oxidative.stress"~"Oxidative Stress",
                                   lvl2 == "photosynthesis"~ "Photosynthesis",
                                   lvl2 == "proteobacteria"~"Protebacteria",
                                   lvl2 == "reproduction"~"Reproduction",
                                   lvl2 == "respiration"~"Respiration",
                                   lvl2 == "sexhormones"~"Sex Hormones",
                                   lvl2 == "shoaling"~"Shoaling",
                                   lvl2 == "stress"~"Stress",
                                   lvl2 == "vision.system"~"Vision System"))) %>% #Renames for widget
  #Level 3 endpoint tidying
  mutate(lvl3_f = factor(case_when(lvl3 == "11bhsd.mrnaexpression"~"11β HSD mRNA expression",
                                   lvl3 == "17bhsd.mrnaexpression"~"17β HSD mRNA expression",
                                   lvl3 == "1433e.mrnaexpression"~"1433e mRNA expression",
                                   lvl3 == "3bhsd.mrnaexpression"~"3β HSD mRNA expression",
                                   lvl3 == "5ht1.mrnaexpression"~"5ht1 mRNA expression",
                                   lvl3 == "aadd45a.mrnaexpression"~"aadd45a mRNA expression",
                                   lvl3 == "abcb1.mrnaexpression"~"abcb1 mRNA expression",
                                   lvl3 == "abcc.mrnaexpression"~"abcc mRNA expression",
                                   lvl3 == "abcc5.mrnaexpression"~"abcc5 mRNA expression",
                                   lvl3 == "abcd.mrnaexpression"~"abcd mRNA expression",
                                   lvl3 == "abnormality.occurrence"~"Abnormality Occurrence",
                                   lvl3 == "abs.rc"~"Absorption Flux/Reaction Center",
                                   lvl3 == "absorption.efficiency"~"Absorption Efficiency",
                                   lvl3 == "abundance"~"Abundance",
                                   lvl3 == "acc1.mrnaexpression"~"acc1 mRNA expression",
                                   lvl3 == "ace.diversity.index"~"ACE Diversity Index",
                                   lvl3 == "ache.activity"~"AcHE Activity",
                                   lvl3 == "ache.mrnaexpression"~"AcHE mRNA expression",
                                   lvl3 == "acid.phosphatase.activity"~"Acid Phosphatase Activity",
                                   lvl3 == "aco.mrnaexpression"~"aco mRNAexpression",
                                   lvl3 == "acrosomal.integrity"~"Acrosomal Integrity",
                                   lvl3 == "actinobacteria"~"Actinobacteria",
                                   lvl3 == "activity"~"Activity",
                                   lvl3 == "activity.during.feeding"~"Activity During Feeding",
                                   lvl3 == "aeromonas"~"Aeromonas",
                                   lvl3 == "agal.consumption"~"Agal Consumption",
                                   lvl3 == "agression.index"~"Aggression Index",
                                   lvl3 == "ahra.mrnaexpression"~"ahrα mRNA expression",
                                   lvl3 == "ahrb.mrnaexpression"~"ahrβ mRNA expression",
                                   lvl3 == "akt.mrnaexpression"~"akt mRNA expression",
                                   lvl3 == "albumin.con"~"Albumin Concentration",
                                   lvl3 == "albumin.globulin.ratio"~"Albumin to Globulin Ratio",
                                   lvl3 == "algal.density"~"Algal Density",
                                   lvl3 == "alkaline.phosphatase.activity"~"Alkaline Phosphatase Activity",
                                   lvl3 == "aproteobacteria"~"α proteobacteria",
                                   lvl3 == "alp.activity"~"Alkaline Phosphatase Activity",
                                   lvl3 == "alp.con"~"Alkaline Phosphatase Concentration",
                                   lvl3 == "alt.activity"~"Alanine Aminotransferase Activity",
                                   lvl3 == "alt.con"~"Alanine Aminotransferase Concentration",
                                   lvl3 == "amino.acid.trans"~"Amino Acid Transport",
                                   lvl3 == "ammonia.con"~"Ammonia Concentration",
                                   lvl3 == "ampka.mrnaexpression"~"ampka mRNA expression",
                                   lvl3 == "ampkb.mrnaexpression"~"ampkb mRNA expression",
                                   lvl3 == "ampkc.mrnaexpression"~"ampkc mRNA expression",
                                   lvl3 == "amylase.mrnaexpression"~"Amylanse mRNA expression",
                                   lvl3 == "antioxidant.act"~"Antioxidant Activity",
                                   lvl3 == "aox.activity"~"Alternative Oxidase Activity",
                                   lvl3 == "apo.mrnaexpression"~"Apolipoprotein mRNA expression",
                                   lvl3 == "ar.mrnaexpression"~"Androgen Receptor mRNA expression",
                                   lvl3 == "ara.mrnaexpression"~"Androgen Receptor α mRNA expression",
                                   lvl3 == "arb.mrnaexpression"~"Androgen Receptor β mRNA expression",
                                   lvl3 == "area"~"Area",
                                   lvl3 == "area.traveled"~"Area Traveled",
                                   lvl3 == "aspartate.aminotransferase.con"~"Aspartate Aminotransferase Concentration",
                                   lvl3 == "assemblage"~"Assemblage",
                                   lvl3 == "assimilation.efficiency"~"Assimilation Efficiency",
                                   lvl3 == "ast.activity"~"Aspartate Aminotransferase Activity",
                                   lvl3 == "ast.con"~"Aspartate Aminotransferase Concentration",
                                   lvl3 == "attachment"~"Attachment",
                                   lvl3 == "atubulin.mrnaexpression"~"α tubulin mRNA expression",
                                   lvl3 == "atubulin.proteinexpression"~"α tubulin Protein expression",
                                   lvl3 == "bacteroidetes"~"Bacteroidetes",
                                   lvl3 == "bcl2.mrnaexpression"~"bcl2 mRNA expression",
                                   lvl3 == "bfcod.activity"~"BFCOD Activity",
                                   lvl3 == "bile.acid.con"~"Bile Acid Concentration",
                                   lvl3 == "bilirubin.con"~"Bilirubin Concentration",
                                   lvl3 == "biomass"~"Biomass",
                                   lvl3 == "bioturbation"~"Bioturbation",
                                   lvl3 == "body.length"~"Body Length",
                                   lvl3 == "body.mass"~"Body Mass",
                                   lvl3 == "body.width"~"Body Width",
                                   lvl3 == "bproteobacteria"~"βproteobacteria",
                                   lvl3 == "brain.histo"~"Brain Histology",
                                   lvl3 == "brain.mass.index"~"Brain Mass Index",
                                   lvl3 == "brain.water.content"~"Brain Water Content",
                                   lvl3 == "burrow.length"~"Burrow Length",
                                   lvl3 == "burrow.litter"~"Burrow Litter",
                                   lvl3 == "burrow.no"~"Burrow Number",
                                   lvl3 == "burrow.vol"~"Burrow Volume",
                                   lvl3 == "burrow.walldensity"~"Burrow Wall Density",
                                   lvl3 == "burrow.wallweight"~"Burrow Wall Weight",
                                   lvl3 == "burrowing"~"Burrowing",
                                   lvl3 == "ca.mrnaexpression"~"ca mRNA expression",
                                   lvl3 == "calcification"~"Calcification",
                                   lvl3 == "calcium.con"~"Calcium Concentration",
                                   lvl3 == "capture.rate"~"Capture Rate",
                                   lvl3 == "carbohydrate.reserves"~"Carbohydrate Reserves",
                                   lvl3 == "cardiolipin.con"~"Cardiolipin Concentration",
                                   lvl3 == "cas8.mrnaexpression"~"cas8 mRNA expression",
                                   lvl3 == "casp3.mrnaexpression"~"casp3 mRNA expression",
                                   lvl3 == "casp373.mrnaexpression"~"casp373 mRNA expression",
                                   lvl3 == "casp3a.mrnaexpression"~"casp3a mRNA expression",
                                   lvl3 == "casp8.mrnaexpression"~"casp8 mRNA expression",
                                   lvl3 == "casp9.mrnaexpression"~"casp9 mRNA expression",
                                   lvl3 == "caspase3.7.mrnaexpression"~"caspase 3/7 mRNA expression",
                                   lvl3 == "cat.activity"~"Catalase Activity",
                                   lvl3 == "cat.mrnaexpression"~"Catalase mRNA expression",
                                   lvl3 == "cell.autoflourescence"~"Cell Autoflourescence",
                                   lvl3 == "cell.density"~"Cell Density",
                                   lvl3 == "cell.granularity"~"Cell Granularity",
                                   lvl3 == "cell.morphology"~"Cell Morphology",
                                   lvl3 == "cell.necrosis"~"Cell Necrosis",
                                   lvl3 == "cell.size"~"Cell Size",
                                   lvl3 == "cell.viability"~"Cell Viability",
                                   lvl3 == "cellular.energy.allocation"~"Cellular Energy Allocation",
                                   lvl3 == "chg.mrnaexpression"~"Choriogenin mRNA expression",
                                   lvl3 == "chgh.mrnaexpression"~"Choriogenin H mRNA expression",
                                   lvl3 == "chgl.mrnaexpression"~"Choriogenin L mRNA expression",
                                   lvl3 == "chlorophyll"~"Chlorophyll",
                                   lvl3 == "cholesterol.con"~"Cholesterol Concentration",
                                   lvl3 == "chrna1.mrnaexpression"~"chrna1 mRNA expression",
                                   lvl3 == "chymotrypsin.activity"~"Chymotrypsin Activity",
                                   lvl3 == "clearance.rate"~"Clearance Rate",
                                   lvl3 == "clutch.num"~"Number of Clutches",
                                   lvl3 == "clutch.size"~"Clutch Size",
                                   lvl3 == "clutch.size.first"~"First Clutch Size",
                                   lvl3 == "clutch.size.firstthree"~"First Three Clutch Sizes",
                                   lvl3 == "co2.consumption"~"CO2 Consumption",
                                   lvl3 == "condition.index"~"Condition Index",
                                   lvl3 == "congestion"~"Congestion",
                                   lvl3 == "copper.con.hepatopancreas"~"Hepatopancreas Copper Concentration",
                                   lvl3 == "cox.activity"~"Cyclooxygenase Activity",
                                   lvl3 == "coxIV.mrnaexpression"~"Cyclooxygenase IV mRNA expression",
                                   lvl3 == "cpt1.mrnaexpression"~"CPT1 mRNA expression",
                                   lvl3 == "creatine.kinase.con"~"Creatine Kinase Concentration",
                                   lvl3 == "crude.ash"~"Crude Ash",
                                   lvl3 == "crude.protein"~"Curde Protein",
                                   lvl3 == "cs.mrnaexpression"~"Chitin Synthase mRNA expression",
                                   lvl3 == "ctsl.mrnaexpression"~"Cathepsin-L mRNA expression",
                                   lvl3 == "cuznsod.mrnaexpression"~"CuZn Superoxide Dismutase mRNA expression",
                                   lvl3 == "cxcr5.mrnaexpression"~"cxcr5 mRNA expression",
                                   lvl3 == "cyp11.mrnaexpression"~"cyp11 mRNA expression",
                                   lvl3 == "cyp11a2.mrnaexpression"~"cyp11a2 mRNA expression",
                                   lvl3 == "cyp17a1.mrnaexpression"~"cyp17a1 mRNA expression",
                                   lvl3 == "cyp19a.mrnaexpression"~"cyp19a mRNA expression",
                                   lvl3 == "cyp19a2.mrnaexpression"~"cyp19a2 mRNA expression",
                                   lvl3 == "cyp19b.mrnaexpression"~"cyp19b mRNA expression",
                                   lvl3 == "cyp1a.mrnaexpression"~"cyp1a  mRNA expression",
                                   lvl3 == "cyp1a1.mrnaexpression"~"cyp1a1 mRNA expression",
                                   lvl3 == "cyp32.mrnaexpression"~"cyp32 mRNA expression",
                                   lvl3 == "cyp450.proteinexpression"~"cyp450 mRNA expression",
                                   lvl3 == "cyp4an1.mrnaexpression"~"cyp4an1 mRNA expression",
                                   lvl3 == "cyp4c33.mrnaexpression"~"cyp4c33 mRNA expression",
                                   lvl3 == "cyp4c34.mrnaexpression"~"cyp4c34 mRNA expression",
                                   lvl3 == "cytoplasmic.membrane.potential"~"Cytoplasmic Membrane Potential",
                                   lvl3 == "cytotoxicity"~"Cytotoxicity",
                                   lvl3 == "d.lactate"~"D Lactate Concentration",
                                   lvl3 == "dao.activity"~"Diamine Oxidase Activity",
                                   lvl3 == "degranulation"~"Degranulation",
                                   lvl3 == "development"~"Development",
                                   lvl3 == "developmental.abnormalities"~"Developmental Abnormalities",
                                   lvl3 == "dgat.mrnaexpression"~"Diglyceride acyltransferase mRNA expression",
                                   lvl3 == "digestive.tract.histo"~"Digestive Tract Histology",
                                   lvl3 == "dio.rc"~"Energy Dissipated/Reaction Center",
                                   lvl3 == "distance.between.fish"~"Distance Between Fish",
                                   lvl3 == "distance.during.feeding"~"Distance During Feeding",
                                   lvl3 == "diversity"~"Diversity",
                                   lvl3 == "dlac.activity"~"DLac Activity",
                                   lvl3 == "dna.damage"~"DNA Damage",
                                   lvl3 == "dopamine.con"~"Dopamine Concentration",
                                   lvl3 == "egg.size"~"Egg Size",
                                   lvl3 == "embryotoxicity"~"Embryotoxicity",
                                   lvl3 == "emergence"~"Emergence",
                                   lvl3 == "endopeptidase.activity"~"Endopeptidase Activity",
                                   lvl3 == "energy.balance"~"Energy Balance",
                                   lvl3 == "energy.reserves"~"Energy Reserves",
                                   lvl3 == "ep.mrnaexpression"~"ep mRNA expression",
                                   lvl3 == "era.mrnaexpression"~"Estrogen Receptor α mRNA expression",
                                   lvl3 == "erb.mrnaexpression"~"Estrogen Receptor β mRNA expression",
                                   lvl3 == "erk.mrnaexpression"~"erk mRNA expression",
                                   lvl3 == "erod.activity"~"EROD Activity",
                                   lvl3 == "esr2.mrnaexpression"~"esr2 mRNA expression",
                                   lvl3 == "esterase.activity"~"Esterase Activity",
                                   lvl3 == "estradiol.level"~"Estradiol Levels",
                                   lvl3 == "estradiol.testosterone.ratio"~"Estradiol to Testosterone Ratio",
                                   lvl3 == "eto.abs"~"ETO/ABS",
                                   lvl3 == "eto.rc"~"ETO/RC",
                                   lvl3 == "exopeptidase.activity"~"Exopeptidase Activity",
                                   lvl3 == "exploration"~"Exploration",
                                   lvl3 == "f0.fm"~"f0/fm",
                                   lvl3 == "f0.fv"~"f0/fv",
                                   lvl3 == "fabp6.mrnaexpression"~"fabp6 mRNA expression",
                                   lvl3 == "fadd.mrnaexpression"~"fadd mRNA expression",
                                   lvl3 == "fas.mrnaexpression"~"fas mRNA expression",
                                   lvl3 == "fatty.vac"~"Fatty Vacuoles",
                                   lvl3 == "fecundity"~"Fecundity",
                                   lvl3 == "feeding.activity"~"Feeding Activity",
                                   lvl3 == "feeding.rate"~"Feeding Rate",
                                   lvl3 == "feeding.time"~"Feeding Time",
                                   lvl3 == "fertilization"~"Fertilization",
                                   lvl3 == "fertilization.rate"~"Fertilization Rate",
                                   lvl3 == "fertilization.yield"~"Fertilization Yield",
                                   lvl3 == "fibrosis"~"Fiborsis",
                                   lvl3 == "filter.feeding"~"Filter Feeding",
                                   lvl3 == "firmicutes"~"Firmicutes",
                                   lvl3 == "food.consumption"~"Food Consumption",
                                   lvl3 == "foraging.activity"~"Foraging Activity",
                                   lvl3 == "foraging.time"~"Foraging Time",
                                   lvl3 == "foxl2.mrnaexpression"~"fox12 mRNA expression",
                                   lvl3 == "free.fatty.acid.con"~"Free Fatty Acid Concentration",
                                   lvl3 == "fshb.mrnaexpression"~"fshβ mRNA expression",
                                   lvl3 == "fshr.mrnaexpression"~"fshr mRNA expression",
                                   lvl3 == "ftzf1.mrnaexpression"~"ftzf1 mRNA expression",
                                   lvl3 == "fv'.fm'"~"fv'/fm",
                                   lvl3 == "fv.f0"~"fv/f0",
                                   lvl3 == "fv.fm"~"fv/fm",
                                   lvl3 == "fv.fm.sm.tfm"~"fv/fm/sm/tfm",
                                   lvl3 == "gadph.mrnaexpression"~"gadhph mRNA expression",
                                   lvl3 == "gall.bladder.histo"~"Gall Bladder Histology",
                                   lvl3 == "gallery.vol"~"Gallery Volume",
                                   lvl3 == "gcl.mrnaexpression"~"gcl mRNA expression",
                                   lvl3 == "gcl1.mrnaexpression"~"gcl1 mRNA expression",
                                   lvl3 == "gclcs.mrnaexpression"~"gclcs mRNA expression",
                                   lvl3 == "gclcu.mrnaexpression"~"gclcu mRNA expression",
                                   lvl3 == "genera"~"Genera",
                                   lvl3 == "gfap.mrnaexpression"~"gfap mRNA expression",
                                   lvl3 == "ggt.activity"~"Gamma-Glutamyl Transferase Activity",
                                   lvl3 == "ggt.con"~"Gamma-Glutamyl Transferase Concentration",
                                   lvl3 == "gill.histo"~"Gill Histology",
                                   lvl3 == "gk.mrnaexpression"~"gk mRNA expression",
                                   lvl3 == "globulin.con"~"Globulin Concentration",
                                   lvl3 == "glucose.con"~"Glucose Concentration",
                                   lvl3 == "glutamate.con"~"Glutamate Concentration",
                                   lvl3 == "glutathione.con"~"Glutathione Concentration",
                                   lvl3 == "glycogen.con"~"Glycogen Concentration",
                                   lvl3 == "glycogen.deplet"~"Glycogen Depletion",
                                   lvl3 == "gnrh.mrnaexpression"~"gnrh mRNA expression",
                                   lvl3 == "gnrhr.mrnaexpression"~"gnrhr mRNA expression",
                                   lvl3 == "gonad.development.index"~"Gonad Development Index",
                                   lvl3 == "gonad.histo"~"Gonad Histology",
                                   lvl3 == "gonad.regression"~"Gonad Regression",
                                   lvl3 == "gonad.somatic.index"~"Gonad Somatic Index",
                                   lvl3 == "gordonia"~"Gordonia",
                                   lvl3 == "got.activity"~"Glutamic Oxaloacetic Transaminase Activity",
                                   lvl3 == "gpt.activity"~"Glutamic Pyruvic Transaminase Activity",
                                   lvl3 == "gpx.activity"~"Glutathione Peroxidase Activity",
                                   lvl3 == "gpx.mrnaexpression"~"Glutathione Peroxidase mRNA expression",
                                   lvl3 == "gpx4b.mrnaexpression"~"gpx4b mRNA expression",
                                   lvl3 == "gr.activity"~"Glutathione Reductase Activity",
                                   lvl3 == "gr.mrnaexpression"~"Glutathione Reductase mRNA expression",
                                   lvl3 == "granulocyte.con"~"Granulocyte Concentration",
                                   lvl3 == "granulocyte.hyalinocyte.ratio"~"Granulocyte to Hyalinocyte Ratio",
                                   lvl3 == "granulocyte.oxidative.activity"~"Granulocyte Oxidative Activity",
                                   lvl3 == "granulocyte.size"~"Granulocyte Size",
                                   lvl3 == "gross.energy"~"Gross Energy",
                                   lvl3 == "growth"~"Growth",
                                   lvl3 == "growth.rate"~"Growth Rate",
                                   lvl3 == "gs.mrnaexpression"~"Glutathione Synthetase mRNA expression",
                                   lvl3 == "gsh-px.activity"~"Glutathione Peroxidase Activity",
                                   lvl3 == "gsh.activity"~"Glutathione Peroxidase Activity",
                                   lvl3 == "gsh.con"~"Glutathione Peroxidase Concentration",
                                   lvl3 == "gsp.activity"~"Glutathione Peroxidase Activity",
                                   lvl3 == "gst.activity"~"Glutathione S-Transferase Activity",
                                   lvl3 == "gst.mrnaexpression"~"Glutathione S-Transferase mRNA expression",
                                   lvl3 == "gst4.mrnaexpression"~"gst4 mRNA expression",
                                   lvl3 == "gstd.mrnaexpression"~"gstd mRNA expression",
                                   lvl3 == "gstp1.mrnaexpression"~"Glutathione S-Transferase mRNA expression",
                                   lvl3 == "gt.con"~"Glutathione Concentration",
                                   lvl3 == "gtha.mrnaexpression"~"gtha mRNA expression",
                                   lvl3 == "gusb.mrnaexpression"~"gusb mRNA expression",
                                   lvl3 == "gut.mucus.vol"~"Gut Mucus Volume",
                                   lvl3 == "gyrus.size"~"Gyrus Size",
                                   lvl3 == "h2o2.con"~"Hydorgen Peroxide Concentration",
                                   lvl3 == "h3.mrnaexpression"~"h3 mRNA expression",
                                   lvl3 == "haemocyanin.con"~"Haemocyanin Concentration",
                                   lvl3 == "hemocyte.con"~"Hemocyte Concentration",
                                   lvl3 == "haemolymph.con"~"Haemolymph Concentration",
                                   lvl3 == "hatching.success"~"Hatching Success",
                                   lvl3 == "hdl.con"~"HDL Concentration",
                                   lvl3 == "heart.rate"~"Heart Rate",
                                   lvl3 == "hepatic.somatic.index"~"Hepatic Somatic Index",
                                   lvl3 == "hepatopancreas.water.con"~"Hepatopancreas Water Concentration",
                                   lvl3 == "hex.mrnaexpression"~"hex mRNA expression",
                                   lvl3 == "hk.mrnaexpression"~"hk mRNA expression",
                                   lvl3 == "hk1.mrnaexpression"~"hk1 mRNA expression",
                                   lvl3 == "ho1.mrnaexpression"~"ho1 mRNA expression",
                                   lvl3 == "hsp70.mrnaexpression"~"hsp70 mRNA expression",
                                   lvl3 == "hsp90.mrnaexpression"~"hsp90 mRNA expression",
                                   lvl3 == "hyalinocyte.size"~"Hyalinocyte Size",
                                   lvl3 == "hylaniocyte.oxidative.activity"~"Hylaniocyte Oxidative Activity",
                                   lvl3 == "id.mrnaexpression"~"Isocitrate Dehydrogenase mRNA expression",
                                   lvl3 == "idh.activity"~"Isocitrate Dehydrogenase Activity",
                                   lvl3 == "idp.mrnaexpression"~"idp mRNA expression",
                                   lvl3 == "ifn.mrnaexpression"~"Interferon mRNA expression",
                                   lvl3 == "ifny.mrnaexpression"~"Interferon y mRNA expression",
                                   lvl3 == "igm.con"~"Immunoglobulin M Concentration",
                                   lvl3 == "igm.mrnaexpression"~"Immunoglobulin M mRNA expression",
                                   lvl3 == "il10.mrnaexpression"~"Interleukin 10 mRNA expression",
                                   lvl3 == "il17.mrnaexpression"~"Interleukin 17 mRNA expression",
                                   lvl3 == "il1a.mrnaexpression"~"Interleukin 1α mRNA expression",
                                   lvl3 == "il1b.mrnaexpression"~"Interleukin 1β mRNA expression",
                                   lvl3 == "il413a.mrnaexpression"~"Interleukin 413α mRNA expression",
                                   lvl3 == "il6.mrnaexpression"~"Interleukin 6 mRNA expression",
                                   lvl3 == "il8.mrnaexpression"~"Interluekin 8 mRNA expression",
                                   lvl3 == "ila.con"~"Interleukin 1α Concentration",
                                   lvl3 == "immobilization"~"Immobilization",
                                   lvl3 == "inflam.infiltrate"~"Inflammatory Filtrate",
                                   lvl3 == "ingestion.rate"~"Ingestion Rate",
                                   lvl3 == "inhibitory.rate"~"Inhibitory Rate",
                                   lvl3 == "instar.develop.time"~"Instar Development Time",
                                   lvl3 == "intermolt.time"~"Intermolt Time",
                                   lvl3 == "intestinal.ca.con"~"Intestinal Calcium Concentration",
                                   lvl3 == "ion.exch.ca"~"Ion Exchange (Calcium)",
                                   lvl3 == "ion.exch.k"~"Ion Exchange (Potassium)",
                                   lvl3 == "ion.exch.na"~"Ion Exchange (Sodium)",
                                   lvl3 == "ion.flow"~"Ion Flow",
                                   lvl3 == "ion.permeability"~"Ion Permeability",
                                   lvl3 == "ion.trans"~"Ion Transport",
                                   lvl3 == "jaw.tissue.damage"~"Jaw Tissue Damage",
                                   lvl3 == "jnk.mrnaexpression"~"jnk mRNA expression",
                                   lvl3 == "jump.freq"~"Jump Frequency",
                                   lvl3 == "jump.height"~"Jump Height",
                                   lvl3 == "kidney.histo"~"Kidney Histology",
                                   lvl3 == "labilization.period"~"Labilization Period",
                                   lvl3 == "lactate.con"~"Lactate Concentration",
                                   lvl3 == "ldh.activity"~"Lactate Dehydrogenase Activity",
                                   lvl3 == "ldh.con"~"Lactate Dehydrogenase Concentration",
                                   lvl3 == "ldl.con"~"LDL Concentration",
                                   lvl3 == "leaf.growth.rate"~"Leaf Growth Rate",
                                   lvl3 == "length"~"Length",
                                   lvl3 == "leukocyte.con"~"Leukocyte Concentration",
                                   lvl3 == "lhb.mrnaexpression"~"Leutinizing Hormone β mRNA expression",
                                   lvl3 == "lhr.mrnaexpression"~"Leutinizing Hormone Receptor mRNA expression",
                                   lvl3 == "lifespan"~"Lifespan",
                                   lvl3 == "light.capture"~"Light Capture",
                                   lvl3 == "light.dark.activity"~"Light/Dark Activity",
                                   lvl3 == "line.crossing"~"Line Crossing",
                                   lvl3 == "lipase.activity"~"Lipase Activity",
                                   lvl3 == "lipase.con"~"Lipase Concentration",
                                   lvl3 == "lipid.content"~"Lipid Content",
                                   lvl3 == "lipid.membrane.con"~"Lipid Membrane Concentration",
                                   lvl3 == "lipid.perox"~"Lipid Peroxidation",
                                   lvl3 == "lipid.storage.con"~"Lipid Storage Concentration",
                                   lvl3 == "lipofuscin.accum"~"Lipofuscin Accumulation",
                                   lvl3 == "liver.histo"~"Liver Histology",
                                   lvl3 == "liver.protein"~"Liver Protein",
                                   lvl3 == "lymphocyte.con"~"Lymphocyte Concentration",
                                   lvl3 == "lys.mrnaexpression"~"Lysozyme mRNA expression",
                                   lvl3 == "lysosomal.membrane.stab"~"Lysosomal Membrane Stability",
                                   lvl3 == "lysozyme.activity"~"Lysozyme Activity",
                                   lvl3 == "manf.mrnaexpression"~"manf mRNA expression",
                                   lvl3 == "mao.activity"~"Monoamine Oxidase Activity",
                                   lvl3 == "mapk.phosphorylation"~"MAPK Phosphorylation",
                                   lvl3 == "mbp.mrnaexpression"~"Myelin Basic Protein mRNA expression",
                                   lvl3 == "mbp.proteinexpression"~"Myelin Basic Protein Expression",
                                   lvl3 == "mda.con"~"Malondialdehyde Concentration",
                                   lvl3 == "meer1.mrnaexpression"~"meer 1 mRNA expression",
                                   lvl3 == "meer2.mrnaexpression"~"meer 2 mRNA expression",
                                   lvl3 == "mek.mrnaexpression"~"mek mRNA expression",
                                   lvl3 == "metamorphosis.yield"~"Metamorphosis Yield",
                                   lvl3 == "mgnrh.mrnaexpression"~"mgnrh mRNA expression",
                                   lvl3 == "min.sat.irradiance"~"Minimum Saturation Irradiance",
                                   lvl3 == "mirconuclei.freq"~"Micronuclei Frequency",
                                   lvl3 == "mitochondrial.mem.integrity"~"Mitochondrial Membrane Integrity",
                                   lvl3 == "mnsod.mrnaexpression"~"Mn Superoxide Dismutase mRNA expression",
                                   lvl3 == "moisture"~"Moisture",
                                   lvl3 == "molecular.permeability"~"Molecular Permeability",
                                   lvl3 == "molt.period.length"~"Molt Period Length",
                                   lvl3 == "mortality"~"Mortality",
                                   lvl3 == "mt10.mrnaexpression"~"mt10 mRNA expression",
                                   lvl3 == "mt20.mrnaexpression"~"mt20 mRNA expression",
                                   lvl3 == "mta.mrnaexpression"~"mta mRNA expression",
                                   lvl3 == "mtb.mrnaexpression"~"mtb mRNA expression",
                                   lvl3 == "mxr.efflux"~"MXR Efflux",
                                   lvl3 == "mytc.mrnaexpression"~"mytc mRNA expression",
                                   lvl3 == "myticina.mrnaexpression"~"myticin α mRNA expression",
                                   lvl3 == "mytilinb.mrnaexpression"~"mytilin β mRNA expression",
                                   lvl3 == "nauplius.to.adult"~"Nauplius to Adult",
                                   lvl3 == "nauplius.to.copepodid"~"Nauplius to Copepodid",
                                   lvl3 == "neutrophil.extracell.trap"~"Neutrophil Extracellular Trap",
                                   lvl3 == "ngn1.mrnaexpression"~"ngn1 mRNA expression",
                                   lvl3 == "nitrogen.conversion"~"Nitrogen Conversion",
                                   lvl3 == "no.prod"~"Nitric Oxide Production",
                                   lvl3 == "nrf2.mrnaexpression"~"nrf2 mRNA expression",
                                   lvl3 == "num.indiv.nosnails"~"Number of Individuals (No Snails)",
                                   lvl3 == "num.indiv.snails"~"Number of Individuals (With Snails)",
                                   lvl3 == "num.offspring"~"Number of Offspring",
                                   lvl3 == "number.of.molts"~"Number of Molts",
                                   lvl3 == "number.of.roots"~"Number of Roots",
                                   lvl3 == "occludin.mrnaexpression"~"occludin mRNA expression",
                                   lvl3 == "offspring.size"~"Offspring Size",
                                   lvl3 == "oocyte.diameter"~"Oocyte Diameter",
                                   lvl3 == "oocyte.num"~"Oocyte Number",
                                   lvl3 == "oxygen.consumption"~"Oxygen Consumption",
                                   lvl3 == "p38.mrnaexpression"~"p38 mRNA expression",
                                   lvl3 == "p38mapk.mrnaexpression"~"p38mapk mRNA expression",
                                   lvl3 == "p53.mrnaexpression"~"p53 mRNA expression",
                                   lvl3 == "pc.pe.ratio"~"PC/PE Ratio",
                                   lvl3 == "pcc.level"~"Protein Carbonylation Content",
                                   lvl3 == "pche.activity"~"Pseudocholinesterase Activity",
                                   lvl3 == "pepckc.mrnaexpression"~"pepckc mRNA expression",
                                   lvl3 == "percent.dstage.larvae"~"Pertengage of D-stage Larvae",
                                   lvl3 == "percent.dveliger"~"Percentage of Veliger Larvae",
                                   lvl3 == "percent.motile.sperm"~"Percentage of Motile Sperm",
                                   lvl3 == "percent.tank.used"~"Percentage of Tank Used",
                                   lvl3 == "percent.trocophore.larvae"~"Percentage of Trocophore Larvae",
                                   lvl3 == "pericardial.area"~"Pericardial Area",
                                   lvl3 == "perox.activity"~"Peroxidase Activity",
                                   lvl3 == "perox.mrnaexpression"~"Peroxidase mRNA expression",
                                   lvl3 == "petb.mrnaexpression"~"petb mRNA expression",
                                   lvl3 == "pglycoprotein.activity"~"Pglycoprotein Activity",
                                   lvl3 == "pgp.mrnaexpression"~"pgp mRNA expression",
                                   lvl3 == "pgrp.mrnaexpression"~"pgrp mRNA expression",
                                   lvl3 == "phagocytosis"~"Phagocytosis",
                                   lvl3 == "phenoloxydase.activity"~"Phenoloxydase Activity",
                                   lvl3 == "phosphorus.con"~"Phosphorus Concentration",
                                   lvl3 == "phosphatase.activity"~"Phosphatase Activity",
                                   lvl3 == "phosphatidylcholine.con"~"Phosphatidylcholine Concentration",
                                   lvl3 == "phosphatidylethanolamine.con"~"Phosphatidylethanolamine Concentration",
                                   lvl3 == "phosphatidylinositol.ceramide.amino.ethylphosphonate.con"~"PCAE Concentration",
                                   lvl3 == "phosphatidylserine.con"~"Phosphatidylserine Concentration",
                                   lvl3 == "photochemical.efficiency"~"Photochemical Efficiency",
                                   lvl3 == "pk.mrnaexpression"~"pk mRNA expression",
                                   lvl3 == "polyp.activity"~"Polyp Activity",
                                   lvl3 == "population.size"~"Population Size",
                                   lvl3 == "potassium.con"~"Potassium Concentration",
                                   lvl3 == "ppara.mrnaexpression"~"pparα mRNA expression",
                                   lvl3 == "ppary.mrnaexpression"~"ppary mRNA expression",
                                   lvl3 == "prdx1.mrnaexpression"~"prdx1 mRNA expression",
                                   lvl3 == "prdx2.mrnaexpression"~"prdx2 mRNA expression",
                                   lvl3 == "prdx3.mrnaexpression"~"prdx3 mRNA expression",
                                   lvl3 == "prdx5.mrnaexpression"~"prdx5 mRNA expression",
                                   lvl3 == "predation.risk"~"Predation Risk",
                                   lvl3 == "predatory.performance"~"Predatory Performance",
                                   lvl3 == "pregnancy.rate"~"Pregnancy Rate",
                                   lvl3 == "protein.carbonyl.con"~"Protein Carbonyl Concentration",
                                   lvl3 == "protein.con"~"Protein Concentration",
                                   lvl3 == "protein.reserves"~"Protein Reserves",
                                   lvl3 == "proteobacteria"~"proteobacteria",
                                   lvl3 == "ps2"~"Photosystem II",
                                   lvl3 == "psaa.mrnaexpression"~"psaa mRNA expression",
                                   lvl3 == "psab.mrnaexpression"~"psab mRNA expression",
                                   lvl3 == "psba.mrnaexpression"~"psba mRNA expression",
                                   lvl3 == "psbl.mrnaexpression"~"psbl mRNA expression",
                                   lvl3 == "pseudomonas"~"pseudomonas",
                                   lvl3 == "rapid.light.resp.curve"~"Rapid Light Response Curve",
                                   lvl3 == "rbc.con"~"Red Blood Cell Concentration",
                                   lvl3 == "rbcl.mrnaexpression"~"rbcl mRNA expression",
                                   lvl3 == "rc.abs"~"RC/ABS",
                                   lvl3 == "redox.state"~"Redox State",
                                   lvl3 == "relative.e.transport.rate"~"Relative e Transport State",
                                   lvl3 == "relocate"~"Relocation",
                                   lvl3 == "reproduction.factor"~"Reproduction Factor",
                                   lvl3 == "reproduction.inhibition"~"Reproduction Inhibition",
                                   lvl3 == "reproductive.freq"~"Reproductive Frequency",
                                   lvl3 == "resistance.time"~"Resistance Time",
                                   lvl3 == "respiration.rate"~"Respiration Rate",
                                   lvl3 == "respiratory.burst"~"Respiratory Burst",
                                   lvl3 == "retrmax"~"Maximal Election Transport Rate",
                                   lvl3 == "root.length"~"Root Length",
                                   lvl3 == "ros.prod"~"Reactive Oxygen Species Production",
                                   lvl3 == "s100a1.mrnaexpression"~"s100a1 mRNA expression",
                                   lvl3 == "saa.mrnaexpression"~"saa mRNA expression",
                                   lvl3 == "sd.mrnaexpression"~"sd mRNA expression",
                                   lvl3 == "segment.regeneration"~"Segment Regeneration",
                                   lvl3 == "serotonin.con"~"Serotonin Concentration",
                                   lvl3 == "settling.rate"~"Settling Rate",
                                   lvl3 == "sex.ratio"~"Sex Ratio",
                                   lvl3 == "sexual.maturity"~"Sexual Maturity",
                                   lvl3 == "sgst.mrnaexpression"~"sgst mRNA expression",
                                   lvl3 == "shannon.diversity.index"~"Shannon Diversity Index",
                                   lvl3 == "shell.growth"~"Shell Growth",
                                   lvl3 == "shell.length"~"Shell Length",
                                   lvl3 == "shell.weight"~"Shell Weight",
                                   lvl3 == "size"~"Size",
                                   lvl3 == "sod.activity"~"Superoxide Dismutase Activity",
                                   lvl3 == "sod.mrnaexpression"~"sod mRNA expression",
                                   lvl3 == "sod1.mrnaexpression"~"sod1 mRNA expression",
                                   lvl3 == "sod3.mrnaexpression"~"sod3 mRNA expression",
                                   lvl3 == "sodium.con"~"Sodium Concentration",
                                   lvl3 == "species.diversity"~"Species Diversity",
                                   lvl3 == "species.richness"~"Species Richness",
                                   lvl3 == "specific.growth.rate"~"Specific Growth Rate",
                                   lvl3 == "sperm.velocity"~"Sperm Velocity",
                                   lvl3 == "srebp1a.mrnaexpression"~"srebp1a mRNA expression",
                                   lvl3 == "star.mrnaexpression"~"star mRNA expression",
                                   lvl3 == "sterol.con"~"Sterol Concentration",
                                   lvl3 == "surface.area"~"Surface Area",
                                   lvl3 == "swim.distance"~"Swim Distance",
                                   lvl3 == "swim.speed"~"Swim Speed",
                                   lvl3 == "swimbladder.inflation"~"Swimbladder Inflation",
                                   lvl3 == "synapsin.mrnaexpression"~"synapsin mRNA expression",
                                   lvl3 == "taxa.num"~"Number of Taxa",
                                   lvl3 == "testosterone.level"~"Testosterone Levels",
                                   lvl3 == "tgfb.mrnaexpression"~"tgfb mRNA expression",
                                   lvl3 == "thiol.con"~"Thiol Concentration",
                                   lvl3 == "thrombocyte.con"~"Thrombocyte Concentration",
                                   lvl3 == "time.to.hatch"~"Time to Hatch",
                                   lvl3 == "time.to.maturation"~"Time to Maturation",
                                   lvl3 == "time.to.nauplius"~"Time to Nauplius",
                                   lvl3 == "time.to.offspring"~"Time to Offspring",
                                   lvl3 == "tlr.mrnaexpression"~"tlr mRNA expression",
                                   lvl3 == "tnfa.mrnaexpression"~"tnfα mRNA expression",
                                   lvl3 == "tnfsf13b.mrnaexpression"~"tnfsf13b mRNA expression",
                                   lvl3 == "total.distance.traveled"~"Total Distance Travelled",
                                   lvl3 == "tp53.con"~"tp53 Concentration",
                                   lvl3 == "tp53.mrnaexpression"~"tp53 mRNA expression",
                                   lvl3 == "tph2.mrnaexpression"~"tph2 mRNA expression",
                                   lvl3 == "tricellulin.mrnaexpression"~"tricellulin mRNA expression",
                                   lvl3 == "triglyceride.cholesterol.ratio"~"Tryglyceride Cholesterol Ratio",
                                   lvl3 == "triglycerides.con"~"Triglyceride Concentration",
                                   lvl3 == "tro.rc"~"TRO/RC",
                                   lvl3 == "trypsin.activity"~"Trypsin Activity",
                                   lvl3 == "tumor.promotion"~"Tumor Promotion",
                                   lvl3 == "turn.angle.light.dark.activity"~"Turn Angle, Light/Dark Activity",
                                   lvl3 == "ucp1.mrnaexpression"~"ucp1 mRNA expression",
                                   lvl3 == "ugst.mrnaexpression"~"ugst mRNA expression",
                                   lvl3 == "uricacid.con"~"Uric Acid Concentration",
                                   lvl3 == "viserosomatic.index"~"Viserosomatic Index",
                                   lvl3 == "volume"~"Volume",
                                   lvl3 == "vtg.mrnaexpression"~"vtg mRNA expression",
                                   lvl3 == "vtg.proteinexpression"~"vtg protein expression",
                                   lvl3 == "vtg1.mrnaexpression"~"vtg1 mRNA expression",
                                   lvl3 == "vtg2.mrnaexpression"~"vtg2 mRNA expression",
                                   lvl3 == "weight.change"~"Weight Change",
                                   lvl3 == "wgst.mrnaexpression"~"wgst mRNA expression",
                                   lvl3 == "wnt16.mrnaexpression"~"wnt16 mRNA expression",
                                   lvl3 == "wnt4.mrnaexpression"~"wnt4 mRNA expression",
                                   lvl3 == "yproteobacteria"~"y proteobacteria",
                                   lvl3 == "zfblue.mrnaexpression"~"zfblue mRNA expression",
                                   lvl3 == "zfrho.mrnaexpression"~"zfrho mRNA expression",
                                   lvl3 == "zo1.mrnaexpression"~"zo1 mRNA expression",
                                   lvl3 == "zoozanthelle.density"~"Zoozanthelle Density"))) %>%
  mutate(bio_f = factor(case_when( #Bio Org Data Tidying
    bio.org == "subcell"~"Subcell",
    bio.org == "cell"~"Cell",
    bio.org == "tissue" ~ "Tissue",
    bio.org == "organism"~"Organism",
    bio.org == "population"~ "Population")))%>%
  mutate(vivo_f = factor(case_when(invitro.invivo == "invivo"~"In Vivo",
                                   invitro.invivo == "invitro"~"In Vitro")))%>% ##Renames for widget (Not using a widget right now, but saving for human health database)
  mutate(life_f = factor(case_when(life.stage == "Early"~"Early",
                                   life.stage == "Juvenile"~"Juvenile",
                                   life.stage == "Adult"~"Adult",
                                   life.stage == "Not Reported"~"Not Reported")))%>% #Renames for widget
  mutate(env_f = factor(case_when(environment == "Freshwater"~"Freshwater",
                                  environment == "Marine" ~ "Marine",
                                  environment == "Terrestrial" ~ "Terrestrial"))) %>%
  mutate(species_f = as.factor(paste(genus,species))) %>% 
  mutate(dose.mg.L.master.converted.reported = factor(dose.mg.L.master.converted.reported)) %>%
  mutate(dose.particles.mL.master.converted.reported = factor(dose.particles.mL.master.converted.reported)) %>% 
  mutate(effect.metric = factor(effect.metric)) %>% #factorize
  mutate(af.time_noNA = replace_na(af.time, "Unavailable")) %>% 
  mutate(acute.chronic_f = factor(case_when(af.time_noNA == 10 ~ "Acute",
                                            af.time_noNA == 1 ~ "Chronic",
                                            af.time_noNA == "Unavailable" ~ "Unavailable"))) %>% #factorize assesment factor time into chronic/acute
  mutate(tier_zero_tech_f = factor(case_when(tech.tier.zero == "Fail" ~ "Red Criteria Failed",
                                             tech.tier.zero == "Pass" ~ "Red Criteria Passed",
                                             tech.tier.zero == "Scoring Not Applicable" ~ "Scoring Not Applicable"))) %>% 
  mutate(tier_zero_risk_f = factor(case_when(risk.tier.zero == "Fail" ~ "Red Criteria Failed",
                                             risk.tier.zero == "Pass" ~ "Red Criteria Passed",
                                             risk.tier.zero == "Scoring Not Applicable" ~ "Scoring Not Applicable"))) %>%
  mutate(weather.biofoul_f = factor(case_when(weather.biofoul == "Y" ~ "Yes",
                                     weather.biofoul == "N" ~ "No"))) %>%
  #Paste the range of treatments used within each study - there is often a range because this is different depending on the endpoint
  group_by(doi) %>% 
  mutate(treatment_range = ifelse(min(treatments) == max(treatments), paste0(treatments), paste0(min(treatments),"-",max(treatments)))) %>% 
  ungroup() %>% 
  #calculate maximum ingestible size (if not already in database)
  mutate(max.size.ingest.mm = ifelse(is.na(max.size.ingest.mm), 
                                     10^(0.9341 * log10(body.length.cm) - 1.1200) * 10,  #(Jamm et al 2020 Nature paper)correction for cm to mm
                                     max.size.ingest.mm)) %>%  # if already present, just use that
  mutate(max.size.ingest.um = 1000 * max.size.ingest.mm) %>%  #makes it less confusing below
  #Make factor for experiment type
  mutate(exp_type_f = factor(case_when(leachate.only == "Y" ~ "Leachate",
                                     chem.exp.typ.nominal == "Particle Only" ~ "Particle Only",
                                     chem.exp.typ.nominal == "co.exp" ~ "Chemical Co-Exposure",
                                     chem.exp.typ.nominal == "sorbed" ~ "Chemical Transfer"))) %>%
  # create label for polydispersity
  mutate(polydispersity = case_when(
    is.na(size.length.min.mm.nominal) ~ "monodisperse",
    !is.na(size.length.min.mm.nominal) ~ "polydisperse")) %>% 
  
  ####prioritize measured parameters for conversions ###
  # minima
  mutate(size.length.min.um.used.for.conversions = case_when(
    is.na(size.length.min.mm.measured) ~ size.length.min.mm.nominal * 1000,
    !is.na(size.length.min.mm.measured) ~ size.length.min.mm.measured * 1000)) %>% 
  mutate(size.width.min.um.used.for.conversions = case_when(
    shape == "sphere" ~ size.length.min.um.used.for.conversions, #all dims same
    shape == "fiber" ~ 0.77 * size.length.min.um.used.for.conversions, #median holds for all particles (Kooi et al 2021)
    shape == "Not Reported" ~ 0.77 * size.length.min.um.used.for.conversions, # average width to length ratio in the marine environment (kooi et al 2021)
    shape == "fragment" ~ 0.77 * size.length.min.um.used.for.conversions)) %>% # average width to length ratio in the marine environment (kooi et al 2021)
  mutate(size.height.min.um.used.for.conversions = case_when(
    shape == "sphere" ~ size.length.min.um.used.for.conversions, #all dims same
    shape == "Not Reported" ~ 0.77 * 0.67 * size.length.min.um.used.for.conversions, # average width to length ratio in the marine environment (kooi et al 2021)
    shape == "fiber" ~  0.77 * size.length.min.um.used.for.conversions, #height same as width for fibers
    shape == "fragment" ~ 0.77 * 0.67 * size.length.min.um.used.for.conversions)) %>% # average width to length ratio in the marine environment AND average height to width ratio (kooi et al 2021)
  # maxima
  mutate(size.length.max.um.used.for.conversions = case_when(
    is.na(size.length.max.mm.measured) ~ size.length.max.mm.nominal * 1000,
    !is.na(size.length.max.mm.measured) ~ size.length.max.mm.measured * 1000)) %>% 
  mutate(size.width.max.um.used.for.conversions = case_when(
    shape == "sphere" ~ size.length.max.um.used.for.conversions, #all dims same
    shape == "fiber" ~ 0.77 * size.length.max.um.used.for.conversions, #median holds for all particles (Kooi et al 2021) #there are no fibers
    shape == "Not Reported" ~ 0.77 * size.length.max.um.used.for.conversions, # average width to length ratio in the marine environment (kooi et al 2021)
    shape == "fragment" ~ 0.77 * size.length.max.um.used.for.conversions)) %>% # average width to length ratio in the marine environment (kooi et al 2021)
  mutate(size.height.max.um.used.for.conversions = case_when(
    shape == "sphere" ~ size.length.max.um.used.for.conversions, #all dims same
    shape == "Not Reported" ~ 0.77 * 0.67 * size.length.max.um.used.for.conversions, # average width to length ratio in the marine environment (kooi et al 2021)
    shape == "fiber" ~ 0.77 * size.length.max.um.used.for.conversions, #hieght same as width
    shape == "fragment" ~ 0.77 * 0.67 * size.length.max.um.used.for.conversions)) %>%  # average width to length ratio in the marine environment AND average height to width ratio (kooi et al 2021)
  
  #calculate minimum and maximum surface area for polydisperse particles
  mutate(particle.surface.area.um2.min = SAfnx(a = size.length.min.um.used.for.conversions,
                                               b = size.width.min.um.used.for.conversions,
                                               c = size.height.min.um.used.for.conversions)) %>%
  mutate(particle.surface.area.um2.max = SAfnx(a = size.length.max.um.used.for.conversions,
                                               b = size.width.max.um.used.for.conversions,
                                               c = size.height.max.um.used.for.conversions)) %>% 
  #calculate minimum and maximum volume for polydisperse particles
  mutate(particle.volume.um3.min = volumefnx_poly(length = size.length.min.um.used.for.conversions,
                                                  width =  size.width.min.um.used.for.conversions)) %>% 
  mutate(particle.volume.um3.max = volumefnx_poly(length = size.length.max.um.used.for.conversions,
                                                  width = size.width.max.um.used.for.conversions)) %>% 
  #calculate minimum and maximum volume for polydisperse particles
  mutate(mass.per.particle.mg.min = massfnx_poly(length = size.length.min.um.used.for.conversions,
                                                 width = size.width.min.um.used.for.conversions,
                                                 p = density.g.cm3)) %>% #equation usess g/cm3
  mutate(mass.per.particle.mg.max = massfnx_poly(length = size.length.max.um.used.for.conversions,
                                                 width = size.width.max.um.used.for.conversions,
                                                 p = density.g.cm3)) %>%   #equation usess g/cm3

  #Volume
  mutate(dose.um3.mL.master = particle.volume.um3 * dose.particles.mL.master) %>%  #calculate volume/mL
  
  #Surface Area
  mutate(dose.um2.mL.master = as.numeric(particle.surface.area.um2) * dose.particles.mL.master) %>% 
  
  #Specific Surface Area
  mutate(dose.um2.ug.mL.master = dose.um2.mL.master / (mass.per.particle.mg / 1000)) %>% #correct mg to ug
  
  #Additional tidying for nicer values
  mutate(authors = gsub(".", " & ", as.character(authors), fixed = TRUE)) %>% 
  mutate(exposure.media = gsub(".", " ", as.character(exposure.media), fixed = TRUE)) %>%
  mutate(detergent = gsub(".", " ", as.character(detergent), fixed = TRUE)) %>%
  mutate(chem.add.nominal = gsub(".", " ", as.character(chem.add.nominal), fixed = TRUE)) %>%
  mutate(exposure.route = gsub(".", " ", as.character(exposure.route), fixed = TRUE)) %>% 
  mutate(sol.rinse = gsub(".", " ", as.character(sol.rinse), fixed = TRUE)) %>%
  mutate(sol.rinse = if_else(sol.rinse == "N", "No", sol.rinse)) %>% 
  mutate(uptake.valid.method = gsub(".", " ", as.character(uptake.valid.method), fixed = TRUE)) %>% 
  mutate(clean.method = gsub(".", " ", as.character(clean.method), fixed = TRUE)) %>% 
  mutate(clean.method = if_else(clean.method == "N", "Not Cleaned", clean.method)) %>% 
  mutate(particle.behavior = gsub(".", " ", as.character(particle.behavior), fixed = TRUE)) %>% 
  mutate(particle.behavior = if_else(particle.behavior == "N", "Not Evaluated", particle.behavior)) %>%
  mutate(tissue.distribution = gsub(".", " ", as.character(tissue.distribution), fixed = TRUE))

#### Overview Setup Continued ####

#Endpoint Categorization setup
aoc_endpoint <- aoc_setup %>% 
  group_by(lvl1_f,lvl2_f,lvl3_f,bio_f) %>% 
  summarise()


#### Search Setup ####

aoc_search <- aoc_setup %>%
         #general
  dplyr::select(doi, authors, year, tier_zero_tech_f, tier_zero_risk_f, species_f, org_f, env_f, life_f, vivo_f, sex, body.length.cm, max.size.ingest.mm,
         #experimental parameters
         exp_type_f, exposure.route, mix, negative.control, reference.material, exposure.media, solvent, detergent,
         media.ph, media.sal.ppt, media.temp, media.temp.min, media.temp.max, exposure.duration.d, acute.chronic_f,
         treatments, replicates, sample.size, dosing.frequency, chem.add.nominal, chem.add.dose.mg.L.nominal, chem.add.dose.mg.L.measured,
           #reported doses
           dose.particles.mL.nominal, dose.particles.mL.min.nominal,
           dose.particles.mL.max.nominal, dose.mg.L.nominal, dose.mg.kg.sed.nominal, dose.mg.kg.food.nominal,
           dose.particles.kg.food.nominal, dose.percent.sed.nominal, dose.particles.m2.nominal, dose.mg.m2.nominal, dose.percent.food.nominal,
           dose.particles.kg.sed.nominal, dose.particles.mL.measured, dose.mg.L.measured, dose.mg.kg.sed.measured, dose.mg.kg.food.measured,
           dose.particles.kg.food.measured, dose.particles.kg.food.min.measured, dose.particles.kg.food.max.measured,
           dose.percent.sed.measured, dose.particles.m2.measured, 
           #master doses
           dose.particles.mL.master, dose.particles.mL.master.converted.reported, dose.mg.L.master, dose.mg.L.master.converted.reported,
           dose.um3.mL.master, dose.um2.mL.master, dose.um2.ug.mL.master,
         #biological effects
         effect_f, direction, lvl1_f, lvl2_f, lvl3_f, bio_f, target.cell.tissue, effect.metric, af.time, af.noec,
         #particle characteristics
         poly_f, shape_f, density.g.cm3, density.reported.estimated, charge, zetapotential.mV, zetapotential.media, functional.group,
         size.length.um.used.for.conversions, size.width.um.used.for.conversions, size_f, particle.surface.area.um2, particle.volume.um3,
         mass.per.particle.mg, weather.biofoul_f,
         #quality
         size.valid, polymer.valid, shape.valid, particle.source, sodium.azide, contaminant.screen, clean.method, sol.rinse, background.plastics,
         concentration.valid, particle.behavior, uptake.valid, uptake.valid.method, tissue.distribution, fed) %>%
  #rename 'master' dose columns so they don't get pivoted
  rename("particles/mL (master)" = dose.particles.mL.master, "particles/mL (master), reported or converted" = dose.particles.mL.master.converted.reported,
         "μg/mL (master)" = dose.mg.L.master, "μ/mL (master), reported or converted" = dose.mg.L.master.converted.reported,
         "μm^3/mL (master)" = dose.um3.mL.master, "μm^2/mL (master)" = dose.um2.mL.master, "μm/ug/mL (master)" = dose.um2.ug.mL.master) %>% 
  #pivot non-master dose columns
  pivot_longer(cols = starts_with("dose"),
               names_to = "Original Dose Units",
               values_to = "Original Concentration") %>%  
  mutate(`Original Dose Units Nominal or Measured` = case_when(grepl("nominal", `Original Dose Units`) ~ "nominal",
                                            grepl("measured", `Original Dose Units`) ~ "measured")) %>% 
  mutate(`Original Dose Units` = case_when(grepl("particles.mL.min", `Original Dose Units`) ~ "particles/mL (min)",
                                    grepl("particles.mL.max", `Original Dose Units`) ~ "particles/mL (max)",
                                    grepl("particles.mL", `Original Dose Units`) ~ "particles/mL",
                                    grepl("mg.L", `Original Dose Units`) ~ "μg/mL",
                                    grepl("mg.kg.sed", `Original Dose Units`) ~ "mg/kg (sediment)",
                                    grepl("mg.kg.food", `Original Dose Units`) ~ "mg/kg (food)",
                                    grepl("particles.kg.food.min", `Original Dose Units`) ~ "particles/kg (min,food)",
                                    grepl("particles.kg.food.max", `Original Dose Units`) ~ "particles/kg (max,food)",
                                    grepl("particles.kg.food", `Original Dose Units`) ~ "particles/kg (food)",
                                    grepl("particles.kg.sed", `Original Dose Units`) ~ "particles/kg (sediment)",
                                    grepl("percent.sed", `Original Dose Units`) ~ "% sediment",
                                    grepl("percent.food", `Original Dose Units`) ~ "% food",
                                    grepl("particles.m2", `Original Dose Units`) ~ "particles/m^2",
                                    grepl("mg.m2", `Original Dose Units`) ~ "mg/m^2")) 
 #Turn all character strings into factors if they aren't already so they are searchable via dropdown
 aoc_search[sapply(aoc_search, is.character)] <- lapply(aoc_search[sapply(aoc_search, is.character)], as.factor)

#### SSD Setup ####

# Master dataset for SSDs
aoc_z <- aoc_setup %>% 
  # environment category data tidying.
  mutate(environment.noNA = replace_na(environment, "Not Reported")) %>% # replaces NA to better relabel.
  mutate(env_f = factor(environment.noNA, levels = c("Marine", "Freshwater", "Terrestrial", "Not Reported"))) 
  # final cleanup and factoring  
  aoc_z$Species <- as.factor(paste(aoc_setup$genus,aoc_setup$species)) #must make value 'Species" (uppercase)
  aoc_z$Group <- as.factor(aoc_z$organism.group) #must make value "Group"
  aoc_z$Group <- fct_explicit_na(aoc_z$Group) #makes sure that species get counted even if they're missing a group
  
  
#### Screening Setup ####

  aoc_quality <- aoc_setup %>%
    filter(tier_zero_tech_f != "Scoring Not Applicable") %>% 
    filter(tier_zero_risk_f != "Scoring Not Applicable") %>% 
    mutate(Study = paste0(authors, " (", year,")")) %>%
    mutate(Study_plus = as.factor(paste0(authors, " (", year,")", " (",doi,")"))) %>%
    distinct(Study, Study_plus, doi, treatment_range, tech.a1, tech.a2, tech.a3, tech.a4, tech.a5, tech.a6, tech.1, tech.2, tech.3, tech.4, tech.5,
             tech.6, tech.7, tech.8, tech.9, tech.10, tech.11, tech.12, risk.13, risk.14, risk.15, risk.16, risk.17, risk.18, risk.19, risk.20,
             lvl1_f, lvl2_f, bio_f, effect_f, life_f, poly_f, shape_f, size_f, species_f, env_f, org_f, acute.chronic_f, tier_zero_tech_f, tier_zero_risk_f) %>%   
     
    pivot_longer(!c(Study, Study_plus, doi, treatment_range, lvl1_f, lvl2_f, bio_f, effect_f, life_f, poly_f, shape_f, size_f, species_f, env_f, org_f, acute.chronic_f, tier_zero_tech_f, tier_zero_risk_f),
                 names_to ="Criteria", 
                 values_to ="Score") %>% 
    #Assign descriptions to numerical scores
    mutate(Score_f = factor(case_when(Score == 0 ~ "Inadequate",
                                      Score == 1 ~ "Adequate with Restrictions",
                                      Score == 2 ~ "Adequate"))) %>%
    #Assign each criteria to appropriate category
    mutate(Category = case_when(Criteria == "tech.a1" ~ "Technical",
                                Criteria == "tech.a2" ~ "Technical",
                                Criteria == "tech.a3" ~ "Technical",
                                Criteria == "tech.a4" ~ "Technical",
                                Criteria == "tech.a5" ~ "Technical",
                                Criteria == "tech.a6" ~ "Technical",
                                Criteria == "tech.1" ~ "Technical",
                                Criteria == "tech.2" ~ "Technical",
                                Criteria == "tech.3" ~ "Technical",
                                Criteria == "tech.4" ~ "Technical",
                                Criteria == "tech.5" ~ "Technical",
                                Criteria == "tech.6" ~ "Technical",
                                Criteria == "tech.7" ~ "Technical",
                                Criteria == "tech.8" ~ "Technical",
                                Criteria == "tech.9" ~ "Technical",
                                Criteria == "tech.10" ~ "Technical",
                                Criteria == "tech.11" ~ "Technical",
                                Criteria == "tech.12" ~ "Technical",
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
    mutate(Category_f = factor(Category, levels = c("Technical", "Risk Assessment"))) %>% 
    #Assign descriptions to each criteria
    mutate(Criteria = case_when(Criteria == "tech.a1" ~ "Test Medium*",
                                Criteria == "tech.a2" ~ "Administration Route*",
                                Criteria == "tech.a3" ~ "Test Species*",
                                Criteria == "tech.a4" ~ "Sample Size*",
                                Criteria == "tech.a5" ~ "Control Group*",
                                Criteria == "tech.a6" ~ "Exposure Duration*",
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
                                                    "Source of Microplastics*","Polymer Type*","Particle Shape*","Particle Size*","Exposure Duration*","Control Group*",
                                                    "Sample Size*", "Test Species*", "Administration Route*","Test Medium*"))) 

#### User Interface ####

ui <- dashboardPage(

  dashboardHeader(title = "Toxicity of Microplastics Explorer", titleWidth = 400),

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
                     menuItem("Human Health", href = "https://sccwrp.shinyapps.io/human_mp_tox_shiny-/", icon = icon("user")),
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
                    fluidRow(
                    #top right box
                    column(width = 12, 
                           
                    p(tags$img(src="welcome.png", width = "40%", height = "40%", style = "float:left; display: block; margin-left: auto; margin-right: 30px;")),
                    
                    h3("What is the Microplastics Toxicity Database?", align = "center"), 
                    
                    strong(p("This database is a repository for microplastics 
                      toxicity data for the California Microplastics Health Effects Workshop.")), 
                    
                    p("This web application allows users to explore toxicity 
                    data using an intuitive interface while retaining the diversity and complexity inherent 
                    to microplastics. Data is extracted from existing, peer-reviewed manuscripts containing 
                    toxicity data pertaining to microplastics."),
                  
                    p("Use the side panel on the left of the page to navigate to each section. Each section provides different information or data visualization options. 
                      More specific instructions may be found within each section.")))),
              
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
                        
                        p(align = "center", a(href = "https://www.heililowman.com/", 'Dr. Heili Lowman'),", University of Nevada Reno ", 
                          tags$a(href="https://twitter.com/heili_lowman", icon("twitter")), tags$a(href="https://github.com/hlowman", icon("github"))),
                        
                        p(align = "center", a(href = "https://agency.calepa.ca.gov/staffdirectory/detail.asp?UID=69294&BDO=7&VW=DET&SL=S", 'Dr. Scott Coffin'),", California State Water Resources Control Board", 
                          tags$a(href="https://twitter.com/DrSCoffin", icon("twitter")), tags$a(href="https://github.com/ScottCoffin", icon("github"))),
                        
                        p(align = "center", a(href = "https://www.sccwrp.org/about/staff/emily-darin/", 'Emily Darin'),", Southern California Coastal Water Research Project",
                          tags$a(href="https://github.com/EmilyDarin", icon("github"))),
                        
                        p(align = "center", a(href = "https://www.sfei.org/users/liz-miller", 'Dr. Ezra Miller'),", San Francisco Estuary Institute"),
                        
                        p(align = "center", a(href = "https://rochmanlab.com/people/", 'Dr. Ludovic Hermabessiere'),", University of Toronto", 
                          tags$a(href="https://twitter.com/HermabessiereL", icon("twitter"))),
                        
                        p(align = "center", a(href = "https://rochmanlab.com/people/", 'Hannah De Frond'),", University of Toronto", 
                          tags$a(href="https://twitter.com/HanDefrond", icon("twitter"))),
                        
                        p(align = "center", "Vera de Ruitjer, Wageningen University"),
                        
                        p(align = "center", "Dr. Samreen Siddiqui, Oregon State University"),
                        
                        p(align = "center", "Andrea Faltynkova, Norwegian University of Science and Technology"),
                        
                        p(align = "center", "Johannes Völker, Norwegian University of Science and Technology"),
                        
                        p(align = "center", "Laura Monclús Anglada, Norwegian University of Science and Technology"),
                        
                        p(align = "center", a(href = "https://www.sccwrp.org/about/staff/syd-kotar/", "Sydney Kotar"),", Southern California Coastal Water Research Project"),
                        
                        p(align = "center", a(href = "https://branderlab.net/", 'Dr. Susanne Brander'),", Oregon State University",
                          tags$a(href="https://twitter.com/smbrander", icon("twitter"))),
                        
                        p(align = "center", a(href = "https://www.ntnu.edu/employees/martin.wagner", 'Dr. Martin Wagner'),", Norwegian University of Science and Technology",
                          tags$a(href="https://twitter.com/martiwag", icon("twitter"))),
                        
                        p(align = "center", a(href = "https://www.wur.nl/en/Persons/Bart-prof.dr.-AA-Bart-Koelmans.htm", 'Dr. Bart Koelmans'),", Wageningen University",
                          tags$a(href="https://twitter.com/MicroplasticLab", icon("twitter"))),
                        
                        p(align = "center", a(href = "https://rochmanlab.com/", 'Dr. Chelsea Rochman'),", University of Toronto",
                          tags$a(href="https://twitter.com/ChelseaRochman", icon("twitter"))),
                        
                        p(align = "center", a(href = "https://www.sccwrp.org/about/staff/alvina-mehinto/", 'Dr. Alvine Mehinto'),", Southern California Coastal Water Research Project"),
                        
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
        p("This plot displays the categorization of measured endpoints in the database. Nodes correspond to the Broad Endpoint Category, 
        the Specific Endpoint Category, Endpoints and the level of biological organization from left to right. The widget 
        below may be used to select endpoints at various Biological Levels of Organization. Click nodes to expand and collapse the plot."),
        br(),
            
        fluidRow(
          
          column(width = 12,
                 
                 column(width = 3,
                        pickerInput(inputId = "bio_check_endpoint", 
                                    label = "Level of Biological Organization",
                                    choices = levels(aoc_endpoint$bio_f),
                                    selected = levels(aoc_endpoint$bio_f),
                                    options = list(`actions-box` = TRUE),
                                    multiple = TRUE)),
          ), #closes out column
          
          column(width = 12,
                 
                 #Go button
                 column(width = 3,
                        actionButton("go_endpoint", "Plot Current Selection", icon("rocket"), style="color: #fff; background-color:  #117a65; border-color:  #0e6655")),
                 
          ), #closes out column
          
          column(width = 12,
          #collapsible tree plot
          collapsibleTree::collapsibleTreeOutput("plot", height = "400px"),
          
          ), #closes out column
          
        ), #close fluid row
        ), #close box
      
), #close tab


#### Search UI ####

tabItem(tabName = "Search",
        
         box(title = "Search Database", status = "primary", width = 12,
             
             column(width = 12, 
             dataTableOutput("databaseDataTable", height = "200px"))   
             
             
         ), #close box
        
),#close search tab

#### Screening UI ####

tabItem(tabName = "Screening",
        
        box(title = "Data Selection", status = "primary", width = 12, collapsible = TRUE,
            
            shinyjs::useShinyjs(), # requires package for "reset" button, DO NOT DELETE - make sure to add any new widget to the reset_input in the server
            id = "screen", # adds ID for resetting filters
            
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
                                                 choices = levels(aoc_quality$lvl1_f),
                                                 selected = levels(aoc_quality$lvl1_f),
                                                 options = list(`actions-box` = TRUE), # option to de/select all
                                                 multiple = TRUE)), # allows for multiple inputs
                              
                              #Specific endpoint selection
                              column(width = 4, #Specific endpoint selection
                                     pickerInput(inputId = "lvl2_quality", 
                                                  label = "Specific Endpoint Category:", 
                                                  choices = levels(aoc_quality$lvl2_f),
                                                  selected = levels(aoc_quality$lvl2_f),
                                                  options = list(`actions-box` = TRUE),
                                                  multiple = TRUE)),
                              
                              #Effect y/n selection
                              column(width = 4,
                                     pickerInput(inputId = "effect_quality", 
                                                 label = "Effect:",
                                                 choices = levels(aoc_quality$effect_f),
                                                 selected = levels(aoc_quality$effect_f),
                                                 options = list(`actions-box` = TRUE),
                                                 multiple = TRUE)),
                              
                     ), #close tabpanel
                     
                     tabPanel("Biology", 
                              
                              #organism group selection
                              column(width = 4,
                                     pickerInput(inputId = "organism_quality",
                                                 label = "Organisms:",
                                                 choices = levels(aoc_quality$org_f),
                                                 selected = levels(aoc_quality$org_f),
                                                 options = list(`actions-box` = TRUE),
                                                 multiple = TRUE),
                                     
                                     #environment selection
                                     pickerInput(inputId = "env_quality", 
                                                 label = "Environment:",
                                                 choices = levels(aoc_quality$env_f),
                                                 selected = levels(aoc_quality$env_f),
                                                 options = list(`actions-box` = TRUE),
                                                 multiple = TRUE)),
                              
                              
                             #species selection
                             column(width = 4,
                                    pickerInput(inputId = "species_quality", 
                                                label = "Species:", 
                                                choices = levels(aoc_quality$species_f),
                                                selected = levels(aoc_quality$species_f),
                                                options = list(`actions-box` = TRUE),
                                                multiple = TRUE), 
                                     
                                     #biological organization selection
                                     pickerInput(inputId = "bio_quality", 
                                                 label = "Biological Organization:",
                                                 choices = levels(aoc_quality$bio_f),
                                                 selected = levels(aoc_quality$bio_f),
                                                 options = list(`actions-box` = TRUE),
                                                 multiple = TRUE)),
                              
                              #life stage selection
                              column(width = 4,
                                     pickerInput(inputId = "life_quality", 
                                                 label = "Life Stages:",
                                                 choices = levels(aoc_quality$life_f),
                                                 selected = levels(aoc_quality$life_f),
                                                 options = list(`actions-box` = TRUE),
                                                 multiple = TRUE),     
                                     
                                     #exposure duration
                                     pickerInput(inputId = "acute.chronic_quality", 
                                                 label = "Exposure Duration:",
                                                 choices = levels(aoc_quality$acute.chronic_f),
                                                 selected = levels(aoc_quality$acute.chronic_f),
                                                 options = list(`actions-box` = TRUE),
                                                 multiple = TRUE)),
                              
                     ), #close tabpanel
                     
                     tabPanel("Particles", 
                              
                              #polymer selection
                              column(width = 4,
                                     pickerInput(inputId = "poly_quality", 
                                                 label = "Polymer:",
                                                 choices = levels(aoc_quality$poly_f),
                                                 selected = levels(aoc_quality$poly_f),
                                                 options = list(`actions-box` = TRUE),
                                                 multiple = TRUE)),
                              
                              #shape selection
                              column(width = 4,
                                     pickerInput(inputId = "shape_quality", 
                                                 label = "Shape:",
                                                 choices = levels(aoc_quality$shape_f),
                                                 selected = levels(aoc_quality$shape_f),
                                                 options = list(`actions-box` = TRUE),
                                                 multiple = TRUE)),
                              
                              #size category selection
                              column(width = 4,
                                     pickerInput(inputId = "size_quality", 
                                                 label = "Size Category:",
                                                 choices = levels(aoc_quality$size_f),
                                                 selected = levels(aoc_quality$size_f),
                                                 options = list(`actions-box` = TRUE),
                                                 multiple = TRUE)),
                              
                     ), #close tabpanel
                     
                     tabPanel("Study Screening", 
                              
                              #technical quality selection
                              column(width = 4,
                                     pickerInput(inputId = "tech_tier_zero_quality", 
                                                 label = "Technical Criteria:",
                                                 choices = levels(aoc_quality$tier_zero_tech_f),
                                                 selected = levels(aoc_quality$tier_zero_tech_f),
                                                 options = list(`actions-box` = TRUE),
                                                 multiple = TRUE)),
                              
                              #risk assessment quality selection
                              column(width = 4,
                                     pickerInput(inputId = "risk_tier_zero_quality", 
                                                 label = "Risk Assessment Criteria:",
                                                 choices = levels(aoc_quality$tier_zero_risk_f),
                                                 selected = levels(aoc_quality$tier_zero_risk_f),
                                                 options = list(`actions-box` = TRUE),
                                                 multiple = TRUE)),
                              
                              #specfic study
                              column(width = 4,
                                     pickerInput(inputId = "study_plus_quality", 
                                                 label = "Study:",
                                                 choices = levels(aoc_quality$Study_plus),
                                                 selected = levels(aoc_quality$Study_plus),
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

        box(title = "Visualize Data", status = "primary", width = 12,
            
            p("Use the cursor to zoom and hover over the plot to view additional information about each study. Some studies are not visible until zoomed in. 
              Alternatively, specific studies may be selected using the filter in the 'Study Screening' tab above."),
            br(),
            p("'Red Criteria' are indicated by (*). Scores of 0, 1, and 2 are respresented by red, grey, and blue tiles respectively."),
            br(),
            
            plotlyOutput("tech_plotly", height = "600px"), 
            
            plotlyOutput("risk_plotly", height = "600px")
               
        ), #close box
        
), #closes out tab


#### Exploration UI ####
  
tabItem(tabName = "Exploration",
            
         box(title = "Data Selection", status = "primary", width = 12, collapsible = TRUE,
            
             shinyjs::useShinyjs(), # requires package for "reset" button, DO NOT DELETE - make sure to add any new widget to the reset_input in the server
             id = "exploration", # adds ID for resetting filters
             
         fluidRow(
           tabBox(width = 12,
             
             tabPanel("Data Type",
                      
                  fluidRow(
                      #Data type selection
                      column(width = 4,
                             pickerInput(inputId = "exp_type_check", 
                             label = "Data Type:",
                             choices = levels(aoc_setup$exp_type_f),
                             selected = levels(aoc_setup$exp_type_f),
                             options = list(`actions-box` = TRUE), 
                             multiple = TRUE))), 
                                 
                      ), #close tabpanel
             
             tabPanel("Effect", 
                      
                  fluidRow(
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
                        multiple = TRUE))),
                      
                      ), #close tabpanel
             
             tabPanel("Biology", 
                      
                   fluidRow(
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
                         multiple = TRUE))),
                      
                      ), #close tabpanel
             
             tabPanel("Particles", 
                      
                  fluidRow(
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
                        multiple = TRUE))),

                      ), #close tabpanel
           
             tabPanel("Study Screening", 
                      
                  fluidRow(    
                      column(width = 12,
                      p("Only 'Particle Only' data are included in the study screening dataset.")), 
                      
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
                            multiple = TRUE))),
                      
                      ), #close tabpanel
             
             tabPanel("Dose Metric",
                      
                  fluidRow(
                       column(width = 4,

                              radioButtons(inputId = "dose_check", 
                              label = "Dose Metric:",
                              choices = c("Particles/mL", "µg/mL", "µm3/mL", "µm2/mL", "µm2/µg/mL"),
                              selected = "µg/mL")),
                      
                       column(width = 8,

                              radioButtons(inputId = "Rep_Con_rad",
                              label = "Do you want to use just the reported, just the converted, or all exposure concentrations?",
                              choices = c("reported", "converted", "all"),
                              selected = "all"))),
                      
                      ), #close tabpanel
             
             tabPanel("Aesthetics", 
                      
                  fluidRow(    
                      column(width = 4,
                             selectInput(inputId = "plot.type", "Plot Type:",
                                         list(boxplot = "boxplot", violin = "violin", beeswarm = "beeswarm"))),
                             # checkboxInput(inputId = "show.points", "Show All Points", FALSE)),
                      
                      column(width = 4,
                             selectInput(inputId = "theme.type_exp", "Dark or Light Mode:",
                                         list(light = "light", dark = "dark"))),
                      
                      column(width = 4,
                             selectInput(inputId = "color.type_exp", "Color Theme:",
                                         list(default = "default", viridis = "viridis", brewer = "brewer", tron = "tron", locusZoom = "locusZoom", d3 = "d3", Nature = "Nature", JAMA = "JAMA")))),
                      
             ), #close tabpanel
             
             tabPanel("Alignment (Advanced)", height = "600px",
                      
                fluidRow(
                      column(width = 12,
                      p("A monodisperse effect concentration (e.g. 5 micron spheres) may be re-scaled to a default size range (e.g. 1 - 5,000 microns) using methods described in", a(href = "https://www.sciencedirect.com/science/article/pii/S0043135421006278", "Kooi et al., (2021)"), "Re-scaling to a default size range allows direct comparison to exposure concentrations for a default size range (which may also be re-scaled). The following radio buttons apply corrections for bioavailability (i.e. limiting available particles to max ingestable size), and a further correction for the ecologically relevant metric (ERM). For a given ERM, the threshold may be related to both mono- or polydisperse particles interchangeably so long as the total magnitude of ERM remains the same (Koelmans et al, 2020). If, for example, 'volume' is chosen below as an ERM, the monodisperse effect concentration is first corrected for bioavailability and aligned to whichever default size range the user chooses below. This aligned threshold (in particles/mL) is then multiplied by a correction for polydisperse volume based on the average volumes for the given range of microplastics in the environment.", strong("Only 'Particle Only' data are available for alignment."))),
                      br(),
                      
                      #ERM Checkbox
                      column(width = 12,
                             radioButtons(inputId = "ERM_check", # ERM (particle, surface area, mass, volume, specific surface area)
                                          label = "Ecologically Relevant Metric:",
                                          choices = c("Unaligned","Particles", "Surface Area", "Volume", "Mass", "Specific Surface Area"),
                                          selected = "Unaligned")),
                      column(width = 12,
                             strong("Starting alpha values are for marine surface water reported in ", a(href = "https://www.sciencedirect.com/science/article/pii/S0043135421006278", "Kooi et al., (2021)")),
                             br(),
                             br()),
                      
                      #Alpha checkbox
                      column(width = 4,
                             numericInput(inputId = "alpha",
                                          label = "Length Alpha Value",
                                          value = 2.07,
                                          step = 0.01)),
                      
                      #Alpha surface area input
                      column(width = 4,
                             numericInput(inputId = "a.sa",
                                          label = "Surface Area Alpha Value",
                                          value = 1.50,
                                          step = 0.01)),
                      
                      #Alpha volume input
                      column(width = 4,
                             numericInput(inputId = "a.v",
                                          label = "Volume Alpha Value",
                                          value = 1.48,
                                          step = 0.01)),
                      
                      #Alpha mass input
                      column(width = 4,
                             numericInput(inputId = "a.m",
                                          label = "Mass Alpha Value",
                                          value = 1.32,
                                          step = 0.01)),
                      
                      #Alpha ssa input
                      column(width = 4,
                             numericInput(inputId = "a.ssa",
                                          label = "Specific Surface Area Alpha Value",
                                          value = 1.98,
                                          step = 0.01)),
                      
                      #average width to length ratio
                      column(width = 4,
                             numericInput(inputId = "R.ave",
                                          label = "Average Particle Width to Length Ratio",
                                          value = 0.77,
                                          step = 0.01)),
                      
                      #average density
                      column(width = 4,
                             numericInput(inputId = "p.ave",
                                          label = "Average Particle Density (g/cm^3)",
                                          value = 1.10,
                                          step = 0.01)),
                      
                      # lower length input
                      column(width = 4,
                             numericInput(inputId = "lower_length",
                                          label = "Lower Length for Default Size Range (µm)",
                                          value = 1)),
                      # upper length input
                      column(width = 4,
                             numericInput(inputId = "upper_length",
                                          label = "Upper Length for Default Size Range (µm)",
                                          value = 5000)),
                      
                      # Switch to choose what determines bioaccessibility
                      column(width = 5,
                             radioButtons(inputId = "ingestion.translocation.switch",
                                          label = "Bioaccessibility limited by tissue translocation (fixed) or mouth size opening (species-dependent)?",
                                          choices = c("ingestion", "translocation"),
                                          selected = "ingestion"
                                          )),
                      
                      # Tissue translocation size limit (if applicable)
                      column(width = 7,
                             numericInput(inputId = "upper.tissue.trans.size.um",
                                                  label = "Upper Length (µm) for Translocatable Particles (only works if bioaccessibility determined by translocation; also excludes data from experiments using particles longer than defined value)",
                                                  value = 83))),
                      
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
              tabBox(width = 12,
                     
                     tabPanel("Organism Group",
                      
                      fluidRow(
                        column(width = 12,      
                        plotOutput(outputId = "organism_plot_react", height = "600px")),
                        
                        column(width = 3,
                               downloadButton("downloadexploration_org", "Download Plot", icon("download"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                     
                     ),#closes tab panel
                     
                     
                     tabPanel("Broad Endpoint Category",
                      
                      fluidRow(
                        column(width = 12,      
                        plotOutput(outputId = "lvl_plot_react", height = "600px")),
                        
                        column(width = 3,
                               downloadButton("downloadexploration_lvl1", "Download Plot", icon("download"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                              
                     ),#closes tab panel
                     
                     tabPanel("Specific Endpoint Category", 
                      
                      fluidRow(  
                      column(width = 12,
                          plotOutput(outputId = "lvl2_plot_react", height = "auto")),
                      
                      column(width = 3,
                             downloadButton("downloadexploration_lvl2", "Download Plot", icon("download"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                              
                     ),#closes tab panel
                     
                     tabPanel("Size",
                        
                        fluidRow(
                        column(width = 12,      
                        plotOutput(outputId = "size_plot_react", height = "600px")),
                        
                        column(width = 3,
                               downloadButton("downloadexploration_size", "Download Plot", icon("download"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                              
                     ),#closes tab panel
                     
                     tabPanel("Shape",
                        
                        fluidRow(
                        column(width = 12,      
                        plotOutput(outputId = "shape_plot_react", height = "600px")),
                        
                        column(width = 3,
                               downloadButton("downloadexploration_shape", "Download Plot", icon("download"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                              
                     ),#closes tab panel
                     
                     tabPanel("Polymer",
                              
                        fluidRow(
                        column(width = 12,      
                        plotOutput(outputId = "poly_plot_react", height = "600px")),
                        
                        column(width = 3,
                               downloadButton("downloadexploration_poly", "Download Plot", icon("download"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                              
                     ),#closes tab panel
              br(),       
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
              tabBox(width = 12, 
                     
                     tabPanel("Data Type",
                              
                    shinyjs::useShinyjs(), # requires package for "reset" button, DO NOT DELETE - make sure to add any new widget to the reset_input in the server
                    id = "ssd", # adds ID for resetting filters
                          
                        fluidRow(    
                          #Data type selection
                          column(width = 4,
                                 pickerInput(inputId = "exp_type_check_ssd", 
                                 label = "Data Type:",
                                 choices = levels(aoc_z$exp_type_f),
                                 selected = "Particle Only",
                                 options = list(`actions-box` = TRUE), 
                                 multiple = TRUE))), 
                              
                     ), #close tabpanel  
                     
                     tabPanel("Effect",
                            
                         fluidRow(  
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
                                  label = "Specific Endpoint Category:",
                                  choices = levels(aoc_z$lvl2_f),
                                  selected = levels(aoc_z$lvl2_f),
                                  options = list(`actions-box` = TRUE), 
                                  multiple = TRUE))), 
                              
                     ), #close tabpanel 
                     
                     tabPanel("Biology",
                        
                         fluidRow( 
                           column(width = 4,
                                  #Organism group selection
                                  pickerInput(inputId = "Group_check_ssd", 
                                  label = "Organism Group:",
                                  choices = levels(aoc_z$Group),
                                  selected = c("Annelida","Cnidaria", "Crustacea", "Echinoderm", "Fish", "Insect", "Mixed", "Mollusca", "Nematoda", "Rotifera"),
                                  options = list(`actions-box` = TRUE), 
                                  multiple = TRUE),
                                  
                                  #Environment selection
                                  pickerInput(inputId = "env_check_ssd", 
                                  label = "Environment:",
                                  choices = levels(aoc_z$env_f),
                                  selected = c("Marine", "Freshwater"),
                                  options = list(`actions-box` = TRUE), 
                                  multiple = TRUE)), 

                           
                           column(width = 4,
                                  #Species selection
                                  pickerInput(inputId = "Species_check_ssd", 
                                  label = "Species:",
                                  choices = levels(aoc_z$Species),
                                  selected = levels(aoc_z$Species),
                                  options = list(`actions-box` = TRUE), 
                                  multiple = TRUE),

                                  #level of biological organization selection
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
                                  multiple = TRUE))),
                              
                     ), #close tabpanel 
                     
                     tabPanel("Particles",
                             
                        fluidRow(       
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
                                  selected = c("1µm < 100µm", "100µm < 1mm", "1mm < 5mm"),
                                  options = list(`actions-box` = TRUE), 
                                  multiple = TRUE))), 
                              
                     ), #close tabpanel 
                     
                     tabPanel("Study Screening",
                          
                        fluidRow(      
                          column(width = 12,        
                          p("Only 'Particle Only' data are included in the study screening dataset.")), 
                              
                          column(width = 4,    
                                  #technical criteria selection
                                  pickerInput(inputId = "tech_tier_zero_check_ssd", 
                                  label = "Technical Criteria:",
                                  choices = levels(aoc_z$tier_zero_tech_f),
                                  selected = "Red Criteria Passed",
                                  options = list(`actions-box` = TRUE),
                                  multiple = TRUE)),
                          
                          column(width = 4,
                                 #technical criteria selection
                                 pickerInput(inputId = "risk_tier_zero_check_ssd", 
                                 label = "Risk Assessment Criteria:",
                                 choices = levels(aoc_z$tier_zero_risk_f),
                                 selected = "Red Criteria Passed",
                                 options = list(`actions-box` = TRUE),
                                 multiple = TRUE))),

                     ), #close tabpanel 
                     
                     tabPanel("Dose Metric",
                         
                        fluidRow(           
                          column(width = 4,
                                 radioButtons(inputId = "dose_check_ssd", 
                                 label = "Dose Metric:",
                                 choices = c("Particles/mL", "µg/mL", "µm3/mL", "µm2/mL", "µm2/µg/mL"),
                                 selected = "Particles/mL")),
                          
                          column(width = 8,
                                 radioButtons(
                                 inputId = "Reported_Converted_rad",
                                 label = "Do you want to use just the reported, just the converted, or all exposure concentrations?",
                                 choices = list("reported", "converted", "all"),
                                 selected = "all"))),
                     ), #close tabPanel
                    
                     tabPanel("Alignment (Advanced)",
                          
                        fluidRow(
                          column(width = 12,
                             p("A monodisperse effect concentration (e.g. 5 micron spheres) may be re-scaled to a default size range (e.g. 1 - 5,000 microns) using methods described in", a(href = "https://www.sciencedirect.com/science/article/pii/S0043135421006278", "Kooi et al., (2021)"), "Re-scaling to a default size range allows direct comparison to exposure concentrations for a default size range (which may also be re-scaled). The following radio buttons apply corrections for bioavailability (i.e. limiting available particles to max ingestable size), and a further correction for the ecologically relevant metric (ERM). For a given ERM, the threshold may be related to both mono- or polydisperse particles interchangeably so long as the total magnitude of ERM remains the same (Koelmans et al, 2020). If, for example, 'volume' is chosen below as an ERM, the monodisperse effect concentration is first corrected for bioavailability and aligned to whichever default size range the user chooses below. This aligned threshold (in particles/mL) is then multiplied by a correction for polydisperse volume based on the average volumes for the given range of microplastics in the environment.", strong("Only 'Particle Only' data are available for alignment."))),
                             br(),
                             
                                    #ERM Checkbox
                                    column(width = 12,
                                           radioButtons(inputId = "ERM_check_ssd", # ERM (particle, surface area, mass, volume, specific surface area)
                                                        label = "Ecologically Relevant Metric:",
                                                        choices = c("Unaligned","Particles", "Surface Area", "Volume", "Mass", "Specific Surface Area"),
                                                        selected = "Volume")),
                                    column(width = 12,
                                    strong("Starting alpha values are for marine surface water reported in ", a(href = "https://www.sciencedirect.com/science/article/pii/S0043135421006278", "Kooi et al., (2021)")),
                                    br(),
                                    br()),
                             
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
                                                        value = 5000)),
                          
                          # Switch to choose what determines bioaccessibility
                          column(width = 5,
                                 radioButtons(inputId = "ingestion.translocation.switch_ssd",
                                              label = "Bioaccessibility limited by tissue translocation (fixed) or mouth size opening (species-dependent)?",
                                              choices = c("ingestion", "translocation"),
                                              selected = "ingestion")),
                          
                          # Tissue translocation size limit (if applicable)
                          column(width = 7,
                                 numericInput(inputId = "upper.tissue.trans.size.um_ssd",
                                              label = "Upper Length (µm) for Translocatable Particles (only works if bioaccessibility determined by translocation; also excludes data from experiments using particles longer than defined value)",
                                              value = 83))
                          ) # close fluidrow
                          
                     ), #close tabpanel  
                     
                     tabPanel("SSD Options (Advanced)",
                          
                        fluidRow(
                          column(width = 12,
                            p("The choice of effect metrics (e.g., NOEC, LOEC, HONEC, ECXX and LCXX) should be carefully considered. Assessment factors are available for converting acute exposures to chronic exposure and estimating NOECs from other effect metrics (e.g. LOEC's), according to the methods described in ", a(href = "https://setac.onlinelibrary.wiley.com/doi/epdf/10.1002/ieam.4214", 'Wigger et al (2020).'), "In brief, an assessment factor of 10 is applied to convert LC/EC25-50 to NOEC, 2 to convert EC/LC20, LOEC, or MIC to NOEC. LC10, EC10 and HONEC are considered equivalent to LOEC. An assessment factor of 10 is applied to convert acute-to-chronic, with determinations of such categories dependent on taxa, as defined in the reference.")),
                               
                          #Effect metric widget
                          column(width = 6,
                                 pickerInput(inputId = "effect.metric_rad_ssd", 
                                 label = "Effect Metric:",
                                 choices = levels(aoc_z$effect.metric),
                                 selected = c("EC10","EC50","EMT50", "IC50","LC50","LOEC", "NOEC"),
                                 options = list(`actions-box` = TRUE),
                                 multiple = TRUE), 
                          
                          #Assessment factor - time
                                 pickerInput(inputId = "AF.time_rad_ssd", 
                                 label = "Apply Assessment Factor for acute and sub-chronic to chronic?",
                                 choices = c("Yes", "No"),
                                 options = list(`actions-box` = TRUE),
                                 selected = "Yes")),
                          
                          #Assessment factor - noec conversion
                          column(width = 6,
                                 pickerInput(inputId = "AF.noec_rad_ssd", # noec/loc assessment factor
                                 label = "Apply Assessment Factor to convert effect metrics to NOECs?",
                                 choices = c("Yes", "No"),
                                 options = list(`actions-box` = TRUE),
                                 selected = "Yes"),
                          
                          #concentration selector (minimum, lower 95% CI, median, mean)
                                 pickerInput(
                                 inputId = "conc.select.rad",
                                 label = "What summary statistic should be used for each species?",
                                 choices = list("Minimum", "Lower 95% CI", "1st Quartile", "Median", "Mean", "3rd Quartile", "Upper 95% CI", "Maximum"),
                                 selected = "1st Quartile"))),
                              
            ) #close tabpanel  
            ), #closes out tabbox
            ), #closes out fluidrow
            
            column(width = 3,
                   actionButton("SSDgo", "Submit Current Selection", icon("rocket"), style="color: #fff; background-color:  #117a65; border-color:  #0e6655")),
            
            column(width = 3,
                   actionButton("reset_ssd", "Reset Filters", icon("redo"), style="color: #fff; background-color: #f39c12; border-color: #d68910")), 
            
            
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
   
   output$plot <- collapsibleTree::renderCollapsibleTree({
     
     collapsibleTree(aoc_filter_endpoint(), root = "Aquatic Organisms Database", hierarchy = c("lvl1_f", "lvl2_f", "lvl3_f", "bio_f"),
                     fontSize = 12, zoomable = FALSE)    
                     
   }) 
   

#### Search S ####
   
   output$databaseDataTable <- DT::renderDataTable(
     aoc_search,
     filter = "top",
     rownames = FALSE,
     extensions = c('Buttons'),
     options = list(
       pageLength = 25,
       dom = 'Brtip',
       buttons = list(I('colvis'), c('copy', 'csv', 'excel')),
       scrollY = 600,
       scrollX = TRUE,
       paging = TRUE,
       columnDefs = list(list(width = '100px', targets = "_all"))),
     colnames = c('DOI', 'Authors', 'Year', 'Technical "Red Criteria"', 'Risk Assessment "Red Criteria"','Species', 'Organism Group', 'Environment', 'Life Stage', 'In vitro/in vivo',
                  'Sex', 'Estimated Body Length (cm)', 'Estimated Maximum Ingestible Size (mm)', 'Experiment Type',
                  'Exposure Route', 'Particle Mix?', 'Negative Control', 'Reference Particle', 'Exposure Media',
                  'Solvent', 'Detergent', 'pH', 'Salinity (ppt)', 'Temperature (Avg)', 'Temperature (Min)',
                  'Temperature (Max)', 'Exposure Duration (days)', 'Acute/Chronic', 'Number of Doses', 'Replicates',
                  'Sample Size', 'Dosing Frequency', 'Chemicals Added', 'Added Chemical Dose (nominal)',
                  'Added Chemical Dose (measured)', 'particles/mL (master)', 'particles/mL (master), reported or converted',
                  'μg/mL (master)', 'μg/mL (master), reported or converted', 'μm^3/mL (master)', 'μm^2/mL (master)',
                  'μm^2/ug/mL (master)', 'Effect', 'Direction', 'Broad Endpoint Category', 'Specific Endpoint Category',
                  'Endpoint', 'Level of Biological Organization', 'Target Cell or Tissue', 'Effect Metric', 'AF Time',
                  'AF NOEC', 'Polymer', 'Shape', 'Density (g/cm^3)', 'Density, reported or estimated', 'Charge',
                  'Zeta Potential (mV)', 'Size Category','Zeta Potential Media', 'Functional Group', 'Particle Length (μm)',
                  'Particle Width (μm)', 'Particle Surface Area (μm^2)', 'Particle Volume (μm^3)', 'Particle Mass (mg)',
                  'Weathered or Biofouled?', 'Size Validated?', 'Polymer Validated?', 'Shape Validated', 'Particle Source','Sodium Azide Present?',
                  'Screened for Chemical Contamination?', 'Particle Cleaning?', 'Solvent Rinse', 'Background Contamination Monitored?',
                  'Concentration Validated?', 'Particle Behavior', 'Uptake Validated?', 'Uptake Validation Method',
                  'Tissue Distribution', 'Organisms Fed?', 'Original Dose Units', 'Original Concentration', 'Original Dose Units Nominal or Measured'))

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
     study_plus_c<-input$study_plus_quality
     
     #Create summary data set based on widget filters
     aoc_quality %>%
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
       filter(Study_plus %in% study_plus_c)
       
     
   })
   
   #Create plot for quality screening scores from quality_filtered data
   tech_plotly <- eventReactive(list(input$go_quality),{
     
     #Technical
     tech <- quality_filtered() %>%
       filter(Category_f == "Technical") %>%  
       #summarize data for plotly
       group_by(Study_plus, Criteria_f, Score) %>%  
       summarise() %>%
       ungroup() %>%
       pivot_wider(names_from = Study_plus, 
                   values_from = Score) %>%   
       column_to_rownames(var="Criteria_f")  
       
     colnames(tech)<- gsub(" \\(10.*", "",colnames(tech))
     colnames(tech)<- gsub(" \\(doi.*", "",colnames(tech))
     
     tech <- tech %>% 
       as.matrix()
     
     #make plotly
     tech_p <- plot_ly(x=colnames(tech), y=rownames(tech), z = tech, type = "heatmap",
                       ygap = .4, xgap = .4,
                       colors = c("tomato", "ivory3", "dodgerblue"),
                       hoverinfo = 'text',
                       showscale = FALSE,
                       hovertemplate = paste(" Study:  %{x}<br>",
                                             "Criteria:  %{y}<br>",
                                             "Score:  %{z}<extra></extra>")) 
     
     tech_p <- tech_p %>% layout(
       title = 'Technical Criteria',
       xaxis = list(
         type = 'category',
         list(fixedrange = TRUE),
         tickfont = list(size = 10)),
       yaxis = list(tickfont = list(size = 10)))

       #print plot
       print(tech_p)
       
       
   })
   
   #Render plotly
   output$tech_plotly <- renderPlotly({
     
     tech_plotly()

   })
   
   #Create plot for quality screening scores from quality_filtered data
   risk_plotly <- eventReactive(list(input$go_quality),{

     #Risk Assessment
     risk <- quality_filtered() %>%
       filter(Category_f == "Risk Assessment") %>%  
       #summarize data for plotly
       group_by(Study_plus, Criteria_f, Score) %>%  
       summarise() %>%
       ungroup() %>%  
       pivot_wider(names_from = Study_plus, 
                   values_from = Score) %>%   
       column_to_rownames(var="Criteria_f") 
     
     colnames(risk)<- gsub(" \\(10.*", "",colnames(risk))
     colnames(risk)<- gsub(" \\(doi.*", "",colnames(risk))
     
     risk <- risk %>% 
       as.matrix()
     
     #make plotly
     risk_p <- plot_ly(x=colnames(risk), y=rownames(risk), z = risk, type = "heatmap",
                       ygap = .4, xgap = .4,
                       colors = c("tomato", "ivory3", "dodgerblue"),
                       hoverinfo = 'text',
                       showscale = FALSE,
                       hovertemplate = paste(" Study:  %{x}<br>",
                                             "Criteria:  %{y}<br>",
                                             "Score:  %{z}<extra></extra>")) 
     
     risk_p <- risk_p %>% layout(
       title = 'Risk Assessment Criteria',
       xaxis = list(
         type = 'category',
         list(fixedrange = TRUE),
         tickfont = list(size = 10)),
       yaxis = list(tickfont = list(size = 10)))
     
     #print plots
     print(risk_p)
     
   })
   
   #Render plotly
   output$risk_plotly <- renderPlotly({
     
     risk_plotly()
     
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
     shinyjs::reset("study_plus_quality")

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
  
    ## ERM parameterization from user input ##
    # Define params for correction #
    alpha <- input$alpha #length power law exponent
    x2D_set <- as.numeric(input$upper_length) #upper size range (default - user defined)
    x1D_set <- input$lower_length #lower size range (default - user defined)
    x1M_set <- input$lower_length #lower size range for ingestible plastic (user defined)
    upper.tissue.trans.size.um <- as.numeric(input$upper.tissue.trans.size.um) #user-defined upper value for tissue trans (numeric)
    ingestion.translocation.switch <- input$ingestion.translocation.switch #user-defined: inputs are "ingestion" or "translocation"
    
    
    # define parameters for power law coefficients
    a.sa <- input$a.sa #1.5 #marine surface area power law
    a.v <- input$a.v #1.48 #a_V for marine surface water volume
    a.m <- input$a.m #1.32 # upper limit fora_m for mass for marine surface water in table S4 
    a.ssa <- input$a.ssa #1.98 # A_SSA for marine surface water
    
    #define additional parameters for calculations based on averages in the environment
    R.ave <- input$R.ave #0.77 #average width to length ratio for microplastics in marine enviornment
    p.ave <- input$p.ave #1.10 #average density in marine surface water
    
    # calculate ERM for each species
    aoc_setup <- aoc_setup %>% 
      ### BIOACCESSIBILITY ###
      # define upper size length for bioaccessibility (user-defined) for ingestion (only used if user defines as such
      mutate(x2M_ingest = case_when(is.na(max.size.ingest.um) ~ x2D_set, 
                                    max.size.ingest.um < x2D_set ~ max.size.ingest.um,
                                    max.size.ingest.um > x2D_set ~ x2D_set)) %>%  #set to default as upper limit or max size ingest, whichever is smaller
      # define upper size length for Translocation 
      mutate(x2M_trans = case_when(is.na(max.size.ingest.um) ~ upper.tissue.trans.size.um, 
                                   max.size.ingest.um  < upper.tissue.trans.size.um ~  max.size.ingest.um,
                                   max.size.ingest.um  > upper.tissue.trans.size.um ~ upper.tissue.trans.size.um)) %>% 
      #define which bioaccessibility limit to use for calculations based on user input
      mutate(ingestion.translocation = ingestion.translocation.switch) %>%  #user-defined bioaccessibility switch. Note that a
      mutate(x2M = case_when(ingestion.translocation == "ingestion" ~ x2M_ingest,
                             ingestion.translocation == "translocation" ~ x2M_trans)) %>% 
      ### Particle ERM ###
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
    ##--- environmental calculations ---###
    #calculate lower ingestible surface area
    mutate(x_LL_sa = SAfnx(a = 0.5 * x1D_set, #length-limited
                           b = 0.5 * x1D_set, #length-limited
                           c = 0.5 * x1D_set)) %>% #length-limited
      #calculate upper ingestible surface area
      mutate(x_UL_sa = SAfnx( 
        a = 0.5 * x2M, #LENGTH-limited (less conservative assumption)
        b = 0.5 * x2M, #length-limited
        c = 0.5 * x2M)) %>%   #length-limited
      #calculate mu_x_poly (env) for surface area
      mutate(mu.sa.poly = mux.polyfnx(a.sa, x_UL_sa, x_LL_sa)) %>% 
      
      ##--- laboratory calculations ---###
      ## define mu_x_mono OR mu_x_poly (lab) for alignment to ERM  #
      #(note that if mixed particles were used, a different equation must be used)
      mutate(mu.sa.mono = case_when(
        polydispersity == "monodisperse" ~ particle.surface.area.um2, # use reported surface area in monodisperse
        polydispersity == "polydisperse" ~  mux.polyfnx(a.x = a.sa, 
                                                        x_LL = particle.surface.area.um2.min,
                                                        x_UL = particle.surface.area.um2.max))) %>% 
      #calculate polydisperse effect concentration for surface area (particles/mL)
      mutate(EC_poly_sa.particles.mL = (EC_mono_p.particles.mL * mu.sa.mono)/mu.sa.poly) %>%  
      #calculate environmentally realistic effect threshold
      mutate(EC_env_sa.particles.mL = EC_poly_sa.particles.mL * CF_bio) %>% 
      
      #### volume ERM ####
    ##--- environmental calculations ---###
    #calculate lower ingestible volume 
    mutate(x_LL_v = volumefnx_poly(length = x1D_set,
                                   width = x1D_set)) %>% 
      #calculate maximum ingestible volume 
      mutate(x_UL_v = volumefnx_poly(length = x2M, #length-limited
                                     width = x2M)) %>% #length-limited
      # calculate mu.v.poly
      mutate(mu.v.poly = mux.polyfnx(a.v, x_UL_v, x_LL_v)) %>% 
      ##--- laboratory calculations ---###
      ## define mu_x_mono OR mu_x_poly (lab) for alignment to ERM  #
      #(note that if mixed particles were used, a different equation must be used)
      mutate(mu.v.mono = case_when(
        polydispersity == "monodisperse" ~ particle.volume.um3, # use reported volume in monodisperse
        polydispersity == "polydisperse" ~ mux.polyfnx(a.x = a.v, 
                                                       x_LL = particle.volume.um3.min,
                                                       x_UL = particle.volume.um3.max))) %>% 
      
      #calculate polydisperse effect concentration for volume (particles/mL)
      mutate(EC_poly_v.particles.mL = (EC_mono_p.particles.mL * mu.v.mono)/mu.v.poly) %>%  
      #calculate environmentally realistic effect threshold
      mutate(EC_env_v.particles.mL = EC_poly_v.particles.mL * CF_bio) %>% 
      
      #### mass ERM ###
      ##--- environmental calculations ---###
      #calculate lower ingestible mass
      mutate(x_LL_m = massfnx_poly(width = x1D_set,
                                   length = x1D_set,
                                   p = p.ave)) %>% 
      #calculate upper ingestible mass
      mutate(x_UL_m = massfnx_poly(width = x2M, #length-limited
                                   length = x2M, #length-limited
                                   p = p.ave)) %>% #average density
      # calculate mu.m.poly
      mutate(mu.m.poly = mux.polyfnx(a.m, x_UL_m, x_LL_m)) %>% 
      ##--- laboratory calculations ---###
      ## define mu_x_mono OR mu_x_poly (lab) for alignment to ERM  #
      #(note that if mixed particles were used, a different equation must be used)
      mutate(mu.m.mono = case_when(
        polydispersity == "monodisperse" ~  mass.per.particle.mg * 1000, # use reported volume in monodisperse
        polydispersity == "polydisperse" ~ mux.polyfnx(a.x = a.m, 
                                                       x_UL = mass.per.particle.mg.max * 1000,
                                                       x_LL = mass.per.particle.mg.min * 1000))) %>% 
      #calculate polydisperse effect concentration for volume (particles/mL)
      mutate(EC_poly_m.particles.mL = (EC_env_p.particles.mL * mu.m.mono)/mu.m.poly) %>%
      #calculate environmentally realistic effect threshold
      mutate(EC_env_m.particles.mL = EC_poly_m.particles.mL * CF_bio) %>% 
      
      ##### specific surface area ERM ####
    mutate(mu.ssa.mono = mu.sa.mono/mu.m.mono) %>% #define mu_x_mono for alignment to ERM (um^2/ug)
      #calculate lower ingestible 1/SSA
      mutate(x_LL_ssa = SSA.inversefnx(sa = x_LL_sa, #surface area
                                       m = x_LL_m) #mass
      ) %>% 
      #calculate upper ingestible SSA  (um^2/ug)
      mutate(x_UL_ssa = SSA.inversefnx(sa = x_UL_sa, #surface area
                                       m = x_UL_m) #mass
      ) %>% 
      #calculate mu_x_poly for specific surface area
      #note that mu were calcaulted for polydisperse particles before, so not special case needed here
      mutate(mu.ssa.inverse.poly = mux.polyfnx(a.ssa, x_UL_ssa, x_LL_ssa)) %>% 
      #calculate polydisperse effect concentration for specific surface area (particles/mL)
      mutate(mu.ssa.poly = 1 / mu.ssa.inverse.poly) %>%  #calculate mu_SSA from inverse
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
    
    #filter out reported, calcualted, or all based on checkbox and make new variable based on µg/mL or particles/mL
    if(Rep_Con_rad == "reported" & dose_check == "µg/mL" & ERM_check == "Unaligned"){
      aoc_setup <- aoc_setup %>% 
        filter(dose.mg.L.master.converted.reported == "reported") %>% 
        mutate(dose_new = dose.mg.L.master)}
    
    if(Rep_Con_rad == "converted" & dose_check == "µg/mL" & ERM_check == "Unaligned"){
      aoc_setup <- aoc_setup %>%
        filter(dose.mg.L.master.converted.reported == "converted") %>% 
        mutate(dose_new = dose.mg.L.master)}
    
    if(Rep_Con_rad == "all" & dose_check == "µg/mL" & ERM_check == "Unaligned"){
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
    if(Rep_Con_rad == "reported" & dose_check == "µm3/mL" & ERM_check == "Unaligned"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = dose.um3.mL.master)}
    
    if(Rep_Con_rad == "converted" & dose_check == "µm3/mL" & ERM_check == "Unaligned"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = dose.um3.mL.master)}
    
    if(Rep_Con_rad == "all" & dose_check == "µm3/mL" & ERM_check == "Unaligned"){
      aoc_setup <- aoc_setup %>%
        mutate(dose_new = dose.um3.mL.master)}
    
    #repeat for surface area
    if(Rep_Con_rad == "reported" & dose_check == "µm2/mL" & ERM_check == "Unaligned"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = dose.um2.mL.master)}
    
    if(Rep_Con_rad == "converted" & dose_check == "µm2/mL" & ERM_check == "Unaligned"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = dose.um2.mL.master)}
    
    if(Rep_Con_rad == "all" & dose_check == "µm2/mL" & ERM_check == "Unaligned"){
      aoc_setup <- aoc_setup %>%
        mutate(dose_new = dose.um2.mL.master)}
    
    #repeat for specific surface area
    if(Rep_Con_rad == "reported" & dose_check == "µm2/µg/mL" & ERM_check == "Unaligned"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = dose.um2.ug.mL.master)}
    
    if(Rep_Con_rad == "converted" & dose_check == "µm2/µg/mL" & ERM_check == "Unaligned"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = dose.um2.ug.mL.master)}
    
    if(Rep_Con_rad == "all" & dose_check == "µm2/µg/mL" & ERM_check == "Unaligned"){
      aoc_setup <- aoc_setup %>%
        mutate(dose_new = dose.um2.ug.mL.master)}
    
    
    
    ## 1 DOSE METRIC = PARTICLES ###
    if(Rep_Con_rad == "reported" & dose_check == "Particles/mL" & ERM_check == "Particles"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_p.particles.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "Particles/mL" & ERM_check == "Particles"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_p.particles.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "Particles/mL" & ERM_check == "Particles"){
      aoc_setup <- aoc_setup %>%
        mutate(dose_new = EC_env_p.particles.mL)}
    
    #repeat for particles with ERM = Surface Area
    if(Rep_Con_rad == "reported" & dose_check == "Particles/mL" & ERM_check == "Surface Area"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new =EC_env_sa.particles.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "Particles/mL" & ERM_check == "Surface Area"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_sa.particles.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "Particles/mL" & ERM_check == "Surface Area"){
      aoc_setup <- aoc_setup %>%
        mutate(dose_new = EC_env_sa.particles.mL)}
    
    #repeat for particles with ERM = Volume
    if(Rep_Con_rad == "reported" & dose_check == "Particles/mL" & ERM_check == "Volume"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_v.particles.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "Particles/mL" & ERM_check == "Volume"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_v.particles.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "Particles/mL" & ERM_check == "Volume"){
      aoc_setup <- aoc_setup %>%
        mutate(dose_new = EC_env_v.particles.mL)}
    
    #repeat for particles with ERM = Mass
    if(Rep_Con_rad == "reported" & dose_check == "Particles/mL" & ERM_check == "Mass"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_m.particles.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "Particles/mL" & ERM_check == "Mass"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_m.particles.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "Particles/mL" & ERM_check == "Mass"){
      aoc_setup <- aoc_setup %>%
        mutate(dose_new = EC_env_m.particles.mL)}
    
    #repeat for particles with ERM = Specific Surface Area
    if(Rep_Con_rad == "reported" & dose_check == "Particles/mL" & ERM_check == "Specific Surface Area"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_ssa.particles.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "Particles/mL" & ERM_check == "Specific Surface Area"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_ssa.particles.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "Particles/mL" & ERM_check == "Specific Surface Area"){
      aoc_setup <- aoc_setup %>%
        mutate(dose_new = EC_env_ssa.particles.mL)}
    
    # 2 DOSE METRIC = Surface Area ###
    if(Rep_Con_rad == "reported" & dose_check == "µm2/mL" & ERM_check == "Particles"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_p.um2.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm2/mL" & ERM_check == "Particles"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_p.um2.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm2/mL" & ERM_check == "Particles"){
      aoc_setup <- aoc_setup %>%
        mutate(dose_new = EC_env_p.um2.mL)}
    
    #repeat for particles with ERM = Surface Area
    if(Rep_Con_rad == "reported" & dose_check == "µm2/mL" & ERM_check == "Surface Area"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_sa.um2.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm2/mL" & ERM_check == "Surface Area"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_sa.um2.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm2/mL" & ERM_check == "Surface Area"){
      aoc_setup <- aoc_setup %>%
        mutate(dose_new = EC_env_sa.um2.mL)}
    
    #repeat for particles with ERM = Volume
    if(Rep_Con_rad == "reported" & dose_check == "µm2/mL" & ERM_check == "Volume"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_v.um2.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm2/mL" & ERM_check == "Volume"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_v.um2.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm2/mL" & ERM_check == "Volume"){
      aoc_setup <- aoc_setup %>%
        mutate(dose_new = EC_env_v.um2.mL)}
    
    #repeat for particles with ERM = Mass
    if(Rep_Con_rad == "reported" & dose_check == "µm2/mL" & ERM_check == "Mass"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_m.um2.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm2/mL" & ERM_check == "Mass"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_m.um2.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm2/mL" & ERM_check == "Mass"){
      aoc_setup <- aoc_setup %>%
        mutate(dose_new = EC_env_m.um2.mL)}
    
    #repeat for particles with ERM = Specific Surface Area
    if(Rep_Con_rad == "reported" & dose_check == "µm2/mL" & ERM_check == "Specific Surface Area"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_ssa.um2.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm2/mL" & ERM_check == "Specific Surface Area"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_ssa.um2.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm2/mL" & ERM_check == "Specific Surface Area"){
      aoc_setup <- aoc_setup %>%
        mutate(dose_new = EC_env_ssa.um2.mL)}
    
    # 3 DOSE METRIC = mass ###
    if(Rep_Con_rad == "reported" & dose_check == "µg/mL" & ERM_check == "Particles"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_p.ug.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µg/mL" & ERM_check == "Particles"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_p.ug.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µg/mL" & ERM_check == "Particles"){
      aoc_setup <- aoc_setup %>%
        mutate(dose_new = EC_env_p.ug.mL)}
    
    #repeat for particles with ERM = Surface Area
    if(Rep_Con_rad == "reported" & dose_check == "µg/mL" & ERM_check == "Surface Area"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_sa.ug.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µg/mL" & ERM_check == "Surface Area"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_sa.ug.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µg/mL" & ERM_check == "Surface Area"){
      aoc_setup <- aoc_setup %>%
        mutate(dose_new = EC_env_sa.ug.mL)}
    
    #repeat for particles with ERM = Volume
    if(Rep_Con_rad == "reported" & dose_check == "µg/mL" & ERM_check == "Volume"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_v.ug.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µg/mL" & ERM_check == "Volume"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_v.ug.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µg/mL" & ERM_check == "Volume"){
      aoc_setup <- aoc_setup %>%
        mutate(dose_new = EC_env_v.ug.mL)}
    
    #repeat for particles with ERM = Mass
    if(Rep_Con_rad == "reported" & dose_check == "µg/mL" & ERM_check == "Mass"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_m.ug.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µg/mL" & ERM_check == "Mass"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_m.ug.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µg/mL" & ERM_check == "Mass"){
      aoc_setup <- aoc_setup %>%
        mutate(dose_new = EC_env_m.ug.mL)}
    
    #repeat for particles with ERM = Specific Surface Area
    if(Rep_Con_rad == "reported" & dose_check == "µg/mL" & ERM_check == "Specific Surface Area"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_ssa.ug.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µg/mL" & ERM_check == "Specific Surface Area"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_ssa.ug.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µg/mL" & ERM_check == "Specific Surface Area"){
      aoc_setup <- aoc_setup %>%
        mutate(dose_new = EC_env_ssa.ug.mL)}
    
    # 4 DOSE METRIC = volume ###
    if(Rep_Con_rad == "reported" & dose_check == "µm3/mL" & ERM_check == "Particles"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_p.um3.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm3/mL" & ERM_check == "Particles"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_p.um3.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm3/mL" & ERM_check == "Particles"){
      aoc_setup <- aoc_setup %>%
        mutate(dose_new = EC_env_p.um3.mL)}
    
    #repeat for particles with ERM = Surface Area
    if(Rep_Con_rad == "reported" & dose_check == "µm3/mL" & ERM_check == "Surface Area"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_sa.um3.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm3/mL" & ERM_check == "Surface Area"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_sa.um3.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm3/mL" & ERM_check == "Surface Area"){
      aoc_setup <- aoc_setup %>%
        mutate(dose_new = EC_env_sa.um3.mL)}
    
    #repeat for particles with ERM = Volume
    if(Rep_Con_rad == "reported" & dose_check == "µm3/mL" & ERM_check == "Volume"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_v.um3.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm3/mL" & ERM_check == "Volume"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_v.um3.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm3/mL" & ERM_check == "Volume"){
      aoc_setup <- aoc_setup %>%
        mutate(dose_new = EC_env_v.um3.mL)}
    
    #repeat for particles with ERM = Mass
    if(Rep_Con_rad == "reported" & dose_check == "µm3/mL" & ERM_check == "Mass"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_m.um3.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm3/mL" & ERM_check == "Mass"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_m.um3.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm3/mL" & ERM_check == "Mass"){
      aoc_setup <- aoc_setup %>%
        mutate(dose_new = EC_env_m.um3.mL)}
    
    #repeat for particles with ERM = Specific Surface Area
    if(Rep_Con_rad == "reported" & dose_check == "µm3/mL" & ERM_check == "Specific Surface Area"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_ssa.um3.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm3/mL" & ERM_check == "Specific Surface Area"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_ssa.um3.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm3/mL" & ERM_check == "Specific Surface Area"){
      aoc_setup <- aoc_setup %>%
        mutate(dose_new = EC_env_ssa.um3.mL)}
    
    # 5 DOSE METRIC = specific surface are ###
    if(Rep_Con_rad == "reported" & dose_check == "µm2/µg/mL" & ERM_check == "Particles"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_p.um2.ug.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm2/µg/mL" & ERM_check == "Particles"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_p.um2.ug.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm2/µg/mL" & ERM_check == "Particles"){
      aoc_setup <- aoc_setup %>%
        mutate(dose_new = EC_env_p.um2.ug.mL)}
    
    #repeat for particles with ERM = Surface Area
    if(Rep_Con_rad == "reported" & dose_check == "µm2/µg/mL" & ERM_check == "Surface Area"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_sa.um2.ug.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm2/µg/mL" & ERM_check == "Surface Area"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_sa.um2.ug.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm2/µg/mL" & ERM_check == "Surface Area"){
      aoc_setup <- aoc_setup %>%
        mutate(dose_new = EC_env_sa.um2.ug.mL)}
    
    #repeat for particles with ERM = Volume
    if(Rep_Con_rad == "reported" & dose_check == "µm2/µg/mL" & ERM_check == "Volume"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_v.um2.ug.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm2/µg/mL" & ERM_check == "Volume"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_v.um2.ug.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm2/µg/mL" & ERM_check == "Volume"){
      aoc_setup <- aoc_setup %>%
        mutate(dose_new = EC_env_v.um2.ug.mL)}
    
    #repeat for particles with ERM = Mass
    if(Rep_Con_rad == "reported" & dose_check == "µm2/µg/mL" & ERM_check == "Mass"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_m.um2.ug.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm2/µg/mL" & ERM_check == "Mass"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_m.um2.ug.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm2/µg/mL" & ERM_check == "Mass"){
      aoc_setup <- aoc_setup %>%
        mutate(dose_new = EC_env_m.um2.ug.mL)}
    
    #repeat for particles with ERM = Specific Surface Area
    if(Rep_Con_rad == "reported" & dose_check == "µm2/µg/mL" & ERM_check == "Specific Surface Area"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_ssa.um2.ug.mL)}
    #converted
    if(Rep_Con_rad == "converted" & dose_check == "µm2/µg/mL" & ERM_check == "Specific Surface Area"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_ssa.um2.ug.mL)}
    #all
    if(Rep_Con_rad == "all" & dose_check == "µm2/µg/mL" & ERM_check == "Specific Surface Area"){
      aoc_setup <- aoc_setup %>%
        mutate(dose_new = EC_env_ssa.um2.ug.mL)}
  
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
      filter(tier_zero_risk_f %in% risk_tier_zero_c) %>%    #risk assessment quality
      filter(case_when(ingestion.translocation.switch == "translocation" ~  between(size.length.um.used.for.conversions, x1D_set, upper.tissue.trans.size.um), #if tissue-trans limited, don't use data with non-translocatable particles
                       ingestion.translocation.switch == "ingestion" ~  between(size.length.um.used.for.conversions, x1D_set, x2D_set)))  #if ingestion-limited, don't use data outside upper default size range
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
  
  organism_plot_react <- eventReactive(list(input$go),{
    
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
    
    print(p)

  })
  
  output$organism_plot_react <- renderPlot({
    
    organism_plot_react()
    
  })
  
  # Size Plot
  
  size_plot_react <- eventReactive(list(input$go),{

    
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
    
    print(p)

  })
  
  output$size_plot_react <- renderPlot({
    
    size_plot_react()
    
  })
  
  # Shape Plot
  
  shape_plot_react <- eventReactive(list(input$go),{
    
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
    
    print(p)
    
  })
  
  output$shape_plot_react <- renderPlot({
    
    shape_plot_react()
    
  })
  
  # Polymer Plot
  
  poly_plot_react <- eventReactive(list(input$go),{
    
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
    
    print(p)
    
  })
  
  output$poly_plot_react <- renderPlot({
    
    poly_plot_react()
    
  })
  
  # Endpoint Plot
  
  lvl_plot_react <- eventReactive(list(input$go),{
    
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
    
    print(p)
    
  })
  
  output$lvl_plot_react <- renderPlot({
    
    lvl_plot_react()
    
  })
  
  lvl2_plot_react <- eventReactive(list(input$go),{
    
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
  
  print(p)
  
  })
  
  output$lvl2_plot_react <- renderPlot(
    
    #dynamic plot height based on widget input
    height = function()if_else(600 < n_distinct(input$lvl2_check)*40, n_distinct(input$lvl2_check)*40, 600),
    
    {
    
    lvl2_plot_react()
    
  })
  
   # Create downloadable png for exploration plots
  
  #organism group
  output$downloadexploration_org <- downloadHandler(
    
    filename = function() {
      paste('Organism_Group', Sys.Date(), '.png', sep='')
    },
    content = function(file) {
      ggsave(file, plot = organism_plot_react(), width = 16, height = 8, device = 'png')
    })
  
  #size
  output$downloadexploration_size <- downloadHandler(
    
    filename = function() {
      paste('Size', Sys.Date(), '.png', sep='')
    },
    content = function(file) {
      ggsave(file, plot = size_plot_react(), width = 16, height = 8, device = 'png')
    })
  
  #shape
  output$downloadexploration_shape <- downloadHandler(
    
    filename = function() {
      paste('Shape', Sys.Date(), '.png', sep='')
    },
    content = function(file) {
      ggsave(file, plot = shape_plot_react(), width = 16, height = 8, device = 'png')
    })
  
  #polymer
  output$downloadexploration_poly <- downloadHandler(
    
    filename = function() {
      paste('Polymer', Sys.Date(), '.png', sep='')
    },
    content = function(file) {
      ggsave(file, plot = poly_plot_react(), width = 16, height = 8, device = 'png')
    })
  
  #lvl1
  output$downloadexploration_lvl1 <- downloadHandler(
    
    filename = function() {
      paste('Broad_Endpoint', Sys.Date(), '.png', sep='')
    },
    content = function(file) {
      ggsave(file, plot = lvl_plot_react(), width = 16, height = 8, device = 'png')
    })
  
  #lvl2
  output$downloadexploration_lvl2 <- downloadHandler(
    
    filename = function() {
      paste('Specific_Endpoint', Sys.Date(), '.png', sep='')
    },
    content = function(file) {
      
      ggsave(file, plot = lvl2_plot_react(), width = 30, height = 20, device = 'png')
    })
  
  
  # Create downloadable csv of filtered dataset.
  # Removed columns created above so the dataset matches Leah's original dataset.
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      
      write.csv(aoc_filter() %>%
        #Select columns
        dplyr::select(c(doi, authors, year, species_f, org_f, env_f, life_f, vivo_f, sex, body.length.cm, max.size.ingest.mm,
                        #experimental parameters
                        exp_type_f, exposure.route, mix, negative.control, reference.material, exposure.media, solvent, detergent,
                        media.ph, media.sal.ppt, media.temp, media.temp.min, media.temp.max, exposure.duration.d, acute.chronic_f,
                        treatments, replicates, sample.size, dosing.frequency, chem.add.nominal, chem.add.dose.mg.L.nominal, chem.add.dose.mg.L.measured,
                        #selected dose
                        dose_new,
                        #biological effects
                        effect_f, direction, lvl1_f, lvl2_f, lvl3_f, bio_f, target.cell.tissue, effect.metric,
                        #particle characteristics
                        poly_f, shape_f, density.g.cm3, density.reported.estimated, charge, zetapotential.mV, zetapotential.media, functional.group,
                        size.length.um.used.for.conversions, size.width.um.used.for.conversions, size_f, particle.surface.area.um2, particle.volume.um3,
                        mass.per.particle.mg, weather.biofoul_f,
                        #quality
                        size.valid, polymer.valid, shape.valid, particle.source, sodium.azide, contaminant.screen, clean.method, sol.rinse, background.plastics,
                        concentration.valid, particle.behavior, uptake.valid, uptake.valid.method, tissue.distribution, fed)) %>%  
       #Rename columns
       dplyr::rename(c("DOI" = doi, "Authors" = authors, "Year" = year, "Species" = species_f, "Organism Group" = org_f, "Environment" = env_f,
            "Life Stage" = life_f, "In vitro/in vivo" = vivo_f, "Sex" = sex, "Estimated Body Length (cm)" = body.length.cm,
            "Estimated Maximum Ingestible Size (mm)" = max.size.ingest.mm,
             #experimental parameters
            "Experiment Type" = exp_type_f, "Exposure Route" = exposure.route, "Particle Mix?" = mix, "Negative Control" = negative.control,
             "Reference Particle" = reference.material, "Exposure Media" = exposure.media, "Solvent" = solvent, "Detergent" = detergent,
            "pH" = media.ph, "Salinity (ppt)" = media.sal.ppt, "Temperature (Avg)" = media.temp, "Temperature (Min)"= media.temp.min,
            "Temperature (Max)" = media.temp.max, "Exposure Duration (days)" = exposure.duration.d, "Acute/Chronic" = acute.chronic_f,
            "Number of Doses" = treatments, "Replicates" = replicates, "Sample Size" = sample.size, "Dosing Frequency" = dosing.frequency,
            "Chemicals Added" = chem.add.nominal, "Added Chemical Dose (nominal)" = chem.add.dose.mg.L.nominal,
            "Added Chemical Dose (measured)" = chem.add.dose.mg.L.measured,
             #selected dose
            "Selected Dose" = dose_new,
             #biological effects
            "Effect" = effect_f, "Direction" = direction, "Broad Endpoint Category" = lvl1_f, "Specific Endpoint Category" = lvl2_f,
            "Endpoint" = lvl3_f, "Level of Biological Organization" = bio_f, "Target Cell or Tissue" = target.cell.tissue,
             "Effect Metric" = effect.metric,
             #particle characteristics
            "Polymer" = poly_f, "Shape" = shape_f, "Density (g/cm^3)" = density.g.cm3, "Density, reported or estimated" = density.reported.estimated,
            "Charge" = charge, "Zeta Potential (mV)" = zetapotential.mV, "Zeta Potential Media" = zetapotential.media, "Functional Group" = functional.group,
            "Particle Length (μm)" = size.length.um.used.for.conversions, "Particle Width (μm)" = size.width.um.used.for.conversions,
            "Size Category" = size_f, "Particle Surface Area (μm^2)" = particle.surface.area.um2, "Particle Volume (μm^3)" = particle.volume.um3,
            "Particle Mass (mg)" = mass.per.particle.mg, "Weathered or Biofouled?" = weather.biofoul_f,
             #quality
             "Size Validated?" = size.valid, "Polymer Validated?" = polymer.valid, "Shape Validated" = shape.valid, "Particle Source" = particle.source,
            "Sodium Azide Present?" = sodium.azide, "Screened for Chemical Contamination?" = contaminant.screen, "Particle Cleaning?" = clean.method,
            "Solvent Rinse" = sol.rinse, "Background Contamination Monitored?" = background.plastics,
            "Concentration Validated?"  = concentration.valid, "Particle Behavior" = particle.behavior, "Uptake Validated?" = uptake.valid,
            "Uptake Validation Method" = uptake.valid.method, "Tissue Distribution" = tissue.distribution, "Organisms Fed?" = fed)),
  
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
    shinyjs::reset("Rep_Con_rad")
    shinyjs::reset("dose_check")
    shinyjs::reset("ERM_check")
    shinyjs::reset("alpha")
    shinyjs::reset("a.sa")
    shinyjs::reset("a.v")
    shinyjs::reset("a.m")
    shinyjs::reset("a.ssa")
    shinyjs::reset("R.ave")
    shinyjs::reset("p.ave")
    shinyjs::reset("lower_length")
    shinyjs::reset("upper_length")
    
  }) #If we add more widgets, make sure they get added here. 

#### SSD S ####

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
    # Define params for alignments #
    alpha = input$alpha_ssd #length power law exponent
    x2D_set = as.numeric(input$upper_length_ssd) #upper size range (default)
    x1D_set = input$lower_length_ssd #lower size range (default)
    x1M_set = input$lower_length_ssd #lower size range for ingestible plastic (user defined)
    upper.tissue.trans.size.um <- as.numeric(input$upper.tissue.trans.size.um_ssd) #user-defined upper value for tissue trans (numeric)
    ingestion.translocation.switch <- input$ingestion.translocation.switch_ssd #user-defined: inputs are "ingestion" or "translocation"
    
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
      ### BIOACCESSIBILITY ###
      # define upper size length for bioaccessibility (user-defined) for ingestion (only used if user defines as such
      mutate(x2M_ingest = case_when(is.na(max.size.ingest.um) ~ x2D_set, 
                                    max.size.ingest.um < x2D_set ~ max.size.ingest.um,
                                    max.size.ingest.um > x2D_set ~ x2D_set)) %>%  #set to default as upper limit or max size ingest, whichever is smaller
      # define upper size length for Translocation 
      mutate(x2M_trans = case_when(is.na(max.size.ingest.um) ~ upper.tissue.trans.size.um, 
                                   max.size.ingest.um  < upper.tissue.trans.size.um ~  max.size.ingest.um,
                                   max.size.ingest.um  > upper.tissue.trans.size.um ~ upper.tissue.trans.size.um)) %>% 
      #define which bioaccessibility limit to use for calculations based on user input
      mutate(ingestion.translocation = ingestion.translocation.switch) %>%  #user-defined bioaccessibility switch. Note that a
      mutate(x2M = case_when(ingestion.translocation == "ingestion" ~ x2M_ingest,
                             ingestion.translocation == "translocation" ~ x2M_trans)) %>% 
      ### Particle ERM ###
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
    ##--- environmental calculations ---###
    #calculate lower ingestible surface area
    mutate(x_LL_sa = SAfnx(a = 0.5 * x1D_set, #length-limited
                           b = 0.5 * x1D_set, #length-limited
                           c = 0.5 * x1D_set)) %>% #length-limited
      #calculate upper ingestible surface area
      mutate(x_UL_sa = SAfnx( 
        a = 0.5 * x2M, #LENGTH-limited (less conservative assumption)
        b = 0.5 * x2M, #length-limited
        c = 0.5 * x2M)) %>%   #length-limited
      #calculate mu_x_poly (env) for surface area
      mutate(mu.sa.poly = mux.polyfnx(a.sa, x_UL_sa, x_LL_sa)) %>% 
      
      ##--- laboratory calculations ---###
      ## define mu_x_mono OR mu_x_poly (lab) for alignment to ERM  #
      #(note that if mixed particles were used, a different equation must be used)
      mutate(mu.sa.mono = case_when(
        polydispersity == "monodisperse" ~ particle.surface.area.um2, # use reported surface area in monodisperse
        polydispersity == "polydisperse" ~  mux.polyfnx(a.x = a.sa, 
                                                        x_LL = particle.surface.area.um2.min,
                                                        x_UL = particle.surface.area.um2.max))) %>% 
      #calculate polydisperse effect concentration for surface area (particles/mL)
      mutate(EC_poly_sa.particles.mL = (EC_mono_p.particles.mL * mu.sa.mono)/mu.sa.poly) %>%  
      #calculate environmentally realistic effect threshold
      mutate(EC_env_sa.particles.mL = EC_poly_sa.particles.mL * CF_bio) %>% 
      
      #### volume ERM ####
    ##--- environmental calculations ---###
    #calculate lower ingestible volume 
    mutate(x_LL_v = volumefnx_poly(length = x1D_set,
                                   width = x1D_set)) %>% 
      #calculate maximum ingestible volume 
      mutate(x_UL_v = volumefnx_poly(length = x2M, #length-limited
                                     width = x2M)) %>% #length-limited
      # calculate mu.v.poly
      mutate(mu.v.poly = mux.polyfnx(a.v, x_UL_v, x_LL_v)) %>% 
      ##--- laboratory calculations ---###
      ## define mu_x_mono OR mu_x_poly (lab) for alignment to ERM  #
      #(note that if mixed particles were used, a different equation must be used)
      mutate(mu.v.mono = case_when(
        polydispersity == "monodisperse" ~ particle.volume.um3, # use reported volume in monodisperse
        polydispersity == "polydisperse" ~ mux.polyfnx(a.x = a.v, 
                                                       x_LL = particle.volume.um3.min,
                                                       x_UL = particle.volume.um3.max))) %>% 
      
      #calculate polydisperse effect concentration for volume (particles/mL)
      mutate(EC_poly_v.particles.mL = (EC_mono_p.particles.mL * mu.v.mono)/mu.v.poly) %>%  
      #calculate environmentally realistic effect threshold
      mutate(EC_env_v.particles.mL = EC_poly_v.particles.mL * CF_bio) %>% 
      
      #### mass ERM ###
      ##--- environmental calculations ---###
      #calculate lower ingestible mass
      mutate(x_LL_m = massfnx_poly(width = x1D_set,
                                   length = x1D_set,
                                   p = p.ave)) %>% 
      #calculate upper ingestible mass
      mutate(x_UL_m = massfnx_poly(width = x2M, #length-limited
                                   length = x2M, #length-limited
                                   p = p.ave)) %>% #average density
      # calculate mu.m.poly
      mutate(mu.m.poly = mux.polyfnx(a.m, x_UL_m, x_LL_m)) %>% 
      ##--- laboratory calculations ---###
      ## define mu_x_mono OR mu_x_poly (lab) for alignment to ERM  #
      #(note that if mixed particles were used, a different equation must be used)
      mutate(mu.m.mono = case_when(
        polydispersity == "monodisperse" ~  mass.per.particle.mg * 1000, # use reported volume in monodisperse
        polydispersity == "polydisperse" ~ mux.polyfnx(a.x = a.m, 
                                                       x_UL = mass.per.particle.mg.max * 1000,
                                                       x_LL = mass.per.particle.mg.min * 1000))) %>% 
      #calculate polydisperse effect concentration for volume (particles/mL)
      mutate(EC_poly_m.particles.mL = (EC_env_p.particles.mL * mu.m.mono)/mu.m.poly) %>%
      #calculate environmentally realistic effect threshold
      mutate(EC_env_m.particles.mL = EC_poly_m.particles.mL * CF_bio) %>% 
      
      ##### specific surface area ERM ####
    mutate(mu.ssa.mono = mu.sa.mono/mu.m.mono) %>% #define mu_x_mono for alignment to ERM (um^2/ug)
      #calculate lower ingestible 1/SSA
      mutate(x_LL_ssa = SSA.inversefnx(sa = x_LL_sa, #surface area
                                       m = x_LL_m) #mass
      ) %>% 
      #calculate upper ingestible SSA  (um^2/ug)
      mutate(x_UL_ssa = SSA.inversefnx(sa = x_UL_sa, #surface area
                                       m = x_UL_m) #mass
      ) %>% 
      #calculate mu_x_poly for specific surface area
      #note that mu were calcaulted for polydisperse particles before, so not special case needed here
      mutate(mu.ssa.inverse.poly = mux.polyfnx(a.ssa, x_UL_ssa, x_LL_ssa)) %>% 
      #calculate polydisperse effect concentration for specific surface area (particles/mL)
      mutate(mu.ssa.poly = 1 / mu.ssa.inverse.poly) %>%  #calculate mu_SSA from inverse
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
      dplyr::filter(risk.13 != 0) %>%  #Drop studies that received a score of 0 for endpoints criteria (this also drops studies that have not yet been scored) - KEEP THIS AFTER THE RED CRITERIA FILTERS  
      dplyr::filter(case_when(ingestion.translocation.switch == "translocation" ~  between(size.length.um.used.for.conversions, x1D_set, upper.tissue.trans.size.um), #if tissue-trans limited, don't use data with non-translocatable particles
                       ingestion.translocation.switch == "ingestion" ~  between(size.length.um.used.for.conversions, x1D_set, x2D_set))) %>%  #if ingestion-limited, don't use data outside upper default size range
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
    
    # ERM parametrization ##
    # Define params for alignments #
    alpha = input$alpha_ssd #length power law exponent
    x2D_set = as.numeric(input$upper_length_ssd) #upper size range (default)
    x1D_set = input$lower_length_ssd #lower size range (default)
    x1M_set = input$lower_length_ssd #lower size range for ingestible plastic (user defined)
    upper.tissue.trans.size.um <- as.numeric(input$upper.tissue.trans.size.um_ssd) #user-defined upper value for tissue trans (numeric)
    ingestion.translocation.switch <- input$ingestion.translocation.switch_ssd #user-defined: inputs are "ingestion" or "translocation"
    
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
      ### BIOACCESSIBILITY ###
      # define upper size length for bioaccessibility (user-defined) for ingestion (only used if user defines as such
      mutate(x2M_ingest = case_when(is.na(max.size.ingest.um) ~ x2D_set, 
                                    max.size.ingest.um < x2D_set ~ max.size.ingest.um,
                                    max.size.ingest.um > x2D_set ~ x2D_set)) %>%  #set to default as upper limit or max size ingest, whichever is smaller
      # define upper size length for Translocation 
      mutate(x2M_trans = case_when(is.na(max.size.ingest.um) ~ upper.tissue.trans.size.um, 
                                   max.size.ingest.um  < upper.tissue.trans.size.um ~  max.size.ingest.um,
                                   max.size.ingest.um  > upper.tissue.trans.size.um ~ upper.tissue.trans.size.um)) %>% 
      #define which bioaccessibility limit to use for calculations based on user input
      mutate(ingestion.translocation = ingestion.translocation.switch) %>%  #user-defined bioaccessibility switch. Note that a
      mutate(x2M = case_when(ingestion.translocation == "ingestion" ~ x2M_ingest,
                             ingestion.translocation == "translocation" ~ x2M_trans)) %>% 
      ### Particle ERM ###
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
    ##--- environmental calculations ---###
    #calculate lower ingestible surface area
    mutate(x_LL_sa = SAfnx(a = 0.5 * x1D_set, #length-limited
                           b = 0.5 * x1D_set, #length-limited
                           c = 0.5 * x1D_set)) %>% #length-limited
      #calculate upper ingestible surface area
      mutate(x_UL_sa = SAfnx( 
        a = 0.5 * x2M, #LENGTH-limited (less conservative assumption)
        b = 0.5 * x2M, #length-limited
        c = 0.5 * x2M)) %>%   #length-limited
      #calculate mu_x_poly (env) for surface area
      mutate(mu.sa.poly = mux.polyfnx(a.sa, x_UL_sa, x_LL_sa)) %>% 
      
      ##--- laboratory calculations ---###
      ## define mu_x_mono OR mu_x_poly (lab) for alignment to ERM  #
      #(note that if mixed particles were used, a different equation must be used)
      mutate(mu.sa.mono = case_when(
        polydispersity == "monodisperse" ~ particle.surface.area.um2, # use reported surface area in monodisperse
        polydispersity == "polydisperse" ~  mux.polyfnx(a.x = a.sa, 
                                                        x_LL = particle.surface.area.um2.min,
                                                        x_UL = particle.surface.area.um2.max))) %>% 
      #calculate polydisperse effect concentration for surface area (particles/mL)
      mutate(EC_poly_sa.particles.mL = (EC_mono_p.particles.mL * mu.sa.mono)/mu.sa.poly) %>%  
      #calculate environmentally realistic effect threshold
      mutate(EC_env_sa.particles.mL = EC_poly_sa.particles.mL * CF_bio) %>% 
      
      #### volume ERM ####
    ##--- environmental calculations ---###
    #calculate lower ingestible volume 
    mutate(x_LL_v = volumefnx_poly(length = x1D_set,
                                   width = x1D_set)) %>% 
      #calculate maximum ingestible volume 
      mutate(x_UL_v = volumefnx_poly(length = x2M, #length-limited
                                     width = x2M)) %>% #length-limited
      # calculate mu.v.poly
      mutate(mu.v.poly = mux.polyfnx(a.v, x_UL_v, x_LL_v)) %>% 
      ##--- laboratory calculations ---###
      ## define mu_x_mono OR mu_x_poly (lab) for alignment to ERM  #
      #(note that if mixed particles were used, a different equation must be used)
      mutate(mu.v.mono = case_when(
        polydispersity == "monodisperse" ~ particle.volume.um3, # use reported volume in monodisperse
        polydispersity == "polydisperse" ~ mux.polyfnx(a.x = a.v, 
                                                       x_LL = particle.volume.um3.min,
                                                       x_UL = particle.volume.um3.max))) %>% 
      
      #calculate polydisperse effect concentration for volume (particles/mL)
      mutate(EC_poly_v.particles.mL = (EC_mono_p.particles.mL * mu.v.mono)/mu.v.poly) %>%  
      #calculate environmentally realistic effect threshold
      mutate(EC_env_v.particles.mL = EC_poly_v.particles.mL * CF_bio) %>% 
      
      #### mass ERM ###
      ##--- environmental calculations ---###
      #calculate lower ingestible mass
      mutate(x_LL_m = massfnx_poly(width = x1D_set,
                                   length = x1D_set,
                                   p = p.ave)) %>% 
      #calculate upper ingestible mass
      mutate(x_UL_m = massfnx_poly(width = x2M, #length-limited
                                   length = x2M, #length-limited
                                   p = p.ave)) %>% #average density
      # calculate mu.m.poly
      mutate(mu.m.poly = mux.polyfnx(a.m, x_UL_m, x_LL_m)) %>% 
      ##--- laboratory calculations ---###
      ## define mu_x_mono OR mu_x_poly (lab) for alignment to ERM  #
      #(note that if mixed particles were used, a different equation must be used)
      mutate(mu.m.mono = case_when(
        polydispersity == "monodisperse" ~  mass.per.particle.mg * 1000, # use reported volume in monodisperse
        polydispersity == "polydisperse" ~ mux.polyfnx(a.x = a.m, 
                                                       x_UL = mass.per.particle.mg.max * 1000,
                                                       x_LL = mass.per.particle.mg.min * 1000))) %>% 
      #calculate polydisperse effect concentration for volume (particles/mL)
      mutate(EC_poly_m.particles.mL = (EC_env_p.particles.mL * mu.m.mono)/mu.m.poly) %>%
      #calculate environmentally realistic effect threshold
      mutate(EC_env_m.particles.mL = EC_poly_m.particles.mL * CF_bio) %>% 
      
      ##### specific surface area ERM ####
    mutate(mu.ssa.mono = mu.sa.mono/mu.m.mono) %>% #define mu_x_mono for alignment to ERM (um^2/ug)
      #calculate lower ingestible 1/SSA
      mutate(x_LL_ssa = SSA.inversefnx(sa = x_LL_sa, #surface area
                                       m = x_LL_m) #mass
      ) %>% 
      #calculate upper ingestible SSA  (um^2/ug)
      mutate(x_UL_ssa = SSA.inversefnx(sa = x_UL_sa, #surface area
                                       m = x_UL_m) #mass
      ) %>% 
      #calculate mu_x_poly for specific surface area
      #note that mu were calcaulted for polydisperse particles before, so not special case needed here
      mutate(mu.ssa.inverse.poly = mux.polyfnx(a.ssa, x_UL_ssa, x_LL_ssa)) %>% 
      #calculate polydisperse effect concentration for specific surface area (particles/mL)
      mutate(mu.ssa.poly = 1 / mu.ssa.inverse.poly) %>%  #calculate mu_SSA from inverse
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
      dplyr::filter(risk.13 != 0) %>%  #Drop studies that received a score of 0 for endpoints criteria (this also drops studies that have not yet been scored) - KEEP THIS AFTER THE RED CRITERIA FILTERS  
      filter(case_when(ingestion.translocation.switch == "translocation" ~  between(size.length.um.used.for.conversions, x1D_set, upper.tissue.trans.size.um), #if tissue-trans limited, don't use data with non-translocatable particles
                       ingestion.translocation.switch == "ingestion" ~  between(size.length.um.used.for.conversions, x1D_set, x2D_set))) %>%  #if ingestion-limited, don't use data outside upper default size range
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
      # geom_segment(data = aochc,aes(x = est, y = percent/100, xend = est, yend = est), linetype = 'dashed', color = "red", size = .5) + #hazard conc line vertical
      # geom_segment(data = aochc,aes(x = lcl, y = percent/100, xend = est, yend = percent/100), linetype = 'dashed', color = "red", size = .5) + #hazard conc line horizontal
      # geom_text(data = aochc, aes(x = est, y = 0.15, label = paste0(percent, "% Hazard Confidence Level")), color = "red", size = 5) + #label for hazard conc
      # geom_text(data = aochc, aes(x = est, y = 0.10, label = paste0(est_format, " ", dose_check_ssd)), color = "red", size = 5) + #label for hazard conc
      geom_text(data = aochc, aes(x = Inf, y = 0.15, hjust = 1.2, vjust = 0, label = paste0(percent, "% Hazard Confidence Level")), color = "red", size = 5) + #label for hazard conc
      geom_text(data = aochc, aes(x = Inf, y = 0.10, hjust = 1.4, vjust = 0, label = paste0(est_format, " ", dose_check_ssd)), color = "red", size = 5) + #label for hazard conc
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
    fitdistrplus::descdist(logConc,boot=1000)
  })
  
  #alt fits with fitdistrplus package
  aocSSDFitLNorm <- reactive({
    #reactive to static
    aocSSD <- aoc_filter_ssd()
    #subset and define variable
    Conc <- aocSSD$Conc
    
    #fit log-normal distribution
    fitdistrplus::fitdist(Conc, "lnorm")
  })
  
  #alt fits with fitdistrplus package
  aocSSDFitLlogis <- reactive({
    #reactive to static
    aocSSD <- aoc_filter_ssd()
    #subset and define variable
    Conc <- aocSSD$Conc
    
    #fit logistic distribution
    fitdistrplus::fitdist(Conc, "logis")
  })
  
  #alt fits with fitdistrplus package
  aocSSDFitWeibull <- reactive({
    #reactive to static
    aocSSD <- aoc_filter_ssd()
    #subset and define variable
    Conc <- aocSSD$Conc
    
    #fit logistic distribution
    fitdistrplus::fitdist(Conc, "weibull")
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
    fitdistrplus::qqcomp(list(aocFitLNorm,
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
    fitdistrplus::ppcomp(list(aocFitLNorm, 
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
    aocFitNorm <- fitdistrplus::fitdist(logConc, "norm")
    #aocFitlogis <- fitdist(logConc, "logis")
    
    
    #plot densities
    fitdistrplus::denscomp(list(aocFitNorm),#, aocFitLogis),
           legendtext=c("log-normal"),#,"log-logistic"),
           xlab = paste0("log10 ",dose_check_ssd))
  })

  # Create "reset" button to revert all filters back to what they began as.
  # Need to call all widgets individually by their ids.
  # See https://stackoverflow.com/questions/44779775/reset-inputs-with-reactive-app-in-shiny for more information.
  observeEvent(input$reset_ssd, {
    shinyjs::reset("exp_type_check_ssd")
    shinyjs::reset("lvl1_check_ssd")
    shinyjs::reset("lvl2_check_ssd")
    shinyjs::reset("poly_check_ssd")
    shinyjs::reset("Group_check_ssd")
    shinyjs::reset("shape_check_ssd")
    shinyjs::reset("env_check_ssd")
    shinyjs::reset("effect_check_ssd")
    shinyjs::reset("size_check_ssd")
    shinyjs::reset("life_check_ssd")
    shinyjs::reset("bio_check_ssd")
    shinyjs::reset("Species_check_ssd")
    shinyjs::reset("tech_tier_zero_check_ssd")
    shinyjs::reset("risk_tier_zero_check_ssd")
    shinyjs::reset("Reported_Converted_rad")
    shinyjs::reset("dose_check_ssd")
    shinyjs::reset("ERM_check_ssd")
    shinyjs::reset("alpha_ssd")
    shinyjs::reset("a.sa_ssd")
    shinyjs::reset("a.v_ssd")
    shinyjs::reset("a.m_ssd")
    shinyjs::reset("a.ssa_ssd")
    shinyjs::reset("R.ave_ssd")
    shinyjs::reset("p.ave_ssd")
    shinyjs::reset("lower_length_ssd")
    shinyjs::reset("upper_length_ssd")
    shinyjs::reset("acute.chronic_check_ssd")
    
  }) #If we add more widgets, make sure they get added here. 
  
  
  } #Server end

#### Full App ####
shinyApp(ui = ui, server = server)


# End of R Shiny app script.
