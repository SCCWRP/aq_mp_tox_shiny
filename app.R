#### Aquatic Microplastics Toxicology Shiny App
#### File created: September 23, 2020
#### Code contributors: Heili Lowman, Leah Thornton Hampton, Scott Coffin, Emily Darin

#### Setup ####

# Anything that should only happen ONCE should be placed in this setup section, prior to the actual shiny structure.

# Load packages
library(tidyverse) #General everything
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

# Load finalized dataset.
aoc <- read_csv("AquaticOrganisms_Clean_final.csv", guess_max = 10000)

#### Introduction Setup ####

# All text inputs below.

#### Overview AO Setup ####

#Final_effect_dataset <- read_csv("Final_effect_dataset.csv")%>%
  #mutate(plot_f = case_when(
    #plot_f == "Polymer" ~ "Polymer",
    #plot_f == "Size" ~ "Size",
    #plot_f == "Shape" ~ "Shape",
    #plot_f == "Organism" ~ "Organism",
    #plot_f == "Lvl1" ~ "Endpoint Category",
    #plot_f == "Life.stage" ~ "Life Stage",
    #plot_f == "Invivo.invivo" ~ "In Vivo or In Vitro",
    #plot_f == "Exposure.route" ~ "Exposure Route"))%>%
  #mutate(plot_f = factor(plot_f))%>%
  #mutate(logEndpoints = log(Endpoints))%>%
  #rename(Percent = Freq)

polydf<-rowPerc(xtabs( ~polymer +effect, aoc)) #pulls polymers by effect 
polyf<-as.data.frame(polydf)%>% #Makes data frame 
  filter(effect %in% c("Y","N"))%>% #Sorts into Yes and No
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
  filter(effect %in% c("Y","N"))%>%
  mutate(size.category = case_when(
    size.category == 1 ~ "1nm < 100nm",
    size.category == 2 ~ "100nm < 1µm",
    size.category == 3.1 ~ "1µm < 10µm",
    size.category == 3.2 ~ "10µm < 100µm",
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
  filter(effect %in% c("Y","N"))%>%
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
  filter(effect %in% c("Y","N"))%>%
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
  filter(effect %in% c("Y","N"))%>%
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
  filter(effect %in% c("Y","N"))%>%
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
  filter(effect %in% c("Y","N"))%>%
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
  filter(effect %in% c("Y","N"))%>%
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

# Master dataset for scatterplots - for Heili's tab.
aoc_v1 <- aoc %>% # start with original dataset
   # full dataset filters.
  mutate(effect_f = factor(case_when(effect == "Y" ~ "Yes",
    effect == "N" ~ "No"),
    levels = c("No", "Yes"))) %>%
  # removing NAs to make data set nicer
  replace_na(list(size.category = 0, shape = "Not Reported", polymer = "Not Reported", life.stage = "Not Reported", chem.exp.typ.nominal = "Particle Only"))

aoc_setup <- aoc_v1 %>% # start with original dataset
  mutate(size_f = factor(case_when(
    size.category == 1 ~ "1nm < 100nm",
    size.category == 2 ~ "100nm < 1µm",
    size.category == 3.1 ~ "1µm < 10µm",
    size.category == 3.2 ~ "10µm < 100µm",
    size.category == 4 ~ "100µm < 1mm",
    size.category == 5 ~ "1mm < 5mm",
    size.category == 0 ~ "Not Reported"),
    levels = c("1nm < 100nm", "100nm < 1µm", "1µm < 10µm", "10µm < 100µm", "100µm < 1mm", "1mm < 5mm", "Not Reported"))) %>% # creates new column with nicer names and order by size levels.
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
  mutate(dose.um3.mL.master = particle.volume.um3 * dose.particles.mL.master) %>%  #calculate volume/mL
  mutate(af.time_noNA = replace_na(af.time, "Unavailable")) %>% 
  mutate(acute.chronic_f = factor(case_when(af.time_noNA == 10 ~ "Acute",
                                            af.time_noNA == 1 ~ "Chronic",
                                            af.time_noNA == "Unavailable" ~ "Unavailable"))) %>% #factorize assesment factor time into chronic/acute
  mutate(tier_zero_tech_f = factor(case_when(tech.tier.zero == "Fail" ~ "Red Criteria Failed",
                                             tech.tier.zero == "Pass" ~ "Red Criteria Passed"))) %>% 
  mutate(tier_zero_risk_f = factor(case_when(risk.tier.zero == "Fail" ~ "Red Criteria Failed",
                                             risk.tier.zero == "Pass" ~ "Red Criteria Passed"))) %>% 
  #Remove leachate and additive/chemical transfer experiments
  dplyr::filter(leachate.only != "Y") %>%
  mutate(chem.exp.typ.nominal_f = factor(case_when(chem.exp.typ.nominal == "Particle Only" ~ "Particle Only",
                                                   chem.exp.typ.nominal == "co.exp" ~ "Chemical Co-Exposure",
                                                   chem.exp.typ.nominal == "sorbed" ~ "Chemical Transfer"))) %>% 
  dplyr::filter(chem.exp.typ.nominal_f == "Particle Only") %>% 
  #calculate maximum ingestible size (if not already in database)
  mutate(max.size.ingest.mm = ifelse(is.na(max.size.ingest.mm), 
    10^(0.9341 * log10(body.length.cm - 1.1200)) * 10,  #(Jamm et al 2020 Nature paper)correction for cm to mm
    max.size.ingest.mm)) # if already present, just use that

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
                 c = c # c = 0.5 * height (note that hieght is 0.67 * width)
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
SSAfnx = function(a, # a = 0.5 * length
                  b,# b = 0.5 * width
                  c,# c = 0.5 * height (note that hieght is 0.67 * width)
                  m #mass (ug)
){
  SSA = (4*pi*(((a*b)^1.6 + (a*c)^1.6 + (b*c)^1.6) / 3)^(1/1.6))/m
  return(SSA)}

## parametrization ##
# Define params for correction #
alpha = 2.07 #table s4 for marine surface water. length
x2D_set = 5000 #upper size range (default)
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
  mutate(x2M = max.size.ingest.mm * 1000) %>% #max size ingest in um
  mutate(CF_bio = CFfnx(x1M = x1M_set, x2M = x2M, x1D = x1D_set, x2D = x2D_set, a = alpha)) %>%  #calculate CF_bio
  mutate(EC_env_p.particles.mL = dose.particles.mL.master * CF_bio) %>%  #aligned particle effect concentraiton (1-5000 um)
  ## Surface area ERM
  mutate(mu.sa.mono = particle.surface.area.um2) %>% #define mu_x_mono for alignment to TRM
  mutate(x_LL_sa = SAfnx(a = x1D_set/2, b = x1D_set/2, c = (2/3)*(x1D_set/2))) %>%  #calculate lower ingestible surface area
  mutate(x_UL_sa = SAfnx(a = x2M/2, b = x2M/2, c = (2/3)*(x2M/2))) %>%  #calculate upper ingestible surface area
  mutate(mu.sa.poly = if(a.sa == 2){mux.polyfnx.2(x_UL_sa, x_LL_sa)} else if (a.sa != 2){mux.polyfnx(a.sa, x_UL_sa, x_LL_sa)}) %>% #calculate mu_x_poly for surface area
  mutate(EC_poly_sa.particles.mL = (EC_env_p.particles.mL * mu.sa.mono)/mu.sa.poly) %>%  #calculate polydisperse effect concentration for surface area (particles/mL)
  ## volume ERM
  mutate(mu.v.mono = particle.volume.um3) %>% #define mu_x_mono for alignment to TRM
  mutate(x_LL_v = volumefnx(R = R.ave, L = x1D_set)) %>%  #calculate lower ingestible volume 
  mutate(x_UL_v = volumefnx(R = R.ave, L = x2M)) %>%  #calculate maximum ingestible volume 
  mutate(mu.v.poly = if(a.v == 2){mux.polyfnx.2(x_UL_v, x_LL_v)} else if (a.v != 2){mux.polyfnx(a.v, x_UL_v, x_LL_v)}) %>% #calculate mu_x_poly for volume
  mutate(EC_poly_v.particles.mL = (EC_env_p.particles.mL * mu.v.mono)/mu.v.poly) %>%  #calculate polydisperse effect concentration for volume (particles/mL)
  ## mass ERM
  mutate(mu.m.mono = mass.per.particle.mg * 1000) %>% #define mu_x_mono for alignment to TRM (ug)
  mutate(x_LL_m = massfnx(R = R.ave, L = x1D_set, p = p.ave)) %>%  #calculate lower ingestible mas
  mutate(x_UL_m = massfnx(R = R.ave, L = x2M, p = p.ave)) %>%  #calculate upper ingestible mass
  mutate(mu.m.poly = if(a.m == 2){mux.polyfnx.2(x_UL_m, x_LL_m)} else if (a.m != 2){mux.polyfnx(a.m, x_UL_m, x_LL_m)}) %>% #calculate mu_x_poly for mass
  mutate(EC_poly_m.particles.mL = (EC_env_p.particles.mL * mu.m.mono)/mu.m.poly) %>%  #calculate polydisperse effect concentration for volume (particles/mL)
  ## specific surface area ERM
  mutate(mu.ssa.mono = mu.sa.mono/mu.m.mono) %>% #define mu_x_mono for alignment to TRM (um^2/ug)
  mutate(x_LL_ssa = SSAfnx(a = x1D_set/2, b = x1D_set/2, c = (2/3)*(x1D_set/2), m = x_LL_m)) %>%  #calculate lower ingestible SSA
  mutate(x_UL_ssa = SSAfnx(a = x2M/2, b = x2M/2, c = (2/3)*(x2M/2), m = x_UL_m)) %>%  #calculate upper ingestible SSA  (um^2/ug)
  mutate(mu.ssa.poly = if(a.ssa == 2){mux.polyfnx.2(x_UL_ssa, x_LL_ssa)} else if (a.ssa != 2){mux.polyfnx(a.ssa, x_UL_ssa, x_LL_ssa)}) %>% #calculate mu_x_poly for specific surface area
  mutate(EC_poly_ssa.particles.mL = (EC_env_p.particles.mL * mu.ssa.mono)/mu.ssa.poly) #calculate polydisperse effect concentration for specific surface area (particles/mL)


#### SSD AO Setup ####

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

# build UI
ui <- fluidPage(theme = shinytheme("flatly"), #light, 

                # #### DARK MODE SWITCH ####
                #   div(class = "custom-control custom-switch",
                #     tags$input(id = "dark_mode", type = "checkbox", class = "custom-control-input",
                #       onclick = HTML("Shiny.setInputValue('dark_mode', document.getElementById('dark_mode').value);")),
                #     tags$label("Dark mode", `for` = "dark_mode", class = "custom-control-label")
                #     ),
                 ####
  
  # App title
  titlePanel(title=div(img(src = "main_logo.png", width = "10%", height = "10%"), "Toxicity of Microplastics Explorer: Aquatic Organisms")),

  # Title panel subtext
  tags$div("Logo created by J.C. Leapman.", tags$a(href="https://twitter.com/jcleapman", tags$img(src="twitter.png", width="2%", height="2%"))),
  tags$div("This website is only intended for use by invited participants of the Microplastics Health Effects Workshop."),
  actionButton("database_link", label="Go to Human Health Database", class = "btn-primary", onclick ="window.open('https://sccwrp.shinyapps.io/human_mp_tox_shiny-/', '_blank')", style = "float:right"),
  
  br(), # line break
  
  # Main panel for displaying outputs
  mainPanel(width = 12,
    
      # Output: set of 6 tabs
      tabsetPanel(type = "tabs",

#### Introduction UI ####        
                  tabPanel("1: Introduction", 
                    
                    br(), 
                    h3("What is the Microplastics Toxicity Database?", align = "center"), #Section 1
                    
                    strong(p("This database is a repository for microplastics 
                      toxicity data that will be used to generate key graphics for the Microplastics Health Effects Workshop.")), 
                    
                    p("This web application allows users to explore toxicity 
                    data using an intuitive interface while retaining the diversity and complexity inherent 
                    to microplastics. Data is extracted from existing, peer-reviewed manuscripts containing 
                    toxicity data pertaining to microplastics."),
                  
                    p("Use the numbered tabs at the top of the page to navigate to each section. Each section provides different information or data visualization options. 
                      More specific instructions may be found within each section."),
                    
                    h3("Why was the Microplastics Toxicity Database and Web Application created?", align = "center"), #Section 2
                    
                    p("The database and application tools have been created for use by the participants of the ", a(href = "https://www.sccwrp.org/about/
                      research-areas/additional-research-areas/
                      trash-pollution/microplastics-health-effects-webinar-series/", 'Microplastics Health Effects Workshop', 
                      .noWS = "outside"),".The purpose of this workshop is to identify the most sensitive and biologically critical endpoints associated with microplastics exposure, 
                      prioritize which microplastics characteristics (e.g., size, shape, polymer) that are of greatest biological concern, and identify 
                      critical thresholds for each at which those biological effects become pronounced. Workshop participants will also make reccomendations for future
                      research investments. Workshop findings will be published in a special issue of ", a(href ="https://microplastics.springeropen.com/", 'Microplastics and Nanoplastics', .noOWs = "outside"),". 
                      These findings will be used directly by the state of California to fulfill ", a(href = "https://www.sccwrp.org/about/research-areas/
                      additional-research-areas/trash-pollution/microplastics-health-effects-webinar-series/history-california-microplastics-legislation/", 'legislative mandates', 
                      .noWS = "outside")," regarding the management of microplastics in drinking water and the aquatic environment."),
                   
                    h3("Contributors", align = "center"), #Section 3: Contributors list with links to twitter and github
                 
                    p(align = "center", a(href = "https://www.sccwrp.org/about/staff/leah-thornton-hampton/", 'Dr. Leah Thornton Hampton'),", Southern California Coastal Water Research Project ", 
                      tags$a(href="https://twitter.com/DrLeahTH", tags$img(src="twitter.png", width="2%", height="2%")), tags$a(href="https://github.com/leahth", tags$img(src="github.png", width="2%", height="2%"))),
                    p(align = "center", a(href = "https://agency.calepa.ca.gov/staffdirectory/detail.asp?UID=69294&BDO=7&VW=DET&SL=S", 'Dr. Scott Coffin'),", California State Water Resources Control Board", 
                      tags$a(href="https://twitter.com/DrSCoffin", tags$img(src="twitter.png", width="2%", height="2%")), tags$a(href="https://github.com/ScottCoffin", tags$img(src="github.png", width="2%", height="2%"))),
                    p(align = "center", "Heili Lowman, Southern California Coastal Water Research Project ",
                      tags$a(href="https://twitter.com/heili_lowman", tags$img(src="twitter.png", width="2%", height="2%")), tags$a(href="https://github.com/hlowman", tags$img(src="github.png", width="2%", height="2%"))), 
                    p(align = "center", a(href = "https://www.sfei.org/users/liz-miller", 'Dr. Ezra Miller'),", Aquatic Science Center"),
                    p(align = "center", a(href = "https://rochmanlab.com/people/", 'Dr. Ludovic Hermabessiere'),", University of Toronto", 
                      tags$a(href="https://twitter.com/HermabessiereL", tags$img(src="twitter.png", width="2%", height="2%"))),
                    p(align = "center", a(href = "https://rochmanlab.com/people/", 'Hannah De Frond'),", University of Toronto", 
                        tags$a(href="https://twitter.com/HanDefrond", tags$img(src="twitter.png", width="2%", height="2%"))),
                    p(align = "center", "Emily Darin, Southern California Coastal Water Research Project",
                      tags$a(href="https://github.com/EmilyDarin", tags$img(src="github.png", width="2%", height="2%"))),
                    p(align = "center", "Syd Kotar, Southern California Coastal Water Research Project"),
                    p(align = "center", "Sarah Khan, Southern California Coastal Water Research Project"),
                    p(align = "center", a(href = "https://www.wur.nl/en/Persons/Bart-prof.dr.-AA-Bart-Koelmans.htm", 'Dr. Bart Koelmans'),", Wageningen University",
                     tags$a(href="https://twitter.com/MicroplasticLab", tags$img(src="twitter.png", width="2%", height="2%"))),
                    p(align = "center", a(href = "https://rochmanlab.com/", 'Dr. Chelsea Rochman'),", University of Toronto",
                      tags$a(href="https://twitter.com/ChelseaRochman", tags$img(src="twitter.png", width="2%", height="2%"))),
                    p(align = "center", a(href = "https://www.sccwrp.org/about/staff/alvina-mehinto/", 'Dr. Alvina Mehinto'),", Southern California Coastal Water Research Project"), 
                    p(align = "center", a(href = "https://www.sccwrp.org/about/staff/steve-weisberg/", 'Dr. Steve Weisberg'),", Southern California Coastal Water Research Project"), 
                  
                    #Logos with links to organizations
                    
                  splitLayout(align = "center", 
                  tags$a(href="https://www.waterboards.ca.gov", tags$img(src="waterboard.png", width = "100%", height = "100%")),
                  tags$a(href="https://www.sccwrp.org", tags$img(src="sccwrp.png", width = "100%", height = "100%")),
                  tags$a(href="https://www.utoronto.ca", tags$img(src="toronto.png", width = "100%", height = "100%")),
                  tags$a(href="https://www.sfei.org/", tags$img(src="sfei.png", width = "100%", height = "100%"))),
                  
                    br(), 
                    
                    verbatimTextOutput(outputId = "Introduction1")),
                
#### Overview AO UI ####

tabPanel("2: Overview", 
         br(), 
         h3("Overview of Toxicological Effects in Aquatic Organisms", align = "center"),
         br(),
         p("Each bar displays the total number of measured endpoints where a statistically signifcant effect was detected (Y) or where a measurement was made but a significant effect was not detected (N)."), 
         br(), 
         p("Detailed descriptions of data categories may be found under the Resources tab."),
         br(),
           
#pickerInput(inputId = "Emily_check", # endpoint checklist
            #label = "Overview", 
            #choices = levels(Final_effect_dataset$plot_f),
            #selected = levels(Final_effect_dataset$plot_f), 
            #options = list(`actions-box` = TRUE), # option to de/select all
            #multiple = TRUE), # allows for multiple inputs
            #br(),

column(width = 12,
        column(width = 12,
        plotOutput(outputId = "tax_plot"),
        br())), 
       
column(width = 12,
       column(width = 6,
              plotOutput(outputId = "vivo_plot"),
              br()), 

    
        column(width = 6,
              plotOutput(outputId = "life_plot"),
              br())), 
       
column(width = 12,
       
      column(width = 6,
              plotOutput(outputId = "polymer_plot"),
              br()), 

        

      column(width = 6,
              plotOutput(outputId = "exposure_plot"),
              br())), 

column(width = 12,   

      column(width = 6,
              plotOutput(outputId = "shape_plot"),
              br()),

      column(width = 6,
       plotOutput(outputId = "size_plot"),
       br()))),


#### Exploration AO UI ####
                tabPanel("3: Exploration",
                    shinyjs::useShinyjs(), # requires package for "reset" button, DO NOT DELETE - make sure to add any new widget to the reset_input in the server
                    id = "heili-tab", # adds ID for resetting Heili's tab's filters
                    
                    h3("Exploration of Toxicological Effects in Aquatic Organisms", align = "center"),
                    br(), 
                    p("Each figure displays a different metric along the y-axis - organism group, broad endpoint category, specific endpoint category, size, shape, 
                      and polymer, respectively.The values in the parentheses represent the number of measurements and studies, respectively, of each metric along the y-axis."),
                    br(),
                    p("The data displayed in these figures only display data from in vitro studies or in vivo studies where doses were reported 
                    as mass or counts per volume - other dosing units (e.g., particle mass/food mass) 
                    are not displayed but are available in the complete database file."),
                    br(), 
                    p("Filter the data: The data may be filtered using the drop-down menus located below. Then, click the 'Update Filters' button 
                      to refresh the data displayed according to your selections."),
                    br(),
                    p("Change the plot type: The data may be visualized as a boxplot, violin plot or beeswarm plot using the drop-down menu below. Users may also visualize all individual data points by using the checkbox."),
                    br(),
                    p("Download the data: Click the 'Download Data' button to retrieve the selected dataset as a '.csv' file."),
                    br(), 
                    
                    
                    # widget headers
                    column(width=12,
                      
                      column(width = 3,
                        h4("Effects")),
                      
                      column(width = 3,
                        h4("Particle Characteristics")),
                      
                      column(width = 3,
                        h4("Biological Factors"))),
                    
                    # widgets
                    h4("Filters"),
                    column(width = 12,
                      
                      column(width = 3,
                        pickerInput(inputId = "lvl1_check", # endpoint checklist
                        label = "Broad Endpoint Category:", 
                        choices = levels(aoc_setup$lvl1_f),
                        selected = levels(aoc_setup$lvl1_f),
                        options = list(`actions-box` = TRUE), # option to de/select all
                        multiple = TRUE)), # allows for multiple inputs
                      
                      column(width = 3,
                        pickerInput(inputId = "poly_check", # polymer checklist
                        label = "Polymer:", 
                        choices = levels(aoc_setup$poly_f),
                        selected = levels(aoc_setup$poly_f),
                        options = list(`actions-box` = TRUE), 
                        multiple = TRUE)),
                      
                      column(width = 3,
                        pickerInput(inputId = "organism_check", # organismal group checklist
                        label = "Organisms:", 
                        choices = levels(aoc_setup$org_f),
                        selected = levels(aoc_setup$org_f),
                        options = list(`actions-box` = TRUE), 
                        multiple = TRUE)), 
                      
                      column(width = 3, 
                        pickerInput(inputId = "bio_check", # bio org checklist
                        label = "Level of Biological Organization", 
                        choices = levels(aoc_setup$bio_f),
                        selected = levels(aoc_setup$bio_f),
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE))), 
                      
                    # New row of widgets
                    column(width = 12,
                      
                      column(width = 3,
                         htmlOutput("secondSelection")), # dependent endpoint checklist
                      
                      column(width = 3,
                        pickerInput(inputId = "shape_check", # shape checklist
                        label = "Shape:", 
                        choices = levels(aoc_setup$shape_f),
                        selected = levels(aoc_setup$shape_f),
                        options = list(`actions-box` = TRUE), 
                        multiple = TRUE)),
                      
                      column(width = 3,
                        pickerInput(inputId = "env_check", # Environment checklist
                        label = "Environment:", 
                        choices = levels(aoc_setup$env_f),
                        selected = levels(aoc_setup$env_f),
                        options = list(`actions-box` = TRUE), 
                        multiple = TRUE)),
                      
                      column(width = 3,
                             htmlOutput("SpeciesSelection_exp"))), # dependent checklist
                      
                    # New row of widgets
                    column(width = 12,
                        
                      column(width = 3,
                        pickerInput(inputId = "effect_check",  # Effect Yes/No widget
                        label = "Effect:",
                        choices = levels(aoc_setup$effect_f),
                        selected = levels(aoc_setup$effect_f),
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE)),
                      
                      column(width = 3,
                        pickerInput(inputId = "size_check", # Environment checklist
                        label = "Size Category:", 
                        choices = levels(aoc_setup$size_f),
                        selected = levels(aoc_setup$size_f),
                        options = list(`actions-box` = TRUE), 
                        multiple = TRUE)),
                      
                      column(width = 3,
                        pickerInput(inputId = "life_check", # life stage checklist
                        label = "Life Stages:", 
                        choices = levels(aoc_setup$life_f),
                        selected = levels(aoc_setup$life_f),
                        options = list(`actions-box` = TRUE), 
                        multiple = TRUE)),
                      
                      column(width = 3,
                             pickerInput(inputId = "acute.chronic_check", # chronic/acute checklist
                                         label = "Exposure Duration:", 
                                         choices = levels(aoc_setup$acute.chronic_f),
                                         selected = levels(aoc_setup$acute.chronic_f),
                                         options = list(`actions-box` = TRUE), 
                                         multiple = TRUE))),

                    # second row of widget headers
                    column(width=12,
                           
                           column(width = 3,
                                  h4("Quality Criteria")),
                           
                           column(width = 3,
                                  h4("Exposure Concentrations")),
                           
                           column(width = 3,
                                  h4("Aesthetics"))),
                    
                    #New row of widgets
                    
                    column(width = 12,
                           
                           column(width = 3,
                                  pickerInput(inputId = "tech_tier_zero_check", # chronic/acute checklist
                                              label = "Technical Quality:", 
                                              choices = levels(aoc_setup$tier_zero_tech_f),
                                              selected = levels(aoc_setup$tier_zero_tech_f),
                                              options = list(`actions-box` = TRUE), 
                                              multiple = TRUE)),
                           
                           p("Choose dosing metric (if ERM calculations desired, choose particles/mL)"),
                           column(width = 3, 
                                  radioButtons(inputId = "dose_check", # dosing units
                                               label = "Particles/mL, mg/L, or um3/mL:",
                                               choices = c("Particles/mL", "mg/L", "um3/mL"),
                                               selected = "mg/L")),
                           
                           column(width = 3,
                                  selectInput(inputId = "plot.type", "Plot Type:", 
                                       list(boxplot = "boxplot", violin = "violin", beeswarm = "beeswarm")),
                                  checkboxInput(inputId = "show.points", "Show All Points", FALSE))),
                           
                  #New row of widgets
                    
                  column(width = 12,
                         
                         column(width = 3,
                                pickerInput(inputId = "risk_tier_zero_check", # chronic/acute checklist
                                            label = "Applicability for Risk Assessment:", 
                                            choices = levels(aoc_setup$tier_zero_risk_f),
                                            selected = levels(aoc_setup$tier_zero_risk_f),
                                            options = list(`actions-box` = TRUE), 
                                            multiple = TRUE)),
                         
                         column(width = 3, 
                                
                                p("Concentrations may be reported in mass/volume or particle #/volume (or sometimes both). Particle volume has been estimated based on morphology. See resources tab for details on calculations."),
                                
                                radioButtons(inputId = "Rep_Con_rad",
                                             label = "Do you want to use just the reported, just the converted, or all exposure concentrations?",
                                             choices = c("reported", "converted", "all"),
                                             selected = "all")),
                         
                         column(width = 3, 
                                selectInput(inputId = "theme.type_exp", "Dark or Light Mode:", 
                                            list(light = "light", dark = "dark")),
                         
                                selectInput(inputId = "color.type_exp", "Color Theme:", 
                                            list(default = "default", viridis = "viridis", brewer = "brewer", tron = "tron", locusZoom = "locusZoom", d3 = "d3", Nature = "Nature", JAMA = "JAMA"))),
                         ), #close out column
                  
                  #Ecologically Relevant Metric Row
                  conditionalPanel(condition = "input.dose_check == 'Particles/mL'",
                  h4("Ecologically Relevant Metric Alignment"),
                  p("A monodisperse effect concentration (e.g. 5 micron spheres) may be re-scaled to a default size range (e.g. 1 - 5,000 microns) using methods described in Kooi et al (2021). Re-scaling to a default size range allows direct comparison to exposure concentrations for a default size range (which may also be re-scaled). The following radio buttons apply corrections for bioavailability (i.e. limiting available particles to max ingestable size), and a further correction for the ecologically relevant metric (ERM). For a given ERM, the threshold may be related to both mono- or polydisperse particles interchangeably so long as the total magnitude of ERM remains the same (Koelmans et al, 2020). If, for example, 'volume' is chosen below as an ERM, the monodisperse effect concentration is first corrected for bioavailability and aligned to whichever default size range the user chooses below. This aligned threshold (in particles/mL) is then multiplied by a correction for polydisperse volume based on the average volumes for the given range of microplastics in the environment "),
                  column(width = 12,
                         
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
                                             label = "Lower length for default size range (um)",
                                             value = 1)),
                         # upper length input
                         column(width = 3,
                                numericInput(inputId = "upper_length_exploration",
                                             label = "Upper length the default size range (um)",
                                             value = 5000))
                          ), #end of 12-point column
                  ), #end of conditional Panel


                    # New row of widgets
                    column(width=12,
                        column(width = 3,
                        actionButton("go", "Update Filters", class = "btn-success")), # adds update action button
                    # "go" is the internal name to refer to the button
                    # "Update Filters" is the title that appears on the app

                        column(width = 3,
                        downloadButton("downloadData", "Download Data", class = "btn-info")), # adds download button
                       
                    # "downloadData" is the internal name
                    # "Download Data" is the title that appears on the button
                      
                        column(width = 3,
                        actionButton("reset_input", "Reset Filters"))), # adds update button
                      
                      # "Reset_input" is the internal name
                      # "Reset Filter" is the title that appears on the button  
                    
                    # New row
                    column(width=12,  
                        column(width = 3,
                          br(),
                          strong(p("To Begin: Click the 'Update Filters' button above.")),
                          br()),
                        
                        column(width = 3, offset = 3,
                          br(),
                          strong(p("To Reset: Click the 'Reset Filters' button above, followed by the 'Update Filters' button to the left.")),
                          br())), 
                    
                    # New row
                    column(width = 12,
                    hr(), # adds divider
                    
                    column(width = 12,
                    plotOutput(outputId = "organism_plot_react", height = "600px"),
                    br())), 
                    
                    column(width = 12,
                  
                    column(width = 12,
                    plotOutput(outputId = "lvl_plot_react", height = "600px"),
                    br())), 

                    column(width = 12,
                    
                    column(width = 12,
                    plotOutput(outputId = "lvl2_plot_react", height = "600px"),
                    br())), 
                    
                    column(width = 12,
                           
                    column(width = 12,
                    plotOutput(outputId = "size_plot_react", height = "600px"),
                    br())), 
                    
                    column(width = 12,
                  
                    column(width = 12,
                    plotOutput(outputId = "shape_plot_react", height = "600px"),
                    br())), 
                    
                    column(width = 12,
                
                    column(width = 12,
                    plotOutput(outputId = "poly_plot_react", height = "600px"),
                    br()))), 

#### SSD AO UI ####
                  tabPanel("4: Species Sensitivity Distribution", 
                    br(), # line break
                    h3("Species Sensitivity Distribution", align = "center"),
                    p("Species sensitivity distributions (SSDs) are cumulative probability distributions that estimate the percent of species affected by a given concentration of exposure using Maximum Likelihood and model averaging. A useful metric often used for setting risk-based thresholds is the concentration that affects 5% of the species, and is reffered to as the 5% Hazard Concentration (HC). For more information on SSDs, refer to", a(href = "https://bit.ly/2Hy4q10", 'Posthuma, Suter II, and Traas (2001).')),
                    br(), # line break
                    p("The choice of effect metrics (e.g. NOEC, LOEC, HONEC, ECXX and LCXX) should be carefully considered. Assessment factors are available for converting acute exposures to chronic exposure and estimating NOECs from other effect metrics (e.g. LOEC's), according to the methods described in ", a(href = "https://setac.onlinelibrary.wiley.com/doi/epdf/10.1002/ieam.4214", 'Wigger et al (2019).'), "In brief, an assessment factor of 10 is applied to convert LC/EC25-50 to NOEC, 2 to convert EC/LC20, LOEC, or MIC to NOEC. LC10, EC10 and HONEC are considered equivalent to LOEC. An assessment factor of 10 is applied to convert acute-to-chronic, with determinations of such categories dependent on taxa, as defined in the reference."),
                    br(),
                    p("Use the options below to filter the toxicity thresholds dataset. Once complete, hit the 'submit' button"),
                    br(),
                    
 
                    # Row 1 of Headers
                    column(width=12,
                           
                           column(width = 3,
                                  h4("Environment & Organism Type")),
                    ), #closes out column
                    
                    # Row 1 of Widgets
                    column(width = 12,
                           
                           #Environment widget
                           column(width = 2,
                                  # alternative to fully listed checklists
                                  # requires shinyWidgets package
                                  pickerInput(inputId = "env_check_ssd", # environment checklist
                                              label = "Environment:", 
                                              choices = levels(aoc_z$env_f),
                                              selected = levels(aoc_z$env_f),   
                                              options = list(`actions-box` = TRUE), # option to de/select all
                                              multiple = TRUE)), # allows for multiple inputs
                           
                           #Organism group widget - reactive to environment
                           column(width = 2,
                                  htmlOutput("GroupSelection")), # organism checklist
                          
                           #Species widget - reactive to environment 
                           column(width = 2,
                                  htmlOutput("SpeciesSelection")),
                           

                           #level of biological organization - reactive to environment, group, and species
                           column(width = 2,
                                  htmlOutput("BiologicalSelection")),
                           
                           #acute/chronic widget
                           column(width = 2,
                                  pickerInput(inputId = "acute.chronic_check_ssd", # chronic/acute checklist
                                              label = "Exposure Duration Type:", 
                                              choices = levels(aoc_z$acute.chronic_f),
                                              selected = levels(aoc_z$acute.chronic_f),
                                              options = list(`actions-box` = TRUE), 
                                              multiple = TRUE)),
                           
                           ), #closes out column
                    
                    
                    # Row 2 of Headers
                    column(width=12,
                           
                           column(width = 3,
                                  h4("Particle Characteristics")),
                    
                           column(width = 3,
                                  h4("Effects")),
                           
                           column(width = 3,
                                  h4("Quality Criteria")),
                           
                    ), #closes out column      
                    
                    # Row 3 of widgets 
                    column(width = 12,
                           
                           #Size widget
                           column(width = 3,
                                  pickerInput(inputId = "size_check_ssd", # organism checklist
                                              label = "Sizes:",
                                              choices = levels(aoc_z$size_f),
                                              selected = levels(aoc_z$size_f),
                                              options = list(`actions-box` = TRUE), # option to de/select all
                                              multiple = TRUE)), # allows for multiple inputs
                           
                           #Endpoint widget - Broad
                           column(width = 3,
                                  htmlOutput("lvl1Selection")), # allows for multiple inputs
                          
                           #quality criteria widget for technical quality
                           column(width = 3,
                                  pickerInput(inputId = "tech_tier_zero_check_ssd", # chronic/acute checklist
                                              label = "Technical Quality:", 
                                              choices = levels(aoc_z$tier_zero_tech_f),
                                              selected = levels(aoc_z$tier_zero_tech_f),
                                              options = list(`actions-box` = TRUE), 
                                              multiple = TRUE)),
                           
                            ), #closes out column
                    
                    #Row 4 of widgets 
                    column(width = 12,    
                           
                           #Morphology widget
                           column(width = 3,
                                  htmlOutput("shapeSelection")), # dependent on sizes
                           
                           #Endpoint widget - Specific
                           column(width = 3,
                                  htmlOutput("lvl2Selection")), #specific endpoint based on previous checkbox
                           
                           #quality criteria widget for applicability to risk assessment
                           column(width = 3,
                                  pickerInput(inputId = "risk_tier_zero_check_ssd", # chronic/acute checklist
                                              label = "Applicability for Risk Assessment:", 
                                              choices = levels(aoc_z$tier_zero_risk_f),
                                              selected = levels(aoc_z$tier_zero_risk_f),
                                              options = list(`actions-box` = TRUE), 
                                              multiple = TRUE)),
                           
                           ),#close out column
                    
                    #Row 5 of widgets
                    column(width = 12,
                           
                           #Polymer widget
                           column(width = 3,
                                  htmlOutput("polySelection")),# polymer selection based on other inputs
                           

                           
                           ),#close out column
                    
                    # Row 3 of Headers
                    column(width=12,
                           
                           column(width = 3,
                                  h4("Exposure Concentrations")),
                           
                    ), #closes out column  
                    
                    
                    #Row 6 of widgets
                    column(width = 12, 
                           
                           #Dose unit selection
                           column(width = 3,
                                  radioButtons(inputId = "particle_mass_check_ssd", # organism checklist
                                               label = "Particles/mL, mg/L, or volume(um3)/mL?",
                                               choices = c("Particles/mL", "mg/L", "um3/mL"),
                                               selected = "Particles/mL")),
                           

                           #converted or reported doses widget
                           column(width = 3, 
                                  radioButtons(
                                    inputId = "Reported_Converted_rad",
                                    label = "Do you want to use just the reported, just the converted, or all exposure concentrations?",
                                    choices = list("reported", "converted", "all"),
                                    selected = "all")),
                           
                           ),#closes out column 
                    
                    #Dose conversion description
                    column(width = 12,
                           column(width = 6, 
                          p("Concentrations may be reported in mass/volume or particle #/volume (or sometimes both). Particle volume has been estimated based on morphology. See resources tab for details on calculations.")),
                    
                    ), #closes out column 
                    
                    # Row 4 of Headers
                    column(width=12,
                           
                           column(width = 3,
                                  h4("Advanced Options")),
                           
                    ), #closes out column  
                    
                    column(width = 12,
                           
                           #Effect metric widget
                           column(width = 3,
                                  pickerInput(inputId = "effect.metric_rad_ssd", # effect metric checklist
                                              label = "Effect Metric:",
                                              choices = levels(aoc_z$effect.metric),
                                              selected = c("LOEC", "NOEC"),
                                              options = list(`actions-box` = TRUE), 
                                              multiple = TRUE)), # allows for multiple inputs
                          
                           #Assessment factor - time
                           column(width = 3,
                                  pickerInput(inputId = "AF.time_rad_ssd", # acute/chronic assessment factor
                                        label = "Apply Assessment Factor for acute and sub-chronic to chronic?",
                                        choices = c("Yes", "No"),
                                        options = list(`actions-box` = TRUE),
                                        selected = "No")),
                           
                           #Assessment factor - noec conversion
                           column(width = 3,
                                  pickerInput(inputId = "AF.noec_rad_ssd", # noec/loc assessment factor
                                        label = "Apply Assessment Factor to convert dose descriptors into NOECs?",
                                        choices = c("Yes", "No"),
                                        options = list(`actions-box` = TRUE),
                                        selected = "No")),
                           
                           #concentration selector (minimum, lower 95% CI, median, mean)
                           column(width = 3,
                                  pickerInput(
                                    inputId = "conc.select.rad",
                                    label = "What summary statistic should be used for each species?",
                                    choices = list("Minimum", "Lower 95% CI", "1st Quartile", "Median", "Mean", "3rd Quartile", "Upper 95% CI", "Maximum"),
                                    selected = "Mean")),
                           
                    ),#close out column
                    
                    #Ecologically Relevant Metric Row
                    conditionalPanel(condition = "input.particle_mass_check_ssd == 'Particles/mL'",
                                     h4("Ecologically Relevant Metric Alignment"),
                                     p("A monodisperse effect concentration (e.g. 5 micron spheres) may be re-scaled to a default size range (e.g. 1 - 5,000 microns) using methods described in Kooi et al (2021). Re-scaling to a default size range allows direct comparison to exposure concentrations for a default size range (which may also be re-scaled). The following radio buttons apply corrections for bioavailability (i.e. limiting available particles to max ingestable size), and a further correction for the ecologically relevant metric (ERM). For a given ERM, the threshold may be related to both mono- or polydisperse particles interchangeably so long as the total magnitude of ERM remains the same (Koelmans et al, 2020). If, for example, 'volume' is chosen below as an ERM, the monodisperse effect concentration is first corrected for bioavailability and aligned to whichever default size range the user chooses below. This aligned threshold (in particles/mL) is then multiplied by a correction for polydisperse volume based on the average volumes for the given range of microplastics in the environment "),
                                     column(width = 12,
                                            
                                            #ERM Checkbox
                                            column(width = 3,
                                                   radioButtons(inputId = "ERM_check_ssd", # ERM (particle, surface area, mass, volume, specific surface area)
                                                                label = "Ecologically Relevant Metric:",
                                                                choices = c("Unaligned","Particles", "Surface Area", "Volume", "Mass", "Specific Surface Area"),
                                                                selected = "Unaligned")),
                                            # #Alpha checkbox
                                            # column(width = 3,
                                            #        numericInput(inputId = "alpha_ssd",
                                            #                     label = "Length Alpha Value (default of 2.07 for marine surface water)",
                                            #                     value = 2.07)),
                                           
                                            # lower length input
                                            column(width = 3,
                                                   numericInput(inputId = "lower_length_ssd",
                                                                label = "Lower length for default size range (um)",
                                                                value = 1)),
                                            # upper length input
                                            column(width = 3,
                                                   numericInput(inputId = "upper_length_ssd",
                                                                label = "Upper length the default size range (um)",
                                                                value = 5000))
                                     ), #end of 12-point column
                    ), #end of conditional Panel
                    
                    br(),
                            column(width = 12,
                                  actionButton("SSDgo", "Submit", class = "btn-success"),
                                  align = "center"), # adds action button 
                    # "SSDgo" is the internal name to refer to the button
                    # "Update" is the title that appears on the app
                           
                    # br(), 
                    # p("Please wait a moment while maximum likelihood estimation is calculated data based on your choices...", align = "center"),
                    # br(),

                    
                    mainPanel("Filtered Data Based on Choices Above:",
                              br(),
                              DT::dataTableOutput(outputId = "aoc_filter_ssd_table"),
                              p("The figure below displays minimum observed effect concentrations for a range of species along with three common distributions"),
                              br(),
                              plotOutput(outputId = "autoplot_dists_react"),
                              p("Different distributions can be fit to the data. Below are some common distributions (llogis = log-logistic; lnorm = log-normal; lgumbel = log-Gumbel)."),
                              br(),
                              h4("Goodness of Fit Table", align = "center"),
                              DT::dataTableOutput(outputId = "table_gof_react"), #using DT package provides better functionality
                              br(),
                              p("The best fitting model is that with the smallest Information Criteria value. Note that several informaiton criteria are listed ", a(href ="http://a100.gov.bc.ca/appsdata/acat/documents/r57400/2_1568399094009_8398900200.pdf", 'Schwarz and Tillmanns (2019)', .noOWs = "outside"),"."),
                              br(),
                              p("Following ", a(href ="https://books.google.com/books?id=c45qtw7tDrsC&lpg=PA113&ots=Zn9Neau5aM&dq=burnham%20and%20anderson%20(2002)%20species%20sensitivity&lr&pg=PA113#v=onepage&q&f=false", 'Burnham and Anderson (2002)', .noOWs = "outside"),", the aicc is recommended for model selection (for which the lowest value is the best fitting model), and is the default information criteria used to predict confidence intervals (unless otherwise specified below). Options inlcude aicc (Akaike's Information Criteria Corrected for sample size; default), aic (Akaike's Information Criteria), or bic (Bayseian Information Criteria). Choose the information criteria used to estimate confidence intervals below:"),
                              br(),
                              column(width = 12,
                                     pickerInput(inputId = "pred_ic_ssd", # prediction model averaging checklist
                                                 label = "Information Criteria:",
                                                 choices = c("aicc", "aic", "bic"), #tells the model which information criteria to use to select best fit
                                                 selected = "aicc",
                                                 options = list(`actions-box` = FALSE), # option to de/select all
                                                 multiple = FALSE)),
                              br(),
                              p("Understanding that other distributions may fit the data almost as well as the 'best' distribution (as evidenced by delta values <2), it is recommended to average such fits based on the relative aicc weights of the distributions (indicated by the weight column in the goodness of fit table) ", a(href ="https://books.google.com/books?id=c45qtw7tDrsC&lpg=PA113&ots=Zn9Neau5aM&dq=burnham%20and%20anderson%20(2002)%20species%20sensitivity&lr&pg=PA113#v=onepage&q&f=false", 'Burnham and Anderson (2002)', .noOWs = "outside"),". Below, choose whether or not multiple distributions should be averaged (weighted according to above table) or if a single distribution should be used."),
                              br(),
                              column(width = 12,
                                     pickerInput(inputId = "pred_ave_ssd", # prediction model averaging checklist
                                                 label = "Averaging:",
                                                 choices = c("TRUE", "FALSE"), #tells the model to average or not
                                                 selected = NULL,
                                                 options = list(`actions-box` = FALSE), # option to de/select all
                                                 multiple = FALSE)),
                              br(),
                              conditionalPanel("input.pred_ave_ssd == 'FALSE'",
                                               p("Choose which distribution will be plotted below (llogis = log-logistic; lnorm = log-normal; lgumbel = log-Gumbel):"),
                                               pickerInput(inputId = "dist",
                                                           label = "Distribution:",
                                                           choices = c("weibull", "llogis", "lnorm", "gamma", "lgumbel"),
                                                           selected = NULL,
                                                           options = list(`actions-box` = FALSE), # option to de/select all
                                                           multiple = FALSE)),
                              br(),
                              p("Choose the hazard concentration (% of species affected)"),
                              numericInput(inputId = "pred_hc_ssd", #hazard concentration input
                                           label = "Hazard Concentration (%)",
                                           value = 5,
                                           min = 0.1,
                                           step = 1,
                                           max = 0.99),
                              br(),
                              p("Choose the number of bootstrap iterations (greater n yields higher confidence, but longer compute time"),
                              br(),
                              numericInput(inputId = "nbootInput", #hazard concentration input
                                           label = "Bootstrap Iterations (n)",
                                           value = 10,
                                           min = 10,
                                           step = 10,
                                           max = 10000),
                              br(),
                              column(width = 12,
                                actionButton("ssdPred", "Predict", class = "btn-success"),
                                align = "center"), # adds action button, "SSDpred" is the internal name to refer to the button # "Predict" is the title that appears on the app
                              br(),
                              p("Please be patient as maximum likelihood estimations are calculated. If a high number of bootstrap simulations are chosen (>100), this may take up to several minutes."),
                              br(),
                              plotOutput(outputId = "aoc_ssd_ggplot", width = "140%", height = "500px", hover = hoverOpts(id = "plot_hover")),
                              verbatimTextOutput("info"),
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
                                     column(width = 4,
                                     downloadButton("downloadSsdPlot", "Download Plot", class = "btn-info"), #download ssdplot
                                     align = "center"),
                                     
                              br(),
                              
                              selectInput(inputId = "theme.type", "Dark or Light Mode:", 
                                          list(light = "light", dark = "dark")),
                              selectInput(inputId = "color.type", "Color Theme:", 
                                          list(viridis = "viridis", brewer = "brewer", tron = "tron", locusZoom = "locusZoom", d3 = "d3", Nature = "Nature", JAMA = "JAMA")),
                              p("The model-averaged 95% confidence interval is indicated by the shaded band and the model-averaged Hazard Concentration (user input value) by the dotted line."),
                              br(),
                              p("Model predictions can also be viewed in tabular format."),
                              br(),
                              h4("SSD Table", align = "center"),
                              DT::dataTableOutput(outputId = "ssd_pred_table"),
                              br(),
                              h4("Additional Diagnostics"),
                              br(),
                              plotOutput(outputId = "ssd_CF_plot"),
                              br(),
                              plotOutput(outputId = "ssd_qq_plot"),
                              plotOutput(outputId = "ssd_pp_plot"),
                              plotOutput(outputId = "ssd_dens_plot"),
                              h4(align = "center", "Credits"),
                              p(align = "center", style = "font-size: 12px;", "This app is built using the R package ", a(href = "https://github.com/bcgov/ssdtools", 'ssdtools', .noWS = "outside"), " version 0.3.2 and share the same functionality."),
                              p(align = "center", style = "font-size: 12px;", "Citation: Thorley, J. and Schwarz C., (2018). ssdtools An R package to fit species Sensitivity Distributions. Journal of Open Source Software, 3(31), 1082. https://doi.org/10.21105/joss.01082."),
                          ) #closes out scott's main panel
                    ), #closes out Scott's tab panel

#### Endpoint Category UI ####

tabPanel("5: Endpoint Categorization", 
         br(),
         p("This plot displays the categorization of measured endpoints in the database. Nodes correspond to the Broad Endpoint Category (blue), 
         the Specific Endpoint Category (green), Endpoints (pink) and the level of biological organization (purple). Alternatively, the widget below may be used to select
         endpoints at various Biological Levels of Organization. Click nodes to expand and collapse the plot."),
         br(),
         
         column(width = 12,
         
         column(width = 3, 
                pickerInput(inputId = "bio_check_endpoint", # bio org checklist
                            label = "Level of Biological Organization", 
                            choices = levels(aoc_endpoint$bio_f),
                            selected = levels(aoc_endpoint$bio_f),
                            options = list(`actions-box` = TRUE),
                            multiple = TRUE)),
         ), #closes out button column
         
         column(width = 12,
         
         #Go button
         column(width = 3,
                actionButton("go_endpoint", "Update Filters", class = "btn-success")), # adds update action button
         
         ), #closes out button column
         
         #collapsible tree plot
         collapsibleTreeOutput("plot", height = "800px"),
         
         ), #closes out tab

#### Resources UI ####

tabPanel("6: Resources", 
         br(),     
         h3(align = "center", a(href = "https://sccwrp-my.sharepoint.com/:b:/g/personal/leahth_sccwrp_org/EYUFX1dOfSdGuHSfrUDcnewBxgttfTCOwom90hrt5nx1FA?e=jFXEyQ", 'Data Category Descriptions')),
         br(),
         h3(align = "center", a(href = "https://sccwrp-my.sharepoint.com/:b:/g/personal/leahth_sccwrp_org/EVd-oEZ-xxtJnWdOCC7KHfoBIOO3ByJz7omFoeruD0W6Sw?e=a3weoV", 'Assessment Factor Descriptions')),
         br(),
         h3(align = "center", a(href = "https://sccwrp-my.sharepoint.com/:b:/g/personal/leahth_sccwrp_org/EXOluRMsb_RPpjsqTjhmuaUBNz3Pd9vkl7Hl09lKFxaxEA?e=AgH5bs", 'Quality Screening: Red Criteria')),
         br(),
         h3(align = "center", a(href = "https://sccwrp-my.sharepoint.com/:b:/g/personal/leahth_sccwrp_org/ETy8vDCXe_pAq88Ky0Xob1gBmCdAXYCsEwDFqCfDTL-DNA?e=e7Ic21", 'Aquatic Organisms Study List')),
         br(),
         h3(align = "center", a(href = "https://sccwrp-my.sharepoint.com/:b:/g/personal/leahth_sccwrp_org/EXf0crCKDPVHo5xBEdw4PQwBxA8cnu0x4WY477CuEzZcPw?e=qs00V3", 'Dose Conversion Methods')),
         
         
         verbatimTextOutput(outputId = "Leah3")),

#### Contact UI ####

tabPanel("7: Contact", 
         br(),
         h4("For scientific questions, please contact Dr. Leah Thornton Hampton (leahth@sccwrp.org)."),
         br(),
         h4("If you encounter technical problems with the web application, please contact Emily Darin (Emily.Darin@student.csulb.edu)."),
         
         verbatimTextOutput(outputId = "Leah4"))

#following three parentheses close out UI. Do not delete. 
        )))   
     

#### Server ####
server <- function (input, output){  #dark mode: #(input, output, session) {

  # # Theme Switch (comment out until Shiny updates)
  # observe({session$setCurrentTheme(
  #   if (isTRUE(input$dark_mode)) dark else light)
  #   })

#### Introduction S ####

  # Introduction does not have any reactive features.
  
#### Overview AO S ####
  
  # Effect plot code for check box 
  
  # Insert the right number of plot output objects into the page using the function from the setup section.
   output$polymer_plot <- renderPlot({
    
    # generate plot
     ggplot(polyfinal,aes(fill=effect, y= logEndpoints, x= polymer, Percent=Percent)) +
       geom_bar(position="stack", stat="identity") +
       geom_text(aes(label= paste0(Endpoints)), position = position_stack(vjust = 0.5),colour="black", size = 5) +
       scale_fill_manual(values = cal_palette("seagrass"))+
       theme_classic() +
       ylab("Number of Endpoints Measured") +
       labs(fill="Effect") +
       ggtitle("Polymer Type")+
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
       ggtitle("In Vitro or In Vivo")+
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
       ggtitle("Particle Size")+
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
       ggtitle("Plastic Shapes")+
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
       ggtitle("Life Stage")+
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
       ggtitle("Organism Group")+
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
       ggtitle("Exposure Route")+
       guides(x = guide_axis(angle = 45))+
       theme(text = element_text(size=17),plot.title = element_text(hjust = 0.5, face="bold"))+
       theme(legend.position = "right",
             axis.ticks= element_blank(),
             axis.text.x = element_text(),
             axis.text.y = element_blank(),
             axis.title.x = element_blank())
   })
   
   
   
   
#### Exploration AO S ####
  
  #Create dependent dropdown checklists: select lvl2 by lvl1.
  output$secondSelection <- renderUI({
    
    lvl1_c <- input$lvl1_check # assign level values to "lvl1_c"
    
    aoc_new <- aoc_setup %>% # take original dataset
      filter(lvl1_f %in% lvl1_c) %>% # filter by level inputs
      mutate(lvl2_f_new = factor(as.character(lvl2_f))) # new subset of factors
      
    pickerInput(inputId = "lvl2_check", 
      label = "Specific Endpoint within Broad Category:", 
      choices = levels(aoc_new$lvl2_f_new),
      selected = levels(aoc_new$lvl2_f_new),
      options = list(`actions-box` = TRUE),
      multiple = TRUE)})
   
   #Create dependent dropdown checklists: select Species by env and group
   output$SpeciesSelection_exp <- renderUI({
     
     #Assign user inputs to variables for this reactive
     env_c <- input$env_check #assign environments
     org_c <- input$organism_check # assign organism input values to "org_c"
     #filter based on user input
     aoc_new <- aoc_setup %>% # take original dataset
       filter(env_f %in% env_c) %>% #filter by environment inputs
       filter(org_f %in% org_c) %>% # filter by organism inputs
       mutate(species_new = factor(as.character(species_f))) # new subset of factors
     
     pickerInput(inputId = "species_check", 
                 label = "Species:", 
                 choices = levels(aoc_new$species_new),
                 selected = levels(aoc_new$species_new),
                 options = list(`actions-box` = TRUE),
                 multiple = TRUE)})
   

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
      mutate(x2M = max.size.ingest.mm * 1000) %>% #max size ingest in um
      mutate(CF_bio = CFfnx(x1M = x1M_set, x2M = x2M, x1D = x1D_set, x2D = x2D_set, a = alpha)) %>%  #calculate CF_bio
      mutate(EC_env_p.particles.mL = dose.particles.mL.master * CF_bio) %>%  #aligned particle effect concentraiton (1-5000 um)
      ## Surface area ERM
      mutate(mu.sa.mono = particle.surface.area.um2) %>% #define mu_x_mono for alignment to TRM
      mutate(x_LL_sa = SAfnx(a = x1D_set/2, b = x1D_set/2, c = (2/3)*(x1D_set/2))) %>%  #calculate lower ingestible surface area
      mutate(x_UL_sa = SAfnx(a = x2M/2, b = x2M/2, c = (2/3)*(x2M/2))) %>%  #calculate upper ingestible surface area
      mutate(mu.sa.poly = if(a.sa == 2){mux.polyfnx.2(x_UL_sa, x_LL_sa)} else if (a.sa != 2){mux.polyfnx(a.sa, x_UL_sa, x_LL_sa)}) %>% #calculate mu_x_poly for surface area
      mutate(EC_poly_sa.particles.mL = (EC_env_p.particles.mL * mu.sa.mono)/mu.sa.poly) %>%  #calculate polydisperse effect concentration for surface area (particles/mL)
      ## volume ERM
      mutate(mu.v.mono = particle.volume.um3) %>% #define mu_x_mono for alignment to TRM
      mutate(x_LL_v = volumefnx(R = R.ave, L = x1D_set)) %>%  #calculate lower ingestible volume 
      mutate(x_UL_v = volumefnx(R = R.ave, L = x2M)) %>%  #calculate maximum ingestible volume 
      mutate(mu.v.poly = if(a.v == 2){mux.polyfnx.2(x_UL_v, x_LL_v)} else if (a.v != 2){mux.polyfnx(a.v, x_UL_v, x_LL_v)}) %>% #calculate mu_x_poly for volume
      mutate(EC_poly_v.particles.mL = (EC_env_p.particles.mL * mu.v.mono)/mu.v.poly) %>%  #calculate polydisperse effect concentration for volume (particles/mL)
      ## mass ERM
      mutate(mu.m.mono = mass.per.particle.mg * 1000) %>% #define mu_x_mono for alignment to TRM (ug)
      mutate(x_LL_m = massfnx(R = R.ave, L = x1D_set, p = p.ave)) %>%  #calculate lower ingestible mass
      mutate(x_UL_m = massfnx(R = R.ave, L = x2M, p = p.ave)) %>%  #calculate upper ingestible mass
      mutate(mu.m.poly = if(a.m == 2){mux.polyfnx.2(x_UL_m, x_LL_m)} else if (a.m != 2){mux.polyfnx(a.m, x_UL_m, x_LL_m)}) %>% #calculate mu_x_poly for mass
      mutate(EC_poly_m.particles.mL = (EC_env_p.particles.mL * mu.m.mono)/mu.m.poly) %>%  #calculate polydisperse effect concentration for volume (particles/mL)
      ## specific surface area ERM
      mutate(mu.ssa.mono = mu.sa.mono/mu.m.mono) %>% #define mu_x_mono for alignment to TRM (um^2/ug)
      mutate(x_LL_ssa = SSAfnx(a = x1D_set/2, b = x1D_set/2, c = (2/3)*(x1D_set/2), m = x_LL_m)) %>%  #calculate lower ingestible SSA
      mutate(x_UL_ssa = SSAfnx(a = x2M/2, b = x2M/2, c = (2/3)*(x2M/2), m = x_UL_m)) %>%  #calculate upper ingestible SSA  (um^2/ug)
      mutate(mu.ssa.poly = if(a.ssa == 2){mux.polyfnx.2(x_UL_ssa, x_LL_ssa)} else if (a.ssa != 2){mux.polyfnx(a.ssa, x_UL_ssa, x_LL_ssa)}) %>% #calculate mu_x_poly for specific surface area
      mutate(EC_poly_ssa.particles.mL = (EC_env_p.particles.mL * mu.ssa.mono)/mu.ssa.poly)  #calculate polydisperse effect concentration for specific surface area (particles/mL)
    
    #filter out reported, calcualted, or all based on checkbox and make new variable based on mg/L or particles/mL
    if(Rep_Con_rad == "reported" & dose_check == "mg/L"){
      aoc_setup <- aoc_setup %>% 
        filter(dose.mg.L.master.converted.reported == "reported") %>% 
        mutate(dose_new = dose.mg.L.master)}
    
    if(Rep_Con_rad == "converted" & dose_check == "mg/L"){
      aoc_setup <- aoc_setup %>%
        filter(dose.mg.L.master.converted.reported == "converted") %>% 
        mutate(dose_new = dose.mg.L.master)}
    
    if(Rep_Con_rad == "all" & dose_check == "mg/L"){
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
        mutate(dose_new =EC_poly_sa.particles.mL)}
    
    if(Rep_Con_rad == "converted" & dose_check == "Particles/mL" & ERM_check == "Surface Area"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_poly_sa.particles.mL)}
    
    if(Rep_Con_rad == "all" & dose_check == "Particles/mL" & ERM_check == "Surface Area"){
      aoc_setup <- aoc_setup %>%
        mutate(dose_new = EC_poly_sa.particles.mL)}
    
    #repeat for particles with ERM = Volume
    if(Rep_Con_rad == "reported" & dose_check == "Particles/mL" & ERM_check == "Volume"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_poly_v.particles.mL)}
    
    if(Rep_Con_rad == "converted" & dose_check == "Particles/mL" & ERM_check == "Volume"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_poly_v.particles.mL)}
    
    if(Rep_Con_rad == "all" & dose_check == "Particles/mL" & ERM_check == "Volume"){
      aoc_setup <- aoc_setup %>%
        mutate(dose_new = EC_poly_v.particles.mL)}
    
    #repeat for particles with ERM = Mass
    if(Rep_Con_rad == "reported" & dose_check == "Particles/mL" & ERM_check == "Mass"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_poly_m.particles.mL)}
    
    if(Rep_Con_rad == "converted" & dose_check == "Particles/mL" & ERM_check == "Mass"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_poly_m.particles.mL)}
    
    if(Rep_Con_rad == "all" & dose_check == "Particles/mL" & ERM_check == "Mass"){
      aoc_setup <- aoc_setup %>%
        mutate(dose_new = EC_poly_m.particles.mL)}
    
    #repeat for particles with ERM = Specific Surface Area
    if(Rep_Con_rad == "reported" & dose_check == "Particles/mL" & ERM_check == "Specific Surface Area"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_poly_ssa.particles.mL)}
    
    if(Rep_Con_rad == "converted" & dose_check == "Particles/mL" & ERM_check == "Specific Surface Area"){
      aoc_setup <- aoc_setup %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_poly_ssa.particles.mL)}
    
    if(Rep_Con_rad == "all" & dose_check == "Particles/mL" & ERM_check == "Specific Surface Area"){
      aoc_setup <- aoc_setup %>%
        mutate(dose_new = EC_poly_ssa.particles.mL)}
  
    # new dataset based on filtering
    aoc_setup %>% # take original dataset
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
  
  output$organism_plot_react <- renderPlot({
    
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
      plot.type + #adds user-defined geom()
      #scale_x_log10() +
      coord_trans(x = "log10") +
      scale_x_continuous(breaks = scales::trans_breaks("log10", function(x) 10^x, n = 10),
                         labels = trans_format("log10", scales::math_format(10^.x))) +
      color.type +
      fill.type +
      # scale_color_manual(values = c("#FD8D3C", "#7F2704")) +
      # scale_fill_manual(values = c("#FD8D3C", "#7F2704")) +
      geom_label_repel(data = aoc_org1,
                      aes(label = paste("(",measurements,",",studies,")")),
                      hjust = 0,
                      direction = "y", 
                      nudge_x = 10^100,
                      segment.colour = NA, size = 3.5, show.legend = FALSE) +
      theme.type +
      #theme_classic() +
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
    print(p)
   
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
      plot.type + #adds user-defined geom()
     # scale_x_log10() +
      coord_trans(x = "log10") +
      scale_x_continuous(breaks = scales::trans_breaks("log10", function(x) 10^x, n = 10),
                         labels = trans_format("log10", scales::math_format(10^.x))) +
      fill.type + #user fill 
      color.type + #user color
      #scale_color_manual(values = c("#A1CAF6", "#4C6FA1")) +
      #scale_fill_manual(values = c("#A1CAF6", "#4C6FA1")) +
      geom_label_repel(data = aoc_size1,
                       aes(label = paste("(",measurements,",",studies,")")),
                       hjust = 0,
                       direction = "y", 
                       nudge_x = 10^100,
                       segment.colour = NA, size = 3.5, show.legend = FALSE) +
      theme.type + #user theme
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
      #scale_x_log10() +
      coord_trans(x = "log10") +
      scale_x_continuous(breaks = scales::trans_breaks("log10", function(x) 10^x, n = 10),
                         labels = trans_format("log10", scales::math_format(10^.x))) +
      plot.type + #adds user-defined geom()
      fill.type + #user color
      color.type + #user color
      # scale_color_manual(values = c("#C7EAE5","#35978F")) +
      #scale_fill_manual(values = c("#C7EAE5", "#35978F")) +
      geom_label_repel(data = aoc_shape1,
                       aes(label = paste("(",measurements,",",studies,")")),
                       hjust = 0,
                       direction = "y", 
                       nudge_x = 10^100,
                       segment.colour = NA, size = 3.5, show.legend = FALSE) +
      theme.type + #user theme
      #theme_classic() +
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
      #scale_x_log10() +
      coord_trans(x = "log10") +
      scale_x_continuous(breaks = scales::trans_breaks("log10", function(x) 10^x, n = 10),
                         labels = trans_format("log10", scales::math_format(10^.x))) +
      plot.type + #adds user-defined geom()
      color.type +
      fill.type +
      #scale_color_manual(values = c("#FAB455", "#A5683C")) +
      #scale_fill_manual(values = c("#FAB455", "#A5683C")) +
      geom_label_repel(data = aoc_poly1,
                       aes(label = paste("(",measurements,",",studies,")")),
                       hjust = 0,
                       direction = "y", 
                       nudge_x = 10^100,
                       segment.colour = NA, size = 3.5, show.legend = FALSE) +
     theme.type +
      # theme_classic() +
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
      #scale_x_log10() +
      coord_trans(x = "log10") +
      scale_x_continuous(breaks = scales::trans_breaks("log10", function(x) 10^x, n = 10),
                         labels = trans_format("log10", scales::math_format(10^.x))) +
      plot.type + #adds user-defined geom()
      color.type +
      fill.type +
      # scale_color_manual(values = c("#A99CD9", "#6C568C")) +
      # scale_fill_manual(values = c("#A99CD9", "#6C568C")) +
      geom_label_repel(data = aoc_lvl1_1,
                       aes(label = paste("(",measurements,",",studies,")")),
                       hjust = 0,
                       direction = "y", 
                       nudge_x = 10^100,
                       segment.colour = NA, size = 3.5, show.legend = FALSE) +
      theme.type +
      #theme_classic() +
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
     # scale_x_log10() +
    coord_trans(x = "log10") +
    scale_x_continuous(breaks = scales::trans_breaks("log10", function(x) 10^x, n = 10),
                       labels = trans_format("log10", scales::math_format(10^.x))) +
      plot.type + #adds user-defined geom()
    color.type +
    fill.type +
      # scale_color_manual(values = c("#A99CD9", "#6C568C")) +
      # scale_fill_manual(values = c("#A99CD9", "#6C568C")) +
      geom_label_repel(data = aoc_lvl2_1,
                       aes(label = paste("(",measurements,",",studies,")")),
                       hjust = 0,
                       direction = "y", 
                       nudge_x = 10^100,
                       segment.colour = NA, size = 3.5, show.legend = FALSE) +
      # theme_classic() +
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
  
  # #This is based on a slider widget input
  # width = function() {
  #   input$cols * 300
  # }, height = function() {
  #   input$rows * 400
  # }
  
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

#### SSD AO S ####

  #Create dependent dropdown checklists: select Group by environment
  output$GroupSelection <- renderUI({
    
    #Assign user inputs to variables for this reactive
    env_c_ssd <- input$env_check_ssd #assign environments
    
    #filter based on user input
    aoc_new <- aoc_z %>% # take original dataset
      filter(env_f %in% env_c_ssd) %>% #filter by environment inputs
      mutate(Group_new = factor(as.character(Group))) # new subset of factors
    
    pickerInput(inputId = "Group_check_ssd", 
                label = "Group:", 
                choices = levels(aoc_new$Group_new),
                selected = levels(aoc_new$Group_new),
                options = list(`actions-box` = TRUE),
                multiple = TRUE)})
  
  #Create dependent dropdown checklists: select Species by env and group
  output$SpeciesSelection <- renderUI({
    
    #Assign user inputs to variables for this reactive
    env_c_ssd <- input$env_check_ssd #assign environments
    Group_c_ssd <- input$Group_check_ssd # assign organism input values to "org_c"
    
    #filter based on user input
    aoc_new <- aoc_z %>% # take original dataset
      filter(env_f %in% env_c_ssd) %>% #filter by environment inputs
      filter(Group %in% Group_c_ssd) %>% # filter by organism inputs
      mutate(Species_new = factor(as.character(Species))) # new subset of factors
    
    pickerInput(inputId = "Species_check_ssd", 
                label = "Species:", 
                choices = levels(aoc_new$Species_new),
                selected = levels(aoc_new$Species_new),
                options = list(`actions-box` = TRUE),
                multiple = TRUE)})
  
  #Create dependent dropdown checklists: select biological organization by env, group, and species
  output$BiologicalSelection <- renderUI({
    
    #Assign user inputs to variables for this reactive
    env_c_ssd <- input$env_check_ssd #assign environments
    Group_c_ssd <- input$Group_check_ssd # assign organism input values to "org_c"
    Species_c_ssd <- input$Species_check_ssd #species select
    
    #filter based on user input
    aoc_new <- aoc_z %>% # take original dataset
      filter(env_f %in% env_c_ssd) %>% #filter by environment inputs
      filter(Group %in% Group_c_ssd) %>% # filter by organism inputs
      filter(Species %in% Species_c_ssd) %>% # filter by organism inputs
      mutate(bio_f_new = factor(as.character(bio_f))) # new subset of factors
    
    pickerInput(inputId = "bio_check_ssd", 
                label = "Biological Organization:", 
                choices = levels(aoc_new$bio_f_new),
                selected = levels(aoc_new$bio_f_new),
                options = list(`actions-box` = TRUE),
                multiple = TRUE)})
  
  
  #Create dependent dropdown checklists: select shape by above input
  output$shapeSelection <- renderUI({
    #Assign user inputs to variables for this reactive
    env_c_ssd <- input$env_check_ssd #assign environments
    Group_c_ssd <- input$Group_check_ssd # assign organism input values to "org_c"
    Species_c_ssd <- input$Species_check_ssd #assign species input
    size_c_ssd <- input$size_check_ssd #assign sizes input
    poly_c_ssd <- input$poly_check_ssd #assign polymer input
    bio_c_ssd <- input$bio_check_ssd #assign bio org input
    tech_tier_zero_c_ssd<-input$tech_tier_zero_check_ssd #assign values to "design_tier_zero_c"
    risk_tier_zero_c_ssd<-input$risk_tier_zero_check_ssd #assign values to "risk_tier_zero_c"
    
    #filter based on user input
    aoc_new <- aoc_z %>% # take original dataset
      filter(env_f %in% env_c_ssd) %>% #filter by environment inputs
      filter(Group %in% Group_c_ssd) %>% # filter by organism inputs
      filter(Species %in% Species_c_ssd) %>% #filter by species inputs
      filter(size_f %in% size_c_ssd) %>% #filter by size inputs
      filter(poly_f %in% poly_c_ssd) %>%  # filter polymers from other checkbox
      filter(bio_f %in% bio_c_ssd) %>%  # filter bio org from other checkbox
      filter(tier_zero_tech_f %in% tech_tier_zero_c_ssd) %>% #technical quality
      filter(tier_zero_risk_f %in% risk_tier_zero_c_ssd) %>%  #risk assessment quality
    mutate(shape_f_new = factor(as.character(shape_f))) # new subset of factors
    
    #populate picker choices based on available factors
    pickerInput(inputId = "shape_check_ssd", 
                label = "Shapes:", 
                choices = levels(aoc_new$shape_f_new),
                selected = levels(aoc_new$shape_f_new),
                options = list(`actions-box` = TRUE),
                multiple = TRUE)})
  
  
  #Create dependent dropdown checklists: select lvl1 by above input
  output$lvl1Selection <- renderUI({
    #Assign user inputs to variables for this reactive
    env_c_ssd <- input$env_check_ssd #assign environments
    Group_c_ssd <- input$Group_check_ssd # assign organism input values to "org_c"
    Species_c_ssd <- input$Species_check_ssd #assign species input
    size_c_ssd <- input$size_check_ssd #assign sizes input
    bio_c_ssd <- input$bio_check_ssd #assign bio org input
    tech_tier_zero_c_ssd<-input$tech_tier_zero_check_ssd #assign values to "design_tier_zero_c"
    risk_tier_zero_c_ssd<-input$risk_tier_zero_check_ssd #assign values to "risk_tier_zero_c"
    
    #filter based on user input
    aoc_new <- aoc_z %>% # take original dataset
      filter(env_f %in% env_c_ssd) %>% #filter by environment inputs
      filter(Group %in% Group_c_ssd) %>% # filter by organism inputs
      filter(Species %in% Species_c_ssd) %>% #filter by species inputs
      filter(size_f %in% size_c_ssd) %>% #filter by size inputs
      filter(bio_f %in% bio_c_ssd) %>%  # filter bio org from other checkbox
      filter(tier_zero_tech_f %in% tech_tier_zero_c_ssd) %>% #technical quality
      filter(tier_zero_risk_f %in% risk_tier_zero_c_ssd) %>%  #risk assessment quality
      mutate(lvl1_f_new = factor(as.character(lvl1_f))) # new subset of factors
    #populate picker choices based on available factors
    pickerInput(inputId = "lvl1_check_ssd", 
                label = "Broad Endpoint:", 
                choices = levels(aoc_new$lvl1_f_new),
                selected = levels(aoc_new$lvl1_f_new),
                options = list(`actions-box` = TRUE),
                multiple = TRUE)})
  
  #Create dependent dropdown checklists: select lvl2 by lvl1 input and Species
  output$lvl2Selection <- renderUI({
    #Assign user inputs to variables for this reactive
    lvl1_c_ssd <- input$lvl1_check_ssd #assign endpoints
    Species_c_ssd <- input$Species_check_ssd #assign species input
    size_c_ssd <- input$size_check_ssd #assign sizes input
    bio_c_ssd <- input$bio_check_ssd #assign bio org input
    tech_tier_zero_c_ssd<-input$tech_tier_zero_check_ssd #assign values to "design_tier_zero_c"
    risk_tier_zero_c_ssd<-input$risk_tier_zero_check_ssd #assign values to "risk_tier_zero_c"
    
    #filter based on user input
    aoc_new <- aoc_z %>% # take original dataset
      filter(Species %in% Species_c_ssd) %>% #filter by species inputs
      filter(lvl1_f %in% lvl1_c_ssd) %>% # filter by level inputs
      filter(size_f %in% size_c_ssd) %>% #filter by size inputs
      filter(bio_f %in% bio_c_ssd) %>%  # filter bio org from other checkbox
      filter(tier_zero_tech_f %in% tech_tier_zero_c_ssd) %>% #technical quality
      filter(tier_zero_risk_f %in% risk_tier_zero_c_ssd) %>%  #risk assessment quality
      mutate(lvl2_f_new = factor(as.character(lvl2_f))) # new subset of factors
    #populate picker choices based on available factors
    pickerInput(inputId = "lvl2_check_ssd", 
                label = "Specific Endpoint within Broad Category:", 
                choices = levels(aoc_new$lvl2_f_new),
                selected = levels(aoc_new$lvl2_f_new),
                options = list(`actions-box` = TRUE),
                multiple = TRUE)})
 
  #Create dependent dropdown checklists: select lvl2 by all other input
  output$polySelection <- renderUI({
    #Assign user inputs to variables for this reactive
    env_c_ssd <- input$env_check_ssd #assign environments
    Group_c_ssd <- input$Group_check_ssd # assign organism input values to "org_c"
    Species_c_ssd <- input$Species_check_ssd #assign species input
    size_c_ssd <- input$size_check_ssd #assign sizes input
    bio_c_ssd <- input$bio_check_ssd #assign bio org input
    lvl2_c_ssd <- input$lvl2_check_ssd #assign endpoints
    tech_tier_zero_c_ssd<-input$tech_tier_zero_check_ssd #assign values to "design_tier_zero_c"
    risk_tier_zero_c_ssd<-input$risk_tier_zero_check_ssd #assign values to "risk_tier_zero_c"
    
    #filter based on user input
    aoc_new <- aoc_z %>% # take original dataset
      filter(env_f %in% env_c_ssd) %>% #filter by environment inputs
      filter(Group %in% Group_c_ssd) %>% # filter by organism inputs
      filter(Species %in% Species_c_ssd) %>% #filter by species inputs
      filter(size_f %in% size_c_ssd) %>% #filter by size inputs
      filter(bio_f %in% bio_c_ssd) %>%  # filter bio org from other checkbox
      filter(lvl2_f %in% lvl2_c_ssd) %>%  #filter by second level endpoints
      filter(tier_zero_tech_f %in% tech_tier_zero_c_ssd) %>% #technical quality
      filter(tier_zero_risk_f %in% risk_tier_zero_c_ssd) %>%  #risk assessment quality
      mutate(poly_f_new = factor(as.character(poly_f))) # new subset of factors
    #populate picker choices based on available factors
    pickerInput(inputId = "poly_check_ssd", 
                label = "Polymers:", 
                choices = levels(aoc_new$poly_f_new),
                selected = levels(aoc_new$poly_f_new),
                options = list(`actions-box` = TRUE),
                multiple = TRUE)}) 

    
  # Create new all tested dataset based on widget filtering and adjusted to reflect the presence of the "update" button.
  aoc_z_L <- eventReactive(list(input$SSDgo),{
    # eventReactive explicitly delays activity until you press the button
    # here we'll use the inputs to create a new dataset that will be fed into the renderPlot calls below
    env_c_ssd <- input$env_check_ssd #assign environments
    Group_c_ssd <- input$Group_check_ssd # assign organism input values to "org_c"
    bio_c_ssd <- input$bio_check_ssd #level of biological organization
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
    Reported_Converted_rad <- input$Reported_Converted_rad #use nominal or calculated exposure concentrations. Options are TRUE (calculated) or FALSE (reported)
    ERM_check_ssd <- input$ERM_check_ssd #ERM
    particle_mass_check_ssd <- input$particle_mass_check_ssd #rename variable
    tech_tier_zero_c_ssd<-input$tech_tier_zero_check_ssd #assign values to "design_tier_zero_c"
    risk_tier_zero_c_ssd<-input$risk_tier_zero_check_ssd #assign values to "risk_tier_zero_c"
    
    ## ERM parametrization ##
    # Define params for correction #
    alpha = 2.07#input$alpha_ssd #table s4 for marine surface water. length
    x2D_set = input$upper_length_ssd #upper size range (default)
    x1D_set = input$lower_length_ssd #lower size range (default)
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
    aoc_z <- aoc_z %>% 
      mutate(x2M = max.size.ingest.mm * 1000) %>% #max size ingest in um
      mutate(CF_bio = CFfnx(x1M = x1M_set, x2M = x2M, x1D = x1D_set, x2D = x2D_set, a = alpha)) %>%  #calculate CF_bio
      mutate(EC_env_p.particles.mL = dose.particles.mL.master * CF_bio) %>%  #aligned particle effect concentraiton (1-5000 um)
      ## Surface area ERM
      mutate(mu.sa.mono = particle.surface.area.um2) %>% #define mu_x_mono for alignment to TRM
      mutate(x_LL_sa = SAfnx(a = x1D_set/2, b = x1D_set/2, c = (2/3)*(x1D_set/2))) %>%  #calculate lower ingestible surface area
      mutate(x_UL_sa = SAfnx(a = x2M/2, b = x2M/2, c = (2/3)*(x2M/2))) %>%  #calculate upper ingestible surface area
      mutate(mu.sa.poly = if(a.sa == 2){mux.polyfnx.2(x_UL_sa, x_LL_sa)} else if (a.sa != 2){mux.polyfnx(a.sa, x_UL_sa, x_LL_sa)}) %>% #calculate mu_x_poly for surface area
      mutate(EC_poly_sa.particles.mL = (EC_env_p.particles.mL * mu.sa.mono)/mu.sa.poly) %>%  #calculate polydisperse effect concentration for surface area (particles/mL)
      ## volume ERM
      mutate(mu.v.mono = particle.volume.um3) %>% #define mu_x_mono for alignment to TRM
      mutate(x_LL_v = volumefnx(R = R.ave, L = x1D_set)) %>%  #calculate lower ingestible volume 
      mutate(x_UL_v = volumefnx(R = R.ave, L = x2M)) %>%  #calculate maximum ingestible volume 
      mutate(mu.v.poly = if(a.v == 2){mux.polyfnx.2(x_UL_v, x_LL_v)} else if (a.v != 2){mux.polyfnx(a.v, x_UL_v, x_LL_v)}) %>% #calculate mu_x_poly for volume
      mutate(EC_poly_v.particles.mL = (EC_env_p.particles.mL * mu.v.mono)/mu.v.poly) %>%  #calculate polydisperse effect concentration for volume (particles/mL)
      ## mass ERM
      mutate(mu.m.mono = mass.per.particle.mg * 1000) %>% #define mu_x_mono for alignment to TRM (ug)
      mutate(x_LL_m = massfnx(R = R.ave, L = x1D_set, p = p.ave)) %>%  #calculate lower ingestible mass
      mutate(x_UL_m = massfnx(R = R.ave, L = x2M, p = p.ave)) %>%  #calculate upper ingestible mass
      mutate(mu.m.poly = if(a.m == 2){mux.polyfnx.2(x_UL_m, x_LL_m)} else if (a.m != 2){mux.polyfnx(a.m, x_UL_m, x_LL_m)}) %>% #calculate mu_x_poly for mass
      mutate(EC_poly_m.particles.mL = (EC_env_p.particles.mL * mu.m.mono)/mu.m.poly) %>%  #calculate polydisperse effect concentration for volume (particles/mL)
      ## specific surface area ERM
      mutate(mu.ssa.mono = mu.sa.mono/mu.m.mono) %>% #define mu_x_mono for alignment to TRM (um^2/ug)
      mutate(x_LL_ssa = SSAfnx(a = x1D_set/2, b = x1D_set/2, c = (2/3)*(x1D_set/2), m = x_LL_m)) %>%  #calculate lower ingestible SSA
      mutate(x_UL_ssa = SSAfnx(a = x2M/2, b = x2M/2, c = (2/3)*(x2M/2), m = x_UL_m)) %>%  #calculate upper ingestible SSA  (um^2/ug)
      mutate(mu.ssa.poly = if(a.ssa == 2){mux.polyfnx.2(x_UL_ssa, x_LL_ssa)} else if (a.ssa != 2){mux.polyfnx(a.ssa, x_UL_ssa, x_LL_ssa)}) %>% #calculate mu_x_poly for specific surface area
      mutate(EC_poly_ssa.particles.mL = (EC_env_p.particles.mL * mu.ssa.mono)/mu.ssa.poly)  #calculate polydisperse effect concentration for specific surface area (particles/mL)
    
    #filter out reported, calcualted, or all based on checkbox and make new variable based on mg/L or particles/mL
    if(Reported_Converted_rad == "reported" & particle_mass_check_ssd == "mg/L"){
      aoc_z <- aoc_z %>% 
        filter(dose.mg.L.master.converted.reported == "reported") %>% 
        mutate(dose_new = dose.mg.L.master)
    } 
    if(Reported_Converted_rad == "converted" & particle_mass_check_ssd == "mg/L"){
      aoc_z <- aoc_z %>% 
        filter(dose.mg.L.master.converted.reported == "converted") %>% 
        mutate(dose_new = dose.mg.L.master)
    } 
    if(Reported_Converted_rad == "all" & particle_mass_check_ssd == "mg/L"){
      aoc_z <- aoc_z %>% 
        mutate(dose_new = dose.mg.L.master)
      
      #repeat for particles (un-aligned)
    }
    if(Reported_Converted_rad == "reported" & particle_mass_check_ssd == "Particles/mL" & ERM_check_ssd ==  "Unaligned"){
      aoc_z <- aoc_z %>% 
        filter(dose.particles.mL.master.converted.reported == "reported") %>% 
        mutate(dose_new = dose.particles.mL.master)
    } 
    if(Reported_Converted_rad == "converted" & particle_mass_check_ssd == "Particles/mL" & ERM_check_ssd ==  "Unaligned"){
      aoc_z <- aoc_z %>% 
        filter(dose.particles.mL.master.converted.reported == "converted") %>% 
        mutate(dose_new = dose.particles.mL.master)
    } 
    if(Reported_Converted_rad == "all" & particle_mass_check_ssd == "Particles/mL" & ERM_check_ssd ==  "Unaligned"){
      aoc_z <- aoc_z %>% 
        mutate(dose_new = dose.particles.mL.master)
    }
    #repeat for volume
    if(Reported_Converted_rad == "reported" & particle_mass_check_ssd == "um3/mL"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = dose.um3.mL.master)}
    
    if(Reported_Converted_rad == "converted" & particle_mass_check_ssd == "um3/mL"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = dose.um3.mL.master)}
    
    if(Reported_Converted_rad == "all" & particle_mass_check_ssd == "um3/mL"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = dose.um3.mL.master)}
    
    ## ERM ## NOTE: ERM doses reported ONLY in particles/mL
    #repeat for particles with ERM = Particles
    if(Reported_Converted_rad == "reported" & particle_mass_check_ssd == "Particles/mL" & ERM_check_ssd == "Particles"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_p.particles.mL)}
    
    if(Reported_Converted_rad == "converted" & particle_mass_check_ssd == "Particles/mL" & ERM_check_ssd == "Particles"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_p.particles.mL)}
    
    if(Reported_Converted_rad == "all" & particle_mass_check_ssd == "Particles/mL" & ERM_check_ssd == "Particles"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_p.particles.mL)}
    
    #repeat for particles with ERM = Surface Area
    if(Reported_Converted_rad == "reported" & particle_mass_check_ssd == "Particles/mL" & ERM_check_ssd == "Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_poly_sa.particles.mL)}
    
    if(Reported_Converted_rad == "converted" & particle_mass_check_ssd == "Particles/mL" & ERM_check_ssd == "Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_poly_sa.particles.mL)}
    
    if(Reported_Converted_rad == "all" & particle_mass_check_ssd == "Particles/mL" & ERM_check_ssd == "Surface Area"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_poly_sa.particles.mL)}
    
    #repeat for particles with ERM = Volume
    if(Reported_Converted_rad == "reported" & particle_mass_check_ssd == "Particles/mL" & ERM_check_ssd == "Volume"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_poly_v.particles.mL)}
    
    if(Reported_Converted_rad == "converted" & particle_mass_check_ssd == "Particles/mL" & ERM_check_ssd == "Volume"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_poly_v.particles.mL)}
    
    if(Reported_Converted_rad == "all" & particle_mass_check_ssd == "Particles/mL" & ERM_check_ssd == "Volume"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_poly_v.particles.mL)}
    
    #repeat for particles with ERM = Mass
    if(Reported_Converted_rad == "reported" & particle_mass_check_ssd == "Particles/mL" & ERM_check_ssd == "Mass"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_poly_m.particles.mL)}
    
    if(Reported_Converted_rad == "converted" & particle_mass_check_ssd == "Particles/mL" & ERM_check_ssd == "Mass"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_poly_m.particles.mL)}
    
    if(Reported_Converted_rad == "all" & particle_mass_check_ssd == "Particles/mL" & ERM_check_ssd == "Mass"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_poly_m.particles.mL)}
    
    #repeat for particles with ERM = Specific Surface Area
    if(Reported_Converted_rad == "reported" & particle_mass_check_ssd == "Particles/mL" & ERM_check_ssd == "Specific Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_poly_ssa.particles.mL)}
    
    if(Reported_Converted_rad == "converted" & particle_mass_check_ssd == "Particles/mL" & ERM_check_ssd == "Specific Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_poly_ssa.particles.mL)}
    
    if(Reported_Converted_rad == "all" & particle_mass_check_ssd == "Particles/mL" & ERM_check_ssd == "Specific Surface Area"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_poly_ssa.particles.mL)}
    
    #left-hand table of all data considered
    aoc_z %>% # take original dataset
      mutate(dose_new = case_when(AF.time_r_ssd == "Yes" & AF.noec_r_ssd == "Yes" ~ dose_new / (af.time * af.noec), #composite assessment factors
                                  AF.time_r_ssd == "Yes" & AF.noec_r_ssd == "No" ~ dose_new / af.time,
                                  AF.time_r_ssd == "No" & AF.noec_r_ssd == "Yes" ~ dose_new / af.noec,
                                  AF.time_r_ssd == "No" & AF.noec_r_ssd == "No" ~ dose_new)) %>% # adjust for assessment factors based on user input
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
   
    env_c_ssd <- input$env_check_ssd #assign environments
    Group_c_ssd <- input$Group_check_ssd # assign organism input values to "org_c"
    bio_c_ssd <- input$bio_check_ssd #level of biological organization
    Species_c_ssd <- input$Species_check_ssd #assign species input
    size_c_ssd <- input$size_check_ssd #assign sizes input
    lvl1_c_ssd <- input$lvl1_check_ssd #assign general endpoints
    lvl2_c_ssd <- input$lvl2_check_ssd #assign specific endpoints
    poly_c_ssd <- input$poly_check_ssd #assign polymers
    shape_c_ssd <- input$shape_check_ssd #assign shapes
    bio_c_ssd <- input$bio_check_ssd #assign bio org input
    effect_metric_rad <- input$effect.metric_rad_ssd #effect metric filtering
    AF.time_r_ssd <- input$AF.time_rad_ssd #yes/no apply assessment factor for acute -> chronic
    AF.noec_r_ssd <- input$AF.noec_rad_ssd #yes/no apply assessment factor for LOEC/ECXX -> NOEC
    Reported_Converted_rad <- input$Reported_Converted_rad #use nominal or calculated exposure concentrations. Options are TRUE (calculated) or FALSE (reported)
    ERM_check_ssd <- input$ERM_check_ssd #ERM
    particle_mass_check_ssd <- input$particle_mass_check_ssd #rename variable
    acute.chronic.c_ssd <- input$acute.chronic_check_ssd #acute chronic checkbox
    conc.select.r <- input$conc.select.rad #concentration selector ("minimum", "lower 95% CI", "1st Quartile", "median", "mean", "3rd Quartile", "upper 95% CI", "maximum")
    tech_tier_zero_c_ssd<-input$tech_tier_zero_check_ssd #assign values to "design_tier_zero_c"
    risk_tier_zero_c_ssd<-input$risk_tier_zero_check_ssd #assign values to "risk_tier_zero_c"
    
    ## ERM parametrization ##
    # Define params for correction #
    alpha = 2.07#input$alpha_ssd #table s4 for marine surface water. length
    x2D_set = input$upper_length_ssd #upper size range (default)
    x1D_set = input$lower_length_ssd #lower size range (default)
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
    aoc_z <- aoc_z %>% 
      mutate(x2M = max.size.ingest.mm * 1000) %>% #max size ingest in um
      mutate(CF_bio = CFfnx(x1M = x1M_set, x2M = x2M, x1D = x1D_set, x2D = x2D_set, a = alpha)) %>%  #calculate CF_bio
      mutate(EC_env_p.particles.mL = dose.particles.mL.master * CF_bio) %>%  #aligned particle effect concentraiton (1-5000 um)
      ## Surface area ERM
      mutate(mu.sa.mono = particle.surface.area.um2) %>% #define mu_x_mono for alignment to TRM
      mutate(x_LL_sa = SAfnx(a = x1D_set/2, b = x1D_set/2, c = (2/3)*(x1D_set/2))) %>%  #calculate lower ingestible surface area
      mutate(x_UL_sa = SAfnx(a = x2M/2, b = x2M/2, c = (2/3)*(x2M/2))) %>%  #calculate upper ingestible surface area
      mutate(mu.sa.poly = if(a.sa == 2){mux.polyfnx.2(x_UL_sa, x_LL_sa)} else if (a.sa != 2){mux.polyfnx(a.sa, x_UL_sa, x_LL_sa)}) %>% #calculate mu_x_poly for surface area
      mutate(EC_poly_sa.particles.mL = (EC_env_p.particles.mL * mu.sa.mono)/mu.sa.poly) %>%  #calculate polydisperse effect concentration for surface area (particles/mL)
      ## volume ERM
      mutate(mu.v.mono = particle.volume.um3) %>% #define mu_x_mono for alignment to TRM
      mutate(x_LL_v = volumefnx(R = R.ave, L = x1D_set)) %>%  #calculate lower ingestible volume 
      mutate(x_UL_v = volumefnx(R = R.ave, L = x2M)) %>%  #calculate maximum ingestible volume 
      mutate(mu.v.poly = if(a.v == 2){mux.polyfnx.2(x_UL_v, x_LL_v)} else if (a.v != 2){mux.polyfnx(a.v, x_UL_v, x_LL_v)}) %>% #calculate mu_x_poly for volume
      mutate(EC_poly_v.particles.mL = (EC_env_p.particles.mL * mu.v.mono)/mu.v.poly) %>%  #calculate polydisperse effect concentration for volume (particles/mL)
      ## mass ERM
      mutate(mu.m.mono = mass.per.particle.mg * 1000) %>% #define mu_x_mono for alignment to TRM (ug)
      mutate(x_LL_m = massfnx(R = R.ave, L = x1D_set, p = p.ave)) %>%  #calculate lower ingestible mass
      mutate(x_UL_m = massfnx(R = R.ave, L = x2M, p = p.ave)) %>%  #calculate upper ingestible mass
      mutate(mu.m.poly = if(a.m == 2){mux.polyfnx.2(x_UL_m, x_LL_m)} else if (a.m != 2){mux.polyfnx(a.m, x_UL_m, x_LL_m)}) %>% #calculate mu_x_poly for mass
      mutate(EC_poly_m.particles.mL = (EC_env_p.particles.mL * mu.m.mono)/mu.m.poly) %>%  #calculate polydisperse effect concentration for volume (particles/mL)
      ## specific surface area ERM
      mutate(mu.ssa.mono = mu.sa.mono/mu.m.mono) %>% #define mu_x_mono for alignment to TRM (um^2/ug)
      mutate(x_LL_ssa = SSAfnx(a = x1D_set/2, b = x1D_set/2, c = (2/3)*(x1D_set/2), m = x_LL_m)) %>%  #calculate lower ingestible SSA
      mutate(x_UL_ssa = SSAfnx(a = x2M/2, b = x2M/2, c = (2/3)*(x2M/2), m = x_UL_m)) %>%  #calculate upper ingestible SSA  (um^2/ug)
      mutate(mu.ssa.poly = if(a.ssa == 2){mux.polyfnx.2(x_UL_ssa, x_LL_ssa)} else if (a.ssa != 2){mux.polyfnx(a.ssa, x_UL_ssa, x_LL_ssa)}) %>% #calculate mu_x_poly for specific surface area
      mutate(EC_poly_ssa.particles.mL = (EC_env_p.particles.mL * mu.ssa.mono)/mu.ssa.poly)  #calculate polydisperse effect concentration for specific surface area (particles/mL)
    
    #filter out reported, calcualted, or all based on checkbox and make new variable based on mg/L or particles/mL
    if(Reported_Converted_rad == "reported" & particle_mass_check_ssd == "mg/L"){
      aoc_z <- aoc_z %>% 
        dplyr::filter(dose.mg.L.master.converted.reported == "reported") %>% 
        mutate(dose_new = dose.mg.L.master)
    } 
    if(Reported_Converted_rad == "converted" & particle_mass_check_ssd == "mg/L"){
      aoc_z <- aoc_z %>% 
        dplyr::filter(dose.mg.L.master.converted.reported == "converted") %>% 
        mutate(dose_new = dose.mg.L.master)
    } 
    if(Reported_Converted_rad == "all" & particle_mass_check_ssd == "mg/L"){
      aoc_z <- aoc_z %>% 
        mutate(dose_new = dose.mg.L.master)
    }
    #repeat for particles (un-aligned)
  if(Reported_Converted_rad == "reported" & particle_mass_check_ssd == "Particles/mL" & ERM_check_ssd ==  "Unaligned"){
    aoc_z <- aoc_z %>% 
      filter(dose.particles.mL.master.converted.reported == "reported") %>% 
      mutate(dose_new = dose.particles.mL.master)
  } 
  if(Reported_Converted_rad == "converted" & particle_mass_check_ssd == "Particles/mL" & ERM_check_ssd ==  "Unaligned"){
    aoc_z <- aoc_z %>% 
      filter(dose.particles.mL.master.converted.reported == "converted") %>% 
      mutate(dose_new = dose.particles.mL.master)
  } 
  if(Reported_Converted_rad == "all" & particle_mass_check_ssd == "Particles/mL" & ERM_check_ssd ==  "Unaligned"){
    aoc_z <- aoc_z %>% 
      mutate(dose_new = dose.particles.mL.master)
  }
    #repeat for volume
    if(Reported_Converted_rad == "reported" & particle_mass_check_ssd == "um3/mL"){
      aoc_z <- aoc_z %>%
        dplyr::filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = dose.um3.mL.master)}
    
    if(Reported_Converted_rad == "converted" & particle_mass_check_ssd == "um3/mL"){
      aoc_z <- aoc_z %>%
        dplyr::filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = dose.um3.mL.master)}
    
    if(Reported_Converted_rad == "all" & particle_mass_check_ssd == "um3/mL"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = dose.um3.mL.master)}
    
    ## ERM ## NOTE: ERM doses reported ONLY in particles/mL
    #repeat for particles with ERM = Particles
    if(Reported_Converted_rad == "reported" & particle_mass_check_ssd == "Particles/mL" & ERM_check_ssd == "Particles"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_env_p.particles.mL)}
    
    if(Reported_Converted_rad == "converted" & particle_mass_check_ssd == "Particles/mL" & ERM_check_ssd == "Particles"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_env_p.particles.mL)}
    
    if(Reported_Converted_rad == "all" & particle_mass_check_ssd == "Particles/mL" & ERM_check_ssd == "Particles"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_env_p.particles.mL)}
    
    #repeat for particles with ERM = Surface Area
    if(Reported_Converted_rad == "reported" & particle_mass_check_ssd == "Particles/mL" & ERM_check_ssd == "Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_poly_sa.particles.mL)}
    
    if(Reported_Converted_rad == "converted" & particle_mass_check_ssd == "Particles/mL" & ERM_check_ssd == "Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_poly_sa.particles.mL)}
    
    if(Reported_Converted_rad == "all" & particle_mass_check_ssd == "Particles/mL" & ERM_check_ssd == "Surface Area"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_poly_sa.particles.mL)}
    
    #repeat for particles with ERM = Volume
    if(Reported_Converted_rad == "reported" & particle_mass_check_ssd == "Particles/mL" & ERM_check_ssd == "Volume"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_poly_v.particles.mL)}
    
    if(Reported_Converted_rad == "converted" & particle_mass_check_ssd == "Particles/mL" & ERM_check_ssd == "Volume"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_poly_v.particles.mL)}
    
    if(Reported_Converted_rad == "all" & particle_mass_check_ssd == "Particles/mL" & ERM_check_ssd == "Volume"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_poly_v.particles.mL)}
    
    #repeat for particles with ERM = Mass
    if(Reported_Converted_rad == "reported" & particle_mass_check_ssd == "Particles/mL" & ERM_check_ssd == "Mass"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_poly_m.particles.mL)}
    
    if(Reported_Converted_rad == "converted" & particle_mass_check_ssd == "Particles/mL" & ERM_check_ssd == "Mass"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_poly_m.particles.mL)}
    
    if(Reported_Converted_rad == "all" & particle_mass_check_ssd == "Particles/mL" & ERM_check_ssd == "Mass"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_poly_m.particles.mL)}
    
    #repeat for particles with ERM = Specific Surface Area
    if(Reported_Converted_rad == "reported" & particle_mass_check_ssd == "Particles/mL" & ERM_check_ssd == "Specific Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "reported") %>%
        mutate(dose_new = EC_poly_ssa.particles.mL)}
    
    if(Reported_Converted_rad == "converted" & particle_mass_check_ssd == "Particles/mL" & ERM_check_ssd == "Specific Surface Area"){
      aoc_z <- aoc_z %>%
        filter(dose.particles.mL.master.converted.reported == "converted") %>%
        mutate(dose_new = EC_poly_ssa.particles.mL)}
    
    if(Reported_Converted_rad == "all" & particle_mass_check_ssd == "Particles/mL" & ERM_check_ssd == "Specific Surface Area"){
      aoc_z <- aoc_z %>%
        mutate(dose_new = EC_poly_ssa.particles.mL)}
    
    
    #right-hand table of just effect data
    aoc_ssd <- aoc_z %>% 
      mutate(dose_new = case_when(AF.time_r_ssd == "Yes" & AF.noec_r_ssd == "Yes" ~ dose_new / (af.time * af.noec),
                                  AF.time_r_ssd == "Yes" & AF.noec_r_ssd == "No" ~ dose_new / (af.time),
                                  AF.time_r_ssd == "No" & AF.noec_r_ssd == "Yes" ~ dose_new / (af.noec),
                                  AF.time_r_ssd == "No" & AF.noec_r_ssd == "No" ~ dose_new)) %>% # adjust for assessment factors based on user input
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
      summarise(micConcEffect = min(dose_new), meanConcEffect = mean(dose_new), medianConcEffect = median(dose_new), SDConcEffect = sd(dose_new),MaxConcEffect = max(dose_new), CI95_LCL = meanConcEffect - 1.96 * SDConcEffect/sqrt(n()), firstQuartileConcEffect = quantile(dose_new, 0.25), CI95_UCL = meanConcEffect + 1.96 * SDConcEffect/sqrt(n()), thirdQuartileConcEffect = quantile(dose_new, 0.75), CountEffect = n(), MinEffectType = lvl1[which.min(dose_new)], Minlvl2EffectType = lvl2[which.min(dose_new)], MinEnvironment = environment[which.min(dose_new)], MinDoi = doi[which.min(dose_new)], MinLifeStage = life.stage[which.min(dose_new)], Mininvitro.invivo = invitro.invivo[which.min(dose_new)]) %>%  #set concentration to minimum observed effect
      mutate_if(is.numeric, ~ signif(., 3))
   
    #dynamically change concentrations used based on user input
    ###concentration selector ("minimum", "lower 95% CI", "1st Quartile", "median", "mean", "3rd Quartile", "upper 95% CI", "maximum")###
    if(conc.select.r == "Minimum"){
      aoc_ssd <- aoc_ssd %>% 
        mutate(Conc = micConcEffect)
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
    col_order <- c("Group", "Species", "Conc", "MinEffectType", "Minlvl2EffectType", "MinEnvironment", "MinDoi", "micConcEffect", "CI95_LCL", "firstQuartileConcEffect", "meanConcEffect", "medianConcEffect", "thirdQuartileConcEffect", "CI95_UCL", "MaxConcEffect", "SDConcEffect", "CountEffect", "MinConcTested", "MaxConcTested", "CountTotal")
    #reorder
    aoc_z_join_order <- aoc_z_join[, col_order]
    
    #'print'
    aoc_z_join_order
  })
  
  
  #print summarize filtered data in data table
  output$aoc_filter_ssd_table <- DT::renderDataTable(server = FALSE,{ #server= FALSE prints ALL data, not just what's shown
    particle_mass_check_ssd <- input$particle_mass_check_ssd
    req(input$SSDgo)
    
    datatable(aoc_filter_ssd(),
              extensions = c('Buttons'),
              options = list(
                dom = 'Brtip',
                buttons = list(I('colvis'), c('copy', 'csv', 'excel')),
                autoWidth = TRUE,
                scrollX = TRUE,
                columnDefs = list(list(width = '50px, targets = "_all'))),#only display the table and nothing else
              colnames = c("Group", "Species", paste0("Most Sensitive Concentration ",  particle_mass_check_ssd), "Min Conc. Broad Endpoint", "Min Conc. Specfic Endpoint", "Min Environment", "DOI", "Minimum Effect Concentration", "95% Lower CI Effect Concentration", "1st Quartile Effect Concentration", "Average Effect Concentration", "Median Effect Concentration", "3rd Quartile Effect Concentration", "95% Upper CI Concentration", "Maximum Observed Effect Concentration", "Std Dev Effect Concentration", "Number of doses with Effects", "Min Concentration Tested (with or without effects)", "Max Concentration Tested (with or without effects)", "Total # Doses Considered"),
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
    particle_mass_check_ssd <- input$particle_mass_check_ssd #assign whether or not to use particles/mL or mass/mL
  
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
     xlab = particle_mass_check_ssd,
     ci = TRUE, #confidence interval plotting
     ribbon = TRUE,
     hc = pred_c_hc_ssd) + #percent hazard concentration
     scale_fill_viridis_d() + #make colors more differentiable 
     scale_colour_viridis_d() +  #make colors more differentiable 
     expand_limits(x = c(0.000000000001,5000)) + # to ensure the species labels fit
    geom_text(data = aochc, aes(x = est, y = 0, label = paste0(percent, "% Hazard Confidence Level")), color = "red", size = 4) + #label for hazard conc
    geom_text(data = aochc, aes(x = est, y = -0.05, label = est_format), color = "red") + #label for hazard conc
     ggtitle("Species Sensitivity for Microplastics")
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
      grDevices::png(..., width = 10, height = 12, res = 250, units = "in")
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
                       "light" 	= theme_gray(base_size = 20),
                       "dark" = dark_theme_bw(base_size = 20)) 
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

     
     particle_mass_check_ssd <- input$particle_mass_check_ssd #assign whether or not to use particles/mL or mass/mL
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
      xlab(ifelse(input$ERM_check_ssd == "Unaligned", particle_mass_check_ssd,
           paste0(particle_mass_check_ssd, " (",input$lower_length_ssd, " to ",input$upper_length_ssd, " um)"))
           )+
      labs(title = "Microplastics Species Sensitivity Distribution",
             subtitle = paste("(ERM = ",input$ERM_check_ssd,")")) +
      coord_trans(x = "log10") +
      scale_x_continuous(breaks = scales::trans_breaks("log10", function(x) 10^x, n = 15),
                         labels = trans_format("log10", scales::math_format(10^.x))) + #comma_signif)+
      geom_segment(data = aochc,aes(x = est, y = percent/100, xend = est, yend = est), linetype = 'dashed', color = "red", size = 1) + #hazard conc line vertical
      geom_segment(data = aochc,aes(x = lcl, y = percent/100, xend = est, yend = percent/100), linetype = 'dashed', color = "red", size = 1) + #hazard conc line horizontal
      geom_text(data = aochc, aes(x = est, y = 0.15, label = paste0(percent, "% Hazard Confidence Level")), color = "red", size = 5) + #label for hazard conc
      geom_text(data = aochc, aes(x = est, y = 0.10, label = paste0(est_format, " ", particle_mass_check_ssd)), color = "red", size = 5) + #label for hazard conc
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
    particle_mass_check_ssd <- input$particle_mass_check_ssd #assign whether or not to use particles/mL or mass/mL
    
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0(particle_mass_check_ssd, " = ", format(e$x,scientific = TRUE), " percent =", percent(e$y), "\n")
    }
    
    paste0(
      "", xy_str(input$plot_hover)
    )
  })
  
  # SSD Table

  output$ssd_pred_table <- DT::renderDataTable(server = FALSE,{ #server= FALSE prints ALL data, not just what's shown
    particle_mass_check_ssd <- input$particle_mass_check_ssd #assign whether or not to use particles/mL or mass/mL
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
                colnames = c("Percent", paste0("Estimated Mean Concentration ",  particle_mass_check_ssd), paste0("Standard Error ",  particle_mass_check_ssd), "Lower 95% Confidence Limit", "Upper 95% Confidence Limit", "Distribution"),
                caption = "Predicted species sensitivity distribution concentrations with uncertanties."
                )
  })
  
  # Cullen and Frey Graph
  output$ssd_CF_plot <- renderPlot({
    req(aoc_filter_ssd())
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
    req(aoc_filter_ssd())
    
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
    req(aoc_filter_ssd())
    
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
    req(aoc_filter_ssd())
    
    #reactive report x-axis
    particle_mass_check_ssd <- input$particle_mass_check_ssd
    
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
           xlab = paste0("log10 ",particle_mass_check_ssd))
  })

  #### Endpoint Category S ####
  
  aoc_filter_endpoint <- eventReactive(list(input$go_endpoint),{
  
  # biological organization widget
  bio_c_endpoint <- input$bio_check_endpoint # assign bio values to "bio_c"
  
  aoc_endpoint %>% # take original dataset
        filter(bio_f %in% bio_c_endpoint) #filter by bio organization
  
  })
  
  output$plot <- renderCollapsibleTree({
    
    collapsibleTree(aoc_filter_endpoint(), root = "Aquatic Organisms Database", hierarchy = c("lvl1_f", "lvl2_f", "lvl3_f", "bio_f"),
                    fontSize = 16, zoomable = FALSE,    
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
  
  } #Server end

#### Full App ####
shinyApp(ui = ui, server = server)


# End of R Shiny app script.
