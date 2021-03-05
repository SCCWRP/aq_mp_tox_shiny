# Microplastics Toxicity Literature Review
# Southern California Coastal Water Research Project
# Team X Data Analysis
# Created on: January 17, 2021 by Leah Thornton Hampton

#### Packages ####

# Attach packages necessary for the actions below.
library(tidyverse) # For data tidying/viz purposes.
library(patchwork) # For knitting separate figures together.
library(devtools) # For installing packages from github.
library(calecopal) # For palettes used in figures below.
# Install by running devtools::install_github("an-bui/calecopal") in console.
# See available palettes by running names(cal_palettes) in console.
# Display a palette by running cal_palette("sbchannel") in console.

#### Data Import ####

aoc <- read_csv("AquaticOrganisms_Clean_final.csv", guess_max = 10000)

#### Set Up #### 

#This is the exact same setup used in the app as of 3/2/21

aoc_v1 <- aoc %>% # start with original dataset
  # full dataset filters.
  mutate(effect_f = factor(case_when(effect == "Y" ~ "Yes",
                                     effect == "N" ~ "No"),
                           levels = c("No", "Yes"))) %>%
  # removing NAs to make data set nicer
  replace_na(list(size.category = 0, shape = "Not Reported", polymer = "Not Reported", life.stage = "Not Reported"))

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
  mutate(bio_f = factor(case_when(bio.org == "cell"~"Cell", #Bio Org Data Tidying
                                  bio.org == "organism"~"Organism",
                                  bio.org == "population"~ "Population",
                                  bio.org == "subcell"~"Subcell",
                                  bio.org == "tissue" ~ "Tissue")))%>%
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
                                             risk.tier.zero == "Pass" ~ "Red Criteria Passed")))


#### Particle Volume Histogram ####

Volume <- aoc_setup %>% 
  drop_na(particle.volume.um3) %>% 
  ggplot(aes(x = particle.volume.um3)) + 
  geom_histogram(bins = 100, color = "darkmagenta", fill = "darkmagenta")+
  scale_x_log10()+
  theme_minimal()+
  theme(text = element_text(size = 14))+
  labs(title = "Distribution of Particle Volume",
       subtitle = "Aquatic Organisms Database",
       x = "Particle Volume (µm^3)",
       y = "Count",
       caption = "Each data point is a measured endpoint/row in the database.\nNumber of bins = 100.")

plot(Volume)

#### Particle Mass Histogram ####

Mass <- aoc_setup %>%
  drop_na(mass.per.particle.mg) %>% 
  mutate(mass_ug = (mass.per.particle.mg*1000)) %>% 
  ggplot(aes(x = mass_ug)) + 
  geom_histogram(bins = 100, color = "darkcyan", fill = "darkcyan")+
  scale_x_log10()+
  theme_minimal()+
  theme(text = element_text(size = 14))+
  labs(title = "Distribution of Particle Mass",
       subtitle = "Aquatic Organisms Database",
       x = "Particle Mass (µg)",
       y = "Count",
       caption = "Each data point is a measured endpoint/row in the database.\nNumber of bins = 100.")

plot(Mass)

#### Particle Mass Histogram ####

Volume <- aoc_setup %>% 
  ggplot(aes(x = particle.volume.um3)) + 
  geom_histogram(bins = 100, color = "darkmagenta", fill = "darkmagenta")+
  scale_x_log10()+
  theme_minimal()+
  theme(text = element_text(size = 14))+
  labs(title = "Distribution of Particle Volume",
       subtitle = "Aquatic Organisms Database",
       x = "Particle Volume (µm^3)",
       y = "Count",
       caption = "Each data point is a measured endpoint/row in the database.\nNumber of bins = 100.")

plot(Volume)

### Size Vs. Dose - Counts####

Count <- aoc_setup %>%
  group_by(dose.particles.mL.master, size.length.um.used.for.conversions) %>% 
  summarise() %>% 
  ggplot(aes(x = dose.particles.mL.master, y = size.length.um.used.for.conversions)) +
  geom_point(color = "darkcyan", alpha = 0.5) +
  theme_bw()+
  theme(text = element_text(size=14))+
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Particle Size by Dose (Particles/Volume)", x = "Dose (particles/mL)", y = "Size (µm)", color = "Effect")+
  stat_smooth(method = "lm", col = "black")

plot(Count)

### Size Vs. Dose - Mass####

Mass <- aoc_setup %>%
  group_by(dose.mg.L.master, size.length.um.used.for.conversions) %>% 
  summarise() %>% 
  ggplot(aes(x = dose.mg.L.master, y = size.length.um.used.for.conversions)) +
  geom_point(color = "deeppink3", alpha = 0.5) +
  theme_bw()+
  theme(text = element_text(size=14))+
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Particle Size by Dose (Mass/Volume)", x = "Dose (mg/L)", y = "Size (µm)", color = "Effect")+
  stat_smooth(method = "lm", col = "black")

plot(Mass)

### Size Vs. Dose - Counts with Effect####

Count_E <- aoc_setup %>%
  drop_na(effect_f) %>% 
  group_by(dose.particles.mL.master, size.length.um.used.for.conversions, effect_f) %>% 
  summarise() %>% 
  ggplot(aes(x = dose.particles.mL.master, y = size.length.um.used.for.conversions, color = effect_f, fill = effect_f)) +
  geom_point(alpha = 0.5) +
  theme_bw()+
  theme(text = element_text(size=14))+
  scale_color_manual(values = c("darkcyan", "darkblue")) +
  scale_fill_manual(values = c("darkcyan", "darkblue")) +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Particle Size by Dose (Particles/Volume)", x = "Dose (particles/mL)", y = "Size (µm)", color = "Effect", fill = "Effect")+
  stat_smooth(method = "lm")

plot(Count_E)

### Size Vs. Dose - Mass with Effect####

Mass_E <- aoc_setup %>%
  drop_na(effect_f) %>% 
  group_by(dose.mg.L.master, size.length.um.used.for.conversions, effect_f) %>% 
  summarise() %>% 
  ggplot(aes(x = dose.mg.L.master, y = size.length.um.used.for.conversions, color = effect_f)) +
  geom_point(alpha = 0.5) +
  theme_bw()+
  theme(text = element_text(size=14))+
  scale_color_manual(values = c("darkcyan", "darkblue")) +
  scale_fill_manual(values = c("darkcyan", "darkblue")) +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Particle Size by Dose (Mass/Volume)", x = "Dose (mg/L)", y = "Size (µm)", color = "Effect")+
  stat_smooth(method = "lm")

plot(Mass_E)


### Size Vs. Dose - Counts - Reported Only ####

Count_Reported <- aoc_setup %>%
  filter(dose.particles.mL.master.converted.reported != "converted") %>% 
  group_by(dose.particles.mL.master, size.length.um.used.for.conversions) %>% 
  summarise() %>% 
  ggplot(aes(x = dose.particles.mL.master, y = size.length.um.used.for.conversions)) +
  geom_point(color = "darkcyan", alpha = 0.5) +
  theme_bw()+
  theme(text = element_text(size=14))+
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Particle Size by Dose (Particles/Volume)", x = "Dose (particles/mL)", y = "Size (µm)", color = "Effect")+
  stat_smooth(method = "lm", col = "black")

plot(Count_Reported)

### Size Vs. Dose - Mass - Reported Only ####

Mass <- aoc_setup %>%
  filter(dose.mg.L.master.converted.reported != "converted") %>% 
  group_by(dose.mg.L.master, size.length.um.used.for.conversions) %>% 
  summarise() %>% 
  ggplot(aes(x = dose.mg.L.master, y = size.length.um.used.for.conversions)) +
  geom_point(color = "deeppink3", alpha = 0.5) +
  theme_bw()+
  theme(text = element_text(size=14))+
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Particle Size by Dose (Mass/Volume)", x = "Dose (mg/L)", y = "Size (µm)", color = "Effect")+
  stat_smooth(method = "lm", col = "black")

plot(Mass)

### Dose - Counts Vs. Dose - Mass ####

Mass_Count <- aoc_setup %>%
  group_by(dose.mg.L.master, dose.particles.mL.master) %>% 
  summarise() %>% 
  ggplot(aes(x = dose.mg.L.master, y = dose.particles.mL.master)) +
  geom_point(color = "deeppink3", alpha = 0.5) +
  theme_bw()+
  theme(text = element_text(size=14))+
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Dose (Particles/Volume) by Dose (Mass/Volume)", x = "Dose (mg/L)", y = "Dose (particles/mL)")+
  stat_smooth(method = "lm", col = "black")

plot(Mass_Count)

### Linear Models ####

x <- aoc_setup %>%
  filter(org_f == "Crustacea") %>% 
  filter(lvl2_f == "Fitness") %>% 
  group_by(size.length.um.used.for.conversions, dose.particles.mL.master, effect_f) %>% 
  summarise() %>% 
  drop_na() %>% 
  lm(formula = effect_f ~ log10(size.length.um.used.for.conversions) + log10(dose.particles.mL.master))

summary(x)
