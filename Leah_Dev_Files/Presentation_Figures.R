# Microplastics Toxicity Literature Review
# Southern California Coastal Water Research Project
# Presentation Figures Mock-Up Script
# Created on: November 18, 2020 by Leah Thornton Hampton

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

# Import finalized dataset.

# Guess_max ensures columns with lots of NAs are not imported as logical vectors, but as numeric/double.
aoc <- read_csv("AquaticOrganisms_Clean_final.csv", guess_max = 10000)

#### Set-Up and Cleaning ####

#This is the same tidying and setup as found in app.R as of 2/1/20
aoc_work <- aoc %>% 
  mutate(effect_f = factor(case_when(effect == "Y" ~ "Yes",
                                     effect == "N" ~ "No"),
                           levels = c("No", "Yes"))) %>%
  replace_na(list(size.category = 0, shape = "Not Reported", polymer = "Not Reported", life.stage = "Not Reported")) %>% 
  mutate(size_f = factor(case_when(
    size.category == 1 ~ "1nm < 100nm",
    size.category == 2 ~ "100nm < 1µm",
    size.category == 3 ~ "1µm < 100µm",
    size.category == 4 ~ "100µm < 1mm",
    size.category == 5 ~ "1mm < 5mm",
    size.category == 0 ~ "Not Reported"),
    levels = c("1nm < 100nm", "100nm < 1µm", "1µm < 100µm", "100µm < 1mm", "1mm < 5mm", "Not Reported"))) %>% 
  mutate(shape_f = factor(case_when(
    shape == "fiber" ~ "Fiber",
    shape == "fragment" ~ "Fragment",
    shape == "sphere" ~ "Sphere",
    shape == "Not Reported" ~ "Not Reported"),
    levels = c("Fiber", "Fragment", "Sphere", "Not Reported"))) %>% 
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
  mutate(org_f = factor(organism.group, levels = c("Algae", "Annelida", "Bacterium", "Cnidaria", "Crustacea",
                                                   "Echinoderm", "Fish", "Insect", "Mollusca", "Nematoda", "Plant", "Rotifera", "Mixed"))) %>% 
  mutate(lvl1_f = factor(case_when(lvl1 == "alimentary.excretory" ~ "Alimentary, Excretory",
                                   lvl1 == "behavioral.sense.neuro" ~ "Behavioral, Sensory, Neurological",
                                   lvl1 == "circulatory.respiratory" ~ "Circulatory, Respiratory",
                                   lvl1 == "community" ~ "Community",
                                   lvl1 == "fitness" ~ "Fitness",
                                   lvl1 == "immune" ~ "Immune",
                                   lvl1 == "metabolism" ~ "Metabolism",
                                   lvl1 == "microbiome" ~ "Microbiome",
                                   lvl1 == "stress" ~ "Stress"))) %>% 
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
                                   lvl2 == "vision.system"~"Vision System"))) %>% 
  mutate(bio_f = factor(case_when(bio.org == "cell"~"Cell", 
                                  bio.org == "organism"~"Organism",
                                  bio.org == "population"~ "Population",
                                  bio.org == "subcell"~"Subcell",
                                  bio.org == "tissue" ~ "Tissue")))%>%
  mutate(vivo_f = factor(case_when(invitro.invivo == "invivo"~"In Vivo",
                                   invitro.invivo == "invitro"~"In Vitro")))%>% 
  mutate(life_f = factor(case_when(life.stage == "Early"~"Early",
                                   life.stage == "Juvenile"~"Juvenile",
                                   life.stage == "Adult"~"Adult",
                                   life.stage == "Not Reported"~"Not Reported")))%>% 
  mutate(env_f = factor(case_when(environment == "Freshwater"~"Freshwater",
                                  environment == "Marine" ~ "Marine",
                                  environment == "Terrestrial" ~ "Terrestrial"))) %>%
  mutate(species_f = as.factor(paste(genus,species))) %>% 
  mutate(dose.mg.L.master.converted.reported = factor(dose.mg.L.master.converted.reported)) %>%
  mutate(dose.particles.mL.master.converted.reported = factor(dose.particles.mL.master.converted.reported)) %>% 
  mutate(effect.metric = factor(effect.metric)) %>%
  mutate(dose.um3.mL.master = particle.volume.um3 * dose.particles.mL.master) %>%  
  mutate(af.time_noNA = replace_na(af.time, "Unavailable")) %>% 
  mutate(acute.chronic_f = factor(case_when(af.time_noNA == 10 ~ "Acute",
                                            af.time_noNA == 1 ~ "Chronic",
                                            af.time_noNA == "Unavailable" ~ "Unavailable")))   

#### JPA Example Figure #### 

Fiber <- aoc_work %>%
  filter(shape_f == "Fiber") %>%
  filter(org_f != "Annelida") %>%
  filter(effect_f != "NA") %>% 
  # filter(life_f == "Early") %>% 
  ggplot(aes(x = dose.particles.mL.master, y = org_f, fill = effect_f, color = effect_f)) +
  # geom_rect(alpha = 0.7, ymin = 0, ymax = Inf, xmin = log10(0), xmax = log10(0.03929), color = "black", fill = "gray87") + #Total particles SFEI grab sample range
  # geom_rect(alpha = 0.1, ymin = 0, ymax = Inf, xmin = log10(0), xmax = log10(0.03368), color = "black", fill = "mediumpurple1") + #Total fibers SFEI grab sample range
  geom_jitter(alpha = 0.8, size = 5, height = .2, width = .1) +
  scale_fill_manual(values = c("royalblue", "darkcyan"))+
  scale_color_manual(values = c("royalblue", "darkcyan"))+
  theme_classic() +
  scale_x_log10() +
  theme(text = element_text(size = 18),
        legend.position="right", 
        legend.title = element_blank(),
        axis.text = element_text(color = "black")) +
  labs(x = "Concentration (particles/mL)", y = "Organism Group")

plot(Fiber)

### Literature Review Presentation Demonstration Plots ####

#Set up for literature review figures

aoc_ex <- aoc %>% # start with original dataset
  filter(environment != "Terrestrial") %>% # removes terrestrial data.
  mutate(size.category.noNA = replace_na(size.category, 0)) %>% # replaces NA with 0 so we can better relabel it
  mutate(size_cat = case_when(
    size.category.noNA == 1 ~ "1nm < 100nm",
    size.category.noNA == 2 ~ "100nm < 1µm",
    size.category.noNA == 3 ~ "1µm < 100µm",
    size.category.noNA == 4 ~ "100µm < 1mm",
    size.category.noNA == 5 ~ "1mm < 5mm",
    size.category.noNA == 0 ~ "unavailable")) %>% # creates a new column with nicer names for later formatting
  mutate(size_cat_f = factor(size_cat, levels = c("1nm < 100nm", "100nm < 1µm", "1µm < 100µm", "100µm < 1mm", "1mm < 5mm", "Not Reported"))) %>% # telling R how to order our different size levels
  mutate(shape.noNA = replace_na(shape, "Not Reported")) %>% # replaces NAs so we can better relabel it
  mutate(shape_f = factor(shape.noNA, levels = c("Fiber", "Fragment", "Sphere", "Not Reported"))) %>% # telling R how to order our different size levels
  mutate(polymer.noNA = replace_na(polymer, "Not Reported")) %>% # replaces NA with 0 so we can better relabel it
  mutate(poly_f = factor(polymer.noNA, levels = c("BIO", "EVA", "PA", "PC", "PE", "PET", "PLA", "PMMA", "PP", "PS", "PUR", "PVC", "Not Reported"))) %>%
  mutate(organism.group.noNA = replace_na(organism.group, "unavailable")) %>% # replaces NA with 0 so we can better relabel it
  mutate(organism.group_f = factor(organism.group.noNA, levels = c("Crustacea","Mollusca","Fish","Algae","Annelida", "Bacterium","Cnidaria","Echinoderm","Insect","Plant","Rotifera","Mixed")))%>%
  mutate(lvl1_cat = case_when(
    lvl1 == "alimentary.excretory" ~ "Alimentary, Excretory",
    lvl1 == "behavioral.sense.neuro" ~ "Behavioral, Sensory, Neurological",
    lvl1 == "circulatory.respiratory" ~ "Circulatory, Respiratory",
    lvl1 == "community" ~ "Community",
    lvl1 == "fitness" ~ "Fitness",
    lvl1 == "immune" ~ "Immune",
    lvl1 == "metabolism" ~ "Metabolism",
    lvl1 == "microbiome" ~ "Microbiome",
    lvl1 == "stress" ~ "Stress")) %>% # creates new column with nicer names.
  mutate(lvl1_f = factor(lvl1_cat)) # order different endpoints.


# Main figure example

main_ex_1 <- aoc_ex %>%
  filter(organism.group_f != "unavailable") %>%
  filter(organism.group_f != "Algae") %>%
  filter(organism.group_f != "Annelida") %>%
  filter(organism.group_f != "Bacterium") %>%
  filter(organism.group_f != "Cnidaria") %>%
  filter(organism.group_f != "Echinoderm") %>%
  filter(organism.group_f != "Insect") %>%
  filter(organism.group_f != "Plant") %>%
  filter(organism.group_f != "Rotifera") %>%
  filter(effect_f == "Y") %>%
  ggplot(aes(x = dose.mg.L, y = poly_f)) +
  scale_x_log10(breaks = c(0.0001, 0.01, 1, 100, 10000), 
                labels = c(0.0001, 0.01, 1, 100, 10000)) +
  geom_boxplot(alpha = 0.7, show.legend = TRUE, aes(color = lvl1_f, fill = lvl1_f, 
                                                    xmin = 0.00005, xmax = 15000)) +
  scale_fill_manual(values = cal_palette("kelp1", n = 8, type = "continuous")) +
  scale_color_manual(values = cal_palette("kelp1", n = 8, type = "continuous")) +
  theme_classic() +
  theme(text = element_text(size = 18),legend.position="bottom", legend.title = element_blank()) +
  labs(x = "Concentration (mg/L)", y = "Organism Group",
       fill = "Endpoint Category", color = "Endpoint Category")

main_ex_1

#Fitness only

main_ex_2 <- aoc_ex %>%
  filter(organism.group_f != "unavailable") %>%
  filter(organism.group_f != "Algae") %>%
  filter(organism.group_f != "Annelida") %>%
  filter(organism.group_f != "Bacterium") %>%
  filter(organism.group_f != "Cnidaria") %>%
  filter(organism.group_f != "Echinoderm") %>%
  filter(organism.group_f != "Insect") %>%
  filter(organism.group_f != "Plant") %>%
  filter(organism.group_f != "Rotifera") %>%
  filter(effect_f == "Y") %>%
  filter(lvl1_f == "Fitness") %>%
  filter(lvl2 != "sexhormones") %>%
  ggplot(aes(x = dose.mg.L, y = organism.group_f)) +
  scale_x_log10(breaks = c(0.0001, 0.01, 1, 100, 10000), 
                labels = c(0.0001, 0.01, 1, 100, 10000)) +
  geom_boxplot(alpha = 0.7, show.legend = TRUE, aes(color = lvl2, fill = lvl2, 
                                                    xmin = 0.00005, xmax = 15000)) +
  scale_fill_manual(values = cal_palette("kelp1", n = 5, type = "continuous")) +
  scale_color_manual(values = cal_palette("kelp1", n = 5, type = "continuous")) +
  theme_classic() +
  theme(text = element_text(size = 18),legend.position="bottom") +
  labs(x = "Concentration (mg/L)", y = "Organism Group",
       fill = "Effect", color = "Effect")

main_ex_2

#Effect of size category on crustacean fitness 

main_ex_3 <- aoc_ex %>%
  filter(size_cat_f != "unavailable") %>%
  filter(organism.group_f == "Crustacea") %>%
  filter(effect_f == "Y") %>%
  filter(lvl1_f == "Fitness") %>%
  ggplot(aes(x = dose.mg.L, y = size_cat_f)) +
  scale_x_log10(breaks = c(0.0001, 0.01, 1, 100, 10000), 
                labels = c(0.0001, 0.01, 1, 100, 10000)) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE, aes(color = size_cat_f, fill = size_cat_f, 
                                                     xmin = 0.00005, xmax = 15000)) +
  scale_fill_manual(values = cal_palette("sbchannel", n = 5, type = "continuous")) +
  scale_color_manual(values = cal_palette("sbchannel", n = 5, type = "continuous")) +
  theme_classic() +
  theme(text = element_text(size = 18),legend.position="none") +
  labs(x = "Concentration (mg/L)", y = "Size Category",
       fill = "Effect", color = "Effect")

main_ex_3


# Size filtered by growth

main_ex_4 <- aoc_ex %>%
  filter(effect_f == "Y") %>%
  filter(size_cat_f != "unavailable") %>%
  filter(lvl2 == "growth") %>%
  ggplot(aes(x = dose.mg.L, y = size_cat_f)) +
  scale_x_log10(breaks = c(0.0001, 0.01, 1, 100, 10000), 
                labels = c(0.0001, 0.01, 1, 100, 10000)) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE, aes(color = size_cat_f, fill = size_cat_f, xmin = 0.00005, xmax = 15000)) +
  scale_fill_manual(values = cal_palette("eschscholzia", n = 6, type = "continuous")) +
  scale_color_manual(values = cal_palette("eschscholzia", n = 6, type = "continuous")) +
  theme_classic() +
  theme(text = element_text(size = 18),legend.position="none") +
  labs(x = "Concentration (mg/L)",
       y = "Size Category")

main_ex_4

# Size filtered by growth with no effect doses

main_ex_5 <- aoc_ex %>%
  filter(size_cat_f != "unavailable") %>%
  filter(lvl2 == "growth") %>%
  ggplot(aes(x = dose.mg.L, y = size_cat_f)) +
  scale_x_log10(breaks = c(0.0001, 0.01, 1, 100, 10000), 
                labels = c(0.0001, 0.01, 1, 100, 10000)) +
  geom_boxplot(position = position_dodge(preserve = "single"), alpha = 0.7, show.legend = TRUE, aes(color = effect_f, fill = effect_f, xmin = 0.00005, xmax = 15000)) +
  scale_fill_manual(values = cal_palette("eschscholzia", n = 2, type = "continuous")) +
  scale_color_manual(values = cal_palette("eschscholzia", n = 2, type = "continuous")) +
  theme_classic() +
  theme(text = element_text(size = 18),legend.position="bottom") +
  labs(x = "Concentration (mg/L)",
       y = "Size Category", color = "Effect", fill = "Effect")

main_ex_5

# Size filtered by growth and if organisms were fed during the exposure

main_ex_6 <- aoc_ex %>%
  filter(size_cat_f != "unavailable") %>%
  filter(lvl2 == "growth") %>%
  filter(fed == "Y")%>%
  ggplot(aes(x = dose.mg.L, y = size_cat_f)) +
  scale_x_log10(breaks = c(0.0001, 0.01, 1, 100, 10000), 
                labels = c(0.0001, 0.01, 1, 100, 10000)) +
  geom_boxplot(position = position_dodge(preserve = "single"), alpha = 0.7, show.legend = TRUE, aes(color = effect_f, fill = effect_f, xmin = 0.00005, xmax = 15000)) +
  scale_fill_manual(values = cal_palette("eschscholzia", n = 2, type = "continuous")) +
  scale_color_manual(values = cal_palette("eschscholzia", n = 2, type = "continuous")) +
  theme_classic() +
  theme(text = element_text(size = 18),legend.position="bottom") +
  labs(x = "Concentration (mg/L)",
       y = "Size Category", color = "Effect", fill = "Effect")

main_ex_6


# Size filtered by growth, if organisms were fed, and if exposure duration 7d or longer

size_growth_fed_expd_ex <- aoc_ex %>%
  filter(effect_f == "Y") %>%
  filter(size_cat_f != "unavailable") %>%
  filter(lvl2 == "growth") %>%
  filter(fed == "Y") %>%
  filter(exposure.duration.d >= 7) %>%
  ggplot(aes(x = dose.mg.L, y = size_cat_f)) +
  scale_x_log10(breaks = c(0.0001, 0.01, 1, 100, 10000), 
                labels = c(0.0001, 0.01, 1, 100, 10000)) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE, aes(color = size_cat_f, fill = size_cat_f, xmin = 0.00005, xmax = 15000)) +
  scale_fill_manual(values = cal_palette("sbchannel", n = 6, type = "continuous")) +
  scale_color_manual(values = cal_palette("sbchannel", n = 6, type = "continuous")) +
  theme_classic() +
  theme(text = element_text(size = 18),legend.position="none") +
  labs(x = "Concentration (mg/L)",
       y = "Size")

size_growth_fed_expd_ex

# Shape example

shape_ex <- aoc_ex %>%
  filter(effect_f == "Y") %>%
  filter(shape_f != "unavailable") %>%
  ggplot(aes(x = dose.mg.L, y = shape_f)) +
  scale_x_log10(breaks = c(0.0001, 0.01, 1, 100, 10000), 
                labels = c(0.0001, 0.01, 1, 100, 10000)) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE, aes(color = shape_f, fill = shape_f, xmin = 0.00005, xmax = 15000)) +
  scale_fill_manual(values = cal_palette("sbchannel", n = 3, type = "continuous")) +
  scale_color_manual(values = cal_palette("sbchannel", n = 3, type = "continuous")) +
  theme_classic() +
  theme(text = element_text(size = 18),legend.position="none") +
  labs(x = "Concentration (mg/L)",
       y = "Shape")

shape_ex

# Shape example with "Ns" and "Ys" plotted next to one another

shape_effect_ex <- aoc_ex %>%
  filter(shape_f != "unavailable") %>%
  ggplot(aes(x = dose.mg.L, y = shape_f)) +
  scale_x_log10(breaks = c(0.0001, 0.01, 1, 100, 10000), 
                labels = c(0.0001, 0.01, 1, 100, 10000)) +
  geom_boxplot(alpha = 0.7, show.legend = TRUE, aes(color = effect_f, fill = effect_f, xmin = 0.00005, xmax = 15000)) +
  scale_fill_manual(values = cal_palette("sbchannel", n = 2, type = "continuous")) +
  scale_color_manual(values = cal_palette("sbchannel", n = 2, type = "continuous")) +
  theme_classic() +
  theme(text = element_text(size = 18),legend.position="bottom") +
  labs(x = "Concentration (mg/L)",
       y = "Shape",
       fill = "Effect", color = "Effect")

shape_effect_ex

# Shape example with "Ns" and "Ys" plotted next to one anotherand filtered to only include crustaceans

shape_effect_crust_ex <- aoc_ex %>%
  filter(shape_f != "unavailable") %>%
  filter(organism.group == "Crustacea") %>%
  ggplot(aes(x = dose.mg.L, y = shape_f)) +
  scale_x_log10(breaks = c(0.0001, 0.01, 1, 100, 10000), 
                labels = c(0.0001, 0.01, 1, 100, 10000)) +
  geom_boxplot(alpha = 0.7, show.legend = TRUE, aes(color = effect_f, fill = effect_f, xmin = 0.00005, xmax = 15000)) +
  scale_fill_manual(values = cal_palette("sbchannel", n = 2, type = "continuous")) +
  scale_color_manual(values = cal_palette("sbchannel", n = 2, type = "continuous")) +
  theme_classic() +
  theme(text = element_text(size = 18),legend.position="bottom") +
  labs(x = "Concentration (mg/L)",
       y = "Shape",
       fill = "Effect",
       color = "Effect")

shape_effect_crust_ex

# Main key figure for presentation

main_ex <- aoc_ex %>%
  filter(effect_f == "Y") %>%
  filter(organism.group == "Fish") %>%
  filter(organism.group == "Crustacea") %>%
  filter(organism.group == "Mollusca") %>%
  #filter(lvl2 == "mortality") %>%
  #filter(lvl2 == "reproduction") %>%
  #filter(lvl2 == "growth") %>%
  ggplot(aes(x = dose.mg.L, y = organism.group)) +
  scale_x_log10(breaks = c(0.0001, 0.01, 1, 100, 10000), 
                labels = c(0.0001, 0.01, 1, 100, 10000)) +
  geom_boxplot() +
  #geom_boxplot(alpha = 0.7, show.legend = FALSE, aes(color = lvl2, fill = lvl2, xmin = 0.00005, xmax = 15000)) +
  #scale_fill_manual(values = cal_palette("sbchannel", n = 3, type = "continuous")) +
  #scale_color_manual(values = cal_palette("sbchannel", n = 3, type = "continuous")) +
  theme_classic() +
  theme(text = element_text(size = 18),legend.position="none") +
  labs(x = "Concentration (mg/L)",
       y = "Organism Group",
       Color = "Endpoint Category",
       Fill = "Endpoint Category")

main_ex


#### Aquatic Organisms Presentation Demonstration Plots Example 1 ####

#Set up for aquatic organisms presentation demonstration plots

# Example 1.0: All organism groups by broad endpoint category

main <- aoc_work %>%
  filter(organism.group_f != "NA") %>%
  filter(effect_f == "Y") %>%
  ggplot(aes(x = dose.mg.L.master, y = organism.group_f)) +
  scale_x_log10() +
  geom_boxplot(alpha = 0.7, show.legend = TRUE, aes(color = organism.group_f, fill = organism.group_f,)) +
  scale_fill_manual(values = cal_palette("kelp1", n = 11, type = "continuous")) +
  scale_color_manual(values = cal_palette("kelp1", n = 11, type = "continuous")) +
  theme_classic() +
  theme(text = element_text(size = 18),legend.position="none", legend.title = element_blank()) +
  labs(x = "Concentration (mg/L)", y = "Organism Group")

main

# Example 1.1: Crustaceans by broad endpoint category

crusty <- aoc_work %>%
  filter(effect_f == "Y") %>%
  filter(organism.group_f == "Crustacea") %>%
  ggplot(aes(x = dose.mg.L.master, y = lvl1_f)) +
  scale_x_log10() +
  geom_boxplot(alpha = 0.7, show.legend = TRUE, aes(color = lvl1_f, fill = lvl1_f,)) +
  scale_fill_manual(values = cal_palette("kelp1", n = 11, type = "continuous")) +
  scale_color_manual(values = cal_palette("kelp1", n = 11, type = "continuous")) +
  theme_classic() +
  theme(text = element_text(size = 18),legend.position="none", legend.title = element_text()) +
  labs(x = "Concentration (mg/L)", y = "Endpoint Category")

crusty

# Example 1.3: Crustaceans fitness by polymer

polymer <- aoc_work %>%
  filter(polymer_cat_f != "Polyvinylchloride") %>%
  filter(polymer_cat_f != "Polyurathane") %>%
  filter(effect_f == "Y") %>%
  filter(lvl1_f == "Fitness") %>%
  filter(organism.group_f == "Crustacea") %>%
  ggplot(aes(x = dose.mg.L.master, y = polymer_cat_f)) +
  scale_x_log10() +
  geom_boxplot(alpha = 0.7, show.legend = TRUE, aes(color = polymer_cat_f, fill = polymer_cat_f,)) +
  scale_fill_manual(values = cal_palette("kelp1", n = 7, type = "continuous")) +
  scale_color_manual(values = cal_palette("kelp1", n = 7, type = "continuous")) +
  theme_classic() +
  theme(text = element_text(size = 18),legend.position="none", legend.title = element_text()) +
  labs(x = "Concentration (mg/L)", y = "Polymer",
       fill = "polymer_cat_f", color = "polymer_cat_f")

polymer

# Example 1.4: Main figure filtered by crustaceans, growth, effect of shape

shape <- aoc_work %>%
  filter(effect_f == "Y") %>%
  filter(lvl1_f == "Fitness") %>%
  filter(organism.group_f == "Crustacea") %>%
  ggplot(aes(x = dose.mg.L.master, y = shape_cat_f)) + 
  scale_x_log10() +
  geom_boxplot(alpha = 0.7, show.legend = TRUE, aes(color = shape_cat_f, fill = shape_cat_f,)) +
  scale_fill_manual(values = cal_palette("kelp1", n = 4, type = "continuous")) +
  scale_color_manual(values = cal_palette("kelp1", n = 4, type = "continuous")) +
  theme_classic() +
  theme(text = element_text(size = 18),legend.position="none", legend.title = element_text()) +
  labs(x = "Concentration (mg/L)", y = "Shape",
       fill = "shape_cat_f", color = "shape_cat_f")

shape

# Example 1.5:  Main figure filtered by crustaceans, growth effect of size category

size <- aoc_work %>%
  filter(size_cat_f != "1mm < 5mm")%>%
  filter(size_cat_f != "Size Not Reported")%>%
  filter(effect_f == "Y") %>%
  filter(lvl1_f == "Fitness") %>%
  filter(organism.group_f == "Crustacea") %>%
  ggplot(aes(x = dose.mg.L.master, y = size_cat_f)) +
  scale_x_log10() +
  geom_boxplot(alpha = 0.7, show.legend = TRUE, aes(color = size_cat_f, fill = size_cat_f,)) +
  scale_fill_manual(values = cal_palette("kelp1", n = 4, type = "continuous")) +
  scale_color_manual(values = cal_palette("kelp1", n = 4, type = "continuous")) +
  theme_classic() +
  theme(text = element_text(size = 18),legend.position="none", legend.title = element_text()) +
  labs(x = "Concentration (mg/L)", y = "Size Category",
       fill = "size_cat_f", color = "size_cat_f")

size

# Example 1.3.1: Crustaceans fitness by polymer - effect/no effect

polymereff <- aoc_work %>%
  filter(polymer_cat_f != "Polyvinylchloride") %>%
  filter(polymer_cat_f != "Polyurathane") %>%
  filter(polymer_cat_f != "Polyisoprene") %>%
  filter(lvl1_f == "Fitness") %>%
  filter(organism.group_f == "Crustacea") %>%
  ggplot(aes(x = dose.mg.L.master, y = polymer_cat_f)) +
  scale_x_log10() +
  geom_boxplot(alpha = 0.7, show.legend = TRUE, aes(color = effect_f, fill = effect_f,)) +
  scale_fill_manual(values = cal_palette("kelp1", n = 7, type = "continuous")) +
  scale_color_manual(values = cal_palette("kelp1", n = 7, type = "continuous")) +
  theme_classic() +
  theme(text = element_text(size = 18),legend.position="right", legend.title = element_text()) +
  labs(x = "Concentration (mg/L)", y = "Polymer",
       fill = "Effect", color = "Effect")

polymereff

# Example 1.4.1: Crustaceans fitness by shape - effect/no effect

shapeeff <- aoc_work %>%
  filter(lvl1_f == "Fitness") %>%
  filter(organism.group_f == "Crustacea") %>%
  ggplot(aes(x = dose.mg.L.master, y = shape_cat_f)) + 
  scale_x_log10() +
  geom_boxplot(alpha = 0.7, show.legend = TRUE, aes(color = effect_f, fill = effect_f,)) +
  scale_fill_manual(values = cal_palette("kelp1", n = 4, type = "continuous")) +
  scale_color_manual(values = cal_palette("kelp1", n = 4, type = "continuous")) +
  theme_classic() +
  theme(text = element_text(size = 18),legend.position="right", legend.title = element_text()) +
  labs(x = "Concentration (mg/L)", y = "Shape",
       fill = "Effect", color = "Effect")

shapeeff

# Example 1.5.1:  Crustaceans fitness by polymer - effect/no effect

sizeeff <- aoc_work %>%
  filter(size_cat_f != "1mm < 5mm")%>%
  filter(size_cat_f != "Size Not Reported")%>%
  filter(lvl1_f == "Fitness") %>%
  filter(organism.group_f == "Crustacea") %>%
  ggplot(aes(x = dose.mg.L.master, y = size_cat_f)) +
  scale_x_log10() +
  geom_boxplot(alpha = 0.7, show.legend = TRUE, aes(color = effect_f, fill = effect_f,)) +
  scale_fill_manual(values = cal_palette("kelp1", n = 4, type = "continuous")) +
  scale_color_manual(values = cal_palette("kelp1", n = 4, type = "continuous")) +
  theme_classic() +
  theme(text = element_text(size = 18),legend.position="right", legend.title = element_text()) +
  labs(x = "Concentration (mg/L)", y = "Size Category",
       fill = "Effect", color = "Effect")

sizeeff

#### Aquatic Organisms Presentation Demonstration Plots Example 2 ####

# Example 2.0: Crustacean development, filtered for at least 7d exposure, size

crustydev7dsize <- aoc_work %>%
  filter(size_cat_f !="1mm < 5mm")%>%
  filter(lvl2 == "development") %>%
  filter(organism.group_f == "Crustacea")%>%
  filter(exposure.duration.d >= 7)%>%
  ggplot(aes(x = dose.mg.L.master, y = size_cat_f)) +
  scale_x_log10() +
  geom_boxplot(alpha = 0.7, show.legend = TRUE, aes(color = effect_f, fill = effect_f,)) +
  scale_fill_manual(values = cal_palette("superbloom3", n = 2, type = "continuous")) +
  scale_color_manual(values = cal_palette("superbloom3", n = 2, type = "continuous")) +
  theme_classic() +
  theme(text = element_text(size = 18),legend.position="top", legend.title = element_blank()) +
  labs(x = "Concentration (mg/L)", y = "Size Category",
       fill = "Effect", color = "Effect")

crustydev7dsize

# Example 2.1: Crustacean development, filtered for at least 7d exposure, polymer

crustydev7dpoly <- aoc_work %>%
  filter(polymer_cat_f != "Polyamide")%>%
  filter(organism.group_f == "Crustacea")%>%
  filter(lvl2 == "development") %>%
  filter(exposure.duration.d >= 7)%>%
  ggplot(aes(x = dose.mg.L.master, y = polymer_cat_f)) +
  scale_x_log10() +
  geom_boxplot(alpha = 0.7, show.legend = TRUE, aes(color = effect_f, fill = effect_f,)) +
  scale_fill_manual(values = cal_palette("superbloom3", n = 2, type = "continuous")) +
  scale_color_manual(values = cal_palette("superbloom3", n = 2, type = "continuous")) +
  theme_classic() +
  theme(text = element_text(size = 18),legend.position="top", legend.title = element_blank()) +
  labs(x = "Concentration (mg/L)", y = "Polymer",
       fill = "Effect", color = "Effect")

crustydev7dpoly

# Example 2.2: Crustacean development, filtered for at least 7d exposure, shape

crustydev7dshape <- aoc_work %>%
  filter(shape_cat_f != "Fiber")%>%
  filter(organism.group_f == "Crustacea")%>%
  filter(lvl2 == "development") %>%
  filter(exposure.duration.d >= 7)%>%
  ggplot(aes(x = dose.mg.L.master, y = shape_cat_f)) +
  scale_x_log10() +
  geom_boxplot(alpha = 0.7, show.legend = TRUE, aes(color = effect_f, fill = effect_f,)) +
  scale_fill_manual(values = cal_palette("superbloom3", n = 2, type = "continuous")) +
  scale_color_manual(values = cal_palette("superbloom3", n = 2, type = "continuous")) +
  theme_classic() +
  theme(text = element_text(size = 18),legend.position="top", legend.title = element_blank()) +
  labs(x = "Concentration (mg/L)", y = "Shape",
       fill = "Effect", color = "Effect")

crustydev7dshape

# Example 2.3: All organism development, filtered for at least 7d exposure

dev7d <- aoc_work %>%
  filter(organism.group_f != "Insect")%>%
  filter(lvl2 == "development") %>%
  filter(exposure.duration.d >= 7)%>%
  ggplot(aes(x = dose.mg.L.master, y = organism.group_f)) +
  scale_x_log10() +
  geom_boxplot(alpha = 0.7, show.legend = TRUE, aes(color = effect_f, fill = effect_f,)) +
  scale_fill_manual(values = cal_palette("superbloom3", n = 2, type = "continuous")) +
  scale_color_manual(values = cal_palette("superbloom3", n = 2, type = "continuous")) +
  theme_classic() +
  theme(text = element_text(size = 18),legend.position="top", legend.title = element_blank()) +
  labs(x = "Concentration (mg/L)", y = "Organism Group",
       fill = "Effect", color = "Effect")

dev7d

# Example 2.4: All organism fitness

fit <- aoc_work %>%
  filter(organism.group_f != "NA")%>%
  filter(organism.group_f != "Insect")%>%
  filter(lvl1_f == "Fitness") %>%
  ggplot(aes(x = dose.mg.L.master, y = organism.group_f)) +
  scale_x_log10() +
  geom_boxplot(alpha = 0.7, show.legend = TRUE, aes(color = effect_f, fill = effect_f,)) +
  scale_fill_manual(values = cal_palette("superbloom3", n = 2, type = "continuous")) +
  scale_color_manual(values = cal_palette("superbloom3", n = 2, type = "continuous")) +
  theme_classic() +
  theme(text = element_text(size = 18),legend.position="top", legend.title = element_blank()) +
  labs(x = "Concentration (mg/L)", y = "Organism Group",
       fill = "Effect", color = "Effect")

fit

# Example 2.5: All organism 

all <- aoc_work %>%
  filter(organism.group_f != "NA")%>%
  ggplot(aes(x = dose.mg.L.master, y = organism.group_f)) +
  scale_x_log10() +
  geom_boxplot(alpha = 0.7, show.legend = TRUE, aes(color = effect_f, fill = effect_f,)) +
  scale_fill_manual(values = cal_palette("superbloom3", n = 2, type = "continuous")) +
  scale_color_manual(values = cal_palette("superbloom3", n = 2, type = "continuous")) +
  theme_classic() +
  theme(text = element_text(size = 18),legend.position="top", legend.title = element_blank()) +
  labs(x = "Concentration (mg/L)", y = "Organism Group",
       fill = "Effect", color = "Effect")

all

#### Aquatic Organisms Presentation Demonstration Plots Example 3 ####

#Example 3.1

all <- aoc_work %>%
  filter(organism.group_f != "NA")%>%
  filter(effect_f == "Y")%>%
  ggplot(aes(x = dose.mg.L.master, y = organism.group_f)) +
  scale_x_log10() +
  geom_boxplot(alpha = 0.7, show.legend = TRUE, aes(color = organism.group_f, fill = organism.group_f,)) +
  scale_fill_manual(values = cal_palette("eschscholzia", n = 11, type = "continuous")) +
  scale_color_manual(values = cal_palette("eschscholzia", n = 11, type = "continuous")) +
  theme_classic() +
  theme(text = element_text(size = 18),legend.position="none", legend.title = element_blank()) +
  labs(x = "Concentration (mg/L)", y = "Organism Group")

all

#Same plot as above but just fish, molluscs and crustacea - 'for simplicity'

fmc <- aoc_work %>%
  filter(organism.group_f != "NA")%>%
  filter(organism.group_f != "Rotifera")%>%
  filter(organism.group_f != "Plant")%>%
  filter(organism.group_f != "Insect")%>%
  filter(organism.group_f != "Echinoderm")%>%
  filter(organism.group_f != "Cnidaria")%>%
  filter(organism.group_f != "Bacterium")%>%
  filter(organism.group_f != "Annelida")%>%
  filter(organism.group_f != "Algae")%>%
  filter(organism.group_f != "Mixed")%>%
  filter(effect_f == "Y")%>%
  ggplot(aes(x = dose.mg.L.master, y = organism.group_f)) +
  scale_x_log10() +
  geom_boxplot(alpha = 0.7, show.legend = TRUE, aes(color = organism.group_f, fill = organism.group_f,)) +
  scale_fill_manual(values = cal_palette("eschscholzia", n = 3, type = "continuous")) +
  scale_color_manual(values = cal_palette("eschscholzia", n = 3, type = "continuous")) +
  theme_classic() +
  theme(text = element_text(size = 18),legend.position="none", legend.title = element_blank()) +
  labs(x = "Concentration (mg/L)", y = "Organism Group")

fmc

#Same plot as above (fish, mollusca and crustaceans) but only looking at 'Fitness'

fitness <- aoc_work %>%
  filter(lvl1_f == "Fitness")%>%
  filter(organism.group_f != "NA")%>%
  filter(organism.group_f != "Rotifera")%>%
  filter(organism.group_f != "Plant")%>%
  filter(organism.group_f != "Insect")%>%
  filter(organism.group_f != "Echinoderm")%>%
  filter(organism.group_f != "Cnidaria")%>%
  filter(organism.group_f != "Bacterium")%>%
  filter(organism.group_f != "Annelida")%>%
  filter(organism.group_f != "Algae")%>%
  filter(organism.group_f != "Mixed")%>%
  filter(effect_f == "Y")%>%
  ggplot(aes(x = dose.mg.L.master, y = organism.group_f)) +
  scale_x_log10() +
  geom_boxplot(alpha = 0.7, show.legend = TRUE, aes(color = organism.group_f, fill = organism.group_f,)) +
  scale_fill_manual(values = cal_palette("eschscholzia", n = 3, type = "continuous")) +
  scale_color_manual(values = cal_palette("eschscholzia", n = 3, type = "continuous")) +
  theme_classic() +
  theme(text = element_text(size = 18),legend.position="none", legend.title = element_blank()) +
  labs(x = "Concentration (mg/L)", y = "Organism Group")

fitness

# Same plot as above but only looking at lvl2 fitness endpoints

crustylvl2 <- aoc_work %>%
  filter(lvl1_f == "Fitness")%>%
  filter(organism.group_f == "Crustacea")%>%
  filter(effect_f == "Y")%>%
  ggplot(aes(x = dose.mg.L.master, y = lvl2_f)) +
  scale_x_log10() +
  geom_boxplot(alpha = 0.7, show.legend = TRUE, aes(color = lvl2_f, fill = lvl2_f,)) +
  scale_fill_manual(values = cal_palette("eschscholzia", n = 5, type = "continuous")) +
  scale_color_manual(values = cal_palette("eschscholzia", n = 5, type = "continuous")) +
  theme_classic() +
  theme(text = element_text(size = 18),legend.position="none", legend.title = element_blank()) +
  labs(x = "Concentration (mg/L)", y = "Specific Endpoint: Fitness")

crustylvl2

# Same plot as above but also looking at where not effect was detected

crustylvl2yn <- aoc_work %>%
  filter(lvl1_f == "Fitness")%>%
  filter(organism.group_f == "Crustacea")%>%
  ggplot(aes(x = dose.mg.L.master, y = lvl2_f)) +
  scale_x_log10() +
  geom_boxplot(alpha = 0.7, show.legend = TRUE, aes(color = effect_f, fill = effect_f,)) +
  scale_fill_manual(values = c("skyblue4", "darkorange1")) +
  scale_color_manual(values = c("skyblue4", "darkorange1")) +
  theme_classic() +
  theme(text = element_text(size = 18),legend.position="top", legend.title = element_blank()) +
  labs(x = "Concentration (mg/L)", y = "Specific Endpoint: Fitness",
       fill = "Effect", color = "Effect")

crustylvl2yn

# Same plot as above but also quality filtered for number of replicates, feeding and particle cleaning

crustylvl2ynquality <- aoc_work %>%
  filter(fed == "Y")%>%
  filter(replicates >= 3)%>%
  filter(clean.method != "N")%>%
  filter(lvl1_f == "Fitness")%>%
  filter(organism.group_f == "Crustacea")%>%
  ggplot(aes(x = dose.mg.L.master, y = lvl2_f)) +
  scale_x_log10() +
  geom_boxplot(alpha = 0.7, show.legend = TRUE, aes(color = effect_f, fill = effect_f,)) +
  scale_fill_manual(values = c("skyblue4", "darkorange1")) +
  scale_color_manual(values = c("skyblue4", "darkorange1")) +
  theme_classic() +
  theme(text = element_text(size = 18),legend.position="top", legend.title = element_blank()) +
  labs(x = "Concentration (mg/L)", y = "Specific Endpoint: Fitness",
       fill = "Effect", color = "Effect")

crustylvl2ynquality

#### Example 4: Aquatic Organisms Presentation ####
# Example 3.0: 

#Plot containing all organisms and types of effects

all <- aoc_work %>%
  filter(organism.group_f !="NA")%>%
  filter(effect_f !="NA")%>%
  ggplot(aes(x = dose.mg.L.master, y = organism.group_f)) +
  scale_x_log10() +
  geom_boxplot(alpha = 0.7, show.legend = TRUE, aes(color = effect_f, fill = effect_f,)) +
  scale_fill_manual(values = c("skyblue4", "darkorange1")) +
  scale_color_manual(values = c("skyblue4", "darkorange1")) +
  theme_classic() +
  theme(text = element_text(size = 22),legend.position="top", legend.title = element_blank()) +
  labs(x = "Concentration (mg/L)", y = "Organism Group",
       fill = "Effect", color = "Effect")

all

#Plot containing crustaceans only and types of effects

crusty <- aoc_work %>%
  filter(organism.group_f =="Crustacea")%>%
  filter(effect_f !="NA")%>%
  ggplot(aes(x = dose.mg.L.master, y = lvl1_f)) +
  scale_x_log10() +
  geom_boxplot(alpha = 0.7, show.legend = TRUE, aes(color = effect_f, fill = effect_f,)) +
  scale_fill_manual(values = c("skyblue4", "darkorange1")) +
  scale_color_manual(values = c("skyblue4", "darkorange1")) +
  theme_classic() +
  theme(text = element_text(size = 22),legend.position="top", legend.title = element_blank()) +
  labs(x = "Concentration (mg/L)", y = "Endpoint Category",
       fill = "Effect", color = "Effect")

crusty

#Plot containing crustacea for fitness 

crustylvl2 <- aoc_work %>%
  filter(effect_f !="NA")%>%
  filter(lvl1_f == "Fitness")%>%
  filter(organism.group_f == "Crustacea")%>%
  ggplot(aes(x = dose.mg.L.master, y = lvl2_f)) +
  expand_limits(x=c(0.00005,15000))+
  scale_x_log10(breaks = c(0.0001, 0.01, 1, 100, 10000), 
                labels = c(0.0001, 0.01, 1, 100, 10000)) +
  geom_boxplot(alpha = 0.7, show.legend = TRUE, aes(color = effect_f, fill = effect_f)) +
  scale_fill_manual(values = c("skyblue4", "darkorange1")) +
  scale_color_manual(values = c("skyblue4", "darkorange1")) +
  theme_classic() +
  theme(text = element_text(size = 22),legend.position="top", legend.title = element_blank()) +
  labs(x = "Concentration (mg/L)", y = "Endpoint Category: Fitness",
       fill = "Effect", color = "Effect")

crustylvl2

#Plot containing crustacea for fitness + quality filters

crustylvl2q <- aoc_work %>%
  filter(effect_f !="NA")%>%
  filter(life.stage == "Early")%>%
  filter(exposure.duration.d >= 7)%>%
  filter(clean.method != "N")%>%
  filter(lvl1_f == "Fitness")%>%
  filter(organism.group_f == "Crustacea")%>%
  ggplot(aes(x = dose.mg.L.master, y = lvl2_f)) +
  expand_limits(x=c(0.00005,15000))+
  scale_x_log10(breaks = c(0.0001, 0.01, 1, 100, 10000), 
                labels = c(0.0001, 0.01, 1, 100, 10000)) +
  geom_boxplot(alpha = 0.7, show.legend = TRUE, aes(color = effect_f, fill = effect_f)) +
  scale_fill_manual(values = c("skyblue4", "darkorange1")) +
  scale_color_manual(values = c("skyblue4", "darkorange1")) +
  theme_classic() +
  theme(text = element_text(size = 22),legend.position="top", legend.title = element_blank()) +
  labs(x = "Concentration (mg/L)", y = "Endpoint Category: Fitness",
       fill = "Effect", color = "Effect")

crustylvl2q

#Plot containing crustacea for fitness in counts

crustylvl2counts <- aoc_work %>%
  filter(effect_f !="NA")%>%
  filter(lvl1_f == "Fitness")%>%
  filter(organism.group_f == "Crustacea")%>%
  ggplot(aes(x = dose.particles.mL.master, y = lvl2_f)) +
  scale_x_log10()+
  geom_boxplot(alpha = 0.7, show.legend = TRUE, aes(color = effect_f, fill = effect_f)) +
  scale_fill_manual(values = c("skyblue4", "darkorange1")) +
  scale_color_manual(values = c("skyblue4", "darkorange1")) +
  theme_classic() +
  theme(text = element_text(size = 22),legend.position="top", legend.title = element_blank()) +
  labs(x = "Concentration (particles/mL)", y = "Endpoint Category: Fitness",
       fill = "Effect", color = "Effect")

crustylvl2counts

#Plot containing crustacea for fitness + quality filters + fibers only

crustylvl2qfiber <- aoc_work %>%
  filter(effect_f !="NA")%>%
  filter(life.stage == "Early")%>%
  filter(exposure.duration.d >= 7)%>%
  filter(clean.method != "N")%>%
  filter(lvl1_f == "Fitness")%>%
  filter(shape_cat_f == "Fiber")%>%
  filter(organism.group_f == "Crustacea")%>%
  ggplot(aes(x = dose.mg.L.master, y = lvl2_f)) +
  expand_limits(x=c(0.00005,15000))+
  scale_x_log10(breaks = c(0.0001, 0.01, 1, 100, 10000), 
                labels = c(0.0001, 0.01, 1, 100, 10000)) +
  geom_boxplot(alpha = 0.7, show.legend = TRUE, aes(color = effect_f, fill = effect_f)) +
  scale_fill_manual(values = c("skyblue4", "darkorange1")) +
  scale_color_manual(values = c("skyblue4", "darkorange1")) +
  theme_classic() +
  theme(text = element_text(size = 22),legend.position="top", legend.title = element_blank()) +
  labs(x = "Concentration (mg/L)", y = "Endpoint Category: Fitness",
       fill = "Effect", color = "Effect")

crustylvl2qfiber

#Plot containing crustacea for fitness + quality filters by shape

crustylvl2qshape <- aoc_work %>%
  filter(effect_f !="NA")%>%
  filter(life.stage == "Early")%>%
  filter(exposure.duration.d >= 7)%>%
  filter(clean.method != "N")%>%
  filter(lvl1_f == "Fitness")%>%
  filter(organism.group_f == "Crustacea")%>%
  ggplot(aes(x = dose.mg.L.master, y = shape_cat_f)) +
  expand_limits(x=c(0.00005,15000))+
  scale_x_log10(breaks = c(0.0001, 0.01, 1, 100, 10000), 
                labels = c(0.0001, 0.01, 1, 100, 10000)) +
  geom_boxplot(alpha = 0.7, show.legend = TRUE, aes(color = effect_f, fill = effect_f,)) +
  scale_fill_manual(values = c("skyblue4", "darkorange1")) +
  scale_color_manual(values = c("skyblue4", "darkorange1")) +
  theme_classic() +
  theme(text = element_text(size = 22),legend.position="top", legend.title = element_blank()) +
  labs(x = "Concentration (mg/L)", y = "Shape",
       fill = "Effect", color = "Effect")

crustylvl2qshape

#Plot containing crustacea for fitness + quality filters by polymer

crustylvl2qpoly <- aoc_work %>%
  filter(effect_f !="NA")%>%
  filter(life.stage == "Early")%>%
  filter(exposure.duration.d >= 7)%>%
  filter(clean.method != "N")%>%
  filter(lvl1_f == "Fitness")%>%
  filter(organism.group_f == "Crustacea")%>%
  ggplot(aes(x = dose.mg.L.master, y = polymer_cat_f)) +
  expand_limits(x=c(0.00005,15000))+
  scale_x_log10(breaks = c(0.0001, 0.01, 1, 100, 10000), 
                labels = c(0.0001, 0.01, 1, 100, 10000)) +
  geom_boxplot(alpha = 0.7, show.legend = TRUE, aes(color = effect_f, fill = effect_f,)) +
  scale_fill_manual(values = c("skyblue4", "darkorange1")) +
  scale_color_manual(values = c("skyblue4", "darkorange1")) +
  theme_classic() +
  theme(text = element_text(size = 22),legend.position="top", legend.title = element_blank()) +
  labs(x = "Concentration (mg/L)", y = "Polymer",
       fill = "Effect", color = "Effect")

crustylvl2qpoly

#Plot containing crustacea for fitness + quality filters by size category

crustylvl2qsize <- aoc_work %>%
  filter(effect_f !="NA")%>%
  filter(life.stage == "Early")%>%
  filter(exposure.duration.d >= 7)%>%
  filter(clean.method != "N")%>%
  filter(lvl1_f == "Fitness")%>%
  filter(organism.group_f == "Crustacea")%>%
  ggplot(aes(x = dose.mg.L.master, y = size_cat_f)) +
  expand_limits(x=c(0.00005,15000))+
  scale_x_log10(breaks = c(0.0001, 0.01, 1, 100, 10000), 
                labels = c(0.0001, 0.01, 1, 100, 10000)) +
  geom_boxplot(alpha = 0.7, show.legend = TRUE, aes(color = effect_f, fill = effect_f,)) +
  scale_fill_manual(values = c("skyblue4", "darkorange1")) +
  scale_color_manual(values = c("skyblue4", "darkorange1")) +
  theme_classic() +
  theme(text = element_text(size = 22),legend.position="top", legend.title = element_blank()) +
  labs(x = "Concentration (mg/L)", y = "Size Category",
       fill = "Effect", color = "Effect")

crustylvl2qsize

#End of Script