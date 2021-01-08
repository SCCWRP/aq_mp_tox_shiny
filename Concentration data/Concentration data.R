## Concentration Data
library(tidyverse)
library(calecopal)
library(ssdtools)
library(DT)
library(plotly)
library(gridExtra)
library(grid)
library(wesanderson)
library(ggdark)
library(broom)

##### Concentration data prep ####
#SFEI data is already prepped
SFEI <- read.csv("Concentration data/SFEI.csv", stringsAsFactors = TRUE) %>% 
  mutate(Location = case_when(
    Location == "Bay\n" ~ "Bay",
    Location == "Sanctuary" ~ "Sanctuary"
  ))

# Adam et al (2019) data needs correcting
adam <- read.csv("Concentration data/adam2019.csv", na.strings = "N.A.") %>% 
  mutate(x1M = Min.particle.size.um,
         x2M = Max.particle.size.um)

#function to derive correction factor (CF) from Koelmans et al (equation 2)
CFfnx = function(a = 1.6, #default alpha from Koelmans et al (2020)
                 x2D = 5000, #set detault values to convert ranges to (1-5,000 um) #5mm is upper defuault 
                 x1D = 1, #1 um is lower default size
                 x2M, x1M){
  
  CF = (x2D^(1-a)-x1D^(1-a))/(x2M^(1-a)-x1M^(1-a))
  
  return(CF)
}
#verify it works (expected answer is 40.37)
CFfnx(x1M = 333, x2M = 5000)

adam <- adam %>% 
  mutate(CF = CFfnx(x1M = x1M, x2M = x2M)) %>%  #create new column with correction factor 
  mutate(particles.m3.corrected = CF * Single.Measurement.conc....m3.) %>% #convert single concenetrations
  mutate(particles.m3.corrected_mean = CF * Mean.conc....m3.) %>%  #convert mean concentrations from distributions
  mutate(particles.m3.corrected_median = CF * Median.conc....m3.) %>%   #convert mean concentrations from distributions
  mutate(particles.single.median.m3 = ifelse(is.na(particles.m3.corrected), particles.m3.corrected_median, particles.m3.corrected)) %>% 
  mutate(particles.m3.master = ifelse(is.na(particles.single.median.m3), particles.m3.corrected_mean, particles.single.median.m3)) %>% 
  mutate(particle.L.master = particles.m3.master/1000) %>% 
  filter(particle.L.master > 0) %>% 
  mutate(System = factor(System))

#write.csv(adam, "Concentration data/adam.csv") #write file so that I can manually add a column that says "sample"
adam<- read.csv("Concentration data/adam.csv", stringsAsFactors = TRUE)

##### SSD prep ####
# Guess_max ensures columns with lots of NAs are not imported as logical vectors, but as numeric/double.
aoc <- read_csv("AquaticOrganisms_Clean_final.csv", guess_max = 10000)
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
    shape == "cube" ~ "Cube",
    shape == NA ~ "Not Reported"),
    levels = c("Fiber", "Fragment", "Sphere", "Cube", "Not Reported"))) %>% # order our different shapes.
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
    polymer == "PUR" ~ "Polyurathane",
    polymer == "PVC" ~ "Polyvinylchloride",
    polymer == "PLA" ~ "Polylactic Acid"))) %>%
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
                                   lvl2 == "agressivity"~"Agressivity",
                                   lvl2 == "ammonia.excretion" ~ "Ammonia Excretion",
                                   lvl2 == "bacteriodetes"~ "Bacteriodetes",
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
                                  environment == "Terrestrial" ~ "Terrestrial"))) #Renames for widget
aoc_z <- aoc_setup %>% # start with Heili's altered dataset (no filtration for terrestrial data)
  # environment category data tidying.
  mutate(environment.noNA = replace_na(environment, "Not Reported")) %>% # replaces NA to better relabel.
  mutate(env_f = factor(environment.noNA, levels = c("Marine", "Freshwater", "Terrestrial", "Not Reported"))) # order our different environments.

# final cleanup and factoring  
aoc_z$Species <- as.factor(paste(aoc_z$genus,aoc_z$species)) #must make value 'Species" (uppercase)
aoc_z$Group <- as.factor(aoc_z$organism.group) #must make value "Group"
aoc_z$Group <- fct_explicit_na(aoc_z$Group) #makes sure that species get counted even if they're missing a group


lvl2_c <- c("Fitness","Abundance","Body Condition","Development","Growth","Mortality")
#Prep food dilution subset
food.dilution <- aoc_z %>% 
  filter(effect_f == "Yes",
         env_f == "Marine") %>% 
  filter(lvl2_f %in% lvl2_c) %>%  # filter by level inputs
  mutate(particle.L = 1000 * dose.particles.mL.master) %>% 
  mutate(Conc = particle.L) %>% 
  drop_na(particle.L)





# read in concentration data
samplesSFEI <- SFEI %>% 
  filter(Sample.Type == "sample") %>% 
  mutate(Conc = Particles.L_Corrected)

#make new dataframe to plot both histograms together
sampleSimple <- samplesSFEI %>%
  select(Conc, Sample.Type) %>% 
  filter(Sample.Type == "sample") %>% 
  droplevels()


food.dilution.simple <- food.dilution %>% 
  select(Conc)

#write.csv(food.dilution.simple, "foodDilutionSimple.csv") # can't figure out how to make a new column with tox for every row, so doing it in excel
food.dilution.simple <- read.csv("Concentration data/foodDilutionSimple.csv", stringsAsFactors = TRUE)

df <- rbind(sampleSimple,food.dilution.simple)

#histogram of samples
samples %>% 
  ggplot(aes(x = Particles.L_Corrected))+
  geom_histogram(aes(x = Particles.L_Corrected, y=..density..), bins = 12) +
  geom_smooth(stat = 'density') +
  scale_x_log10()

#### Plot SSD Data ####

# histogram of SSD (particle/L)
 hist.tox <- food.dilution %>% 
 # filter(dose.particles.mL.master < 1000) %>% 
  ggplot(aes(x = particle.L))+
 geom_histogram(aes(x = particle.L, y=..density..), fill = "blue", bins = 20, alpha = 0.5) +
   geom_smooth(stat = 'density', color = "blue") +
   scale_x_log10() +
   # scale_x_continuous(labels = scales::scientific) +
   xlab("Concentration (particles/L)")+
   labs(title = "Histograms of Food Dilution LOECs",
        caption = "all data corrected to 1-5,000 um; nominal and converted particles/L; SCCWRP tox dataset") +
   theme(plot.title = element_text(hjust = 0.5, size = 20),
         axis.title = element_text(size = 16),
         axis.text =  element_text(size = 16),
         legend.text = element_text(size =14),
         legend.title = element_blank())
 #display
 hist.tox
 #save
 ggsave(hist.tox,
        filename = "FoodDilutionHistogram_particlesL.png",
        path = "Concentration data/plots",
        width = 8,
        scale = 2,
        dpi = 500)
 

 # histogram of SSD (mg/L)
 hist.tox.mass <- food.dilution %>% 
   filter(dose.mg.L.master< 10000) %>% 
   #filter(particle.L < 300) %>% 
   ggplot(aes(x = dose.mg.L.master))+
   geom_histogram(aes(x = dose.mg.L.master, y=..density..), fill = "green", bins = 12, alpha = 0.5) +
   geom_smooth(stat = 'density', color = "green") +
   scale_x_log10() +
   # scale_x_continuous(labels = scales::scientific) +
   xlab("Concentration (mg/L)")+
   labs(title = "Histograms of Food Dilution LOECs",
        caption = "all data corrected to 1-5,000 um; nominal and converted mg/L; SCCWRP tox dataset") +
   theme(plot.title = element_text(hjust = 0.5, size = 20),
         axis.title = element_text(size = 16),
         axis.text =  element_text(size = 16),
         legend.text = element_text(size =14),
         legend.title = element_blank())
 
 #display
 hist.tox.mass
 #save
 ggsave(hist.tox.mass,
        filename = "FoodDilutionHistogram_Particles_mgL.png",
        path = "Concentration data/plots",
        width = 8,
        scale = 2,
        dpi = 500)
 
 #plot together
 hist.tox.mass.particles <- grid.arrange(hist.tox, hist.tox.mass,
                                       ncol = 2,
                                       top = textGrob("Toxicity Histograms by Mass and Particles", gp=gpar(fontsize = 22, font=6)))
 ggsave( hist.tox.mass.particles,
        filename = " hist.tox.mass.particles.png",
        path = "Concentration data/plots",
        width = 8,
        scale = 2,
        dpi = 500)
 
 
 
 #histogram of SFEI concentrations 
 hist.SFEI.occurrence <- df %>% 
   filter(Conc < 10000000) %>%
   filter(Sample.Type == "sample") %>% 
   ggplot(aes(x = Conc, fill = Sample.Type, color = Sample.Type))+
   geom_histogram(aes(x = Conc, y=..density..), bins = 15, alpha = 0.6,position = 'identity') +
   geom_smooth(stat = 'density') +
   scale_x_log10() +
   #coord_cartesian(xlim = c(1,1000)) +
   # scale_x_continuous(labels = scales::scientific) +
   xlab("Concentration (particles/L)")+
   scale_y_continuous(name = "Relative Density", labels = scales::percent)+
   scale_fill_discrete(labels = c("Environmental Concentration")) +
   scale_color_discrete(labels = c("Environmental Concentration")) +
   labs(title = "Microplastics Concentrations in SFEI Dataset",
        caption = "SFEI 2019 data; all data corrected to 1-5,000 um; nominal particle/L")
 hist.SFEI.occurrence
 
 
 #display
 hist.SFEI.occurrence_white <- hist.SFEI.occurrence + 
   theme_minimal()+
   theme(plot.title = element_text(hjust = 0.5, size = 20),
         axis.title = element_text(size = 16),
         axis.text =  element_text(size = 16),
         legend.text = element_text(size =14),
         legend.title = element_blank(),
         legend.position = "none")
 
hist.SFEI.occurrence_white
ggsave(hist.SFEI.occurrence_white, filename = "Histogram_SFEI_occurrence_white.png", path = "Concentration data/plots",
       height = 4, width = 7, scale = 2, dpi = 320)
#display
hist.SFEI.occurrence_dark <- hist.SFEI.occurrence + 
  dark_theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.title = element_text(size = 16),
        axis.text =  element_text(size = 16),
        legend.text = element_text(size =14),
        legend.title = element_blank())

hist.SFEI.occurrence_dark
ggsave(hist.SFEI.occurrence_dark, filename = "Histogram_SFEI_occurrence_dark.png", path = "Concentration data/plots",
       height = 4, width = 7, scale = 2, dpi = 320)


 
#### Plot SFEI with Tox ####
#histogram of both tox and concentrations 
hist.tox.occurrence <- df %>% 
  filter(Conc < 10000000) %>% 
  ggplot(aes(x = Conc, fill = Sample.Type, color = Sample.Type))+
  geom_histogram(aes(x = Conc, y=..density..), bins = 15, alpha = 0.6,position = 'identity') +
  geom_smooth(stat = 'density') +
  scale_x_log10() +
  coord_cartesian(xlim = c(1,10000000)) +
 # scale_x_continuous(labels = scales::scientific) +
  xlab("Concentration (particles/L)")+
  scale_y_continuous(name = "Relative Density", labels = scales::percent)+
  scale_fill_discrete(labels = c("Environmental Concentration", "LOEC")) +
  scale_color_discrete(labels = c("Environmental Concentration", "LOEC")) +
  labs(title = "Histograms of Concentrations in SFEI Dataset vs. Food Dilution LOECs",
       caption = "SFEI 2019 data; all data corrected to 1-5,000 um; nominal particle/L; SCCWRP tox dataset",
       fill = "Env. Conc. or Tox. Conc.",
       color = "Env. Conc. or Tox. Conc.")
 

#display
hist.tox.occurrence_white <- hist.tox.occurrence + 
  theme(plot.title = element_text(hjust = 0.5, size = 20),
                                                         axis.title = element_text(size = 16),
                                                         axis.text =  element_text(size = 16),
                                                         legend.text = element_text(size =14),
                                                       legend.title = element_blank())

hist.tox.occurrence_white

#save
ggsave(hist.tox.occurrence_white, filename = "Histogram_SFEI_tox_occurrence_white.png", path = "Concentration data/plots",
       width = 8, scale = 2, dpi = 500)

#Dark mode
hist.tox.occurrence_dark <- hist.tox.occurrence +
  dark_mode()+
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.title = element_text(size = 16),
        axis.text =  element_text(size = 16),
        legend.text = element_text(size =14),
        legend.title = element_blank())
hist.tox.occurrence_dark

#save
ggsave(hist.tox.occurrence_dark, filename = "Histogram_SFEI_tox_occurrence_dark.png", path = "Concentration data/plots",
       height = 4, width = 7, scale = 2, dpi = 320)



#ECDF by season
ECDF.Season <- samplesSFEI %>% 
  ggplot(aes(x = Particles.L_Corrected, color = Season))+
  stat_ecdf(geom = "point", size = 2) +
  stat_ecdf(geom = "step", linetype = 'solid', alpha = 0.6, size = 1.5) +
  scale_color_manual(values = wes_palette("GrandBudapest2"))+
  geom_vline(xintercept = 75.6, linetype = 'dashed', color = 'red') +
  geom_vline(xintercept = 11, linetype = 'dashed', color = 'red') +
  geom_vline(xintercept = 521, linetype = 'dashed', color = 	'red') +
  geom_text(label = "95% LCL", color = 'red', x = log10(13), y = 0.07)+
  geom_text(label = "5% hazard concentration", color = 'red', x = log10(105), y = 0.07)+
  geom_text(label = "95% UCL", color = 'red', x = log10(440), y = 0.07)+
  ylab("Cumulative Density") +
  xlab("Particles/L (1-5,000 um)")+
  scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(trans = "log10") +
  annotation_logticks(sides = "b") +  #log scale rick marks on bottom
  labs(title = "SFEI Concentrations ECDF by Season",
       subtitle = "Particles/L corrected to 1-5,000 um",
       caption = "Hazard Concentration from Koelmans et al (2020)")

#white mode
ECDF.Season_SFEI_white <- ECDF.Season +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 14))
ECDF.Season_SFEI_white

#save
ggsave(ECDF.Season_SFEI_white, filename = "ECDF.Season_SFEI_white.png", path = "Concentration data/plots",
       width = 8, scale = 2, dpi = 500)
#dark mode
ECDF.Season_SFEI_dark <- ECDF.Season +
  dark_theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 14))
ECDF.Season_SFEI_dark
#save
ggsave(ECDF.Season_SFEI_dark, filename = "ECDF.Season_SFEI_dark.png", path = "Concentration data/plots",
       height = 4, width = 7, scale = 2, dpi = 320)

#ECDF by sample type
ECDF.SampleType <- SFEI %>% 
  ggplot(aes(x = Particles.L_Corrected, color = Sample.Type))+
  stat_ecdf(geom = "point", size = 2) +
  stat_ecdf(geom = "step", linetype = 'solid', alpha = 0.6, size = 1.5) +
  scale_color_manual(values = wes_palette("FantasticFox1"))+
  geom_vline(xintercept = 75.6, linetype = 'dashed', color = 'red') +
  geom_vline(xintercept = 11, linetype = 'dashed', color = 'red') +
  geom_vline(xintercept = 521, linetype = 'dashed', color = 	'red') +
  geom_text(label = "95% LCL", color = 'red', x = log10(13), y = 0.07)+
  geom_text(label = "5% hazard concentration", color = 'red', x = log10(105), y = 0.07)+
  geom_text(label = "95% UCL", color = 'red', x = log10(440), y = 0.07)+
  ylab("Cumulative Density") +
  xlab("Particles/L (1-5,000 um)")+
  scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(trans = "log10") +
  annotation_logticks(sides = "b")+ #log scale rick marks on bottom
  labs(title = "SFEI Concentrations ECDF by Sample Type",
       subtitle = "Particles/L corrected to 1-5,000 um",
       caption = "Hazard Concentration from Koelmans et al (2020)")

ECDF.SampleType_white<- ECDF.SampleType +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 14))
ECDF.SampleType_white
#save
ggsave(ECDF.SampleType_white, filename = "ECDF.SampleType_white.png", path = "Concentration data/plots",
       height = 4, width = 7, scale = 2, dpi = 500)

#dark
ECDF.SampleType_dark<- ECDF.SampleType +
  dark_theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 14))
ECDF.SampleType_dark
#save
ggsave(ECDF.SampleType_dark, filename = "ECDF.SampleType_dark.png", path = "Concentration data/plots",
       height = 4, width = 7, units = "in", scale = 2, dpi = 320)

#ECDF by location
ECDF.Location <- SFEI %>% 
  filter(Location != "NA") %>% 
  ggplot(aes(x = Particles.L_Corrected, color = Location))+
  stat_ecdf(geom = "point", size = 2) +
  stat_ecdf(geom = "step", linetype = 'solid', alpha = 0.6, size = 1.5) +
  scale_color_manual(values = wes_palette("Cavalcanti1"))+
  geom_vline(xintercept = 75.6, linetype = 'dashed', color = 'red') +
  geom_vline(xintercept = 11, linetype = 'dashed', color = 'red') +
  geom_vline(xintercept = 521, linetype = 'dashed', color = 	'red') +
  geom_text(label = "95% LCL", color = 'red', x = log10(13), y = 0.07)+
  geom_text(label = "5% hazard concentration", color = 'red', x = log10(105), y = 0.07)+
  geom_text(label = "95% UCL", color = 'red', x = log10(440), y = 0.07)+
  ylab("Cumulative Density") +
  xlab("Particles/L (1-5,000 um)")+
  scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(trans = "log10") +
  annotation_logticks(sides = "b")+ #log scale rick marks on bottom
  labs(title = "SFEI Concentrations ECDF by Location",
       subtitle = "Particles/L corrected to 1-5,000 um",
       caption = "Hazard Concentration from Koelmans et al (2020)")

ECDF.Location_white <- ECDF.Location +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 14))
ECDF.Location_white
#save
ggsave(ECDF.Location_white, filename = "ECDF.Location_white.png", path = "Concentration data/plots",
       width = 8, scale = 2, dpi = 500)

#dark
ECDF.Location_dark<- ECDF.Location +
  dark_theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 14))
ECDF.Location_dark
#save
ggsave(ECDF.Location_dark, filename = "ECDF.Location_dark.png", path = "Concentration data/plots",
       height = 4, width = 7, scale = 2, dpi = 320)

ECDF.Location.Type.Season <- grid.arrange(ECDF.SampleType, ECDF.Season, ECDF.Location,
                                       ncol = 2,
                                       top = textGrob("Occurrence Cumulative Distributions by Sample Type, Season, and Location", gp=gpar(fontsize = 22, font=6)))
ggsave(ECDF.Location.Type.Season,
       filename = "ECDF.Location.Type.Season.png",
       path = "Concentration data/plots",
       width = 8,
       scale = 2,
       dpi = 500)

#modelling
sample_dists <- ssd_fit_dists(samplesSFEI, #data frame
                           left = "Conc", #string of the column in data with the concentrations
                           # right = left, #string of the column with the right concentration values. If different from left, then the data are considerd to be censored
                           dists = c("weibull", "llogis", "lnorm", "gamma", "lgumbel"), #char vector of distribution anmes
                           computable = FALSE, #flag specifying whether to only return fits with numerically computable standard errors
                           silent = FALSE) #flag indicating whether fits should fail silently

autoplot(sample_dists) #plots the distribution in ggplot2
sample_gof<- as.data.frame(ssd_gof(sample_dists)) %>% mutate_if(is.numeric, ~ signif(., 3))

datatable(sample_gof,
          extensions = 'Buttons',
          options = list(
            dom = 'Brt', #buttons, processing display element, table
            buttons = c('copy', 'csv', 'excel')),
          class = "compact",
          colnames = c("Distribution", "Anderson-Darling","Kolmogorv Smirnov", "Cramer-Von Mises", "Akaike's Information Criteria", "Akaike's Information Criteria (Corrected for sample size)", "Bayesian Information Criteria", "delta", "weight"),
          caption = "Distributions and their according fit paramaters are displayed",
          selection = list(c(6), target = 'column'))


#there are multiple fitting distributions, so check which fits best
sample_gof <- ssd_gof(sample_dists)
sample_gof[order(sample_gof$delta), ] #orders by delta. Use the aicc (Akaike's Information Criterion corrected for sample size) for model selection 
write.csv(sample_gof,"Concentration data/sample_gof.csv")
set.seed(99)
sample_pred <- predict(sample_dists,
                                average = TRUE,
                                ic = "aicc",
                                nboot = 10,
                                ci= TRUE) #estimates model-averaged estimates based on aicc

sample_pred # The resultant object is a data frame of the estimated concentration (est) with standard error (se) and lower (lcl) and upper (ucl) 95% confidence limits by percent of species affected (percent). The confidence limits are estimated using parametric bootstrapping.

sample_pred %>% mutate_if(is.numeric, ~ signif(., 3)) %>% 
  datatable(rownames = FALSE,
            extensions = c('Buttons', 'Scroller'),
            options = list(
              dom = 'Brftp',
              scrollY = 400,
              scroller = TRUE,
              buttons = c('copy', 'csv', 'excel')), 
            class = "compact",
            colnames = c("Percent", "Estimated Mean Concentration", "Standard Error", "Lower 95% Confidence Limit", "Upper 95% Confidence Limit", "Distribution"),
            caption = "Predicted Concentration distribution with uncertanties."
  )

#order data
sampleSSD <- samplesSFEI[order(samplesSFEI$Conc), ]
sampleSSD$frac <- ppoints(samplesSFEI$Conc, 0.5)

aoc_hc5 <- c(75.6)

ECDF_model_occurrence <- ggplot(sample_pred,aes_string(x = "est")) +
  geom_xribbon(aes_string(xmin = "lcl", xmax = "ucl", y = "percent/100"), alpha = 0.2, color = "#81a88d", fill = "#81a88d") +
  geom_line(aes_string(y = "percent/100"), linetype = 'dashed', alpha = 0.8) +
  geom_point(data = sampleSSD,aes(x = Conc, y =frac, color = Season, shape = Location), size =4) + 
  #geom_text(data = sampleSSD, aes(x = Conc, y = frac, label = Location), hjust = 1.1, size = 4) + #season labels
  scale_y_continuous("Cumulative Distribution (%)", labels = scales::percent) +
  #expand_limits(y = c(0, 1)) +
  xlab("Concentration (particles/L)")+
  labs(title = "SF Bay 2019 Microplastics Concentration Cumulative Distribution Function",
       subtitle = "Smoothing/95% CI ribbon based on average of log-logical and log-normal Distributions Fit",
       caption = "SFEI 2019 data; sampling corrected to 1-5,000 um") +
  coord_trans(x = "log10") +
  scale_x_continuous(breaks = scales::trans_breaks("log10", function(x) 10^x),labels = comma_signif)+
  scale_color_manual(values = wes_palette("Darjeeling2"))
 
  #white mode
ECDF_model_occurrence_white <- ECDF_model_occurrence +
  geom_vline(xintercept = 75.6, linetype = 'dashed', color = 'red') +
  geom_vline(xintercept = 11, linetype = 'dashed', color = 'red') +
  geom_vline(xintercept = 521, linetype = 'dashed', color = 	'red') +
  geom_text(label = "5% HC: 95% LCL", color = 'red', x = 15, y = 0)+
  geom_text(label = "5% hazard concentration", color = 'red', x = 110, y = 0.03)+
  geom_text(label = "5% HC: 95% UCL", color = 'red', x = 400, y = 0)+
  geom_text(x = 110, y = 0, label = "75.6 particles/L", color = 'red') +  #label for hazard conc
  geom_hline(yintercept = 0.925, linetype = 'twodash', color = "#A2A475") +
  geom_text(label = "92.5% samples below 5% HC Mean", x = 4.5, y = 0.94, color = "#A2A475") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))
ECDF_model_occurrence_white

ggsave(ECDF_model_occurrence_white,
       filename = "ECDF_model_occurrence_white.png",
       path = "Concentration data/plots",
       width = 8,
       scale = 2,
       dpi = 500)

#dark mode
ECDF_model_occurrence_dark <- ECDF_model_occurrence +
  geom_vline(xintercept = 75.6, linetype = 'dashed', color = 'red') +
  geom_vline(xintercept = 11, linetype = 'dashed', color = 'red') +
  geom_vline(xintercept = 521, linetype = 'dashed', color = 	'red') +
  geom_text(label = "5% HC: 95% LCL", color = 'red', x = 17, y = 0, size = 6)+
  geom_text(label = "5% hazard concentration", color = 'red', x = 130, y = 0.1, size = 6)+
  geom_text(label = "5% HC: 95% UCL", color = 'red', x = 350, y = 0.05, size = 6)+
  geom_text(x = 120, y = 0.05, label = "75.6 particles/L", color = 'red', size = 6) +  #label for hazard conc
  geom_hline(yintercept = 0.925, linetype = 'twodash', color = "yellow") +
  geom_text(label = "92.5%", x = 3.0, y = 0.96, color = "yellow", size = 6) +
  dark_theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))
ECDF_model_occurrence_dark

ggsave(ECDF_model_occurrence_dark,
       filename = "ECDF_model_occurrence_dark.png",
       path = "Concentration data/plots",
       width = 7,
       height = 4,
       scale = 2,
       units = 'in',
       dpi = 320)

#### SFEI Binomial Test ####
#Use median HC5 (75.6 particles/L) and 5% CI (11 particles/L)
SFEI_Summary <- data.frame(SFEI %>% 
                             filter(Sample.Type == "sample") %>% 
                             group_by(Location) %>% 
                             summarize(total = n(),
                             n_gtHC5 = sum(Particles.L_Corrected > 75.6),
                             p_gtHC5 = 100 * (n_gtHC5 / total), #Percentage above median HC5
                             n_gtHC5_5pCI = sum(Particles.L_Corrected > 11),
                             p_gtHC5_5pCI = 100 * (n_gtHC5_5pCI / total))) #percentage above 5% CI HC5
#3 samples above, 49 below,  5.77% above HC5

## Table 3.1 of the 'Listing Policy'  https://www.waterboards.ca.gov/water_issues/programs/tmdl/303d_listing.html
# Null Hypothesis: ACtual exceedance proportion <= 3 %
# Alternative hypothesis: ACtual exceedance proportion > 18 percent
# minimum effect size is 15%
# *Application of the binomial test requires a minimum sample size of 16. The number of exceedances required using the binomial test at a sample size of 16 is extended to smaller sample sizes.	
# Table 3.1 states that if the sample size is between 48-59, List if the number of exceedances are equal to or greater than 5

SFEI_binom <- SFEI_Summary %>% 
  group_by(Location) %>% 
  do(tidy(binom.test(.$n_gtHC5, .$total, alternative = "two.sided", p =0.03)))

#join to make final table
SFEI_binom_table <- left_join(SFEI_Summary, SFEI_binom, by = "Location") %>% 
  select(-c(parameter, method, alternative, statistic, estimate)) %>% 
  mutate_if(is.numeric, round,3) %>% 
  rename(Num.Samples = total,
         Num.Exceedances.HC5 = n_gtHC5,
         Percent.Exceedances.HC5 = p_gtHC5,
         Num.Exceedances.HC5.5.Percent.CI = n_gtHC5_5pCI,
         Percent.Exceedances.HC5.5.Percent.CI = p_gtHC5_5pCI)
write.csv(SFEI_binom_table, "Concentration data/SFEI_binom_table.csv")
# Total
binom.test(3, 3 + 48, p =0.03)

# Bay
Bay.Binom <- binom.test(1, 1 + 30, p =0.03) 

#Sanctuary
binom.test(2, 2 + 20, p =0.04, "g") 

#### Plot Adam Data ####

# read in concentration data
samplesADAM <- adam %>% 
  mutate(Conc = particle.L.master)

#make new dataframe to plot both histograms together
sampleSimpleADAM <- samplesADAM %>%
  select(Conc, Sample.Type) %>% 
  droplevels()

#make new dataframe to plot both histograms together
dfADAM <- rbind(sampleSimpleADAM,food.dilution.simple)


#histogram of both tox and concentrations 
hist.tox.occurrence_ADAM <- dfADAM %>% 
  filter(Conc < 1000000000) %>% 
  ggplot(aes(x = Conc, fill = Sample.Type, color = Sample.Type))+
  geom_histogram(aes(x = Conc, y=..density..), bins = 20, alpha = 0.6,position = 'identity') +
  geom_smooth(stat = 'density') +
  scale_x_log10() +
  #coord_cartesian(xlim = c(0,100000000)) +
  # scale_x_continuous(labels = scales::scientific) +
  xlab("Concentration (particles/L)")+
  scale_y_continuous(name = "Relative Density", labels = scales::percent)+
  scale_fill_discrete(labels = c("Environmental Concentration", "LOEC")) +
  scale_color_discrete(labels = c("Environmental Concentration", "LOEC")) +
  labs(title = "Histograms of Concentrations in Adam et al 2019 Dataset vs. Food Dilution LOECs",
       caption = "Adam et al. 2019 data; all data corrected to 1-5,000 um; nominal particle/L; SCCWRP tox dataset",
       fill = "Env. Conc. or Tox. Conc.",
       color = "Env. Conc. or Tox. Conc.") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.title = element_text(size = 16),
        axis.text =  element_text(size = 16),
        legend.text = element_text(size =14),
        legend.title = element_blank())
#display
hist.tox.occurrence_ADAM
#save
ggsave(hist.tox.occurrence_ADAM,
       filename = "Histogram_tox_occurrence_ADAM.png",
       path = "Concentration data/plots",
       width = 8,
       scale = 2,
       dpi = 500)


#Dark mode
hist.tox.occurrence_ADAM_dark <- hist.tox.occurrence_ADAM +
  dark_mode()+
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.title = element_text(size = 16),
        axis.text =  element_text(size = 16),
        legend.text = element_text(size =14),
        legend.title = element_blank())
hist.tox.occurrence_ADAM_dark

#save
ggsave(hist.tox.occurrence_ADAM_dark, filename = "hist.tox.occurrence_ADAM_dark.png", path = "Concentration data/plots",
       height = 4, width = 7, scale = 2, dpi = 320)


#ECDF by System
ECDF.System.ADAM <- adam %>% 
filter(System != "") %>% 
  ggplot(aes(x = particle.L.master, color = System))+
  stat_ecdf(geom = "point", size = 2) +
  stat_ecdf(geom = "step", linetype = 'solid', alpha = 0.6, size = 1.5) +
  scale_color_manual(values = wes_palette("Darjeeling1"))+
  geom_vline(xintercept = 75.6, linetype = 'dashed', color = 'red') +
  geom_vline(xintercept = 11, linetype = 'dashed', color = 'red') +
  geom_vline(xintercept = 521, linetype = 'dashed', color = 	'red') +
  geom_text(label = "95% LCL", color = 'red', x = log10(13), y = 0.07)+
  geom_text(label = "5% hazard concentration", color = 'red', x = log10(105), y = 0.07)+
  geom_text(label = "95% UCL", color = 'red', x = log10(440), y = 0.07)+
  ylab("Cumulative Density") +
  xlab("Particles/L (1-5,000 um)")+
  scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(trans = "log10") +
  annotation_logticks(sides = "b")+ #log scale rick marks on bottom
  theme_minimal() +
  labs(title = "Global Concentrations ECDF by System",
       subtitle = "Particles/L corrected to 1-5,000 um",
       caption = "Concentration data from Adams et al (2019); corrected for size via Koelmans. Hazard Concentration from Koelmans et al (2020)")+
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 14))
ECDF.System.ADAM

ggsave(ECDF.System.ADAM,
       filename = "ECDF.System.ADAM.png",
       path = "Concentration data/plots",
       width = 8,
       scale = 2,
       dpi = 500)


#Dark mode
ECDF.System.ADAM_dark <- ECDF.System.ADAM +
  dark_mode()+
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.title = element_text(size = 16),
        axis.text =  element_text(size = 16),
        legend.text = element_text(size =14),
        legend.title = element_blank())
ECDF.System.ADAM_dark

#save
ggsave(ECDF.System.ADAM_dark, filename = "ECDF.System.ADAM_dark.png", path = "Concentration data/plots",
       height = 4, width = 7, scale = 2, dpi = 320)



#modelling
sample_dists_ADAM <- ssd_fit_dists(samplesADAM, #data frame
                              left = "Conc", #string of the column in data with the concentrations
                              # right = left, #string of the column with the right concentration values. If different from left, then the data are considerd to be censored
                              dists = c("weibull", "llogis", "lnorm", "gamma", "lgumbel"), #char vector of distribution anmes
                              computable = FALSE, #flag specifying whether to only return fits with numerically computable standard errors
                              silent = FALSE) #flag indicating whether fits should fail silently

autoplotADAM<- autoplot(sample_dists_ADAM) #plots the distribution in ggplot2
autoplotADAM
ssd_gof(sample_dists_ADAM) #check the goodness of fit
#there are multiple fitting distributions, so check which fits best
sample_gof_ADAM <- ssd_gof(sample_dists_ADAM)
sample_gof_ADAM[order(sample_gof_ADAM$delta), ] #orders by delta. Use the aicc (Akaike's Information Criterion corrected for sample size) for model selection 
write.csv(sample_gof_ADAM,"Concentration data/sample_gof_ADAM.csv")
#choose the distribution that you want to plot
sample_dists_ADAM_choice <- ssd_fit_dists(samplesADAM, #data frame
                                   left = "Conc", #string of the column in data with the concentrations
                                   # right = left, #string of the column with the right concentration values. If different from left, then the data are considerd to be censored
                                   dists = c("lgumbel"), #char vector of distribution anmes
                                   computable = FALSE, #flag specifying whether to only return fits with numerically computable standard errors
                                   silent = FALSE) #flag indicating whether fits should fail silently
set.seed(99)
sample_pred_ADAM <- predict(sample_dists_ADAM_choice,
                       average = FALSE,
                       ic = "aicc",
                       nboot = 10,
                       ci= TRUE) #estimates model-averaged estimates based on aicc

sample_pred_ADAM # The resultant object is a data frame of the estimated concentration (est) with standard error (se) and lower (lcl) and upper (ucl) 95% confidence limits by percent of species affected (percent). The confidence limits are estimated using parametric bootstrapping.

sample_pred_ADAM %>% mutate_if(is.numeric, ~ signif(., 3)) %>% 
  datatable(rownames = FALSE,
            extensions = c('Buttons', 'Scroller'),
            options = list(
              dom = 'Brftp',
              scrollY = 400,
              scroller = TRUE,
              buttons = c('copy', 'csv', 'excel')), 
            class = "compact",
            colnames = c("Percent", "Estimated Mean Concentration", "Standard Error", "Lower 95% Confidence Limit", "Upper 95% Confidence Limit", "Distribution"),
            caption = "Predicted Concentration distribution with uncertanties."
  )

#order data
samplesADAM <- samplesADAM %>% 
  filter(System != "") #take out blanks

sampleSSDADAM <- samplesADAM[order(samplesADAM$Conc), ]
sampleSSDADAM$frac <- ppoints(samplesADAM$Conc, 0.5)

aoc_hc5 <- c(75.6) #hazard concentration manually from Koelmans

ECDF_model_occurrence_ADAM <- ggplot(sample_pred_ADAM,aes_string(x = "est")) +
  geom_xribbon(aes_string(xmin = "lcl", xmax = "ucl", y = "percent/100"), alpha = 0.2, color = "#81a88d", fill = "#81a88d") +
  geom_line(aes_string(y = "percent/100"), linetype = 'dashed', alpha = 0.8) +
  geom_point(data = sampleSSDADAM,aes(x = Conc, y =frac, color = System), size =1) + 
  #geom_text(data = sampleSSD, aes(x = Conc, y = frac, label = Location), hjust = 1.1, size = 4) + #season labels
  scale_y_continuous("Cumulative Distribution (%)", labels = scales::percent) +
  #expand_limits(y = c(0, 1)) +
  xlab("Concentration (particles/L)")+
  labs(title = "Adam et al 2019 Microplastics Concentration Cumulative Distribution Function",
       subtitle = "Smoothing/95% CI ribbon based on average of log-logical and log-normal Distributions Fit",
       caption = "Adam et al 2019 data; sampling corrected to 1-5,000 um") +
  coord_trans(x = "log10") +
  scale_x_continuous(breaks = scales::trans_breaks("log10", function(x) 10^x),labels = comma_signif)+
  scale_color_manual(values = wes_palette("Darjeeling2"))

#white mode
ECDF_model_occurrence_ADAM_white <- ECDF_model_occurrence_ADAM +
  geom_vline(xintercept = 75.6, linetype = 'dashed', color = 'red') +
  geom_vline(xintercept = 11, linetype = 'dashed', color = 'red') +
  geom_vline(xintercept = 521, linetype = 'dashed', color = 	'red') +
  geom_text(label = "5% HC: 95% LCL", color = 'red', x = 15, y = 0)+
  geom_text(label = "5% hazard concentration", color = 'red', x = 110, y = 0.03)+
  geom_text(label = "5% HC: 95% UCL", color = 'red', x = 400, y = 0)+
  geom_text(x = 110, y = 0, label = "75.6 particles/L", color = 'red') +  #label for hazard conc
  geom_hline(yintercept = 0.925, linetype = 'twodash', color = "#A2A475") +
  #geom_text(label = "92.5% samples below 5% HC Mean", x = 4.5, y = 0.94, color = "#A2A475") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))
ECDF_model_occurrence_ADAM_white

ggsave(ECDF_model_occurrence_ADAM_white,
       filename = "ECDF_model_occurrence_ADAM_white.png",
       path = "Concentration data/plots",
       width = 8,
       scale = 2,
       dpi = 500)

#dark mode
ECDF_model_occurrence_ADAM_dark <- ECDF_model_occurrence_ADAM +
  geom_vline(xintercept = 75.6, linetype = 'dashed', color = 'red') +
  geom_vline(xintercept = 11, linetype = 'dashed', color = 'red') +
  geom_vline(xintercept = 521, linetype = 'dashed', color = 	'red') +
  geom_text(label = "95% LCL", color = 'red', x = 2, y = 0, size = 5)+
  geom_text(label = "5% HC", color = 'red', x = 80, y = 0, size = 5)+
  geom_text(label = "95% UCL", color = 'red', x = 3500, y = 0, size = 5)+
  geom_text(x = 120, y = 0.1, label = "75.6 particles/L", color = 'red', size =5) +  #label for hazard conc
  #geom_hline(yintercept = 0.925, linetype = 'twodash', color = "yellow") +
  #geom_text(label = "92.5%", x = 3.0, y = 0.96, color = "yellow", size = 6) +
  dark_theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))
ECDF_model_occurrence_ADAM_dark


#save
ggsave(ECDF_model_occurrence_ADAM_dark, filename = "ECDF_model_occurrence_ADAM_dark.png", path = "Concentration data/plots",
       height = 4, width = 7, scale = 2, dpi = 320)

