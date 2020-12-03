## Concentration Data
library(tidyverse)
library(calecopal)
library(ssdtools)
library(DT)
library(plotly)
library(fastDummies)

SFEI <- read.csv("SFEI.csv", stringsAsFactors = TRUE)

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
  mutate(env_f = factor(environment.noNA, levels = c("Marine", "Freshwater", "Terrestrial", "Not Reported"))) %>% # order our different environments.
  drop_na(dose.mg.L.master)  #must drop NAs or else nothing will work 

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
samples <- SFEI %>% 
  filter(Sample.Type == "sample") %>% 
  mutate(Conc = Particles.L_Corrected)

#make new dataframe to plot both histograms together
sampleSimple <- samples %>%
  select(Conc, Sample.Type) %>% 
  filter(Sample.Type == "sample") %>% 
  droplevels()


food.dilution.simple <- food.dilution %>% 
  select(Conc)

#write.csv(food.dilution.simple, "foodDilutionSimple.csv") # can't figure out how to make a new column with tox fore every row, so doing it in excell
food.dilution.simple <- read.csv("foodDilutionSimple.csv")

df <- rbind(sampleSimple,food.dilution.simple)

#histogram of samples
samples %>% 
  ggplot(aes(x = Particles.L_Corrected))+
  geom_histogram(aes(x = Particles.L_Corrected, y=..density..), bins = 12) +
  geom_smooth(stat = 'density') +
  scale_x_log10()

# histogram of SSD (parrticle/L)
 food.dilution %>% 
  #filter(particle.L < 300) %>% 
  ggplot(aes(x = particle.L))+
  geom_histogram(bins = 15) +
  scale_x_log10()
 
 # histogram of SSD (mg/L)
 food.dilution %>% 
   filter(dose.mg.L.master< 10000) %>% 
   #filter(particle.L < 300) %>% 
   ggplot(aes(x = dose.mg.L.master))+
   geom_histogram(aes(x = dose.mg.L.master, y=..density..), fill = "blue", bins = 12, alpha = 0.5) +
   geom_smooth(stat = 'density', color = "blue") +
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
 

#histogram of both tox and concentrations
df %>% 
  filter(Conc < 100000000) %>% 
  ggplot(aes(x = Conc, fill = Sample.Type, color = Sample.Type))+
  geom_histogram(aes(x = Conc, y=..density..), bins = 30, alpha = 0.6) +
  geom_smooth(stat = 'density') +
  scale_x_log10() +
  coord_cartesian(xlim = c(1,100000000)) +
 # scale_x_continuous(labels = scales::scientific) +
  xlab("Concentration (particles/L)")+
  scale_y_continuous(name = "Relative Density", labels = scales::percent)+
  scale_fill_discrete(labels = c("Environmental Concentration", "LOEC")) +
  scale_color_discrete(labels = c("Environmental Concentration", "LOEC")) +
  labs(title = "Histograms of Concentrations in SFEI Dataset vs. Food Dilution LOECs",
       caption = "SFEI 2017 data; all data corrected to 1-5,000 um; nominal particle/L; SCCWRP tox dataset",
       fill = "Env. Conc. or Tox. Conc.",
       color = "Env. Conc. or Tox. Conc.") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.title = element_text(size = 16),
        axis.text =  element_text(size = 16),
        legend.text = element_text(size =14),
        legend.title = element_blank())


#ECDF by season
samples %>% 
  ggplot(aes(x = Particles.L_Corrected, color = Season))+
  stat_ecdf(geom = "point") +
  stat_ecdf(geom = "step", linetype = 'dashed', alpha = 0.75) +
  geom_vline(xintercept = 75.6, linetype = 'dashed', color = "red") +
  geom_text(label = "5% hazard concentration", color = "red", alpha = 0.75, x = 95, y = 0.10)+
  scale_color_manual(values = cal_palette("superbloom3"))+
  ylab("Cumulative Density") +
  xlab("Particles/L (1-5,000 um)")+
  scale_y_continuous(labels = scales::percent)+
  theme_minimal()

#ECDF by sample type
SFEI %>% 
  ggplot(aes(x = Particles.L_Corrected, color = Sample.Type))+
  stat_ecdf(geom = "point") +
  stat_ecdf(geom = "step", linetype = 'dashed', alpha = 0.75) +
  geom_vline(xintercept = 75.6, linetype = 'dashed', color = "red") +
  geom_text(label = "5% hazard concentration", color = "red", x = 95, y = 0.10)+
  ylab("Cumulative Density") +
  xlab("Particles/L (1-5,000 um)")+
  scale_y_continuous(labels = scales::percent)+
  theme_minimal()

#modelling
sample_dists <- ssd_fit_dists(samples, #data frame
                           left = "Conc", #string of the column in data with the concentrations
                           # right = left, #string of the column with the right concentration values. If different from left, then the data are considerd to be censored
                           dists = c("weibull", "llogis", "lnorm", "gamma", "lgumbel"), #char vector of distribution anmes
                           computable = FALSE, #flag specifying whether to only return fits with numerically computable standard errors
                           silent = FALSE) #flag indicating whether fits should fail silently

autoplot(sample_dists) #plots the distribution in ggplot2
ssd_gof(sample_dists) #check the goodness of fit
#there are multiple fitting distributions, so check which fits best
sample_gof <- ssd_gof(sample_dists)
sample_gof[order(sample_gof$delta), ] #orders by delta. Use the aicc (Akaike's Information Criterion corrected for sample size) for model selection 
write.csv(sample_gof,"sample_gof.csv")
set.seed(99)
sample_pred <- predict(sample_dists,
                                average = TRUE,
                                ic = "aicc",
                                nboot = 10,
                                ci= TRUE) #estimates model-averaged estimates based on aicc

sample_pred # The resultant object is a data frame of the estimated concentration (est) with standard error (se) and lower (lcl) and upper (ucl) 95% confidence limits by percent of species affected (percent). The confidence limits are estimated using parametric bootstrapping.

sample_pred %>% mutate_if(is.numeric, ~ signif(., 3)) %>% 
  dattable(rownames = FALSE,
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
sampleSSD <- samples[order(samples$Conc), ]
sampleSSD$frac <- ppoints(samples$Conc, 0.5)

aoc_hc5 <- c(75.6)

gp2 <- ggplot(sample_pred,aes_string(x = "est")) +
  geom_xribbon(aes_string(xmin = "lcl", xmax = "ucl", y = "percent/100"), alpha = 0.2, color = "lightblue") +
  geom_line(aes_string(y = "percent/100"), linetype = 'dashed', alpha = 0.8) +
  geom_point(data = sampleSSD,aes(x = Conc, y =frac, color = Season, shape = Location), size =4) + 
  #geom_text(data = sampleSSD, aes(x = Conc, y = frac, label = Location), hjust = 1.1, size = 4) + #season labels
  scale_y_continuous("Cumulative Distribution (%)", labels = scales::percent) +
  #expand_limits(y = c(0, 1)) +
  xlab("Concentration (particles/L)")+
  labs(title = "Concentration Cumulative Distribution",
       subtitle = "Average of log-logical and log-normal Distributions Fit",
       caption = "SFEI 2017 data; sampling corrected to 1-5,000 um") +
  coord_trans(x = "log10") +
  scale_x_continuous(breaks = scales::trans_breaks("log10", function(x) 10^x),labels = comma_signif)+
  geom_vline(xintercept = 75.6, color = "red") +
  geom_text(x = 110, y = 0.04, label = "5 % Hazard Confidence Level", color = "red", size = 4) + #label for hazard conc
  geom_text(x = 110, y = 0, label = "75.6 particles/L", color = "red") +  #label for hazard conc
  theme(plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5))

gp2

ggplotly(gp2)
