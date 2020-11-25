#### Aquatic Microplastics Toxicology Shiny App
#### File created: September 23, 2020
#### Code contributors: Heili Lowman, Leah Thornton Hampton, Scott Coffin, Emily Darin

#### Setup ####

# Anything that should only happen ONCE should be placed in this setup section, prior to the actual shiny structure.

# Load packages
library(tidyverse) #General everything
library(RColorBrewer)
library(ggplot2) #General plotting
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

# Load finalized dataset.
aoc <- read_csv("AquaticOrganisms_Clean_final.csv", guess_max = 10000)

#### Leah Setup ####

# All text inputs below.

#### Emily Setup ####

Final_effect_dataset <- read_csv("Final_effect_dataset.csv")%>%
  mutate(plot_f = case_when(
    plot_f == "Polymer" ~ "Polymer",
    plot_f == "Size" ~ "Size",
    plot_f == "Shape" ~ "Shape",
    plot_f == "Organism" ~ "Organism",
    plot_f == "Lvl1" ~ "Endpoint Category",
    plot_f == "Life.stage" ~ "Life Stage",
    plot_f == "Invivo.invivo" ~ "In Vivo or In Vitro",
    plot_f == "Exposure.route" ~ "Exposure Route"))%>%
  mutate(plot_f = factor(plot_f))%>%
  mutate(logEndpoints = log(Endpoints))%>%
  rename(Percent = Freq)

# Adding function for multiple graph output.
# Code adapted from https://gist.github.com/wch/5436415/ and comment at https://gist.github.com/wch/5436415/#gistcomment-1608976 .

# Creates function called "get_plot_output_list" where the input variable is "input_n".
get_plot_output_list <- function(input_n) {
  
  # For every value in "input_n", insert it as "i" into the function below and then save the full output into "plot_output_list":
  plot_output_list <- lapply(input_n, function(i) {
    
    # Render the individual plots      
    renderPlotly({
      
      # use the original dataset
      Final_effect_dataset %>%
        
        # filter by input
        filter(plot_f==i) %>%
        
        # generate plot
        ggplot(aes(fill=effect, y= logEndpoints, x=Type, Percent=Percent)) +
        geom_bar(position="stack", stat="identity") +
        geom_text(aes(label= paste0(Endpoints)), position = position_stack(vjust = 0.5),colour="black") +
        scale_fill_manual(values = cal_palette(case_when(i=="Polymer"~"wetland", i=="Organism"~"sbchannel", i=="Size"~"seagrass",i=="Shape"~"gayophytum",i=="Endpoint Category"~"figmtn",i=="Life Stage"~"dudleya",i=="Exposure Route"~"halfdome",i=="In Vivo or In Vitro"~"kelp2")))+
        theme_classic() +
        ylab("Number of Endpoints Measured") +
        labs(fill="Effect") +
        guides(x = guide_axis(n.dodge = 2)) +
        ggtitle(case_when(i=="Polymer"~"Polymer", i=="Organism"~"Organism", i=="Size"~"Particle Size",i=="Shape"~"Shape",i=="Endpoint Category"~"Endpoint Category",i=="Life Stage"~"Life Stage",i=="Exposure Route"~"Exposure Route",i=="In Vivo or In Vitro"~"In Vivo or In vitro"))+
        theme(plot.title = element_text(hjust = 0.5, face="bold"))+
        theme(legend.position = "right",
          axis.ticks= element_blank(),
          axis.text.x = element_text(angle=45, size = 10),
          axis.text.y = element_blank(),
          axis.title.x = element_blank())
      
      ggplotly(tooltip = 'Percent')%>%
        config(displayModeBar = FALSE)
      
    })
    
  })
  
  do.call(tagList, plot_output_list) # Need to call it as a list to display properly.
  
  return(plot_output_list) # Returns the full list of stored plots.
}

#### Heili Setup ####

# Master dataset for scatterplots - for Heili's tab.
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
  

#### Scott Setup ####

# Master dataset for SSDs
aoc_z <- aoc_setup %>% # start with Heili's altered dataset (no filtration for terrestrial data)
  # environment category data tidying.
  mutate(environment.noNA = replace_na(environment, "Not Reported")) %>% # replaces NA to better relabel.
  mutate(env_f = factor(environment.noNA, levels = c("Marine", "Freshwater", "Terrestrial", "Not Reported"))) %>% # order our different environments.
  drop_na(dose.mg.L.master)  #must drop NAs or else nothing will work 
 
# final cleanup and factoring  
aoc_z$Species <- as.factor(paste(aoc_z$genus,aoc_z$species)) #must make value 'Species" (uppercase)
aoc_z$Group <- as.factor(aoc_z$organism.group) #must make value "Group"
aoc_z$Group <- fct_explicit_na(aoc_z$Group) #makes sure that species get counted even if they're missing a group

# Create Shiny app. Anything in the sections below (user interface & server) should be the reactive/interactive parts of the shiny application.

#### User Interface ####

ui <- fluidPage(theme = shinytheme("flatly"),  
  
  # App title
  titlePanel(h1("Microplastics Toxicity Database")),
  
  # Title panel subtext
  tags$div("This website is only intended for use by invited participants of the Microplastics Health Effects Workshop."),
  
  br(), # line break
  
  # Main panel for displaying outputs
  mainPanel(width = 12,
    
      # Output: set of 6 tabs
      tabsetPanel(type = "tabs",

#### Leah UI ####        
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
                   
                    h3("Can I see the raw data?", align = "center"), #Section 3 
                    
                    p("Workshop participants also have access to the complete, raw database as an .xls file by directly contacting Dr. Leah Thornton Hampton (leahth@sccwrp.org), and are welcome to conduct their own analyses.
                      Users may also download meta data associated with visualizations and analyses in the Exploration and Species Sensitivity Distribution tabs."),
                    
                    h3("Contributors", align = "center"), #Section 4: Contributors list with links to twitter and github
                 
                    p(align = "center", a(href = "https://www.sccwrp.org/about/staff/leah-thornton-hampton/", 'Dr. Leah Thornton Hampton'),", Southern California Coastal Water Research Project ", 
                      tags$a(href="https://twitter.com/DrLeahTH", tags$img(src="twitter.png", width="2%", height="2%")), tags$a(href="https://github.com/leahth", tags$img(src="github.png", width="2%", height="2%"))),
                    p(align = "center", a(href = "https://www.sccwrp.org/about/staff/heili-lowman/", 'Dr. Heili Lowman'),", Southern California Coastal Water Research Project ",
                      tags$a(href="https://twitter.com/heili_lowman", tags$img(src="twitter.png", width="2%", height="2%")), tags$a(href="https://github.com/hlowman", tags$img(src="github.png", width="2%", height="2%"))), 
                    p(align = "center", a(href = "https://agency.calepa.ca.gov/staffdirectory/detail.asp?UID=69294&BDO=7&VW=DET&SL=S", 'Dr. Scott Coffin'),", California State Water Resources Control Board", 
                      tags$a(href="https://twitter.com/DrSCoffin", tags$img(src="twitter.png", width="2%", height="2%")), tags$a(href="https://github.com/ScottCoffin", tags$img(src="github.png", width="2%", height="2%"))),
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
                    p(align = "center", a(href = "https://twitter.com/ChelseaRochman", 'Dr. Chelsea Rochman'),", University of Toronto",
                      tags$a(href="https://twitter.com/MicroplasticLab", tags$img(src="twitter.png", width="2%", height="2%"))),
                    p(align = "center", a(href = "https://www.sccwrp.org/about/staff/alvina-mehinto/", 'Dr. Alvina Mehinto'),", Southern California Coastal Water Research Project"), 
                    p(align = "center", a(href = "https://www.sccwrp.org/about/staff/steve-weisberg/", 'Dr. Steve Weisberg'),", Southern California Coastal Water Research Project"), 
                  
                    #Logos with links to organizations
                    
                  splitLayout(align = "center", 
                  tags$a(href="https://www.waterboards.ca.gov", tags$img(src="waterboard.png", width = "100%", height = "100%")),
                  tags$a(href="https://www.swccrp.org", tags$img(src="sccwrp.png", width = "100%", height = "100%")),
                  tags$a(href="https://www.utoronto.ca", tags$img(src="toronto.png", width = "100%", height = "100%")),
                  tags$a(href="https://www.sfei.org/", tags$img(src="sfei.png", width = "100%", height = "100%"))),
                  
                    br(), 
                    
                    verbatimTextOutput(outputId = "Leah1")),
                
#### Emily UI ####

tabPanel("2: Overview", 
         br(), 
         h3("Microplastics in Aquatic Environments: Overview of Toxicological Effects", align = "center"),
         br(),
         p("Check the boxes below to visualize figures. Each bar displays the total number of measured endpoints within the database. Measured endpoints where a statistically signifcant effect was detected as indicated by 'Y' or where a measurement was made but a significant effect was not detected 'N'."), 
         br(),
         p("Use the drop down menu at the top of the page to visualize different figures. Hover the cursor over each stacked bar to display the number of measured endpoints that are currently included in the database. 
           Click on the legend to select data."),
         br(), 
         p("Detailed descriptions of data categories may be found under the Resources tab."),
         br(),
           
pickerInput(inputId = "Emily_check", # endpoint checklist
            label = "Overview", 
            choices = levels(Final_effect_dataset$plot_f),
            selected = levels(Final_effect_dataset$plot_f), 
            options = list(`actions-box` = TRUE), # option to de/select all
            multiple = TRUE), # allows for multiple inputs
            br(),

uiOutput(outputId= "Emily_plot")),

#### Heili UI ####
                  tabPanel("3: Exploration",
                    shinyjs::useShinyjs(), # requires package for "reset" button, DO NOT DELETE - make sure to add any new widget to the reset_input in the server
                    id = "heili-tab", # adds ID for resetting Heili's tab's filters
                    
                    h3("Microplastics in Aquatic Environments: Exploration of Toxicological Effects", align = "center"),
                    br(), # line break
                    p("Each figure displays a different metric along the y-axis - organism group, broad endpoint category, specific endpoint category, size, shape, and polymer, respectively. All doses are displayed in mass per volume. Doses 
                    were either reported in mass per volume or converted from doses originally presented as particle count per volume."),
                    br(),
                    p("The data displayed in these figures are not filtered for quality and only display data where doses were reported as 
                      mass per volume or were converted from doses reported from counts per volume - other dosing units (e.g., particle mass/kg sediment) 
                      are not displayed but are available in the complete database file."),
                    br(), # line break
                    p("Filter the data: The data may be filtered using the drop-down menus located below. Then, click the 'Update Filters' button to refresh the data displayed according to your selections."),
                    br(), # line break
                    p("Download the data: Click the 'Download Data' button to retrieve the selected dataset as a '.csv' file."),
                    br(), # line break
                    
                    
                    # widget headers
                    column(width=12,
                      
                      column(width = 3,
                        h4("Effects")),
                      
                      column(width = 3,
                        h4("Particle Characteristics")),
                      
                      column(width = 3,
                        h4("Biological Factors"))),
                    
                    # widgets
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
                      pickerInput(inputId = "organism_check", # organismal checklist
                        label = "Organisms:", 
                        choices = levels(aoc_setup$org_f),
                        selected = levels(aoc_setup$org_f),
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
                        multiple = TRUE))), 
                      
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
                        multiple = TRUE))),
                    
                    # New row of widgets
                    column(width=12,
                      
                        column(width = 3),
                      
                        #Slider Widget - commented out for now
                        #column(width = 3,
                        #sliderInput("range", # Allows for max input
                          #label = "Particle Size (µm):", #Labels widget
                          #min = 0, max = 4000, value = 4000)),
                      
                        column(width = 3,
                        pickerInput(inputId = "bio_check", # bio org checklist
                          label = "Level of Biological Organization", 
                          choices = levels(aoc_setup$bio_f),
                          selected = levels(aoc_setup$bio_f),
                          options = list(`actions-box` = TRUE),
                          multiple = TRUE))), 
                      
                    #In vitro/in vivo widget - commented out for now
                       # column(width = 3,
                       # pickerInput(inputId = "vivo_check", 
                       #    label = "In Vitro or In Vivo:", 
                       #    choices = levels(aoc_setup$vivo_f),
                       #    selected = levels(aoc_setup$vivo_f),   
                       #    options = list(`actions-box` = TRUE), 
                       #    multiple = TRUE))
                       
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
                        column(width = 3),
                        column(width = 3,
                          br(),
                          strong(p("To Reset: Click the 'Reset Filters' button above, followed by the 'Update Filters' button to the left.")),
                          br())), 
                    
                    # New row
                    column(width = 12,
                    hr()), # adds divider
                    
                    column(width = 6,
                    plotOutput(outputId = "organism_plot_react"),
                    br()), 
                  
                    column(width = 6,
                    plotOutput(outputId = "lvl_plot_react"),
                    br()), 

                    column(width = 12,
                    
                    column(width = 6,
                    plotOutput(outputId = "lvl2_plot_react"),
                    br()), 
                    
                    column(width = 6,
                    plotOutput(outputId = "size_plot_react"),
                    br())), 
                    
                    column(width = 12,
                  
                    column(width = 6,
                    plotOutput(outputId = "shape_plot_react"),
                    br()), 
                    
                    column(width = 6,
                    plotOutput(outputId = "poly_plot_react"),
                    br()))), 

#### Scott UI ####
                  tabPanel("4: Species Sensitivity Distribution", 
                    br(), # line break
                    h3("Species Sensitivity Distribution", align = "center"),
                    p("Species sensitivity distributions (SSDs) are cumulative probability distributions that estimate the percent of species affected by a given concentration of exposure using Maximum Likelihood and model averaging. A useful metric often used for setting risk-based thresholds is the concentration that affects 5% of the species, and is reffered to as the 5% Hazard Concentration (HC). For more information on SSDs, refer to", a(href = "https://bit.ly/2Hy4q10", 'Posthuma, Suter II, and Traas (2001).')),
                    br(), # line break
                    p("Use the options below to filter the toxicity thresholds dataset. Once complete, hit the 'submit' button"),
                    
                    # widget 1
                    column(width = 12,
                           column(width = 4,
                                  # alternative to fully listed checklists
                                  # requires shinyWidgets package
                                  pickerInput(inputId = "env_check_ssd", # environment checklist
                                              label = "Environment:", 
                                              choices = levels(aoc_z$env_f),
                                              selected = levels(aoc_z$env_f),   
                                              options = list(`actions-box` = TRUE), # option to de/select all
                                              multiple = TRUE)), # allows for multiple inputs
                           # Organism widget
                           column(width = 4,
                                         pickerInput(inputId = "Group_check_ssd", # organism checklist
                                              label = "Organism Groups:", 
                                              choices = levels(aoc_z$Group),
                                              selected = levels(aoc_z$Group),   
                                              options = list(`actions-box` = TRUE), # option to de/select all
                                              multiple = TRUE)), # allows for multiple inputs
                           
                           htmlOutput("SpeciesSelection")), # dependent Species checklist
                           br(),
                           p("Advanced options. Suggest using defaults."),
                           br(),
                    
                           column(width = 12,
                           #Size widget
                           column(width = 4,
                                  pickerInput(inputId = "size_check_ssd", # organism checklist
                                              label = "Sizes:",
                                              choices = levels(aoc_z$size_f),
                                              selected = levels(aoc_z$size_f),
                                              options = list(`actions-box` = TRUE), # option to de/select all
                                              multiple = TRUE)), # allows for multiple inputs
                          
                            #Endpoint widget
                           column(width = 4,
                                  pickerInput(inputId = "lvl1_check_ssd", # organism checklist
                                              label = "Broad Endpoint:",
                                              choices = levels(aoc_z$lvl1_f),
                                              selected = levels(aoc_z$lvl1_f),
                                              options = list(`actions-box` = TRUE), # option to de/select all
                                              multiple = TRUE)), # allows for multiple inputs
                           column(width = 4,
                                  htmlOutput("lvl2Selection")), #specific endpoint based on previous checkbox
                           
                           #Polymer widget
                           column(width = 4,
                                  pickerInput(inputId = "poly_check_ssd", # organism checklist
                                              label = "Polymers:",
                                              choices = levels(aoc_z$poly_f),
                                              selected = levels(aoc_z$poly_f),
                                              options = list(`actions-box` = TRUE), # option to de/select all
                                              multiple = TRUE)),# allows for multiple inputs
                           ),#close out column
                    p("Concentrations may be reported in mass/volume or particle #/volume (or sometimes both). Using methods described in", a(href ="https://pubs.acs.org/doi/10.1021/acs.est.0c02982", "Koelmans et. al (2020)"), " units have been converted."),
                    column(width = 12,
                                  radioButtons(
                                    inputId = "Reported_Converted_rad",
                                    label = "Do you want to use just the reported, just the converted, or all exposure concentrations?",
                                    choices = list("reported", "converted", "all"),
                                    selected = "all")),
                    br(),
                            column(width = 12,
                                  actionButton("SSDgo", "Submit", class = "btn-success"),
                                  align = "center"), # adds action button 
                    # "SSDgo" is the internal name to refer to the button
                    # "Update" is the title that appears on the app
                           
                    br(), 
                    p("Please wait a moment while maximum likelihood estimation is calculated data based on your choices...", align = "center"),
                    br(),

                    
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
                              p("The best fitting model is that with the smallest Information Criteria value. Note that several informaiton criteria are listed. Burnham and Anderson (2002) recommend using Akiak'es Information Criteria (Corrected for sample size) [aicc] for model selection. The model with the smallest aicc is indicated by the smallest delta value in the goodness of fit table. For further information on the advantages of an information theoretic approach in the context of selecting SSDs the reader is referred to Schwarz and Tillmanns (2019)."),
                              br(),
                              p("Following Burnham and Anderson (2002), the aicc is recommended for model selection (for which the lowest value is the best fitting model), and is the default information criteria used to predict confidence intervals (unless otherwise specified below). Options inlcude aicc (Akaike's Information Criteria Corrected for sample size; default), aic (Akaike's Information Criteria), or bic (Bayseian Information Criteria)"),
                              br(),
                              column(width = 12,
                                     pickerInput(inputId = "pred_ic_ssd", # prediction model averaging checklist
                                                 label = "Information Criteria:",
                                                 choices = c("aicc", "aic", "bic"), #tells the model which information criteria to use to select best fit
                                                 selected = "aicc",
                                                 options = list(`actions-box` = FALSE), # option to de/select all
                                                 multiple = FALSE)),
                              br(),
                              p("Understanding that other distributions may fit the data almost as well as the 'best' distribution (as evidenced by delta values <2), it is recommended to average such fits based on the relative aicc weights of the distributions (indicated by the weight column in the goodness of fit table) (Burnham and Anderson 2002). Below, choose whether or not multiple distributions should be averaged (delta <2) or if a single distribution (chosen by lowest information criteria selected above) should be used."),
                              br(),
                              column(width = 12,
                                     pickerInput(inputId = "pred_ave_ssd", # prediction model averaging checklist
                                                 label = "Averaging:",
                                                 choices = c("TRUE", "FALSE"), #tells the model to average or not
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
                              p("Please be patient as maximum likelihood estimations are calculated. If a high number of boostrap simulations are chosen (>100), this may take up to several minutes."),
                              br(),
                              h4("Species Sensitivity Distribution", align = "center"),
                              plotOutput(outputId = "aoc_ssd_ggplot", width = "160%", height = "500px", hover = hoverOpts(id = "plot_hover")),
                              verbatimTextOutput("info"),
                              br(),
                              column(width = 12,
                                     downloadButton("downloadSsdPlot", "Download Plot", class = "btn-info"), #download ssdplot
                                     align = "center"),
                              br(),
                              p("The model-averaged 95% confidence interval is indicated by the shaded band and the model-averaged Hazard Concentration (user input value) by the dotted line."),
                              br(),
                              p("Model predictions can also be viewed in tabular format."),
                              br(),
                              h4("SSD Table", align = "center"),
                              DT::dataTableOutput(outputId = "ssd_pred_table"),
                              br(),
                              h4(align = "center", "Credits"),
                              p(align = "center", style = "font-size: 12px;", "This app is built using the R package ", a(href = "https://github.com/bcgov/ssdtools", 'ssdtools', .noWS = "outside"), " version 0.3.2 and share the same functionality."),
                              p(align = "center", style = "font-size: 12px;", "Citation: Thorley, J. and Schwarz C., (2018). ssdtools An R package to fit species Sensitivity Distributions. Journal of Open Source Software, 3(31), 1082. https://doi.org/10.21105/joss.01082."),
                          ) #closes out scott's main panel
                    ), #closes out Scott's tab panel
#### Resources UI ####

tabPanel("5: Resources", 
         br(),
         p("Use the links below to view resource files. For access to the complete database (.xls file), please contact Dr. Leah Thornton Hampton directly (leahth@sccwrp.org)"),
         br(),     
         h3(align = "center", a(href = "https://sccwrp-my.sharepoint.com/:b:/g/personal/leahth_sccwrp_org/Eb8XXdAvn9BBpOB6Z6klzEcBlb6mFpJcYJrHBAQk7r1z3A?e=tRTqDM", 'Data Category Descriptions')),
         br(),
         h3(align = "center", a(href = "https://sccwrp-my.sharepoint.com/:b:/g/personal/leahth_sccwrp_org/EXDS25x3JAJHhZAj3qDwWgIBeB-oz0mIihclR2oOckPjhg?e=GtOeB5", 'Aquatic Organisms Study List')),
         br(),
         #h3(align = "center", a(href = "https://sccwrp-my.sharepoint.com/:b:/g/personal/leahth_sccwrp_org/ES_FUiwiELtNpWgrPCS1Iw4Bkn3-aeiDjZxmtMLjg3uv3g?e=bmuNgG", 'Human Study List')),
         
         verbatimTextOutput(outputId = "Leah2")),

#### Contact UI ####

tabPanel("6: Contact", 
         br(),
         h4("For scientific questions or access to the complete database, please contact Dr. Leah Thornton Hampton (leahth@sccwrp.org)."),
         br(),
         h4("If you encounter technical problems with the web application, please contact Emily Darin (Emily.Darin@student.csulb.edu)."),
         
         verbatimTextOutput(outputId = "Leah3"))

#following three parentheses close out UI. Do not delete. 
        )))   
     

#### Server ####
server <- function(input, output) {

#### Leah S ####

  # Leah does not have any reactive features.
  
#### Emily S ####
  
  # Effect plot code for check box 
  
  # Insert the right number of plot output objects into the page using the function from the setup section.
  output$Emily_plot <- renderUI({ 
    
    # Using user-provided selections.
    get_plot_output_list(input$Emily_check) 
    
    })
  
#### Heili S ####
  
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
    range_n <- input$range # assign values to "range_n"
    
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
      filter(env_f %in% env_c) #%>% #filter by environment
      #filter(size.length.um.used.for.conversions <= range_n) #For size slider widget - currently commented out
      
  })
     
  
  brewer.pal(n = 9, name = "Oranges")
  
  
  # Use newly created dataset from above to generate plots for size, shape, polymer, and endpoint plots on four different rows.
  
  #Organism plot
  
  output$organism_plot_react <- renderPlot({
    
    ggplot(aoc_filter(), aes(x = dose.mg.L.master, y = org_f)) +
      scale_x_log10(breaks = c(0.00000001, 0.000001, 0.0001, 0.01, 1, 100, 10000, 1000000), 
                    labels = c(0.00000001, 0.000001, 0.0001, 0.01, 1, 100, 10000, 1000000)) +
      geom_boxplot(alpha = 0.7, aes(color = effect_f, fill = effect_f)) +
      scale_color_manual(values = c("#FD8D3C", "#7F2704")) +
      scale_fill_manual(values = c("#FD8D3C", "#7F2704")) +
      theme_classic() +
      theme(text = element_text(size=18), 
            legend.position = "right") +
      labs(x = "Concentration (mg/L)",
           y = "Organism",
           color = "Effect?",
           fill = "Effect?")
    
  })
  
  
  # Size Plot
  
  output$size_plot_react <- renderPlot({

    ggplot(aoc_filter(), aes(x = dose.mg.L.master, y = size_f)) +
      geom_boxplot(alpha = 0.7, aes(color = effect_f, fill = effect_f)) +
      scale_x_log10(breaks = c(0.00000001, 0.000001, 0.0001, 0.01, 1, 100, 10000, 1000000), 
        labels = c(0.00000001, 0.000001, 0.0001, 0.01, 1, 100, 10000, 1000000)) +
      scale_color_manual(values = c("#A1CAF6", "#4C6FA1")) +
      scale_fill_manual(values = c("#A1CAF6", "#4C6FA1")) +
      theme_classic() +
      theme(text = element_text(size=18), 
        legend.position = "right") +
      labs(x = "Concentration (mg/L)",
        y = "Size",
        color = "Effect?",
        fill = "Effect?")

  })
  
  # Shape Plot
  
  output$shape_plot_react <- renderPlot({
    
    ggplot(aoc_filter(), aes(x = dose.mg.L.master, y = shape_f)) +
      scale_x_log10(breaks = c(0.00000001, 0.000001, 0.0001, 0.01, 1, 100, 10000, 1000000), 
        labels = c(0.00000001, 0.000001, 0.0001, 0.01, 1, 100, 10000, 1000000)) +
      geom_boxplot(alpha = 0.7, aes(color = effect_f, fill = effect_f)) +
      scale_color_manual(values = c("#C7EAE5","#35978F")) +
      scale_fill_manual(values = c("#C7EAE5", "#35978F")) +
      theme_classic() +
      theme(text = element_text(size=18), 
        legend.position = "right") +
      labs(x = "Concentration (mg/L)",
        y = "Shape",
        color = "Effect?",
        fill = "Effect?")
    
  })
  
  # Polymer Plot
  
  output$poly_plot_react <- renderPlot({
    
    ggplot(aoc_filter(), aes(x = dose.mg.L.master, y = poly_f)) +
      scale_x_log10(breaks = c(0.00000001, 0.000001, 0.0001, 0.01, 1, 100, 10000, 1000000), 
        labels = c(0.00000001, 0.000001, 0.0001, 0.01, 1, 100, 10000, 1000000)) +
      geom_boxplot(alpha = 0.7, aes(color = effect_f, fill = effect_f)) +
      scale_color_manual(values = c("#FAB455", "#A5683C")) +
      scale_fill_manual(values = c("#FAB455", "#A5683C")) +
      theme_classic() +
      theme(text = element_text(size=18),
        legend.position = "right") +
      labs(x = "Concentration (mg/L)",
        y = "Polymer",
        color = "Effect?",
        fill = "Effect?")
    
  })
  
  # Endpoint Plot
  
  output$lvl_plot_react <- renderPlot({
    
    ggplot(aoc_filter(), aes(x = dose.mg.L.master, y = lvl1_f)) +
      scale_x_log10(breaks = c(0.00000001, 0.000001, 0.0001, 0.01, 1, 100, 10000, 1000000), 
        labels = c(0.00000001, 0.000001, 0.0001, 0.01, 1, 100, 10000, 1000000)) +
      geom_boxplot(alpha = 0.7, aes(color = effect_f, fill = effect_f)) +
      scale_color_manual(values = c("#A99CD9", "#6C568C")) +
      scale_fill_manual(values = c("#A99CD9", "#6C568C")) +
      theme_classic() +
      theme(text = element_text(size=18),
        legend.position = "right") +
      labs(x = "Concentration (mg/L)",
        y = "Endpoint",
        color = "Effect?",
        fill = "Effect?")
    
  })
  
  #Lvl2 Plot 
  
  output$lvl2_plot_react <- renderPlot({
    
    ggplot(aoc_filter(), aes(x = dose.mg.L.master, y = lvl2_f)) +
      scale_x_log10(breaks = c(0.00000001, 0.000001, 0.0001, 0.01, 1, 100, 10000, 1000000), 
                    labels = c(0.00000001, 0.000001, 0.0001, 0.01, 1, 100, 10000, 1000000)) +
      geom_boxplot(alpha = 0.7, aes(color = effect_f, fill = effect_f)) +
      scale_color_manual(values = c("#A99CD9", "#6C568C")) +
      scale_fill_manual(values = c("#A99CD9", "#6C568C")) +
      theme_classic() +
      theme(text = element_text(size=18),
            legend.position = "right") +
      labs(x = "Concentration (mg/L)",
           y = "Specific Endpoint",
           color = "Effect?",
           fill = "Effect?")
    
  })
  
  
  
  # Create downloadable csv of filtered dataset.
  # Removed columns created above so the dataset matches Leah's original dataset.
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(aoc_filter() %>%
          select(-c(effect_f, size_f, shape_f, poly_f, org_f, lvl1_f, lvl2_f, bio_f, vivo_f, life_f, env_f)), 
        file, row.names = FALSE)
    }
  )
  
  # Create "reset" button to revert all filters back to what they began as.
  # Need to call all widgets individually by their ids.
  # See https://stackoverflow.com/questions/44779775/reset-inputs-with-reactive-app-in-shiny for more information.
  observeEvent(input$reset_input, {
    shinyjs::reset("lvl1_check")
    shinyjs::reset("poly_check")
    shinyjs::reset("organism_check")
    shinyjs::reset("shape_check")
    shinyjs::reset("env_check")
    shinyjs::reset("effect_check")
    shinyjs::reset("size_check")
    shinyjs::reset("life_check")
    shinyjs::reset("bio_check")
  }) #If we add more widgets, make sure they get added here. 

#### Scott S ####

  #Create dependent dropdown checklists: select Species by Group.
  output$SpeciesSelection <- renderUI({
    
    Group_c <- input$Group_check_ssd # assign level values to "lvl1_c"
    
    aoc_new <- aoc_z %>% # take original dataset
      filter(Group %in% Group_c) %>% # filter by level inputs
      mutate(Species_new = factor(as.character(Species))) # new subset of factors
    
    pickerInput(inputId = "Species_check_ssd", 
                label = "Species:", 
                choices = levels(aoc_new$Species_new),
                selected = levels(aoc_new$Species_new),
                options = list(`actions-box` = TRUE),
                multiple = TRUE)})
  
  #Create dependent dropdown checklists: select lvl2 by lvl1.
  output$lvl2Selection <- renderUI({
    
    lvl1_c <- input$lvl1_check_ssd # assign level values to "lvl1_c"
    
    aoc_new <- aoc_setup %>% # take original dataset
      filter(lvl1_f %in% lvl1_c) %>% # filter by level inputs
      mutate(lvl2_f_new = factor(as.character(lvl2_f))) # new subset of factors
    
    pickerInput(inputId = "lvl2_check_ssd", 
                label = "Specific Endpoint within Broad Category:", 
                choices = levels(aoc_new$lvl2_f_new),
                selected = levels(aoc_new$lvl2_f_new),
                options = list(`actions-box` = TRUE),
                multiple = TRUE)})
  
  
  # Create new all tested dataset based on widget filtering and adjusted to reflect the presence of the "update" button.
  aoc_z_L <- eventReactive(list(input$SSDgo),{
    # eventReactive explicitly delays activity until you press the button
    # here we'll use the inputs to create a new dataset that will be fed into the renderPlot calls below
    env_c_ssd <- input$env_check_ssd #assign environments
    Group_c_ssd <- input$Group_check_ssd # assign organism input values to "org_c"
    Species_c_ssd <- input$Species_check_ssd #assign species input
    size_c_ssd <- input$size_check_ssd #assign sizes input
    lvl1_c_ssd <- input$lvl1_check_ssd #assign endpoints
    poly_c_ssd <- input$poly_check_ssd #assign polymers
   
    #filter out reported, calcualted, or all based on checkbox
     Reported_Converted_rad <- input$Reported_Converted_rad #use nominal or calculated exposure concentrations. Options are TRUE (calculated) or FALSE (reported)
    if(Reported_Converted_rad == "reported"){
      aoc_z <- aoc_z %>% 
        filter(dose.mg.L.master.converted.reported != "converted")
    } 
    if(Reported_Converted_rad == "converted"){
      aoc_z <- aoc_z %>% 
        filter(dose.mg.L.master.converted.reported != "reported")
    } 
    if(Reported_Converted_rad == "all"){
      aoc_z <- aoc_z 
    }
    
    #left-hand table of all data considered
    aoc_z %>% # take original dataset
      filter(env_f %in% env_c_ssd) %>% #filter by environment inputs
      filter(Group %in% Group_c_ssd) %>% # filter by organism inputs
      filter(Species %in% Species_c_ssd) %>% #filter by species inputs
      filter(size_f %in% size_c_ssd) %>% #filter by size inputs
      filter(lvl1_f %in% lvl1_c_ssd) %>% # filter by level inputs
      filter(poly_f %in% poly_c_ssd) %>% #filter by polymer inputs
      filter(dose.mg.L.master > 0) %>% #clean out no dose data
      group_by(Species) %>% 
            summarise(MinConcTested = min(dose.mg.L.master), MaxConcTested = max(dose.mg.L.master), CountTotal = n()) %>%   #summary data for whole database
      mutate_if(is.numeric, ~ signif(., 4)) %>% 
      drop_na() #must drop NAs or else nothing will work
        })
  
  # Create new effect dataset based on widget filtering and adjusted to reflect the presence of the "update" button.
  aoc_z_R <- eventReactive(list(input$SSDgo),{
    # eventReactive explicitly delays activity until you press the button
    # here we'll use the inputs to create a new dataset that will be fed into the renderPlot calls below
    
    env_c_ssd <- input$env_check_ssd #assign environments
    Group_c_ssd <- input$Group_check_ssd # assign organism input values to "org_c"
    Species_c_ssd <- input$Species_check_ssd #assign species input
    size_c_ssd <- input$size_check_ssd #assign sizes input
    lvl1_c_ssd <- input$lvl1_check_ssd #assign endpoints
    poly_c_ssd <- input$poly_check_ssd #assign polymers
    
    #filter out reported, calcualted, or all based on checkbox
    Reported_Converted_rad <- input$Reported_Converted_rad #use nominal or calculated exposure concentrations. Options are TRUE (calculated) or FALSE (reported)
    if(Reported_Converted_rad == "converted"){
      aoc_z <- aoc_z %>% 
        filter(dose.mg.L.master.converted.reported != "converted")
    } 
    if (Reported_Converted_rad == "reported"){
      aoc_z <- aoc_z %>% 
        filter(dose.mg.L.master.converted.reported != "reported")
    } 
    if (Reported_Converted_rad == "all"){
        aoc_z <- aoc_z 
    }
    
    #right-hand table of just effect data
    aoc_z %>% 
      filter(env_f %in% env_c_ssd) %>% #filter by environment inputs
      filter(Group %in% Group_c_ssd) %>% # filter by organism inputs
      filter(Species %in% Species_c_ssd) %>% #filter by species inputs
      filter(size_f %in% size_c_ssd) %>% #filter by size inputs
      filter(lvl1_f %in% lvl1_c_ssd) %>% # filter by level inputs
      filter(poly_f %in% poly_c_ssd) %>% #filter by polymer inputs
      filter(effect_f == "Yes") %>% #only select observed effects
      group_by(Species, Group) %>%
      summarise(Conc = min(dose.mg.L.master), meanConcEffect = mean(dose.mg.L.master), medianConcEffect = median(dose.mg.L.master), SDConcEffect = sd(dose.mg.L.master),MaxConcEffect = max(dose.mg.L.master), CountEffect = n(), MinEffectType = lvl1[which.min(dose.mg.L.master)], MinEnvironment = environment[which.min(dose.mg.L.master)], MinDoi = doi[which.min(dose.mg.L.master)], MinLifeStage = life.stage[which.min(dose.mg.L.master)], Mininvitro.invivo = invitro.invivo[which.min(dose.mg.L.master)]) %>%  #set concentration to minimum observed effect
      mutate_if(is.numeric, ~ signif(., 3)) %>% 
      drop_na(Conc) #must drop NAs or else nothing will work
     })
  
  #Join
  aoc_filter_ssd <- reactive({
    req(aoc_z_L)
    req(aoc_z_R)
    
    #join datasets (final)
    aoc_z_join <- right_join(aoc_z_L(), aoc_z_R(), by = "Species") 
    #order list
    col_order <- c("Group", "Species", "Conc", "MinEffectType", "MinEnvironment", "MinDoi", "meanConcEffect", "medianConcEffect", "SDConcEffect", "MaxConcEffect", "CountEffect", "MinConcTested", "MaxConcTested", "CountTotal")
    #reorder
    aoc_z_join_order <- aoc_z_join[, col_order]
    
    #'print'
    aoc_z_join_order
  })
  
  
  #print summarize filtered data in data table
  output$aoc_filter_ssd_table <- DT::renderDataTable({
    req(input$SSDgo)
    
    datatable(aoc_filter_ssd(),
              extensions = c('Buttons'),
              options = list(
                dom = 'Brtip',
                buttons = list(I('colvis'), c('copy', 'csv', 'excel')),
                autoWidth = TRUE,
                scrollX = TRUE,
                columnDefs = list(list(width = '50px, targets = "_all'))),#only display the table and nothing else
              colnames = c("Group", "Species", "Most Sensitive Concentration (mg/L)", "Most Sensitive Effect", "Most Sensitive Environment", "DOI", "Average Effect Concentration (mg/L)", "Median Effect Concentration (mg/L)", "Std Dev Effect Concentration (mg/L)", "Maximum Observed Effect Concentration (mg/L)", "Number of doses with Effects", "Min Concentration Tested (with or without effects) (mg/L)", "Max Concentration Tested (with or without effects) (mg/L)", "Total # Doses Considered"),
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
  output$table_gof_react <- DT::renderDataTable({
    datatable(gof(),
              options = list(dom = 't'), #only display the table and nothing else
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
    
    set.seed(99)
    stats::predict(fit_dists(), #Predict fitdist. 
            average = pred_c_ave_ssd, #flag tells whether or not to average models from user input
            ic = pred_c_ic_ssd, #tells which information criteria to use - user input
            nboot = nbootNum, #number of bootstrap samples to use to estimate SE and CL
            ci= TRUE) #estimates confidence intervals
  }) 
 
# **SSD Plot ----
#Create the plot for species sensitivity distribution
SSD_plot_react <- reactive({
    req(input$ssdPred) #won't start until button is pressed for prediction
    pred_c_hc_ssd <- as.numeric(input$pred_hc_ssd) #assign hazard concentration from numeric input
   
    #reactive -> static data
    aoc_ssd <- aoc_filter_ssd() %>% 
      arrange(Conc)
    
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
     xlab = "Concentration (mg/L)",
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
    device <- function(..., width, height) {
      grDevices::png(..., width = 12, height = 16, res = 600, units = "in")
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
    
    set.seed(99)
    ssd_hc(fit_dists(), #dataset
           percent = pred_c_hc_ssd, #numeric threshold input by user (default is 0.05)
           nboot = 10, # number of bootstrap predictions to make. 10 is minimum, 1,000 is default
           average = pred_c_ave_ssd, #tells whether or not the average models
           ic = pred_c_ic_ssd, #tells which information criteria to use
           ci = TRUE) #flag to estimate confidence intervals using parametric bootstrapping
  })
  
#Plot SSD data with ggplot
   ssd_ggplot <- reactive({
    # calculate fraction
    aoc_ssd <- aoc_filter_ssd() %>% 
      arrange(Conc)
    
    aoc_ssd$frac <- ppoints(aoc_ssd$Conc, 0.5)
    
    #convert hazard concentration to sig digits
    aochc <- aoc_hc()
    
    aochc$est_format <-format(aochc$est, digits = 3, scientific = TRUE)
    
    ggplot(aoc_pred(),aes_string(x = "est")) +
      geom_xribbon(aes_string(xmin = "lcl", xmax = "ucl", y = "percent/100"), alpha = 0.2) +
      geom_line(aes_string(y = "percent/100")) +
      geom_point(data = aoc_ssd,aes(x = Conc, y =frac, color = Group)) + 
      geom_text_repel(data = aoc_ssd, aes(x = Conc, y = frac, label = Species, color = Group), nudge_x = 0.2, size = 4, segment.alpha = 0.5) + #species labels
      scale_y_continuous("Species Affected (%)", labels = scales::percent) +
      expand_limits(x = c(0.000000001, 100000),y = c(0, 1)) + #ensure species labels fit
      xlab("Concentration (mg/L)")+
      coord_trans(x = "log10") +
      scale_x_continuous(breaks = scales::trans_breaks("log10", function(x) 10^x),labels = comma_signif)+
      geom_segment(data = aochc,aes(x = est, y = percent/100, xend = est, yend = est), linetype = 'dashed', color = "red", size = 1) + #hazard conc line vertical
      geom_segment(data = aochc,aes(x = lcl, y = percent/100, xend = est, yend = percent/100), linetype = 'dashed', color = "red", size = 1) + #hazard conc line horizontal
      geom_text(data = aochc, aes(x = est, y = -0.09, label = paste0(percent, "% Hazard Confidence Level")), color = "red", size = 5) + #label for hazard conc
      geom_text(data = aochc, aes(x = est, y = -0.05, label = est_format), color = "red", size = 5) + #label for hazard conc
      scale_fill_viridis(discrete = TRUE) +  #make colors more differentiable 
      scale_color_viridis(discrete = TRUE)  #make colors more differentiable 
  })
  
  
  output$aoc_ssd_ggplot <- renderPlot({
    ssd_ggplot()
  })
  
  
  #hover text for info
  output$info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("Concentration (mg/L)=", format(e$x,scientific = TRUE), " percent=", percent(e$y), "\n")
    }
    
    paste0(
      "", xy_str(input$plot_hover)
    )
  })
  
  # SSD Table

  output$ssd_pred_table <- DT::renderDataTable({
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
                colnames = c("Percent", "Estimated Mean Concentration", "Standard Error", "Lower 95% Confidence Limit", "Upper 95% Confidence Limit", "Distribution"),
                caption = "Predicted species sensitivity distribution concentrations with uncertanties."
                )
  })

  } #Server end

#### Full App ####
shinyApp(ui = ui, server = server)

# End of R Shiny app script.
