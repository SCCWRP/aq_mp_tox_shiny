#### Aquatic Microplastics Toxicology Shiny App
#### File created: September 23, 2020
#### Code contributors: Heili Lowman, Leah Thornton Hampton, Scott Coffin, Emily Darin

#### Setup ####

# Anything that should only happen ONCE should be placed in the setup section, prior to the actual shiny structure.

# Load packages
library(tidyverse)
library(patchwork)
library(tigerstats)
library(ggplot2)
library(ggrepel)
library(calecopal)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(scales)
library(reshape2)
library(ssdtools) #for species sensitivity distributions
library(DT) #to build HTML data tables
library(plotly) #to make plots interactive
#library(htmlwidgets) #to animate time-series. May not be necessary

#options(scipen=999) #globally overrides scientific notation so that the x-axis isn't half-scientific

# Load finalized dataset.
aoc <- read_csv("AquaticOrganisms_Clean_final.csv", guess_max = 10000)

# Add log transformed concentration columns for easier plotting below.
#aoc$log_dose.mg.L <- log10(aoc$dose.mg.L)
#aoc$log_dose.particles.mL <- log10(aoc$dose.particles.mL)

# Add factor and releved effects column.
aoc$effect_f <- factor(aoc$effect, levels = c("Y", "N"))

#### Leah Setup ####

#### Emily Setup ####

Final_effect_dataset <- read_csv("Final_effect_dataset.csv")%>%
  mutate(plot_f = case_when(
    plot_f == "Polymer" ~ "Polymer",
    plot_f == "Size" ~ "Size",
    plot_f == "Shape" ~ "Shape",
    plot_f == "Organism" ~ "Organism",
    plot_f == "Lvl1" ~ "Endpoints",
    plot_f == "Life.stage" ~ "Life Stage",
    plot_f == "Invivo.invivo" ~ "Invivo or Invitro",
    plot_f == "Exposure.route" ~ "Exposure route"))%>%
  mutate(plot_f = factor(plot_f))

# Adding function for multiple graph output.
# Code adapted from https://gist.github.com/wch/5436415/ and comment at https://gist.github.com/wch/5436415/#gistcomment-1608976 .

# Creates function called "get_plot_output_list" where the input variable is "input_n".
get_plot_output_list <- function(input_n) {
  
  # For every value in "input_n", insert it as "i" into the function below and then save the full output into "plot_output_list":
  plot_output_list <- lapply(input_n, function(i) {
    
    # Commenting out these lines because don't *exactly* know what they're doing.
    #plotname <- paste("plot", i, sep="")
    #plot_output_object <- plotOutput(plotname)
    #plot_output_object <- 
    
    # Render the individual plots      
    renderPlotly({
      
      # use the original dataset
      Final_effect_dataset %>%
        
        # filter by input
        filter(plot_f==i) %>%
        
        # generate plot
        ggplot(aes(fill=effect, y=Freq, x=Type, Endpoints=Endpoints)) +
        geom_bar(position="stack", stat="identity") +
        geom_text(aes(label= paste0(Freq,"%")), position = position_stack(vjust = 0.5),colour="black") +
        scale_fill_manual(values = cal_palette(case_when(i=="Polymer"~"wetland", i=="Organism"~"oak", i=="Size"~"bigsur2",i=="Shape"~"sierra2",i=="Endpoints"~"lake",i=="Life Stage"~"conifer",i=="Exposure Route"~"coastaldune1",i=="Invivo or Invitro"~"sbchannel")))+
        theme_classic() +
        labs(fill="Effect") +
        theme(legend.position = "right",
          axis.ticks=element_blank(),
          axis.text.y=element_blank(),
          axis.title.y = element_blank())
      
      ggplotly(tooltip = 'Endpoints')
      
    })
    
  })
  
  do.call(tagList, plot_output_list) # Need to call it as a list to display properly.
  
  return(plot_output_list) # Returns the full list of stored plots.
}

#### Heili Setup ####

# Master dataset for scatterplots - for Heili's tab.
aoc_x <- aoc %>% # start with original dataset
  # full dataset filters.
  filter(effect == "Y") %>% # only includes those datapoints with demonstrated effects.
  # size category data tidying.
  mutate(size.category.noNA = replace_na(size.category, 0)) %>% # replaces NA with 0 so we can better relabel it.
  mutate(size_cat = case_when(
    size.category.noNA == 1 ~ "1nm < 100nm",
    size.category.noNA == 2 ~ "100nm < 1µm",
    size.category.noNA == 3 ~ "1µm < 100µm",
    size.category.noNA == 4 ~ "100µm < 1mm",
    size.category.noNA == 5 ~ "1mm < 5mm",
    size.category.noNA == 0 ~ "unavailable")) %>% # creates new column with nicer names.
  mutate(size_f = factor(size_cat, levels = c("1nm < 100nm", "100nm < 1µm", "1µm < 100µm", "100µm < 1mm", "1mm < 5mm", "unavailable"))) %>% # order our different size levels.
  # shape category data tidying.
  mutate(shape.noNA = replace_na(shape, "unavailable")) %>% # replaces NAs to better relabel.
  mutate(shape_f = factor(shape.noNA, levels = c("fiber", "fragment", "sphere", "unavailable"))) %>% # order our different shapes.
  # polymer category data tidying.
  mutate(polymer.noNA = replace_na(polymer, "unavailable")) %>% # replaces NA to better relabel.
  mutate(poly_f = factor(polymer.noNA, levels = c("BIO", "EVA", "PA", "PC", "PE", "PET", "PLA", "PMMA", "PP", "PS", "PUR", "PVC", "unavailable"))) %>% # order our different polymers.
  # taxonomic category data tidying.
  mutate(organism.noNA = replace_na(organism.group, "unavailable")) %>% # replaces NA to better relabel.
  mutate(org_f = factor(organism.noNA, levels = c("Algae", "Annelida", "Bacteria", "Cnidaria", "Crustacea", "Echinoderm", "Fish", "Insect", "Mollusca", "Nematoda", "Plant", "Rotifera", "unavailable"))) %>% # order our different organisms.
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
  mutate(lvl1_f = factor(lvl1_cat))%>%# order different endpoints.
  # Level 2 Data tidying
  mutate(lvl2_cat = case_when(
    lvl2 == "abundance"~"Abundance",
    lvl2 == "agressivity"~"Agressivity",
    lvl2 == "bacteriodetes"~ "Bacteriodetes",
    lvl2 == "actinobacteria"~"Actinobacteria",
    lvl2 == "ammonia.excretion" ~ "Ammonia Excretion",
    lvl2 == "blood"~"Blood",
    lvl2 == "boldness"~"Boldness",
    lvl2 == "body.condition"~"Body Condition",
    lvl2 == "brainhisto"~"Brain Histological Abnormalities",
    lvl2 == "burrowing"~"Burrowing",
    lvl2 == "carb.metabolism"~"Carb Metabolism",
    lvl2 == "chemokines.cytokines"~"Chemokines",
    lvl2 == "circulatory"~"Circulatory",
    lvl2 == "detoxification"~"Detoxification",
    lvl2 == "developement"~"Developement",
    lvl2 == "digestion"~"Digestion",
    lvl2 == "digestive enzymes"~"Digestive Enzymes",
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
    lvl2 == "liver.kidney.products" ~" Liver and Kidney Products",
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
    lvl2 == "vision.system"~"Vision System"))%>% #Renames for widget
  mutate(lvl2_f = factor(lvl2_cat))%>%#order different endpoint categories
  mutate(bio_cat = case_when(           #Bio Organization Data Tidying
    bio.org == "cell"~"Cell",
    bio.org == "organism"~"Organism",
    bio.org == "population"~ "Population",
    bio.org == "subcell"~"Subcell",
    bio.org == "tissue" ~ "Tissue"))%>%
  mutate(bio_f = factor(bio_cat)) #order different bio organization categories for bio organization widget

    
#filter out terrestrial data
aoc_y <- aoc_x %>% 
filter(environment != "Terrestrial") # removes terrestrial data.


#### Scott Setup ####

# Master dataset for SSDs
aoc_z <- aoc_x %>% # start with Heili's altered dataset (no filtration for terrestrial data)
  # environment category data tidying.
  mutate(environment.noNA = replace_na(environment, "unavailable")) %>% # replaces NA to better relabel.
  mutate(env_f = factor(environment.noNA, levels = c("Marine", "Freshwater", "Terrestrial", "unavailable"))) %>% # order our different environments.
  #must drop NAs or else nothing will work 
  drop_na(dose.mg.L) %>% 
  #SSD package depends on specific naming conventions. Prep factors accordingly below
  mutate(Conc = dose.mg.L)   #must make value named 'Conc' for this package

# final cleanup and factoring  
aoc_z$species <- str_replace(aoc_z$species,"franciscana�","franciscana") #fix <?> unicode symbol in francisca species
aoc_z$Species <- as.factor(paste(aoc_z$genus,aoc_z$species)) #must make value 'Species" (uppercase)
aoc_z$Group <- as.factor(aoc_z$organism.group) #must make value "Group"
aoc_z$Group <- fct_explicit_na(aoc_z$Group) #makes sure that species get counted even if they're missing a group

#mutate(species_f = factor(Species, levels = c("Daphnia magna","Mytilus galloprovincialis","Arenicola marina","Scenedesmus obliquus","Chlorella NA","Raphidocelis subcapitata","Dunaliella salina","Cyprinodon variegatus","Daphnia galeata","Paracentrotus lividus","Amphibalanus amphitrite","Artemia franciscana�","Pinctada margaritifera","Multiple NA","Pimephales promelas","Brachionus koreanus","Paracyclopina nana","Danio rerio","Lemna minor","Chlamydomas reinhardtii","Tigriopus japonicus","Chlorella pyrenoidosa","Hydra attenuata","Pomatoschistus microps","Mytilus NA","Acropora muricata","Heliopora coerulea","Pocillopora verrucosa","Mytilus spp","Scrobicularia plana","Perna viridis","Oryzias latipes","Barbodes gonionotus","Chaetoceros neogracile","Halomonas alkaliphila","Crassostrea gigas","Pocillopora damicornis","Mytilus edulis","Artemia parthenogenetica","Microcystis flos-aquae","Sebastes schlegelii","Eriocheir sinensis","Seletonema costatum","Karenia mikimotoi","Ceriodaphnia dubia","Dicentrachus labrax","Brachionus plicatilis","Tigriopus fulvus","Oryzias melastigma","Oryzias sinensis","Zacco temminckii","Oreochromis niloticus","Clarias gariepinus","Carassius carassius", "Cyprinus carpio")))# order our different species.

# Create Shiny app. Anything in the sections below (user interface & server) should be the reactive/interactive parts of the shiny application.

#### User Interface ####
ui <- fluidPage( theme= "classic",
  
  # App title
  titlePanel(h1("Microplastics Toxicity Database")),
  
  # Title panel subtext
  tags$div(
    "This is a draft website to present the results of the aquatic microplastics toxicology database. Do not use without prior consulting with Leah Thornton Hampton (leahth@sccwrp.org)."),
  
  br(), # line break
  
  # Main panel for displaying outputs
  mainPanel(
    
      # Output: set of 5 tabs
      tabsetPanel(type = "tabs",

#### Leah UI ####        
                  tabPanel("Introduction", 
                    
                    #Place holder for a cute logo someday? 
                                  
                    br(), # line break
                    h3("What is the Microplastics Toxicity Database?", align = "center", style = "color:darkcyan"),
                    
                    strong(p("The Microplastics Toxicity Database is a repository for microplastics 
                      toxicity data pertaining to both human and aquatic organism health.")), 
                    
                    p("Microplastics are a ubiquitous suite of environmental contaminants that comprise 
                      an incredible range of shapes, sizes, polymers and chemical additives. In addition, 
                      studies focused on the effects of microplastics are being rapidly published and 
                      often vary in quality. Because of this, it is challenging to identify sensitive biological 
                      endpoints and prioritize potential drivers of microplastic toxicity."),
                    
                    p("This web application is intended to meet these challenges
                    by allowing users to explore toxicity 
                    data using an intuitive interface while retaining the diversity and complexity inherent 
                    to microplastics. Data is extracted from existing, peer-reviewed manuscripts containing 
                    toxicity data pertaining to microplastics and associated chemicals and organized into 5 
                    main categories:"),
                    
                    img(src = "data_categories_image.png", height = "90%", width = "90%", style = "display:block;margin-left: auto; margin-right: auto;"),
                    br(),
                    p("This web application allows users to visualize the data while selecting for specific 
                      parameters within the data categories above. For instance, a user may want to visualize 
                      how polymer type impacts the growth of early life stage fish that were exposed to 
                      microplastics for 7 days or longer."),
                    
                    h3("Why was the Microplastics Toxicity Database and Web Application created?", align = "center", style = "color:darkcyan"),
                    
                    p("The database and application tools have been created for use by the participants of the ", a(href = "https://www.sccwrp.org/about/
                      research-areas/additional-research-areas/
                      trash-pollution/microplastics-health-effects-webinar-series/", 'Microplastics Health Effects Workshop', 
                      .noWS = "outside"),
                      ". The purpose of this workshop is to identify the primary pathways by which microplastics affect biota, prioritize 
                      the microplastics characteristics (e.g., size, shape, polymer) that are of greatest biological concern, and identify 
                      critical thresholds for each at which those biological effects become pronounced. These findings will 
                      be used directly by the state of California to fulfill ", a(href = "https://www.sccwrp.org/about/research-areas/
                      additional-research-areas/trash-pollution/microplastics-health-effects-webinar-series/history-california-microplastics-legislation/", 'legislative mandates', 
                      .noWS = "outside")," regarding the
                      management of microplastics in drinking water and the aquatic environment."),
                   
                    h3("How do I use the Microplastics Toxicity Database Web Application?", align = "center", style = "color:darkcyan"),
                    
                    p("By clicking on the tabs at the top of this page, you may navigate to different section. Each section provides different information or data visualization options. 
                      More specific instructions may be found within each section."),
                  
                    h3("Contributors", align = "center", style = "color:darkcyan"),
                    br(),
                    
                    p(align = "center", a(href = "https://www.sccwrp.org/about/staff/leah-thornton-hampton/", 'Dr. Leah Thornton Hampton'),", Southern California Coastal Water Research Project ", 
                      tags$a(href="https://twitter.com/DrLeahTH", tags$img(src="twitter.png", width="2%", height="2%")), tags$a(href="https://github.com/leahth", tags$img(src="github.png", width="2%", height="2%"))),
                    p(align = "center", a(href = "https://www.sccwrp.org/about/staff/heili-lowman/", 'Dr. Heili Lowman'),", Southern California Coastal Water Research Project ",
                      tags$a(href="https://twitter.com/heili_lowman", tags$img(src="twitter.png", width="2%", height="2%")), tags$a(href="https://github.com/hlowman", tags$img(src="github.png", width="2%", height="2%"))), 
                    p(align = "center", a(href = "https://agency.calepa.ca.gov/staffdirectory/detail.asp?UID=69294&BDO=7&VW=DET&SL=S", 'Dr. Scott Coffin'),", California State Water Resources Control Board", 
                      tags$a(href="https://twitter.com/DrSCoffin", tags$img(src="twitter.png", width="2%", height="2%")), tags$a(href="https://github.com/ScottCoffin", tags$img(src="github.png", width="2%", height="2%"))),
                    p(align = "center", a(href = "https://www.sfei.org/users/liz-miller", 'Dr. Ezra Miller'),", Aquatic Science Center"),
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
                    
                    br(),
                    
                    h3("Contact", align = "center", style = "color:darkcyan"),
                    
                    p(align = "center", "For more information about the database or other questions, please contact Dr. Leah Thornton Hampton (leahth@sccwrp.org)."),
                    
                    br(),
                    
                  splitLayout(align = "center", 
                  tags$a(href="https://www.waterboards.ca.gov", tags$img(src="waterboard.png", width = "100%", height = "100%")),
                  tags$a(href="https://www.swccrp.org", tags$img(src="sccwrp.png", width = "100%", height = "100%")),
                  tags$a(href="https://www.sfei.org/", tags$img(src="sfei.png", width = "100%", height = "100%"))),
                  
                    
                    br(), 
                    
                    verbatimTextOutput(outputId = "Leah1")),
                
                  tabPanel("Resources", 
                      br(),     
                      h3(align = "center", a(href = "https://sccwrp-my.sharepoint.com/:b:/g/personal/leahth_sccwrp_org/Eb8XXdAvn9BBpOB6Z6klzEcBlb6mFpJcYJrHBAQk7r1z3A?e=tRTqDM", 'Data Category Descriptions')),
                      br(),
                      h3(align = "center", a(href = "https://sccwrp-my.sharepoint.com/:b:/g/personal/leahth_sccwrp_org/EXDS25x3JAJHhZAj3qDwWgIBeB-oz0mIihclR2oOckPjhg?e=GtOeB5", 'Aquatic Organisms Study List')),
                      br(),
                      h3(align = "center", a(href = "https://sccwrp-my.sharepoint.com/:b:/g/personal/leahth_sccwrp_org/ES_FUiwiELtNpWgrPCS1Iw4Bkn3-aeiDjZxmtMLjg3uv3g?e=bmuNgG", 'Human Study List')),
                           
                    verbatimTextOutput(outputId = "Leah2")),
        
#### Emily UI ####

tabPanel("Data Overview", #tab opening
         br(), # line break
         h3("Measured Effects of Microplastics", align = "center", style = "color:darkcyan"),
         br(), # line break
        
    
awesomeCheckboxGroup(inputId = "Emily_check", # effect checklist
            label = "Effects:", # checklist label
            choices = levels(Final_effect_dataset$plot_f), # options for user
            selected = "Polymer",# default selected
            inline = TRUE), #allows for multiple selections at once
            br(),
            
uiOutput(outputId= "Emily_plot")),

#### Heili UI ####
                  tabPanel("Data Exploration", 
                    h3("Microplastics in Aquatic Environments: Data Exploration of Toxicological Effects", align = "center", style = "color:darkcyan"),
                    br(), # line break
                    p("The figures below display data from the literature review of toxicological effects of microplastics on aquatic organisms. All data displayed - individual points and boxplots - are from studies in which there was a demonstrated significant toxicological effect of microplastics."),
                    br(), # line break
                    p("Each row of figures displays a different value along the y-axis - size, shape, and polymer, respectively. Each column of figures displays a different unit along the x-axis - mg/L and particles/mL, respectively.The data may be filtered by organism and/or endpoint using the drop-down menus located below."),
                    br(), # line break
                    p("To the left of each boxplot are displayed the number of individuals measurements or observations (the first value within parentheses) and the number of published studies from which the data was collected (the second value within parentheses)."),
                    br(), # line break
                    
                    # widgets
                    column(width = 12,
                      column(width = 3,
                      # alternative to fully listed checklists
                      # requires shinyWidgets package
                      pickerInput(inputId = "organism_check", # organismal checklist
                        label = "Organisms:", 
                        choices = levels(aoc_y$org_f),
                        selected = levels(aoc_y$org_f),   
                        options = list(`actions-box` = TRUE), # option to de/select all
                        multiple = TRUE)), # allows for multiple inputs
                      
                      column(width = 3,
                      pickerInput(inputId = "lvl1_check", # endpoint checklist
                        label = "Endpoint Examined:", 
                        choices = levels(aoc_y$lvl1_f),
                        selected = levels(aoc_y$lvl1_f), 
                        options = list(`actions-box` = TRUE), # option to de/select all
                        multiple = TRUE)), # allows for multiple inputs
                      
                      #level 2 widget 
                      
                      column(width = 3,
                      pickerInput(inputId = "lvl2_check", # endpoint checklist
                        label = "Endpoint Category:", 
                        choices = levels(aoc_y$lvl2_f),
                        selected = levels(aoc_y$lvl2_f), 
                        options = list(`actions-box` = TRUE), # option to de/select all
                        multiple = TRUE)), # allows for multiple inputs
                      
                      #Bio organization widget
                      
                      column(width = 3,
                             pickerInput(inputId = "bio_check", # endpoint checklist
                              label = "Level of Biological Organization", 
                              choices = levels(aoc_y$bio_f),
                              selected = levels(aoc_y$bio_f), 
                              options = list(`actions-box` = TRUE), # option to de/select all
                              multiple = TRUE)), # allows for multiple inputs
                      
                      
                    
                      column(width = 3,
                        actionButton("go", "Update"))), # adds action button 
                    # "go" is the internal name to refer to the button
                    # "Update" is the title that appears on the app
                      
                      br(), # line break
                      hr(), # adds divider
                    
                    #mainPanel(
                      br(), # line break
                      plotOutput(outputId = "size_plot_react"),
                      br(), # line break
                      plotOutput(outputId = "shape_plot_react"),
                      br(), # line break
                      plotOutput(outputId = "poly_plot_react")), 
        
#### Scott UI ####
                  tabPanel("Species Sensitivity Distribution", 
                    br(), # line break
                    h3("Species Sensitivity Distribution", align = "center", style = "color:darkcyan"),
                    p("Species sensitivity distributions (SSDs) are cumulative probability distributions that estimate the percent of species affected by a given concentration of exposure using Maximum Likelihood and model averaging. A useful metric often used for setting risk-based thresholds is the concentration that affects 5% of the species, and is reffered to as the 5% Hazard Concentration (HC). For more information on SSDs, refer to Posthuma, Suter II, and Traas (2001)."),
                    br(), # line break
                    p("Use the options below to filter the toxicity thresholds dataset. Once complete, hit the 'submit' button"),
                    
                    # widget 1
                    column(width = 12,
                           column(width = 3,
                                  # alternative to fully listed checklists
                                  # requires shinyWidgets package
                                  pickerInput(inputId = "env_check_ssd", # environment checklist
                                              label = "Environment:", 
                                              choices = levels(aoc_z$env_f),
                                              selected = levels(aoc_z$env_f),   
                                              options = list(`actions-box` = TRUE), # option to de/select all
                                              multiple = TRUE)), # allows for multiple inputs
                           # Organism widget
                           column(width = 3,
                                         pickerInput(inputId = "Group_check_ssd", # organism checklist
                                              label = "Organism Groups:", 
                                              choices = levels(aoc_z$Group),
                                              selected = levels(aoc_z$Group),   
                                              options = list(`actions-box` = TRUE), # option to de/select all
                                              multiple = TRUE)), # allows for multiple inputs
                           #Species widget
                           column(width = 3,
                                  pickerInput(inputId = "Species_check_ssd", # organism checklist
                                              label = "Species:",
                                              choices = levels(aoc_z$Species),
                                              selected = levels(aoc_z$Species),
                                              options = list(`actions-box` = TRUE), # option to de/select all
                                              multiple = TRUE))), # allows for multiple inputs
                           br(),
                           p("Advanced options. Suggest using defaults."),
                           br(),
                    
                           column(width = 12,
                           #Size widget
                           column(width = 3,
                                  pickerInput(inputId = "size_check_ssd", # organism checklist
                                              label = "Sizes:",
                                              choices = levels(aoc_z$size_f),
                                              selected = levels(aoc_z$size_f),
                                              options = list(`actions-box` = TRUE), # option to de/select all
                                              multiple = TRUE)), # allows for multiple inputs
                          
                            #Endpoint widget
                           column(width = 3,
                                  pickerInput(inputId = "lvl1_check_ssd", # organism checklist
                                              label = "Endpoints:",
                                              choices = levels(aoc_z$lvl1_f),
                                              selected = levels(aoc_z$lvl1_f),
                                              options = list(`actions-box` = TRUE), # option to de/select all
                                              multiple = TRUE)), # allows for multiple inputs
                           
                           #Polymer widget
                           column(width = 3,
                                  pickerInput(inputId = "poly_check_ssd", # organism checklist
                                              label = "Polymers:",
                                              choices = levels(aoc_z$poly_f),
                                              selected = levels(aoc_z$poly_f),
                                              options = list(`actions-box` = TRUE), # option to de/select all
                                              multiple = TRUE)), # allows for multiple inputs
                           
                           
                            column(width = 3,
                                  actionButton("SSDgo", "Submit"))), # adds action button 
                    # "SSDgo" is the internal name to refer to the button
                    # "Update" is the title that appears on the app
                           
                    br(), # line break
                    p("Please wait a moment while maximum likelihood estimation is calculated data based on your choices."),
                    br(),

                    
                    mainPanel("Microplastics in Aquatic Environments: Species Sensitivity Distributions",
                              br(), # line break
                              br(),
                              DT::dataTableOutput(outputId = "aoc_filter_ssd_table"),
                              p("The figure below displays minimum observed effect concentrations for a range of species along with three common distributions"),
                              br(),
                              plotOutput(outputId = "autoplot_dists_react"),
                              p("Different distributions can be fit to the data. Below are some common distributions (llogis = log-logistic; lnorm = log-normal; lgumbel = log-Gumbel)."),
                              br(),
                              p("Goodness of Fit Table"),
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
                              actionButton("ssdPred", "Predict"), # adds action button, "SSDpred" is the internal name to refer to the button # "Predict" is the title that appears on the app
                              br(),
                              p("Please be patient as maximum likelihood estimations are calculated. This may take several minutes."),
                              br(),
                              p("Species Sensitivity Distribution"),
                              plotOutput(outputId = "SSD_plot_react"),
                              br(),
                              p("The model-averaged 95% confidence interval is indicated by the shaded band and the model-averaged Hazard Concentration (user input value) by the dotted line."),
                              br(),
                              p("Below you will find an estimate of the hazard concentration at the user-specified level with assocaited 95% confidence interval."),
                              br(),
                              DT::dataTableOutput(outputId = "aoc_hc_table"), #print hazard concentration table
                              br(),
                              p("If the plot above is not working, you may find it below as a ggplot."),
                              br(),
                              plotOutput(outputId = "aoc_ssd_ggplot"),
                              br(),
                              p("This app is built using the R package ssdtools version 0.3.2, and share the same functionality. Citation: Thorley, J. and Schwarz C., (2018). ssdtools An R package to fit pecies Sensitivity Distributions. Journal of Open Source Software, 3(31), 1082. https://doi.org/10.21105/joss.01082.")
                              ) #closes out scott's main panel
                    ) #closes out Scott's tab panel
        

        ##### dummy tab entered by Heili ####
        # commented out for the time being
        # tabPanel("File Upload", 
        #   
        #   br(), # line break
        #   
        #   h3("Additional Data Exploration", align = "center", style = "color:darkcyan"),
        #   
        #   p("Use the file upload feature on the left-hand side of the page to upload your own dataset and explore it using the resulting plot. Datasets may only be uploaded in '.csv' format. Column titles must be one of the following: state, region_us_census, rank, costume, candy, pounds_candy_sold."),
        #   
        #   sidebarLayout(
        #     
        #   sidebarPanel(
        # 
        #     fileInput("file1", "Drag and drop data file here:", # .csv file input
        #       multiple = FALSE,
        #       accept = c(".csv"))),
        # 
        #   mainPanel(p("Region's top costumes:"),
        #     plotOutput(outputId = "costume_graph"))
        #     )
        # )
          
#following three parentheses close out UI. Do not delete. 
        )))   
        #))  #comment-out these two parentheses. they must be here, but need to figure out where forward parentheses need to be. 

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
  
  # Create new dataset based on widget filtering and adjusted to reflect the presence of the "update" button.
  aoc_filter <- eventReactive(list(input$go),{
    # eventReactive explicitly delays activity until you press the button
    # here we'll use the inputs to create a new dataset that will be fed into the renderPlot calls below
    
    # every selection widget should be represented as a new variable below
    org_c <- input$organism_check # assign organism input values to "org_c"
    lvl1_c <- input$lvl1_check # assign level values to "lvl1_c"
    lvl2_c<-input$lvl2_check 
    bio_c<-input$bio_check# assign bio values to bio_c
    
    aoc_y %>% # take original dataset
      filter(org_f %in% org_c) %>% # filter by organism inputs
      filter(lvl1_f %in% lvl1_c)%>% # filter by level inputs
      filter(lvl2_f %in% lvl2_c)%>%#filter by level 2 inputs 
      filter(bio_f %in% bio_c) #filter by bio organization
  })
  
  # Use newly created dataset from above to generate plotly plots for size, shape, and polymer plots on three different rows (for sizing display purposes).
  
  output$size_plot_react <- renderPlot({
    
    # Creating dataset to output counts.
    aoc_size1 <- aoc_filter() %>%
      drop_na(dose.mg.L) %>%
      group_by(size_f) %>% # need to include so there's a recognized "y"
      summarize(dose.mg.L = quantile(dose.mg.L, .1), # need for recognized "x"
        measurements = n(),
        studies = n_distinct(article))

    size1 <- ggplot(aoc_filter(), aes(x = dose.mg.L, y = size_f)) +
      geom_boxplot(alpha = 0.7, show.legend = FALSE, aes(color = size_f, fill = size_f)) +
      scale_x_log10(breaks = c(0.0001, 0.01, 1, 100, 10000), 
        labels = c(0.0001, 0.01, 1, 100, 10000)) +
      scale_color_manual(values = cal_palette("sbchannel", n = 6, type = "continuous")) +
      scale_fill_manual(values = cal_palette("sbchannel", n = 6, type = "continuous")) +
      geom_text_repel(data = aoc_size1, 
        aes(label = paste("(",measurements,",",studies,")")),
        nudge_x = -1,
        nudge_y = -0.25,
        segment.colour = NA) +
      theme_classic() +
      theme(text = element_text(size=16)) +
      labs(x = "Concentration (mg/L)",
        y = "Size")
    
    # Creating dataset to output counts.
    aoc_size2 <- aoc_filter() %>%
      group_by(size_f) %>%
      drop_na(dose.particles.mL) %>%
      summarize(dose.particles.mL = quantile(dose.particles.mL, .1), 
        measurements = n(),
        studies = n_distinct(article))
    
    size2 <- ggplot(aoc_filter(), aes(x = dose.particles.mL, y = size_f)) +
      geom_boxplot(alpha = 0.7, show.legend = FALSE, aes(color = size_f, fill = size_f)) +
      geom_text_repel(data = aoc_size2, 
        aes(label = paste("(",measurements,",",studies,")")),
        nudge_x = -1,
        nudge_y = -0.25,
        segment.colour = NA) +
      scale_x_log10(breaks = c(1, 10000, 100000000, 1000000000000, 10000000000000000), 
        labels = c(1, 10000, 100000000, 1000000000000, 10000000000000000)) +
      scale_color_manual(values = cal_palette("sbchannel", n = 6, type = "continuous")) +
      scale_fill_manual(values = cal_palette("sbchannel", n = 6, type = "continuous")) +
      theme_classic() +
      theme(text = element_text(size=16)) +
      labs(x = "Concentration (particles/mL)",
        y = " ")
    
    (size1 + size2) # using patchwork to combine figures
    
  })
  
  output$shape_plot_react <- renderPlot({
    
    aoc_shape1 <- aoc_filter() %>%
      drop_na(dose.mg.L) %>%
      group_by(shape_f) %>% 
      summarize(dose.mg.L = quantile(dose.mg.L, .1),
        measurements = n(),
        studies = n_distinct(article))
    
    shape1 <- ggplot(aoc_filter(), aes(x = dose.mg.L, y = shape_f)) +
      scale_x_log10(breaks = c(0.0001, 0.01, 1, 100, 10000), 
        labels = c(0.0001, 0.01, 1, 100, 10000)) +
      geom_boxplot(alpha = 0.7, show.legend = FALSE, aes(color = shape_f, fill = shape_f)) +
      scale_color_manual(values = cal_palette("chaparral3")) +
      scale_fill_manual(values = cal_palette("chaparral3")) +
      geom_text_repel(data = aoc_shape1, 
        aes(label = paste("(",measurements,",",studies,")")),
        nudge_x = -1,
        nudge_y = -0.25,
        segment.colour = NA) +
      theme_classic() +
      theme(text = element_text(size=16)) +
      labs(x = "Concentration (mg/L)",
        y = "Shape")
    
    aoc_shape2 <- aoc_filter() %>%
      drop_na(dose.particles.mL) %>%
      group_by(shape_f) %>% 
      summarize(dose.particles.mL = quantile(dose.particles.mL, .1),
        measurements = n(),
        studies = n_distinct(article))
    
    shape2 <- ggplot(aoc_filter(), aes(x = dose.particles.mL, y = shape_f)) +
      scale_x_log10(breaks = c(1, 10000, 100000000, 1000000000000, 10000000000000000), 
        labels = c(1, 10000, 100000000, 1000000000000, 10000000000000000)) +
      geom_boxplot(alpha = 0.7, show.legend = FALSE, aes(color = shape_f, fill = shape_f)) +
      scale_color_manual(values = cal_palette("chaparral3")) +
      scale_fill_manual(values = cal_palette("chaparral3")) +
      geom_text_repel(data = aoc_shape2, 
        aes(label = paste("(",measurements,",",studies,")")),
        nudge_x = -1,
        nudge_y = -0.25,
        segment.colour = NA) +
      theme_classic() +
      theme_classic() +
      theme(text = element_text(size=16)) +
      labs(x = "Concentration (particles/mL)",
        y = " ")
    
    (shape1 + shape2) # patchwork combining plots
    
  })
  
  output$poly_plot_react <- renderPlot({
    
    aoc_poly1 <- aoc_filter() %>%
      drop_na(dose.mg.L) %>%
      group_by(poly_f) %>% 
      summarize(dose.mg.L = quantile(dose.mg.L, .1),
        measurements = n(),
        studies = n_distinct(article))
    
    poly1 <- ggplot(aoc_filter(), aes(x = dose.mg.L, y = poly_f)) +
      scale_x_log10(breaks = c(0.0001, 0.01, 1, 100, 10000), 
        labels = c(0.0001, 0.01, 1, 100, 10000)) +
      geom_boxplot(alpha = 0.7, show.legend = FALSE, aes(color = poly_f, fill = poly_f)) +
      scale_color_manual(values = cal_palette("canary", n = 15, type = "continuous")) +
      scale_fill_manual(values = cal_palette("canary", n = 15, type = "continuous")) +
      geom_text_repel(data = aoc_poly1, 
        aes(label = paste("(",measurements,",",studies,")")),
        nudge_x = -1,
        nudge_y = -0.25,
        segment.colour = NA) +
      theme_classic() +
      theme(text = element_text(size=16)) +
      labs(x = "Concentration (mg/L)",
        y = "Polymer")
    
    aoc_poly2 <- aoc_filter() %>%
      drop_na(dose.particles.mL) %>%
      group_by(poly_f) %>% 
      summarize(dose.particles.mL = quantile(dose.particles.mL, .1),
        measurements = n(),
        studies = n_distinct(article))
    
    poly2 <- ggplot(aoc_filter(), aes(x = dose.particles.mL, y = poly_f)) +
      scale_x_log10(breaks = c(1, 10000, 100000000, 1000000000000, 10000000000000000), 
        labels = c(1, 10000, 100000000, 1000000000000, 10000000000000000)) +
      geom_boxplot(alpha = 0.7, show.legend = FALSE, aes(color = poly_f, fill = poly_f)) +
      scale_color_manual(values = cal_palette("canary", n = 15, type = "continuous")) +
      scale_fill_manual(values = cal_palette("canary", n = 15, type = "continuous")) +
      geom_text_repel(data = aoc_poly2, 
        aes(label = paste("(",measurements,",",studies,")")),
        nudge_x = -1,
        nudge_y = -0.25,
        segment.colour = NA) +
      theme_classic() +
      theme(text = element_text(size=16)) +
      labs(x = "Concentration (particles/mL)",
        y = " ")
    
    (poly1 + poly2) # join plots together using patchwork
    
  })

#### Scott S ####

  # Create new dataset based on widget filtering and adjusted to reflect the presence of the "update" button.
  aoc_filter_ssd <- eventReactive(list(input$SSDgo),{
    # eventReactive explicitly delays activity until you press the button
    # here we'll use the inputs to create a new dataset that will be fed into the renderPlot calls below
    
    env_c_ssd <- input$env_check_ssd #assign environments
    Group_c_ssd <- input$Group_check_ssd # assign organism input values to "org_c"
    Species_c_ssd <- input$Species_check_ssd #assign species input
    size_c_ssd <- input$size_check_ssd #assign sizes input
    lvl1_c_ssd <- input$lvl1_check_ssd #assign endpoints
    poly_c_ssd <- input$poly_check_ssd #assign polymers
    
    aoc_z %>% # take original dataset
      filter(env_f %in% env_c_ssd) %>% #filter by environment inputs
      filter(Group %in% Group_c_ssd) %>% # filter by organism inputs
      filter(Species %in% Species_c_ssd) %>% #filter by species inputs
      filter(size_f %in% size_c_ssd) %>% #filter by size inputs
      filter(lvl1_f %in% lvl1_c_ssd) %>% # filter by level inputs
      filter(poly_f %in% poly_c_ssd) %>% #filter by polymer inputs
      group_by(Species, Group) %>% 
      summarise(Conc = min(Conc)) #set concentration to minimum observed effect
  })
  
  #print summarize filtered data in data table
  output$aoc_filter_ssd_table <- DT::renderDataTable({
    req(input$SSDgo)
    
    datatable(aoc_filter_ssd(),
              options = list(), #only display the table and nothing else
              class = "compact",
              colnames = c("Species", "Group", "Sensitive Concentration (mg/L)"),
              caption = "Filtered Data"
    )
  })
  
  # Use newly created dataset from above to generate SSD
  
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
    
    set.seed(99)
    predict(fit_dists(), #object
            average = pred_c_ave_ssd, #tells whether or not the average models
            ic = pred_c_ic_ssd, #tells which information criteria to use
            nboot = 10,
            ci= TRUE) #estimates confidence intervals
  }) 
 

#Create the plot for species sensitivity distribution
  output$SSD_plot_react <- renderPlot({
    req(input$ssdPred) #won't start until button is pressed for prediction
    
    pred_c_hc_ssd <- as.numeric(input$pred_hc_ssd) #assign hazard concentration from numeric input
    
    ## create progress bar ##
    # Create 0-row data frame which will be used to store data
    dat <- data.frame(x = numeric(0), y = numeric(0))
    
    withProgress(message = 'Predicting Maximum Likelihood Estimation from Selected Model', value = 0, {
      # Number of times we'll go through the loop
      n <- 26
      
      for (i in 1:n) {
        # Each time through the loop, add another row of data. This is
        # a stand-in for a long-running computation.
        dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = "This may take several minutes")
        
        # Pause for 10 seconds to simulate a long computation.
        Sys.sleep(3)
      }
    })
    
    ## generate plot from prediction ##
   ssd_plot(
     aoc_filter_ssd(), #data
     aoc_pred(), #prediction
     color = "Group",
     label = "Species",
     xlab = "Concentration (mg/L)",
     ci = TRUE, #confidence interval
     ribbon = TRUE,
     hc = pred_c_hc_ssd) + #percent hazard concentration
     scale_fill_viridis_d() + #make colors more differentiable 
     scale_colour_viridis_d() +  #make colors more differentiable 
     expand_limits(x = 5000) + # to ensure the species labels fit
     ggtitle("Species Sensitivity for Microplastics")
      })
  
  
  ## Sub-plots ##
  #Determine Hazard Concentration
  
  #Estimate hazard concentration
  aoc_hc <- eventReactive(list(input$ssdPred),{
    
    #user inputs
    pred_c_ave_ssd <- as.logical(input$pred_ave_ssd) #assign prediction averaging choice
    pred_c_ic_ssd <- input$pred_ic_ssd #assign prediction information criteria choice
    pred_c_hc_ssd <- as.numeric(input$pred_hc_ssd) #assign hazard concentration from numeric input
    
    set.seed(99)
    ssd_hc(fit_dists(),
           percent = pred_c_hc_ssd,
           nboot = 10,
           average = pred_c_ave_ssd, #tells whether or not the average models
           ic = pred_c_ic_ssd, #tells which information criteria to use
           ci = TRUE)
  })
  
#Print table of hazard concentration data  
  output$aoc_hc_table <- DT::renderDataTable({
    req(input$ssdPred)
    
    datatable(aoc_hc(),
              options = list(dom = 't'), #only display the table and nothing else
              class = "compact",
              colnames = c("Percent", "Estimated Concentration (mg/L)", "Standard Error", "Lower Confidence Limit", "Upper Confidence Limit","Distribution Type"),
              caption = "Hazard Concentration from Filtered Data"
    )
  })

# #define hazard concentration value for figure
#   aoc_hc_sub <- eventReactive(list(input$ssdPred),{
#    
#   return(hc)
# })
  
#Plot SSD data with ggplot
  output$aoc_ssd_ggplot <- renderPlot({
   # req(input$ssdPred)
    
    hc <- aoc_hc() %>% 
      select(est) #defining estimation from above calculation
    hc <- as.numeric(hc)
    
   #Plot species sensitivity data with ggplot

    ggplot(aoc_pred(),
           aes_string(x = "est")) +
      geom_xribbon(aes_string(xmin = "lcl", xmax = "ucl", y = "percent/100"), alpha = 0.2) +
      geom_line(aes_string(y = "percent/100")) +
      geom_ssd(data = aoc_filter_ssd(),
               aes_string(x = "Conc")) +
      scale_y_continuous("Species Affected (%)", labels = scales::percent) +
      expand_limits(y = c(0, 1)) +
      xlab("Concentration (mg/L)")+
      coord_trans(x = "log10") +
      scale_x_continuous(
        breaks = scales::trans_breaks("log10", function(x) 10^x),
        labels = comma_signif) +
      geom_hcintersect(xintercept = hc, yintercept = 5 / 100) #utilizes hazard conc model predicted estimation
   })

  
  
  
  # server-side for dummy file input tab
  # notice - I don't refer to anything reactive within the "({})" with additional parentheses, because as long as the call is created and used within these brackets, you don't need the addition parentheses.
    output$costume_graph <- renderPlot({
    
    req(input$file1) # Using user-supplied dataset
    
    spooky <- read_csv(input$file1$datapath) # Reads in dataset as a .csv dataframe
      
    region_costume <- spooky %>%
      group_by(region_us_census) %>%
      count(costume, rank) # Creates a new dataset
    
    ggplot(region_costume, aes(x = costume, y = n)) +
      geom_col(aes(fill = rank)) +
      coord_flip() +
      scale_fill_manual(values = c("black", "purple", "orange")) +
      facet_grid(region_us_census~.) +
      theme_minimal() # plots the data
    
      })
  
  } #Server end

#### Full App ####
shinyApp(ui = ui, server = server)

# End of R Shiny app script.

