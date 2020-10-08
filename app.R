#### Aquatic Microplastics Toxicology Shiny App
#### File created: September 23, 2020
#### Code contributors: Heili Lowman, Leah Thornton Hampton, Scott Coffin, Emily Darin

#### Setup ####

# Anything that should only happen ONCE should be placed in the setup section, prior to the actual shiny structure.

# Load packages
library(tidyverse)
library(patchwork)
library(calecopal)
library(shiny)
library(shinythemes)
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
aoc$log_dose.mg.L <- log10(aoc$dose.mg.L)
aoc$log_dose.particles.mL <- log10(aoc$dose.particles.mL)

# Add factor and releved effects column.
aoc$effect_f <- factor(aoc$effect, levels = c("Y", "N"))

#### Leah Setup ####

#### Emily Setup ####

#Data frame for Shape

shape<-data.frame(Shape=c("cube","fiber","fragment","sphere"), N =c(100,70,79,86), Y =c(0,30,21,14))
shape_data<-melt(shape, id.vars='Shape') 

#Data frame for Size

size_<-data.frame(size=c("<1um","1um < 10um","10um < 100um","100um<1mm","1mm < 5mm"), N =c(62,60,71,79,70), Y =c(38,40,29,21,30))
size.class<-melt(size_, id.vars='size')

#Data frame for Polymer 

poly_<-data.frame(poly=c("BIO","EVA","LTX","PA","PC","PE","PET","PI","PLA","PMMA","PP","PS","PUR","PVC"), N =c(4,71,100,76,71,74,88,100,84,64,66,70,0,64), Y =c(96,29,0,24,29,26,12,0,16,36,34,30,100,36))
poly.class<-melt(poly_, id.vars='poly')

#Data frame for taxonomic group

tax<-data.frame(taxa=c("Algae","Annelida","Bacterium","Cnidaria","Crustacea","Echinoderm","Fish","Insect","Mollusca","Nematoda","Plant","Rotifera"), N =c(56,79,32,72,70,84,74,41,79,20,73,54), Y =c(44,21,68,28,30,16,26,59,21,80,27,46))
tax.class<-melt(tax, id.vars='taxa') 

# Data frame for lvl 

lvl_1<-data.frame(lvl1=c("Alimentary/excretory","Behavioral/sense/neuro","Circulatory.respiratory","Community","Fitness","Immune","Metabolism","Microbiome","Stress"), N =c(67,70,70,97,97,67,66,67,62), Y =c(33,30,30,3,3,33,34,33,38))
lvl1.class<-melt(lvl_1, id.vars='lvl1') 

# Data frame for life stage 

life_1<-data.frame(life=c("Early","Juvenile","Adult"), N =c(74,70,76), Y =c(26,30,24))
life.class<-melt(life_1, id.vars='life') 

# Data frame for vivo graph

vivo_1<-data.frame(vivo=c("Invitro","Invivo"), N =c(19,72), Y =c(81,28))
vivo.class<-melt(vivo_1, id.vars='vivo') 

# Data frame for exposure route 

route_1<-data.frame(route=c("Food","Maternal.Transfer","Sediment","Water"), N =c(82,60,79,70), Y =c(18,40,21,30))
route.class<-melt(route_1, id.vars='route') 

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
  mutate(lvl1_f = factor(lvl1_cat)) # order different endpoints.

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
  drop_na(dose.mg.L) 

#SSD package depends on specific naming conventions. Prep factors accordingly below
aoc_z$Conc <- aoc_z$dose.mg.L #must make value named 'Conc' for this package
aoc_z$Species <-paste(aoc_z$genus,aoc_z$species) #must make value 'Species" (uppercase)
aoc_z$Group <- as.factor(aoc_z$organism.group) #must make value "Group"
aoc_z$Group <- fct_explicit_na(aoc_z$Group) #makes sure that species get counted even if they're missing a group


# Create Shiny app. Anything in the sections below (user interface & server) should be the reactive/interactive parts of the shiny application.

#### User Interface ####
ui <- fluidPage(
  
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
                    
                    img(src = "data_categories_image.png", height = 400, width = 400, style = "display:block;margin-left: auto; margin-right: auto;"),
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
                  
                    h3("Contact", align = "center", style = "color:darkcyan"),
                    
                    p("For more information about the database or other questions, please contact Dr. Leah Thornton Hampton (leahth@sccwrp.org)."),
                    
                    br(),
                    
                    img(src = "sccwrp.png", height = 100, width = 100, style = "display:block;margin-left: auto; margin-right: auto;"),
                    
                    br(), 
                    
                    verbatimTextOutput(outputId = "Leah1")),
                
                  tabPanel("Resources", 
                           
                      h3(align = "center", a(href = "https://sccwrp-my.sharepoint.com/:b:/g/personal/leahth_sccwrp_org/Eb8XXdAvn9BBpOB6Z6klzEcBlb6mFpJcYJrHBAQk7r1z3A?e=tRTqDM", 'Data Category Descriptions')),
                      br(),
                      h3(align = "center", a(href = "https://sccwrp-my.sharepoint.com/:b:/g/personal/leahth_sccwrp_org/EXDS25x3JAJHhZAj3qDwWgIBeB-oz0mIihclR2oOckPjhg?e=GtOeB5", 'Aquatic Organisms Study List')),
                      br(),
                      h3(align = "center", a(href = "https://sccwrp-my.sharepoint.com/:b:/g/personal/leahth_sccwrp_org/ES_FUiwiELtNpWgrPCS1Iw4Bkn3-aeiDjZxmtMLjg3uv3g?e=bmuNgG", 'Human Study List')),
                           
                    verbatimTextOutput(outputId = "Leah2")),
        
#### Emily UI ####
                  tabPanel("Data Overview", 
                    br(), # line break
                    h3("Measured Effects of Different Shapes", align = "center", style = "color:darkcyan"),
                    br(), # line break
                    plotOutput(outputId = "shape_plot"),

            br(), # line break
            h3("Measured Effects of Different Size Categories", align = "center", style = "color:darkcyan"),
            br(), # line break
            plotOutput(outputId = "size_plot"),

            br(), # line break
            h3("Measured Effects of Different Polymers", align = "center", style = "color:darkcyan"),
            br(), # line break
            plotOutput(outputId = "poly_plot"),

            br(), # line break
            p("Measured Effects by Taxonomic Group"),
            br(), # line break
            plotOutput(outputId = "tax_plot"),

            br(), # line break
            p("Measured Effects of Life Stages"),
            br(), # line break
            plotOutput(outputId = "life_plot")), #second parenthese closes out tab

            
      #### Heili UI ####
                  tabPanel("Data Exploration", 
                    br(), # line break
                    p("The figures below display data from the literature review of toxicological effects of microplastics on aquatic organisms. All data displayed - individual points and boxplots - are from studies in which there was a demonstrated significant toxicological effect of microplastics."),
                    br(), # line break
                    p("Each row of figures displays a different value along the y-axis - size, shape, and polymer, respectively. Each column of figures displays a different unit along the x-axis - mg/L and particles/mL, respectively. The data may be filtered by organism and/or endpoint using the selection widgets on the left-hand side of the window."),
                    br(), # line break
                    
                    sidebarPanel("Use the options below to filter the dataset.",
                      br(), # line break
                      
                      checkboxGroupInput(inputId = "organism_check", # organismal checklist
                        label = "Organisms:",
                        choices = levels(aoc_y$org_f), 
                        selected = levels(aoc_y$org_f)), # Default is to have everything selected.
                      br(),
                      
                      checkboxGroupInput(inputId = "lvl1_check", # endpoint checklist
                        label = "Endpoint Examined:",
                        choices = levels(aoc_y$lvl1_f), 
                        selected = levels(aoc_y$lvl1_f)), # Default is to have everything selected.
                      br()), # line break
                    
                    mainPanel("Microplastics in Aquatic Environments: Data Exploration of Toxicological Effects",
                      br(), # line break
                      plotOutput(outputId = "size_plot_react"),
                      br(), # line break
                      plotOutput(outputId = "shape_plot_react"),
                      br(), # line break
                      plotOutput(outputId = "poly_plot_react"))), 
        
#### Scott UI ####
                  tabPanel("Species Sensitivity Distribution", 
                    br(), # line break
                    h3("Species Sensitivity Distribution", align = "center", style = "color:darkcyan"),
                    p("Species sensitivity distributions (SSDs) are cumulative probability distributions that estimate the percent of species affected by a given concentration of exposure using Maximum Likelihood and model averaging. A useful metric often used for setting risk-based thresholds is the concentration that affects 5% of the species, and is reffered to as the 5% Hazard Concentration (HC). For more information on SSDs, refer to Posthuma, Suter II, and Traas (2001)."),
                    br(), # line break
                    
                    sidebarPanel("Use the options below to filter the dataset. NOTE: changes may take a long time to appear",
                                 br(), # line break
                                 br(),
                                 checkboxGroupInput(inputId = "env_check_ssd", # environment checklist
                                                    label = "Environment:",
                                                    choices = levels(aoc_z$env_f), 
                                                    selected = levels(aoc_z$env_f)), # Default is to have everything selected.
                                 br(),
                                 
                                 checkboxGroupInput(inputId = "size_check_ssd", # organismal checklist
                                                    label = "Sizes:",
                                                    choices = levels(aoc_z$size_f), 
                                                    selected = levels(aoc_z$size_f)), # Default is to have everything selected.
                                 br(),
                                 
                                 checkboxGroupInput(inputId = "lvl1_check_ssd", # endpoint checklist
                                                    label = "Endpoint Examined:",
                                                    choices = levels(aoc_z$lvl1_f), 
                                                    selected = levels(aoc_z$lvl1_f)), # Default is to have everything selected.
                                 br(), # line break 
                                 
                                 checkboxGroupInput(inputId = "polyf_check_ssd", # endpoint checklist
                                       label = "Polymers Examined:",
                                       choices = levels(aoc_z$poly_f), 
                                       selected = levels(aoc_z$poly_f)), # Default is to have everything selected.
                                 br(), #line break
                                 
                                 actionButton("SSDButton", "Submit"),
                    
                    br()), # line break

                    
                    mainPanel("Microplastics in Aquatic Environments: Species Sensitivity Distributions",
                              br(), # line break
                              br(),
                              p("The figure below displays minimum observed effect concentrations for a range of species along with three common distributions"),
                              br(),
                              plotOutput(outputId = "autoplot_dists_react"),
                              p("Different distributions can be fit to the data. Below are some common distributions (llogis = log-logistic; lnorm = log-normal). Given multiple distributions, choose the best fitting distribution."),
                              p("Goodness of Fit Table"),
                              DT::dataTableOutput(outputId = "table_gof_react"), #using DT package provides better functionality
                              p("KEY:"),
                              p("ad = Anderson-Darling statistic; ks = Kolmogorov-Smirnov statistic; cvm = Cramer-von Mises statistic; aic = Akaike's Information Criterion; aicc = Akaike's Information Criterion corrected for sample size; bic = Bayesian Information Criterion"),
                              p("Following Burnham and Anderson (2002) we recommend the aicc for model selection. The best fitting model is that with the lowest aicc (indicated by the model with a delta value in the goodness of fit table). For further information on the advantages of an information theoretic approach in the context of selecting SSDs the reader is referred to Schwarz and Tillmanns (2019)."),
                              br(),
                              p("Species Sensitivity Distribution"),
                              plotOutput(outputId = "SSD_plot_react"),
                              br(),
                              p("The model-averaged 95% confidence interval is indicated by the shaded band and the model-averaged 5% Hazard Concentration (HC5) by the dotted line."),
                              br(),
                              p("This app is built from the R package ssdtools version 0.3.2, and share the same functionality. Citation: Thorley, J. and Schwarz C., (2018). ssdtools An R package to fit pecies Sensitivity Distributions. Journal of Open Source Software, 3(31), 1082. https://doi.org/10.21105/joss.01082.")
                              ))
                    

#following three parentheses close out UI. Do not delete. 
        )))   
        #))  #comment-out these two parentheses. they must be here, but need to figure out where forward parentheses need to be. 

#### Server ####
server <- function(input, output) {
  
#### Leah S ####
  output$Leah1 <- renderText({
    #paste0("You can also add outputs like this. Every output (text, plot, table) has a render function equivalent (renderText, renderPlot, renderTable).")
  })
  
  output$Leah2 <- renderText({
  
  })
  
#### Emily S ####
  
  output$Emily1 <- renderText({
    paste0("You can also add outputs like this. Every output (text, plot, table) has a render function equivalent (renderText, renderPlot, renderTable).")
  })

  # Stacked bar chart for Shape 
  
  output$shape_plot <- renderPlot({ggplot(shape_data, aes(fill=variable, y=value, x=Shape)) + 
    geom_bar(position="stack", stat="identity")+
    geom_text(x=1,y=60, label="100%", size=4.5, color="white")+
    geom_text(x=2,y=60, label="70%", size=4.5, color="white")+
    geom_text(x=2,y=15, label="30%", size=4.5, color="white")+
    geom_text(x=3,y=60, label="79%", size=4.5, color="white")+
    geom_text(x=3,y=13, label="21%", size=4.5, color="white")+
    geom_text(x=4,y=60, label="86%", size=4.5, color="white")+
    geom_text(x=4,y=5, label="14%", size=4.5, color="white")+
    annotate("text", x=1:4,y=105,label=c("4", "105","2,104","2,366"),size=4.5,color="Chocolate3")+
    scale_fill_manual(values = cal_palette("wetland")) + 
    labs(x = "Microplastic Shape",
         color = "System") +
    theme_classic() +
    theme(legend.position = "right")+
    labs(fill="Effect")+
    theme(axis.ticks=element_blank(),axis.text.y=element_blank(),axis.title.y = element_blank())})
  

  
  #Stacked bar chart for Size 
  
  output$size_plot<-renderPlot({ggplot(size.class, aes(fill=variable, y=value, x=size))+ 
    geom_bar(position="stack", stat="identity")+
    scale_fill_manual(values = cal_palette("halfdome"))+
    labs(x = "Size Category",
         color = "System")+
    annotate("text", x=1:5, y=70, label=c("62%","60%","71%","79%","70%"),size=4.5,color="white")+
    annotate("text",x=1:5,y=105, label=c("373","484","1975","1773","138"),size=4.5,color="lightsteelblue4")+
    geom_text(x=1,y=15,label="38%",size=4.5,color="white")+
    geom_text(x=2,y=12,label="40%",size=4.5,color="white")+
    geom_text(x=3,y=20,label="29%",size=4.5,color="white")+
    geom_text(x=4,y=17,label="21%",size=4.5,color="white")+
    geom_text(x=5,y=20,label="30%",size=4.5,color="white")+
    theme_classic()+ 
    theme(legend.position = "right")+
    labs(fill="Effect")+
    theme(axis.ticks=element_blank(),axis.text.y=element_blank(),axis.title.y = element_blank())})
  
  #Stacked bar chart for Polymer 
  
  output$poly_plot<-renderPlot({ggplot(poly.class, aes(fill=variable, y=value, x=poly))+ 
    geom_bar(position="stack", stat="identity")+
    scale_fill_manual(values = cal_palette("tidepool"))+
    labs(x = "Polymer Type",
         color = "System")+
    annotate("text",x=1,y=98,label=c("4%"),size=4.5,color="white")+
    annotate("text", x=2:12, y=70, label=c("71%","100%","76%","71%","74%","88%","100%","84%","64%","66%","70%"),size=4.5,color="white")+
    annotate("text",x=14,y=65,label=c("64%"),size=4.5,color="white")+
    geom_text(x=1,y=50,label="96%",size=4.5,color="white")+
    geom_text(x=2,y=15,label="29%",size=4.5,color="white")+
    geom_text(x=4,y=12,label="24%",size=4.5,color="white")+
    geom_text(x=5,y=15,label="29%",size=4.5,color="white")+
    geom_text(x=6,y=15,label="26%",size=4.5,color="white")+
    geom_text(x=7,y=8,label="22%%",size=4.5,color="white")+
    geom_text(x=9,y=6,label="26%",size=4.5,color="white")+
    geom_text(x=10,y=20,label="36%",size=4.5,color="white")+
    geom_text(x=11,y=18,label="32%",size=4.5,color="white")+
    geom_text(x=12,y=16,label="30%",size=4.5,color="white")+
    geom_text(x=13,y=60,label="100%",size=4.5,color="white")+
    geom_text(x=14,y=20,label="36%",size=4.5,color="white")+
    annotate("text", x=1:14,y=105,label=c("5","7","4","54","28","1,583","175","1","39","22","224","2,164","1","304"),size=4.5,color="darkslategray")+
    theme_classic()+ 
    theme(legend.position = "right")+
    labs(fill="Effect")+
    theme(axis.ticks=element_blank(),axis.text.y=element_blank(),axis.title.y = element_blank())})
  
    # Stacked bar for Taxonomic level 
  
  output$tax_plot<-renderPlot({ggplot(tax.class, aes(fill=variable, y=value, x=taxa))+ 
      geom_bar(position="stack", stat="identity")+
      scale_fill_manual(values = cal_palette("dudleya"))+
      labs(x = "Taxonomic Group",
           color = "System")+
      annotate("text", x=1:12,y=12,label=c("44%","21%","68%","28%","30%","16%","26%","59%","21%","80%","27%","46%"),size=4.5,color="darkolivegreen")+
      geom_text(x=1,y=60,label="56%",size=4.5,color="white")+
      geom_text(x=2,y=60,label="79%",size=4.5,color="white")+
      geom_text(x=3,y=85,label="32%",size=4.5,color="white")+
      geom_text(x=4,y=60,label="72%",size=4.5,color="white")+
      geom_text(x=5,y=60,label="70%",size=4.5,color="white")+
      geom_text(x=6,y=60,label="84%",size=4.5,color="white")+
      geom_text(x=7,y=60,label="74%",size=4.5,color="white")+
      geom_text(x=8,y=85,label="41%",size=4.5,color="white")+
      geom_text(x=9,y=60,label="79%",size=4.5,color="white")+
      geom_text(x=10,y=90,label="20%",size=4.5,color="white")+
      geom_text(x=11,y=60,label="73%",size=4.5,color="white")+
      geom_text(x=12,y=60,label="54%",size=4.5,color="white")+
      annotate("text",x=1:12,y=105,label=c("355","243","84","80","1151","65","1584","45","1011","63","33","80"),size=4.5,color="goldenrod4")+
      theme_classic()+ 
      theme(legend.position = "right")+
      labs(fill="Effect")+
      theme(axis.ticks=element_blank(),axis.text.y=element_blank(),axis.title.y = element_blank())})
  
      # Stacked bar chart lvl_1
  
  output$lvl1_plot<-renderPlot({ggplot(lvl1.class, aes(fill=variable, y=value, x=lvl1))+ 
      geom_bar(position="stack", stat="identity")+
      scale_fill_manual(values = cal_palette("vermillion"))+
      labs(x = "Endpoint Category",
           color = "System")+
      annotate("text", x=1:9,y=70,label=c("67%","70%","70%","97%","97%","67%","66%","67%","62%"),size=4.5,color="white")+
      annotate("text",x=1:3,y=15,label=c("33%","30%","30%"),size=4.5,color="white")+
      annotate("text",x=4:5,y=5,label=c("3%","3%"),size=4.5,color="white")+
      annotate("text",x=6:9,y=15,label=c("33%","34%","33%","38%"),size=4.5,color="white")+
      annotate("text",x=1:9,y=105,label=c("280","448","131","68","2,009","293","1,481","55","107"),size=4.5,color="indianred3")+
      theme_classic()+ 
      theme(legend.position = "right")+
      labs(fill="Effect")+
      theme(axis.ticks=element_blank(),axis.text.y=element_blank(),axis.title.y = element_blank())})
    
  
  # Stacked bar chart for Life Stage 
  
  output$life_plot<-renderPlot({ggplot(life.class, aes(fill=variable, y=value, x=life)) + 
      geom_bar(position="stack", stat="identity")+
      annotate("text",x= 1:3,y=70, label=c("74%","70%","76%"),size=4.5,color="dodgerblue4")+
      annotate("text",x= 1:3,y=15, label=c("26%","30%","24%"),size=4.5,color="white")+
      annotate("text", x=1:3,y=105,label=c("4", "105","2,104"),size=4.5,color="dodgerblue4")+
      scale_fill_manual(values = cal_palette("sbchannel")) + 
      labs(x = "Life Stage",
           color = "System") +
      theme_classic() +
      theme(legend.position = "right")+
      labs(fill="Effect")+
      theme(axis.ticks=element_blank(),axis.text.y=element_blank(),axis.title.y = element_blank())})
  
  # Stacked bar for Invitro/Invivo
  
  output$vivo_plot<-renderPlot({ggplot(vivo.class, aes(fill=variable, y=value, x=vivo)) + 
    geom_bar(position="stack", stat="identity")+
    geom_text(x=1,y=50,label="81%",size=4.5,color="white")+
    geom_text(x=2,y=15,label="28%",size=4.5,color="white")+
    geom_text(x=1,y=90,label="19%",size=4.5,color="darkolivegreen3")+
    geom_text(x=2,y=50,label="72%",size=4.5,color="darkolivegreen3")+
    annotate("text", x=1:2, y=105, label=c("91","109"),size=4.5,color="darkolivegreen3")+
    scale_fill_manual(values = cal_palette("arbutus"))+ 
    labs(x = "Invitro/Invivo",
         color = "System") +
    theme_classic() +
    theme(legend.position = "right")+
    labs(fill="Effect")+
    theme(axis.ticks=element_blank(),axis.text.y=element_blank(),axis.title.y = element_blank())})
  
    
    #Stacked bar for Exposure Route 
  
  output$route_plot<-renderPlot({ggplot(route.class, aes(fill=variable, y=value, x=route))+ 
    geom_bar(position="stack", stat="identity")+
    annotate("text",x=1:4,y=60,label=c("82%","60%","79%","70%"),size=4.5,color="darkgoldenrod3")+
    annotate("text",x=1:4,y=15,label=c("18%","40%","21%","30%"),size=4.5,color="darkgoldenrod3")+
    annotate("text", x=1:4, y=105, label=c("605","10","488","3643"),size=4.5,color="darkgoldenrod3")+
    scale_fill_manual(values = cal_palette("desert"))+ 
    labs(x = "Exposure Route",
    color = "System") +
    theme_classic() +
    theme(legend.position = "right")+
    labs(fill="Effect")+
    theme(axis.ticks=element_blank(),axis.text.y=element_blank(),axis.title.y = element_blank())})
  
  
#### Heili S ####
  
  # Create new dataset based on widget filtering.
  aoc_filter <- reactive({
    aoc_y %>%
      filter(org_f %in% input$organism_check) %>%
      filter(lvl1_f %in% input$lvl1_check)
  })
  
  # Use newly created dataset from above to generate patchwork plots for size, shape, and polymer plots on three different rows (for sizing display purposes).
  
  output$size_plot_react <- renderPlot({
     
    size1 <- ggplot(aoc_filter(), aes(x = dose.mg.L, y = size_f)) +
      scale_x_log10(breaks = c(0.0001, 0.01, 1, 100, 10000), 
        labels = c(0.0001, 0.01, 1, 100, 10000)) +
      geom_boxplot(alpha = 0.7, show.legend = FALSE, aes(color = size_f, fill = size_f)) +
      scale_color_manual(values = cal_palette("sbchannel", n = 6, type = "continuous")) +
      scale_fill_manual(values = cal_palette("sbchannel", n = 6, type = "continuous")) +
      #geom_jitter(size = 3, alpha = 0.2, height = 0.1, color = "grey80") +
      theme_classic() +
      theme(legend.position="none") +
      labs(x = "Concentration (mg/L)",
        y = "Size")
    
    size2 <- ggplot(aoc_filter(), aes(x = dose.particles.mL, y = size_f)) +
      scale_x_log10(breaks = c(1, 10000, 100000000, 1000000000000, 10000000000000000), 
        labels = c(1, 10000, 100000000, 1000000000000, 10000000000000000)) +
      geom_boxplot(alpha = 0.7, show.legend = FALSE, aes(color = size_f, fill = size_f)) +
      scale_color_manual(values = cal_palette("sbchannel", n = 6, type = "continuous")) +
      scale_fill_manual(values = cal_palette("sbchannel", n = 6, type = "continuous")) +
      #geom_jitter(size = 3, alpha = 0.2, height = 0.1, color = "grey80") +
      theme_classic() +
      theme(legend.position="none") +
      labs(x = "Concentration (particles/mL)",
        y = " ")
    
    (size1 + size2)
    
  })
  
  output$shape_plot_react <- renderPlot({
    
    shape1 <- ggplot(aoc_filter(), aes(x = dose.mg.L, y = shape_f)) +
      scale_x_log10(breaks = c(0.0001, 0.01, 1, 100, 10000), 
        labels = c(0.0001, 0.01, 1, 100, 10000)) +
      geom_boxplot(alpha = 0.7, show.legend = FALSE, aes(color = shape_f, fill = shape_f)) +
      scale_color_manual(values = cal_palette("chaparral3")) +
      scale_fill_manual(values = cal_palette("chaparral3")) +
      #geom_jitter(size = 3, alpha = 0.2, height = 0.1, color = "grey80") +
      theme_classic() +
      theme(legend.position="none") +
      labs(x = "Concentration (mg/L)",
        y = "Shape")
    
    shape2 <- ggplot(aoc_filter(), aes(x = dose.particles.mL, y = shape_f)) +
      scale_x_log10(breaks = c(1, 10000, 100000000, 1000000000000, 10000000000000000), 
        labels = c(1, 10000, 100000000, 1000000000000, 10000000000000000)) +
      geom_boxplot(alpha = 0.7, show.legend = FALSE, aes(color = shape_f, fill = shape_f)) +
      scale_color_manual(values = cal_palette("chaparral3")) +
      scale_fill_manual(values = cal_palette("chaparral3")) +
      #geom_jitter(size = 3, alpha = 0.2, height = 0.1, color = "grey80") +
      theme_classic() +
      theme(legend.position="none") +
      labs(x = "Concentration (particles/mL)",
        y = " ")
    
    (shape1 + shape2)
    
  })
  
  output$poly_plot_react <- renderPlot({
    
    poly1 <- ggplot(aoc_filter(), aes(x = dose.mg.L, y = poly_f)) +
      scale_x_log10(breaks = c(0.0001, 0.01, 1, 100, 10000), 
        labels = c(0.0001, 0.01, 1, 100, 10000)) +
      geom_boxplot(alpha = 0.7, show.legend = FALSE, aes(color = poly_f, fill = poly_f)) +
      scale_color_manual(values = cal_palette("canary", n = 15, type = "continuous")) +
      scale_fill_manual(values = cal_palette("canary", n = 15, type = "continuous")) +
      #geom_jitter(size = 3, alpha = 0.2, height = 0.1, color = "grey80") +
      theme_classic() +
      theme(legend.position="none") +
      labs(x = "Concentration (mg/L)",
        y = "Polymer")
    
    poly2 <- ggplot(aoc_filter(), aes(x = dose.particles.mL, y = poly_f)) +
      scale_x_log10(breaks = c(1, 10000, 100000000, 1000000000000, 10000000000000000), 
        labels = c(1, 10000, 100000000, 1000000000000, 10000000000000000)) +
      geom_boxplot(alpha = 0.7, show.legend = FALSE, aes(color = poly_f, fill = poly_f)) +
      scale_color_manual(values = cal_palette("canary", n = 15, type = "continuous")) +
      scale_fill_manual(values = cal_palette("canary", n = 15, type = "continuous")) +
      #geom_jitter(size = 3, alpha = 0.2, height = 0.1, color = "grey80") +
      theme_classic() +
      theme(legend.position="none") +
      labs(x = "Concentration (particles/mL)",
        y = " ")
    
    (poly1 + poly2)
    
  })

#### Scott S ####
  
  
  # Create new dataset based on widget filtering. Note: copied from Heili
  # dependency on input$SSDButton
  
  #eventReactive(input$SSDButton, {
               
  aoc_filter_ssd <- reactive({
    
    aoc_z %>%
      filter(env_f %in% input$env_check_ssd) %>%
      filter(size_f %in% input$size_check_ssd) %>%
      filter(lvl1_f %in% input$lvl1_check_ssd) %>%
      filter(poly_f %in% input$polyf_check_ssd) %>%
    group_by(Species, Group) %>% 
      summarise(Conc = min(Conc)) #set concentration to minimum observed effect
    })
  #})
  
  # Use newly created dataset from above to generate SSD
  
  #create distribution based on newly created dataset
  fit_dists <- reactive({
    ssd_fit_dists(aoc_filter_ssd())
  }) 
  
  #create an autoplot of the distributions
  output$autoplot_dists_react <- renderPlot({
    autoplot(fit_dists())
  })
  
  #Make a dataframe (aoc_pred) of the estimated concentration (est) with standard error (se) and lower (lcl) and upper (ucl) 95% confidence limits by percent of species affected (percent). The confidence limits are estimated using parametric bootstrapping.
    aoc_pred <- reactive({
    set.seed(99)
    predict(fit_dists(), ci= TRUE) #estimates model-averaged estimates based on aicc
  }) 
 
  #Render table for goodness of fit
  output$table_gof_react <- DT::renderDataTable({
    req(fit_dists())
    gof <- ssd_gof(fit_dists()) %>%
      mutate_if(is.numeric, ~ signif(., 3)) %>%
      arrange(delta) #orders by delta of fit
    gof
  })

#Create the plot for species sensitivity distribution
  output$SSD_plot_react <- renderPlot({
    ssd_plot(aoc_filter_ssd(), aoc_pred(), #native ggplot with SSD package
                   color = "Group", label = "Species",
                   xlab = "Concentration (mg/L)", ribbon = TRUE) +
      scale_fill_viridis_d() + #make colors more differentiable 
      scale_colour_viridis_d() + #make colors more differentiable 
      expand_limits(x = 5000) + # to ensure the species labels fit
            ggtitle("Species Sensitivity for Microplastics")
      })
  
  } #Server end

#### Full App ####
shinyApp(ui = ui, server = server)

# End of R Shiny app script.

