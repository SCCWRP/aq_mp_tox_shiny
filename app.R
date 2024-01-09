#### Aquatic Microplastics Toxicology Shiny App
#### File created: September 23, 2020
#### Code contributors: Heili Lowman, Leah Thornton Hampton, Scott Coffin, Emily Darin

#### Setup ####

# Load packages
library(tidyverse) #General everything
library(shinydashboard)
library(RColorBrewer) #color palette
library(ggplot2) #plotting
library(calecopal) #Color palette devtools::install_github("an-bui/calecopal")
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
library(msm) ## rtnorm - get upper and lower limit of shape distribution
library(GeneralizedHyperbolic) ## normal-inverse Gaussian
library(stats)
library(caret) # for random forest predictions
library(randomForest) # for random forest predictions

#### Load finalized dataset (prepped in RDAmaker.R) ####
aoc <- readRDS("aoc_setup_tomex2.RDS")
aoc_endpoint <- readRDS("aoc_endpoint_tomex2.RDS")
aoc_quality <- readRDS("aoc_quality_tomex2.RDS")
aoc_search <- readRDS("aoc_search_tomex2.RDS")
aoc_setup <- readRDS("aoc_setup_tomex2.RDS")
# aoc_v1 <- readRDS("aoc_v1.RDS")
aoc_z <- readRDS("aoc_z_tomex2.RDS")

#prediction models generated in aq_mp_tox_modelling repo (Scott_distributions_no_touchy.Rmd)
predictionModel_tissue.translocation <- readRDS("prediction/randomForest_oxStress.rds")
predictionModel_food.dilution <- readRDS("prediction/randomForest_foodDilution.rds")
test_data_prediction <- readr::read_csv("prediction/test_data_prediction.csv") %>% mutate_if(is.character, factor) %>%  dplyr::select(-`...1`) #contains spaces!
test_data_calculator <- read.csv("calculator/test_data_calculator.csv", stringsAsFactors = TRUE)
valid_values <- readr::read_csv("prediction/valid_values.csv") %>%  dplyr::select(-`...1`) #contains spaces!test_data_calculator <- read.csv("calculator/test_data_calculator.csv", stringsAsFactors = TRUE
train_data_prediction <- readr::read_csv("prediction/training_data_prediction.csv") %>% mutate_if(is.character, factor) %>%  dplyr::select(-`...1`) #contains spaces!


##### Load functions #####
source("functions.R")

#### Welcome Setup ####

#### Overview Setup ####

polydf<-rowPerc(xtabs( ~poly_f +effect_f, aoc)) #pulls polymers by effect 
polyf<-as.data.frame(polydf)%>% #Makes data frame
  # mutate(effect = case_when(effect == "Y" ~ "Yes",
  #                           effect == "N" ~ "No")) %>% 
  filter(effect_f %in% c("Yes","No"))%>% #Sorts into Yes and No
  # mutate(polymer = factor(case_when(
  #   polymer == "BIO" ~ "Biopolymer",
  #   polymer == "EVA" ~ "Polyethylene Vinyl Acetate",
  #   polymer == "LTX" ~ "Latex",
  #   polymer == "PA" ~ "Polyamide",
  #   polymer == "PE" ~ "Polyethylene",
  #   polymer == "PC" ~ "Polycarbonate",
  #   polymer == "PET" ~ "Polyethylene Terephthalate",
  #   polymer == "PI" ~ "Polyisoprene",
  #   polymer == "PMMA" ~ "Polymethylmethacrylate",
  #   polymer == "PP" ~ "Polypropylene",
  #   polymer == "PS" ~ "Polystyrene",
  #   polymer == "PUR" ~ "Polyurethane",
  #   polymer == "PVC" ~ "Polyvinylchloride",
  #   polymer == "PLA" ~ "Polylactic Acid",
  #   polymer == "Not Reported" ~ "Not Reported"))) %>%
  mutate_if(is.numeric, round,0) #rounds percents 

Endpoints<-xtabs(~poly_f +effect_f ,aoc) #Pulls all study obs. for polymer from dataset

polyfinal<- data.frame(cbind(polyf, Endpoints))%>% #adds it as a column
  rename(Endpoints='Freq.1')%>% #renames column
  mutate(logEndpoints = log(Endpoints))%>%
  rename(Percent = Freq)#renames column

sizedf<-rowPerc(xtabs(~size_f +effect_f, aoc))

sizef<-as.data.frame(sizedf)%>%
  # mutate(effect = case_when(effect == "Y" ~ "Yes",
  #                           effect == "N" ~ "No")) %>%
  filter(effect_f %in% c("Yes","No"))%>%
  # mutate(size.category = case_when(
  #   size.category == 1 ~ "1nm < 100nm",
  #   size.category == 2 ~ "100nm < 1µm",
  #   size.category == 3 ~ "1µm < 100µm",
  #   size.category == 4 ~ "100µm < 1mm",
  #   size.category == 5 ~ "1mm < 5mm",
  #   size.category == 0 ~ "Not Reported"))%>%
  rename(Type = "size_f")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Size")

study_s<-xtabs(~size_f +effect_f,aoc)

sizefinal<- data.frame(cbind(sizef, study_s))%>%
  rename(Endpoints='Freq.1')%>%
  rename(category='size_f')%>%
  mutate(logEndpoints = log(Endpoints))%>%
  rename(Percent = Freq)#renames column

shapedf<-rowPerc(xtabs(~shape_f + effect_f, aoc))

shapef<-as.data.frame(shapedf)%>%
  # mutate(effect = case_when(effect == "Y" ~ "Yes",
  #                           effect == "N" ~ "No")) %>%
  filter(effect_f %in% c("Yes","No"))%>%
  rename(Type="shape_f")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Shape")
  # mutate(Type = case_when(
  #   Type == "sphere" ~ "Sphere",
  #   Type == "fragment" ~ "Fragment",
  #   Type == "fiber" ~ "Fiber"))

study_sh<-xtabs(~shape_f + effect_f,aoc)

shapefinal<- data.frame(cbind(shapef, study_sh))%>%
  rename(Endpoints='Freq.1')%>%
  rename(category='shape_f')%>%
  mutate(logEndpoints = log(Endpoints))%>%
  rename(Percent = Freq)#renames column

taxdf<-rowPerc(xtabs(~org_f +effect_f, aoc))

taxf<-as.data.frame(taxdf)%>%
  # mutate(effect = case_when(effect == "Y" ~ "Yes",
  #                           effect == "N" ~ "No")) %>%
  filter(effect_f %in% c("Yes","No"))%>%
  rename(Type= "org_f")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Organism")

study_t<-xtabs(~org_f +effect_f,aoc)

taxfinal<- data.frame(cbind(taxf, study_t))%>%
  rename(Endpoints='Freq.1')%>%
  rename(category='org_f')%>%
  mutate(logEndpoints = log(Endpoints))%>%
  rename(Percent = Freq)#renames column

lvl1df<-rowPerc(xtabs(~lvl1_f +effect_f, aoc))

lvl1f<-as.data.frame(lvl1df)%>%
  # mutate(effect = case_when(effect == "Y" ~ "Yes",
  #                           effect == "N" ~ "No")) %>%
  filter(effect_f %in% c("Yes","No"))%>%
  rename(Type= "lvl1_f")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Lvl1")
  # mutate(Type = case_when(
  #   Type == "alimentary.excretory" ~ "Alimentary, Excretory",
  #   Type == "behavioral.sense.neuro" ~ "Behavioral, Sensory, Neurological",
  #   Type == "circulatory.respiratory" ~ "Circulatory, Respiratory",
  #   Type == "community" ~ "Community",
  #   Type == "fitness" ~ "Fitness",
  #   Type == "immune" ~ "Immune",
  #   Type == "metabolism" ~ "Metabolism",
  #   Type == "microbiome" ~ "Microbiome",
  #   Type == "stress" ~ "Stress"))

study_l<-xtabs(~lvl1_f +effect_f,aoc)

lvl1final<- data.frame(cbind(lvl1f, study_l))%>%
  rename(Endpoints='Freq.1')%>%
  rename(category='lvl1_f')%>%
  mutate(logEndpoints = log(Endpoints))%>%
  rename(Percent = Freq)#renames column

lifedf<-rowPerc(xtabs(~life_f +effect_f, aoc))

lifef<-as.data.frame(lifedf)%>%
  # mutate(effect = case_when(effect == "Y" ~ "Yes",
  #                           effect == "N" ~ "No")) %>%
  filter(effect_f %in% c("Yes","No"))%>%
  rename(Type= "life_f")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Life.stage")

studyli<-xtabs(~life_f +effect_f ,aoc)

lifefinal<- data.frame(cbind(lifef, studyli))%>%
  rename(Endpoints='Freq.1')%>%
  rename(category='life_f')%>%
  mutate(logEndpoints = log(Endpoints))%>%
  rename(Percent = Freq)#renames column

vivodf<-rowPerc(xtabs(~vivo_f +effect_f, aoc))

vivof<-as.data.frame(vivodf)%>%
  # mutate(effect = case_when(effect == "Y" ~ "Yes",
  #                           effect == "N" ~ "No")) %>%
  filter(effect_f %in% c("Yes","No"))%>%
  rename(Type= "vivo_f")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Invivo.invivo")
  # mutate(Type = case_when(
  #   Type=="invivo"~"In Vivo",
  #   Type=="invitro"~"In Vitro"))

study_v<-xtabs(~vivo_f +effect_f,aoc)

vivofinal<- data.frame(cbind(vivof, study_v))%>%
  rename(Endpoints='Freq.1')%>%
  rename(category='vivo_f')%>%
  mutate(logEndpoints = log(Endpoints))%>%
  rename(Percent = Freq)#renames column

routedf<-rowPerc(xtabs(~exposure.route +effect_f, aoc))

routef<-as.data.frame(routedf)%>%
  # mutate(effect = case_when(effect == "Y" ~ "Yes",
  #                           effect == "N" ~ "No")) %>%
  filter(effect_f %in% c("Yes","No"))%>%
  rename(Type= "exposure.route")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Exposure.route")
  # mutate(Type = case_when(
  #   Type == "coparental.exposure" ~"Co-Parental Exposure",
  #   Type == "paternal.exposure" ~ "Paternal Exposure",
  #   Type == "maternal.exposure" ~ "Maternal Exposure",
  #   Type == "food" ~ "Food",
  #   Type == "water" ~ "Water",
  #   Type == "sediment" ~ "Sediment",
  #   Type == "media" ~ "Media"))

study_r<-xtabs(~exposure.route +effect_f,aoc)

routefinal<- data.frame(cbind(routef, study_r))%>%
  rename(Endpoints='Freq.1')%>%
  rename(category='exposure.route')%>%
  mutate(logEndpoints = log(Endpoints))%>%
  rename(Percent = Freq)#renames column


## Create environmentally realistic data
synthetic_data_builder <- function(count){
  #Preset parameters for pdfs
  ## Generate values for the three distributions
  set.seed(123)
  simulated.data <- data.frame(Size = numeric(0))
  for(i in 1:count){
    X <- X.func()
    simulated.data <- rbind(simulated.data, X)}}


#### User Interface ####

ui <- dashboardPage(

  dashboardHeader(title = "Toxicity of Microplastics Explorer 2.0", titleWidth = 400),

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
                     menuItem("Calculators", tabName = "Calculators", icon = icon("calculator")),
                     menuItem("Predictions", tabName = "Predictions", icon = icon("brain")),
                     menuItem("Resources", tabName = "Resources", icon = icon("question-circle")),
                     menuItem("Data Submission", tabName = "Submission", icon = icon("fas fa-file-upload")),
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
              h1("Welcome to the Toxicity of Microplastics Explorer 2.0,",br(),"Aquatic Organisms Database!", align = 'center'),
              br(),
              
              
              box(status = "primary", width = 12,
                    fluidRow(
                    #top right box
                    column(width = 12, 
                           
                    p(tags$img(src="welcome.png", width = "40%", height = "40%", style = "float:left; display: block; margin-left: auto; margin-right: 30px;")),
                    
                    h3("Toxicity of Microplastics Explorer 2.0", align = "center"), 
                    
                    p("The Toxicity of Microplastics Explorer 2.0 (ToMEx 2.0) is a major expansion of the orginal ToMEx database coordinated by SCCWRP through
                              a four-part virtual workshop series of approximately 70 researchers from 14 different nations. ToMEx 2.0 will be released to the public in fall 2023."),
                    
                    strong(p("Disclaimer: When using ToMEx 2.0, it is highly recommended that underlying data are carefully scrutinized before finalizing analyses or drawing major conclusions.")),
                    
                    h3("What is the Microplastics Toxicity Database?", align = "center"), 
                    
                    strong(p("This database is a repository for microplastics 
                      toxicity data for the California Microplastics Health Effects Workshop.")), 
                    
                    p("This web application allows users to explore toxicity 
                    data using an intuitive interface while retaining the diversity and complexity inherent 
                    to microplastics. Data is extracted from existing, peer-reviewed manuscripts containing 
                    toxicity data pertaining to microplastics."),
                  
                    p("A full length description of the database and web application is published in ", 
                      a(href = "https://www.springeropen.com/collections/sccwrp", 'Microplastics and Nanoplastics'),
                      ". To access the open access manuscript, ", a(href = "https://microplastics.springeropen.com/articles/10.1186/s43591-022-00032-4", 'click here'),"."),
                    
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
                        
                        p(align = "center", a(href = "http://wincowger.com/", 'Dr. Win Cowger'),", Moore Institute for Plastic Pollution Research", 
                          tags$a(href="https://twitter.com/Win_OpenData", icon("twitter")), tags$a(href="https://github.com/wincowgerDEV", icon("github"))),
                        
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
          collapsibleTree::collapsibleTreeOutput("plot", height = "700px"),
          
          ), #closes out column
          
        ), #close fluid row
        ), #close box
      
), #close tab


#### Search UI ####

tabItem(tabName = "Search",
        
        box(title = "Search Database", status = "primary", width = 12, height = "1000px",
            
            column(width = 3,
                   downloadButton("download_search", "Download Selected Data (Excel File)", icon("download"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
            
            dataTableOutput("databaseDataTable", height = "600px")
             
         ), #close box
        
),#close search tab

#### Screening UI ####

tabItem(tabName = "Screening",
        
        box(title = "Data Selection", status = "primary", width = 12, collapsible = TRUE,
            
            shinyjs::useShinyjs(), # requires package for "reset" button, DO NOT DELETE - make sure to add any new widget to the reset_input in the server
            id = "screen", # adds ID for resetting filters
            
            p("This plot displays scores from the quality screening exercise developed by", a(href ="https://pubs.acs.org/doi/abs/10.1021/acs.est.0c03057", 'de Ruijter et al. (2020)', .noOWs = "outside"), "with some modification.
            If a single study received multiple scores within a category, the highest score is shown in the plot. 
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
                              
                              column(width = 12,
                                     strong("Warning:"),"'Red criteria' do not represent full scoring criteria.",
                                     br(),
                                     br(),
                              ), 
                              
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
                             selected = "Particle Only",
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
                         strong("Warning:"),"Only 'Particle Only' data are included in the study screening dataset.", 
                         br(),
                         "'Red criteria' do not represent full scoring criteria. Additional scoring criteria may be downloaded via the Search tab or visualized via the Study Screening tab.",
                         br(),
                         br(),
                         ),          
                      
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
                                 strong("Warning:"),"Only 'Particle Only' data are included in the study screening dataset.", 
                                 br(),
                                 "'Red criteria' do not represent full scoring criteria. Additional scoring criteria may be downloaded via the Search tab or visualized via the Study Screening tab.",
                                 br(),
                                 br(),
                          ),  
                              
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
            
            column(width = 3,
                   downloadButton("downloadData_ssd", "Download Raw Data (Excel File)", icon("download"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
            
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
        
        box(title = "SSD Results: Table", status = "primary", width = 12, collapsible = TRUE, height = "675px",

            
            DT::dataTableOutput(outputId = "ssd_pred_table", height = "500px"),
           
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

#### Calculators UI ####
tabItem(tabName = "Calculators",

        box(title = "Probability Distributions", status = "primary", width = 12, collapsible = TRUE,

            p("Kooi & Koelmans (2019) provide probability distributions for microplastics in various matrices. This tab allows one to simulate data using user-defined parameters and examine summary statistics."),
            br(),

            fluidRow(
              tabBox(width = 12,

                     tabPanel("Probability Distributions",

                              shinyjs::useShinyjs(), # requires package for "reset" button, DO NOT DELETE - make sure to add any new widget to the reset_input in the server
                              id = "Calculators", # adds ID for resetting filters

                              fluidRow(
                                # Alpha
                                column(width = 3,
                                       numericInput(inputId = "length_alpha_calculator",
                                                   label = "Power law for size (length)",
                                                   value = 2.64,
                                                   min = 0.5,
                                                   max = 3.0)),
                                #xmin
                                column(width = 3,
                                       numericInput(inputId = "xmin_calculator",
                                                    label = "Minimum particle length (μm)",
                                                    value = 1,
                                                    min = 0.001,
                                                    max = 4999)),
                                #particle count
                                column(width = 3,
                                       numericInput(inputId = "particle.count_calculator",
                                                    label = "# of particles to generate",
                                                    value = 1000,
                                                    min = 1,
                                                    max = 100000)),
                                #action buttons 
                                column(width = 4,
                                       actionButton("go_simulate", "Simulate data", icon("rocket"), style="color: #fff; background-color:  #117a65; border-color:  #0e6655")),
                                #column(width = 3,
                                 #      actionButton("reset_input", "Reset Filters", icon("redo"), style="color: #fff; background-color: #f39c12; border-color: #d68910")), 
                                column(width = 3,
                                       downloadButton("downloadData_simulate", "Download Data (Excel File)", icon("download"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                     ),
             
              fluidRow(
                
                column(width = 12,
                       
                       plotOutput(outputId = "simulated.data.histogram", height = "500px"),
                )
              ),
                     ), #closes tabPanel
              
              tabPanel("Alignments",
                       
                       shinyjs::useShinyjs(), # requires package for "reset" button, DO NOT DELETE - make sure to add any new widget to the reset_input in the server
                       id = "Alignments", # adds ID for resetting filters
                       
                       fluidRow(
                       
                         column(width = 12,
                         p("This tab allows users to upload laboratory toxicity data (monodisperse or polydisperse) and calculate ERM-aligned polydisperse values corrected to a default size range of the user's choice (e.g. 1 - 5,000 um) using the equations and parameters in", a(href = "https://www.sciencedirect.com/science/article/pii/S0043135421006278", "Kooi et al., (2021).")),
                         
                         
                         br(),
                         
                         strong("An illustrated and detailed example of how alignments are performed may be found in this document."),
                         
                         br(),
                         
                         column(width = 4,
                                downloadButton("illustrated_example", "Download Illustrated Example", icon("download"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                         
                         br(),
                         br(),
                         br(),
                         
                         p("First, ensure data is formatted correctly (see example dataset for guidance), then choose site-specific distribution parameters using the widgets below, press 'calculate', and download the new dataset. Note that the uploaded dataset can have any number of columns in addition to the minimum needed for performing alignments (max.size.ingest.um [numeric], dose.particles.mL.master[numeric], polydispersity [binary categorical], particle.surface.area.um2 [numeric], particle.volume.um.3 [numeric], mass.per.particle.mg [numeric]). Note that data labeled as 'polydisperse' must have minimum and maximum parameters, while data labeled 'monodisperse' do not."),
                         
                         
                         br(),
                         
                         strong("Use this example dataset as a guide to format data for upload"),
                       
                       
                         br(),
                         
                         column(width = 4,
                                downloadButton("testData_calculator", "Download Example Data", icon("download"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                       
                         br(),
                         br(),
                         br(),
                         
                         # Input: Select a file ---
                         column(width = 6,
                         fileInput("alignment_file", "Upload csv File",
                                   multiple = FALSE,
                                   accept = c("text/csv",
                                              "text/comma-separated-values,text/plain",
                                              ".csv"))), 
                         br(),
                         p("Choose the bioaccessibility parameters, and site-specific polydisperse microplastics distributions parameters.")),
                        
                         #ERM Checkbox
                         column(width = 12,
                                
                                # Switch to choose what determines bioaccessibility
                                column(width = 5,
                                       radioButtons(inputId = "ingestion.translocation.switch_calculator",
                                                    label = "Bioaccessibility limited by tissue translocation (fixed) or mouth size opening (species-dependent)?",
                                                    choices = c("ingestion", "translocation"),
                                                    selected = "ingestion")),
                                
                                # Tissue translocation size limit (if applicable)
                                column(width = 4,
                                       numericInput(inputId = "upper.tissue.trans.size.um_calculator",
                                                    label = "Upper Length (µm) for Translocatable Particles (only works if bioaccessibility determined by translocation; also excludes data from experiments using particles longer than defined value)",
                                                    value = 83))
                                ),
                         
                         column(width = 12,
                                strong("Starting alpha values are for marine surface water reported in ", a(href = "https://www.sciencedirect.com/science/article/pii/S0043135421006278", "Kooi et al., (2021)")),
                                br(),
                                br()),
                         
                         #Alpha checkbox
                         column(width = 4,
                                numericInput(inputId = "alpha_calculator",
                                             label = "Length Alpha Value",
                                             value = 2.07,
                                             step = 0.01)),
                         
                         #Alpha surface area input
                         column(width = 4,
                                numericInput(inputId = "a.sa_calculator",
                                             label = "Surface Area Alpha Value",
                                             value = 1.50,
                                             step = 0.01)),
                         
                         #Alpha volume input
                         column(width = 4,
                                numericInput(inputId = "a.v_calculator",
                                             label = "Volume Alpha Value",
                                             value = 1.48,
                                             step = 0.01)),
                         
                         #Alpha mass input
                         column(width = 4,
                                numericInput(inputId = "a.m_calculator",
                                             label = "Mass Alpha Value",
                                             value = 1.32,
                                             step = 0.01)),
                         
                         #Alpha ssa input
                         column(width = 4,
                                numericInput(inputId = "a.ssa_calculator",
                                             label = "Specific Surface Area Alpha Value",
                                             value = 1.98,
                                             step = 0.01)),
                         
                         #average width to length ratio
                         column(width = 4,
                                numericInput(inputId = "R.ave_calculator",
                                             label = "Average Particle Width to Length Ratio",
                                             value = 0.77,
                                             step = 0.01)),
                         
                         #average density
                         column(width = 4,
                                numericInput(inputId = "p.ave_calculator",
                                             label = "Average Particle Density (g/cm^3)",
                                             value = 1.10,
                                             step = 0.01)),
                         
                         # lower length input
                         column(width = 4,
                                numericInput(inputId = "lower_length_calculator",
                                             label = "Lower Length for Default Size Range (µm)",
                                             value = 1)),
                         # upper length input
                         column(width = 4,
                                numericInput(inputId = "upper_length_calculator",
                                             label = "Upper Length for Default Size Range (µm)",
                                             value = 5000)),
                         br(),
                         
                         #Action Buttons
                         column(width = 4,
                                actionButton("go_calculator", "Align data", icon("rocket"), style="color: #fff; background-color:  #117a65; border-color:  #0e6655")),
                                              
                         column(width = 12,
                                  # Show the table with the predictions
                                  DT::dataTableOutput("alignmentTable")),
                           
                           column(width = 4,
                                  br(),
                                  downloadButton("downloadData_calculator", "Download Aligned Data", icon("download"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                           
                       )#close fluidrow for panel       
              )# closes tabpanel
                         
            ) #closes tabox
        ) # closes fluidrow
        ) #closes box
                     ), #close tabItem

#### Predictions UI #####
tabItem(tabName = "Predictions",
        
        box(title = "Model Predictions of Microplastics Effect Thresholds", status = "primary", width = 12, collapsible = TRUE,
            
            p("Coffin et al (in prep) provide a machine learning model to predict concentrations of microplastics expected to result in a given effect for a given species for given particle characteristics."),
            br(),
            
            fluidRow(
              tabBox(width = 12,
                     
                     tabPanel("Start",
                              
                              fluidRow(
                                column(width = 12,
                                
                                p("This model predicts the ERM-aligned (1- 5,000 um) concentrations that would be expected to produce an effect in a species of interest for a given effect metric (e.g., NOEC, LOEC). The model was trained on quality-controlled effects data in the ToMEx database and utilizes a random forest structure. The model has been optimized to give the most accurate predictions using the fewest number of parameters. For the food dilution ERM, the model R^2 is 0.87, and for the tissue translocation ERM, the model R^22 is 0.82 based on a subset (25%) of the training data. See Coffin et al (in prep) for additional details, and instructions on the formatting of independent variables for uploading. Additional details regarding this methodology, including a walkthrough of how to use this tab are included in", a(href = "https://youtu.be/ymPMYkcmgDg", "Dr. Scott Coffin's SETAC North America 2021 presentation (YouTube link).", .noOWs = "outside"))),
                                
                                br(),
                                
                                column(width = 4,
                                       p("Test data may be used as a guide for preparing user data."),
                                       br(),
                                       downloadButton("testData_prediction", "Download Test Data", icon("download"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                
                                column(width = 12,
                                       br(),
                                p("For categorical variables, ensure data are valid values (i.e. levels exist within training dataset). Click below to download a list of valid values for each variable name.")),
                                       br(),
                                
                                column(width = 4,
                                       downloadButton("validValues_prediction", "Download Valid Values", icon("download"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                              
                              )), #end start tabPanel
                     
                     tabPanel("Upload Data",
                                
                              fluidRow(
                                column(width = 12,
                                       
                                p("It is critical for column names and factor levels to be formatted in exactly the same manner as the training dataset. Once data are formatted correctly, upload below. Note that the dataset may have any number of additional columns, so long as it has all of the columns listed in the example dataset."),
                                
                                # Input: Select a file ---
                                column(width = 6,
                                fileInput("prediction_file", "Upload csv File:",
                                          multiple = FALSE,
                                          accept = c("text/csv",
                                                     "text/comma-separated-values,text/plain",
                                                     ".csv"))
                                )# end column
                              )#end column
                              ), #end fluidRow
                              br(),
                              fluidRow(column(width = 12,
                                p("Smoothed histograms of the training dataset are overlaid on the user-uploaded data. As test data diverges in relative abundance from training data, model predictions lower in accuracy."),
                              column(width = 12,
                                     plotOutput(outputId = "predictionDataSkim"))
                              ),
                              column(width = 12,
                                     column(width = 3,
                                            downloadButton("download_predictionDataSkim", "Download Plot", icon("download"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))
                              )
                              
                     ), # end upload tabPanel
                     
                     tabPanel("Model Selection",
                              
                              fluidRow(
                              #model select
                              column(width = 12,
                              p("Two models are currently available for predicting effect concentrations based on the `food dilution` and `tissue translocation` Ecologically Relevant Effect Metrics (ERMs). Choose the ERM for model predictions.")),
                              column(width = 4,
                                     radioButtons(inputId = "ERM_radio", 
                                                  label = "ERM:",
                                                  choices = c("food dilution", "tissue translocation"),
                                                  selected = "tissue translocation")),
                                #action buttons 
                                column(width = 4,
                                       br(),
                                       actionButton("go_predict", "Predict Effect Concentrations", icon("rocket"), style="color: #fff; background-color:  #117a65; border-color:  #0e6655"))
                              )), #closes model selection tabPanel
                     
                     tabPanel("Predictions Table",
                              
                              fluidRow(
                                
                                column(width = 12,
                                p("Predicted effect concentrations for the uploaded data can be viewed and downloaded here.")),
                                
                                column(width = 12,
                                       downloadButton("downloadData_prediction", "Download Prediction Data (Excel File)", icon("download"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                
                                ), # end of fluidrow
                              
                              fluidRow(
                                
                                column(width = 12,
                                       # Show the table with the predictions
                                       DT::dataTableOutput("predictionsTable")))         
                              ), # closes predictions table tabPanel
                     
                     tabPanel("Prediction Accuracy",
                              
                              fluidRow(
                                column(width = 12,
                                p("If known concentrations were uploaded, predicted effect concentrations can be compared here. The dashed line represents a perfect agreement between predicted and measured effect concentrations.")),
                                
                                column(width = 3,
                                       pickerInput(inputId = "prediction_var",
                                                   label = "Variable to Highlight:",
                                                   choices = c("`Organism Group`", "`Life Stage`", "`Species`","`Level of Biological Organization`","`Exposure Route`","Environment","`Acute/Chronic`",
                                                               "translocatable", "`Effect Score`", "`Effect Metric`","`Broad Endpoint Category`","`Specific Endpoint Category`"),
                                                   selected = "Organism Group",
                                                   options = list(`actions-box` = FALSE), # option to de/select all
                                                   multiple = FALSE)),
                                column(width = 12,
                                       plotOutput(outputId = "predictionsScatter")),
                                column(width = 12,
                                       column(width = 3,
                                              downloadButton("downloadScatter_predictions", "Download Plot", icon("download"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))
                              )) #closes accuracy tabPanel
                     
                     
              ) #closes tabox
            ) # closes fluidrow
        ) #closes box
), #close tabItem
  
#### Resources UI ####

tabItem(tabName = "Resources", 
         
        
         box(title = "Resources", width = 6, status = "primary",     
         p(align = "center",a(href = "https://sccwrp-my.sharepoint.com/:b:/g/personal/leahth_sccwrp_org/EeyE7n7JZdJPi_EYUD_D-dsBxNv5qlBtzwihmr9SbxH_Og?e=Crfu6Z", 'Data Category Descriptions')),
         br(),
         p(align = "center",a(href = "https://sccwrp-my.sharepoint.com/:b:/g/personal/leahth_sccwrp_org/EeEqwDA28OdNq4zXvO-U-p4B6aF-3v-rCvq0xB7oy8GAZg?e=27smxu", 'Study Screening Rubric')),
         br(),
         p(align = "center",a(href = "https://sccwrp-my.sharepoint.com/:b:/g/personal/leahth_sccwrp_org/EZ0r1AdQqsJGnPuvDJFNyxMBQ60ibEbAiCqrjNqRtlX7gg?e=18fTUr", 'Aquatic Organisms Study List'))),
         
        ), #close tab

#### Data Submission UI ####

tabItem(tabName = "Submission", 
        
        box(title = "Data Submission", width = 6, status = "primary",
            p("The ToMEx Database is currently being updated through the ToMEx 2.0 Workgroup. For more information or to find out how to get invovled, please contact tomex@sccwrp.org or visit the ",
            a(href ="https://microplastics.sccwrp.org/",
                   'SCCWRP ToMEx 2.0 Webpage', .noOWs = "outside"),"."),
            
            # p("To submit new data to ToMEx, download the data submission template using the link below. Complete the submission template
            #   using the embedded descriptions and the ", 
            #   a(href ="https://sccwrp-my.sharepoint.com/:b:/g/personal/leahth_sccwrp_org/EeyE7n7JZdJPi_EYUD_D-dsBxNv5qlBtzwihmr9SbxH_Og?e=Crfu6Z",
            #     'Data Category Descriptions', .noOWs = "outside"),
            #   ". Once the data submission template is completed, upload the completed template using the button below."),
            br(),
            p("For questions regarding data submission or to check to see if data from a specific study has already been uploaded to ToMEx, please email tomex@sccwrp.org"),
            br(),
            # p(align = "center", downloadButton(href = "https://sccwrp-my.sharepoint.com/:x:/g/personal/leahth_sccwrp_org/EfKDcb9J1ShHup4Js_NVZ_kBjGVqYfvVQ-2HPDAb79YOVg?e=Pq1S4V", label = "Download Data Submission Template", icon("download"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
            br(),
            # p(align = "center", actionButton(inputId = "submit", onclick = "window.open('https://sccwrp-my.sharepoint.com/:f:/g/personal/leahth_sccwrp_org/EhnzSiN8GqZFjnGpTbNJgskBGaWp0sVKtnB9nrqszAYoQA')", label = "Upload Validated Data Template", icon("file-upload"), style="color: #fff; background-color:  #117a65; border-color:  #0e6655"))
            ),
        
        ), #close tab

#### Contact UI ####

tabItem(tabName = "Contact", 
         
        box(title = "Contact", width = 6, status = "primary",
         p("For scientific and technical questions, please email tomex@sccwrp.org."),
         ),
         
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
     ggplot(polyfinal,aes(fill=effect_f, y= logEndpoints, x= poly_f, Percent=Percent)) +
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
     ggplot(vivofinal,aes(fill=effect_f, y= logEndpoints, x= Type, Percent=Percent)) +
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
     ggplot(sizefinal,aes(fill=effect_f, y= logEndpoints, x= Type, Percent=Percent)) +
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
     ggplot(shapefinal,aes(fill=effect_f, y= logEndpoints, x= Type, Percent=Percent)) +
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
     ggplot(lifefinal,aes(fill=effect_f, y= logEndpoints, x= Type, Percent=Percent)) +
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
     ggplot(taxfinal,aes(fill=effect_f, y= logEndpoints, x= Type, Percent=Percent)) +
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
     ggplot(routefinal,aes(fill=effect_f, y= logEndpoints, x= Type, Percent=Percent)) +
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
     style = "bootstrap",
     options = list(
       dom = 'ltipr',
       scrollY = 600,
       scrollX = TRUE,
       autoWidth = TRUE,
       bautoWidth = FALSE
       ))
   
   output$download_search = downloadHandler(filename = paste('ToMEx_Search', Sys.Date(), '.csv', sep=''),
                                            content = function(file) {
                                              s = input$databaseDataTable_rows_all
                                              readr::write_excel_csv(aoc_search[s, , drop = FALSE], file)
                                            })

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
       group_by(Study_plus, Criteria_f) %>% 
       slice_max(Score) %>% #If multiple scores are received for a single category, the highest score is selected
       ungroup() %>% 
       pivot_wider(names_from = Study_plus, 
                   values_from = Score) %>%   
       column_to_rownames(var="Criteria_f")  
       
     # colnames(tech)<- gsub(" \\(10.*", "",colnames(tech))
     # colnames(tech)<- gsub(" \\(doi.*", "",colnames(tech))
     
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
       group_by(Study_plus, Criteria_f) %>% 
       slice_max(Score) %>% #If multiple scores are received for a single category, the highest score is selected
       ungroup() %>%
       pivot_wider(names_from = Study_plus, 
                   values_from = Score) %>%   
       column_to_rownames(var="Criteria_f") 
     
     # colnames(risk)<- gsub(" \\(10.*", "",colnames(risk))
     # colnames(risk)<- gsub(" \\(doi.*", "",colnames(risk))
     
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
      mutate(EC_env_p.particles.mL = EC_poly_p.particles.mL * CF_bio) %>%  #aligned particle effect concentration (1-5000 um)
      
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
    filter(exp_type_f %in% exp_type_c) %>%   #filter by experiment type
    filter(org_f %in% org_c) %>%  # filter by organism inputs
    filter(lvl1_f %in% lvl1_c) %>%  # filter by level inputs
    filter(lvl2_f %in% lvl2_c) %>%   #filter by level 2 inputs
    filter(bio_f %in% bio_c) %>%  #filter by bio organization
    filter(effect_f %in% effect_c) %>%  #filter by effect
    filter(life_f %in% life_c) %>%  #filter by life stage
    filter(poly_f %in% poly_c) %>%  #filter by polymer
    filter(size_f %in% size_c) %>%  #filter by size class
    filter(shape_f %in% shape_c) %>%  #filter by shape
    filter(species_f %in% species_c) %>%  #filter by species
    filter(env_f %in% env_c) %>%  #filter by environment
    filter(acute.chronic_f %in% acute.chronic.c) %>% #acute/chronic
    filter(tier_zero_tech_f %in% tech_tier_zero_c) %>%  #technical quality
    filter(tier_zero_risk_f %in% risk_tier_zero_c)  %>%  #risk assessment quality
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
    filename = paste('ToMEx_Exploration', Sys.Date(), '.csv', sep=''),
    content = function(file) {
      
      aoc_filter_tidy <- aoc_filter() %>%
        #Select columns
        dplyr::select(c(doi, authors, year, species_f, org_f, env_f, life_f, vivo_f, sex, body.length.cm, max.size.ingest.mm,
                        #experimental parameters
                        exp_type_f, exposure.route, mix, negative.control, reference.material, exposure.media, solvent, detergent,
                        media.ph, media.sal.ppt, media.temp, media.temp.min, media.temp.max, exposure.duration.d, `Recovery (Days)`, acute.chronic_f,
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
            "Uptake Validation Method" = uptake.valid.method, "Tissue Distribution" = tissue.distribution, "Organisms Fed?" = fed))
  
      readr::write_excel_csv(aoc_filter_tidy, file)
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
    shinyjs::reset("xmin_calculator")
    shinyjs::reset("length_alpha_calculator")
    shinyjs::reset("particle.count_calculator")
    
  }) #If we add more widgets, make sure they get added here. 

#### SSD S ####

  # Create new all tested dataset based on widget filtering and adjusted to reflect the presence of the "update" button.
  aoc_ssd_filtered <- eventReactive(list(input$SSDgo),{
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
    effect_metric_rad <- input$effect.metric_rad_ssd #effect metric filtering
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
      mutate(dose_new = case_when((AF.time_r_ssd == "Yes" & AF.noec_r_ssd == "Yes") ~ (dose_new / (af.time * af.noec)), #composite assessment factors
                                  (AF.time_r_ssd == "Yes" & AF.noec_r_ssd == "No") ~ (dose_new / af.time),
                                  (AF.time_r_ssd == "No" & AF.noec_r_ssd == "Yes") ~ (dose_new / af.noec),
                                  (AF.time_r_ssd == "No" & AF.noec_r_ssd == "No") ~ dose_new)) %>% # adjust for assessment factors based on user input
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
      dplyr::filter(effect.metric %in% effect_metric_rad) %>%  #filter for effect metric
      dplyr::filter(acute.chronic_f %in% acute.chronic.c_ssd) %>%  #acute chronic filter
      dplyr::filter(risk.13 != 0) %>%  #Drop studies that received a score of 0 for endpoints criteria (this also drops studies that have not yet been scored) - KEEP THIS AFTER THE RED CRITERIA FILTERS  
      dplyr::filter(case_when(ingestion.translocation.switch == "translocation" ~  between(size.length.um.used.for.conversions, x1D_set, upper.tissue.trans.size.um), #if tissue-trans limited, don't use data with non-translocatable particles
                       ingestion.translocation.switch == "ingestion" ~  between(size.length.um.used.for.conversions, x1D_set, x2D_set))) %>%  #if ingestion-limited, don't use data outside upper default size range
      group_by(Species) %>% 
      drop_na(dose_new) 
  })
    
    aoc_z_L <- eventReactive(list(input$SSDgo),{aoc_ssd_filtered() %>% 
            summarise(MinConcTested = min(dose_new), MaxConcTested = max(dose_new), CountTotal = n())# %>%   #summary data for whole database
     # mutate_if(is.numeric, ~ signif(., 6))
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
      mutate(dose_new = case_when((AF.time_r_ssd == "Yes" & AF.noec_r_ssd == "Yes") ~ (dose_new / (af.time * af.noec)), #composite assessment factors
                                  (AF.time_r_ssd == "Yes" & AF.noec_r_ssd == "No") ~ (dose_new / af.time),
                                  (AF.time_r_ssd == "No" & AF.noec_r_ssd == "Yes") ~ (dose_new / af.noec),
                                  (AF.time_r_ssd == "No" & AF.noec_r_ssd == "No") ~ dose_new)) %>% # adjust for assessment factors based on user input
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
      summarise(minConcEffect = min(dose_new), meanConcEffect = mean(dose_new), medianConcEffect = median(dose_new), SDConcEffect = sd(dose_new),MaxConcEffect = max(dose_new), CI95_LCL = meanConcEffect - 1.96 * SDConcEffect/sqrt(n()), firstQuartileConcEffect = quantile(dose_new, 0.25), CI95_UCL = meanConcEffect + 1.96 * SDConcEffect/sqrt(n()), thirdQuartileConcEffect = quantile(dose_new, 0.75), CountEffect = n(), MinEffectType = lvl1_f[which.min(dose_new)], Minlvl2EffectType = lvl2_f[which.min(dose_new)], MinEnvironment = env_f[which.min(dose_new)], MinDoi = doi[which.min(dose_new)], MinLifeStage = life_f[which.min(dose_new)], Mininvitro.invivo = vivo_f[which.min(dose_new)])# %>%  #set concentration to minimum observed effect
      #mutate_if(is.numeric, ~ signif(., 6))
   
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
    
    datatable(aoc_filter_ssd() %>%  mutate_if(is.numeric, ~ signif(., 3)),
              extensions = c('Buttons'),
              style = "bootstrap",
              options = list(
                dom = 'Brtip',
                buttons = list(I('colvis'), c('copy', 'csv', 'excel')),
                scrollY = 400,
                scrollH = TRUE,
                sScrollX = TRUE),
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
    
    set.seed(99) #reproducibility
    
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
     # mutate_if(is.numeric, ~ signif(., 7)) %>%
      arrange(delta) #orders by delta of fit
  }) 
  
  #Render table for goodness of fit
  output$table_gof_react <- DT::renderDataTable(server= FALSE,{  #prints ALL data, not just what's shown 
    req(gof())
    gof <- gof() %>% 
      mutate_if(is.numeric, ~ signif(., 4))
    
     datatable(gof,
              extensions = 'Buttons',
              style = "bootstrap",
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
                style = "bootstrap",
                extensions = c('Buttons', 'Scroller'),
                options = list(
                  autoWidth = FALSE,
                  dom = 'Brtip',
                  scrollY = 400,
                  scroller = TRUE,
                  buttons = c('copy', 'csv', 'excel')
                ), 
                class = "compact",
                colnames = c("Percent", paste0("Estimated Mean Concentration ",  dose_check_ssd), paste0("Standard Error ",  dose_check_ssd), "Lower 95% Confidence Limit", "Upper 95% Confidence Limit", "Distribution", "Proportion of Data Sets Successfully Fitted"),
                caption = "Predicted species sensitivity distribution concentrations with uncertanties.
                Note: Mehinto et al. (2022) (doi: 10.1186/s43591-022-00033-3) refers to the point estimate as the 'median' which is interchangable with the 'estimated mean concentration' reported in the table below. If 10 or more iterations are used to bootstrap the model, the mean and median become identical.")
               
  })
  
  output$databaseDataTable <- DT::renderDataTable(
    aoc_search,
    filter = "top",
    rownames = FALSE,
    style = "bootstrap",
    options = list(
      dom = 'ltipr',
      scrollY = 600,
      scrollX = TRUE,
      autoWidth = TRUE,
      bautoWidth = FALSE
    ))
  

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
  
  output$downloadData_ssd <- downloadHandler(
    filename = paste('SSD_RawData', Sys.Date(), '.csv', sep=''),
    
    content = function(file) {
      
      ssd_raw_data_tidy <- aoc_ssd_filtered() %>%
                  ungroup() %>% 
                  dplyr::select(source, doi, authors, Group, Species, lvl1_f, lvl3_f, bio_f, effect.metric, acute.chronic_f, shape_f, poly_f, polydispersity, size.length.min.um.used.for.conversions, size.length.max.um.used.for.conversions,
                         size.length.um.used.for.conversions, dose.particles.mL.master, dose.particles.mL.master.converted.reported, 
                         dose.mg.L.master, dose.mg.L.master.converted.reported, EC_env_v.particles.mL, dose_new,
                         tech.a1, tech.a2, tech.a3, tech.a4, tech.a5, tech.a6,
                         tech.1, tech.2, tech.3, tech.4, tech.5, tech.6,
                         tech.7, tech.8, tech.9, tech.10, tech.11, tech.12,
                         risk.b1, risk.13, risk.14, risk.15, risk.16, risk.17,
                         risk.18, risk.19, risk.20) %>% 
                  dplyr::rename("DOI" = doi,
                         "First Author" = authors,
                         "Particle Morphology" = shape_f,
                         "Effect Metric" = effect.metric,
                         "Exposure Duration" = acute.chronic_f,
                         "Polymer" = poly_f,
                         "Mixture or Single Size" = polydispersity,
                         "Minimum Length (polydisperse only)" = size.length.min.um.used.for.conversions,
                         "Maximum Length (polydisperse only)" = size.length.max.um.used.for.conversions,
                         "Original Dose (Particles/mL)" = dose.particles.mL.master, 
                         "Original Dose (Particles/mL), Reported or Converted" = dose.particles.mL.master.converted.reported,
                         "Original Dose (mg/L)" = dose.mg.L.master,
                         "Original Dose (mg/L), Reported or Converted" = dose.mg.L.master.converted.reported,
                         "Organism Group" = Group,
                         "Species" = Species,
                         "Endpoint Category" = lvl1_f,
                         "Measured Endpoint" = lvl3_f,
                         "Biological Level of Organization" = bio_f,
                         "Particle Size (µm)" = size.length.um.used.for.conversions, 
                         "Aligned Dose, Volume (Particles/mL)" = EC_env_v.particles.mL, 
                         "Aligned Dose, Volume, Assessment Factors Applied (Particles/mL)" = dose_new,
                         "Test Medium Vehice Reported" = tech.a1, 
                         "Administration Route Reported" = tech.a2, 
                         "Test Species Reported" = tech.a3, 
                         "Sample Size Reported" = tech.a4, 
                         "Control Group Reported" = tech.a5, 
                         "Exposure Duration Reported" = tech.a6,
                         "Particle Size" = tech.1, 
                         "Particle Shape" = tech.2, 
                         "Polymer Type" = tech.3, 
                         "Source of MP" = tech.4, 
                         "Concentration Reporting" = tech.5, 
                         "Chemical Purity" = tech.6,
                         "Contamination Prevention" = tech.7, 
                         "Verification of Background Contamination" = tech.8, 
                         "Verification of Exposure" = tech.9, 
                         "Homogeneity of Exposure" = tech.10, 
                         "Exposure Validation" = tech.11, 
                         "Replication" = tech.12,
                         "Number of MP Treament Groups" = risk.b1, 
                         "Endpoints" = risk.13, 
                         "Presence of Natural (food) Particles" = risk.14, 
                         "Reporting Effect Thresholds" = risk.15, 
                         "Quality of Dose Response Relationship" = risk.16, 
                         "Concentration Range Tested" = risk.17,
                         "Aging and Biofouling" = risk.18, 
                         "Diversity of MP Tested" = risk.19, 
                         "Exposure Time" = risk.20)
      
      readr::write_excel_csv(ssd_raw_data_tidy, file)
  })
  
  ##### Calculators #####
  simulated_distribution <- eventReactive(list(input$go_simulate),{

    #default parameters for simulated distribution
    mu1 <- 0.08
    mu2 <- 0.44
    sd1 <- 0.03
    sd2 <- 0.19
    lambda1 <- 0.06
    lambda2 <- 0.94
    d.alpha <- 73.8 #tail heaviness
    d.beta <- 69.9  #asymmetry
    d.mu <- 0.840   #location
    d.delta <- 0.0972 #scale
    ##### SIMULATED DISTRIBUTION FUNCTIONS #####
    ### Data builer equations ###
    X.func <- function (X, xmin, alpha){
      success <- FALSE
      while (!success){
        U = runif(1, 0, 1)
        X = xmin*(1-U)^(1/(1-alpha))
        success <- X < 5000} ##should be smaller than 5000 um 
      return(X)}
    
    D.func <- function (D){
      success <- FALSE
      while (!success){
        D = rnig(1, mu = d.mu, alpha = d.alpha, beta = d.beta, delta = d.delta)
        success <- D < 2.63} ## include upper limit of 2.63, the max. 
      return(D)
    }
    
    #functions to create simualted data
    synthetic_data_builder <- function(count, alpha, xmin){
      #Preset parameters for pdfs
      ## Generate values for the three distributions
      set.seed(123)
      simulated.data <- data.frame(Size = numeric(0))
      
      for(i in 1:count){
        X <- X.func(xmin = xmin, alpha = alpha)
        simulated.data <- rbind(simulated.data, X)
      }
      # SIZE #
      colnames(simulated.data) <- c("Size")
      ## SHAPE DISTRIBUTION
      #Sample N random uniforms U
      U =runif(count)
      #Sampling from the mixture
      for(i in 1:count){
        if(U[i]<lambda1){
          simulated.data$Shape[i] = rtnorm(1,mu1,sd1, lower = 0, upper = 1)
        }else{
          simulated.data$Shape[i] = rtnorm(1,mu2,sd2, lower = 0, upper = 1)
        }
      }
      ## DENSITY DISTRIBUTION
      Dens <- data.frame(Density = numeric(0));
      for(i in 1:count){
        X <- D.func()
        Dens <- rbind(Dens, X)}
      colnames(Dens) <- c("Density")
      simulated.data <- cbind(simulated.data, Dens)
      #add info
      simulated.distribution <- simulated.data %>%
        mutate(size.category = factor(case_when(
          Size < 0.01 & Size >= 0.001 ~ "10nm < 1nm",
          Size < 0.1 & Size >= 0.010 ~ "100nm < 10nm",
          Size < 1 & Size >= 0.100 ~ "100nm < 1µm",
          Size < 10 & Size >= 1 ~ "1µm < 10µm",
          Size < 100 & Size >= 10 ~ "10µm < 100µm",
          Size < 1000 & Size >= 100 ~ "100µm < 1mm",
          Size < 5000 & Size >= 1000 ~ "1mm < 5mm"))) %>%
        mutate(mass.mg = Density * Size^3 * 1E-9) %>%
        mutate(um3 = (Size^3)) %>%
        mutate(particles.total = factor(as.character(count)))
    } # end of builder function
    
    #variables to set for funcions (user-defined)
  xmin.user <- input$xmin_calculator #UM
  alpha.user <- input$length_alpha_calculator #for marine surface water Kooi et al 2021
  count.user <- input$particle.count_calculator
  
    #build
    simulated.distribution <- synthetic_data_builder(count = count.user, xmin = xmin.user, alpha = alpha.user)
    #save
    simulated.distribution
  })
  
  ### histrogram of simulate data ###
  output$simulated.data.histogram <- renderPlot({
    
    #plot
    simulated_distribution() %>% 
      ggplot(aes(x = Size, fill = size.category)) +
      scale_x_log10(name = "Size (um)",
                    limits = c(input$xmin_calculator, 5000)) +
      geom_histogram() +
      theme_minimal()
    })
  
  # Create downloadable csv of filtered dataset.
  # Removed columns created above so the dataset matches Leah's original dataset.
  output$downloadData_simulate <- downloadHandler(
    filename = function() {
      paste('simulated_data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(simulated_distribution() %>% dplyr::rename(c("Size (um)" = Size, "Shape (CSF, unitless)" = Shape, "Density (g cm^3)" = Density, "Mass (mg)" = mass.mg, "Volume (um^3)" = um3)), file, row.names = FALSE)
    }
  )
  
  ###### --Alignment Calculator ####
  
  # example dataset with minimum columns needed for making alignments
  output$testData_calculator <- downloadHandler(
    filename = function() {
      paste('example_data-', '.csv', sep='')
    },
    content = function(file) {
      write.csv(test_data_calculator, file, row.names = FALSE)
    }
  )
  
  # Alignment walkthrough (based on .Rmd file from modelling repo)
  output$illustrated_example <- downloadHandler(
    filename = function() {
      "Illustrated_Alignment_Example.html"
    },
    content = function(file) {
      file.copy("calculator/ERM-Illustrative-Example.html", file)
    }
  )
  
  #align data
  alignedData_calculator <- eventReactive(list(input$go_calculator),{
    #read in user dataset
    raw <- read.csv(input$alignment_file$datapath, stringsAsFactors = TRUE)
    
    ## ERM parametrization ##
    # Define params for alignments #
    alpha = input$alpha_calculator #length power law exponent
    x2D_set = as.numeric(input$upper_length_calculator) #upper size range (default)
    x1D_set = input$lower_length_calculator #lower size range (default)
    x1M_set = input$lower_length_calculator #lower size range for ingestible plastic (user defined)
    upper.tissue.trans.size.um <- as.numeric(input$upper.tissue.trans.size.um_calculator) #user-defined upper value for tissue trans (numeric)
    ingestion.translocation.switch <- input$ingestion.translocation.switch_calculator #user-defined: inputs are "ingestion" or "translocation"
    ERM.switch <- input$ERM_check_calculator
    
    # define parameters for power law coefficients
    a.sa = input$a.sa_calculator #1.5 #marine surface area power law
    a.v = input$a.v_calculator#1.48 #a_V for marine surface water volume
    a.m = input$a.m_calculator#1.32 # upper limit fora_m for mass for marine surface water in table S4 
    a.ssa = input$a.ssa_calculator #1.98 # A_SSA for marine surface water
    
    #define additional parameters for calculations based on averages in the environment
    R.ave = input$R.ave_calculator #0.77 #average width to length ratio for microplastics in marine enviornment
    p.ave = input$p.ave_calculator#1.10 #average density in marine surface water
    
    # calculate ERM for each species
    aligned <- raw %>%
      #print values used to align
      mutate(alpha = alpha,
             x2D_set = x2D_set,
             x1D_set = x1D_set,
             x1M_set = x1M_set,
             upper.tissue.trans.size.um = upper.tissue.trans.size.um,
             ingestion.translocation.switch = ingestion.translocation.switch,
             a.sa = a.sa,
             a.v = a.v,
             a.m = a.m,
             a.ssa = a.ssa,
             p.ave = p.ave,
             R.ave = R.ave) %>% 
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
      mutate(EC_env_ssa.um2.ug.mL =  EC_env_ssa.particles.mL * mux.polyfnx(a.x = a.ssa, x_UL = x2D_set, x_LL = x1D_set)) %>% 
      
      #annotate aligned ERM of interest for user interpretability
      mutate("Surface-Area Aligned Exposure Concentration (particles/mL)" = EC_env_sa.particles.mL,
             "Volume Aligned Exposure Concentration (particles/mL)" = EC_env_v.particles.mL,
             "Mass Aligned Exposure Concentration (particles/mL)" = EC_env_m.particles.mL,
             "Specific Surface Area Aligned Exposure Concentration (particles/mL)" = EC_env_ssa.particles.mL) %>% 
      #nudge to front
      dplyr::relocate("Surface-Area Aligned Exposure Concentration (particles/mL)",
                      "Volume Aligned Exposure Concentration (particles/mL)",
                      "Mass Aligned Exposure Concentration (particles/mL)",
                      "Specific Surface Area Aligned Exposure Concentration (particles/mL)") 
  
    #print
  aligned
    
  })
  
  #render calculated values in datatable
  output$alignmentTable = DT::renderDataTable({
    req(input$alignment_file)
    
    datatable(alignedData_calculator() %>%  mutate_if(is.numeric, ~ signif(., 3)),
    extensions = c('Buttons'),
    style = "bootstrap",
    options = list(
      dom = 'Brtip',
      buttons = list(I('colvis'), c('copy', 'csv', 'excel')),
      scrollY = 400,
      scrollH = TRUE,
      sScrollX = TRUE,
      columnDefs = list(list(width = '50px, targets = "_all'))),#only display the table and nothing else
    caption = "Filtered Data") %>% 
    formatStyle(
      c("Surface-Area Aligned Exposure Concentration (particles/mL)", "Volume Aligned Exposure Concentration (particles/mL)", "Mass Aligned Exposure Concentration (particles/mL)", "Specific Surface Area Aligned Exposure Concentration (particles/mL)"),
      backgroundColor = '#a9d6d6')
    
  })
  
  # test data based on 25% of training dataset
  output$downloadData_calculator <- downloadHandler(
    filename = function() {
      paste('aligned_data-', '.csv', sep='')
    },
    content = function(file) {
      write.csv(alignedData_calculator(), file, row.names = FALSE)
    }
  )
  
  ##### Predictions #####
  
  # test data based on 25% of training dataset
  output$testData_prediction <- downloadHandler(
    filename = function() {
      paste('test_data-', '.csv', sep='')
    },
    content = function(file) {
      write.csv(test_data_prediction, file, row.names = FALSE)
    }
  )
  
  # valid values
  output$validValues_prediction <- downloadHandler(
    filename = function() {
      paste('validValues', '.csv', sep='')
    },
    content = function(file) {
      write.csv(valid_values, file, row.names = FALSE)
    }
  )
  
  
  #skim user-input dataset
  output$predictionDataSkim <- renderPlot({
    req(input$prediction_file)
    #read in user data
    test <- readr::read_csv(input$prediction_file$datapath) %>% 
      mutate(train_test = "test")
    #read in training data
    train <- train_data_prediction %>% 
      mutate(train_test = "train")
    #merge user data with training data
    df <- rbind(test, train)
    
    #plot histograms with overlapping data by color
    df %>% 
   dplyr::select(c(where(is.numeric), "train_test")) %>%                     # Keep only numeric columns
      gather(key = "attribue", value = "value", # Convert to key-value pairs
             -train_test) %>%                             #keep train_test variable
      ggplot(aes(x = value, fill = train_test, color = train_test)) +         # Plot the values
      facet_wrap(~ attribue, scales = "free") +   # In separate panels
     geom_density(alpha = 0.3, kernel = "rectangular") +
      scale_fill_discrete(name = "Training or Test Data") +# as density
      scale_color_discrete(name = "Training or Test Data") +# as density
      ylab("Relative Proportion") +
      xlab("Variable") +
      theme_minimal() +
      theme(legend.position = "top")
  })
  
  # Create downloadable png for skimr
  output$download_predictionDataSkim <- downloadHandler(
    filename = function() {
      paste('Train_vs_Test_Histograms', Sys.Date(), '.png', sep='')
    },
    content = function(file) {
      ggsave(file, plot = predictionDataSkim(), width = 16, height = 8, device = 'png')
    })
  
  # make predictions based on user-uploaded dataste
  prediction_reactiveDF<-eventReactive(list(input$go_predict),{
    req(input$prediction_file)
    
    #choose model based on user-selected ERM
    if(input$ERM_radio == "tissue translocation"){
      model = predictionModel_tissue.translocation
    }
    
    if(input$ERM_radio == "food dilution"){
    model = predictionModel_food.dilution}
    
    #define dataframe based on user upload
    df <- readr::read_csv(input$prediction_file$datapath) #needs to be read_csv to keep names formatted
    
    df$predictions <- caret::predict.train(model, newdata = df, na.action = na.omit)
    
    df <- df %>%
     # dplyr::select(-X) %>% 
      mutate(predictions.linear = 10 ^ predictions) %>% 
      dplyr::relocate(predictions, predictions.linear) %>% 
      rename("Predicted Conc. (particles/mL; 1-5,000 um)" = predictions.linear,
             "Predicted Conc. (log10 particles/mL; 1-5,000 um)" = predictions#,
             #"Empirical Tissue Translocation ERM Conc. (log 10 particles/mL; 1-5,000 um)" = log10.particles.mL.ox.stress.known,
             #"Empirical Food Dilution ERM Conc. (log 10 particles/mL; 1-5,000 um)" = log10.particles.mL.food.dilution.known
             )
    
    return(df)
  })
  
  #render predictions in datatable
  output$predictionsTable = DT::renderDataTable({
    req(input$prediction_file)
    
    datatable(prediction_reactiveDF() %>%  mutate_if(is.numeric, ~ signif(., 3)),
              extensions = c('Buttons'),
              style = "bootstrap",
              options = list(
                dom = 'Brtip',
                buttons = list(I('colvis'), c('copy', 'csv', 'excel')),
                scrollY = 400,
                scrollH = TRUE,
                sScrollX = TRUE,
                columnDefs = list(list(width = '50px, targets = "_all'))),#only display the table and nothing else
              caption = "Filtered Data") %>% 
      formatStyle(
        c("Predicted Conc. (particles/mL; 1-5,000 um)", "Predicted Conc. (log10 particles/mL; 1-5,000 um)"),
        backgroundColor = '#a9d6d6')
    
    })
  
  # Downloadable csv of selected dataset ----
  output$downloadData_prediction <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(prediction_reactiveDF(), file, row.names = FALSE)
    }
  )
  
  #render scatterplot comparing predicted and known concentrations
  
  output$predictionsScatter <- renderPlot({
    
    prediction_var <- input$prediction_var
    
        #choose known concentrations based on ERM
    if(input$ERM_radio == "tissue translocation"){
   scatterPlot <- prediction_reactiveDF() %>% 
      ggplot(aes(x = `Empirical Tissue Translocation ERM Conc. (log 10 particles/mL; 1-5,000 um)`,
                 y = `Predicted Conc. (log10 particles/mL; 1-5,000 um)`))
    }
    
    if(input$ERM_radio == "food dilution"){
      scatterPlot <- prediction_reactiveDF() %>% 
        ggplot(aes(x = `Empirical Food Dilution ERM Conc. (log 10 particles/mL; 1-5,000 um)`,
                   y = `Predicted Conc. (log10 particles/mL; 1-5,000 um)`
                   ))
    }
   
    #add layers to plot
    scatterPlot <- scatterPlot +
      geom_point(aes_string(color = prediction_var)) + 
      geom_smooth(method = "lm", se=TRUE, color="red", formula = y ~ x) +
      geom_abline(slope = 1, linetype = "dashed") +
      #scale_color_manual(values = variable) +
      #display r2 and equation
      ggpubr::stat_regline_equation(label.y = 7, aes(label = ..eq.label..)) +
      ggpubr::stat_regline_equation(label.y = 7.5, aes(label = ..rr.label..)) +
      xlab("Known Effect Concentrations (Particles/mL)") +
      ylab("Predicted Effect Concentrations (Particles/mL)") +
      theme_minimal()
    
   print(scatterPlot) 
   
  })
  
  # Create downloadable png for scatterplot
  output$downloadScatter_predictions <- downloadHandler(
        filename = function() {
      paste('Predicted vs. Observed Scatter', Sys.Date(), '.png', sep='')
    },
    content = function(file) {
      ggsave(file, plot = predictionsScatter(), width = 16, height = 8, device = 'png')
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
    shinyjs::reset("ingestion.translocation.switch_ssd")
    shinyjs::reset("upper.tissue.trans.size.um_ssd")
    shinyjs::reset("effect.metric_rad_ssd")
    shinyjs::reset("AF.time_rad_ssd")
    shinyjs::reset("AF.noec_rad_ssd")
    shinyjs::reset("conc.select.rad")

  }) #If we add more widgets, make sure they get added here. 
  
  
  } #Server end

#### Full App ####
shinyApp(ui = ui, server = server)


# End of R Shiny app script.
