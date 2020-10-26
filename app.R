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

polydf<-rowPerc(xtabs( ~polymer +effect, aoc))
polyf<-as.data.frame(polydf)%>% 
  filter(effect %in% c("Y","N"))%>%
  rename(type= "polymer")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Polymer")

polyf

sizedf<-rowPerc(xtabs(~size.category +effect, aoc))
sizef<-as.data.frame(sizedf)%>%
  filter(effect %in% c("Y","N"))%>%
  mutate(size.category = case_when(
    size.category == 1 ~ "<1µ",
    size.category == 2 ~ "1µ < 10µ",
    size.category == 3 ~ "10µ < 100µ",
    size.category == 4 ~ "100µ < 1mm",
    size.category == 5 ~ "1mm < 5mm",
    size.category == 0 ~ "unavailable"))%>%
  rename(type= "size.category")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Size")

shapedf<-rowPerc(xtabs(~shape + effect, aoc))
shapef<-as.data.frame(shapedf)%>%
  filter(effect %in% c("Y","N"))%>%
  rename(type="shape")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Shape")

taxdf<-rowPerc(xtabs(~organism.group +effect, aoc))
taxf<-as.data.frame(taxdf)%>%
  filter(effect %in% c("Y","N"))%>%
  rename(type= "organism.group")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Organism")


lvl1df<-rowPerc(xtabs(~lvl1 +effect, aoc))
lvl1f<-as.data.frame(lvl1df)%>%
  filter(effect %in% c("Y","N"))%>%
  rename(type= "lvl1")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Lvl1")


lifedf<-rowPerc(xtabs(~life.stage +effect, aoc))
lifef<-as.data.frame(lifedf)%>%
  filter(effect %in% c("Y","N"))%>%
  rename(type= "life.stage")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Life.stage")


vivodf<-rowPerc(xtabs(~invitro.invivo +effect, aoc))
vivof<-as.data.frame(vivodf)%>%
  filter(effect %in% c("Y","N"))%>%
  rename(type= "invitro.invivo")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Invivo.invivo")


routedf<-rowPerc(xtabs(~exposure.route +effect, aoc))
routef<-as.data.frame(routedf)%>%
  filter(effect %in% c("Y","N"))%>%
  rename(type= "exposure.route")%>%
  mutate_if(is.numeric, round,0)%>%
  mutate(plot="Exposure.route")

A<-rbind(polyf,shapef)
B<-rbind(A,sizef)
C<-rbind(B,taxf)
D<-rbind(C,lvl1f)
E<-rbind(D,lifef)
G<-rbind(E,vivof)
Final_effect_dataset<-rbind(G,routef)

Final_effect_dataset<-Final_effect_dataset%>%
  mutate(plot_f=factor(plot))

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
    renderPlot({
      
      # use the original dataset
      Final_effect_dataset %>%
        
        # filter by input
        filter(plot_f==i) %>%
        
        # generate plot
        ggplot(aes(fill=effect, y=Freq, x=type)) +
        geom_bar(position="stack", stat="identity") +
        geom_text(aes(label= paste0(Freq,"%")), position = position_stack(vjust = 0.5),colour="orange2") +
        scale_fill_manual(values = cal_palette("wetland")) +
        theme_classic() +
        labs(fill="Effect") +
        theme(legend.position = "right",
          axis.ticks=element_blank(),
          axis.text.y=element_blank(),
          axis.title.y = element_blank())
      
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
                      tags$a(href="https://twitter.com/DrScottCoffin", tags$img(src="twitter.png", width="2%", height="2%")), tags$a(href="https://github.com/ScottCoffin", tags$img(src="github.png", width="2%", height="2%"))),
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
        
    
pickerInput(inputId = "Emily_check", # effect checklist
            label = "Effects:", # checklist label
            choices = levels(Final_effect_dataset$plot_f), # options for user
            selected = "Polymer", # default selected
            multiple = TRUE), # allows for multiple selections at once
            br(),
uiOutput(outputId= "Emily_plot")),

#### Heili UI ####
                  tabPanel("Data Exploration", 
                    h3("Microplastics in Aquatic Environments: Data Exploration of Toxicological Effects", align = "center", style = "color:darkcyan"),
                    br(), # line break
                    p("The figures below display data from the literature review of toxicological effects of microplastics on aquatic organisms. All data displayed - individual points and boxplots - are from studies in which there was a demonstrated significant toxicological effect of microplastics."),
                    br(), # line break
                    p("Each row of figures displays a different value along the y-axis - size, shape, and polymer, respectively. Each column of figures displays a different unit along the x-axis - mg/L and particles/mL, respectively.The data may be filtered by organism and/or endpoint using the selection widgets on the left-hand side of the window."),
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
                        multiple = TRUE))), # allows for multiple inputs
                      
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
                              )),
        
        # dummy tab entered by Heili
        tabPanel("File Upload", 
          
          br(), # line break
          
          h3("Additional Data Exploration", align = "center", style = "color:darkcyan"),
          
          p("Use the file upload feature on the left-hand side of the page to upload your own dataset and explore it using the resulting plot. Datasets may only be uploaded in '.csv' format. Column titles must be one of the following: state, region_us_census, rank, costume, candy, pounds_candy_sold."),
          
          sidebarLayout(
            
          sidebarPanel(

            fileInput("file1", "Drag and drop data file here:", # .csv file input
              multiple = FALSE,
              accept = c(".csv"))),

          mainPanel(p("Region's top costumes:"),
            plotOutput(outputId = "costume_graph"))
            )
        )
          
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
  
  # Create new dataset based on widget filtering.
  aoc_filter <- reactive({
    aoc_y %>%
      filter(org_f %in% input$organism_check) %>%
      filter(lvl1_f %in% input$lvl1_check)
  })
  
  # Use newly created dataset from above to generate plotly plots for size, shape, and polymer plots on three different rows (for sizing display purposes).
  
  output$size_plot_react <- renderPlot({
    
    size1 <- ggplot(aoc_filter(), aes(x = dose.mg.L, y = size_f)) +
      geom_boxplot(alpha = 0.7, show.legend = FALSE, aes(color = size_f, fill = size_f)) +
      scale_x_log10(breaks = c(0.0001, 0.01, 1, 100, 10000), 
        labels = c(0.0001, 0.01, 1, 100, 10000)) +
      scale_color_manual(values = cal_palette("sbchannel", n = 6, type = "continuous")) +
      scale_fill_manual(values = cal_palette("sbchannel", n = 6, type = "continuous")) +
      theme_classic() +
      labs(x = "Concentration (mg/L)",
        y = "Size")
    
    size2 <- ggplot(aoc_filter(), aes(x = dose.particles.mL, y = size_f)) +
      geom_boxplot(alpha = 0.7, show.legend = FALSE, aes(color = size_f, fill = size_f)) +
      scale_x_log10(breaks = c(1, 10000, 100000000, 1000000000000, 10000000000000000), 
        labels = c(1, 10000, 100000000, 1000000000000, 10000000000000000)) +
      scale_color_manual(values = cal_palette("sbchannel", n = 6, type = "continuous")) +
      scale_fill_manual(values = cal_palette("sbchannel", n = 6, type = "continuous")) +
      theme_classic() +
      labs(x = "Concentration (particles/mL)",
        y = " ")
    
    (size1 + size2) # using patchwork to combine figures
    
  })
  
  output$shape_plot_react <- renderPlot({
    
    shape1 <- ggplot(aoc_filter(), aes(x = dose.mg.L, y = shape_f)) +
      scale_x_log10(breaks = c(0.0001, 0.01, 1, 100, 10000), 
        labels = c(0.0001, 0.01, 1, 100, 10000)) +
      geom_boxplot(alpha = 0.7, show.legend = FALSE, aes(color = shape_f, fill = shape_f)) +
      scale_color_manual(values = cal_palette("chaparral3")) +
      scale_fill_manual(values = cal_palette("chaparral3")) +
      theme_classic() +
      labs(x = "Concentration (mg/L)",
        y = "Shape")
    
    shape2 <- ggplot(aoc_filter(), aes(x = dose.particles.mL, y = shape_f)) +
      scale_x_log10(breaks = c(1, 10000, 100000000, 1000000000000, 10000000000000000), 
        labels = c(1, 10000, 100000000, 1000000000000, 10000000000000000)) +
      geom_boxplot(alpha = 0.7, show.legend = FALSE, aes(color = shape_f, fill = shape_f)) +
      scale_color_manual(values = cal_palette("chaparral3")) +
      scale_fill_manual(values = cal_palette("chaparral3")) +
      theme_classic() +
      labs(x = "Concentration (particles/mL)",
        y = " ")
    
    (shape1 + shape2) # patchwork combining plots
    
  })
  
  output$poly_plot_react <- renderPlot({
    
    poly1 <- ggplot(aoc_filter(), aes(x = dose.mg.L, y = poly_f)) +
      scale_x_log10(breaks = c(0.0001, 0.01, 1, 100, 10000), 
        labels = c(0.0001, 0.01, 1, 100, 10000)) +
      geom_boxplot(alpha = 0.7, show.legend = FALSE, aes(color = poly_f, fill = poly_f)) +
      scale_color_manual(values = cal_palette("canary", n = 15, type = "continuous")) +
      scale_fill_manual(values = cal_palette("canary", n = 15, type = "continuous")) +
      theme_classic() +
      labs(x = "Concentration (mg/L)",
        y = "Polymer")
    
    poly2 <- ggplot(aoc_filter(), aes(x = dose.particles.mL, y = poly_f)) +
      scale_x_log10(breaks = c(1, 10000, 100000000, 1000000000000, 10000000000000000), 
        labels = c(1, 10000, 100000000, 1000000000000, 10000000000000000)) +
      geom_boxplot(alpha = 0.7, show.legend = FALSE, aes(color = poly_f, fill = poly_f)) +
      scale_color_manual(values = cal_palette("canary", n = 15, type = "continuous")) +
      scale_fill_manual(values = cal_palette("canary", n = 15, type = "continuous")) +
      theme_classic() +
      labs(x = "Concentration (particles/mL)",
        y = " ")
    
    (poly1 + poly2) # join plots together using patchwork
    
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

