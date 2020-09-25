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

# Load finalized dataset.
aoc <- read_csv("AquaticOrganisms_Clean_final.csv", guess_max = 10000)

# Add log transformed concentration columns for easier plotting below.
aoc$log_dose.mg.L <- log10(aoc$dose.mg.L)
aoc$log_dose.particles.mL <- log10(aoc$dose.particles.mL)

# Add factor and releved effects column.
aoc$effect_f <- factor(aoc$effect, levels = c("Y", "N"))

#### Leah Setup ####

#### Emily Setup ####

#### Heili Setup ####

# Master dataset for scatterplots - for Heili's tab.
aoc_y <- aoc %>% # start with original dataset
  # full dataset filters.
  filter(effect == "Y") %>% # only includes those datapoints with demonstrated effects.
  filter(environment != "Terrestrial") %>% # removes terrestrial data.
  # size category data tidying.
  mutate(size.category.noNA = replace_na(size.category, 0)) %>% # replaces NA with 0 so we can better relabel it.
  mutate(size_cat = case_when(
    size.category.noNA == 1 ~ "<1µm",
    size.category.noNA == 2 ~ "1µm < 10µm",
    size.category.noNA == 3 ~ "10µm < 100µm",
    size.category.noNA == 4 ~ "100µm < 1mm",
    size.category.noNA == 5 ~ "1mm < 5mm",
    size.category.noNA == 0 ~ "unavailable")) %>% # creates new column with nicer names.
  mutate(size_f = factor(size_cat, levels = c("<1µm", "1µm < 10µm", "10µm < 100µm", "100µm < 1mm", "1mm < 5mm", "unavailable"))) %>% # order our different size levels.
  # shape category data tidying.
  mutate(shape.noNA = replace_na(shape, "unavailable")) %>% # replaces NAs to better relabel.
  mutate(shape_f = factor(shape.noNA, levels = c("fiber", "fragment", "sphere", "unavailable"))) %>% # order our different shapes.
  # polymer category data tidying.
  mutate(polymer.noNA = replace_na(polymer, "unavailable")) %>% # replaces NA to better relabel.
  mutate(poly_f = factor(polymer.noNA, levels = c("BIO", "EVA", "PA", "PC", "PE", "PET", "PLA", "PMMA", "PP", "PS", "PUR", "PVC", "unavailable"))) %>% # order our different polymers.
  # taxonomic category data tidying.
  mutate(organism.noNA = replace_na(organism.group, "unavailable")) %>% # replaces NA to better relabel.
  mutate(org_f = factor(organism.noNA, levels = c("Algae", "Annelida", "Bacteria", "Cnidaria", "Crustacea", "Echinoderm", "Fish", "Insect", "Mollusca", "Nematoda", "Plant", "Rotifera", "unavailable"))) # order our different organisms.

#### Scott Setup ####

# Create Shiny app. Anything in the sections below (user interface & server) should be the reactive/interactive parts of the shiny application.

#### User Interface ####
ui <- fluidPage(
  
  # App title
  titlePanel("Aquatic Microplastics Toxicology Review"),
  
  # Title panel subtext
  tags$div(
    "This is a draft website to present the results of the aquatic microplastics toxicology literature review. Do not use without prior consulting with Leah Thornton Hampton (leahth@sccwrp.org)."),
  
  br(), # line break
  
  # Main panel for displaying outputs
  mainPanel(
    
      # Output: set of 5 tabs
      tabsetPanel(type = "tabs",

#### Leah UI ####        
                  tabPanel("Introduction", 
                    br(), # line break
                    p("You can add paragraphs of text this way, each using a new p()."),
                    br(), # line break
                    verbatimTextOutput(outputId = "Leah1")),
                  tabPanel("Metadata", 
                    br(), # line break
                    p("You can add paragraphs of text this way, each using a new p()."),
                    br(), # line break
                    verbatimTextOutput(outputId = "Leah2")),
        
#### Emily UI ####
                  tabPanel("Data Overview", 
                    br(), # line break
                    p("You can add paragraphs of text this way, each using a new p()."),
                    br(), # line break
                    verbatimTextOutput(outputId = "Emily1")),
        
#### Heili UI ####
                  tabPanel("Data Exploration", 
                    br(), # line break
                    p("The figures below display data from the literature review of toxicological effects of microplastics on aquatic organisms. All data displayed - individual points and boxplots - are from studies in which there was a demonstrated significant toxicological effect of microplastics."),
                    br(), # line break
                    p("Each row of figures displays a different value along the y-axis - size, shape, and polymer, respectively. Each column of figures displays a different unit along the x-axis - mg/L and particles/mL, respectively. The data may be filtered by organism and/or endpoint using the selection widgets on the left-hand side of the window."),
                    br(), # line break
                    
                    sidebarPanel("Use the options below to filter the dataset.",
                      br(), # line break
                      radioButtons(inputId = "organism_select", # multiple choice
                        label = "Organisms:",
                        choices = unique(aoc_y$org_f)),
                    br()), # line break
                    mainPanel("Microplastics in Aquatic Environments Data Exploration of Toxicological EFfects",
                      p(" "),
                      plotOutput(outputId = "ssp_plot"))), # patchwork plot
        
#### Scott UI ####
                  tabPanel("Species Sensitivity Distribution", 
                    br(), # line break
                    p("You can add paragraphs of text this way, each using a new p()."),
                    br(), # line break
                    verbatimTextOutput(outputId = "Scott1"))
        )
  )
  
  )

#### Server ####
server <- function(input, output) {
  
#### Leah S ####
  output$Leah1 <- renderText({
    paste0("You can also add outputs like this. Every output (text, plot, table) has a render function equivalent (renderText, renderPlot, renderTable).")
  })
  
  output$Leah2 <- renderText({
    paste0("You can also add outputs like this. Every output (text, plot, table) has a render function equivalent (renderText, renderPlot, renderTable).")
  })
  
#### Emily S ####
  output$Emily1 <- renderText({
    paste0("You can also add outputs like this. Every output (text, plot, table) has a render function equivalent (renderText, renderPlot, renderTable).")
  })
  
#### Heili S ####
  
  output$ssp_plot <- renderPlot({
    ggplot(aoc_y, aes(x = dose.mg.L, y = size_f)) +
      scale_x_log10(breaks = c(0.0001, 0.01, 1, 100, 10000), 
        labels = c(0.0001, 0.01, 1, 100, 10000)) +
      geom_boxplot(alpha = 0.7, show.legend = FALSE, aes(color = size_f, fill = size_f)) +
      scale_color_manual(values = cal_palette("sbchannel", n = 6, type = "continuous")) +
      scale_fill_manual(values = cal_palette("sbchannel", n = 6, type = "continuous")) +
      geom_jitter(size = 3, alpha = 0.2, height = 0.1, color = "grey80") +
      theme_classic() +
      theme(legend.position="none") +
      labs(x = "Concentration (mg/L)",
        y = "Size")
  })

#### Scott S ####
  output$Scott1 <- renderText({
    paste0("You can also add outputs like this. Every output (text, plot, table) has a render function equivalent (renderText, renderPlot, renderTable).")
  })
  
  
  }

#### Full App ####
shinyApp(ui = ui, server = server)

# End of R Shiny app script.