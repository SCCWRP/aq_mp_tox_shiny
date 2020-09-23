#### Aquatic Microplastics Toxicology Shiny App
#### File created: September 23, 2020
#### Code contributors: Heili Lowman, Leah Thornton Hampton, Scott Coffin, Emily Darin

#### Setup ####

# Load packages
library(tidyverse)
library(patchwork)
library(calecopal)
library(shiny)
library(shinythemes)

# Load finalized dataset
aoc <- read_csv("AquaticOrganisms_Clean_final.csv", guess_max = 10000)

# Create Shiny app

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
                    p("You can add paragraphs of text this way, each using a new p()."),
                    br(), # line break
                    verbatimTextOutput(outputId = "Heili1")),
        
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
  output$Heili1 <- renderText({
    paste0("You can also add outputs like this. Every output (text, plot, table) has a render function equivalent (renderText, renderPlot, renderTable).")
  })

#### Scott S ####
  output$Scott1 <- renderText({
    paste0("You can also add outputs like this. Every output (text, plot, table) has a render function equivalent (renderText, renderPlot, renderTable).")
  })
  
  
  }

#### Full App ####
shinyApp(ui = ui, server = server)

# End of R Shiny app script.