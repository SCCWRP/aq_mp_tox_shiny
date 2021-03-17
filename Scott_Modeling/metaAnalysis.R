#Load packages ####

# Attach packages necessary for the actions below.
library(tidyverse) # For data tidying/viz purposes.
library(devtools) # For installing packages from github.
library(calecopal) # For palettes used in figures below.
library(plotly) #make plots interactive
library(viridis) #colors
library(patchwork)
library(ggrepel) #prevent overlapping labels in ggplot

# To install Calecopal, run devtools::install_github("an-bui/calecopal") in console.
# See available palettes by running names(cal_palettes) in console.
# Display a palette by running cal_palette("sbchannel") in console.


##### Data preparation #####

# Import finalized dataset.

# Guess_max ensures columns with lots of NAs are not imported as logical vectors, but as numeric/double.
aoc <- read_csv("AquaticOrganisms_Clean_final.csv", guess_max = 10000)
