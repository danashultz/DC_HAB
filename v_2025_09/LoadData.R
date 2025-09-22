# Load Data script for app_v2.R

# Load libraries
library(shiny)
library(bslib) #add cards to format
library(tidyverse)
library(readxl) #read in excel files
library(plotly) #interative plots
library(leaflet) #map
library(DT) #data table formatting
library(shinymanager) #authentication screen

# Import data for shiny
df_st <- read_excel("inputs/rtd_sites.xlsx")
df <- read.csv("inputs/rtd_lab_results.csv")

# Import data if running driectly from this file
# df_st <- read_excel("v_2025_09/inputs/rtd_sites.xlsx")
# df <- read.csv("v_2025_09/inputs/rtd_lab_results.csv")

# Sidebar blurbs
sp1 <-
  p(
    "The Water Boards and Restore the Delta began a collaborative partnership
    in 2022 to monitor HABs in the Delta near Stockton."
    )

sp2 <-
  p(
    "This dashboard was created by the Central Valley Water Board to
    share laboratory data from HAB monitoring of the Delta near Stockton with 
    the public and other agency staff."
  )

sp3 <- 
  p(
    #style = "text-align: center;",
    "For questions, contact",
    HTML(
      "<a href='mailto:dana.shultz@waterboards.ca.gov' > dana.shultz@\nwaterboards.ca.gov</a>"
    )
  )

# Set popup tooltip text for Stations map
# set popup text
tooltip_text <- paste0(
  "<strong>","Site: ","</strong>", df_st$StationName, " (", df_st$StationID, ")" %>%
  lapply(htmltools::HTML)
)

# Arrange data table to have most recent data at the top
df_table <- df %>%
  mutate(CollectionDate = as.Date(CollectionDate, format = "%m/%d/%Y")) %>% 
  arrange(desc(CollectionDate)) %>% 
  mutate(ProjectCode = "WB_RTD_HAB_Monitoring")

# Format data for plotting
df_plot <- df %>% 
  mutate(Year = factor(Year)) %>% 
  #Set ND results to 0.01 for plotting 
  mutate(`Total Microcystins` = case_when(`TotalMicrocystins` == "ND" ~ "0.075",
                                          TRUE ~ `TotalMicrocystins`)) %>% 
  mutate(`Total Microcystins` = as.numeric(`Total Microcystins`)) %>% 
  # rename col with "_nd" for clarity, will use for plotly tooltip
  mutate(total_microcystins_nd = TotalMicrocystins) %>%
  select(-TotalMicrocystins) %>% 
  mutate(CollectionDate = as.Date(CollectionDate, format = "%m/%d/%Y"))

# Station names for checkbox input
s <- unique(df_plot$Station)
y <- unique(df_plot$Year)

# Set some parameters for plotly scatter
#Define shapes to use for Station icons
my8shapes <- c(15,16,17,18,15,16,17,18)

ymin = min(df_plot$`Total Microcystins`)
ymax = max(df_plot$`Total Microcystins`)
