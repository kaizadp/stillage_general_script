## SCRIPT FOR PROCESSING STILLAGE DATA 
## KFP 2021-09-12

####################### #
####################### #


# I. LOAD PACKAGES -------------------------------------------------------------
library(tidyverse)  
library(lubridate)
library(readxl)

#
# II. SOURCE THE FUNCTIONS -----------------------------------------------------
source("code/00-stillage_functions.R")

#
# III. LOAD FILES --------------------------------------------------------------
# well plate layout, in wide-form
dat_layout = read_excel("data/2021-09-01_geobacter_layout.xlsx", na = "")

# intensities data, in  wide-form
dat_data = read.csv("data/2021-09-01_geobacter_data.csv", na = "")

#
# IV. PROCESS DATA -------------------------------------------------------------
# process the well plate layout data
layout_processed = process_wellplate_layout(dat_layout)

# process the intensities data and merge with the layout
data_processed = process_data(dat_data, dat_layout)

#
# V. PLOT ----------------------------------------------------------------------
data_processed %>% 
  ggplot(aes(x = HOURS, 
             y = intensity_bl_corrected, 
             color = as.character(Wavelength)))+
  geom_line()+
  facet_wrap(~sample_name)+
  labs(x = "time elapsed, hours",
       y = "intensity (blank corrected)",
       color = "wavelength, nm",
       title = "Geobacter, 2021-09-01")+
  theme_bw()

ggsave("output/2021-09-01_geobacter.png")
