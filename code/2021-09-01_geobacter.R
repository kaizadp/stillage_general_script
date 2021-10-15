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
       title = "Geobacter, Growth on Stillage")+
  #  scale_y_continuous(breaks = seq(0, 1.25, by = 0.25), labels = seq(0, 1.25, by = 0.25))+
 
theme_bw() %+replace%
  theme(legend.text = element_text(size = 15),
        legend.key.size = unit(1.5, 'lines'),
        legend.title = element_text(hjust = .5, size = 20, face = "bold"),
        plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text = element_text(size = 15, color = "black"),
        axis.title = element_text(size = 20, face = "bold", color = "black"),
        
        # formatting for facets
        panel.background = element_blank(),
        strip.background = element_rect(colour="white", fill="white"), #facet formatting
        panel.spacing.x = unit(1.5, "lines"), #facet spacing for x axis
        panel.spacing.y = unit(1.5, "lines"), #facet spacing for x axis
        strip.text.x = element_text(size=15, face="bold"), #facet labels
        strip.text.y = element_text(size=12, face="bold", angle = 270) #facet labels
  )


ggsave("output/2021-09-01_geobacter_DMR.png")


