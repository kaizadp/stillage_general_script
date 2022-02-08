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
source("00-stillage_functions.R")

#
# III. LOAD FILES --------------------------------------------------------------
# well plate layout, in wide-form
dat_layout = read_excel("Depleted_DMR_R_plate_layout_22_2_1.xlsx", na = "")

# intensities data, in  wide-form
dat_data = read.csv("DMR_growth_plate1_data_Rstudio_copy.csv", na = "")

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
             y = intensity_Blk_corrected, 
             color = as.character(Wavelength)))+
  geom_line()+
  facet_wrap(~ sample_name, labeller = labeller(sample_name = label_wrap_gen(width = 16)))+
  labs(x = "Time, (Hours)",
       y = "Optical Density",
       color = "wavelength, nm",
       title = "Geobacter, Growth on DMR and Various Media ")+
  #  scale_y_continuous(breaks = seq(0, 1.25, by = 0.25), labels = seq(0, 1.25, by = 0.25))+
  
  theme_bw() %+replace%
  theme(legend.text = element_text(size = 15),
        legend.key.size = unit(1.5, 'lines'),
        legend.title = element_text(hjust = .5, size = 20, face = "bold"),
        plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 20, face = "bold", color = "black"),
        
        # formatting for facets
        panel.background = element_blank(),
        strip.background = element_rect(colour="white", fill="white"), #facet formatting
        panel.spacing.x = unit(1.5, "lines"), #facet spacing for x axis
        panel.spacing.y = unit(1.5, "lines"), #facet spacing for x axis
        strip.text.x = element_text(size=15, face="bold"), #facet labels
        strip.text.y = element_text(size=12, face="bold", angle = 270) #facet labels
  )


ggsave("output/2021-12-22_geobacter_w_o_DMR_730_only.png")
ggsave("output/2021-12-22_geobacter_DMR.png")

## graph only SD-1 


data_processed %>% 
  filter(grepl("SD-1", sample_name)) %>%
  ggplot(aes(x = HOURS, 
             y = intensity_Blk_corrected, 
             color = as.character(Wavelength)))+
  geom_line()+
  facet_wrap(~ sample_name, labeller = labeller(sample_name = label_wrap_gen(width = 16)))+
  labs(x = "Time, (Hours)",
       y = "Optical Density",
       color = "wavelength, nm",
       title = "Geobacter, Growth on DMR and Various Media ")+
  #  scale_y_continuous(breaks = seq(0, 1.25, by = 0.25), labels = seq(0, 1.25, by = 0.25))+
  
  theme_bw() %+replace%
  theme(legend.text = element_text(size = 15),
        legend.key.size = unit(1.5, 'lines'),
        legend.title = element_text(hjust = .5, size = 20, face = "bold"),
        plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 20, face = "bold", color = "black"),
        
        # formatting for facets
        panel.background = element_blank(),
        strip.background = element_rect(colour="white", fill="white"), #facet formatting
        panel.spacing.x = unit(1.5, "lines"), #facet spacing for x axis
        panel.spacing.y = unit(1.5, "lines"), #facet spacing for x axis
        strip.text.x = element_text(size=15, face="bold"), #facet labels
        strip.text.y = element_text(size=12, face="bold", angle = 270) #facet labels
  )









# combining two dates -----------------------------------------------------


# well plate layout, in wide-form
dat_layout = read_excel("Depleted_DMR_R_plate_layout_22_2_1.xlsx", na = "")

# intensities data, in  wide-form
dat_data1 = read.csv("DMR_growth_plate1_data_Rstudio_copy.csv", na = "")
dat_data2 = read.csv("DMR_growth_plate2_data_Rstudio_copy.csv", na = "")

layout_processed = process_wellplate_layout(dat_layout)

# process the intensities data and merge with the layout
data_processed1 = process_data(dat_data1, dat_layout)
data_processed2 = process_data(dat_data2, dat_layout)

# get the max time from data_processed1 and add to data_processed2
max_hr = 
  data_processed1 %>% 
  ungroup() %>% 
  dplyr::summarise(max = max(HOURS)) %>% 
  pull(max)

data_processed2_v2 = 
  data_processed2 %>% 
  mutate(HOURS = HOURS + max_hr)

data_processed_combined = 
  bind_rows(data_processed1, data_processed2_v2)


data_processed_combined %>% 
  filter(grepl("SD-1", sample_name)) %>% 
  ggplot(aes(x = HOURS, 
             y = intensity_Blk_corrected, 
             color = as.character(Wavelength)))+
  geom_line()+
  facet_wrap(~ sample_name, labeller = labeller(sample_name = label_wrap_gen(width = 16)))+
  labs(x = "Time, (Hours)",
       y = "Optical Density",
       color = "wavelength, nm",
       title = "SD-1 Growth on DMR and Various Media ")+
  #  scale_y_continuous(breaks = seq(0, 1.25, by = 0.25), labels = seq(0, 1.25, by = 0.25))+
  
  theme_bw() %+replace%
  theme(legend.text = element_text(size = 15),
        legend.key.size = unit(1.5, 'lines'),
        legend.title = element_text(hjust = .5, size = 20, face = "bold"),
        plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 20, face = "bold", color = "black"),
        
        # formatting for facets
        panel.background = element_blank(),
        strip.background = element_rect(colour="white", fill="white"), #facet formatting
        panel.spacing.x = unit(1.5, "lines"), #facet spacing for x axis
        panel.spacing.y = unit(1.5, "lines"), #facet spacing for x axis
        strip.text.x = element_text(size=15, face="bold"), #facet labels
        strip.text.y = element_text(size=12, face="bold", angle = 270) #facet labels
  )

ggsave("output/2021-12-28_geobacter_DMR_plate.png")

crunch::write.csv(data_processed_combined, paste0("data_processed_combined",".csv"), row.names = F, na="")

