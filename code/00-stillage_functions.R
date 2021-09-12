## FUNCTIONS FOR PROCESSING STILLAGE DATA
## DO NOT MODIFY THIS FILE,
## SOURCE THIS FILE IN YOUR SCRIPT
## KFP 2021-09-12

####################### #
####################### #

#
# PROCESS LAYOUT ----------------------------------------------------------
process_wellplate_layout = function(dat_layout){

  dat_layout %>% 
    tidyr::gather(number, sample_name, -Well) %>% 
    mutate(well_number = paste0(Well, number)) %>% 
    dplyr::select(well_number, sample_name)
  
}
#
# PROCESS DATA ------------------------------------------------------------

process_data = function(dat_data, dat_layout){
  
  dat_data %>% 
    filter(!is.na(Time)) %>% 
    mutate(Time2 = Time) %>% 
    separate(Time2, sep = ":", into = c("hh", "mm", "ss")) %>% 
    mutate(HOURS = as.numeric(hh) + (as.numeric(mm)/60) + (as.numeric(ss)/3600)) %>% 
    dplyr::select(Wavelength, HOURS, A1:A12, B1:B12, C1:C12, D1:D12, E1:E12, F1:F12, G1:G12, H1:H12) %>% 
    tidyr::gather(well_number, intensity, -c(Wavelength, HOURS)) %>% 
    filter(!is.na(intensity)) %>% 
    left_join(layout_processed) %>% 
    group_by(Wavelength, HOURS, sample_name) %>% 
    dplyr::summarise(intensity = mean(intensity),
                     intensity = round(intensity, 3)) %>% 
    separate(sample_name, sep = "_", into = c("sample_name", "media_type")) %>% 
    mutate(media_type = if_else(is.na(media_type), "sample", "blank")) %>% 
    pivot_wider(
      names_from = media_type,
      values_from = intensity,
      names_repair = "unique") %>% 
    mutate(intensity_bl_corrected = sample - blank) %>% 
    filter(!is.na(intensity_bl_corrected))

}


