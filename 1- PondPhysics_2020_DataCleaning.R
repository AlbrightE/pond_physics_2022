# Project: "Macrophyte-hydrodynamic interactions mediate stratification and dissolved oxygen dynamics in ponds"
#           Part of ISU Hort Farm Pond Study, 2020
# Last Modified 22 September 2022
# Contributors: Ellen Albright, Robert Ladwig, Grace Wilkinson, Tyler Butts, Nate Lawrence
# Description: Field data cleaning - high frequency temperature loggers (t-chains) and manual YSI profiles for DO

# Data citation: ______________

# The cleaning code produces the following data tables:
# 1. Six text files of high frequency temperature profiles in wide format for use in r Lake Analyzer (one file per site)
#      "temp_hf_Fdeep_wide.txt" - Pond F, deep site
#      "temp_hf_Bdeep_wide.txt" - Pond B, deep site
#      "temp_hf_Fshallow_wide.txt" - Pond F, shallow site
#      "temp_hf_Bshallow_wide.txt" - Pond B, shallow site
#      "temp_hf_Fmid_wide.txt" - Pond F, middle site
#      "temp_hf_Bmid_wide.txt" - Pond B, middle site
# 2. One long-format csv file of all high frequency temperature profile data (all sites combined)
#      "temp_profiles_2020.csv"
# 3. Six individual csv files for each high frequency temperature profile site, with interpolated values
#      "temp_interpolated_Fdeep.csv"
#      "temp_interpolated_Fmiddle.csv"
#      "temp_interpolated_Fshallow.csv"
#      "temp_interpolated_Bdeep.csv"
#      "temp_interpolated_Bmiddle.csv"
#      "temp_interpolated_Bshallow.csv"
# 3. One csv file of all manual YSI multi-parameter profiles (18 sites across 2 ponds)
#      "DO_profiles.csv"


# Clear environment, set working directory - UPDATE AS NEEDED ---------------------------------------------------------
rm(list=ls())
setwd('C:/Users/Ellen/Desktop/Box Sync/Albright DISSERTATION/Albright_Chapter_4/DATA/SUBMIT')

# REQUIRED PACKAGES FOR CODE - install as needed
# install.packages("tidyverse")
# install.packages("RColorBrewer")
# install.packages("viridis")
# install.packages("lubridate")
library(tidyverse)
library(RColorBrewer)
library(viridis)
library(lubridate)

# OVERVIEW OF SCRIPT ORGANIZATION ### ----------------------------------------------------------------------------------------------------------------
# PART 1 - High Frequency Temperature Profiles - Data cleaning
#      1A - Pond F, Deep Site
#      1B - Pond B, Deep Site
#      1C - Pond F, Shallow Site
#      1D - Pond B, Shallow Site
#      1E - Pond F, Middle Site
#      1F - Pond B, Middle Site
#      1G - Bind it all together!
# PART 2 - High Frequency Temperature Profiles - interpolation for visualization
# PART 3 - Manual Sonde Profiles - Data Cleaning

# High Frequency Temperature Profiles - Data cleaning  ------------------------------------------------------------------------------------------
### PART 1A - Pond F, Deep Site ---------------------------------------------------------------------------------
# Read in individual csv's from each temperature logger (each represents a different depth in profile)
# Select columns you want to work with, and give them sensible names. Add columns for pond, site id, and water depth
F_Deep_0<-read.csv("F_Deep_0m.csv")
F_Deep_0<-F_Deep_0 %>% 
  rename(datetime=?..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=19, temp_depth_m=0) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 
  
F_Deep_0.25<-read.csv("F_Deep_0.25m.csv")
F_Deep_0.25<-F_Deep_0.25 %>% 
  rename(datetime=?..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=19, temp_depth_m=0.25) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_Deep_0.5<-read.csv("F_Deep_0.5m.csv")
F_Deep_0.5<-F_Deep_0.5 %>% 
  rename(datetime=?..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=19, temp_depth_m=0.5) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_Deep_0.75<-read.csv("F_Deep_0.75m.csv")
F_Deep_0.75<-F_Deep_0.75 %>% 
  rename(datetime=?..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=19, temp_depth_m=0.75) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_Deep_1<-read.csv("F_Deep_1m.csv")
F_Deep_1<-F_Deep_1 %>% 
  rename(datetime=?..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=19, temp_depth_m=1) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_Deep_1.25<-read.csv("F_Deep_1.25m.csv")
F_Deep_1.25<-F_Deep_1.25 %>% 
  rename(datetime=?..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=19, temp_depth_m=1.25) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_Deep_1.5<-read.csv("F_Deep_1.5m.csv")
F_Deep_1.5<-F_Deep_1.5 %>% 
  rename(datetime=?..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=19, temp_depth_m=1.5) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_Deep_2<-read.csv("F_Deep_2m.csv")
F_Deep_2<-F_Deep_2 %>% 
  rename(datetime=?..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=19, temp_depth_m=2) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_deep<-rbind(F_Deep_0,F_Deep_0.25,F_Deep_0.5,F_Deep_0.75,F_Deep_1,F_Deep_1.25,F_Deep_1.5,F_Deep_2)
F_deep<-filter(F_deep,doy>"142"&doy<"241") #filter for data range of all sensors in lake

str(F_deep) #datetime became a character somehow, need to address that
# convert character data to date and time using lubridate (code from T. Butts)
F_deep_date = F_deep %>% 
  mutate(datetime = mdy_hm(datetime))
View(F_deep_date)
str(F_deep_date) #yay it's a date in ISO 8601 format!

# Quick visualizations to make sure everything looks alright - looking good!
ggplot(data=F_deep_date, aes(x=doy_frac, y=temp_c, group=temp_depth_m))+
  geom_line(aes(color=temp_depth_m),size=1)+
  scale_color_viridis_c(direction = -1)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Temperature (C)")+theme(axis.text=element_text(color="black",size=12),axis.title=element_text(size=14))+
  theme(legend.title=element_blank(), legend.position = c(0.8,0.1),legend.text = element_text(size=14))

ggplot(F_deep_date, aes(doy, temp_depth_m, fill= temp_c)) + 
  geom_tile()+xlim(143,233)+
  scale_fill_viridis(option="C",name="Temperature (C)")+
  scale_y_reverse()+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Depth (m)")+theme(axis.text=element_text(color="black",size=12),legend.position="none",axis.title=element_text(size=14))
# ggsave("F_deep_heatmap.png",width=6.5, height=3.3, units="in", dpi=300)

#pivot to wide format to work with lake analyzer and similar ... save as tab-delimited file
F_deep_wide<-F_deep_date %>%
  select(datetime,temp_depth_m,temp_c) %>% 
  pivot_wider(names_from=temp_depth_m,values_from=temp_c) %>% 
  rename(wtr_0="0", wtr_0.25="0.25", wtr_0.5="0.5", wtr_0.75="0.75", wtr_1="1", wtr_1.25="1.25", wtr_1.5="1.5", wtr_2="2")
write.table(F_deep_wide, file="temp_hf_Fdeep_wide.txt",row.names = FALSE,sep = "\t")

# Write one csv for temperature profiles from deep site, pond F
write.csv(F_deep_date,file="temp_hf_Fdeep.csv",row.names = FALSE)


### PART 1B - Pond B, Deep Site ------------------------------------------------------------------------------------------
# Read in individual csv's from each temperature logger (each represents a different depth in profile)
# Select columns you want to work with, and give them sensible names. Add columns for pond, site id, and water depth
B_Deep_0<-read.csv("B_Deep_0m.csv")
B_Deep_0<-B_Deep_0 %>% 
  rename(datetime=?..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="B", site_id=19, temp_depth_m=0) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

B_Deep_0.25<-read.csv("B_Deep_0.25m.csv")
B_Deep_0.25<-B_Deep_0.25 %>% 
  rename(datetime=?..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="B", site_id=19, temp_depth_m=0.25) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

B_Deep_0.5<-read.csv("B_Deep_0.5m.csv")
B_Deep_0.5<-B_Deep_0.5 %>% 
  rename(datetime=?..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="B", site_id=19, temp_depth_m=0.5) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

B_Deep_0.75<-read.csv("B_Deep_0.75m.csv")
B_Deep_0.75<-B_Deep_0.75 %>% 
  rename(datetime=?..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="B", site_id=19, temp_depth_m=0.75) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

B_Deep_1.25<-read.csv("B_Deep_1.25m.csv")
B_Deep_1.25<-B_Deep_1.25 %>% 
  rename(datetime=?..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="B", site_id=19, temp_depth_m=1.25) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

B_Deep_1.5<-read.csv("B_Deep_1.5m.csv")
B_Deep_1.5<-B_Deep_1.5 %>% 
  rename(datetime=?..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="B", site_id=19, temp_depth_m=1.5) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

B_Deep_2<-read.csv("B_Deep_2m.csv")
B_Deep_2<-B_Deep_2 %>% 
  rename(datetime=?..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="B", site_id=19, temp_depth_m=2) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

B_deep<-rbind(B_Deep_0,B_Deep_0.25,B_Deep_0.5,B_Deep_0.75,B_Deep_1.25,B_Deep_1.5,B_Deep_2)
B_deep<-filter(B_deep,doy>"142"&doy<"241") #filter for data range of all sensors in lake

str(B_deep) #datetime became a character somehow, need to address that
# convert character data to date and time using lubridate (code from T. Butts)
B_deep_date = B_deep %>% 
  mutate(datetime = mdy_hm(datetime))
View(B_deep_date)
str(B_deep_date) #yay it's a date in ISO 8601 format!

# Quick visualizations to make sure everything looks alright - looking good!
ggplot(data=B_deep_date, aes(x=doy_frac, y=temp_c, group=temp_depth_m))+
  geom_line(aes(color=temp_depth_m),size=1)+
  scale_color_viridis_c(direction = -1)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Temperature (C)")+theme(axis.text=element_text(color="black",size=12),axis.title=element_text(size=14))+
  theme(legend.title=element_blank(), legend.position = c(0.8,0.1),legend.text = element_text(size=14))

ggplot(B_deep_date, aes(doy, temp_depth_m, fill= temp_c)) + 
  geom_tile()+xlim(143,233)+
  scale_fill_viridis(option="C",name="Temperature (C)")+
  scale_y_reverse()+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Depth (m)")+theme(axis.text=element_text(color="black",size=12),legend.position="none",axis.title=element_text(size=14))
# ggsave("B_deep_heatmap.png",width=6.5, height=3.3, units="in", dpi=300)

#pivot to wide format to work with lake analyzer and similar ... save as tab-delimited file
B_deep_wide<-B_deep_date %>%
  select(datetime,temp_depth_m,temp_c) %>% 
  pivot_wider(names_from=temp_depth_m,values_from=temp_c) %>% 
  rename(wtr_0="0", wtr_0.25="0.25", wtr_0.5="0.5", wtr_0.75="0.75", wtr_1.25="1.25", wtr_1.5="1.5", wtr_2="2")
write.table(B_deep_wide, file="temp_hf_Bdeep_wide.txt",row.names = FALSE,sep = "\t")

# Write one csv for temperature profiles from deep site, pond B
write.csv(B_deep_date,file="temp_hf_Bdeep.csv",row.names = FALSE)

### PART 1C - Pond F, Shallow Site ---------------------------------------------------------------------------------------------------
# Read in individual csv's from each temperature logger (each represents a different depth in profile)
# Select columns you want to work with, and give them sensible names. Add columns for pond, site id, and water depth
F_Shallow_0<-read.csv("F_Shallow_0m.csv")
F_Shallow_0<-F_Shallow_0 %>% 
  rename(datetime=?..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=21, temp_depth_m=0) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_Shallow_0.75<-read.csv("F_Shallow_0.75m.csv")
F_Shallow_0.75<-F_Shallow_0.75 %>% 
  rename(datetime=?..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=21, temp_depth_m=0.75) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_Shallow_1<-read.csv("F_Shallow_1m.csv")
F_Shallow_1<-F_Shallow_1 %>% 
  rename(datetime=?..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=21, temp_depth_m=1) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c)

F_Shallow_1.25<-read.csv("F_Shallow_1.25m.csv")
F_Shallow_1.25<-F_Shallow_1.25 %>% 
  rename(datetime=?..DateTime, doy=DOY, doy_frac=DOY_frace, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=21, temp_depth_m=1.25) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_Shallow_1.5<-read.csv("F_Shallow_1.5m.csv")
F_Shallow_1.5<-F_Shallow_1.5 %>% 
  rename(datetime=?..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=21, temp_depth_m=1.5) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_Shallow<-rbind(F_Shallow_0,F_Shallow_0.75,F_Shallow_1,F_Shallow_1.25,F_Shallow_1.5)
F_Shallow<-filter(F_Shallow,doy>"142"&doy<"241") #filter for data range of all sensors in lake

str(F_Shallow) #datetime became a character somehow, need to address that
# convert character data to date and time using lubridate (code from T. Butts)
F_Shallow_date = F_Shallow %>% 
  mutate(datetime = mdy_hm(datetime))
str(F_Shallow_date) #yay it's a date in ISO 8601 format!

# Quick visualizations to make sure everything looks alright - looking good!
ggplot(data=F_Shallow_date, aes(x=doy_frac, y=temp_c, group=temp_depth_m))+
  geom_line(aes(color=temp_depth_m),size=1)+
  scale_color_viridis_c(direction = -1)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Temperature (C)")+theme(axis.text=element_text(color="black",size=12),axis.title=element_text(size=14))+
  theme(legend.title=element_blank(), legend.position = c(0.8,0.1),legend.text = element_text(size=14))

ggplot(F_Shallow_date, aes(doy, temp_depth_m, fill= temp_c)) + 
  geom_tile()+xlim(143,233)+
  scale_fill_viridis(option="C",name="Temperature (C)")+
  scale_y_reverse()+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Depth (m)")+theme(axis.text=element_text(color="black",size=12),legend.position="none",axis.title=element_text(size=14))
# ggsave("B_deep_heatmap.png",width=6.5, height=3.3, units="in", dpi=300)

#pivot to wide format to work with lake analyzer and similar ... save as tab-delimited file
F_shallow_wide<-F_Shallow_date %>%
  select(datetime,temp_depth_m,temp_c) %>% 
  pivot_wider(names_from=temp_depth_m,values_from=temp_c) %>% 
  rename(wtr_0="0", wtr_0.75="0.75", wtr_1="1", wtr_1.25="1.25", wtr_1.5="1.5")
write.table(F_shallow_wide, file="temp_hf_Fshallow_wide.txt",row.names = FALSE,sep = "\t")

# Write one csv for temperature profiles from deep site, pond F
write.csv(F_Shallow_date,file="temp_hf_Fshallow.csv",row.names = FALSE)

### PART 1D - Pond B, Shallow Site --------------------------------------------------------------------------------------------------
# Read in individual csv's from each temperature logger (each represents a different depth in profile)
# Select columns you want to work with, and give them sensible names. Add columns for pond, site id, and water depth
B_Shallow_0<-read.csv("B_Shallow_0m.csv")
B_Shallow_0<-B_Shallow_0 %>% 
  rename(datetime=?..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="B", site_id=21, temp_depth_m=0) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

B_Shallow_0.75<-read.csv("B_Shallow_0.75m.csv")
B_Shallow_0.75<-B_Shallow_0.75 %>% 
  rename(datetime=?..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="B", site_id=21, temp_depth_m=0.75) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

B_Shallow_1.25<-read.csv("B_Shallow_1.25m.csv")
B_Shallow_1.25<-B_Shallow_1.25 %>% 
  rename(datetime=?..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="B", site_id=21, temp_depth_m=1.25) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

B_Shallow_1.5<-read.csv("B_Shallow_1.5m.csv")
B_Shallow_1.5<-B_Shallow_1.5 %>% 
  rename(datetime=?..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="B", site_id=21, temp_depth_m=1.5) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

B_Shallow<-rbind(B_Shallow_0,B_Shallow_0.75,B_Shallow_1.25,B_Shallow_1.5)
B_Shallow<-filter(B_Shallow,doy>"142"&doy<"241") #filter for data range of all sensors in lake

str(B_Shallow) #datetime became a character somehow, need to address that
# convert character data to date and time using lubridate (code from T. Butts)
B_Shallow_date = B_Shallow %>% 
  mutate(datetime = mdy_hm(datetime))
str(B_Shallow_date) #yay it's a date in ISO 8601 format!

# Quick visualizations to make sure everything looks alright - looking good!
ggplot(data=B_Shallow_date, aes(x=doy_frac, y=temp_c, group=temp_depth_m))+
  geom_line(aes(color=temp_depth_m),size=1)+
  scale_color_viridis_c(direction = -1)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Temperature (C)")+theme(axis.text=element_text(color="black",size=12),axis.title=element_text(size=14))+
  theme(legend.title=element_blank(), legend.position = c(0.8,0.1),legend.text = element_text(size=14))

ggplot(B_Shallow_date, aes(doy, temp_depth_m, fill= temp_c)) + 
  geom_tile()+xlim(143,233)+
  scale_fill_viridis(option="C",name="Temperature (C)")+
  scale_y_reverse()+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Depth (m)")+theme(axis.text=element_text(color="black",size=12),legend.position="none",axis.title=element_text(size=14))
# ggsave("B_deep_heatmap.png",width=6.5, height=3.3, units="in", dpi=300)

#pivot to wide format to work with lake analyzer and similar ... save as tab-delimited file
B_shallow_wide<-B_Shallow_date %>%
  select(datetime,temp_depth_m,temp_c) %>% 
  pivot_wider(names_from=temp_depth_m,values_from=temp_c) %>% 
  rename(wtr_0="0", wtr_0.75="0.75", wtr_1.25="1.25", wtr_1.5="1.5")
write.table(B_shallow_wide, file="temp_hf_Bshallow_wide.txt",row.names = FALSE,sep = "\t")

# Write one csv for temperature profiles from deep site, pond F
write.csv(B_Shallow_date,file="temp_hf_Bshallow.csv",row.names = FALSE)


### PART 1E - Pond F, Middle Site --------------------------------------------------------------------------------------------
# Read in individual csv's from each temperature logger (each represents a different depth in profile)
# Select columns you want to work with, and give them sensible names. Add columns for pond, site id, and water depth
F_Mid_0<-read.csv("F_Mid_0m.csv")
F_Mid_0<-F_Mid_0 %>% 
  rename(datetime=?..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=20, temp_depth_m=0) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_Mid_0.5<-read.csv("F_Mid_0.5m.csv")
F_Mid_0.5<-F_Mid_0.5 %>% 
  rename(datetime=?..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=20, temp_depth_m=0.5) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_Mid_0.75<-read.csv("F_Mid_0.75m.csv")
F_Mid_0.75<-F_Mid_0.75 %>% 
  rename(datetime=?..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=20, temp_depth_m=0.75) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_Mid_1<-read.csv("F_Mid_1m.csv")
F_Mid_1<-F_Mid_1 %>% 
  rename(datetime=?..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=20, temp_depth_m=1) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_Mid_1.25<-read.csv("F_Middle_1.25m.csv")
F_Mid_1.25<-F_Mid_1.25 %>% 
  rename(datetime=?..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=20, temp_depth_m=1.25) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_Mid_1.5<-read.csv("F_Middle_1.5m.csv")
F_Mid_1.5<-F_Mid_1.5 %>% 
  rename(datetime=?..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=20, temp_depth_m=1.5) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_Mid_2<-read.csv("F_Mid_2m.csv")
F_Mid_2<-F_Mid_2 %>% 
  rename(datetime=?..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=20, temp_depth_m=2) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_Mid<-rbind(F_Mid_0, F_Mid_0.5, F_Mid_0.75, F_Mid_1, F_Mid_1.25, F_Mid_1.5, F_Mid_2)
F_Mid<-filter(F_Mid,doy>"142"&doy<"241") #filter for data range of all sensors in lake

str(F_Mid) #datetime became a character somehow, need to address that
# convert character data to date and time using lubridate (code from T. Butts)
F_Mid_date = F_Mid %>% 
  mutate(datetime = mdy_hm(datetime))
View(F_Mid_date)
str(F_Mid_date) #yay it's a date in ISO 8601 format!

# Quick visualizations to make sure everything looks alright - looking good!
ggplot(data=F_Mid_date, aes(x=doy_frac, y=temp_c, group=temp_depth_m))+
  geom_line(aes(color=temp_depth_m),size=1)+
  scale_color_viridis_c(direction = -1)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Temperature (C)")+theme(axis.text=element_text(color="black",size=12),axis.title=element_text(size=14))+
  theme(legend.title=element_blank(), legend.position = c(0.8,0.1),legend.text = element_text(size=14))

ggplot(F_Mid_date, aes(doy, temp_depth_m, fill= temp_c)) + 
  geom_tile()+xlim(143,233)+
  scale_fill_viridis(option="C",name="Temperature (C)")+
  scale_y_reverse()+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Depth (m)")+theme(axis.text=element_text(color="black",size=12),legend.position="none",axis.title=element_text(size=14))
# ggsave("F_deep_heatmap.png",width=6.5, height=3.3, units="in", dpi=300)

#pivot to wide format to work with lake analyzer and similar ... save as tab-delimited file
F_Mid_wide<-F_Mid_date %>%
  select(datetime,temp_depth_m,temp_c) %>% 
  pivot_wider(names_from=temp_depth_m,values_from=temp_c) %>% 
  rename(wtr_0="0", wtr_0.5="0.5", wtr_0.75="0.75", wtr_1="1", wtr_1.25="1.25", wtr_1.5="1.5", wtr_2="2")
write.table(F_Mid_wide, file="temp_hf_Fmid_wide.txt",row.names = FALSE,sep = "\t")

# Write one csv for temperature profiles from deep site, pond F
write.csv(F_Mid_date,file="temp_hf_Fmid.csv",row.names = FALSE)

### PART 1F - Pond B, Middle Site ---------------------------------------------------------------------------------------------------
# Read in individual csv's from each temperature logger (each represents a different depth in profile)
# Select columns you want to work with, and give them sensible names. Add columns for pond, site id, and water depth
B_Mid_0<-read.csv("B_Mid_0m.csv")
B_Mid_0<-B_Mid_0 %>% 
  rename(datetime=?..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="B", site_id=20, temp_depth_m=0) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

B_Mid_0.25<-read.csv("B_Mid_0.25m.csv")
B_Mid_0.25<-B_Mid_0.25 %>% 
  rename(datetime=?..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="B", site_id=20, temp_depth_m=0.25) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

B_Mid_0.5<-read.csv("B_Mid_0.5m.csv")
B_Mid_0.5<-B_Mid_0.5 %>% 
  rename(datetime=?..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="B", site_id=20, temp_depth_m=0.5) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

B_Mid_0.75<-read.csv("B_Mid_0.75m.csv")
B_Mid_0.75<-B_Mid_0.75 %>% 
  rename(datetime=?..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="B", site_id=20, temp_depth_m=0.75) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

B_Mid_1.25<-read.csv("B_Middle_1.25m.csv")
B_Mid_1.25<-B_Mid_1.25 %>% 
  rename(datetime=?..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="B", site_id=20, temp_depth_m=1.25) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

B_Mid_1.5<-read.csv("B_Middle_1.5m.csv")
B_Mid_1.5<-B_Mid_1.5 %>% 
  rename(datetime=?..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="B", site_id=20, temp_depth_m=1.5) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

B_Mid_2<-read.csv("B_Mid_2m.csv")
B_Mid_2<-B_Mid_2 %>% 
  rename(datetime=?..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="B", site_id=20, temp_depth_m=2) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

B_mid<-rbind(B_Mid_0,B_Mid_0.25,B_Mid_0.5,B_Mid_0.75,B_Mid_1.25,B_Mid_1.5,B_Mid_2)
B_mid<-filter(B_mid,doy>"142"&doy<"241") #filter for data range of all sensors in lake

str(B_mid) #datetime became a character somehow, need to address that
# convert character data to date and time using lubridate (code from T. Butts)
B_mid_date = B_mid %>% 
  mutate(datetime = mdy_hm(datetime))
str(B_mid_date) #yay it's a date in ISO 8601 format!

# Quick visualizations to make sure everything looks alright - looking good!
ggplot(data=B_mid_date, aes(x=doy_frac, y=temp_c, group=temp_depth_m))+
  geom_line(aes(color=temp_depth_m),size=1)+
  scale_color_viridis_c(direction = -1)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Temperature (C)")+theme(axis.text=element_text(color="black",size=12),axis.title=element_text(size=14))+
  theme(legend.title=element_blank(), legend.position = c(0.8,0.1),legend.text = element_text(size=14))

ggplot(B_mid_date, aes(doy, temp_depth_m, fill= temp_c)) + 
  geom_tile()+xlim(143,233)+
  scale_fill_viridis(option="C",name="Temperature (C)")+
  scale_y_reverse()+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Depth (m)")+theme(axis.text=element_text(color="black",size=12),legend.position="none",axis.title=element_text(size=14))
# ggsave("B_deep_heatmap.png",width=6.5, height=3.3, units="in", dpi=300)

#pivot to wide format to work with lake analyzer and similar ... save as tab-delimited file
B_mid_wide<-B_mid_date %>%
  select(datetime,temp_depth_m,temp_c) %>% 
  pivot_wider(names_from=temp_depth_m,values_from=temp_c) %>% 
  rename(wtr_0="0", wtr_0.25="0.25", wtr_0.5="0.5", wtr_0.75="0.75", wtr_1.25="1.25", wtr_1.5="1.5", wtr_2="2")
write.table(B_mid_wide, file="temp_hf_Bmid_wide.txt",row.names = FALSE,sep = "\t")

# Write one csv for temperature profiles from deep site, pond B
write.csv(B_mid_date,file="temp_hf_Bmid.csv",row.names = FALSE)

### PART 1G - Bind all long-format tables together to produce one temperature chain data frame (keep wide format separate for LakeAnalyzer) ----------------------
tchain_dat<-rbind(F_deep_date, F_Mid_date, F_Shallow_date, B_deep_date, B_mid_date, B_Shallow_date)
write.csv(tchain_dat, file="temp_profiles_2020.csv", row.names=FALSE)


# PART 2 - High Frequency Temperature Profiles - interpolation for visualization  ------------------------------------------------------------------------------------
# Read in all high frequency temperature chain data
temp<-read.csv("temp_profiles_2020.csv", as.is=T)

summary(temp$temp_c[temp$pond=="F"])
summary(temp$temp_c[temp$pond=="B"])

tapply(temp$temp_c[temp$site_id=="19"], temp$pond[temp$site_id=="19"],median)
tapply(temp$temp_c[temp$site_id=="20"], temp$pond[temp$site_id=="20"],median)
tapply(temp$temp_c[temp$site_id=="21"], temp$pond[temp$site_id=="21"],median)

# Notes: same color ramp across sites/ponds
#        ponds B and F have essentially the same (within <1 C) mean and median temperature by pond and site level
#        so automatic continuous ramp will assign the same midpoint for each heat map
#        however, pond B has a lower minimum and maximum than F by ~1.5 C, so limits of ramps will differ in actual values of high and low colors
#        tried out custom breaks and ramps, but didn't like results
#        ended up going with scale_fill_gradientn and setting the limits to the range of the WHOLE temp data set

### Pond F, Deep Site --------------------------------------------------------------------------------------------------------------------------
# Select only Pond F, deep site to work with first (code from Nate) 
Fdeep<-subset(temp, pond=="F" & site_id=="19")
#doy_frac, temp_depth_m, temp_c

#Grab variables we need
Fdeeper<- Fdeep[c("doy_frac", "temp_depth_m", "temp_c")]

#Melt so that cast will recognize the data frame
Fmelt<- melt(Fdeeper, id=c("doy_frac", "temp_depth_m"))

#Now cast so that we have columns of temp for each depth
Fcast<- cast(Fmelt, doy_frac~temp_depth_m)

#create dataframe (definitely easier way to do this but it works)
Fcasttwo<-data.frame(Fcast)

#add whatever depths you want to (as the numeric value here)
Fcasttwo$X1.75<- 0

#calculate however you need to
Fcasttwo$X1.75<- rowMeans(Fcasttwo[c("X1.5","X2")],)

#now remelt to long format
Ffinal<- melt(Fcasttwo, id=c("doy_frac"))

#I lost your column names
#Grab just the numbers from this character
Ffinal$temp_depth_m <- substr(Ffinal$variable, 2, 9)

#And convert to numeric
Ffinal$temp_depth_m <- as.numeric(Ffinal$temp_depth_m)

#this is numeric too
Ffinal$temp_c<- as.numeric(Ffinal$value)

View(Ffinal)
write.csv(Ffinal, "temp_interpolated_Fdeep.csv",row.names = FALSE)

#F_deep<-

ggplot(Ffinal, aes(doy_frac, temp_depth_m, fill= temp_c)) + 
  geom_tile()+ylim(c(2.15,-0.15))+
  xlim(143,233)+
  scale_fill_gradientn(colors = viridis_pal(option="inferno")(9),name="Temperature (C)",
                       limits=c(14,38),)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Depth (m)")+theme(axis.text=element_text(color="black",size=12),legend.position="right",axis.title=element_text(size=14))

### Pond F, Middle Site -----------------------------------------
# Select only Pond F, middle site 
Fmiddle<-subset(temp, pond=="F" & site_id=="20")

#Grab variables we need
Fmid<- Fmiddle[c("doy_frac", "temp_depth_m", "temp_c")]

#Melt so that cast will recognize the data frame
Fmelt_mid<- melt(Fmid, id=c("doy_frac", "temp_depth_m"))

#Now cast so that we have columns of temp for each depth
Fcast_mid<- cast(Fmelt_mid, doy_frac~temp_depth_m)

#create dataframe (definitely easier way to do this but it works)
Fcast_mid1<-data.frame(Fcast_mid)

#add whatever depths you want to (as the numeric value here)
Fcast_mid1$X1.75<- 0
Fcast_mid1$X0.25<- 0

#calculate however you need to
Fcast_mid1$X1.75<- rowMeans(Fcast_mid1[c("X1.5","X2")],)
Fcast_mid1$X0.25<- rowMeans(Fcast_mid1[c("X0","X0.5")],)

#now remelt to long format
Ffinal_mid<- melt(Fcast_mid1, id=c("doy_frac"))

#I lost your column names
#Grab just the numbers from this character
Ffinal_mid$temp_depth_m <- substr(Ffinal_mid$variable, 2, 9)

#And convert to numeric
Ffinal_mid$temp_depth_m <- as.numeric(Ffinal_mid$temp_depth_m)
Ffinal_mid$temp_c<- as.numeric(Ffinal_mid$value)

View(Ffinal_mid)
write.csv(Ffinal_mid, "temp_interpolated_Fmiddle.csv",row.names = FALSE)


F_mid<-ggplot(Ffinal_mid, aes(doy_frac, temp_depth_m, fill= temp_c)) + 
  geom_tile()+xlim(143,233)+
  scale_fill_viridis(option="C",name="Temperature (C)")+
  ylim(c(2.15,-0.15))+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Depth (m)")+theme(axis.text=element_text(color="black",size=12),legend.position="none",axis.title=element_text(size=14))

### Pond F, Shallow Site -------------------------------------------------------------------------
# Select only Pond F, shallow site 
Fshal<-subset(temp, pond=="F" & site_id=="21")

#Grab variables we need
Fshallow<- Fshal[c("doy_frac", "temp_depth_m", "temp_c")]

#Melt so that cast will recognize the data frame
Fmelt_shallow<- melt(Fshallow, id=c("doy_frac", "temp_depth_m"))

#Now cast so that we have columns of temp for each depth
Fcast_shallow<- cast(Fmelt_shallow, doy_frac~temp_depth_m)

#create dataframe (definitely easier way to do this but it works)
Fcast_shallow2<-data.frame(Fcast_shallow)

#add whatever depths you want to (as the numeric value here)
Fcast_shallow2$X0.25<- 0
Fcast_shallow2$X0.5<- 0

#calculate however you need to
Fcast_shallow2$X0.25<-Fcast_shallow2$X0-((Fcast_shallow2$X0-Fcast_shallow2$X0.75)/3)
Fcast_shallow2$X0.5<-Fcast_shallow2$X0-(((Fcast_shallow2$X0-Fcast_shallow2$X0.75)/3)*2)

#now remelt to long format
Ffinal_shallow<- melt(Fcast_shallow2, id=c("doy_frac"))

#I lost your column names
#Grab just the numbers from this character
Ffinal_shallow$temp_depth_m <- substr(Ffinal_shallow$variable, 2, 9)

#And convert to numeric
Ffinal_shallow$temp_depth_m <- as.numeric(Ffinal_shallow$temp_depth_m)
Ffinal_shallow$temp_c<- as.numeric(Ffinal_shallow$value)


View(Ffinal_shallow)
write.csv(Ffinal_shallow, "temp_interpolated_Fshallow.csv",row.names = FALSE)


F_shallow<-ggplot(Ffinal_shallow, aes(doy_frac, temp_depth_m, fill= temp_c)) + 
  geom_tile()+xlim(143,233)+
  scale_fill_viridis(option="C",name="Temperature (C)")+
  ylim(c(2.15,-0.15))+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Depth (m)")+theme(axis.text=element_text(color="black",size=12),legend.position="none",axis.title=element_text(size=14))

grid.arrange(F_deep,F_mid,F_shallow,nrow=3)

### Pond B, Deep Site -----------------------------------------------------------------------
# Now let's do Pond B, deep site 
Bdeep<-subset(temp, pond=="B" & site_id=="19")
#doy_frac, temp_depth_m, temp_c

#Grab variables we need
Bdeeper<- Bdeep[c("doy_frac", "temp_depth_m", "temp_c")]

#Melt so that cast will recognize the data frame
Bmelt<- melt(Bdeeper, id=c("doy_frac", "temp_depth_m"))

#Now cast so that we have columns of temp for each depth
Bcast<- cast(Bmelt, doy_frac~temp_depth_m)

#create dataframe (definitely easier way to do this but it works)
Bcasttwo<-data.frame(Bcast)

#add whatever depths you want to (as the numeric value here)
Bcasttwo$X1.75<- 0
Bcasttwo$X1<- 0

#calculate however you need to
Bcasttwo$X1.75<- rowMeans(Bcasttwo[c("X1.5","X2")],)
Bcasttwo$X1<- rowMeans(Bcasttwo[c("X0.75","X1.25")],)


#now remelt to long format
Bfinal<- melt(Bcasttwo, id=c("doy_frac"))

#I lost your column names
#Grab just the numbers from this character
Bfinal$temp_depth_m <- substr(Bfinal$variable, 2, 9)

#And convert to numeric
Bfinal$temp_depth_m <- as.numeric(Bfinal$temp_depth_m)

#this is numeric too
Bfinal$temp_c<- as.numeric(Bfinal$value)

View(Bfinal)

B_deep<-ggplot(Bfinal, aes(doy_frac, temp_depth_m, fill= temp_c)) + 
  geom_tile()+xlim(143,233)+
  scale_fill_viridis(option="C",name="Temperature (C)")+
  ylim(c(2.15,-0.15))+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Depth (m)")+theme(axis.text=element_text(color="black",size=12),legend.position="none",axis.title=element_text(size=14))

### Pond B, Middle Site ----------------------------------------------------------------------
# Select only Pond B, middle site 
Bmiddle<-subset(temp, pond=="B" & site_id=="20")
#doy_frac, temp_depth_m, temp_c

#Grab variables we need
Bmid<- Bmiddle[c("doy_frac", "temp_depth_m", "temp_c")]

#Melt so that cast will recognize the data frame
Bmelt_mid<- melt(Bmid, id=c("doy_frac", "temp_depth_m"))

#Now cast so that we have columns of temp for each depth
Bcast_mid<- cast(Bmelt_mid, doy_frac~temp_depth_m)

#create dataframe (definitely easier way to do this but it works)
Bcast_mid2<-data.frame(Bcast_mid)

#add whatever depths you want to (as the numeric value here)
Bcast_mid2$X1.75<- 0
Bcast_mid2$X1<- 0

#calculate however you need to
Bcast_mid2$X1.75<- rowMeans(Bcast_mid2[c("X1.5","X2")],)
Bcast_mid2$X1<- rowMeans(Bcast_mid2[c("X0.75","X1.25")],)

#now remelt to long format
Bfinal_mid<- melt(Bcast_mid2, id=c("doy_frac"))

#I lost your column names
#Grab just the numbers from this character
Bfinal_mid$temp_depth_m <- substr(Bfinal_mid$variable, 2, 9)

#And convert to numeric
Bfinal_mid$temp_depth_m <- as.numeric(Bfinal_mid$temp_depth_m)
Bfinal_mid$temp_c<- as.numeric(Bfinal_mid$value)

View(Bfinal_mid)

B_mid<-ggplot(Bfinal_mid, aes(doy_frac, temp_depth_m, fill= temp_c)) + 
  geom_tile()+xlim(143,233)+
  scale_fill_viridis(option="C",name="Temperature (C)")+
  ylim(c(2.15,-0.15))+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Depth (m)")+theme(axis.text=element_text(color="black",size=12),legend.position="none",axis.title=element_text(size=14))

### Pond B, Shallow Site ---------------------------------------------------------------------
# Select only Pond B, shallow site 
Bshal<-subset(temp, pond=="B" & site_id=="21")

#Grab variables we need
Bshallow<- Bshal[c("doy_frac", "temp_depth_m", "temp_c")]

#Melt so that cast will recognize the data frame
Bmelt_shallow<- melt(Bshallow, id=c("doy_frac", "temp_depth_m"))

#Now cast so that we have columns of temp for each depth
Bcast_shallow<- cast(Bmelt_shallow, doy_frac~temp_depth_m)

#create dataframe (definitely easier way to do this but it works)
Bcast_shallow2<-data.frame(Bcast_shallow)

#add whatever depths you want to (as the numeric value here)
Bcast_shallow2$X0.25<- 0
Bcast_shallow2$X0.5<- 0
Bcast_shallow2$X1<- 0

#calculate however you need to
Bcast_shallow2$X0.25<-Bcast_shallow2$X0-((Bcast_shallow2$X0-Bcast_shallow2$X0.75)/3)
Bcast_shallow2$X0.5<-Bcast_shallow2$X0-(((Bcast_shallow2$X0-Bcast_shallow2$X0.75)/3)*2)
Bcast_shallow2$X1<-rowMeans(Bcast_shallow2[c("X0.75","X1.25")],)

#now remelt to long format
Bfinal_shallow<- melt(Bcast_shallow2, id=c("doy_frac"))

#I lost your column names
#Grab just the numbers from this character
Bfinal_shallow$temp_depth_m <- substr(Bfinal_shallow$variable, 2, 9)

#And convert to numeric
Bfinal_shallow$temp_depth_m <- as.numeric(Bfinal_shallow$temp_depth_m)
Bfinal_shallow$temp_c<- as.numeric(Bfinal_shallow$value)


View(Bfinal_shallow)

B_shallow<-ggplot(Bfinal_shallow, aes(doy_frac, temp_depth_m, fill= temp_c)) + 
  geom_tile()+xlim(143,233)+
  scale_fill_viridis(option="C",name="Temperature (C)")+
  ylim(c(2.15,-0.15))+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Depth (m)")+theme(axis.text=element_text(color="black",size=12),legend.position="none",axis.title=element_text(size=14))


grid.arrange(B_deep,B_mid,B_shallow,nrow=3)
write.csv(Bfinal, "temp_interpolated_Bdeep.csv",row.names = FALSE)
write.csv(Bfinal_mid, "temp_interpolated_Bmiddle.csv",row.names = FALSE)
write.csv(Bfinal_shallow, "temp_interpolated_Bshallow.csv",row.names = FALSE)


# PART 3 - Manual Sonde Profiles ------------------------------------------------------------------------------------
# Load in data files (one csv for each pond, DOY). Clean up variable names, add necessary variables (DOY, Pond ID), prepare to merge files

### DOY 150, Pond F--------------------------------------------------------------------------------------------
DOY150F<-read.csv("spatialdo_150_PondF.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY150F['temp_c']<-((DOY150F$Temp...F.)-32)/1.8
DOY150F['pond']<-'F'
DOY150F['doy']<-150

names(DOY150F) # The variable names from the sonde are a mess

# Rename and select columns of interest
F_150<-DOY150F %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..?S.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..?g.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL) %>% 
  filter(vertical_m>=0.01)

ggplot(data=F_150, aes(x=odo_sat, y=vertical_m, group=site_id))+
  geom_line()+ylim(2,0)+
  facet_wrap(~site_id)
#F_150, final clean

### DOY 150, Pond B--------------------------------------------------------------------------------------------
DOY150B<-read.csv("spatialdo_150_PondB.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY150B['temp_c']<-((DOY150B$Temp...F.)-32)/1.8
DOY150B['pond']<-'B'
DOY150B['doy']<-150

# Rename and select columns of interest
B_150<-DOY150B %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..?S.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..?g.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL) %>% 
  filter(vertical_m>=0.01)

B_150$odo_sat[B_150$site_id == "B11" & B_150$vertical_m > 1.5 |B_150$site_id == "B11" & B_150$odo_sat > 159] <- NA
B_150$odo_sat[B_150$site_id == "B2" & B_150$odo_sat < 142] <- NA
B_150$odo_sat[B_150$site_id == "B5" & B_150$odo_sat > 210 | B_150$site_id == "B5" & B_150$vertical_m > 1.7] <- NA
B_150$odo_sat[B_150$site_id == "B8" & B_150$odo_sat > 210] <- NA
B_150$odo_sat[B_150$site_id == "B1" & B_150$odo_sat > 149] <- NA


B150<-B_150 %>% drop_na(odo_sat)

ggplot(data=B150, aes(x=odo_sat, y=vertical_m, group=site_id))+
  geom_line()+ylim(2,0)+
  facet_wrap(~site_id)
#B150 final clean


### DOY 157, Pond F--------------------------------------------------------------------------------------------
DOY157F<-read.csv("spatialdo_157_PondF.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY157F['temp_c']<-((DOY157F$Temp...F.)-32)/1.8
DOY157F['pond']<-'F'
DOY157F['doy']<-157

# Rename and select columns of interest
F_157<-DOY157F %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..?S.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..?g.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL) %>% 
  filter(vertical_m>=0.01)

F_157$odo_sat[F_157$site_id == "F10" & F_157$vertical_m < 0.8 & F_157$odo_sat>190]<- NA
F_157$odo_sat[F_157$site_id == "F12" & F_157$vertical_m > 0.3 & F_157$odo_sat<180]<- NA
F_157$odo_sat[F_157$site_id == "F14" & F_157$odo_sat>178 | F_157$site_id == "F14" & F_157$vertical_m>1]<- NA
F_157$odo_sat[F_157$site_id == "F15" & F_157$odo_sat>166.5 | F_157$site_id == "F15" & F_157$vertical_m>1]<- NA
F_157$odo_sat[F_157$site_id == "F5" & F_157$vertical_m>1.45 | F_157$site_id == "F5" & F_157$odo_sat < 170] <- NA
F_157$odo_sat[F_157$site_id == "F6" & F_157$odo_sat>168]<- NA
F_157$odo_sat[F_157$site_id == "F7" & F_157$odo_sat>195]<- NA
F_157$odo_sat[F_157$site_id == "F8" & F_157$odo_sat>245 | F_157$site_id == "F8" & F_157$vertical_m>1.48]<- NA
F_157$odo_sat[F_157$site_id == "F8" & F_157$odo_sat<190 & F_157$vertical_m>0.5]<- NA

F157<-F_157 %>% drop_na(odo_sat)

ggplot(data=F157, aes(x=odo_sat, y=vertical_m, group=site_id))+
  geom_line()+ylim(2,0)+
  facet_wrap(~site_id)

# F157 is final

### DOY 157, Pond B--------------------------------------------------------------------------------------------
DOY157B<-read.csv("spatialdo_157_PondB.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY157B['temp_c']<-((DOY157B$Temp...F.)-32)/1.8
DOY157B['pond']<-'B'
DOY157B['doy']<-157

# Rename and select columns of interest
B_157<-DOY157B %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..?S.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..?g.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL) %>% 
  filter(vertical_m>=0.01)

B_157$odo_sat[B_157$site_id == "B10" & B_157$odo_sat<138.2]<- NA
B_157$odo_sat[B_157$site_id == "B11" & B_157$odo_sat>137.5]<- NA
B_157$odo_sat[B_157$site_id == "B15" & B_157$odo_sat>120]<- NA
B_157$odo_sat[B_157$site_id == "B16" & B_157$odo_sat>126]<- NA
B_157$odo_sat[B_157$site_id == "B4" & B_157$odo_sat>120]<- NA
B_157$odo_sat[B_157$site_id == "B5" & B_157$odo_sat>128.9]<- NA
B_157$odo_sat[B_157$site_id == "B8" & B_157$vertical_m>1.25 | B_157$site_id == "B8" & B_157$odo_sat>160]<- NA

B157<-B_157 %>% drop_na(odo_sat)

ggplot(data=B_157, aes(x=odo_sat, y=vertical_m, group=site_id))+
  geom_line()+ylim(2,0)+
  facet_wrap(~site_id)

#B157 is final

### DOY 166, Pond F--------------------------------------------------------------------------------------------
DOY166F<-read.csv("spatialdo_166_PondF.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY166F['temp_c']<-((DOY166F$Temp...F.)-32)/1.8
DOY166F['pond']<-'F'
DOY166F['doy']<-166

# Rename and select columns of interest
F_166<-DOY166F %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..?S.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..?g.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL) %>% 
  filter(vertical_m>=0.01)

F_166$odo_sat[F_166$site_id == "F11" & F_166$odo_sat<146 | F_166$site_id == "F11" & F_166$odo_sat>148]<- NA
F_166$odo_sat[F_166$site_id == "F12" & F_166$odo_sat>142]<- NA
F_166$odo_sat[F_166$site_id == "F14" & F_166$odo_sat>155]<- NA
F_166$odo_sat[F_166$site_id == "F2" & F_166$odo_sat>123]<- NA
F_166$odo_sat[F_166$site_id == "F5" & F_166$odo_sat>140.5]<- NA
F_166$odo_sat[F_166$site_id == "F7" & F_166$odo_sat>158]<- NA
F_166$odo_sat[F_166$site_id == "F8" & F_166$vertical_m>1.1]<- NA
F_166$odo_sat[F_166$site_id == "F9" & F_166$odo_sat>143]<- NA

F166<-F_166 %>% drop_na(odo_sat)

ggplot(data=F166, aes(x=odo_sat, y=vertical_m, group=site_id))+
  geom_line()+ylim(2,0)+
  facet_wrap(~site_id)

#F166 is final

### DOY 166, Pond B--------------------------------------------------------------------------------------------
DOY166B<-read.csv("spatialdo_166_PondB.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY166B['temp_c']<-((DOY166B$Temp...F.)-32)/1.8
DOY166B['pond']<-'B'
DOY166B['doy']<-166

# Rename and select columns of interest
B_166<-DOY166B %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..?S.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..?g.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL) %>% 
  filter(vertical_m>=0.01)

B_166$odo_sat[B_166$site_id == "B1" & B_166$odo_sat<129.5]<- NA
B_166$odo_sat[B_166$site_id == "B11" & B_166$vertical_m>1.4]<- NA
B_166$odo_sat[B_166$site_id == "B14" & B_166$odo_sat>167]<- NA
B_166$odo_sat[B_166$site_id == "B17" & B_166$odo_sat<143.4]<- NA
B_166$odo_sat[B_166$site_id == "B5" & B_166$odo_sat>123]<- NA
B_166$odo_sat[B_166$site_id == "B8" & B_166$odo_sat>163]<- NA

B166<-B_166 %>% drop_na(odo_sat)

ggplot(data=B166, aes(x=odo_sat, y=vertical_m, group=site_id))+
  geom_line()+ylim(2,0)+
  facet_wrap(~site_id)

# B166 is final

### DOY 171, Pond F--------------------------------------------------------------------------------------------
DOY171F<-read.csv("spatialdo_171_PondF.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY171F['temp_c']<-((DOY171F$Temp...F.)-32)/1.8
DOY171F['pond']<-'F'
DOY171F['doy']<-171

# Rename and select columns of interest
F_171<-DOY171F %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..?S.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..?g.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL) %>% 
  filter(vertical_m>=0.01)

F_171$odo_sat[F_171$site_id == "F10" & F_171$odo_sat>133.75] <- NA
F_171$odo_sat[F_171$site_id == "F11" & F_171$odo_sat>128.3] <- NA
F_171$odo_sat[F_171$site_id == "F13" & F_171$odo_sat<123.5] <- NA
F_171$odo_sat[F_171$site_id == "F14" & F_171$odo_sat>126] <- NA
F_171$odo_sat[F_171$site_id == "F2" & F_171$odo_sat>104.5] <- NA
F_171$odo_sat[F_171$site_id == "F5" & F_171$odo_sat>131] <- NA
F_171$odo_sat[F_171$site_id == "F6" & F_171$odo_sat>108] <- NA
F_171$odo_sat[F_171$site_id == "F7" & F_171$odo_sat>126] <- NA
F_171$odo_sat[F_171$site_id == "F8" & F_171$odo_sat>139] <- NA

F171<-F_171 %>% drop_na(odo_sat)

ggplot(data=F171, aes(x=odo_sat, y=vertical_m, group=site_id))+
  geom_line()+ylim(2,0)+
  facet_wrap(~site_id)
#F171 final

### DOY 171, Pond B--------------------------------------------------------------------------------------------
DOY171B<-read.csv("spatialdo_171_PondB.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY171B['temp_c']<-((DOY171B$Temp...F.)-32)/1.8
DOY171B['pond']<-'B'
DOY171B['doy']<-171

# Rename and select columns of interest
B_171<-DOY171B %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..?S.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..?g.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL) %>% 
  filter(vertical_m>=0.01)

B_171$odo_sat[B_171$site_id == "B11" & B_171$odo_sat>124] <- NA
B_171$odo_sat[B_171$site_id == "B5" & B_171$odo_sat>109.8] <- NA

B171<-B_171 %>% drop_na(odo_sat)

ggplot(data=B171, aes(x=odo_sat, y=vertical_m, group=site_id))+
  geom_line()+ylim(2,0)+
  facet_wrap(~site_id)
#B171 final


### DOY 178, Pond F--------------------------------------------------------------------------------------------
DOY178F<-read.csv("spatialdo_178_PondF.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY178F['temp_c']<-((DOY178F$Temp...F.)-32)/1.8
DOY178F['pond']<-'F'
DOY178F['doy']<-178

# Rename and select columns of interest
F_178<-DOY178F %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..?S.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..?g.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL) %>% 
  filter(vertical_m>=0.01)

F_178$odo_sat[F_178$site_id == "F1" & F_178$odo_sat>103.3] <- NA
F_178$odo_sat[F_178$site_id == "F10" & F_178$odo_sat>125.5] <- NA
F_178$odo_sat[F_178$site_id == "F11" & F_178$vertical_m>1.2] <- NA
F_178$odo_sat[F_178$site_id == "F12" & F_178$odo_sat>132.8] <- NA
F_178$odo_sat[F_178$site_id == "F13" & F_178$odo_sat>133] <- NA
F_178$odo_sat[F_178$site_id == "F14" & F_178$odo_sat>149] <- NA
F_178$odo_sat[F_178$site_id == "F15" & F_178$odo_sat<124 | F_178$site_id == "F15" & F_178$odo_sat<127] <- NA
F_178$odo_sat[F_178$site_id == "F16" & F_178$odo_sat>112.5] <- NA
F_178$odo_sat[F_178$site_id == "F17" & F_178$odo_sat>114] <- NA
F_178$odo_sat[F_178$site_id == "F2" & F_178$odo_sat>104] <- NA
F_178$odo_sat[F_178$site_id == "F3" & F_178$odo_sat>100] <- NA
F_178$odo_sat[F_178$site_id == "F4" & F_178$odo_sat>111] <- NA
F_178$odo_sat[F_178$site_id == "F6" & F_178$odo_sat>109.5] <- NA
F_178$odo_sat[F_178$site_id == "F9" & F_178$odo_sat>120] <- NA

F178<-F_178 %>% drop_na(odo_sat)


ggplot(data=F178, aes(x=odo_sat, y=vertical_m, group=site_id))+
  geom_line()+ylim(2,0)+
  facet_wrap(~site_id)


### DOY 178, Pond B--------------------------------------------------------------------------------------------
DOY178B<-read.csv("spatialdo_178_PondB.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY178B['temp_c']<-((DOY178B$Temp...F.)-32)/1.8
DOY178B['pond']<-'B'
DOY178B['doy']<-178

# Rename and select columns of interest
B_178<-DOY178B %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..?S.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..?g.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL) %>% 
  filter(vertical_m>=0.01)

B_178$odo_sat[B_178$site_id == "B11" & B_178$odo_sat>110] <- NA
B_178$odo_sat[B_178$site_id == "B15" & B_178$odo_sat>117] <- NA
B_178$odo_sat[B_178$site_id == "B17" & B_178$odo_sat>109.5] <- NA
B_178$odo_sat[B_178$site_id == "B8" & B_178$odo_sat>106] <- NA
B_178$odo_mgL[B_178$site_id == "B11" & B_178$vertical_m>1] <- NA
B_178$odo_mgL[B_178$site_id == "B8" & B_178$odo_mgL>8.6] <- NA

B178<-B_178 %>% drop_na(odo_sat, odo_mgL)

ggplot(data=B_178, aes(x=odo_sat, y=vertical_m, group=site_id))+
  geom_line()+ylim(2,0)+
  facet_wrap(~site_id)

### DOY 186, Pond F--------------------------------------------------------------------------------------------
DOY186F<-read.csv("spatialdo_186_PondF.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY186F['temp_c']<-((DOY186F$Temp...F.)-32)/1.8
DOY186F['pond']<-'F'
DOY186F['doy']<-186

# Rename and select columns of interest
F_186<-DOY186F %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..?S.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..?g.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL) %>% 
  filter(vertical_m>=0.01)

F_186$odo_sat[F_186$site_id == "F10" & F_186$odo_sat>124.5] <- NA
F_186$odo_sat[F_186$site_id == "F11" & F_186$odo_sat>126] <- NA
F_186$odo_sat[F_186$site_id == "F12" & F_186$odo_sat>143] <- NA
F_186$odo_sat[F_186$site_id == "F14" & F_186$vertical_m>1.2] <- NA
F_186$odo_sat[F_186$site_id == "F14" & F_186$odo_sat>140] <- NA
F_186$odo_sat[F_186$site_id == "F2" & F_186$odo_sat>130] <- NA
F_186$odo_sat[F_186$site_id == "F3" & F_186$odo_sat>117.8] <- NA
F_186$odo_sat[F_186$site_id == "F4" & F_186$odo_sat>135.3] <- NA
F_186$odo_sat[F_186$site_id == "F5" & F_186$odo_sat<130.8] <- NA
F_186$odo_sat[F_186$site_id == "F6" & F_186$odo_sat>131] <- NA
F_186$odo_sat[F_186$site_id == "F7" & F_186$odo_sat>143] <- NA
F_186$odo_sat[F_186$site_id == "F8" & F_186$vertical_m>1.1] <- NA
F_186$odo_sat[F_186$site_id == "F9" & F_186$odo_sat>125] <- NA

F186<-F_186 %>% drop_na(odo_sat)

ggplot(data=F_186, aes(x=odo_sat, y=vertical_m, group=site_id))+
  geom_line()+ylim(2,0)+
  facet_wrap(~site_id)

#F186 final

### DOY 186, Pond B--------------------------------------------------------------------------------------------
DOY186B<-read.csv("spatialdo_186_PondB.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY186B['temp_c']<-((DOY186B$Temp...F.)-32)/1.8
DOY186B['pond']<-'B'
DOY186B['doy']<-186

# Rename and select columns of interest
B_186<-DOY186B %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..?S.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..?g.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL) %>% 
  filter(vertical_m>=0.01)

B_186$odo_sat[B_186$site_id == "B10" & B_186$odo_sat>102.5] <- NA
B_186$odo_sat[B_186$site_id == "B12" & B_186$vertical_m>0.65 |B_186$site_id == "B12" & B_186$odo_sat>98.5] <- NA
B_186$odo_sat[B_186$site_id == "B14" & B_186$odo_sat>110 |B_186$site_id == "B14" & B_186$odo_sat<107.5] <- NA
B_186$odo_sat[B_186$site_id == "B15" & B_186$odo_sat>105 & B_186$vertical_m<0.2] <- NA
B_186$odo_sat[B_186$site_id == "B15" & B_186$odo_sat<104.5] <-NA
B_186$odo_sat[B_186$site_id == "B16" & B_186$odo_sat>105 & B_186$vertical_m<0.2] <- NA
B_186$odo_sat[B_186$site_id == "B17" & B_186$odo_sat<106 & B_186$vertical_m<0.3] <- NA
B_186$odo_sat[B_186$site_id == "B17" & B_186$odo_sat>106.3] <-NA
B_186$odo_sat[B_186$site_id == "B18" & B_186$odo_sat>107.7] <-NA
B_186$odo_sat[B_186$site_id == "B5" & B_186$odo_sat<76.8] <-NA

B186<-B_186 %>% drop_na(odo_sat)

ggplot(data=B_186, aes(x=odo_sat, y=vertical_m, group=site_id))+
  geom_line()+ylim(2,0)+
  facet_wrap(~site_id)



### DOY 192, Pond F--------------------------------------------------------------------------------------------
DOY192F<-read.csv("spatialdo_192_PondF.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY192F['temp_c']<-((DOY192F$Temp...F.)-32)/1.8
DOY192F['pond']<-'F'
DOY192F['doy']<-192

# Rename and select columns of interest
F_192<-DOY192F %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..?S.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..?g.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL) %>% 
  filter(vertical_m>=0.01)


ggplot(data=F_192, aes(x=odo_sat, y=vertical_m, group=site_id))+
  geom_line()+ylim(2,0)+
  facet_wrap(~site_id)

# final is F_192

### DOY 192, Pond B--------------------------------------------------------------------------------------------
DOY192B<-read.csv("spatialdo_192_PondB.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY192B['temp_c']<-((DOY192B$Temp...F.)-32)/1.8
DOY192B['pond']<-'B'
DOY192B['doy']<-192

# Rename and select columns of interest
B_192<-DOY192B %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..?S.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..?g.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL) %>% 
  filter(vertical_m>=0.01)

B_192$odo_sat[B_192$site_id == "B12" & B_192$odo_sat<75 & B_192$vertical_m<0.25] <-NA
B_192$odo_sat[B_192$site_id == "B14" & B_192$odo_sat<78.7] <-NA
B_192$odo_sat[B_192$site_id == "B4" & B_192$odo_sat>78.8 | B_192$site_id == "B4" & B_192$odo_sat>79 & B_192$vertical_m<0.2] <-NA

B192<-B_192 %>% drop_na(odo_sat)

ggplot(data=B192, aes(x=odo_sat, y=vertical_m, group=site_id))+
  geom_line()+ylim(2,0)+
  facet_wrap(~site_id)

# final is B192

### DOY 199, Pond F--------------------------------------------------------------------------------------------
DOY199F<-read.csv("spatialdo_199_PondF.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY199F['temp_c']<-((DOY199F$Temp...F.)-32)/1.8
DOY199F['pond']<-'F'
DOY199F['doy']<-199

# Rename and select columns of interest
F_199<-DOY199F %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..?S.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..?g.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL) %>% 
  filter(vertical_m>=0.01)

F_199$odo_sat[F_199$site_id == "F10" & F_199$odo_sat>92.5] <- NA
F_199$odo_sat[F_199$site_id == "F10" & F_199$odo_sat<88 & F_199$vertical_m<0.25] <- NA
F_199$odo_sat[F_199$site_id == "F10" & F_199$odo_sat<89 & F_199$vertical_m<0.15] <- NA
F_199$odo_sat[F_199$site_id == "F13" & F_199$odo_sat<=92.5 & F_199$vertical_m<=0.25] <- NA
F_199$odo_sat[F_199$site_id == "F13" & F_199$odo_sat<=93.2 & F_199$vertical_m<=0.15] <- NA
F_199$odo_sat[F_199$site_id == "F14" & F_199$odo_sat>92.6] <- NA
F_199$odo_sat[F_199$site_id == "F15" & F_199$odo_sat<91 |F_199$site_id == "F15" & F_199$odo_sat>91.95] <- NA
F_199$odo_sat[F_199$site_id == "F18" & F_199$odo_sat<93.7] <- NA
F_199$odo_sat[F_199$site_id == "F6" & F_199$odo_sat<89.5 & F_199$vertical_m<=0.22] <- NA
F_199$odo_sat[F_199$site_id == "F7" & F_199$odo_sat<82] <- NA
F_199$odo_sat[F_199$site_id == "F8" & F_199$odo_sat<84] <- NA
F_199$odo_sat[F_199$site_id == "F9" & F_199$odo_sat<82.5] <- NA

F199<-F_199 %>% drop_na(odo_sat)

ggplot(data=F_199, aes(x=odo_sat, y=vertical_m, group=site_id))+
  geom_line()+ylim(2,0)+
  facet_wrap(~site_id)

# F199 is final

### DOY 199, Pond B--------------------------------------------------------------------------------------------
DOY199B<-read.csv("spatialdo_199_PondB.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY199B['temp_c']<-((DOY199B$Temp...F.)-32)/1.8
DOY199B['pond']<-'B'
DOY199B['doy']<-199

# Rename and select columns of interest
B_199<-DOY199B %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..?S.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..?g.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL) %>% 
  filter(vertical_m>=0.01)

B_199$odo_sat[B_199$site_id == "B10" & B_199$odo_sat<92.9] <- NA
B_199$odo_sat[B_199$site_id == "B11" & B_199$odo_sat<93 & B_199$vertical_m<0.25] <- NA
B_199$odo_sat[B_199$site_id == "B12" & B_199$odo_sat<88] <- NA
B_199$odo_sat[B_199$site_id == "B12" & B_199$odo_sat<90.05 & B_199$vertical_m<0.52] <- NA
B_199$odo_sat[B_199$site_id == "B13" & B_199$odo_sat>94.5] <- NA
B_199$odo_sat[B_199$site_id == "B14" & B_199$odo_sat<93] <- NA
B_199$odo_sat[B_199$site_id == "B14" & B_199$odo_sat<93.5 & B_199$vertical_m<0.4] <- NA
B_199$odo_sat[B_199$site_id == "B15" & B_199$odo_sat>93.7] <- NA
B_199$odo_sat[B_199$site_id == "B16" & B_199$odo_sat>94.7] <- NA
B_199$odo_sat[B_199$site_id == "B17" & B_199$odo_sat<94.4 & B_199$vertical_m<0.28] <- NA
B_199$odo_sat[B_199$site_id == "B17" & B_199$odo_sat>95.2] <- NA
B_199$odo_sat[B_199$site_id == "B18" & B_199$odo_sat>94] <- NA
B_199$odo_sat[B_199$site_id == "B18" & B_199$odo_sat>93.9 |B_199$site_id == "B18" & B_199$odo_sat<93.7] <- NA
B_199$odo_sat[B_199$site_id == "B2" & B_199$odo_sat>90.1 |B_199$site_id == "B2" & B_199$odo_sat<85.08] <- NA
B_199$odo_sat[B_199$site_id == "B4" & B_199$odo_sat>91.25] <- NA
B_199$odo_sat[B_199$site_id == "B5" & B_199$odo_sat<91.5 & B_199$vertical_m<0.25] <- NA
B_199$odo_sat[B_199$site_id == "B6" & B_199$odo_sat>88.45] <- NA
B_199$odo_sat[B_199$site_id == "B7" & B_199$odo_sat<92 & B_199$vertical_m<0.2] <- NA
B_199$odo_sat[B_199$site_id == "B8" & B_199$odo_sat<90.5 & B_199$vertical_m<0.2] <- NA
B_199$odo_sat[B_199$site_id == "B8" & B_199$odo_sat<90.5 & B_199$vertical_m<0.3] <- NA

B199<-B_199 %>% drop_na(odo_sat)

ggplot(data=B199, aes(x=temp_c, y=vertical_m, group=site_id))+
  geom_line()+ylim(2,0)+
  facet_wrap(~site_id)

#B199 is final

### DOY 206, Pond F--------------------------------------------------------------------------------------------
DOY206F<-read.csv("spatialdo_206_PondF.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY206F['temp_c']<-((DOY206F$Temp...F.)-32)/1.8
DOY206F['pond']<-'F'
DOY206F['doy']<-206

# Rename and select columns of interest
F_206<-DOY206F %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..?S.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..?g.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL) %>% 
  filter(vertical_m>=0.01)

F_206$odo_sat[F_206$site_id == "F18" & F_206$odo_sat<86.3] <- NA
F_206$odo_sat[F_206$site_id == "F2" & F_206$odo_sat<78.4] <- NA
F_206$odo_sat[F_206$site_id == "F5" & F_206$odo_sat<77.5 & F_206$vertical_m<1] <- NA

F206<-F_206 %>% drop_na(odo_sat)

ggplot(data=F206, aes(x=odo_sat, y=vertical_m, group=site_id))+
  geom_line()+ylim(2,0)+
  facet_wrap(~site_id)
# F206 is final

### DOY 206, Pond B--------------------------------------------------------------------------------------------
DOY206B<-read.csv("spatialdo_206_PondB.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY206B['temp_c']<-((DOY206B$Temp...F.)-32)/1.8
DOY206B['pond']<-'B'
DOY206B['doy']<-206

# Rename and select columns of interest
B_206<-DOY206B %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..?S.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..?g.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL) %>% 
  filter(vertical_m>=0.01)

B_206$odo_sat[B_206$site_id == "B10" & B_206$odo_sat<=77.5 & B_206$vertical_m<0.26] <- NA
B_206$odo_sat[B_206$site_id == "B12" & B_206$odo_sat<76] <- NA
B_206$odo_sat[B_206$site_id == "B14" & B_206$odo_sat<78.8] <- NA
B_206$odo_sat[B_206$site_id == "B15" & B_206$odo_sat<79.85] <- NA
B_206$odo_sat[B_206$site_id == "B18" & B_206$odo_sat<=81.5 & B_206$vertical_m<0.2] <- NA
B_206$odo_sat[B_206$site_id == "B7" & B_206$odo_sat<76.5 & B_206$vertical_m<0.2] <- NA


B206<-B_206 %>% drop_na(odo_sat)


ggplot(data=B206, aes(x=odo_sat, y=vertical_m, group=site_id))+
  geom_line()+ylim(2,0)+
  facet_wrap(~site_id)

#B206 is final


### DOY 213, Pond F--------------------------------------------------------------------------------------------
DOY213F<-read.csv("spatialdo_213_PondF.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY213F['temp_c']<-((DOY213F$Temp...F.)-32)/1.8
DOY213F['pond']<-'F'
DOY213F['doy']<-213

# Rename and select columns of interest
F_213<-DOY213F %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..?S.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..?g.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL) %>% 
  filter(vertical_m>=0.01)

F_213$odo_sat[F_213$site_id == "F12" & F_213$odo_sat>86] <- NA
F_213$odo_sat[F_213$site_id == "F13" & F_213$odo_sat<88.5 & F_213$vertical_m<0.34] <- NA
F_213$odo_sat[F_213$site_id == "F16" & F_213$odo_sat>91.5] <- NA

F213<-F_213 %>% drop_na(odo_sat)


ggplot(data=F_213, aes(x=odo_sat, y=vertical_m, group=site_id))+
  geom_line()+ylim(2,0)+
  facet_wrap(~site_id)

#F213 is final

### DOY 213, Pond B--------------------------------------------------------------------------------------------
DOY213B<-read.csv("spatialdo_213_PondB.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY213B['temp_c']<-((DOY213B$Temp...F.)-32)/1.8
DOY213B['pond']<-'B'
DOY213B['doy']<-213

# Rename and select columns of interest
B_213<-DOY213B %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..?S.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..?g.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL) %>% 
  filter(vertical_m>=0.01)

B_213$odo_sat[B_213$site_id == "B1" & B_213$odo_sat<92.5] <- NA
B_213$odo_sat[B_213$site_id == "B13" & B_213$odo_sat<87.8] <- NA
B_213$odo_sat[B_213$site_id == "B5" & B_213$odo_sat<90.5 & B_213$vertical_m<0.3] <- NA


B213<-B_213 %>% drop_na(odo_sat)

ggplot(data=B_213, aes(x=odo_sat, y=vertical_m, group=site_id))+
  geom_line()+ylim(2,0)+
  facet_wrap(~site_id)

#B213 is final

### DOY 220, Pond F--------------------------------------------------------------------------------------------
DOY220F<-read.csv("spatialdo_220_PondF.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY220F['temp_c']<-((DOY220F$Temp...F.)-32)/1.8
DOY220F['pond']<-'F'
DOY220F['doy']<-220

# Rename and select columns of interest
F_220<-DOY220F %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..?S.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..?g.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL) %>% 
  filter(vertical_m>=0.01)

ggplot(data=F_220, aes(x=odo_sat, y=vertical_m, group=site_id))+
  geom_line()+ylim(2,0)+
  facet_wrap(~site_id)

### DOY 220, Pond B--------------------------------------------------------------------------------------------
DOY220B<-read.csv("spatialdo_220_PondB.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY220B['temp_c']<-((DOY220B$Temp...F.)-32)/1.8
DOY220B['pond']<-'B'
DOY220B['doy']<-220

# Rename and select columns of interest
B_220<-DOY220B %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..?S.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..?g.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL) %>% 
  filter(vertical_m>=0.01)

B_220$odo_sat[B_220$site_id == "B15" & B_220$odo_sat<89] <- NA
B_220$odo_sat[B_220$site_id == "B15" & B_220$odo_sat<89.8 & B_220$vertical_m<0.38] <- NA
B_220$odo_sat[B_220$site_id == "B12" & B_220$odo_sat<87.5] <- NA
B_220$odo_sat[B_220$site_id == "B12" & B_220$odo_sat<=88 & B_220$vertical_m<0.4] <- NA
B_220$odo_sat[B_220$site_id == "B13" & B_220$odo_sat<89.5] <- NA
B_220$odo_sat[B_220$site_id == "B13" & B_220$odo_sat<89.8 & B_220$vertical_m<0.25] <- NA
B_220$odo_sat[B_220$site_id == "B16" & B_220$odo_sat<92 & B_220$vertical_m<0.2] <- NA
B_220$odo_sat[B_220$site_id == "B18" & B_220$odo_sat>90.8] <- NA
B_220$odo_sat[B_220$site_id == "B4" & B_220$odo_sat<85 | B_220$site_id=="B4" & B_220$odo_sat>85.4] <- NA
B_220$odo_sat[B_220$site_id == "B5" & B_220$odo_sat<84] <- NA

B220<-B_220 %>% drop_na(odo_sat)

ggplot(data=B220, aes(x=odo_sat, y=vertical_m, group=site_id))+
  geom_line()+ylim(2,0)+
  facet_wrap(~site_id)
#B220 is final

### DOY 228, Pond F--------------------------------------------------------------------------------------------
DOY228F<-read.csv("spatialdo_228_PondF.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY228F['temp_c']<-((DOY228F$Temp...F.)-32)/1.8
DOY228F['pond']<-'F'
DOY228F['doy']<-228

# Rename and select columns of interest
F_228<-DOY228F %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..?S.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..?g.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL) %>% 
  filter(vertical_m>=0.01)

F_228$odo_sat[F_228$site_id == "F16" & F_228$odo_sat>92.5 | F_228$site_id == "F16" & F_228$odo_sat<91.7] <- NA
F_228$odo_sat[F_228$site_id == "F18" & F_228$odo_sat<92.5 | F_228$site_id == "F18" & F_228$odo_sat>92.7] <- NA
F_228$odo_sat[F_228$site_id == "F5" & F_228$odo_sat<90] <- NA

F228<-F_228 %>% drop_na(odo_sat)

ggplot(data=F228, aes(x=odo_sat, y=vertical_m, group=site_id))+
  geom_line()+ylim(2,0)+
  facet_wrap(~site_id)
#F228 is final

### DOY 228, Pond B--------------------------------------------------------------------------------------------
DOY228B<-read.csv("spatialdo_228_PondB.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY228B['temp_c']<-((DOY228B$Temp...F.)-32)/1.8
DOY228B['pond']<-'B'
DOY228B['doy']<-228

# Rename and select columns of interest
B_228<-DOY228B %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..?S.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..?g.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL) %>% 
  filter(vertical_m>=0.01)

B_228$odo_sat[B_228$site_id == "B1" & B_228$odo_sat<95.5 & B_228$vertical_m<0.25] <- NA
B_228$odo_sat[B_228$site_id == "B1" & B_228$odo_sat<94.5 & B_228$vertical_m<0.27] <- NA
B_228$odo_sat[B_228$site_id == "B10" & B_228$odo_sat<92 & B_228$vertical_m<0.32] <- NA
B_228$odo_sat[B_228$site_id == "B11" & B_228$odo_sat<91.5 & B_228$vertical_m<0.32] <- NA
B_228$odo_sat[B_228$site_id == "B12" & B_228$odo_sat>91.9] <- NA
B_228$odo_sat[B_228$site_id == "B12" & B_228$odo_sat<91.6 & B_228$vertical_m<0.35] <- NA
B_228$odo_sat[B_228$site_id == "B13" & B_228$odo_sat>93] <- NA
B_228$odo_sat[B_228$site_id == "B14" & B_228$odo_sat<93.1 & B_228$vertical_m<0.45] <- NA
B_228$odo_sat[B_228$site_id == "B15" & B_228$odo_sat<91.3 | B_228$site_id == "B15" & B_228$odo_sat>91.4] <- NA
B_228$odo_sat[B_228$site_id == "B18" & B_228$odo_sat>93.75] <- NA
B_228$odo_sat[B_228$site_id == "B2" & B_228$odo_sat>97.1] <- NA
B_228$odo_sat[B_228$site_id == "B3" & B_228$odo_sat<95 | B_228$site_id == "B3" & B_228$odo_sat>96] <- NA
B_228$odo_sat[B_228$site_id == "B4" & B_228$odo_sat<90.8 & B_228$vertical_m<0.32] <- NA
B_228$odo_sat[B_228$site_id == "B5" & B_228$odo_sat<93.2 & B_228$vertical_m<0.32 |B_228$site_id == "B5" & B_228$odo_sat<90] <- NA
B_228$odo_sat[B_228$site_id == "B6" & B_228$odo_sat>94.7] <- NA
B_228$odo_sat[B_228$site_id == "B7" & B_228$odo_sat>93.3] <- NA
B_228$odo_sat[B_228$site_id == "B9" & B_228$odo_sat>90.5] <- NA

B228<-B_228 %>% drop_na(odo_sat)

ggplot(data=B228, aes(x=odo_sat, y=vertical_m, group=site_id))+
  geom_line()+ylim(2,0)+
  facet_wrap(~site_id)
#B228 is final

### DOY 234, Pond F--------------------------------------------------------------------------------------------
DOY234F<-read.csv("spatialdo_234_PondF.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY234F['temp_c']<-((DOY234F$Temp...F.)-32)/1.8
DOY234F['pond']<-'F'
DOY234F['doy']<-234

# Rename and select columns of interest
F_234<-DOY234F %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..?S.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..?g.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL) %>% 
  filter(vertical_m>=0.01)

F234<-F_234 %>% drop_na(odo_sat)

ggplot(data=F234, aes(x=odo_sat, y=vertical_m, group=site_id))+
  geom_line()+ylim(2,0)+
  facet_wrap(~site_id)

### DOY 234, Pond B--------------------------------------------------------------------------------------------
DOY234B<-read.csv("spatialdo_234_PondB.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY234B['temp_c']<-((DOY234B$Temp...F.)-32)/1.8
DOY234B['pond']<-'B'
DOY234B['doy']<-234

# Rename and select columns of interest
B_234<-DOY234B %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..?S.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..?g.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL) %>% 
  filter(vertical_m>=0.01)

B_234$odo_sat[B_234$site_id == "B2" & B_234$odo_sat<76.25 & B_234$vertical_m<0.35] <- NA
B_234$odo_sat[B_234$site_id == "B2" & B_234$odo_sat>77.5] <- NA

B234<-B_234 %>% drop_na(odo_sat)

ggplot(data=B_234, aes(x=odo_sat, y=vertical_m, group=site_id))+
  geom_line()+ylim(2,0)+
  facet_wrap(~site_id)
#B234 is final

# Join all spatial profile data frames together
spatial_DO<-rbind(B150, B157, B166, B171, B178, B186, B192, B199, B206, B213, B220, B228, B234,
                  F_150, F157, F166, F171, F178, F186, F_192, F199, F206, F213, F_220, F228, F234)


write.csv(spatial_DO, file="DO_profiles_2020.csv", row.names=FALSE)
