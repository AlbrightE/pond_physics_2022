# Project: "Macrophyte-hydrodynamic interactions mediate stratification and dissolved oxygen dynamics in ponds"
#           Part of ISU Hort Farm Pond Study, 2020
# Last Modified 22 September 2022
# Contributors: Ellen Albright, Robert Ladwig, Grace Wilkinson
# Description: "final" analyses and figures for manuscript submission, L&O

# Data citation: ______________

# NOTE: Pond ID B = Nutrient addition pond (warm colors, "pondB_col")
#       Pond ID F = Reference pond (cool colors, "pondF_col")

# The code can be run with the following data sets (in order first used in script)
#    1. "hort20_surface_dat.csv" - YSI multi-parameter sonde profiles - surface (0-30 cm) mean, daily mean
#    2. "temp_profiles_2020.csv" - High frequency temperature profiles across experimental ponds B and F, May-August 2020 
#    3. "hf_surfaceDO.csv" - High frequency surface dissolved oxygen (DO) concentrations and saturation in experimental ponds B and F, May-Aug 2020
#    4. "biomass_canopy_2020.csv" - Aquatic macrophyte biomass and canopy height, experimental ponds B and F, May-August 2020 
#          Biomass, canopy height, and species composition for submersed aquatic macrophytes across in two experimental ponds
#    5. "biomass_map_edit.csv" - Aquatic macrophyte biomass data with information for mapping sites and species, hand-entered
#    6. Individual csv's for each chain of temperature sensors with missing 0.25m intervals have been interpolated for visualization
#          "temp_interpolated_Fdeep.csv", "temp_interpolated_Fmiddle.csv", "temp_interpolated_Fshallow.csv"
#          "temp_interpolated_Bdeep.csv", "temp_interpolated_Bmiddle.csv", "temp_interpolated_Bshallow.csv"
#    7. "meteorological_ISU_horfarm.csv" - meterological data from ISU soil moisture network
#        Data Source: https://mesonet.agron.iastate.edu/agclimate/ 
#    8. "DO_profiles_2020.csv" - manual, continuous profiles of DO across experimental ponds B and F, May-August 2020
#    9. High frequency temperature profiles in wide format for use in r Lake Analyzer (text files, one file per site)
#          "temp_hf_Fmid_wide.txt", "temp_hf_Bmid_wide.txt"
#   10. "pond_hypsography.txt" - depth area relationship for experimental ponds
#   11. "wind2_wide.txt" - wide format wind data (from meteorological_ISU_hortfarm)

#TEXT FILES OF WIDE-FORMAT DATA FOR MIDDLE SITE OF EACH POND - CALC thermodepth, LN. Wide format wind and hypso for LN


# Clear environment, set working directory - UPDATE AS NEEDED
setwd('C:/Users/Ellen/Desktop/Box Sync/Albright DISSERTATION/Albright_Chapter_4/DATA/SUBMIT')

# REQUIRED PACKAGES FOR CODE - install as needed
# install.packages("tidyverse")
# install.packages("mgcv")
# install.packages("ggnewscale")
# install.packages("rLakeAnalyzer")
# install.packages("lubridate")
# intall.packages("viridis")
# install.packages("gridExtra")
# install.packages("cowplot")
# install.packages("RColorBrewer")

library(tidyverse)
library(mgcv)
library(ggnewscale)
library(rLakeAnalyzer)
library(lubridate)
library(viridis)
library(gridExtra)
library(cowplot)
library(RColorBrewer)


### OVERVIEW OF SCRIPT ORGANIZATION ------------------------------------------------------------------------------------------------------------------------------------------
# Figures included in main manuscript
# PART 1: EXPERIMENTAL CONTEXT AND ENVIRONMENTAL STRESSORS
#         Fig. 2 - Stressors and Responses (time series of chlorophyll-a, surface water temperature, surface DO, macrophyte biomass; note nutrient additions and heatwave)
#         Fig. 3 - Macrophyte biomass response to stressors (map)
# PART 2: SPATIOTEMPORAL VARIATION IN THERMAL STRUCTURE - Describe and explain
#         Fig. 4 - Temperature profile and canopy height time series (intrinsic factors)
#         Fig. 5 - Air temperature and wind speed (extrinsic factors)
# PART 3: SPATIOTEMPORAL VARIATION IN DISSOLVED OXYGEN - Describe and explain
#         Table 1 - Coefficient of variation for DO over space and time
#         Fig. 6 - Heatmap of bottom water DO and macrophyte biomass
# Figures included in supplementary materials
#         Fig. S1 - Examples of individual DO saturation profiles near deep sites (methods)
#         Fig. S2 - Estimated thermocline depth over time by pond
#         Fig. S3 - Lake Number over time by pond
#         Fig. S4 - Bottom water temperature time series across sampling sites
#         Fig. S5 - Time series of surface, bottom, a delta-dissolved oxygen saturation
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------


#   Plotting Colors
pondB_col_transparent = rgb(249,87,0, max = 255, alpha = 150)
pondB_col = rgb(249,87,0, max = 255, alpha = 200)
pondF_col_transparent = rgb(0,164,204, max = 255, alpha = 150)
pondF_col = rgb(0,164,204, max = 255, alpha = 200)

pondB_col_transparent = rgb(219,165,34, max = 255, alpha = 150)
pondB_col = rgb(219,165,34, max = 255, alpha = 200)
pondF_col_transparent = rgb(0, 137, 134, max = 255, alpha = 150)
pondF_col = rgb(0, 137, 134, max = 255, alpha = 200)

pondB_col_light = rgb(219,165,34, max = 255, alpha = 70)
pondB_col_dark = rgb(219,165,34, max = 255, alpha = 255)
pondF_col_light = rgb(0, 137, 134, max = 255, alpha = 70)
pondF_col_dark = rgb(0, 137, 134, max = 255, alpha = 255)

# MAIN MANUSCRIPT FIGURES -----------------------------------------------------------------------------------------------------------------
# PART 1: EXPERIMENTAL CONTEXT AND ENVIRONMENTAL STRESSORS --------------------------------------------------------------------------------
#### FIGURE 2A - Chlorophyll-a time series (gam code from Grace Wilkinson) --------------------------------------------------------------------

# Read in data (hort20_surface_dat.csv), subset by study pond
pondB_chl = read_csv("hort20_surface_dat.csv") %>%
  filter(pond_id == 'B') %>%
  filter(!(is.na(chla)))
pondF_chl = read_csv("hort20_surface_dat.csv") %>%
  filter(pond_id == 'F') %>%
  filter(!(is.na(chla)))

#Chlorophyll GAM - Pond B
chlB_gam <- gam(chla ~ s(doy, k = 40), 
                data = pondB_chl, method = 'REML')
summary(chlB_gam)
gam.check(chlB_gam)
#Chlorophyll GAM - Pond F
chlF_gam <- gam(chla ~ s(doy, k = 40), 
                data = pondF_chl, method = 'REML')
summary(chlF_gam)
gam.check(chlF_gam)

#### FIGURE 2A----------------------------------------------------------------------
chla_plot<-
ggplot(data=pondB_chl, aes(x=doy, y=chla))+
  geom_rect(aes(xmin=185, xmax=190, ymin=-Inf, ymax=Inf), color=NA, fill="grey80")+
  geom_point(color=pondB_col, size=1.5)+
  geom_smooth(method = "gam", formula = y ~ s(x, k = 40), size = 1, color=pondB_col, fill=pondB_col_transparent)+
  ylim(0,12.5)+xlim(143,240)+
  new_scale_color() +
  geom_point(data=pondF_chl, aes(x=doy, y=chla),color=pondF_col,size=2,shape=15)+
  geom_smooth(data=pondF_chl, aes(x=doy, y=chla),method = "gam", formula = y ~ s(x, k = 40), size = 1, color=pondF_col, fill=pondF_col_transparent)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab(bquote("Chlorophyll-a (?g"*~L^-1*")"))+theme(axis.text=element_text(color="black",size=9),axis.title=element_text(size=11),
                                                                          axis.title.x=element_blank(),axis.text.x=element_blank())+
  geom_vline(xintercept=176,color="grey10",linetype="dashed",size=0.7)+
  geom_vline(xintercept=211,color="grey10",linetype="dashed",size=0.7)
  
  #### FIGURE 2B - Surface water temperature time series ------------------------------------------------------------------------------------------
# read in high frequency temperature data (temp_profiles_2020.csv), subset to exclude shallow site
temp_deeper = read_csv("temp_profiles_2020.csv") %>%
  filter(site_id != '21')

# Delineate "heat event"
# calculate the mean daily surface water temperature (0-0.25m) based on the middle and deep sites for each pond
mean_pond_daily_surface_temp<-temp_deeper %>% 
  filter(temp_depth_m >= 0 & temp_depth_m <= 0.25) %>%
  group_by(doy, pond) %>% 
  mutate(pond_meanTEMP = mean(temp_c), pond_maxTEMP=max(temp_c), pond_minTEMP=min(temp_c)) %>% 
  select(doy, pond, pond_meanTEMP, pond_maxTEMP,pond_minTEMP) %>% 
  slice(n=1) %>% 
  ungroup()

# Determine 90 and 95 percentiles for pond-level, daily mean surface (0-0.25 m) water temperature
quantile(mean_pond_daily_surface_temp$pond_meanTEMP[mean_pond_daily_surface_temp$pond=="F"], probs=c(0.9, 0.95)) # 90% = 29.83, 95%=30.60
quantile(mean_pond_daily_surface_temp$pond_meanTEMP[mean_pond_daily_surface_temp$pond=="B"], probs=c(0.9, 0.95)) # 90% = 29.29, 95%=29.80


#### FIGURE 2B----------------------------------------------------------------------
surface_temp_plot<-
ggplot(data=mean_pond_daily_surface_temp, aes(group=pond))+
  ylim(16, 39)+xlim(143,240)+
  geom_vline(xintercept=176,color="grey10",linetype="dashed",size=0.7)+
  geom_vline(xintercept=211,color="grey10",linetype="dashed",size=0.7)+
  geom_rect(aes(xmin=185, xmax=190, ymin=-Inf, ymax=Inf), color=NA, fill="grey80")+
  geom_hline(yintercept=30.6,color="grey10",size=0.7)+
  geom_hline(yintercept=29.83,color="grey10",size=0.7)+
  geom_line(aes(x=doy, y=pond_minTEMP,color=pond),size=1)+
  geom_line(aes(x=doy, y=pond_maxTEMP,color=pond),size=1)+
  scale_color_manual(values=c(pondB_col_transparent, pondF_col_transparent))+
  new_scale_color()+
  geom_line(aes(x=doy, y=pond_meanTEMP,color=pond),size=1.1)+
  geom_line(aes(x=doy, y=pond_meanTEMP,color=pond),size=1.1)+
  geom_point(aes(x=doy, y=pond_meanTEMP,color=pond,shape=pond),size=2)+
  geom_point(aes(x=doy, y=pond_meanTEMP,color=pond,shape=pond),size=2)+
  scale_color_manual(values=c(pondB_col, pondF_col))+
  scale_shape_manual(values=c(16,15))+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab(bquote("Surface Water Temperature (?C)"))+
  theme(axis.text=element_text(color="black",size=9),axis.title=element_text(size=11),legend.position="none",axis.title.x=element_blank(),axis.text.x=element_blank())


#### FIGURE 2C - Surface water dissolved oxygen time series --------------------------------------------------------------------------------
# read in high frequency DO data (hf_surfaceDO.csv), subset to exclude experimental ponds that Tyler studied
hfDO<-read_csv("hf_surfaceDO.csv") %>% 
  filter(pond=="B" | pond=="F")

# calculate daily mean
daily_hfDO<-hfDO %>% 
  group_by(pond, doy) %>% 
  summarize(across(c(do, dosat), ~mean(., na.rm=T))) %>%
  ungroup()

#### FIGURE 2C --------------------------------------------------------------------
hfDOplot<-
  ggplot(data=hfDO, aes(x=doy_frac, y=dosat, group=pond))+
  geom_hline(yintercept=100,color="grey30",linetype="solid",size=0.7)+
  geom_rect(aes(xmin=185, xmax=190, ymin=-Inf, ymax=Inf), color=NA, fill="grey80")+
  geom_line(aes(color=pond), size=1)+
  scale_color_manual(values=c(pondB_col_transparent, pondF_col_transparent))+
  new_scale_color() +
  geom_line(data=daily_hfDO,aes(x=doy, y=dosat, group=pond, color=pond), size=1.1)+
  scale_color_manual(values=c(pondB_col_dark, pondF_col_dark))+
  ylim(0,250)+xlim(143,240)+
  geom_vline(xintercept=176,color="grey10",linetype="dashed",size=0.7)+
  geom_vline(xintercept=211,color="grey10",linetype="dashed",size=0.7)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Surface DO Saturation (%)")+theme(axis.text=element_text(color="black",size=9),axis.title=element_text(size=11),
                                                                axis.title.x=element_blank(),axis.text.x=element_blank())+
  theme(legend.title=element_blank(), legend.position = "none",legend.text = element_text(size=10))

#### FIGURE 2D - Mean macrophyte biomass time series by study pond ------------------------------------------------------------------------------
# read in macrophyte data and subset biomass data
biomass = read_csv("biomass_canopy_2020.csv") %>% 
  filter(site_type == "Biomass")

# calculate biomass per 1 sq-meter (divide biomass by area sampled by rake, 0.4m2)
biomass$biomass_gm2 <- biomass$biomass_g/0.4
# need to add some categories by hand...
write.csv(biomass, "biomass_map_raw.csv",row.names=FALSE)
# read back in...
bio_map<-read.csv("biomass_map_edit.csv")

# Calculate whole-pond mean biomass, but equal weight to inner versus outer sites
bio_mean_pondweight<-bio_map %>% 
  mutate(pondweight = case_when(.$site_id %in% c("F1","F3","F4","F6","F7","F9","F10","F12","F13","F15","F16","F18","F2","F17",
                                                 "B1","B3","B4","B6","B7","B9","B10","B12","B13","B15","B16","B18","B2","B17")~.0357,
                                .$site_id %in% c ("F5","F8","F11","F14","B5","B8","B11","B14")~0.125)) %>% 
  select(doy, pond, site_id, biomass_gm2, pondweight)

bio_mean_pondweight$pondweight_bio<-bio_mean_pondweight$biomass_gm2*bio_mean_pondweight$pondweight

bio_weighted_mean<-bio_mean_pondweight %>% 
  group_by(pond, doy) %>% 
  mutate(pond_meanBiomass = sum(pondweight_bio), pond_SEMBio = (sd(biomass_gm2))/sqrt(18)) %>% 
  select(doy, pond, pond_meanBiomass, pond_SEMBio) %>% 
  slice(n=1) %>% 
  ungroup()

#### FIGURE 2D ----------------------------------------------------------------------------------------------------------------------------------
mac_bio_plot<-
ggplot(data=bio_weighted_mean)+
  xlim(143,240)+ylim(10,115)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year, 2020")+ylab(bquote("Mean Macrophyte Biomass (g "*~m^-2*")")) +theme(axis.text.x=element_text(color="black",size=9),axis.title.x=element_text(size=11))+
  theme(legend.title=element_blank(), legend.position = "none",legend.text = element_text(size=10))+
  geom_rect(aes(xmin=185, xmax=190, ymin=-Inf, ymax=Inf), color=NA, fill="grey80")+
  geom_vline(xintercept=176,color="grey10",linetype="dashed",size=0.7)+
  geom_vline(xintercept=211,color="grey10",linetype="dashed",size=0.7)+
  geom_point(aes(x=doy, y=pond_meanBiomass, group=pond, color=pond, shape=pond), size=3)+
  geom_errorbar(aes(x=doy, ymin=pond_meanBiomass-pond_SEMBio, ymax=pond_meanBiomass+pond_SEMBio, group=pond, color=pond),width=0,size=1,position=position_dodge(0.05))+
  scale_color_manual(values=c(pondB_col_dark, pondF_col_dark))+
  scale_shape_manual(values=c(19,15))


#### FULL FIGURE 2
final_figure2<-plot_grid(chla_plot,surface_temp_plot,hfDOplot,mac_bio_plot,nrow=4,align="v")
ggsave("Figure2.png", final_figure2, width=5.5, height=10.5, units="in", dpi=300)

#### FIGURE 3 - MACROPHYTE BIOMASS RESPONSE TO STRESSORS -------------------------------------------------------------------------------------------
biomass_map_fig3<-
  ggplot(bio_map,aes(X,Y,size=biomass_gm2,fill=Species))+
  geom_rect(aes(xmin=0.5, xmax=3.5, ymin=0.5, ymax=6.5), size=1, color="black", fill=NA)+
  geom_rect(aes(xmin=0.5, xmax=3.5, ymin=7.5, ymax=13.5), size=1, color="black", fill=NA)+
  geom_rect(aes(xmin=4.5, xmax=7.5, ymin=0.5, ymax=6.5), size=1, color="black", fill=NA)+
  geom_rect(aes(xmin=4.5, xmax=7.5, ymin=7.5, ymax=13.5), size=1, color="black", fill=NA)+
  geom_rect(aes(xmin=8.5, xmax=11.5, ymin=0.5, ymax=6.5), size=1.5, color="#e31a1c", fill=NA)+
  geom_rect(aes(xmin=8.5, xmax=11.5, ymin=7.5, ymax=13.5), size=1.5, color="#e31a1c", fill=NA)+
  geom_rect(aes(xmin=12.5, xmax=15.5, ymin=0.5, ymax=6.5), size=1, color="black", fill=NA)+
  geom_rect(aes(xmin=12.5, xmax=15.5, ymin=7.5, ymax=13.5), size=1, color="black", fill=NA)+
  geom_rect(aes(xmin=16.5, xmax=19.5, ymin=0.5, ymax=6.5), size=1, color="black", fill=NA)+
  geom_rect(aes(xmin=16.5, xmax=19.5, ymin=7.5, ymax=13.5), size=1, color="black", fill=NA)+
  geom_rect(aes(xmin=20.5, xmax=23.5, ymin=0.5, ymax=6.5), size=1, color="black", fill=NA)+
  geom_rect(aes(xmin=20.5, xmax=23.5, ymin=7.5, ymax=13.5), size=1, color="black", fill=NA)+
  geom_segment(aes(x=8, y=7, xend=8, yend=14), size=1, linetype="dashed", color="black")+
  geom_segment(aes(x=16, y=7, xend=16, yend=14), size=1, linetype="dashed", color="black")+
  scale_size(range=c(2,20))+
  scale_fill_manual(values=c("#dfc27d","#003c30","#35978f","#c7eae5"))+
  geom_point(shape=21,colour="black")+theme_void()+
  labs(x="",y="",size=bquote("Biomass (g"*~m^-2*")"),fill="")+
  theme(legend.position="bottom",legend.box="vertical",legend.text=element_text(size=11),legend.key.width=unit(1.5, "cm"))+
  guides(size=guide_legend(nrow=1))

ggsave("Figure3.png", biomass_map_fig3, width=8, height=7, units="in", dpi=300)

# PART 2: SPATIOTEMPORAL VARIATION IN THERMAL STRUCTURE --------------------------------------------------------------------------------------------
#### FIGURE 4 - Water temp heat maps and macrophyte canopy height (intrinsic factors controlling pond thermal structure) ---------------------------
# read in macrophyte data and subset canopy data
canopy = read_csv("biomass_canopy_2020.csv") %>% 
  filter(site_type == "Canopy")
# calculate canopy height as a percent of water depth
canopy$canopy_per <- (canopy$canopy_m/canopy$depth_m)*100

# Read in all high frequency temperature chain data
temp <- read.csv("temp_profiles_2020.csv", as.is=T)
# summaries - range of FULL data set is 14.04 to 38.6
summary(temp$temp_c[temp$pond=="F"]) # median = 24.74, mean=24.79
summary(temp$temp_c[temp$pond=="B"]) # median = 24.64, mean=24.64

# Read in individual csv's with interpolated values for missing intervals
Ffinal <- read.csv("temp_interpolated_Fdeep.csv")
Ffinal_mid <- read.csv("temp_interpolated_Fmiddle.csv")
Ffinal_shallow <- read.csv("temp_interpolated_Fshallow.csv")
Bfinal <- read.csv("temp_interpolated_Bdeep.csv")
Bfinal_mid <- read.csv("temp_interpolated_Bmiddle.csv")
Bfinal_shallow <- read.csv("temp_interpolated_Bshallow.csv")

#### FIGURE 4 ---------------------------------------------------------------------
# Water temperature profile heat maps - testing spectral color ramp
spref_shal<-ggplot(Ffinal_shallow, aes(doy_frac, temp_depth_m, fill= temp_c)) + 
  geom_tile()+ylim(c(2.15,-0.15))+
  geom_segment(aes(x = 185, y = -0.15, xend = 190, yend = -0.15), size=1.2, col="black")+
  geom_segment(aes(x = 185, y = 1.65, xend = 190, yend = 1.65), size=1.2, col="black")+
  xlim(143,233)+
  scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11, 'Spectral')),name="Temperature (C)",
                       limits=c(14,38),)+
  theme_classic()+theme(plot.margin=margin(t=0,r=0, b=0, l=2, unit="pt"))+
  xlab(" ") + ylab("Depth (m)")+theme(axis.text=element_text(color="black",size=8),legend.position="none",axis.title=element_text(size=10))

spnut_shal<-ggplot(Bfinal_shallow, aes(doy_frac, temp_depth_m, fill= temp_c)) + 
  geom_tile()+ylim(c(2.15,-0.15))+
  geom_segment(aes(x = 185, y = -0.15, xend = 190, yend = -0.15), size=1.2, col="black")+
  geom_segment(aes(x = 185, y = 1.65, xend = 190, yend = 1.65), size=1.2, col="black")+
  xlim(143,233)+
  scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11, 'Spectral')),name="Temperature (C)",
                       limits=c(14,38),)+
  theme_classic()+theme(plot.margin=margin(t=0,r=0, b=0, l=2, unit="pt"))+
  xlab("Day of Year") + ylab("Depth (m)")+theme(axis.text=element_text(color="black",size=8),legend.position="none",axis.title=element_text(size=10))

spref_mid<-ggplot(Ffinal_mid, aes(doy_frac, temp_depth_m, fill= temp_c)) + 
  geom_tile()+ylim(c(2.15,-0.15))+
  geom_segment(aes(x = 185, y = -0.15, xend = 190, yend = -0.15), size=1.2, col="black")+
  geom_segment(aes(x = 185, y = 2.15, xend = 190, yend = 2.15), size=1.2, col="black")+
  xlim(143,233)+
  scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11, 'Spectral')),name="Temperature (C)",
                       limits=c(14,38),)+
  theme_classic()+theme(plot.margin=margin(t=0,r=0, b=0, l=2, unit="pt"))+
  xlab(" ") +theme(axis.text=element_text(color="black",size=8),legend.position="none",axis.title=element_text(size=10),
                   axis.text.y=element_blank(), axis.title.y = element_blank(), axis.line.y=element_blank(), axis.ticks.y=element_blank())

spnut_mid<-ggplot(Bfinal_mid, aes(doy_frac, temp_depth_m, fill= temp_c)) + 
  geom_tile()+ylim(c(2.15,-0.15))+
  geom_segment(aes(x = 185, y = -0.15, xend = 190, yend = -0.15), size=1.2, col="black")+
  geom_segment(aes(x = 185, y = 2.15, xend = 190, yend = 2.15), size=1.2, col="black")+
  xlim(143,233)+
  scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11, 'Spectral')),name="Temperature (C)",
                       limits=c(14,38),)+
  theme_classic()+theme(plot.margin=margin(t=0,r=0, b=0, l=2, unit="pt"))+
  
  xlab("Day of Year") +theme(axis.text=element_text(color="black",size=8),legend.position="none",axis.title=element_text(size=10),
                             axis.text.y=element_blank(), axis.title.y = element_blank(), axis.line.y=element_blank(), axis.ticks.y=element_blank())

spref_deep<-ggplot(Ffinal, aes(doy_frac, temp_depth_m, fill= temp_c)) + 
  geom_tile()+ylim(c(2.15,-0.15))+
  geom_segment(aes(x = 185, y = -0.15, xend = 190, yend = -0.15), size=1.2, col="black")+
  geom_segment(aes(x = 185, y = 2.15, xend = 190, yend = 2.15), size=1.2, col="black")+
  xlim(143,233)+
  scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11, 'Spectral')),name="Temperature (C)",
                       limits=c(14,38),)+
  theme_classic()+theme(plot.margin=margin(t=0,r=0, b=0, l=2, unit="pt"))+
  xlab(" ") +theme(axis.text=element_text(color="black",size=8),legend.position="none",axis.title=element_text(size=10),
                   axis.text.y=element_blank(), axis.title.y = element_blank(), axis.line.y=element_blank(), axis.ticks.y=element_blank())

spnut_deep<-ggplot(Bfinal, aes(doy_frac, temp_depth_m, fill= temp_c)) + 
  geom_tile()+ylim(c(2.15,-0.15))+
  geom_segment(aes(x = 185, y = -0.15, xend = 190, yend = -0.15), size=1.2, col="black")+
  geom_segment(aes(x = 185, y = 2.15, xend = 190, yend = 2.15), size=1.2, col="black")+
  xlim(143,233)+
  scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11, 'Spectral')),name="Temperature (C)",
                       limits=c(14,38),)+
  theme_classic()+theme(plot.margin=margin(t=0,r=0, b=0, l=2, unit="pt"))+
  xlab("Day of Year") +theme(axis.text=element_text(color="black",size=8),legend.position="none",axis.title=element_text(size=10),
                             axis.text.y=element_blank(), axis.title.y = element_blank(), axis.line.y=element_blank(), axis.ticks.y=element_blank())


# canopy plots
# pond F, shallow site
F21_can<-ggplot(subset(canopy, site_id=="F21"), aes(x=doy, y=canopy_per))+
  geom_area(fill="grey80", alpha=0.8)+
  geom_line(color="grey20", size=1)+
  xlim(143,234)+
  theme_classic()+theme(plot.margin=margin(t=5,r=2, b=6, l=0, unit="pt"))+
  ylab("Canopy (%)")+theme(axis.text.y=element_text(color="black",size=8), axis.title.y =element_text(size=10),legend.position="none",
                           axis.text.x=element_blank(), axis.title.x = element_blank(), axis.line.x=element_blank(), axis.ticks.x=element_blank(),
                           plot.title = element_text(size=12, face="bold.italic", hjust=0.5))

# pond B, shallow site
B21_can<-ggplot(subset(canopy, site_id=="B21"), aes(x=doy, y=canopy_per))+
  geom_area(fill="grey80", alpha=0.8)+
  geom_line(color="grey20", size=1)+
  xlim(143,234)+ 
  theme_classic()+theme(plot.margin=margin(t=0,r=2, b=4, l=0, unit="pt"))+
  ylab("Canopy (%)")+theme(axis.text.y=element_text(color="black",size=8), axis.title.y =element_text(size=10),legend.position="none",
                           axis.text.x=element_blank(), axis.title.x = element_blank(), axis.line.x=element_blank(), axis.ticks.x=element_blank(),
                           plot.title = element_text(size=14, face="bold.italic", hjust=0.5))

# pond F, middle site
F20_can<-ggplot(subset(canopy, site_id=="F20"), aes(x=doy, y=canopy_per))+
  geom_area(fill="grey80", alpha=0.8)+
  geom_line(color="grey20", size=1)+
  xlim(143,234)+
  theme_classic()+theme(plot.margin=margin(t=5,r=2, b=6, l=0, unit="pt"))+
  theme(legend.position="none",
        axis.text.x=element_blank(), axis.title.x = element_blank(), axis.line.x=element_blank(), axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), axis.title.y = element_blank(), axis.line.y=element_blank(), axis.ticks.y=element_blank(),
        plot.title = element_text(size=12, face="bold.italic", hjust=0.5))

# pond B, middle site
B20_can<-ggplot(subset(canopy, site_id=="B20"), aes(x=doy, y=canopy_per))+
  geom_area(fill="grey80", alpha=0.8)+
  geom_line(color="grey20", size=1)+
  xlim(143,234)+ 
  theme_classic()+theme(plot.margin=margin(t=0,r=2, b=4, l=0, unit="pt"))+
  theme(legend.position="none",
        axis.text.x=element_blank(), axis.title.x = element_blank(), axis.line.x=element_blank(), axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), axis.title.y = element_blank(), axis.line.y=element_blank(), axis.ticks.y=element_blank(),
        plot.title = element_text(size=14, face="bold.italic", hjust=0.5))

# pond F, deep site
F19_can<-ggplot(subset(canopy, site_id=="F19"), aes(x=doy, y=canopy_per))+
  geom_area(fill="grey80", alpha=0.8)+
  geom_line(color="grey20", size=1)+
  xlim(143,234)+
  theme_classic()+theme(plot.margin=margin(t=5,r=2, b=6, l=0, unit="pt"))+
  theme(legend.position="none",
        axis.text.x=element_blank(), axis.title.x = element_blank(), axis.line.x=element_blank(), axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), axis.title.y = element_blank(), axis.line.y=element_blank(), axis.ticks.y=element_blank(),
        plot.title = element_text(size=12, face="bold.italic", hjust=0.5))

# pond B, deep site
B19_can<-ggplot(subset(canopy, site_id=="B19"), aes(x=doy, y=canopy_per))+
  geom_area(fill="grey80", alpha=0.8)+
  geom_line(color="grey20", size=1)+
  xlim(143,234)+
  theme_classic()+theme(plot.margin=margin(t=0,r=2, b=4, l=0, unit="pt"))+
  theme(legend.position="none",
        axis.text.x=element_blank(), axis.title.x = element_blank(), axis.line.x=element_blank(), axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), axis.title.y = element_blank(), axis.line.y=element_blank(), axis.ticks.y=element_blank(),
        plot.title = element_text(size=14, face="bold.italic", hjust=0.5))

heat_legend<-ggplot(Ffinal, aes(doy_frac, temp_depth_m, fill= temp_c)) + 
  geom_tile()+ylim(c(2.15,-0.15))+
  xlim(143,233)+
  scale_fill_gradientn(colors = viridis_pal(option="inferno")(9),name="Temperature (?C)",
                       limits=c(14,38),)+
  theme_classic()+theme(plot.margin=margin(t=0,r=0, b=0, l=2, unit="pt"))+
  xlab(" ") + ylab("Depth (m)")+theme(axis.text=element_text(color="black",size=8),legend.position="bottom",legend.key.width=unit(1.5, "cm"),axis.title=element_text(size=10))

# Legend
heat_legendsp<-ggplot(Ffinal, aes(doy_frac, temp_depth_m, fill= temp_c)) + 
  geom_tile()+ylim(c(2.15,-0.15))+
  xlim(143,233)+
  scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11, 'Spectral')),name="Temperature (?C)",
                       limits=c(14,38),)+
  theme_classic()+theme(plot.margin=margin(t=0,r=0, b=0, l=2, unit="pt"))+
  xlab(" ") + ylab("Depth (m)")+theme(axis.text=element_text(color="black",size=8),legend.position="bottom",legend.key.width=unit(1.5, "cm"),axis.title=element_text(size=10))

splegend_heat<-get_legend(heat_legendsp)

# Final figure
spectral_heatmap<-grid.arrange(F21_can, F20_can, F19_can, spref_shal, spref_mid, spref_deep,
                               B21_can, B20_can, B19_can, spnut_shal, spnut_mid,spnut_deep,
                               splegend_heat,
                               layout_matrix = rbind(c(1, 2, 3),
                                                     c(4, 5, 6),
                                                     c(4, 5, 6),
                                                     c(4, 5, 6),
                                                     c(7, 8, 9),
                                                     c(10, 11, 12),
                                                     c(10, 11, 12),
                                                     c(10, 11, 12),
                                                     c(13, 13, 13)))


ggsave("Figure4_spectral.png", spectral_heatmap, width=8, height=7, units="in", dpi=300)


#### FIGURE 5 - Air temperature and wind speed (extrinsic factors influencing pond thermal structure) --------------------------------------------
met<-read.csv("meteorological_ISU_hortfarm.csv")

# Calculate daily means for meteorological varialbes
met_daily_means<-met %>%
  group_by(doy) %>%
  summarize(across(c(AirTemp_C, WindSpeed_ms, Rain), ~mean(., na.rm=T))) %>%
  ungroup()

air<-ggplot()+
  geom_rect(aes(xmin=185, xmax=190, ymin=-Inf, ymax=Inf), color=NA, fill="grey85")+
  geom_line(data=met, aes(x=doy_frac, y=AirTemp_C), color="gray60",size=0.3)+
  geom_line(data=met_daily_means, aes(x=doy, y=AirTemp_C), color="deeppink4",size=1.1)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Air Temperature (?C)")+
  theme(axis.text=element_text(color="black",size=9),legend.position="none",axis.title=element_text(size=11),
        axis.title.x = element_blank())

wind<-ggplot()+
  geom_rect(aes(xmin=185, xmax=190, ymin=-Inf, ymax=Inf), color=NA, fill="grey85")+
  geom_line(data=met, aes(x=doy_frac, y=WindSpeed_ms), color="gray60",size=0.3)+
  geom_line(data=met_daily_means, aes(x=doy, y=WindSpeed_ms), color="cadetblue4",size=1.1)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab(bquote("Wind Speed (m"*~s^-2*")"))+
  theme(axis.text=element_text(color="black",size=9),legend.position="none",axis.title=element_text(size=11))

met_plot<-plot_grid(air,wind,nrow=2,align="v")

ggsave("Figure5_windair.png", met_plot, width=5, height=5, units="in", dpi=300)

# PART 3: SPATIOTEMPORAL VARIATION IN DISSOLVED OXYGEN - Describe and explain --------------------------------------------------------------
#### TABLE 1 - Coefficient of variation for DO over space and time --------------------------------------------------------------------------
# read in manual DO profiles
do<-read.csv("DO_profiles_2020.csv")

# Calculate depth-based variable means for manual profiles (take the mean of DO over each depth interval, for each site on each sampling day)
#  Surface depth (0 to 0.25 m)
DO_surface<-do %>%
  group_by(doy, pond, site_id) %>%
  filter(vertical_m >= 0 & vertical_m <= 0.25) %>%
  summarize(across(c(odo_mgL, odo_sat, temp_c), ~mean(., na.rm=T))) %>%
  ungroup()

# Bottom depth >= (max(vertical_m))-0.25
DO_bottom<-do %>%
  group_by(doy, pond, site_id) %>%
  filter(vertical_m >= (max(vertical_m))-0.25) %>%
  summarize(across(c(odo_mgL, odo_sat, temp_c), ~mean(., na.rm=T))) %>%
  ungroup()

# Subset surface and bottom water DO data frames by pond
hypoDO_F<-subset(DO_bottom, pond=="F")
hypoDO_B<-subset(DO_bottom, pond=="B")
epiDO_F<-subset(DO_surface, pond=="F")
epiDO_B<-subset(DO_surface, pond=="B")

### Calculate the mean and coefficient of variation (mean/standard deviation) for DO 
# Reference pond, bottom water, TEMPORAL VARIATION
hypoDO_F_summary_time<-hypoDO_F %>% 
  group_by(site_id) %>% 
  mutate(mean_DOsat=mean(odo_sat), CV_DOsat=(sd(odo_sat))/mean(odo_sat)*100) %>%
  select(site_id,mean_DOsat,CV_DOsat) %>% 
  slice(n=1) %>% 
  ungroup()
min(hypoDO_F_summary_time$CV_DOsat) #min=24.1
max(hypoDO_F_summary_time$CV_DOsat) #max=45.6
mean(hypoDO_F_summary_time$CV_DOsat) #mean=34.4

# Reference pond, bottom water, SPATIAL VARIATION
hypoDO_F_summary_space<-hypoDO_F %>% 
  group_by(doy) %>% 
  mutate(mean_DOsat=mean(odo_sat), CV_DOsat=(sd(odo_sat))/mean(odo_sat)*100) %>%
  select(doy,mean_DOsat,CV_DOsat) %>% 
  slice(n=1) %>% 
  ungroup()
min(hypoDO_F_summary_space$CV_DOsat) #min=2.0
max(hypoDO_F_summary_space$CV_DOsat) #max=16.3
mean(hypoDO_F_summary_space$CV_DOsat) #mean=7.5

# Nutrient addition pond, bottom water, TEMPORAL VARIATION
hypoDO_B_summary_time<-hypoDO_B %>% 
  group_by(site_id) %>% 
  mutate(mean_DOsat=mean(odo_sat), CV_DOsat=(sd(odo_sat))/mean(odo_sat)*100) %>%
  select(site_id,mean_DOsat,CV_DOsat) %>% 
  slice(n=1) %>% 
  ungroup()
min(hypoDO_B_summary_time$CV_DOsat) #min=22.1
max(hypoDO_B_summary_time$CV_DOsat) #max=40.3
mean(hypoDO_B_summary_time$CV_DOsat) #mean=26.8

# Nutrient addition pond, bottom water, SPATIAL VARIATION
hypoDO_B_summary_space<-hypoDO_B %>% 
  group_by(doy) %>% 
  mutate(mean_DOsat=mean(odo_sat), CV_DOsat=(sd(odo_sat))/mean(odo_sat)*100) %>%
  select(doy,mean_DOsat,CV_DOsat) %>% 
  slice(n=1) %>% 
  ungroup()
min(hypoDO_B_summary_space$CV_DOsat) #min=2.0
max(hypoDO_B_summary_space$CV_DOsat) #max=16.9
mean(hypoDO_B_summary_space$CV_DOsat) #mean=7.5

# Reference pond, surface water, TEMPORAL VARIATION
epiDO_F_summary_time<-epiDO_F %>% 
  group_by(site_id) %>% 
  mutate(mean_DOsat=mean(odo_sat), CV_DOsat=(sd(odo_sat))/mean(odo_sat)*100) %>%
  select(site_id,mean_DOsat,CV_DOsat) %>% 
  slice(n=1) %>% 
  ungroup()
min(epiDO_F_summary_time$CV_DOsat) #min=20.2
max(epiDO_F_summary_time$CV_DOsat) #max=34.4
mean(epiDO_F_summary_time$CV_DOsat) #mean=28.9

# Reference pond, surface water, SPATIAL VARIATION
epiDO_F_summary_space<-epiDO_F %>% 
  group_by(doy) %>% 
  mutate(mean_DOsat=mean(odo_sat), CV_DOsat=(sd(odo_sat))/mean(odo_sat)*100) %>%
  select(doy,mean_DOsat,CV_DOsat) %>% 
  slice(n=1) %>% 
  ungroup()
min(epiDO_F_summary_space$CV_DOsat) #min=1.8
max(epiDO_F_summary_space$CV_DOsat) #max=11.3
mean(epiDO_F_summary_space$CV_DOsat) #mean=5.9

# Nutrient addition pond, surface water, TEMPORAL VARIATION
epiDO_B_summary_time<-epiDO_B %>% 
  group_by(site_id) %>% 
  mutate(mean_DOsat=mean(odo_sat), CV_DOsat=(sd(odo_sat))/mean(odo_sat)*100) %>%
  select(site_id,mean_DOsat,CV_DOsat) %>% 
  slice(n=1) %>% 
  ungroup()
min(epiDO_B_summary_time$CV_DOsat) #min=20.6
max(epiDO_B_summary_time$CV_DOsat) #max=28.8
mean(epiDO_B_summary_time$CV_DOsat) #mean=22.8

# Nutrient addition pond, surface water, SPATIAL VARIATION
epiDO_B_summary_space<-epiDO_B %>% 
  group_by(doy) %>% 
  mutate(mean_DOsat=mean(odo_sat), CV_DOsat=(sd(odo_sat))/mean(odo_sat)*100) %>%
  select(doy,mean_DOsat,CV_DOsat) %>% 
  slice(n=1) %>% 
  ungroup()
min(epiDO_B_summary_space$CV_DOsat) #min=2.1
max(epiDO_B_summary_space$CV_DOsat) #max=9.5
mean(epiDO_B_summary_space$CV_DOsat) #mean=4.4


#### FIGURE 6 - Heatmap of bottom water DO and macrophyte biomass ----------------------------------------------------------------------------
# work with bottom DO dataframes (DO), subset by pond
# Subset surface and bottom water DO data frames by pond (hypoDO_F, hypoDO_B)

# order sites by water depth
hypoDO_F$site_order<-ordered(hypoDO_F$site_id, levels=c("F18", "F17", "F16", "F13", "F15", "F12", "F10", "F7", "F9", "F6", "F4", "F1", "F2", "F3", "F14", "F11", "F8", "F5"))
hypoDO_F$doy.f<-as.factor(hypoDO_F$doy)

hypoDO_B$site_order<-ordered(hypoDO_B$site_id, levels=c("B18", "B17", "B16", "B13", "B15", "B12", "B10", "B7", "B9", "B6", "B4", "B1", "B2", "B3", "B14", "B11", "B8", "B5"))
hypoDO_B$doy.f<-as.factor(hypoDO_B$doy)


hypo_DO_F<-ggplot(data=hypoDO_F, aes(x=doy.f, y=site_order, fill=odo_sat))+
  geom_tile()+
  scale_fill_gradientn(colors = viridis_pal(option="mako")(9),name="Bottom Water DO (%)",
                       limits=c(50,250),)+  
  xlab("Day of Year") + ylab("Site ID")+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text=element_text(color="black",size=8),axis.title=element_text(size=10),legend.position="none")

hypo_DO_B<-ggplot(data=hypoDO_B, aes(x=doy.f, y=site_order, fill=odo_sat))+
  geom_tile()+
  scale_fill_gradientn(colors = viridis_pal(option="mako")(9),name="Bottom Water DO (%)",
                       limits=c(50,250),)+  
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab(" ")+
  theme(axis.text=element_text(color="black",size=8),axis.title=element_text(size=10),legend.position="none")

hypoDO_legend<-ggplot(data=hypoDO_F, aes(x=doy.f, y=site_order, fill=odo_sat))+
  geom_tile()+
  geom_text(aes(label = round(odo_sat, 1)))+
  scale_fill_gradientn(colors = viridis_pal(option="mako")(9),name="Bottom Water DO (%)",
                       limits=c(50,250),)+  
  xlab("Day of Year") + ylab("Site ID")+
  theme(axis.text=element_text(color="black",size=8),axis.title=element_text(size=10),legend.position="top",legend.key.width=unit(1.5, "cm"))
legend_hypoDO<-get_legend(hypoDO_legend)

blank<-ggplot()+theme_void()

# add pond mean biomass
meanbiomass_pondB<-subset(bio_mean_pondweightTEST, pond=="B")
meanbiomass_pondF<-subset(bio_mean_pondweightTEST, pond=="F")

pondB_biomassbarplot<-
  ggplot(data=meanbiomass_pondB, aes(x=doy.f, y=pond_meanBio))+
  geom_bar(stat="identity", position="dodge", width=0.5, fill=pondB_col) +
  geom_errorbar( aes(ymin=pond_meanBio-pond_SEMBio, ymax=pond_meanBio+pond_SEMBio), color=pondB_col,alpha=0.9, size=0.9,
                 width = 0.2, position = position_dodge(0.5))+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab(bquote("Mean Biomass (g DW"*~m^-2*")"))+theme(axis.text=element_text(color="black",size=8),axis.title=element_text(size=10))+
  theme(legend.title=element_blank(), legend.position = "none",legend.text = element_text(size=10),axis.title.x=element_blank())

pondF_biomassbarplot<-
  ggplot(data=meanbiomass_pondF, aes(x=doy.f, y=pond_meanBio))+
  geom_bar(stat="identity", position="dodge", width=0.5, fill=pondF_col) +
  geom_errorbar( aes(ymin=pond_meanBio-pond_SEMBio, ymax=pond_meanBio+pond_SEMBio), color=pondF_col,alpha=0.9, size=0.9,
                 width = 0.2, position = position_dodge(0.5))+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab(bquote("Mean Biomass (g DW"*~m^-2*")"))+theme(axis.text=element_text(color="black",size=8),axis.title=element_text(size=10))+
  theme(legend.title=element_blank(), legend.position = "none",legend.text = element_text(size=10),axis.title.x=element_blank())


#### FIGURE 6 -------------------------------------------------------------------
new_do_heatmap<-grid.arrange(pondF_biomassbarplot, pondB_biomassbarplot,
                             hypo_DO_F, hypo_DO_B,
                             legend_hypoDO, blank,
                             layout_matrix = rbind(c(1, 1, 2, 2),
                                                   c(1, 1, 2, 2),
                                                   c(3, 3, 4, 4),
                                                   c(3, 3, 4, 4),
                                                   c(3, 3, 4, 4),
                                                   c(3, 3, 4, 4),
                                                   c(3, 3, 4, 4),
                                                   c(5, 5, 5, 6)))
ggsave("FigureS8_doheatmap.png", new_do_heatmap, width=7, height=9, unit="in", dpi=300)



# -------------------------------------------------------------------------------------------------------------------------------------------------------------------
# SUPPLEMENTARY INFORMATION FIGURES ---------------------------------------------------------------------------------------------------------------------------------
#     Fig. S1 - Examples of individual DO saturation profiles near deep sites (methods)
#     Fig. S2 - Estimated thermocline depth over time by pond
#     Fig. S3 - Lake Number over time by pond
#     Fig. S4 - Bottom water temperature time series across sampling sites
#     Fig. S5 - Time series of surface, bottom, a delta-dissolved oxygen saturation

#### Figure S1 - Examples of individual DO saturation profiles near deep sites (methods) ----------------------------------------------------------------------------
# work with manual dissolved oxygen dataframe, deepest sampling site (#5)
DO_b5<-do %>% 
  filter(site_id=="B5") %>% 
  filter(vertical_m>=0.01)
DO_b5$doy.f<-as.factor(DO_b5$doy)

DOB5<-ggplot(data=DO_b5, aes(x=odo_sat, y=vertical_m, group=doy.f, color=doy.f))+
  geom_line(size=2)+
  ylim(1.6,0)+
  geom_vline(xintercept=100,color="grey50",linetype="solid",size=0.8)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Dissolved Oxygen Saturation (%)") + ylab("Depth (m)")+theme(axis.text=element_text(color="black",size=9),axis.title=element_text(size=11))+
  theme(legend.position = "none",legend.text = element_text(size=10))+guides(color=guide_legend(title="Day of Year"))

DO_f5<-subset(do, site_id=="F5")
DO_f5$doy.f<-as.factor(DO_f5$doy)

DOF5<-ggplot(data=DO_f5, aes(x=odo_sat, y=vertical_m, group=doy.f, color=doy.f))+
  geom_line(size=2)+
  ylim(1.6,0)+
  geom_vline(xintercept=100,color="grey50",linetype="solid",size=0.8)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Dissolved Oxygen Saturation (%)") + ylab("Depth (m)")+theme(axis.text=element_text(color="black",size=9),axis.title=element_text(size=11))+
  theme(legend.position = "none",legend.text = element_text(size=10))+guides(color=guide_legend(title="Day of Year"))

DO_legend<-get_legend(DOF5) #NOTE: need to remove legend.position="none" from DOf5 before this will work
DO_profiles<-grid.arrange(DOB5,DOF5,DO_legend, nrow=1,
                          layout_matrix = rbind(c(1, 1, 1, 2, 2, 2, 3)))

ggsave("DO_profiles.png", DO_profiles, width=8, height=4, units="in", dpi=300)

#### Figure S2 - Estimated thermocline depth over time by pond -----------------------------------------------------------------------------------------------------
# Read in high frequency water temperature profiles in WIDE format
wtrF<-load.ts("temp_hf_Fmid_wide.txt")
wtrB<-load.ts("temp_hf_Bmid_wide.txt")

# Read in pond bathymetry information 
bathy<-load.bathy("pond_hypsography.txt") # load hypsographic curve

#calculate depth of thermocline
ts_thermoF<-ts.thermo.depth(wtrF) 
ts_thermoB<-ts.thermo.depth(wtrB) 

# convert to data frame
thermo_Fdf<-as.data.frame(ts_thermoF)
thermo_Bdf<-as.data.frame(ts_thermoB)

# thermoline depth - Add DOY, bind
#    Pond F
thermo_Fdf$doy<-yday(thermo_Fdf$datetime)
thermo_Fdf$doy_frac<-hour(thermo_Fdf$datetime)
thermo_Fdf$min<-minute(thermo_Fdf$datetime)
thermo_Fdf$minfrac[thermo_Fdf$min=="30"] <- 0.5
thermo_Fdf$minfrac[thermo_Fdf$min=="0"] <- 0
thermo_Fdf$hourfrac<-(thermo_Fdf$doy_frac + thermo_Fdf$minfrac)/24
thermo_Fdf$doy_frac<-thermo_Fdf$doy+thermo_Fdf$hourfrac
thermo_F<-thermo_Fdf %>% 
  select(datetime,doy,doy_frac,thermo.depth) %>% 
  mutate(pond="F")

#    Pond B
thermo_Bdf$doy<-yday(thermo_Bdf$datetime)
thermo_Bdf$doy_frac<-hour(thermo_Bdf$datetime)
thermo_Bdf$min<-minute(thermo_Bdf$datetime)
thermo_Bdf$minfrac[thermo_Bdf$min=="30"] <- 0.5
thermo_Bdf$minfrac[thermo_Bdf$min=="0"] <- 0
thermo_Bdf$hourfrac<-(thermo_Bdf$doy_frac + thermo_Bdf$minfrac)/24
thermo_Bdf$doy_frac<-thermo_Bdf$doy+thermo_Bdf$hourfrac
thermo_B<-thermo_Bdf %>% 
  select(datetime,doy,doy_frac,thermo.depth) %>% 
  mutate(pond="B")

thermo_BOTH<-rbind(thermo_F, thermo_B) # bind dataframes for both ponds together

# remove points where ponds are not stratified - thermocline at surface or bottom
thermo_filter<-thermo_BOTH %>% 
  filter(thermo.depth != "NaN")
thermo_filterF<-subset(thermo_filter, pond=="F")
thermo_filterB<-subset(thermo_filter, pond=="B")

#### FIGURE S2 ------------------------------------------------
thermo_f<-
  ggplot(data=thermo_filterF, aes(x=doy_frac, y=thermo.depth))+
  geom_rect(aes(xmin=180, xmax=198, ymin=-Inf, ymax=Inf), color=NA, fill="grey80")+
  geom_rect(aes(xmin=177.5, xmax=179.5, ymin=-Inf, ymax=Inf), color=NA, fill="grey80")+
  geom_rect(aes(xmin=199.8, xmax=210, ymin=-Inf, ymax=Inf), color=NA, fill="grey80")+
  geom_rect(aes(xmin=155.5, xmax=160.7, ymin=-Inf, ymax=Inf), color=NA, fill="grey80")+
  geom_rect(aes(xmin=168, xmax=174.9, ymin=-Inf, ymax=Inf), color=NA, fill="grey80")+
  geom_line(size=1, color=pondF_col)+xlim(143,233)+ylim(2,0)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Thermocline Depth (m)")+theme(axis.text=element_text(color="black",size=10),axis.title=element_text(size=12),
                                                            axis.text.x=element_blank(), axis.title.x=element_blank())
thermo_b<-
  ggplot(data=thermo_filterB, aes(x=doy_frac, y=thermo.depth))+
  geom_rect(aes(xmin=178, xmax=192, ymin=-Inf, ymax=Inf), color=NA, fill="grey80")+
  geom_rect(aes(xmin=156.8, xmax=159, ymin=-Inf, ymax=Inf), color=NA, fill="grey80")+
  geom_rect(aes(xmin=169.5, xmax=174.5, ymin=-Inf, ymax=Inf), color=NA, fill="grey80")+
  geom_vline(xintercept=176,color="black",linetype="dashed",size=0.8)+
  geom_vline(xintercept=211,color="black",linetype="dashed",size=0.8)+
  geom_line(size=1, color=pondB_col)+xlim(143,233)+ylim(2,0)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Thermocline Depth (m)")+theme(axis.text=element_text(color="black",size=10),axis.title=element_text(size=12))

thermo_plot<-grid.arrange(thermo_f, thermo_b, nrow=2)
ggsave("FigureS2_thermdepth.png", thermo_plot, width=7, height=6, units="in", dpi=300)

#### Figure S3 - Lake Number over time by pond ---------------------------------------------------------------------------------------------------------------------
# manual Lake Number script (R. Ladwig)
# use wtrF, wtrB, bathy loaded for thermocline depth calculations

wnd<-load.ts("wind2_wide.txt") # load wind data 

Ao<-400 # area of ponds
wnd.height<-4 # height of wind sensor above the ponds

# ts.lake.number
lake_no <- ts.lake.number(wtrF, wnd, wnd.height, bathy, seasonal=TRUE)
plot(lake_no, ylab='Lake Number', xlab='Date')
lake_noB<-ts.lake.number(wtrB, wnd, wnd.height, bathy, seasonal=TRUE)
plot(lake_noB, ylab='Lake Number', xlab='Date')

# manual lake number 
# Pond F (Reference Pond)
df = wtrF 
idx = match(wnd$datetime,df$datetime)
df = df[idx,]

ssi <-ts.schmidt.stability(df, bathy)
plot(ssi, ylab='Schmidt stability', xlab='Date')
zv <- bathy$depths %*% bathy$areas / sum(bathy$areas )

metaDeps <- ts.meta.depths(wtr = df)
epi.temp <- ts.layer.temperature(wtr = df, top = 0, bottom = metaDeps$top, bathy = bathy, na.rm = T)
hypo.temp <- ts.layer.temperature(wtr = df, top = metaDeps$top, bottom = max(bathy$depths), bathy = bathy, na.rm = T)
wnd_shear <- 1.310e-3 * 1.4310e-3 * wnd$wnd^2
wnd_shear <- uStar(wndSpeed = wnd$wnd, wndHeight = wnd.height, 
                   averageEpiDense = water.density(epi.temp$layer.temp))

bthA = bathy$areas
bthD = bathy$depths
uStar = sqrt(wnd_shear)
St = ssi
metaT = metaDeps$top
metaB = metaDeps$bottom
averageHypoDense = water.density(hypo.temp$layer.temp) 
g	<-	9.81
dz	<-	0.1
# if bathymetry has negative values, remove.
# intepolate area and depth to 0
Ao	<-	bthA[1]
Zo	<-	bthD[1]
if (Ao==0){stop('Surface area cannot be zero, check *.bth file')}
#interpolates the bathymetry data
layerD	<-	seq(Zo,max(bthD),dz)
layerA	<-	stats::approx(bthD,bthA,layerD)$y
#find depth to the center of volume
Zv = layerD*layerA*dz                    
Zcv = sum(Zv)/sum(layerA)/dz

St_uC = St$schmidt.stability*Ao/g
# Calculates the Lake Number according to the formula provided
Ln = g*St_uC*(metaT+metaB)/(2*averageHypoDense*uStar^2*Ao^(3/2)*Zcv)

df.ln <- data.frame('datetime' = df$datetime,
                    'lakenumber' = Ln)



# Pond B (Nutrient Addition Pond)
df = wtrB
idx = match(wnd$datetime,df$datetime)
df = df[idx,]

ssi <-ts.schmidt.stability(df, bathy)
plot(ssi, ylab='Schmidt stability', xlab='Date')
zv <- bathy$depths %*% bathy$areas / sum(bathy$areas )

metaDeps <- ts.meta.depths(wtr = df)
epi.temp <- ts.layer.temperature(wtr = df, top = 0, bottom = metaDeps$top, bathy = bathy, na.rm = T)
hypo.temp <- ts.layer.temperature(wtr = df, top = metaDeps$top, bottom = max(bathy$depths), bathy = bathy, na.rm = T)
wnd_shear <- 1.310e-3 * 1.4310e-3 * wnd$wnd^2
wnd_shear <- uStar(wndSpeed = wnd$wnd, wndHeight = wnd.height, 
                   averageEpiDense = water.density(epi.temp$layer.temp))

bthA = bathy$areas
bthD = bathy$depths
uStar = sqrt(wnd_shear)
St = ssi
metaT = metaDeps$top
metaB = metaDeps$bottom
averageHypoDense = water.density(hypo.temp$layer.temp) 
g	<-	9.81
dz	<-	0.1
# if bathymetry has negative values, remove.
# intepolate area and depth to 0
Ao	<-	bthA[1]
Zo	<-	bthD[1]
if (Ao==0){stop('Surface area cannot be zero, check *.bth file')}
#interpolates the bathymetry data
layerD	<-	seq(Zo,max(bthD),dz)
layerA	<-	stats::approx(bthD,bthA,layerD)$y
#find depth to the center of volume
Zv = layerD*layerA*dz                    
Zcv = sum(Zv)/sum(layerA)/dz

St_uC = St$schmidt.stability*Ao/g
# Calculates the Lake Number according to the formula provided
Ln = g*St_uC*(metaT+metaB)/(2*averageHypoDense*uStar^2*Ao^(3/2)*Zcv)

df.lnB <- data.frame('datetime' = df$datetime,
                    'lakenumber' = Ln)



# add DOY and join LN data frames from each pond
#    Pond F
df.ln$doy<-yday(df.ln$datetime)
df.ln$doy_frac<-hour(df.ln$datetime)
df.ln$min<-minute(df.ln$datetime)
df.ln$minfrac[df.ln$min=="30"] <- 0.5
df.ln$minfrac[df.ln$min=="0"] <- 0
df.ln$hourfrac<-(df.ln$doy_frac + df.ln$minfrac)/24

df.ln$doy_frac<-df.ln$doy+df.ln$hourfrac
LN_F<-df.ln %>% 
  select(datetime,doy,doy_frac,lakenumber)
LN_F$pond<-"F"

#    Pond B
df.lnB$doy<-yday(df.lnB$datetime)
df.lnB$doy_frac<-hour(df.lnB$datetime)
df.lnB$min<-minute(df.lnB$datetime)
df.lnB$minfrac[df.lnB$min=="30"] <- 0.5
df.lnB$minfrac[df.lnB$min=="0"] <- 0
df.lnB$hourfrac<-(df.lnB$doy_frac + df.lnB$minfrac)/24

df.lnB$doy_frac<-df.lnB$doy+df.lnB$hourfrac
LN_B<-df.lnB %>% 
  select(datetime,doy,doy_frac,lakenumber)
LN_B$pond<-"B"

LN_BOTH<-rbind(LN_F, LN_B) # bind LN dataframes for both ponds together

LN_daily<-LN_BOTH %>% #calculate daily mean LN
  group_by(pond, doy) %>% 
  summarize(mean_LN=mean(lakenumber))

ggplot(data=LN_BOTH, aes(x=doy_frac, y=lakenumber, group=pond))+
  geom_line(aes(color=pond),size=1)+xlim(143,233)+  scale_y_continuous(trans='log10') +
  geom_hline(yintercept=1, linetype = 2) +
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Lake Number")+theme(axis.text=element_text(color="black",size=12),axis.title=element_text(size=14))

ggplot(data=LN_daily, aes(x=doy, y=mean_LN, group=pond))+
  geom_line(aes(color=pond),size=1)+xlim(143,233)+  scale_y_continuous(trans='log10') +
  geom_hline(yintercept=1, linetype = 2) +
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Lake Number")+theme(axis.text=element_text(color="black",size=12),axis.title=element_text(size=14))

#### Figure S3 -----------------------------------------------------------------------------------------------------------------------------
#lake number pond F
LNF_plot<-
  ggplot(data=LN_F, aes(x=doy_frac, y=lakenumber))+
  geom_line(color=pondF_col_dark,size=1)+scale_y_continuous(trans='log10') +
  xlim(143,233)+
  geom_hline(yintercept=1, linetype = 2) +
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab(" ") + ylab("Lake Number")+theme(axis.text=element_text(color="black",size=9),axis.title=element_text(size=11),
                                        axis.title.x=element_blank(),axis.text.y=element_text(angle=90,hjust=0.5))
#lake number pond B
LNB_plot<-
  ggplot(data=LN_B, aes(x=doy_frac, y=lakenumber))+
  geom_line(color=pondB_col_dark,size=1)+xlim(143,233)+  scale_y_continuous(trans='log10') +
  geom_hline(yintercept=1, linetype = 2) +
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Lake Number")+theme(axis.text=element_text(color="black",size=9),axis.title=element_text(size=11),
                                                        axis.text.y=element_text(angle=90,hjust=0.5))

LN_full<-grid.arrange(LNF_plot, LNB_plot, nrow=2)
ggsave("LakeNumber.png",LN_full,width=8, height=6, unit="in", dpi=300)


#### Figure S4 - Time series of bottom water temperatures by station, pond ----------------------------------------------------------------------
# Data - need high freq. bottom water temperature (use "temp" df)

# subset for bottom temperature sensors only
temp_2m_deep<-subset(temp, temp_depth_m==2 & site_id==19)
temp_2m_middle<-subset(temp, temp_depth_m==2 & site_id==20)
temp_1.5m_shallow<-subset(temp, temp_depth_m==1.5 & site_id==21)

F_middle_deepTemp<-subset(temp_2m_middle, pond=="F" & doy>184 & doy<213)
max(F_middle_deepTemp$temp_c) #22.525
B_middle_deepTemp<-subset(temp_2m_middle, pond=="B" & doy>184 & doy<213)
max(B_middle_deepTemp$temp_c) #25.02778
# POND F UP TO 2.5 DEGREES COOLER FOLLOWING HEATWAVE, WHILE MACROPHYTES PRESENT

# calculate daily average bottom temp. at each site
# use values to note change in daily mean over heatwave
temp_daily_2m_deep<-temp_2m_deep %>% 
  group_by(doy, pond) %>% 
  mutate(daily_temp=mean(temp_c), dailymax=max(temp_c), dailymin=min(temp_c), daily_range=dailymax-dailymin) %>% 
  select(doy, pond, daily_temp, dailymax, dailymin, daily_range) %>% 
  slice(n=1) %>% 
  ungroup()

temp_daily_2m_mid<-temp_2m_middle %>% 
  group_by(doy, pond) %>% 
  mutate(daily_temp=mean(temp_c)) %>% 
  select(doy, pond, daily_temp) %>% 
  slice(n=1) %>% 
  ungroup()

temp_daily_1.5m_shallow<-temp_1.5m_shallow %>% 
  group_by(doy, pond) %>% 
  mutate(daily_temp=mean(temp_c)) %>% 
  select(doy, pond, daily_temp) %>% 
  slice(n=1) %>% 
  ungroup()

# FIGURE S4 -----------------------------------------------------------------------------------------------------
# plot hf bottom temp - can see more daily variation without plants!
# deep site
bottomtemp_deep<-
  ggplot(data=temp_2m_deep, aes(x=doy_frac, y=temp_c, group=pond))+
  geom_rect(aes(xmin=185, xmax=190, ymin=-Inf, ymax=Inf), color=NA, fill="grey80")+
  geom_line(aes(color=pond),size=1.1)+xlim(143,234)+ylim(14,26)+
  scale_color_manual(values=c(pondB_col, pondF_col))+
  geom_vline(xintercept=176,color=pondB_col_transparent,linetype="dashed",size=0.8)+
  geom_vline(xintercept=211,color=pondB_col_transparent,linetype="dashed",size=0.8)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") +theme(axis.text.x=element_text(color="black",size=9),axis.title.x=element_text(size=11),
                             axis.text.y=element_blank(), axis.title.y = element_blank())+
  theme(legend.title=element_blank(), legend.position = "none",legend.text = element_text(size=10))

# middle site
bottomtemp_mid<-
  ggplot(data=temp_2m_middle, aes(x=doy_frac, y=temp_c, group=pond))+
  geom_rect(aes(xmin=185, xmax=190, ymin=-Inf, ymax=Inf), color=NA, fill="grey80")+
  geom_line(aes(color=pond),size=1.1)+ylim(14,26)+xlim(143,234)+
  scale_color_manual(values=c(pondB_col, pondF_col))+
  geom_vline(xintercept=176,color=pondB_col_transparent,linetype="dashed",size=0.8)+
  geom_vline(xintercept=211,color=pondB_col_transparent,linetype="dashed",size=0.8)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year")+theme(axis.text.x=element_text(color="black",size=9),axis.title.x=element_text(size=11),
                            axis.text.y=element_blank(), axis.title.y = element_blank())+
  theme(legend.title=element_blank(), legend.position = "none",legend.text = element_text(size=10))

# shallow site
bottomtemp_shallow<-
  ggplot(data=temp_1.5m_shallow, aes(x=doy_frac, y=temp_c, group=pond))+
  geom_rect(aes(xmin=185, xmax=190, ymin=-Inf, ymax=Inf), color=NA, fill="grey80")+
  geom_line(aes(color=pond),size=1.1)+ylim(14,26)+xlim(143,234)+
  scale_color_manual(values=c(pondB_col, pondF_col))+
  geom_vline(xintercept=176,color=pondB_col_transparent,linetype="dashed",size=0.8)+
  geom_vline(xintercept=211,color=pondB_col_transparent,linetype="dashed",size=0.8)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Bottom Temperature (?C)")+theme(axis.text=element_text(color="black",size=9),axis.title=element_text(size=11))+
  theme(legend.title=element_blank(), legend.position = "none",legend.text = element_text(size=10))


bottom_temp_fig<-grid.arrange(bottomtemp_shallow, bottomtemp_mid, bottomtemp_deep,
                              layout_matrix = rbind(c(1, 2, 3)))
ggsave("FigureS4.png", bottom_temp_fig, width=8, height=3, units="in", dpi=300)

#### Figure S5 - Time series of surface, bottom, a delta-dissolved oxygen saturation ----------------------------------------------------------------------
# use manual profile, dissolved oxygen data ("do")
# already subset for surface ("DO_surface") and bottom ("DO_bottom") waters

# calculate "delta-DO" as the difference between surface and bottom water DO concentration, saturation
delta_DO<-merge(x=DO_surface, y=DO_bottom, by=c("doy","pond", "site_id"), all.x=TRUE) # x is surface, y is bottom
delta_DO$delta_DO_mgL<-delta_DO$odo_mgL.x-delta_DO$odo_mgL.y # delta calculated as surface-bottom
delta_DO$delta_DO_sat<-delta_DO$odo_sat.x-delta_DO$odo_sat.y

deltaDO<-delta_DO %>% 
  select(doy,pond,site_id,delta_DO_mgL,delta_DO_sat)

### explore DO differences by pond - gam smoothing
DO_hypo_F<-subset(DO_bottom, pond=="F")
DO_hypo_B<-subset(DO_bottom, pond=="B")
DO_epi_F<-subset(DO_surface, pond=="F")
DO_epi_B<-subset(DO_surface, pond=="B")
DO_delta_F<-subset(delta_DO, pond=="F")
DO_delta_B<-subset(delta_DO, pond=="B")

# Bottom DO GAMs
hypoDO_F_gam <- gam(odo_sat ~ s(doy, k = 10), 
                    data = DO_hypo_F, method = 'REML')
summary(hypoDO_F_gam)
gam.check(hypoDO_F_gam)
hypoDO_B_gam <- gam(odo_sat ~ s(doy, k = 10), 
                    data = DO_hypo_B, method = 'REML')
summary(hypoDO_B_gam)

# Surface DO GAMS
epiDO_F_gam <- gam(odo_sat ~ s(doy, k = 10), 
                   data = DO_epi_F, method = 'REML')
summary(epiDO_F_gam)
epiDO_B_gam <- gam(odo_sat ~ s(doy, k = 10), 
                   data = DO_epi_B, method = 'REML')
summary(epiDO_B_gam)

# Delta DO GAMS
deltaDO_F_gam <- gam(delta_DO_sat ~ s(doy, k = 10), 
                     data = DO_delta_F, method = 'REML')
summary(deltaDO_F_gam)
deltaDO_B_gam <- gam(delta_DO_sat ~ s(doy, k = 10), 
                     data = DO_delta_B, method = 'REML')
summary(deltaDO_B_gam)


# Figure S5A - surface gam
surface_do_plot<-
ggplot(data=DO_epi_B, aes(x=doy, y=odo_sat))+
  geom_hline(yintercept=100,color="grey50",linetype="solid",size=0.8)+
  geom_rect(aes(xmin=185, xmax=190, ymin=-Inf, ymax=Inf), color=NA, fill="grey80")+
  geom_point(color=pondB_col, size=3)+
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10), size = 1, color=pondB_col, fill=pondB_col_transparent)+
  ylim(0,250)+xlim(143,240)+
  new_scale_color() +
  geom_point(data=DO_epi_F, aes(x=doy, y=odo_sat),color=pondF_col,size=3,shape=15)+
  geom_smooth(data=DO_epi_F, aes(x=doy, y=odo_sat),method = "gam", formula = y ~ s(x, k = 10), size = 1, color=pondF_col, fill=pondF_col_transparent)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  geom_vline(xintercept=176,color=pondB_col_transparent,linetype="dashed",size=0.8)+
  geom_vline(xintercept=211,color=pondB_col_transparent,linetype="dashed",size=0.8)+
  xlab("Day of Year") + ylab(bquote("Surface Water DO (%)"))+theme(axis.text=element_text(color="black",size=9),axis.title=element_text(size=11))

# Figure S5B - bottom gam
bottom_do_plot<-
ggplot(data=DO_hypo_B, aes(x=doy, y=odo_sat))+
  geom_hline(yintercept=100,color="grey50",linetype="solid",size=0.8)+
  geom_rect(aes(xmin=185, xmax=190, ymin=-Inf, ymax=Inf), color=NA, fill="grey80")+
  geom_point(color=pondB_col, size=3)+
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10), size = 1, color=pondB_col, fill=pondB_col_transparent)+
  ylim(0,250)+xlim(143,240)+
  new_scale_color() +
  geom_point(data=DO_hypo_F, aes(x=doy, y=odo_sat),color=pondF_col,size=3,shape=15)+
  geom_smooth(data=DO_hypo_F, aes(x=doy, y=odo_sat),method = "gam", formula = y ~ s(x, k = 10), size = 1, color=pondF_col, fill=pondF_col_transparent)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  geom_vline(xintercept=176,color=pondB_col_transparent,linetype="dashed",size=0.8)+
  geom_vline(xintercept=211,color=pondB_col_transparent,linetype="dashed",size=0.8)+
  xlab("Day of Year") + ylab(bquote("Bottom Water DO (%)"))+theme(axis.text=element_text(color="black",size=9),axis.title=element_text(size=11))

# Figure S5C - delta gam
delta_do_plot<-
  ggplot(data=DO_delta_B, aes(x=doy, y=delta_DO_sat))+
  geom_hline(yintercept=0,color="grey50",linetype="solid",size=0.8)+
  geom_rect(aes(xmin=185, xmax=190, ymin=-Inf, ymax=Inf), color=NA, fill="grey80")+
  geom_point(color=pondB_col, size=3)+
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10), size = 1, color=pondB_col, fill=pondB_col_transparent)+
  ylim(-65,40)+xlim(143,240)+
  new_scale_color() +
  geom_point(data=DO_delta_F, aes(x=doy, y=delta_DO_sat),color=pondF_col,size=3,shape=15)+
  geom_smooth(data=DO_delta_F, aes(x=doy, y=delta_DO_sat),method = "gam", formula = y ~ s(x, k = 10), size = 1, color=pondF_col, fill=pondF_col_transparent)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  geom_vline(xintercept=176,color=pondB_col_transparent,linetype="dashed",size=0.8)+
  geom_vline(xintercept=211,color=pondB_col_transparent,linetype="dashed",size=0.8)+
  xlab("Day of Year") + ylab(bquote("Surface - Bottom Water DO (%)"))+theme(axis.text=element_text(color="black",size=9),axis.title=element_text(size=11))

DO_SI_FULL<-grid.arrange(surface_do_plot, bottom_do_plot, delta_do_plot, nrow=2)
ggsave("FigureS5DO.png", DO_SI_FULL, width=8, height=7.5, units="in", dpi=300)

