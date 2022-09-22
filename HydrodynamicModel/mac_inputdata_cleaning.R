# script to document how raw macrophyte canopy height and biomass data were cleaned/normalized for model input

# read in all canopy and biomass data
mac<-read.csv("biomass_canopy_2020.csv")
# read in manual DO profile data table (includes water depth for each biomass sampling site and event!)
DO<-read.csv("DO_profiles_2020.csv")

### BIOMASS --------------------------------------------------------------------------------------------------
# subset to work with canopy and biomass data separately (different spatial resolution)
bio<-subset(mac, site_type == "Biomass")
bio<-bio %>% 
  mutate(doy.f=factor(doy))

# calculate biomass density (g DW per m3 water) - this normalization is needed for model input!
# Use manual sensor profiles (df=DO) pull maximum water depth for each sampling site and point
water_depth<-DO %>% 
  group_by(doy, pond, site_id) %>% 
  summarize(across(c(vertical_m), ~max(.,na.rm=T))) %>% 
  ungroup()

# join water depths from manual profiles sites (above) to biomass data frame
bio_depth<-merge(x=bio, y=water_depth, by=c("doy","pond", "site_id"), all.x=TRUE)
# calculate the volume of water (so water depth, vertical_m, times the area of sediment in the rake pull 0.4m2)
bio_depth$volume<-bio_depth$vertical_m*0.4
# calculate biomass per volume water (g DW/ m3 water)
bio_depth$biomass_gm3<-bio_depth$biomass_g/bio_depth$volume

biomass_z<-bio_depth %>% 
  select(doy, pond, site_id, p_foliosus, p_nodosus, biomass_g, vertical_m, volume, biomass_gm3)

# so working with biomass_z
biomass_F19<-subset(biomass_z, site_id=="F5"|site_id=="F2")
biomass_F19<-biomass_F19 %>% 
  group_by(pond, doy) %>% 
  summarise(across(c(biomass_gm3), ~mean(.,na.rm=T))) %>% 
  rename(mean_biomass = biomass_gm3) %>% 
  select(doy, mean_biomass, pond) %>% 
  ungroup()
write.csv(biomass_F19, "biomass_F19.csv", row.names=FALSE)

biomass_B19<-subset(biomass_z, site_id=="B5"|site_id=="B2")
biomass_B19<-biomass_B19 %>% 
  group_by(pond, doy) %>% 
  summarise(across(c(biomass_gm3), ~mean(.,na.rm=T))) %>% 
  dplyr::rename(mean_biomass = biomass_gm3) %>% 
  select(doy, mean_biomass, pond) %>% 
  ungroup()


### CANOPY -----------------------------------------------------------------------------------------------
canopy<-subset(mac, site_type=="Canopy")
canpy_b19<-subset(canopy, site_id=="B19")
canopy_f<-subset(canopy, pond=="F")
canopy_f<-canopy_f %>% 
  mutate(doy.f=factor(doy))

canopy_fmean<-canopy_f %>% 
  group_by(doy.f) %>% 
  summarise(across(c(canopy_m), ~mean(.,na.rm=T))) %>% 
  rename(mean_canopy=canopy_m) %>% 
  select(doy.f, mean_canopy) %>% 
  ungroup()
write.csv(canopy_fmean, "canopy_F_pondmean.csv", row.names=FALSE)


canopy_F19<-subset(canopy_f, site_id=="F19")
canopy_F19<-canopy_F19 %>% 
  select(doy.f,pond,site_id,canopy_m) %>% 
  rename(mean_canopy=canopy_m)
canopy_F19[canopy_F19==0]<-0.5 #replace 0 values at end of time series with 0.5 - more accurate to plant coverage near sensor, not captured in manual sampling
write.csv(canopy_F19, "canopy_F19.csv", row.names=FALSE)
