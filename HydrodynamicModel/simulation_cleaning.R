library(tidyverse)
library(gridExtra)
library(cowplot)


# No macrophytes ----------------------------------------------------------------------
no.mac<-read.csv("model_out_NOMAC_NEW.csv")
# convert "variable" depths to numeric values and rename
no.mac_test<-no.mac %>% 
  dplyr::rename(depth_m=variable) %>% 
  mutate(depth_m=recode(depth_m, "0 m"=0, "0.25 m"=0.25, "0.5 m"=0.5, "0.75 m"=0.75, "1 m"=1, "1.25 m"=1.25, "1.5 m"=1.5, "2 m"=2))

## interpolate missing depths ----------------------------------------
library(reshape)
#doy_frac, depth_m, temp_c

### Grab variables we need
Bmid<- no.mac_test[c("doy_frac", "depth_m", "temp_c")]

### Melt so that cast will recognize the data frame
Bmelt_mid<- melt(Bmid, id=c("doy_frac", "depth_m"))

### Now cast so that we have columns of temp for each depth
Bcast_mid<- cast(Bmelt_mid, doy_frac~depth_m)

### create dataframe (definitely easier way to do this but it works)
Bcast_mid2<-data.frame(Bcast_mid)

### add whatever depths you want to (as the numeric value here)
Bcast_mid2$X1.75<- 0

### calculate however you need to
Bcast_mid2$X1.75<- rowMeans(Bcast_mid2[c("X1.5","X2")],)

### now remelt to long format
no_mac_inter_final<- melt(Bcast_mid2, id=c("doy_frac"))

### I lost your column names, Grab just the numbers from this character
no_mac_inter_final$depth_m <- substr(no_mac_inter_final$variable, 2, 9)

### And convert to numeric
no_mac_inter_final$depth_m <- as.numeric(no_mac_inter_final$depth_m)
no_mac_inter_final$temp_c<- as.numeric(no_mac_inter_final$value)
# no_mac_inter_final

# Model results from field data--------------------------------------------------------------------------
nat.mac<-read.csv("model_out_F19_NEW.csv")

nat.mac_test<-nat.mac %>% 
  dplyr::rename(depth_m=variable) %>% 
  mutate(depth_m=recode(depth_m, "0 m"=0, "0.25 m"=0.25, "0.5 m"=0.5, "0.75 m"=0.75, "1 m"=1, "1.25 m"=1.25, "1.5 m"=1.5, "2 m"=2))

## interpolate missing values--------------------------------------------------
library(reshape)
### Grab variables we need
Bdeep2<- nat.mac_test[c("doy_frac", "depth_m", "temp_c")]

### Melt so that cast will recognize the data frame
Bmelt_deep2<- melt(Bdeep2, id=c("doy_frac", "depth_m"))

### Now cast so that we have columns of temp for each depth
Bcast_deep2<- cast(Bmelt_deep2, doy_frac~depth_m)

### create dataframe (definitely easier way to do this but it works)
Bcast_deep3<-data.frame(Bcast_deep2)

### add whatever depths you want to (as the numeric value here)
Bcast_deep3$X1.75<- 0

### calculate however you need to
Bcast_deep3$X1.75<- rowMeans(Bcast_deep3[c("X1.5","X2")],)

### now remelt to long format
nat_mac_inter_final<- melt(Bcast_deep3, id=c("doy_frac"))

### I lost your column names, Grab just the numbers from this character
nat_mac_inter_final$depth_m <- substr(nat_mac_inter_final$variable, 2, 9)

### And convert to numeric
nat_mac_inter_final$depth_m <- as.numeric(nat_mac_inter_final$depth_m)
nat_mac_inter_final$temp_c<- as.numeric(nat_mac_inter_final$value)
#nat_mac_inter_final

## bottom water only --------------------------------------------
no.mac.2<-no.mac %>% 
  filter(variable=="2 m")
no.mac.2$group<-"No Macrophytes"
no.mac.2<-no.mac.2 %>% 
  select(group,doy_frac,temp_c)

nat.mac.2<-nat.mac %>% 
  filter(variable=="2 m")
nat.mac.2$group<-"Macrophytes"
nat.mac.2<-nat.mac.2 %>% 
  select(group,doy_frac,temp_c)


# BIND TOGETHER ------------------------------------------------------------------------------------------------
mac<-rbind(no.mac.2, nat.mac.2)


# plot of bottom water temperature ------------------------------------------------------------------------------
bottom_temp<-
ggplot(data=mac, aes(x=doy_frac, y=temp_c, group=group))+
  geom_rect(aes(xmin=185, xmax=190, ymin=-Inf, ymax=Inf), color=NA, fill="grey80")+
  geom_line(aes(col=group), size=1)+
  scale_color_manual(values=c("grey10","deeppink3"))+
  xlim(143,241)+ylim(10, 35)+
  theme_linedraw()  + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  ylab("Temperature (°C)")+xlab("Day of Year")+theme(legend.position="none")

tapply(mac$temp_c, mac$group, mean)
tapply(mac$temp_c, mac$group, max)

### HEATMAPS

# no macrophytes heatmap
ggplot(no_mac_inter_final, aes(doy_frac, depth_m, fill= temp_c)) + 
  geom_tile()+ylim(c(2.15,-0.15))+
  geom_segment(aes(x = 185, y = -0.15, xend = 190, yend = -0.15), size=1.2, col="black")+
  geom_segment(aes(x = 185, y = 2.15, xend = 190, yend = 2.15), size=1.2, col="black")+
  xlim(143,233)+
  scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11, 'Spectral')),name="Temperature (C)",
                       limits=c(14,38),)+
  theme_classic()+theme(plot.margin=margin(t=0,r=0, b=0, l=2, unit="pt"))+
  xlab(" ") + ylab("Depth (m)")+theme(axis.text=element_text(color="black",size=8),legend.position="none",axis.title=element_text(size=10))
no_mac_heat<-
ggplot(no_mac_inter_final, aes((doy_frac), depth_m)) +
  geom_tile(aes(fill = as.numeric(temp_c))) +
  scale_fill_gradientn(limits = c(14,38),
                       colours = rev(RColorBrewer::brewer.pal(11, 'Spectral')))+
  theme_linedraw()  + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  geom_segment(aes(x = 185, y = -0.15, xend = 190, yend = -0.15), size=1.2, col="black")+
  geom_segment(aes(x = 185, y = 2.15, xend = 190, yend = 2.15), size=1.2, col="black")+
  xlab(' ') + 
  ylab('Depth (m)') + theme(legend.position="top")+
  labs(fill = 'Temperature (C)')+  scale_y_reverse() 

no_mac_heat_final<-
  ggplot(no_mac_inter_final, aes((doy_frac), depth_m)) +
  geom_tile(aes(fill = as.numeric(temp_c))) +
  scale_fill_gradientn(limits = c(14,38),
                       colours = rev(RColorBrewer::brewer.pal(11, 'Spectral')))+
  theme_linedraw()  + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  geom_segment(aes(x = 185, y = -0.15, xend = 190, yend = -0.15), size=1.2, col="black")+
  geom_segment(aes(x = 185, y = 2.15, xend = 190, yend = 2.15), size=1.2, col="black")+
  xlab(' ') + 
  ylab('Depth (m)') + theme(legend.position="none")+
  labs(fill = 'Temperature (C)')+  scale_y_reverse() 

legend_heat<-get_legend(no_mac_heat)


# model results from field data heatmap
nat_mac_heat<-
ggplot(nat_mac_inter_final, aes((doy_frac), depth_m)) +
  geom_tile(aes(fill = as.numeric(temp_c))) +
  scale_fill_gradientn(limits = c(14,38),
                       colours = rev(RColorBrewer::brewer.pal(11, 'Spectral')))+
  theme_linedraw()  + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  geom_segment(aes(x = 185, y = -0.15, xend = 190, yend = -0.15), size=1.2, col="black")+
  geom_segment(aes(x = 185, y = 2.15, xend = 190, yend = 2.15), size=1.2, col="black")+
  xlab(' ') + 
  ylab('Depth (m)') + theme(legend.position="none")+
  labs(fill = 'Temperature (C)')+  scale_y_reverse() 

full_mac_sim<-
  grid.arrange(nat_mac_heat, no_mac_heat_final, bottom_temp, nrow=3)
ggsave("mac_simulation_NEW.png", full_mac_sim, width=6, height=10, unit="in", dpi=300)


