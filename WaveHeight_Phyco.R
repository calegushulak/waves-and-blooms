# Buoy by Hilary Dugan, updated 2024-06-13

## Wave height and period calculation for high res wind speed and direction data
library(tidyverse)
library(lubridate)

# Download Mendota high-frequency data from Environmental Data Initiative 
# North Temperate Lakes LTER: High Frequency Data: Meteorological, Dissolved Oxygen, Chlorophyll, Phycocyanin - Lake Mendota Buoy 2006 - current
# https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-ntl&identifier=129
inUrl3  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/129/36/af29c64a7de5ad797b5709b5f7718cb9" 
infile3 <- tempfile()
download.file(inUrl3,infile3,method="curl")
buoy.hf <- read_csv(infile3) 

# Create hourly dataframe where wind direction is averaged correctly 
buoy.hour = buoy.hf |> 
  mutate(datetime = ymd_hms(paste(sampledate,sampletime))) |> 
  select(datetime, sampledate, phyco = phyco_rfu, chl = chlor_rfu, 
         wdir = wind_dir, wspd = wind_speed, 
         turb = turbidity, do = do_raw, par = par) |> 
  mutate(Uu = wspd * sin(2 * pi * wdir/360)) |> 
  mutate(Vv = wspd * cos(2 * pi * wdir/360)) |> 
  mutate(Hour = hour(datetime)) |> 
  group_by(sampledate, Hour) |> 
  summarise_all(mean) |> 
  mutate(wdir = as.vector(atan2(Uu,Vv) * 360/2/pi)) |> 
  mutate(wdir = if_else(wdir < 0, wdir+360, wdir)) #if <0 add 360 
  

### Now add fetch values based on buoy location
### fetch is in meters
buoy.hour <- buoy.hour %>%
  mutate(Fetch_m = case_when(wdir >= 0 & wdir < 22.5 ~ 5613.7, # N-S 1
                             wdir >= 22.5 & wdir < 67.5 ~ 4266.8, # NE-SW
                             wdir >= 67.5 & wdir < 112.5 ~ 9192.8, # E-W
                             wdir >= 112.5 & wdir < 157.5 ~ 6472.9, # SE-NW
                             wdir >= 157.5 & wdir < 202.5 ~ 5613.7, # S-N
                             wdir >= 202.5 & wdir < 247.5 ~ 4266.8, # SW-NE
                             wdir >= 247.5 & wdir < 292.5 ~ 9192.8, # W-E
                             wdir >= 292.5 & wdir < 337.5 ~ 6472.9, # NW-SE
                             wdir >= 337.5 & wdir <= 360 ~ 5613.7)) # N-S 2


### now we determine if waves are fetch limited
### Nimish eq II-2-35
buoy.hour <- buoy.hour %>%
  mutate(tlim.s = 77.23 * ((Fetch_m^0.67)/((wspd^0.34)*(9.81^0.33)))) %>% ## SI unit 
  mutate(tlim.hr = tlim.s / 3600)
### all are duration limited

### calculate drag coefficient
buoy.hour <- buoy.hour %>%
  mutate(CD = 0.001*(1.1+(0.035*wspd))) ## not U10, just wind speed

### calculate friction velocity
## rearrange equation fr.vel= sqrt(CU^2)
buoy.hour <- buoy.hour %>%
  mutate(fr.vel.ms = sqrt(CD * (wspd^2)))

### now calc effective fetch using fr.vel and 1 hour durations
## e. II-2-38
## corrected by Ben
buoy.hour <- buoy.hour %>%
  mutate(Ef.fetch = 5.23e-3 * sqrt(fr.vel.ms*9.81*(3600^3)))

## Wave height
buoy.hour <- buoy.hour %>%
  mutate(H.m = 4.13e-2 * fr.vel.ms * sqrt(Ef.fetch/9.81)) %>%
  mutate(H.cm = H.m*100)

### plot wave height hist. and density
ggplot(buoy.hour, aes(x=H.cm))+
  geom_histogram(aes(y=..density..), binwidth = 1,
                 color="black", fill="grey")+
  geom_density(alpha=.25, fill="blue")+
  ylab("Density")+
  xlab("Estimated wave height (cm)")+
  #ggtitle("Lake Mendota high res summer 2012-2022")+
  theme_bw()

## Wave Period 
buoy.hour <- buoy.hour %>%
  mutate(T.sec = 0.651 * (((fr.vel.ms*Ef.fetch)/(9.81^2))^0.33))

### Plot waver period hist. and density
ggplot(buoy.hour, aes(T.sec))+
  geom_histogram(aes(y=..density..), binwidth = 0.1,
                 color="black", fill="grey")+
  geom_density(alpha=.25, fill="blue")+
  ylab("Density")+
  xlab("Estimated wave period (s)")+
  #ggtitle("Lake Mendota high res summer 2012-2022")+
  theme_bw()

######################################################################
# scale chl and phyco
buoy.hour = buoy.hour |> 
  mutate(year = year(sampledate)) |> 
  group_by(year) |> 
  mutate(chl = scale(chl)[,1], phyco = scale(phyco)[,1]) |> 
  ungroup()

# calculate differences 
buoy.diff = buoy.hour |> 
  dplyr::filter(month(sampledate) == 7, year >= 2015) |> # limit to July post 2015
  filter(hour(datetime) %in% (10:17)) |> # average between 10 am and 5 pm
  group_by(sampledate, year) |> 
  summarise_all(mean) |> 
  ungroup() |> 
  mutate(phycoDiff = c(NA, diff(phyco))) |> 
  mutate(wspdDiff = c(NA, diff(wspd))) |> 
  mutate(waveDiff = c(NA, diff(H.cm))) |> 
  mutate(windGroup = if_else(wspd > 5, '> 5 m/s', '< 5 m/s'))

ggplot(buoy.diff |> filter(!is.na(H.cm))) +
  geom_smooth(aes(x = H.cm, phycoDiff), col = 'lightblue4', method = 'lm') +
  geom_point(aes(x = H.cm, phycoDiff, fill = windGroup), shape = 21, stroke = 0.2) +
  scale_fill_manual(values = c('#96b59a', 'lightblue4'), name = 'Wind Speed') +
  xlab('Wave height (cm)') +
  ylab('Daily difference in phycocyanin [scaled]') +
  theme_bw(base_size = 9)

ggsave('Figures/waveheight_phyco.png', width = 6, height = 3, units = 'in', dpi = 500)
