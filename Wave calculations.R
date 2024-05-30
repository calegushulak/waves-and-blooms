### Using LTER high frequency buoy data to calculate wave height and period
### also plotting for fig. 2
### Need wind speed, wind direction, phyco
library(tidyverse)
library(climaemet)
library(cowplot)

# Download Mendota high-frequency data 
inUrl3  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/129/36/af29c64a7de5ad797b5709b5f7718cb9" 
infile3 <- tempfile()
download.file(inUrl3,infile3,method="curl")
hres <- read_csv(infile3) 

## OR

## read in from file
#### high res Lake Mendoata Buoy data
hres <- read.csv(file.choose(), header=TRUE)

### remove spotty years 2006-2011
### keeping 2012-2022 
hres.yr <- hres %>%
  filter(year4 >= 2012) 

## filter down to summer time
## first convert date to dates with lubridate
hres.yr$date <- ymd(hres.yr$sampledate)

### Yearly summer periods based on Rowher et al. 
hres.12 <-hres.yr %>%
  filter(date >= "2012-06-10" & date < "2012-10-03")

hres.13 <-hres.yr %>%
  filter(date >= "2013-06-18" & date < "2013-10-22")

hres.14 <-hres.yr %>%
  filter(date >= "2014-06-14" & date < "2014-10-12")

hres.15 <-hres.yr %>%
  filter(date >= "2015-06-10" & date < "2015-10-14")

hres.16 <-hres.yr %>%
  filter(date >= "2016-05-30" & date < "2016-10-26")

hres.17 <-hres.yr %>%
  filter(date >= "2017-06-13" & date < "2017-10-21")

hres.18 <-hres.yr %>%
  filter(date >= "2018-06-02" & date < "2018-10-14")

hres.19 <-hres.yr %>%
  filter(date >= "2019-07-03" & date < "2019-10-10")

hres.20 <-hres.yr %>%
  filter(date >= "2020-06-24" & date < "2020-10-11")

hres.21 <-hres.yr %>%
  filter(date >= "2021-06-16" & date < "2021-10-12")

hres.22 <-hres.yr %>%
  filter(date >= "2022-06-14" & date < "2022-10-12")

### merge back together
hres.sum <- rbind(hres.12, hres.13, hres.14, hres.15, hres.16, hres.17,
                  hres.18, hres.19, hres.20, hres.21, hres.22)

##### data specification for below
### hres.yr is all data 2012-2022
### hres.sum is summer data 2012-2022

### select relevant variables
hres.dt <- hres.sum %>%
  select(year4, sampledate, sampletime, wind_speed, wind_dir, phyco_rfu)

### Make Wind rose plot
## drop NAs
hres.d <- hres.dt %>%
  drop_na()

### wind rose speed and direction 
speed <- hres.d$wind_speed
direction <- hres.d$wind_dir

### Plot windrose
mend.sum.wr <- 
  ggwindrose(speed, direction,
             n_directions=16,
             #n_speeds=7,
             speed_cuts=seq(0,21,3),
             calm_wind = 0,
             col_pal="ag_GrnYl")
             #plot_title = "Summer wind patterns: 2012-2022")
mend.sum.wr
### success - this will be panel A of Fig 2


### Back to wave calculations
### add fetch values - based on location of buoy in Lake Mendota
hres.f <- hres.dt %>%
  mutate(Fetch_m = case_when(wind_dir >= 0 & wind_dir < 22.5 ~ 5613.7, # N-S 1
                             wind_dir >= 22.5 & wind_dir < 67.5 ~ 4266.8, # NE-SW
                             wind_dir >= 67.5 & wind_dir < 112.5 ~ 9192.8, # E-W
                             wind_dir >= 112.5 & wind_dir < 157.5 ~ 6472.9, # SE-NW
                             wind_dir >= 157.5 & wind_dir < 202.5 ~ 5613.7, # S-N
                             wind_dir >= 202.5 & wind_dir < 247.5 ~ 4266.8, # SW-NE
                             wind_dir >= 247.5 & wind_dir < 292.5 ~ 9192.8, # W-E
                             wind_dir >= 292.5 & wind_dir < 337.5 ~ 6472.9, # NW-SE
                             wind_dir >= 337.5 & wind_dir <= 360 ~ 5613.7)) # N-S 2


### now we determine if waves are fetch limited
### 
hres.f <- hres.f %>%
  mutate(tlim.s = 77.23 * ((Fetch_m^0.67)/((wind_speed^0.34)*(9.81^0.33)))) %>% ## SI unit 
  mutate(tlim.hr = tlim.s / 3600)
### all are duration limited

### calculate drag coefficient
hres.f <- hres.f %>%
  mutate(CD=0.001*(1.1+(0.035*wind_speed))) ## not U10, just wind speed

### calculate friction velocity
## rearrange equation fr.vel= sqrt(CU^2)
hres.f <- hres.f %>%
  mutate(fr.vel.ms = sqrt(CD * (wind_speed^2)))

### now calc effective fetch using fr.vel and 1 hour durations
## 
hres.f <- hres.f %>%
  mutate(Ef.fetch = 5.23e-3 * sqrt(fr.vel.ms*9.81*(3600^3)))


## Calc Wave height
## rearrange equation
hres.f <- hres.f %>%
  mutate(H.m = 4.13e-2 * fr.vel.ms * sqrt(Ef.fetch/9.81)) %>%
  mutate(H.cm = H.m*100)

## Plot wave height density
hres.wave.hsum <- 
  ggplot(hres.f, aes(x=H.cm))+
  geom_histogram(aes(y=..density..), binwidth = 1,
                 color="black", fill="grey")+
  geom_density(alpha=.25, fill="blue")+
  ylab("Density")+
  xlab("Estimated wave height (cm)")+
  #ggtitle("Lake Mendota high res summer 2012-2022")+
  theme_bw()
hres.wave.hsum
### This is panel C

## Calc Wave Period
hres.f <- hres.f %>%
  mutate(T.sec = 0.651 * (((fr.vel.ms*Ef.fetch)/(9.81^2))^0.33))

## Plot wave period
hres.wave.psum <- 
  ggplot(hres.f, aes(T.sec))+
  geom_histogram(aes(y=..density..), binwidth = 0.1,
                 color="black", fill="grey")+
  geom_density(alpha=.25, fill="blue")+
  ylab("Density")+
  xlab("Estimated wave period (s)")+
  #ggtitle("Lake Mendota high res summer 2012-2022")+
  theme_bw()
hres.wave.psum
### This is panel D 

### save wave calculation results
write.csv(hres.f, "highres_summer_wave_results.csv")

###################################################################################
##### Hourly data 
hres.dt
### this is now summer restricted

### make hours for averaging
hres.hr <- hres.dt %>%
  mutate(sampletime = hms(sampletime),
         hours = hour(sampletime))

### Hilary's code to properly mean wind direction 
hr.wind <- hres.hr %>%
  drop_na(wind_speed, wind_dir) %>%
  mutate(Uu = wind_speed * sin(2 * pi * wind_dir/360)) %>% 
  mutate(Vv = wind_speed * cos(2 * pi * wind_dir/360)) %>%
  group_by(sampledate, hours) %>%
  summarise_all(mean) %>%
  mutate(wind_dir = as.vector(atan2(Uu,Vv) * 360/2/pi)) %>%
  mutate(wind_dir = if_else(wind_dir < 0, wind_dir+360, wind_dir)) # if <0 add 360 


### Same code as above for hourly wave calculations
### now add fetch to hourly data
hr.wind <- hr.wind %>%
  mutate(Fetch_m = case_when(wind_dir >= 0 & wind_dir < 22.5 ~ 5613.7, # N-S 1
                             wind_dir >= 22.5 & wind_dir < 67.5 ~ 4266.8, # NE-SW
                             wind_dir >= 67.5 & wind_dir < 112.5 ~ 9192.8, # E-W
                             wind_dir >= 112.5 & wind_dir < 157.5 ~ 6472.9, # SE-NW
                             wind_dir >= 157.5 & wind_dir < 202.5 ~ 5613.7, # S-N
                             wind_dir >= 202.5 & wind_dir < 247.5 ~ 4266.8, # SW-NE
                             wind_dir >= 247.5 & wind_dir < 292.5 ~ 9192.8, # W-E
                             wind_dir >= 292.5 & wind_dir < 337.5 ~ 6472.9, # NW-SE
                             wind_dir >= 337.5 & wind_dir <= 360 ~ 5613.7)) # N-S 2
### success! 


### now we determine if waves are fetch limited
###
hr.wind <- hr.wind %>%
  mutate(tlim.s = 77.23 * ((Fetch_m^0.67)/((wind_speed^0.34)*(9.81^0.33)))) %>% ## SI unit 
  mutate(tlim.hr = tlim.s / 3600)
### all are duration limited

### calculate drag coefficient
hr.wind <- hr.wind %>%
  mutate(CD=0.001*(1.1+(0.035*wind_speed))) ## not U10, just wind speed

### calculate friction velocity
## rearrange equation fr.vel= sqrt(CU^2)
hr.wind <- hr.wind %>%
  mutate(fr.vel.ms = sqrt(CD * (wind_speed^2)))

### now calc effective fetch using fr.vel and 1 hour durations
##
hr.wind <- hr.wind %>%
 mutate(Ef.fetch = 5.23e-3 * sqrt(fr.vel.ms*9.81*(3600^3)))

## Wave height
hr.wind <- hr.wind %>%
  mutate(H.m = 4.13e-2 * fr.vel.ms * sqrt(Ef.fetch/9.81)) %>%
  mutate(H.cm = H.m*100)

### basic histogram
wave.hsum.hr <- 
  ggplot(hr.wind, aes(H.cm))+
  geom_histogram(binwidth=1, position="identity", 
                          color="black", fill="grey")+
  ylab("Count")+
  xlab("Estimated Mean Wave Height (cm)")+
  ggtitle("Lake Mendota summer hourly 2012-2022")+
           theme_bw()
wave.hsum.hr

## Wave Period
hr.wind <- hr.wind %>%
  mutate(T.sec = 0.651 * (((fr.vel.ms*Ef.fetch)/(9.81^2))^0.33))

## basic histogram
wave.psum.hr <- 
  ggplot(hr.wind, aes(T.sec))+
  geom_histogram(binwidth=0.1, position="identity", 
                 color="black", fill="grey")+
  ylab("Count")+
  xlab("Estimated Mean Wave Period (s)")+
  ggtitle("Lake Mendota summer hourly 2012-2022")+
  theme_bw()
wave.psum.hr

## save results
write.csv(hr.wind, "summer_hourly_wave_results.csv")


####### Wind events
### See wind duration code for calculation
## hourly over 3 m/s 

### Wave event data with 1, 3, and 5 m/s groupings
weve_1_3_5 <- read.csv(file.choose(), header=TRUE)

### Set levels as factor and rearrange
weve.f <- weve_1_3_5 %>%
  mutate(across(Wind.Speed, factor, levels=c("> 5 m/s", "> 3 m/s", 
                                             "> 1 m/s")))
### Plot stacked histogram of wind events
weve.p <-
  ggplot(weve.f, aes(x=Duration, fill=Wind.Speed,
                     color=Wind.Speed))+
  geom_histogram(position="stack", alpha=0.5,
                 breaks=c(1, 3, 6, 12,24,48,96,192,288))+
  scale_x_continuous(trans='log2',
                     breaks=c(1,3,6,12,24,48,96,192,288))+
  scale_color_manual(values=c("#003366", "#009966", "#CC6600")) + 
  scale_fill_manual(values=c("#003366", "#009966", "#CC6600"))+
  ylab("Count")+
  xlab("Wind event duration (hours)")+
  labs(fill = "Wind Speed", color="Wind Speed")+
  theme_bw()+
  theme(legend.position = "inside")

weve.p
### This is panel B of Fig 2 

### Put Fig. 2 together
fig_2 <- plot_grid(mend.sum.wr,
                   weve.p, 
                   hres.wave.hsum, 
                   hres.wave.psum, 
                   ncol=2, align="hv",
                   labels =c('A', 'B', 'C', 'D'))

fig_2
### save the update
