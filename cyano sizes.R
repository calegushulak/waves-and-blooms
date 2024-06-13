### Lake Mendota cyanobacteria sizes plotting 
library(tidyverse)
library(cowplot)


# https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-ntl.88.31&entityid=f2de15b2fff6ae962a04c150c0a1c510
# 
# ### read data
# phyto <- read.csv(file.choose(), header = TRUE)

# Download Mendota Phytoplankton data
# North Temperate Lakes LTER: Phytoplankton - Madison Lakes Area 1995 - current
inUrl  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/88/31/f2de15b2fff6ae962a04c150c0a1c510" 
infile <- tempfile()
download.file(inUrl,infile,method="curl")
phyto <- read_csv(infile) 

## Restrict to Mendota cyanobacteria
cy <- phyto %>%
  filter(lakeid == "ME", division == "Cyanophyta")

### Year as factor just in case
cy_cat <- transform(cy, year = as.factor(year4))


### histogram of GALD sizes
mency <- 
  ggplot(cy, aes(gald))+
  geom_histogram(binwidth=0.1, position="identity", 
                 color="black", fill="grey")+
  scale_x_log10()+
  ylab("Count")+
  xlab("GALD μm")+
  theme_bw()+
  theme(axis.title = element_text(size=12))
mency

### Create boxplot of all taxa
plot <- 
  ggplot(cy_cat, aes(genus, gald))+
  geom_boxplot()+
  labs(y="log GALD um", x = "Genus")+
  scale_y_continuous(trans='log10')+
  #ylim(0,1000)+
  theme_bw()+
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))
plot
ggssave("")

### Combine anabaena and dolichospermum genera as Dolichospermum
# cytax <- read.csv(file.choose(), header=TRUE)
cytax = phyto |> 
  filter(lakeid == 'ME', division == "Cyanophyta") |> 
  mutate(genus = if_else(genus == 'Anabaena', 'Dolichospermum', genus))

## grab some key genera
bloomers <- cytax %>%
  filter(genus > 0.0,
         genus %in% c("Dolichospermum", 
                      "Aphanizomenon",
                      "Chroococcus",
                      # "Chroococcaceae",
                      "Microcystis",
                      "Pseudanabaena",
                      "Synechococcus",
                      "Woronichinia"))

#### add symbols to names for filaments/chains, cells, cells and colonies
bloomers <- bloomers %>%
mutate(genus = case_when(genus == 'Dolichospermum' ~ 'Dolichospermumǂ',
                           genus == 'Aphanizomenon' ~ 'Aphanizomenonǂ',
                           genus == 'Chroococcaceae' ~ 'Chroococcaceae*',
                           genus == 'Microcystis' ~ 'Microcystis°',
                           genus == 'Pseudanabaena' ~ 'Pseudanabaenaǂ',
                           genus == 'Synechococcus' ~ 'Synechococcus*',
                           genus == 'Woronichinia' ~ 'Woronichinia°'))


cy_box <- 
  ggplot(bloomers, aes(genus, gald))+
  geom_boxplot()+
  scale_y_log10()+
  labs(y="GALD μm", x="Taxon")+
  #scale_y_continuous(breaks = seq(0,1000, by=100))+
  theme_bw()+
  theme(axis.text.x = element_text(angle=45, hjust=1))
#
cy_box

### Plot Fig. 4 
fig_4 <- 
  plot_grid(align="h", mency, cy_box, ncol=2,
          labels=c("A", "B"))

fig_4 
