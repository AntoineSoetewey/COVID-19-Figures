# clean environment
remove(list = ls())
# required packages
library(ggplot2)
library(ggrepel)
library(zoo)
library(lme4)
library(plyr)
library(dplyr)
library(scales)
library(ggpubr)
library(grid)
library(gridExtra)
library(magrittr)
library(ggpol)
library(reshape2)
library(XML)
library(patchwork)
library(stringr)


# structure of the population
pop <- read.csv("TF_SOC_POP_STRUCT_2020.txt",
  sep = ";",
  stringsAsFactors = FALSE
)

# add brussels as a province
pop$PROVINCE <- ifelse(pop$TX_ADM_DSTR_DESCR_FR == "Arrondissement de Bruxelles-Capitale",
  "Provincie Brussels",
  pop$TX_PROV_DESCR_NL
)

# extract province names
pop <- mutate(pop, PROVINCE = as.factor(sapply(strsplit(PROVINCE, split = " ", fixed = TRUE), function(x) (x[2])))) %>%
  select(PROVINCE, CD_AGE, CD_SEX, MS_POPULATION) %>%
  rename(
    AGE = CD_AGE,
    SEX = CD_SEX,
    POPULATION = MS_POPULATION
  )

## Recoding pop$PROVINCE
pop$PROVINCE <- recode_factor(pop$PROVINCE,
  "Henegouwen" = "Hainaut",
  "Luik" = "Liège",
  "Luxemburg" = "Luxembourg",
  "Namen" = "Namur",
  "Vlaams-Brabant" = "Brabant",
  "Waals-Brabant" = "Brabant",
  "Brussels" = "Brabant"
)


## Cutting AGE into AGEGROUP and rename factors
pop$AGEGROUP <- cut(pop$AGE,
  include.lowest = TRUE,
  right = FALSE,
  breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 120)
)
pop$AGEGROUP <- recode_factor(pop$AGEGROUP,
  "[0,10)" = "0-9",
  "[10,20)" = "10-19",
  "[20,30)" = "20-29",
  "[30,40)" = "30-39",
  "[40,50)" = "40-49",
  "[50,60)" = "50-59",
  "[60,70)" = "60-69",
  "[70,80)" = "70-79",
  "[80,90)" = "80-89",
  "[90,120]" = "90+"
)

pop <- aggregate(POPULATION ~ AGEGROUP + SEX + PROVINCE, pop, sum) %>%
  select(PROVINCE, AGEGROUP, SEX, POPULATION)

# add new intakes for Belgium as a whole
belgium <- aggregate(POPULATION ~ AGEGROUP + SEX, pop, sum) %>%
  mutate(PROVINCE = "Belgium") %>%
  select(PROVINCE, AGEGROUP, SEX, POPULATION)

# bind population by province and belgium
pop_all <- rbind(pop, belgium)

# SEX as factor
pop_all$SEX <- factor(pop_all$SEX, labels = c("Women", "Men"))






# import Sciensano hospitalisations data
dat <- read.csv("https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv",
  fileEncoding = "UTF-8",
  stringsAsFactors = FALSE
)

# aggregate new intakes by province and date
dat <- dat %>%
  mutate(
    DATE = as.Date(DATE),
    SEX = factor(SEX, labels = c("Women", "Men")),
    AGEGROUP = as.factor(AGEGROUP),
    PROVINCE2 = case_when(
      PROVINCE %in% c("BrabantWallon", "VlaamsBrabant", "Brussels") ~ "Brabant",
      !PROVINCE %in% c("BrabantWallon", "VlaamsBrabant", "Brussels") ~ PROVINCE
    ),
    PROVINCE2 = case_when(
      PROVINCE == "OostVlaanderen" ~ "Oost-Vlaanderen",
      PROVINCE == "WestVlaanderen" ~ "West-Vlaanderen",
      !PROVINCE %in% c("OostVlaanderen", "WestVlaanderen") ~ PROVINCE2
    ),
    PROVINCE = as.factor(PROVINCE2)
  ) %>%
  select(DATE, PROVINCE, AGEGROUP, SEX, CASES)


dat<- dat[complete.cases(dat), ] # eliminate data without province, agegroup, sex 


# add new intakes for Belgium as a whole
belgium <- aggregate(CASES ~ DATE + AGEGROUP + SEX, dat, sum) %>%
  mutate(PROVINCE = "Belgium") %>%
  select(DATE, PROVINCE, AGEGROUP, SEX, CASES)

## bind and merge
dat_all <- rbind(dat, belgium)


# all period 

allperiod <- as.data.frame(expand.grid(as.Date(min(dat_all$DATE): max(dat_all$DATE)),
                                       levels(dat_all$PROVINCE), 
                                       levels(dat_all$AGEGROUP), 
                                       levels(dat_all$SEX)))
names(allperiod)<-c("DATE","PROVINCE","AGEGROUP","SEX")

dat_all<-merge(dat_all, allperiod, by=c("DATE","PROVINCE","AGEGROUP","SEX"), all=T)

dat_all[is.na(dat_all)] <- 0
dat_all <- aggregate(CASES ~ DATE + PROVINCE + AGEGROUP + SEX, dat_all, sum)


#merge with pop
dat_all <- merge(dat_all, pop_all)


# subset for period and province
start <- as.Date("2020-03-01")
end <- as.Date("2020-05-31")
dat <- subset(dat_all, DATE >= start & DATE <= end & PROVINCE == "Belgium")
nweeks <- as.numeric(end - start) / 7

# cases per week
dat$CASES_divid <- dat$CASES / nweeks

lab <- aggregate(CASES_divid ~ AGEGROUP + SEX, dat, sum)

limit <- 900

# plot for Belgium
bel_p1 <- ggplot(data = dat) +
  geom_bar(aes(AGEGROUP, CASES_divid, group = SEX, fill = SEX),
    stat = "identity",
    subset(dat, SEX == "Women")
  ) +
  geom_bar(aes(AGEGROUP, -CASES_divid, group = SEX, fill = SEX),
    stat = "identity",
    subset(dat, SEX == "Men")
  ) +
  scale_y_continuous(
    limits = c(-limit, limit),
    breaks = seq(-500, 500, 500),
    labels = abs(seq(-500, 500, 500))
  ) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Belgium",
    subtitle = paste0(format(start, format = "%d/%m/%Y"), " - ", format(end, format = "%d/%m/%Y")),
    x = "Age group",
    y = "Number of cases per week"
  ) +
  theme(
    legend.position = c(.95, .15),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(5.5, 5.5, 30, 5.5), "points")
  )

## plots for provinces
dat <- subset(dat_all, DATE >= start & DATE <= end & PROVINCE != "Belgium")
nweeks <- as.numeric(end - start) / 7

# cases per week
dat$CASES_divid <- dat$CASES / nweeks


pro_p1 <- ggplot(data = dat) +
  facet_wrap(vars(PROVINCE),
    scales = "fixed",
    ncol = 9
  ) +
  geom_bar(aes(AGEGROUP, CASES_divid, group = SEX, fill = SEX),
    stat = "identity",
    subset(dat, SEX == "Women")
  ) +
  geom_bar(aes(AGEGROUP, -CASES_divid, group = SEX, fill = SEX),
    stat = "identity",
    subset(dat, SEX == "Men")
  ) +
  scale_y_continuous(
    limits = c(-400, 400),
    breaks = seq(-300, 300, 300),
    labels = abs(seq(-300, 300, 300))
  ) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Age and sex specific COVID-19 cases per week in Belgium",
    subtitle = paste0(format(start, format = "%d/%m/%Y"), " - ", format(end, format = "%d/%m/%Y")),
    x = "Age group",
    y = "Number of cases per week"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(5.5, 5.5, 0, 5.5), "points")
  )




# subset for period and province
start <- as.Date("2020-06-01")
end <- as.Date("2020-08-31")
dat <- subset(dat_all, DATE >= start & DATE <= end & PROVINCE == "Belgium")
nweeks <- as.numeric(end - start) / 7

# cases per week
dat$CASES_divid <- dat$CASES / nweeks

# plot for Belgium
bel_p2 <- ggplot(data = dat) +
  geom_bar(aes(AGEGROUP, CASES_divid, group = SEX, fill = SEX),
    stat = "identity",
    subset(dat, SEX == "Women")
  ) +
  geom_bar(aes(AGEGROUP, -CASES_divid, group = SEX, fill = SEX),
    stat = "identity",
    subset(dat, SEX == "Men")
  ) +
  scale_y_continuous(
    limits = c(-limit, limit),
    breaks = seq(-500, 500, 500),
    labels = abs(seq(-500, 500, 500))
  ) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "",
    subtitle = paste0(format(start, format = "%d/%m/%Y"), " - ", format(end, format = "%d/%m/%Y")),
    x = "",
    y = "Number of cases per week"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(5.5, 5.5, 30, 5.5), "points")
  )

## plots for provinces
dat <- subset(dat_all, DATE >= start & DATE <= end & PROVINCE != "Belgium")
nweeks <- as.numeric(end - start) / 7

# cases per week
dat$CASES_divid <- dat$CASES / nweeks

pro_p2 <- ggplot(data = dat) +
  facet_wrap(vars(PROVINCE),
    scales = "fixed",
    ncol = 9
  ) +
  geom_bar(aes(AGEGROUP, CASES_divid, group = SEX, fill = SEX),
    stat = "identity",
    subset(dat, SEX == "Women")
  ) +
  geom_bar(aes(AGEGROUP, -CASES_divid, group = SEX, fill = SEX),
    stat = "identity",
    subset(dat, SEX == "Men")
  ) +
  scale_y_continuous(
    limits = c(-400, 400),
    breaks = seq(-300, 300, 300),
    labels = abs(seq(-300, 300, 300))
  ) +
  coord_flip() +
  theme_minimal() +
  labs(
    subtitle = paste0(format(start, format = "%d/%m/%Y"), " - ", format(end, format = "%d/%m/%Y")),
    x = "Age group",
    y = "Number of cases per week"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(5.5, 5.5, 0, 5.5), "points")
  )




# subset for period and province
start <- as.Date("2020-09-01")
end <- as.Date(max(dat_all$DATE, na.rm = TRUE))
dat <- subset(dat_all, DATE >= start & DATE <= end & PROVINCE == "Belgium")
nweeks <- as.numeric(end - start) / 7

# cases per week
dat$CASES_divid <- dat$CASES / nweeks

# plot for Belgium
bel_p3 <- ggplot(data = dat) +
  geom_bar(aes(AGEGROUP, CASES_divid, group = SEX, fill = SEX),
    stat = "identity",
    subset(dat, SEX == "Women")
  ) +
  geom_bar(aes(AGEGROUP, -CASES_divid, group = SEX, fill = SEX),
    stat = "identity",
    subset(dat, SEX == "Men")
  ) +
  scale_y_continuous(
    limits = c(-limit, limit),
    breaks = seq(-500, 500, 500),
    labels = abs(seq(-500, 500, 500))
  ) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "",
    subtitle = paste0(format(start, format = "%d/%m/%Y"), " - ", format(end, format = "%d/%m/%Y")),
    x = "",
    y = "Number of cases per week"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(5.5, 5.5, 30, 5.5), "points")
  )

## adjust caption at the end of the figure
caption <- grobTree(
  textGrob("Niko Speybroeck (@NikoSpeybroeck), Antoine Soetewey (@statsandr) & Angel Rosas (@arosas_aguirre) \n Data: https://epistat.wiv-isp.be/covid/  ",
    x = 1, hjust = 1, vjust = 0,
    gp = gpar(col = "black", fontsize = 11, lineheight = 1.2)
  ),
  cl = "ann"
)

## plots for provinces
dat <- subset(dat_all, DATE >= start & DATE <= end & PROVINCE != "Belgium")
nweeks <- as.numeric(end - start) / 7

# cases per week
dat$CASES_divid <- dat$CASES / nweeks

pro_p3 <- ggplot(data = dat) +
  facet_wrap(vars(PROVINCE),
    scales = "fixed",
    ncol = 9
  ) +
  geom_bar(aes(AGEGROUP, CASES_divid, group = SEX, fill = SEX),
    stat = "identity",
    subset(dat, SEX == "Women")
  ) +
  geom_bar(aes(AGEGROUP, -CASES_divid, group = SEX, fill = SEX),
    stat = "identity",
    subset(dat, SEX == "Men")
  ) +
  scale_y_continuous(
    limits = c(-400, 400),
    breaks = seq(-300, 300, 300),
    labels = abs(seq(-300, 300, 300))
  ) +
  coord_flip() +
  theme_minimal() +
  labs(
    subtitle = paste0(format(start, format = "%d/%m/%Y"), " - ", format(end, format = "%d/%m/%Y")),
    x = "Age group",
    y = "Number of cases per week"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(5.5, 5.5, 0, 5.5), "points")
  )

# save plot
png(file = "pyramid-plot_facets_week.png", width = 25.71428 * 360, height = 12 * 360, units = "px", pointsize = 7, res = 300)
ggarrange(ggarrange(pro_p1, pro_p2, pro_p3, ncol = 1),
  grid.arrange(bel_p1, bel_p2, bel_p3, bottom = caption, ncol = 3),
  ncol = 1, heights = c(2, 1)
)
dev.off()




library(GADMTools)
library(RColorBrewer)
library(tmap)
library(sf)


map <- gadm_sf_loadCountries(c("BEL"), level = 2, basefile = "./")$sf

map <- map %>%
  mutate(PROVINCE = case_when(
    NAME_2 %in% c("Brabant Wallon", "Vlaams Brabant", "Bruxelles") ~ "Brabant",
    !NAME_2 %in% c("Brabant Wallon", "Vlaams Brabant", "Bruxelles") ~ NAME_2
  )) %>%
  group_by(PROVINCE) %>%
  summarise(geometry = st_union(geometry))


mapBelgium<- tm_shape(map, unit = "km") +
  tm_borders(lwd = 2, col ="gray")+
  tm_layout(frame=F)

tmap_save(mapBelgium,units="cm", height = 12,  width = 25.4,  filename="map.png", dpi=300)



library(png)

img1 <- readPNG("pyramid-plot_facets_week.png")
img2 <- readPNG("map.png")


# Load downloaded images and add alpha channel

img1 = abind::abind(img1, img1[,,1]) # add an alpha channel

img2 = abind::abind(img2, img2[,,1]) # add an alpha channel

# Make semi-transparent
img1[,,4] <- 1
img2[,,4] <- 0.2

# Create output image


library(graphics)

png('pyramid-plot_facets_week.png', width = 25.71428 * 360, height = 12 * 360, units = "px", res=300)
par(mar = c(0,0,0,0), xaxs="i", yaxs="i")
plot.new()

rasterImage(img1, 0, 0, 1, 1)
rasterImage(img2, 0, 0, 1, 1)
dev.off()


