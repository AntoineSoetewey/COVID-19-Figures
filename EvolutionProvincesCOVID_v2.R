# clean environment
remove(list = ls())
# required packages
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)
library(RColorBrewer)
library(ggpubr)

# colors
reds <- c("white", brewer.pal(5, "Reds"))
blues <- c("white",brewer.pal(5, "Blues"))

## caption at the end of figure
caption <- grobTree(
  textGrob("Niko Speybroeck (@NikoSpeybroeck), Antoine Soetewey (@statsandr) & Angel Rosas (@arosas_aguirre) \n Data: https://epistat.wiv-isp.be/covid/  ",
           x = 1, hjust = 1, vjust = 0,
           gp = gpar(col = "black", fontsize = 8, lineheight = 1)
  ),
  cl = "ann"
)

# import Sciensano hospitalisations data
dat <- read.csv("https://epistat.sciensano.be/Data/COVID19BE_HOSP.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)

# aggregate new intakes by province and date
dat <- dat %>%
  mutate(
    DATE = as.Date(DATE),
    PROVINCE2 = case_when(
      PROVINCE %in% c("BrabantWallon", "VlaamsBrabant", "Brussels") ~ "Brabant",
      !PROVINCE %in% c("BrabantWallon", "VlaamsBrabant", "Brussels") ~ PROVINCE
    ),
    PROVINCE2 = case_when(
      PROVINCE == "OostVlaanderen" ~ "Oost-Vlaanderen",
      PROVINCE == "WestVlaanderen" ~ "West-Vlaanderen",
      !PROVINCE %in% c("OostVlaanderen", "WestVlaanderen") ~ PROVINCE2
    ),
    PROVINCE = PROVINCE2
  )

dat <- aggregate(NEW_IN ~ DATE + PROVINCE, dat, sum)

## insuring all dates for provinces
alldates <- as.data.frame(expand.grid(
              seq(as.Date(min(dat$DATE)), as.Date(max(dat$DATE)), by="days"),
              levels(as.factor(dat$PROVINCE))))
names(alldates)<-c("DATE","PROVINCE")

dat<-merge(dat, alldates, by=c("DATE","PROVINCE"), all=T)

# add new intakes for Belgium as a whole
belgium <- aggregate(NEW_IN ~ DATE, dat, sum) %>%
  mutate(PROVINCE = "Belgium") %>%
  select(DATE, PROVINCE, NEW_IN)

dat <- rbind(dat, belgium) %>%
  mutate(
    population = case_when(
      PROVINCE == "Antwerpen" ~ 1857986,
      PROVINCE == "Brabant" ~ 403599 + 1208542 + 1146175,
      PROVINCE == "Hainaut" ~ 1344241,
      PROVINCE == "Liège" ~ 1106992,
      PROVINCE == "Limburg" ~ 874048,
      PROVINCE == "Luxembourg" ~ 284638,
      PROVINCE == "Namur" ~ 494325,
      PROVINCE == "Oost-Vlaanderen" ~ 1515064,
      PROVINCE == "West-Vlaanderen" ~ 1195796,
      PROVINCE == "Belgium" ~ 11431406
    ),
    NEW_IN_divid = NEW_IN / population * 100000
  )

##### MAPS

# preparing 
cuts= c(0, 0.3, 1.0, 2, 3, 4, 8)
cutslab= c("[ 0.0, 0.3 ]", "] 0.3, 1.0 ]", "] 1.0, 2.0 ]", "] 2.0, 3.0 ]","] 3.0, 4.0 ]", "> 4.0")

## FUNCTION MAPS
library(GADMTools)
library(RColorBrewer)
library(tmap)
library(sf)

gx_map<-function(first_date, second_date){
  
  ##agregate data by period
  dat_ag <- filter(dat, PROVINCE != "Belgium") %>%
    group_by(PROVINCE) %>%
    summarize(
      "per" = sum(NEW_IN_divid[DATE >= first_date & DATE < second_date], na.rm = T) / length(first_date:second_date))
    
  ## map 
  map <- gadm_sf_loadCountries(c("BEL"), level = 2, basefile = "./")$sf
    
  map <- map %>%
    mutate(PROVINCE = case_when(
      NAME_2 %in% c("Brabant Wallon", "Vlaams Brabant", "Bruxelles") ~ "Brabant",
      !NAME_2 %in% c("Brabant Wallon", "Vlaams Brabant", "Bruxelles") ~ NAME_2
    )) %>%
    group_by(PROVINCE) %>%
    summarise(geometry = st_union(geometry)) %>%
    left_join(dat_ag, by = "PROVINCE") %>%
    mutate(
      class = cut(per, breaks = cuts, labels=cutslab, include.lowest = TRUE)) 
  
  ## points for labels
  points <- st_centroid(map)
  points <- cbind(map, st_coordinates(st_centroid(map$geometry)))

  points <- mutate(points,
                   num = paste("(", format(round(per, 2),nsmall=2), ")"))
  
#chosen second_date (for the last period)
  if (second_date == max(dat$DATE)) {
    second_date = second_date
  } else {
    second_date = second_date-1
  }
  
## period title
  period <- paste0(
    format(first_date, format = "%d/%m"), " - ",
    format(second_date, format = "%d/%m"), "   "
  )
  
## plotting
  ggplot(map) +
    geom_sf(aes(fill = class)) +
    scale_fill_manual(values = reds, drop = FALSE) +
     geom_text(
        data = points, aes(x = X+0.01, y = Y + 0.07, label = PROVINCE), col = "black", size = 2,  nudge_x = -0.07,
          check_overlap = TRUE
    ) +
    geom_text(
    data = points, aes(x = X, y = Y-0.02, label = num), col = "black", size = 2.5,  nudge_x = -0.07,
    check_overlap = TRUE
    ) +
    
    labs(title= bquote(bold(.(period))), fill ="Daily hospitalizations \n (x100,000 inh.)") +
    theme_void() +
    theme(
      title=element_text(size = 10, color = "black"),
      plot.title = element_text(hjust = 0.5),
      # Change legend
      legend.position = "none",
      legend.key.size = unit(0.9, "line"),
      legend.title = element_text(size = 12, color = "black", face="bold"),
      legend.text = element_text(color = "black", size = 11),
      plot.margin = unit(c(+0.1, 0, +0.2, 0), "cm")
  )
}

# dates for periods
date0 <- as.Date("2020-03-15")
date1 <- as.Date("2020-04-15")
date2 <- as.Date("2020-05-15")
date3 <- as.Date("2020-06-15")
date4 <- as.Date("2020-07-15")
date5 <- as.Date("2020-08-15")
date6 <- as.Date("2020-09-15")
date7 <- max(dat$DATE)
# get_legend
legend<- get_legend(gx_map(date0,date1) +     
                      theme(legend.position = "right"))

# saving
png(file = "EvolutionHospitalizations_red.png", width = 12 * 360, height = 6* 360, units = "px", pointsize = 7, res = 300)
grid.arrange(          gx_map(date0,date1),
                       gx_map(date1,date2),
                       gx_map(date2,date3),
                       gx_map(date3,date4),
                       gx_map(date4,date5),
                       gx_map(date5,date6),
                       gx_map(date6,date7),
                       legend,
                      ncol=4, nrow = 2, 
                      widths = c(1,1,1,1),
                      bottom = caption)
dev.off()


