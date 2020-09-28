# clean environment
remove(list = ls())
# required packages
library(ggplot2)
library(ggrepel)
library(zoo)
library(lme4)
library(dplyr)
library(scales)
library(ggpubr)
library(grid)
library(gridExtra)

# import Sciensano hospitalisations data
dat <- read.csv("https://epistat.sciensano.be/Data/COVID19BE_HOSP.csv", fileEncoding = "UTF-8", stringsAsFactors=FALSE)

# aggregate new intakes by province and date

dat <- dat %>%
              mutate(DATE = as.Date(DATE),
                    PROVINCE2 = case_when(PROVINCE %in% c("BrabantWallon","VlaamsBrabant","Brussels") ~ "Brabant", 
                              !PROVINCE %in% c("BrabantWallon","VlaamsBrabant","Brussels") ~ PROVINCE),
                    PROVINCE2 = case_when(PROVINCE =="OostVlaanderen" ~ "Oost-Vlaanderen",
                                          PROVINCE =="WestVlaanderen" ~ "West-Vlaanderen",
                                          !PROVINCE %in%  c("OostVlaanderen","WestVlaanderen") ~ PROVINCE2),
                    PROVINCE = PROVINCE2)

dat <- aggregate(NEW_IN ~ DATE + PROVINCE, dat, sum) 


# add new intakes for Belgium as a whole

belgium <- aggregate(NEW_IN ~ DATE, dat, sum) %>%
      mutate(PROVINCE = "Belgium") %>%
        select(DATE, PROVINCE, NEW_IN)

## 

dat <- rbind(dat, belgium) %>% 
         mutate(population = case_when(
                      PROVINCE == "Antwerpen" ~ 1857986,
                      PROVINCE == "Brabant" ~ 403599+1208542+1146175,
                      PROVINCE == "Hainaut" ~ 1344241,
                      PROVINCE == "Liège" ~ 1106992,
                      PROVINCE == "Limburg" ~ 874048,
                      PROVINCE == "Luxembourg" ~ 284638,
                      PROVINCE == "Namur" ~ 494325,
                      PROVINCE == "Oost-Vlaanderen" ~ 1515064,
                      PROVINCE == "West-Vlaanderen" ~ 1195796,
                      PROVINCE == "Belgium" ~ 11431406), 
          NEW_IN_divid = NEW_IN / population * 100000)

dat$PROVINCE <- relevel(as.factor(dat$PROVINCE), ref = "Belgium")

# Create plot in english
fig_trends <- ggplot(
  subset(dat, DATE >= "2020-06-21"),
  aes(x = DATE, y = NEW_IN_divid)
) +
  annotate("rect",
           ymin = 0, ymax = 0.5,
           xmin = as.Date(-Inf), xmax = as.Date(Inf),
           alpha = .05,
           fill = "gold"
  ) +
  annotate("rect",
           ymin = 0.5, ymax = 1,
           xmin = as.Date(-Inf), xmax = as.Date(Inf),
           alpha = .2,
           fill = "orange"
  ) +
  annotate("rect",
           ymin = 1, ymax = Inf,
           xmin = as.Date(-Inf), xmax = as.Date(Inf),
           alpha = .2,
           fill = "red"
  ) +
  geom_point(
    size = 1L,
    colour = "steelblue"
  ) +
  labs(x = "", y = "Number of hospitalisations (per 100,00 inhabitants)") +
  theme_minimal() +
  facet_wrap(vars(PROVINCE),
             scales = "free",
             ncol = 2
  ) +
  geom_smooth(
    se = FALSE,
    col = "grey",
    method = "gam",
    formula = y ~ s(x)
  ) +
  geom_vline(
    xintercept = as.Date("2020-07-01"), linetype = "dashed",
    color = "lightgrey", size = 0.5
  ) +
  geom_vline(
    xintercept = as.Date("2020-08-01"), linetype = "dashed",
    color = "lightgrey", size = 0.5
  ) +
  geom_vline(
    xintercept = as.Date("2020-09-01"), linetype = "dashed",
    color = "lightgrey", size = 0.5
  ) +
  labs(
    title = "Evolution of hospital admissions in Belgium - COVID-19"
  ) +
  scale_y_continuous(breaks = seq(from = 0, to = 1.5, by = 0.5), limits = c(0, 1.5)) +
  scale_x_date(labels = date_format("%d-%m"))

## adjust caption at the end of the trend figure
caption <- grobTree(
  textGrob("* Solid lines: curves fitted to observations",
           x = 0, hjust = 0, vjust = 0,
           gp = gpar(col = "darkgray", fontsize = 7, lineheight = 1.2)
  ),
  textGrob("Niko Speybroeck (@NikoSpeybroeck), Antoine Soetewey (@statsandr) & Angel Rosas (@arosas_aguirre) \n Data: https://epistat.wiv-isp.be/covid/  ",
           x = 1, hjust = 1, vjust = 0,
           gp = gpar(col = "black", fontsize = 7.5, lineheight = 1.2)
  ),
  cl = "ann"
)


##### MAPS

### Obtaining Belgium shapefile at province level

library(GADMTools)
library(RColorBrewer)
library(tmap)



# agregated data to join with the map 
# calculating the daily rate in two periods :
## 1. last week of March- first week of April (14 days)
## 2. Last 14 days reported by Scienciano

dat_ag <- filter(dat, PROVINCE != "Belgium") %>%
  group_by(PROVINCE) %>%
  summarize(
    "per1" = sum(NEW_IN_divid[DATE>=as.Date("2020-03-25") & DATE<=as.Date("2020-04-07")], na.rm = T)/14,
    "per2"= sum(NEW_IN_divid[DATE>=(max(dat$DATE)-13) & DATE<=max(dat$DATE)], na.rm = T)/14)



## sf structure
map <- gadm_sf_loadCountries(c("BEL"), level = 2, basefile = "./")$sf

map <- map %>%
  mutate(PROVINCE = case_when(NAME_2 %in% c("Brabant Wallon","Vlaams Brabant","Bruxelles") ~ "Brabant", 
                              !NAME_2 %in% c("Brabant Wallon","Vlaams Brabant","Bruxelles") ~ NAME_2)) %>% 
  group_by(PROVINCE) %>% 
  summarize(geometry = st_union(geometry)) %>% 
    left_join(dat_ag, by="PROVINCE") %>%
      mutate(
         class1 = cut(per1,
                      breaks = c(0, 0.001, 0.25, 0.5, 1, 3, 5, 7),
                      include.lowest = TRUE, 
                      labels = c("0.0", "] 0.0, 0.25 ]", "] 0.25, 0.5 ]", "] 0.5, 1.0 ]","] 1.0, 3.0 ]","] 3.0, 5.0]"," > 5.0" )),
         class2 = cut(per2,
                      breaks = c(0, 0.001, 0.25, 0.5, 1, 3, 5, 7),
                      include.lowest = TRUE, 
                      labels = c("0.0", "] 0.0, 0.25 ]", "] 0.25, 0.5 ]", "] 0.5, 1.0 ]","] 1.0, 3.0 ]","] 3.0, 5.0]"," > 5.0" )))
  


###### MAPS WITH GGPLOT

points <- st_centroid(map)
points <- cbind(map, st_coordinates(st_centroid(map$geometry)))

points <- mutate(points,
  num_1 = paste("(", round(per1, 2), ")"),
  num_2 = paste("(", round(per2, 2), ")")
  )


period1 <- paste0("Period: 25/03 - 07/04","   ")
period2<- paste0("Period: ", format(max(dat$DATE)-14, format = "%d/%m"), " - ", 
                 format(max(dat$DATE)-1, format = "%d/%m"),"   ")


library(RColorBrewer)
reds <- brewer.pal(7, "Reds")
blues <- brewer.pal(7, "Blues")


map1 <- ggplot(map) +
  geom_sf(aes(fill = class1)) +
  
  scale_fill_manual(values = blues, drop = FALSE) +
  
  geom_text(
    data = points, aes(x = X , y = Y + 0.06 , label = PROVINCE), col = "black", size = 3,
    check_overlap = TRUE
  ) +
  geom_text(
    data = points, aes(x = X , y = Y , label = num_1), col = "black", size = 3,
    check_overlap = TRUE
  ) +
  
  labs(fill = bquote(atop(NA, atop("Hospitalisations (x100,000 inhabitants)", bold(.(period1)))))) +
  theme_void() +
  theme(
    # Change legend
    legend.position = c(0.2, 0.22),
    legend.key.size=unit(0.7,"line"),
    legend.title = element_text(size = 12, color = "black"),
    legend.text = element_text(color = "black"),
    plot.margin = unit(c(+0.2, 0, -0.5, 3), "cm")
  )


map2 <- ggplot(map) +
  geom_sf(aes(fill = class2)) +
  
  scale_fill_manual(values = blues, drop = FALSE) +
  
  geom_text(
    data = points, aes(x = X , y = Y + 0.06 , label = PROVINCE), col = "black", size = 3,
    check_overlap = TRUE
  ) +
  geom_text(
    data = points, aes(x = X , y = Y , label = num_2), col = "black", size = 3,
    check_overlap = TRUE
  ) +
  
  labs(fill = bquote(atop(NA, atop("Hospitalisations (x100,000 inhabitants)", bold(.(period2)))))) +
  theme_void() +
  theme(
    # Change legend
    legend.position = c(0.2, 0.22),
    legend.key.size=unit(0.7,"line"),
    legend.title = element_text(size = 12, color = "black"),
    legend.text = element_text(color = "black"),
    plot.margin = unit(c(+0.2, 0, -0.5, 3), "cm")
  )

# save plot
png(file = "Belgian_Hospitalisations_2409_2col.png", width = 15 * 360, heigh = 7 * 360, units = "px", pointsize = 7, res = 300)
ggarrange(ggarrange(map1, map2, ncol = 1),
          grid.arrange(fig_trends, bottom = caption),
          ncol = 2, widths = c(1, 1.5)
)
dev.off()
