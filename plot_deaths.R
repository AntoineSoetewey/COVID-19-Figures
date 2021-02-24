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


# import statbel data
dat_statbel <- read.csv(file = "DEMO_DEATH_OPEN_W05.txt",
                        sep = ";",
                        stringsAsFactors = TRUE,
                        header = TRUE)

# choose period
dat_statbel$DT_DATE <- as.Date(dat_statbel$DT_DATE, format = "%d/%m/%Y")
starting_date <- "2020-03-01"
end_date <- max(dat_statbel$DT_DATE)
dat_statbel <- subset(dat_statbel, DT_DATE >= starting_date & DT_DATE <= end_date)

# label provinces
dat_statbel$CD_PROV <- as.factor(dat_statbel$CD_PROV)
## Recoding dat_statbel$CD_PROV
dat_statbel$CD_PROV <- recode_factor(dat_statbel$CD_PROV,
  "4000" = "Brabant",
  "10000" = "Antwerpen",
  "20001" = "Brabant",
  "20002" = "Brabant",
  "30000" = "West-Vlaanderen",
  "40000" = "Oost-Vlaanderen",
  "50000" = "Hainaut",
  "60000" = "Liège",
  "70000" = "Limburg",
  "80000" = "Luxembourg",
  "90000" = "Namur"
)
# View(dat_statbel)

# aggregate deaths by province
dat_statbel_agg <- aggregate(MS_NUM_DEATH ~ CD_PROV, dat_statbel, sum)
names(dat_statbel_agg) <- c("PROVINCE", "DEATH")
View(dat_statbel_agg)

dat_statbel_agg <- dat_statbel_agg %>%
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
    DEATH_divid = DEATH / population * 100000
  )




##### MAPS

### Obtaining Belgium shapefile at province level

library(GADMTools)
library(RColorBrewer)
library(tmap)
library(sf)


dat_ag <- filter(dat_statbel_agg, PROVINCE != "Belgium") %>%
  group_by(PROVINCE) %>%
  summarize(
    "per1" = sum(DEATH_divid)
  )


## sf structure
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
    class1 = cut(per1,
      breaks = c(900, 950, 1000, 1050, 1100, 1150, 9999),
      include.lowest = TRUE,
      labels = c("[ 900, 950]", "] 950, 1000 ]", "] 1000, 1050 ]", "] 1050, 1100 ]", "] 1100, 1150 ]", " > 1150")
    )
  )






###### MAPS WITH GGPLOT

points <- st_centroid(map)
points <- cbind(map, st_coordinates(st_centroid(map$geometry)))

points <- mutate(points,
  num_1 = paste("(", format(round(per1, 2), nsmall = 2), ")")
)


period1 <- paste0("Period ", starting_date, " to ", end_date)



library(RColorBrewer)
reds <- brewer.pal(7, "Reds")
blues <- brewer.pal(7, "Blues")


map1 <- ggplot(map) +
  geom_sf(aes(fill = class1)) +
  scale_fill_manual(values = reds, drop = FALSE) +
  geom_text(
    data = points, aes(x = X, y = Y + 0.06, label = PROVINCE), col = "black", size = 2.4, nudge_x = -0.07,
    check_overlap = TRUE
  ) +
  geom_text(
    data = points, aes(x = X, y = Y, label = num_1), col = "black", size = 3, nudge_x = -0.07,
    check_overlap = TRUE
  ) +
  labs(fill = bquote(atop(NA, atop("Deaths (x100,000 inh.)", bold(.(period1))))),
       title = "Deaths by province in Belgium") +
  theme_void() +
  theme(
    # Change legend
    legend.position = c(0.2, 0.2),
    legend.key.size = unit(0.9, "line"),
    legend.title = element_text(size = 12, color = "black"),
    legend.text = element_text(color = "black"),
    plot.margin = unit(c(+0.2, 0, +0.5, 3), "cm")
  )
# map1

## adjust caption at the end of the trend figure
caption <- grobTree(
    textGrob("Niko Speybroeck (@NikoSpeybroeck), Antoine Soetewey (@statsandr) & Angel Rosas (@arosas_aguirre)  \nData: Statbel   ",
           x = 1, hjust = 1, vjust = 0,
           gp = gpar(col = "black", fontsize = 10, lineheight = 1.2)
  ),
  cl = "ann"
)


# save plot
png(file = "plot_deaths_statbel_2202.png", width = 7 * 360, height = 7 * 360, units = "px", pointsize = 7, res = 300)
ggarrange(grid.arrange(map1, bottom = caption))
dev.off()
