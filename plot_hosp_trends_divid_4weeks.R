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
library(broom)

# import Sciensano hospitalisations data
dat <- read.csv("https://epistat.sciensano.be/Data/COVID19BE_HOSP.csv")

# aggregate new intakes by province and date
dat <- aggregate(NEW_IN ~ DATE + PROVINCE, dat, sum)

# add new intakes for Belgium as a whole
belgium <- aggregate(NEW_IN ~ DATE, dat, sum)
belgium$PROVINCE <- "Belgium"
col_order <- c("DATE", "PROVINCE", "NEW_IN")
belgium <- belgium[, col_order]
dat <- rbind(dat, belgium)

# transform date and provinces
dat$DATE <- as.Date(dat$DATE)
dat$PROVINCE <- factor(dat$PROVINCE,
  levels = c(
    "Antwerpen",
    "BrabantWallon",
    "Brussels",
    "Hainaut",
    "Liège",
    "Limburg",
    "Luxembourg",
    "Namur",
    "OostVlaanderen",
    "VlaamsBrabant",
    "WestVlaanderen",
    "Belgium"
  ),
  labels = c(
    "Antwerpen",
    "Brabant Wallon",
    "Brussels",
    "Hainaut",
    "Liège",
    "Limburg",
    "Luxembourg",
    "Namur",
    "Oost-Vlaanderen",
    "Vlaams-Brabant",
    "West-Vlaanderen",
    "Belgique/België"
  )
)

# compute NEW_IN by population size
dat <- dat %>%
  mutate(population = case_when(
    PROVINCE == "Antwerpen" ~ 1857986,
    PROVINCE == "Brabant Wallon" ~ 403599,
    PROVINCE == "Brussels" ~ 1208542,
    PROVINCE == "Hainaut" ~ 1344241,
    PROVINCE == "Liège" ~ 1106992,
    PROVINCE == "Limburg" ~ 874048,
    PROVINCE == "Luxembourg" ~ 284638,
    PROVINCE == "Namur" ~ 494325,
    PROVINCE == "Oost-Vlaanderen" ~ 1515064,
    PROVINCE == "Vlaams-Brabant" ~ 1146175,
    PROVINCE == "West-Vlaanderen" ~ 1195796,
    PROVINCE == "Belgique/België" ~ 11431406
  )) %>%
  mutate(NEW_IN_divid = NEW_IN / population * 1000000)

# create df of last n days to previous n days
n <- 7
dat_t7 <- dat %>%
  group_by(PROVINCE) %>%
  slice((n() - (n - 1)):n())

dat_t14 <- dat %>%
  group_by(PROVINCE) %>%
  slice((n() - (2 * n - 1)):(n() - n))

dat_t21 <- dat %>%
  group_by(PROVINCE) %>%
  slice((n() - (3 * n - 1)):(n() - 2 * n))

dat_t28 <- dat %>%
  group_by(PROVINCE) %>%
  slice((n() - (4 * n - 1)):(n() - 3 * n))

# create means for the periods
dat_7mean <- aggregate(NEW_IN_divid ~ PROVINCE, dat_t7, mean)
dat_14mean <- aggregate(NEW_IN_divid ~ PROVINCE, dat_t14, mean)
dat_21mean <- aggregate(NEW_IN_divid ~ PROVINCE, dat_t21, mean)
dat_28mean <- aggregate(NEW_IN_divid ~ PROVINCE, dat_t28, mean)

# Create plot in dutch/fr
fig_trends <- ggplot(
  subset(dat, DATE >= "2020-05-02"), # subset data from May 3
  aes(x = DATE, y = NEW_IN_divid)
) +
  geom_point(
    size = 1L,
    colour = "steelblue"
  ) +
  labs(x = "", y = "Nombre d'hospitalisations (par 1,000,000 habitants) / Hospitalisaties (per 1,000,000 inwoners)") +
  theme_minimal() +
  facet_wrap(vars(PROVINCE),
    scales = "free"
  ) +
  geom_vline(
    xintercept = as.Date("2020-05-04"), linetype = "dashed",
    color = "darkgrey", size = 0.5
  ) +
  geom_text(aes(x = as.Date("2020-05-04"), label = "1a", y = 3 * 10),
    colour = "darkgrey", hjust = -0.1,
  ) +
  geom_vline(
    xintercept = as.Date("2020-05-11"), linetype = "dashed",
    color = "darkgrey", size = 0.5
  ) +
  geom_text(aes(x = as.Date("2020-05-11"), label = "1b", y = 3 * 10),
    colour = "darkgrey", hjust = -0.1,
  ) +
  geom_vline(
    xintercept = as.Date("2020-05-18"), linetype = "dashed",
    color = "darkgrey", size = 0.5
  ) +
  geom_text(aes(x = as.Date("2020-05-18"), label = "2", y = 3 * 10),
    colour = "darkgrey", hjust = -0.1,
  ) +
  labs(
    title = "Evolution des admissions hospitalières / Evolutie van de hospitalisaties - COVID-19"
  ) +
  scale_y_continuous(breaks = seq(from = 0, to = 10 * 10, by = 1 * 10), limits = c(0, 3 * 10)) +
  scale_x_date(labels = date_format("%d-%m")) +
  geom_hline(
    yintercept = 100 / (11431406 / 1000000),
    linetype = "dashed",
    color = "darkgrey"
    # ,
    #    size=4
  ) +
  geom_segment(
    data = dat_7mean,
    aes(
      y = NEW_IN_divid,
      yend = NEW_IN_divid,
      x = max(dat$DATE) - (n - 1),
      xend = max(dat$DATE)
    ),
    color = "darkgrey",
    lwd = 1.2
  ) +
  geom_segment(
    data = dat_14mean,
    aes(
      y = NEW_IN_divid,
      yend = NEW_IN_divid,
      x = max(dat$DATE) - n - (n - 1),
      xend = max(dat$DATE) - n
    ),
    color = "darkgrey",
    lwd = 1.2
  ) +
  geom_segment(
    data = dat_21mean,
    aes(
      y = NEW_IN_divid,
      yend = NEW_IN_divid,
      x = max(dat$DATE) - 2 * n - (n - 1),
      xend = max(dat$DATE) - 2 * n
    ),
    color = "darkgrey",
    lwd = 1.2
  ) +
  geom_segment(
    data = dat_28mean,
    aes(
      y = NEW_IN_divid,
      yend = NEW_IN_divid,
      x = max(dat$DATE) - 3 * n - (n - 1),
      xend = max(dat$DATE) - 3 * n
    ),
    color = "darkgrey",
    lwd = 1.2
  ) +
  geom_text(
    data = dat_7mean,
    mapping = aes(
      x = max(dat$DATE) - 1,
      y = NEW_IN_divid,
      label = format(round(NEW_IN_divid, 1), nsmall = 1),
      vjust = -0.5,
      hjust = 1
    ),
    color = "darkgrey",
    size = 4,
    fontface = "bold"
  ) +
  geom_text(
    data = dat_14mean,
    mapping = aes(
      x = max(dat$DATE) - n - 1,
      y = NEW_IN_divid,
      label = format(round(NEW_IN_divid, 1), nsmall = 1),
      vjust = -0.5,
      hjust = 1
    ),
    color = "darkgrey",
    size = 4,
    fontface = "bold"
  ) +
  geom_text(
    data = dat_21mean,
    mapping = aes(
      x = max(dat$DATE) - 2 * n - 1,
      y = NEW_IN_divid,
      label = format(round(NEW_IN_divid, 1), nsmall = 1),
      vjust = -0.5,
      hjust = 1
    ),
    color = "darkgrey",
    size = 4,
    fontface = "bold"
  ) +
  geom_text(
    data = dat_28mean,
    mapping = aes(
      x = max(dat$DATE) - 3 * n - 1,
      y = NEW_IN_divid,
      label = format(round(NEW_IN_divid, 1), nsmall = 1),
      vjust = -0.5,
      hjust = 1
    ),
    color = "darkgrey",
    size = 4,
    fontface = "bold"
  )

# fig_trends

## adjust caption at the end of the trend figure
caption <- grobTree(
  textGrob(" * Lignes solides : moyennes 7 jours (4 semaines) / Volle lijnen : 7-daags gemiddelde (4 weken) \n * Lignes pointillées : phases de déconfinement / Gestippelde lijnen: fases afbouw lockdown",
    x = 0, hjust = 0, vjust = 0,
    gp = gpar(col = "darkgray", fontsize = 7, lineheight = 0.8)
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

## sf structure
map <- gadm_sf_loadCountries(c("BEL"), level = 2, basefile = "./")$sf
map <- map %>%
  rename("PROVINCE" = NAME_2)

map$PROVINCE[c(1, 5)] <- c("Brussels", "Vlaams-Brabant")



## agregating data
dat_ag <- dat %>%
  group_by(PROVINCE) %>%
  summarize(
    "new_in" = sum(NEW_IN[DATE >= (Sys.Date() - 31)], na.rm = T),
    "new_in2" = sum(NEW_IN[DATE >= (Sys.Date() - 8)], na.rm = T),
    "population" = max(population, na.rm = T)
  ) %>%
  mutate(
    new_in_divid = new_in / population * 100000,
    new_in_divid2 = new_in2 / population * 100000,
    class2 = cut(new_in_divid2,
      breaks = c(0, 0.001, 2.5, 9, 100),
      include.lowest = TRUE, labels = c("0.0", "] 0.0, 2.5 ]", "] 2.5, 9.0 ]", "> 9.0")
    )
  )



map.data <- left_join(map, dat_ag, by = "PROVINCE")
map.data <- subset(map.data, !PROVINCE %in% "Belgium")

###### MAPS WITH GGPLOT

points <- st_centroid(map.data)
points <- cbind(map.data, st_coordinates(st_centroid(map.data$geometry)))

points <- mutate(points,
  num_1 = paste("(", round(new_in_divid, 1), ")"),
  num_2 = paste("(", round(new_in_divid2, 1), ")"),
  q1 = as.numeric(cut(new_in_divid,
    breaks = quantile(new_in_divid, probs = seq(0, 1, by = 0.2), na.rm = TRUE),
    include.lowest = TRUE
  )),
  q2 = as.numeric(cut(new_in_divid2,
    breaks = quantile(new_in_divid2, probs = seq(0, 1, by = 0.2), na.rm = TRUE),
    include.lowest = TRUE
  )),
  q1 = as.factor(ifelse(q1 < 5, 1, 2)),
  q2 = as.factor(ifelse(q2 < 5, 1, 2))
)

points1 <- subset(points, !PROVINCE %in% "Vlaams-Brabant")
points2 <- subset(points, PROVINCE %in% "Vlaams-Brabant")

period1 <- paste0(
  "Période / periode : ", format(Sys.Date() - 31, format = "%d/%m"), " - ",
  format(Sys.Date() - 1, format = "%d/%m"), "   "
)
period2 <- paste0(
  "Période / periode : ", format(Sys.Date() - 8, format = "%d/%m"), " - ",
  format(Sys.Date() - 1, format = "%d/%m"), "   "
)

fig_map2 <- ggplot(map.data) +
  geom_sf(aes(fill = class2)) +
  # here you can change the number of blues in the pallete "n" (maximum=9)
  scale_fill_manual(values = c("white", "#ffe5e5", "#ff9999", "#ff0000"), drop = FALSE) +
  geom_text(
    data = points1, aes(x = X, y = Y + 0.03, label = PROVINCE, colour = q1), size = 3,
    check_overlap = TRUE
  ) +
  scale_colour_manual(values = c("black", "white"), guide = FALSE) +
  geom_text(
    data = points1, aes(x = X, y = Y - 0.03, label = num_2, colour = q1), size = 3,
    check_overlap = TRUE
  ) +
  geom_text(
    data = points2, aes(x = X + 0.07, y = Y + 0.09, label = PROVINCE), col = "black", size = 3,
    check_overlap = TRUE
  ) +
  geom_text(
    data = points2, aes(x = X + 0.07, y = Y + 0.03, label = num_2), col = "black", size = 3,
    check_overlap = TRUE
  ) +
  labs(fill = bquote(atop(NA, atop("Admissions hospitalières / \nHospitalisaties (x 100,000 hab./inw.)", bold(.(period2)))))) +
  # labs(fill=paste0("Admissions hospitalières / \nHospitalisaties (x 100,000 hab./inw.) \n", period1))+
  theme_void() +
  theme(
    # Change legend
    legend.position = c(0.2, 0.22),
    legend.title = element_text(size = 12, color = "black"),
    # legend.title = element_text(size = 9, color = "black"),
    legend.text = element_text(color = "black"),
    plot.margin = unit(c(+0.2, 0, -0.5, 3), "cm")
  )


png(file = "Belgian_Hospitalisations_COVID-19_4weeks.png", width = 15 * 360, heigh = 7 * 360, units = "px", pointsize = 7, res = 300)
ggarrange(fig_map2,
  grid.arrange(fig_trends, bottom = caption),
  ncol = 2, widths = c(1, 1.5)
)
dev.off()
