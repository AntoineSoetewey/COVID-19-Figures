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
dat <- subset(dat, DATE >= max(dat$DATE) - 27)

dat$PROVINCE <- factor(dat$PROVINCE,
  levels = c(
    "Antwerpen",
    "BrabantWallon",
    "Brussels",
    "Hainaut",
    unique(dat$PROVINCE)[5],
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
    PROVINCE == levels(dat$PROVINCE)[1] ~ 1857986,
    PROVINCE == levels(dat$PROVINCE)[2] ~ 403599,
    PROVINCE == levels(dat$PROVINCE)[3] ~ 1208542,
    PROVINCE == levels(dat$PROVINCE)[4] ~ 1344241,
    PROVINCE == levels(dat$PROVINCE)[5] ~ 1106992,
    PROVINCE == levels(dat$PROVINCE)[6] ~ 874048,
    PROVINCE == levels(dat$PROVINCE)[7] ~ 284638,
    PROVINCE == levels(dat$PROVINCE)[8] ~ 494325,
    PROVINCE == levels(dat$PROVINCE)[9] ~ 1515064,
    PROVINCE == levels(dat$PROVINCE)[10] ~ 1146175,
    PROVINCE == levels(dat$PROVINCE)[11] ~ 1195796,
    PROVINCE == levels(dat$PROVINCE)[12] ~ 11431406
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
    xintercept = as.Date("2020-05-11"), linetype = "dashed",
    color = "darkgrey", size = 0.5
  ) +
  geom_text(aes(x = as.Date("2020-05-11"), label = "1b", y = 2 * 10),
    colour = "darkgrey", hjust = -0.1,
  ) +
  geom_vline(
    xintercept = as.Date("2020-05-18"), linetype = "dashed",
    color = "darkgrey", size = 0.5
  ) +
  geom_text(aes(x = as.Date("2020-05-18"), label = "2", y = 2 * 10),
    colour = "darkgrey", hjust = -0.1,
  ) +
  labs(
    title = "Evolution des admissions hospitalières / Evolutie van de hospitalisaties - COVID-19"
  ) +
  scale_y_continuous(breaks = seq(from = 0, to = 10 * 10, by = 1 * 10), limits = c(0, 2 * 10)) +
  scale_x_date(labels = date_format("%d-%m")) +
  geom_hline(
    yintercept = 100 / (11431406 / 1000000),
    linetype = "dashed",
    color = "red"
  ) +
  geom_hline(
    yintercept = 50 / (11431406 / 1000000),
    linetype = "dashed",
    color = "steelblue"
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
  textGrob(" * Lignes solides : moyennes 7 jours (4 semaines) / Volle lijnen : 7-daags gemiddelde (4 weken) \n * Lignes pointillées : phases de déconfinement / Gestippelde lijnen : fases afbouw lockdown \n * Lignes rouge et bleue : seuils de 100 et 50 cas par jour / Rode en blauwe lijnen : drempels van 100 en 50 gevallen per dag",
    x = 0, hjust = 0, vjust = 0,
    gp = gpar(col = "darkgray", fontsize = 7, lineheight = 0.8)
  ),
  textGrob("Niko Speybroeck (@NikoSpeybroeck), Antoine Soetewey (@statsandr) & Angel Rosas (@arosas_aguirre) \n Data : https://epistat.wiv-isp.be/covid/  ",
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

## sf data
map <- gadm_sf_loadCountries(c("BEL"), level = 2, basefile = "./")$sf
map <- map %>%
  arrange(NAME_2) %>%
  mutate(
    PROVINCE = levels(dat$PROVINCE)[-12],
    NEW_IN_divid = dat_7mean$NEW_IN_divid[-12],
    class = cut(NEW_IN_divid,
      breaks = c(0, 0.001, 2.5, 9, 100),
      include.lowest = TRUE, labels = c("0.0", "] 0.0, 2.5 ]", "] 2.5, 9.0 ]", "> 9.0")
    )
  )

## label points
points <- st_centroid(map)
points <- cbind(map, st_coordinates(st_centroid(map$geometry)))

points <- mutate(points,
  num = paste("(", format(round(NEW_IN_divid, 1), nsmall = 1), ")")
)

points1 <- subset(points, !PROVINCE %in% "Vlaams-Brabant")
points2 <- subset(points, PROVINCE %in% "Vlaams-Brabant")

# map

period <- paste0(
  "Période / periode : ", format(max(dat$DATE) - 6, format = "%d/%m"), " - ",
  format(max(dat$DATE), format = "%d/%m"), "   "
)


## plot
fig_map <- ggplot(map) +
  geom_sf(aes(fill = class)) +
  scale_fill_manual(values = c("white", "#ffe5e5", "#ff9999", "#ff0000"), drop = FALSE) +
  geom_text(
    data = points1, aes(x = X, y = Y + 0.03, label = PROVINCE), col = "black", size = 3,
    check_overlap = TRUE
  ) +
  geom_text(
    data = points1, aes(x = X, y = Y - 0.03, label = num), col = "black", size = 3,
    check_overlap = TRUE
  ) +
  geom_text(
    data = points2, aes(x = X + 0.07, y = Y + 0.09, label = PROVINCE), col = "black", size = 3,
    check_overlap = TRUE
  ) +
  geom_text(
    data = points2, aes(x = X + 0.07, y = Y + 0.03, label = num), col = "black", size = 3,
    check_overlap = TRUE
  ) +
  labs(fill = bquote(atop(NA, atop("Admissions hospitalières / \nHospitalisaties (x 1,000,000 hab./inw.)", bold(.(period)))))) +
  theme_void() +
  theme(
    # Change legend
    legend.position = c(0.2, 0.22),
    legend.title = element_text(size = 12, color = "black"),
    legend.text = element_text(color = "black"),
    plot.margin = unit(c(+0.2, 0, -0.5, 3), "cm")
  )


png(file = "Belgian_Hospitalisations_COVID-19_4weeks_limited.png", width = 15 * 360, heigh = 7 * 360, units = "px", pointsize = 7, res = 300)
ggarrange(fig_map,
  grid.arrange(fig_trends, bottom = caption),
  ncol = 2, widths = c(1, 1.5)
)
dev.off()
