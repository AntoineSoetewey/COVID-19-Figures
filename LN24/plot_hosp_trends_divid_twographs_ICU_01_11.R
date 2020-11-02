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


# import Sciensano hospitalizations data
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

dat <- aggregate(TOTAL_IN_ICU ~ DATE + PROVINCE, dat, sum)


# add new intakes for Belgium as a whole

belgium <- aggregate(TOTAL_IN_ICU ~ DATE, dat, sum) %>%
  mutate(PROVINCE = "Belgium") %>%
  select(DATE, PROVINCE, TOTAL_IN_ICU)

##

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
    TOTAL_IN_ICU_divid = TOTAL_IN_ICU / population * 100000
  )

dat$PROVINCE <- relevel(as.factor(dat$PROVINCE), ref = "Belgium")

# choose period
subdat <- subset(dat, DATE >= "2020-09-17")

# Create plot in French
fig_trends <- ggplot(
  subdat,
  aes(x = DATE, y = TOTAL_IN_ICU_divid)
) +
  geom_vline(
    xintercept = as.Date("2020-09-28"), linetype = "dashed",
    color = "lightgrey", size = 0.7
  ) +
  geom_vline(
    xintercept = as.Date("2020-10-04"), linetype = "dashed",
    color = "lightgrey", size = 0.7
  ) +
  geom_vline(
    xintercept = as.Date("2020-10-23"), linetype = "dashed",
    color = "lightgrey", size = 0.7
  ) +
  geom_vline(
    xintercept = as.Date("2020-10-30"), linetype = "dashed",
    color = "lightgrey", size = 0.7
  ) +
  # annotate("rect",
  #   ymin = -Inf, ymax = 0.5,
  #   xmin = as.Date(-Inf), xmax = as.Date(Inf),
  #   alpha = .05
  # ) +
  # annotate("rect",
  #   ymin = 0.5, ymax = 1,
  #   xmin = as.Date(-Inf), xmax = as.Date(Inf),
  #   alpha = .1
  # ) +
  annotate("rect",
    ymin = 2000 / 11431406 * 100000, ymax = Inf,
    xmin = as.Date(-Inf), xmax = as.Date(Inf),
    alpha = .05,
  ) +
  geom_point(
    size = 1L,
    colour = "lightblue",
    shape = 1
  ) +
  labs(x = "", y = "Patients en soins intensifs (par 100,00 habitants)") +
  theme_minimal() +
  facet_wrap(vars(PROVINCE),
    scales = "free",
    ncol = 5
  ) +
  geom_smooth(
    se = FALSE,
    col = "grey",
    method = "gam",
    formula = y ~ s(x)
  ) +
  labs(
    title = "Evolution des patients en soins intensifs en Belgique - COVID-19"
  ) +
  scale_y_continuous(breaks = seq(from = 0, to = max(subdat$TOTAL_IN_ICU_divid), by = 2), limits = c(0, max(subdat$TOTAL_IN_ICU_divid))) +
  scale_x_date(labels = date_format("%d/%m"), date_breaks = "2 weeks") +
  theme(
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(5.5, 5.5, 20, 5.5), "points")
  )

## adjust caption at the end of the trend figure
caption <- grobTree(
  textGrob("* Lignes solides: courbes adaptées aux observations",
    x = 0, hjust = 0, vjust = 0,
    gp = gpar(col = "darkgray", fontsize = 8, lineheight = 1.2)
  ),
  textGrob("Niko Speybroeck (@NikoSpeybroeck), Antoine Soetewey (@statsandr) & Angel Rosas (@arosas_aguirre) \n Données: https://epistat.wiv-isp.be/covid/  ",
    x = 1, hjust = 1, vjust = 0,
    gp = gpar(col = "black", fontsize = 10, lineheight = 1.2)
  ),
  cl = "ann"
)


##### MAPS

### Obtaining Belgium shapefile at province level

library(GADMTools)
library(RColorBrewer)
library(tmap)
library(sf)

maxi <- max(dat$DATE)
mini <- max(dat$DATE) - 13
divi <- length(mini:maxi)

# agregated data to join with the map
# calculating the daily rate in two periods :
## 1. last week of March- first week of April (14 days)
## 2. Last 14 days reported by Scienciano

dat$PROVINCE <- as.character(dat$PROVINCE)


dat_ag <- filter(dat, PROVINCE != "Belgium") %>%
  group_by(PROVINCE) %>%
  summarize(
    "per1" = sum(TOTAL_IN_ICU_divid[DATE >= as.Date("2020-03-25") & DATE <= as.Date("2020-04-07")], na.rm = T) / 14,
    "per2" = sum(TOTAL_IN_ICU_divid[DATE >= mini & DATE <= maxi], na.rm = T) / divi
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
                 breaks = c(0, 0.5, 1.0, 1.5, 3, 5, 99),
                 include.lowest = TRUE,
                 labels = c("[ 0.0, 0.5 ]", "] 0.5, 1.0 ]", "] 1.0, 1.5 ]", "] 1.5, 3.0 ]", "] 3.0, 5.0]", " > 5.0")
    ),
    class2 = cut(per2,
                 breaks = c(0, 0.5, 1.0, 1.5, 3, 5, 99),
                 include.lowest = TRUE,
                 labels = c("[ 0.0, 0.5 ]", "] 0.5, 1.0 ]", "] 1.0, 1.5 ]", "] 1.5, 3.0 ]", "] 3.0, 5.0]", " > 5.0")
    )
      )





###### MAPS WITH GGPLOT

points <- st_centroid(map)
points <- cbind(map, st_coordinates(st_centroid(map$geometry)))

points <- mutate(points,
  num_1 = paste("(", format(round(per1, 2), nsmall = 2), ")"),
  num_2 = paste("(", format(round(per2, 2), nsmall = 2), ")")
)


period1 <- paste0("Période: 25/03 - 07/04", "   ")
period2 <- paste0(
  "Période: ", format(mini, format = "%d/%m"), " - ",
  format(maxi, format = "%d/%m"), "   "
)


library(RColorBrewer)
reds <- brewer.pal(7, "Reds")
blues <- brewer.pal(7, "Blues")


map1 <- ggplot(map) +
  geom_sf(aes(fill = class1)) +
  scale_fill_manual(values = blues, drop = FALSE) +
  geom_text(
    data = points, aes(x = X, y = Y + 0.06, label = PROVINCE), col = "black", size = 2.4,  nudge_x = -0.07,
    check_overlap = TRUE
  ) +
  geom_text(
    data = points, aes(x = X, y = Y, label = num_1), col = "black", size = 3,  nudge_x = -0.07,
    check_overlap = TRUE
  ) +
  labs(fill = bquote(atop(NA, atop("Nombre moyen de patients\nen USI (x100,000 hab.)", bold(.(period1)))))) +
  theme_void() +
  theme(
    # Change legend
    legend.position = c(0.18, 0.2),
    legend.key.size = unit(0.9, "line"),
    legend.title = element_text(size = 12, color = "black"),
    legend.text = element_text(color = "black"),
    plot.margin = unit(c(+0.2, 0, +0.5, 3), "cm")
  )


map2 <- ggplot(map) +
  geom_sf(aes(fill = class2)) +
  scale_fill_manual(values = blues, drop = FALSE) +
  geom_text(
    data = points, aes(x = X, y = Y + 0.06, label = PROVINCE), col = "black", size = 2.4,  nudge_x = -0.07,
    check_overlap = TRUE
  ) +
  geom_text(
    data = points, aes(x = X, y = Y, label = num_2), col = "black", size = 3, nudge_x = -0.07,
    check_overlap = TRUE
  ) +
  labs(fill = bquote(atop(NA, atop("Nombre moyen de patients\nen USI (x100,000 hab.)", bold(.(period2)))))) +
  theme_void() +
  theme(
    # Change legend
    legend.position = c(0.18, 0.2),
    legend.key.size = unit(0.9, "line"),
    legend.title = element_text(size = 12, color = "black"),
    legend.text = element_text(color = "black"),
    plot.margin = unit(c(+0.2, 0, +0.5, 3), "cm")
  )

# save plot
png(file = "Belgian_ICU_0111.png", width = 15 * 360, height = 7 * 360, units = "px", pointsize = 7, res = 300)
ggarrange(ggarrange(map1, map2, ncol = 1),
  grid.arrange(fig_trends, bottom = caption),
  ncol = 2, widths = c(1, 1.5)
)
dev.off()
