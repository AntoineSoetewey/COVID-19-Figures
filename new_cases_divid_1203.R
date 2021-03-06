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

# import data
dat <- read.csv("https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv")

## Recoding dat$AGEGROUP
dat$AGEGROUP <- recode_factor(dat$AGEGROUP,
                              "40-49" = "30-59",
                              "10-19" = "0-19",
                              "30-39" = "30-59",
                              "50-59" = "30-59",
                              "70-79" = "60-79",
                              "60-69" = "60-79",
                              "0-9" = "0-19",
                              "90+" = "80+",
                              "80-89" = "80+"
)

## Reordering dat$AGEGROUP
dat$AGEGROUP <- factor(dat$AGEGROUP,
  levels = c("0-19", "20-29", "30-59", "60-79", "80+")
)

# aggregate new cases by province and date
dat <- aggregate(CASES ~ DATE + PROVINCE + AGEGROUP, dat, sum)

# add new cases for Belgium as a whole
belgium <- aggregate(CASES ~ DATE + AGEGROUP, dat, sum)
belgium$PROVINCE <- "Belgium"
col_order <- c("DATE", "PROVINCE", "AGEGROUP", "CASES")
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
    "Brabant wallon",
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

# compute CASES by population size
dat <- dat %>%
  mutate(population = case_when(
    PROVINCE == "Antwerpen" ~ 1857986,
    PROVINCE == "Brabant wallon" ~ 403599,
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
  mutate(CASES_divid = CASES / population * 100000)

# Create plot in dutch/fr
p <- dat %>%
  filter(DATE >= "2020-12-01" & DATE <= max(dat$DATE)) %>%
  ggplot() +
  aes(x = DATE, weight = CASES, color = AGEGROUP, linetype = AGEGROUP) +
  # geom_bar(fill = "steelblue") +
  theme_minimal() +
  facet_wrap(vars(PROVINCE),
             scales = "free") +
  geom_line(aes(y = rollmean(CASES, 7, fill = NA)), size = 0.5
  ) +
    labs(x = "", y = "Nouveaux cas confirmés / Bevestigde nieuwe gevallen") +
  labs(
    title = "Evolution des nouveaux cas confirmés / Evolutie van nieuwe bevestigde gevallen - COVID-19",
    color = "Age/Leeftijd"
  ) +
  scale_x_date(labels = date_format("%b %d")) +
  scale_linetype_manual("Age", values=1:5) +
  scale_color_manual("Age", values = hue_pal()(5)) +
  NULL
p

## adjust caption at the end of the trend figure
caption <- grobTree(
  textGrob(" * Ligne solide : moyenne mobile sur 7 jours / Volle lijnen : 7-daags voortschrijdend gemiddelde",
           x = 0, hjust = 0, vjust = 0,
           gp = gpar(col = "darkgray", fontsize = 7, lineheight = 1.2)
  ),
  textGrob("Niko Speybroeck (@NikoSpeybroeck), Antoine Soetewey (@statsandr) & Angel Rosas (@arosas_aguirre) \n Data: https://epistat.wiv-isp.be/covid/  ",
           x = 1, hjust = 1, vjust = 0,
           gp = gpar(col = "black", fontsize = 7.5, lineheight = 1.2)
  ),
  cl = "ann"
)

# save plot
png(file = "new_cases_divid_1203_age.png", width = 15 * 360, heigh = 7 * 360, units = "px", pointsize = 7, res = 300)
ggarrange(grid.arrange(p, bottom = caption),
          ncol = 1, widths = c(1, 1.5)
)
dev.off()


# Create plot in dutch/fr

p <- dat %>%
  filter(DATE >= "2020-12-01" & DATE <= max(dat$DATE) & PROVINCE != "Belgique/België") %>%
  ggplot() +
  aes(x = DATE, weight = CASES, color = PROVINCE) +
  # geom_bar(fill = "steelblue") +
  theme_minimal() +
  facet_wrap(vars(AGEGROUP),
             scales = "free") +
  geom_line(aes(y = rollmean(CASES, 7, fill = NA)), size = 0.5
  ) +
  labs(x = "", y = "Nouveaux cas confirmés / Bevestigde nieuwe gevallen") +
  labs(
    title = "Evolution des nouveaux cas confirmés / Evolutie van nieuwe bevestigde gevallen - COVID-19",
    color = ""
  ) +
  scale_x_date(labels = date_format("%b %d")) +
  scale_color_manual(values = c("darkgray", "red", rep("darkgray", 9))) +
  # scale_color_manual(values = c("darkgray", "red"), labels = c("Autres", "BW")) + 
  NULL
p

# save plot
png(file = "new_cases_divid_1203_province.png", width = 15 * 360, heigh = 7 * 360, units = "px", pointsize = 7, res = 300)
ggarrange(grid.arrange(p, bottom = caption),
          ncol = 1, widths = c(1, 1.5)
)
dev.off()
