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

# aggregate new cases by province and date
dat <- aggregate(CASES ~ DATE + PROVINCE, dat, sum)

# add new cases for Belgium as a whole
belgium <- aggregate(CASES ~ DATE, dat, sum)
belgium$PROVINCE <- "Belgium"
col_order <- c("DATE", "PROVINCE", "CASES")
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
    "Li\xe8ge",
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

# aggregate total number of cases per 100,000 inhabitants
dat_total <- aggregate(CASES_divid ~ PROVINCE, dat, sum)

# add total number of cases (/100000 inhabitants) per province
dat <- dat %>%
mutate(total = case_when(
  PROVINCE == "Antwerpen" ~ dat_total[1, 2],
  PROVINCE == "Brabant wallon" ~ dat_total[2, 2],
  PROVINCE == "Brussels" ~ dat_total[3, 2],
  PROVINCE == "Hainaut" ~ dat_total[4, 2],
  PROVINCE == "Liège" ~ dat_total[5, 2],
  PROVINCE == "Limburg" ~ dat_total[6, 2],
  PROVINCE == "Luxembourg" ~ dat_total[7, 2],
  PROVINCE == "Namur" ~ dat_total[8, 2],
  PROVINCE == "Oost-Vlaanderen" ~ dat_total[9, 2],
  PROVINCE == "Vlaams-Brabant" ~ dat_total[10, 2],
  PROVINCE == "West-Vlaanderen" ~ dat_total[11, 2],
  PROVINCE == "Belgique/België" ~ dat_total[12, 2]
))

# Create plot in ndls/fr
p <- ggplot(dat) +
  aes(x = DATE, weight = CASES_divid) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  facet_wrap(vars(PROVINCE),
             scales = "free_x") +
  geom_line(aes(y = rollmean(CASES_divid, 7, fill = NA)),
            color = "darkgrey", size = 0.5
  ) +
    labs(x = "", y = "Nouveaux cas confirmés (par 100,000 habitants) / Bevestigde nieuwe gevallen (per 100,000 inwoners)") +
  labs(
    title = "Evolution des nouveaux cas confirmés / Evolutie van nieuwe bevestigde gevallen - COVID-19"
  ) +
  scale_x_date(labels = date_format("%m-%Y")) +
  geom_text(
    data    = dat_total,
    mapping = aes(as.Date(max(dat$DATE) - 15), y=max(dat$CASES_divid, na.rm = TRUE) - 3,
                  label = paste0("Total: ", round(CASES_divid))),
    color="darkgrey",
    size = 3
  )

## adjust caption at the end of the trend figure
caption <- grobTree(
  textGrob(" * Ligne solide : moyenne mobile sur 7 jours / Volle lijnen : 7-daags voortschrijdend gemiddelde  \n * Barres : nouveaux cas confirmés / Staafdiagrammen : nieuwe bevestigde gevallen",
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
png(file = "new_cases_divid.png", width = 15 * 360, heigh = 7 * 360, units = "px", pointsize = 7, res = 300)
ggarrange(grid.arrange(p, bottom = caption),
          ncol = 1, widths = c(1, 1.5)
)
dev.off()
