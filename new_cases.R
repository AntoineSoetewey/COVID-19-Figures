# required packages
library(ggplot2)
library(ggrepel)
library(zoo)
library(lme4)
library(dplyr)

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
                       levels = c("Antwerpen",
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
                                  "Belgium"),
                       labels = c("Antwerpen",
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
                                  "Belgium"))

# Create plot in english
ggplot(dat) +
  aes(x = DATE, weight = CASES) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  facet_wrap(vars(PROVINCE), scales = "free_y") +
  geom_line(aes(y = rollmean(CASES, 5, fill = NA))
            , color = "#0c4c8a") +
  labs(x = "", y = "Confirmed new cases") +
  labs(
    title = "Evolution of daily confirmed new cases (5-day moving average) - COVID-19",
    caption = "Niko Speybroeck (@NikoSpeybroeck), Antoine Soetewey (@statsandr) & Angel Rosas (@am_rosasa)\n
    Data: https://epistat.wiv-isp.be/covid/")

# Create plot in ndls/fr
ggplot(dat) +
  aes(x = DATE, weight = CASES) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  facet_wrap(vars(PROVINCE), scales = "free_y") +
  geom_line(aes(y = rollmean(CASES, 5, fill = NA))
            , color = "#0c4c8a") +
  labs(x = "", y = "Nouveaux cas confirmés / Bevestigde nieuwe gevallen") +
  labs(
    title = "Evolution des nouveaux cas confirmés / Evolutie van nieuwe bevestigde gevallen - COVID-19",
    caption = "Niko Speybroeck (@NikoSpeybroeck), Antoine Soetewey (@statsandr) & Angel Rosas (@am_rosasa)\n
    Data: https://epistat.wiv-isp.be/covid/")
