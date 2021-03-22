# clean environment
remove(list = ls())
# required packages
library(ggplot2)

############
# PROVINCE #
############

# import data
dat <- read.csv("https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv")

dat <- select(dat, c("DATE", "PROVINCE", "CASES"))

dat <- transform(dat,
          DATE = as.Date(DATE)
)

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
                         "WestVlaanderen"
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
                         "West-Vlaanderen"
                       )
)

max_date <- max(as.Date(dat$DATE), na.rm = TRUE)
dat <- subset(dat, DATE >= max_date - 14)

dat$week <- ifelse(dat$DATE <= max_date - 7,
                   "week1",
                   "week2")

belgium <- aggregate(CASES ~ week, dat, sum)

dat_week1 <- subset(dat, week == "week1")
dat_week1 <- aggregate(CASES ~ week + PROVINCE, dat_week1, sum)
dat_week2 <- subset(dat, week == "week2")
dat_week2 <- aggregate(CASES ~ week + PROVINCE, dat_week2, sum)

dat_diff <- data.frame(PROVINCE = dat_week1$PROVINCE,
                       CASES_diff = dat_week2$CASES - dat_week1$CASES)
dat_diff$contrib_change <- dat_diff$CASES_diff / (belgium[2, 2] - belgium[1, 2])

dat_diff$contrib_level <- dat_week2$CASES / (belgium[2, 2])

dat <- dat_diff

p <- ggplot(dat, aes(x = contrib_level * 100, y = contrib_change * 100)) +
  geom_point(color = "red") +
  geom_text_repel(aes(label = PROVINCE)) +
  theme_minimal() +
  labs(
    x = "% contribution to change",
    y = "% contribution to level"
    ) +
  NULL
p

## adjust caption at the end of the trend figure
caption <- grobTree(
  # textGrob(" * Ligne solide : moyenne mobile sur 7 jours / Volle lijnen : 7-daags voortschrijdend gemiddelde",
  #          x = 0, hjust = 0, vjust = 0,
  #          gp = gpar(col = "darkgray", fontsize = 7, lineheight = 1.2)
  # ),
  textGrob("Niko Speybroeck (@NikoSpeybroeck), Antoine Soetewey (@statsandr) & Angel Rosas (@arosas_aguirre) \n Data: https://epistat.wiv-isp.be/covid/  ",
           x = 1, hjust = 1, vjust = 0,
           gp = gpar(col = "black", fontsize = 7.5, lineheight = 1.2)
  ),
  cl = "ann"
)

# save plot
png(file = "contrib_provinces.png", width = 15 * 360, heigh = 7 * 360, units = "px", pointsize = 7, res = 300)
ggarrange(grid.arrange(p, bottom = caption),
          ncol = 1, widths = c(1, 1.5)
)
dev.off()

#############
# AGE GROUP #
#############

# import data
dat <- read.csv("https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv")

dat <- select(dat, c("DATE", "AGEGROUP", "CASES"))

dat <- transform(dat,
                 DATE = as.Date(DATE),
                 AGEGROUP = factor(AGEGROUP)
)

max_date <- max(as.Date(dat$DATE), na.rm = TRUE)
dat <- subset(dat, DATE >= max_date - 14)

dat$week <- ifelse(dat$DATE <= max_date - 7,
                   "week1",
                   "week2")

belgium <- aggregate(CASES ~ week, dat, sum)

dat_week1 <- subset(dat, week == "week1")
dat_week1 <- aggregate(CASES ~ week + AGEGROUP, dat_week1, sum)
dat_week2 <- subset(dat, week == "week2")
dat_week2 <- aggregate(CASES ~ week + AGEGROUP, dat_week2, sum)

dat_diff <- data.frame(AGEGROUP = dat_week1$AGEGROUP,
                       CASES_diff = dat_week2$CASES - dat_week1$CASES)
dat_diff$contrib_change <- dat_diff$CASES_diff / (belgium[2, 2] - belgium[1, 2])

dat_diff$contrib_level <- dat_week2$CASES / (belgium[2, 2])

dat <- dat_diff

p <- ggplot(dat, aes(x = contrib_level * 100, y = contrib_change * 100)) +
  geom_point(color = "red") +
  geom_text_repel(aes(label = AGEGROUP)) +
  theme_minimal() +
  labs(
    x = "% contribution to change",
    y = "% contribution to level"
  ) +
  NULL
p

## adjust caption at the end of the trend figure
caption <- grobTree(
  # textGrob(" * Ligne solide : moyenne mobile sur 7 jours / Volle lijnen : 7-daags voortschrijdend gemiddelde",
  #          x = 0, hjust = 0, vjust = 0,
  #          gp = gpar(col = "darkgray", fontsize = 7, lineheight = 1.2)
  # ),
  textGrob("Niko Speybroeck (@NikoSpeybroeck), Antoine Soetewey (@statsandr) & Angel Rosas (@arosas_aguirre) \n Data: https://epistat.wiv-isp.be/covid/  ",
           x = 1, hjust = 1, vjust = 0,
           gp = gpar(col = "black", fontsize = 7.5, lineheight = 1.2)
  ),
  cl = "ann"
)

# save plot
png(file = "contrib_age.png", width = 15 * 360, heigh = 7 * 360, units = "px", pointsize = 7, res = 300)
ggarrange(grid.arrange(p, bottom = caption),
          ncol = 1, widths = c(1, 1.5)
)
dev.off()
