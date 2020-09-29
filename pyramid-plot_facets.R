# clean environment
remove(list = ls())
# required packages
library(ggplot2)
library(ggrepel)
library(zoo)
library(lme4)
library(plyr)
library(dplyr)
library(scales)
library(ggpubr)
library(grid)
library(gridExtra)
library(magrittr)
library(ggpol)
library(reshape2)
library(XML)
library(patchwork)

# import Sciensano hospitalisations data
dat <- read.csv("https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv",
  fileEncoding = "UTF-8",
  stringsAsFactors = FALSE
)

# aggregate new intakes by province and date
dat <- dat %>%
  mutate(
    DATE = as.Date(DATE),
    SEX = factor(SEX, labels = c("Women", "Men")),
    AGEGROUP = as.factor(AGEGROUP),
    PROVINCE2 = case_when(
      PROVINCE %in% c("BrabantWallon", "VlaamsBrabant", "Brussels") ~ "Brabant",
      !PROVINCE %in% c("BrabantWallon", "VlaamsBrabant", "Brussels") ~ PROVINCE
    ),
    PROVINCE2 = case_when(
      PROVINCE == "OostVlaanderen" ~ "Oost-Vlaanderen",
      PROVINCE == "WestVlaanderen" ~ "West-Vlaanderen",
      !PROVINCE %in% c("OostVlaanderen", "WestVlaanderen") ~ PROVINCE2
    ),
    PROVINCE = as.factor(PROVINCE2)
  ) %>%
  select(DATE, PROVINCE, AGEGROUP, SEX, CASES)

# add new intakes for Belgium as a whole
belgium <- aggregate(CASES ~ DATE + AGEGROUP + SEX, dat, sum) %>%
  mutate(PROVINCE = "Belgium") %>%
  select(DATE, PROVINCE, AGEGROUP, SEX, CASES)

##
dat_all <- rbind(dat, belgium) %>%
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
    CASES_divid = CASES / population * 100000
  )
dat_all <- dat_all[!is.na(dat_all$AGEGROUP), ]


# subset for period and province
start <- as.Date("2020-03-01")
end <- as.Date("2020-05-31")
dat <- subset(dat_all, DATE >= start & DATE <= end & PROVINCE == "Belgium")

lab <- aggregate(CASES ~ AGEGROUP + SEX, dat, sum)

# plot for Belgium
bel_p1 <- ggplot(data = dat) +
  geom_bar(aes(AGEGROUP, CASES, group = SEX, fill = SEX),
    stat = "identity",
    subset(dat, SEX == "Women")
  ) +
  geom_bar(aes(AGEGROUP, -CASES, group = SEX, fill = SEX),
    stat = "identity",
    subset(dat, SEX == "Men")
  ) +
  scale_y_continuous(
    breaks = seq(-round_any(max(lab$CASES), 1000, f = ceiling), round_any(max(lab$CASES), 1000, f = ceiling), 2000),
    labels = abs(seq(-round_any(max(lab$CASES), 1000, f = ceiling), round_any(max(lab$CASES), 1000, f = ceiling), 2000))
  ) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Belgium",
    subtitle = paste0(format(start, format = "%d/%m/%Y"), " - ", format(end, format = "%d/%m/%Y")),
    x = "Age group",
    y = "Number of cases"
  ) +
  theme(
    legend.position = c(.95, .15),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(5.5, 5.5, 30, 5.5), "points")
  )

## plots for provinces
dat <- subset(dat_all, DATE >= start & DATE <= end & PROVINCE != "Belgium")

lab <- aggregate(CASES ~ AGEGROUP + SEX, dat, sum)

pro_p1 <- ggplot(data = dat) +
  facet_wrap(vars(PROVINCE),
    scales = "fixed",
    ncol = 9
  ) +
  geom_bar(aes(AGEGROUP, CASES, group = SEX, fill = SEX),
    stat = "identity",
    subset(dat, SEX == "Women")
  ) +
  geom_bar(aes(AGEGROUP, -CASES, group = SEX, fill = SEX),
    stat = "identity",
    subset(dat, SEX == "Men")
  ) +
  scale_y_continuous(
    breaks = seq(-round_any(max(lab$CASES), 500, f = ceiling), round_any(max(lab$CASES), 500, f = ceiling), 500),
    labels = abs(seq(-round_any(max(lab$CASES), 500, f = ceiling), round_any(max(lab$CASES), 500, f = ceiling), 500))
  ) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Axe and sex specific COVID-19 cases in Belgium",
    subtitle = paste0(format(start, format = "%d/%m/%Y"), " - ", format(end, format = "%d/%m/%Y")),
    x = "Age group",
    y = "Number of cases"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(5.5, 5.5, 0, 5.5), "points")
  )




# subset for period and province
start <- as.Date("2020-06-01")
end <- as.Date("2020-08-31")
dat <- subset(dat_all, DATE >= start & DATE <= end & PROVINCE == "Belgium")

lab <- aggregate(CASES ~ AGEGROUP + SEX, dat, sum)

# plot for Belgium
bel_p2 <- ggplot(data = dat) +
  geom_bar(aes(AGEGROUP, CASES, group = SEX, fill = SEX),
    stat = "identity",
    subset(dat, SEX == "Women")
  ) +
  geom_bar(aes(AGEGROUP, -CASES, group = SEX, fill = SEX),
    stat = "identity",
    subset(dat, SEX == "Men")
  ) +
  scale_y_continuous(
    breaks = seq(-round_any(max(lab$CASES), 1000, f = ceiling), round_any(max(lab$CASES), 1000, f = ceiling), 500),
    labels = abs(seq(-round_any(max(lab$CASES), 1000, f = ceiling), round_any(max(lab$CASES), 1000, f = ceiling), 500))
  ) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "",
    subtitle = paste0(format(start, format = "%d/%m/%Y"), " - ", format(end, format = "%d/%m/%Y")),
    x = "",
    y = "Number of cases"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(5.5, 5.5, 30, 5.5), "points")
  )

## plots for provinces
dat <- subset(dat_all, DATE >= start & DATE <= end & PROVINCE != "Belgium")

lab <- aggregate(CASES ~ AGEGROUP + SEX, dat, sum)

pro_p2 <- ggplot(data = dat) +
  facet_wrap(vars(PROVINCE),
    scales = "fixed",
    ncol = 9
  ) +
  geom_bar(aes(AGEGROUP, CASES, group = SEX, fill = SEX),
    stat = "identity",
    subset(dat, SEX == "Women")
  ) +
  geom_bar(aes(AGEGROUP, -CASES, group = SEX, fill = SEX),
    stat = "identity",
    subset(dat, SEX == "Men")
  ) +
  scale_y_continuous(
    breaks = seq(-round_any(max(lab$CASES), 300, f = ceiling), round_any(max(lab$CASES), 300, f = ceiling), 300),
    labels = abs(seq(-round_any(max(lab$CASES), 300, f = ceiling), round_any(max(lab$CASES), 300, f = ceiling), 300))
  ) +
  coord_flip() +
  theme_minimal() +
  labs(
    subtitle = paste0(format(start, format = "%d/%m/%Y"), " - ", format(end, format = "%d/%m/%Y")),
    x = "Age group",
    y = "Number of cases"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(5.5, 5.5, 0, 5.5), "points")
  )




# subset for period and province
start <- as.Date("2020-09-01")
end <- as.Date(max(dat_all$DATE, na.rm = TRUE))
dat <- subset(dat_all, DATE >= start & DATE <= end & PROVINCE == "Belgium")

lab <- aggregate(CASES ~ AGEGROUP + SEX, dat, sum)

# plot for Belgium
bel_p3 <- ggplot(data = dat) +
  geom_bar(aes(AGEGROUP, CASES, group = SEX, fill = SEX),
    stat = "identity",
    subset(dat, SEX == "Women")
  ) +
  geom_bar(aes(AGEGROUP, -CASES, group = SEX, fill = SEX),
    stat = "identity",
    subset(dat, SEX == "Men")
  ) +
  scale_y_continuous(
    breaks = seq(-round_any(max(lab$CASES), 1000, f = ceiling), round_any(max(lab$CASES), 1000, f = ceiling), 1000),
    labels = abs(seq(-round_any(max(lab$CASES), 1000, f = ceiling), round_any(max(lab$CASES), 1000, f = ceiling), 1000))
  ) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "",
    subtitle = paste0(format(start, format = "%d/%m/%Y"), " - ", format(end, format = "%d/%m/%Y")),
    x = "",
    y = "Number of cases"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(5.5, 5.5, 30, 5.5), "points")
  )

## adjust caption at the end of the figure
caption <- grobTree(
  textGrob("Niko Speybroeck (@NikoSpeybroeck), Antoine Soetewey (@statsandr) & Angel Rosas (@arosas_aguirre) \n Data: https://epistat.wiv-isp.be/covid/  ",
    x = 1, hjust = 1, vjust = 0,
    gp = gpar(col = "black", fontsize = 11, lineheight = 1.2)
  ),
  cl = "ann"
)

## plots for provinces
dat <- subset(dat_all, DATE >= start & DATE <= end & PROVINCE != "Belgium")

lab <- aggregate(CASES ~ AGEGROUP + SEX, dat, sum)

pro_p3 <- ggplot(data = dat) +
  facet_wrap(vars(PROVINCE),
    scales = "fixed",
    ncol = 9
  ) +
  geom_bar(aes(AGEGROUP, CASES, group = SEX, fill = SEX),
    stat = "identity",
    subset(dat, SEX == "Women")
  ) +
  geom_bar(aes(AGEGROUP, -CASES, group = SEX, fill = SEX),
    stat = "identity",
    subset(dat, SEX == "Men")
  ) +
  scale_y_continuous(
    breaks = seq(-round_any(max(lab$CASES), 500, f = ceiling), round_any(max(lab$CASES), 500, f = ceiling), 500),
    labels = abs(seq(-round_any(max(lab$CASES), 500, f = ceiling), round_any(max(lab$CASES), 500, f = ceiling), 500))
  ) +
  coord_flip() +
  theme_minimal() +
  labs(
    subtitle = paste0(format(start, format = "%d/%m/%Y"), " - ", format(end, format = "%d/%m/%Y")),
    x = "Age group",
    y = "Number of cases"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(5.5, 5.5, 0, 5.5), "points")
  )

# save plot
png(file = "pyramid-plot_facets.png", width = 25.71428 * 360, height = 12 * 360, units = "px", pointsize = 7, res = 300)
ggarrange(ggarrange(pro_p1, pro_p2, pro_p3, ncol = 1),
  grid.arrange(bel_p1, bel_p2, bel_p3, bottom = caption, ncol = 3),
  ncol = 1, heights = c(2, 1)
)
dev.off()
