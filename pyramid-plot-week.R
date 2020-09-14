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

# import data
dat_all <- read.csv("https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv",
  stringsAsFactors = TRUE
)

dat_all$DATE <- as.Date(dat_all$DATE, "%Y-%m-%d")

## Recoding dat_all$SEX into dat_all$SEX_rec
dat_all$SEX <- recode_factor(dat_all$SEX,
  "F" = "Female",
  "M" = "Male"
)

# subset for period
start <- as.Date("2020-03-01")
end <- as.Date("2020-05-31")
dat <- subset(dat_all, DATE >= start & DATE <= end)
nweeks <- as.numeric(end - start) / 7

# aggregate new cases by province and date
dat <- aggregate(CASES ~ AGEGROUP + SEX, dat, sum)

# cases per week
dat$CASES <- dat$CASES / nweeks

# create limits for number of cases for all periods
lower_limit <- -500
upper_limit <- 600

# Create plots in engl
p1 <- ggplot(data = dat) +
  geom_bar(aes(AGEGROUP, CASES, group = SEX, fill = SEX),
    stat = "identity",
    subset(dat, SEX == "Female")
  ) +
  geom_bar(aes(AGEGROUP, -CASES, group = SEX, fill = SEX),
    stat = "identity",
    subset(dat, SEX == "Male")
  ) +
  scale_y_continuous(
    limits = c(lower_limit, upper_limit),
    breaks = seq(lower_limit, upper_limit, 200),
    labels = abs(seq(lower_limit, upper_limit, 200))
  ) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "COVID-19 cases in Belgium",
    subtitle = paste0(format(start, format = "%d/%m/%Y"), " - ", format(end, format = "%d/%m/%Y")),
    # caption = "Niko Speybroeck (@NikoSpeybroeck), Antoine Soetewey (@statsandr) \n Data: https://epistat.wiv-isp.be/covid/",
    x = "Age group",
    y = "Number of cases per week"
  ) +
  theme(
    legend.position = c(.95, .15),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.title = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "bold")
  )

p1

# subset for period
start <- as.Date("2020-06-01")
end <- as.Date("2020-08-31")
dat <- subset(dat_all, DATE >= start & DATE <= end)
nweeks <- as.numeric(end - start) / 7

# aggregate new cases by province and date
dat <- aggregate(CASES ~ AGEGROUP + SEX, dat, sum)

# cases per week
dat$CASES <- dat$CASES / nweeks

# Create plots in engl
p2 <- ggplot(data = dat) +
  geom_bar(aes(AGEGROUP, CASES, group = SEX, fill = SEX),
    stat = "identity",
    subset(dat, SEX == "Female")
  ) +
  geom_bar(aes(AGEGROUP, -CASES, group = SEX, fill = SEX),
    stat = "identity",
    subset(dat, SEX == "Male")
  ) +
  scale_y_continuous(
    limits = c(lower_limit, upper_limit),
    breaks = seq(lower_limit, upper_limit, 200),
    labels = abs(seq(lower_limit, upper_limit, 200))
  ) +
  coord_flip() +
  theme_minimal() +
  labs( # title = "COVID-19 cases in Belgium",
    subtitle = paste0(format(start, format = "%d/%m/%Y"), " - ", format(end, format = "%d/%m/%Y")),
    # caption = "Niko Speybroeck (@NikoSpeybroeck), Antoine Soetewey (@statsandr) \n Data: https://epistat.wiv-isp.be/covid/",
    x = "Age group",
    y = "Number of cases per week"
  ) +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(face = "bold")
  )

p2

# subset for period
start <- as.Date("2020-09-01")
end <- as.Date(max(dat_all$DATE, na.rm = TRUE))
dat <- subset(dat_all, DATE >= start & DATE <= end)
nweeks <- as.numeric(end - start) / 7

# aggregate new cases by province and date
dat <- aggregate(CASES ~ AGEGROUP + SEX, dat, sum)

# cases per week
dat$CASES <- dat$CASES / nweeks

# Create plots in engl
p3 <- ggplot(data = dat) +
  geom_bar(aes(AGEGROUP, CASES, group = SEX, fill = SEX),
    stat = "identity",
    subset(dat, SEX == "Female")
  ) +
  geom_bar(aes(AGEGROUP, -CASES, group = SEX, fill = SEX),
    stat = "identity",
    subset(dat, SEX == "Male")
  ) +
  scale_y_continuous(
    limits = c(lower_limit, upper_limit),
    breaks = seq(lower_limit, upper_limit, 200),
    labels = abs(seq(lower_limit, upper_limit, 200))
  ) +
  coord_flip() +
  theme_minimal() +
  labs( # title = "COVID-19 cases in Belgium",
    subtitle = paste0(format(start, format = "%d/%m/%Y"), " - ", format(end, format = "%d/%m/%Y")),
    caption = "Niko Speybroeck (@NikoSpeybroeck), Antoine Soetewey (@statsandr) \n Data: https://epistat.wiv-isp.be/covid/",
    x = "Age group",
    y = "Number of cases per week"
  ) +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(face = "bold")
  )

p3

p1 + p2 + p3

# save plot
ggsave("pyramid-plot-week-limit.png")
# ggsave("pyramid-plot-week.pdf")
