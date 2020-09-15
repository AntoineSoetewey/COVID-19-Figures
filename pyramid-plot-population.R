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

options(scipen = 99)

# import data
dat_all <- read.csv("TF_SOC_POP_STRUCT_2020.txt",
                    sep = ";",
                    stringsAsFactors = TRUE
)

dat <- dat_all[, c("CD_AGE", "CD_SEX", "MS_POPULATION")]

## Cutting dat$CD_AGE into dat$CD_AGEGROUP
dat$CD_AGEGROUP <- cut(dat$CD_AGE,
  include.lowest = TRUE,
  right = FALSE,
  breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 120)
)
## Recoding dat$CD_AGEGROUP
dat$CD_AGEGROUP <- recode_factor(dat$CD_AGEGROUP,
  "[0,10)" = "0-9",
  "[10,20)" = "10-19",
  "[20,30)" = "20-29",
  "[30,40)" = "30-39",
  "[40,50)" = "40-49",
  "[50,60)" = "50-59",
  "[60,70)" = "60-69",
  "[70,80)" = "70-79",
  "[80,90)" = "80-89",
  "[90,120]" = "90+"
)
View(dat)

## Recoding dat_all$SEX into dat_all$SEX_rec
dat$CD_SEX <- recode_factor(dat$CD_SEX,
  "F" = "Female",
  "M" = "Male"
)

names(dat) <- c("AGE", "SEX", "POPULATION", "AGEGROUP")

# aggregate new cases by age group and sex
dat <- aggregate(POPULATION ~ AGEGROUP + SEX, dat, sum)

# Create plots in engl
p <- ggplot(data = dat) +
  geom_bar(aes(AGEGROUP, POPULATION, group = SEX, fill = SEX),
    stat = "identity",
    subset(dat, SEX == "Female")
  ) +
  geom_bar(aes(AGEGROUP, -POPULATION, group = SEX, fill = SEX),
    stat = "identity",
    subset(dat, SEX == "Male")
  ) +
  scale_y_continuous(
    breaks = seq(-round_any(max(dat$POPULATION), 100000, f = ceiling), round_any(max(dat$POPULATION), 1000, f = ceiling), 200000),
    labels = abs(seq(-round_any(max(dat$POPULATION), 100000, f = ceiling), round_any(max(dat$POPULATION), 1000, f = ceiling), 200000))
  ) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Population structure in Belgium",
    subtitle = "2019",
    caption = "Niko Speybroeck (@NikoSpeybroeck), Antoine Soetewey (@statsandr) \n Data: Statbel",
    x = "Age group",
    y = "Population"
  ) +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.title = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "bold")
  )

p

# save plot
ggsave("pyramid-plot-population.png")
# ggsave("pyramid-plot.pdf")
