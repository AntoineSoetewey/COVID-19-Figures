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
  mutate(NEW_IN_divid = NEW_IN / population * 100000)

# create ratio of last n days to previous n days
n <- 7
dat_t7 <- dat %>%
  group_by(PROVINCE) %>%
  slice((n() - (n - 1)):n())

dat_t14 <- dat %>%
  group_by(PROVINCE) %>%
  slice((n() - (2 * n - 1)):(n() - n))

dat_ratio <- data.frame(aggregate(NEW_IN ~ PROVINCE, dat_t7, sum),
  NEW_IN_14 = aggregate(NEW_IN ~ PROVINCE, dat_t14, sum)[, 2]
)
dat_ratio$ratio <- dat_ratio$NEW_IN / dat_ratio$NEW_IN_14

# create dataframe
dat_t <- rbind(dat_t7, dat_t14)
grp <- c(rep(1, 84), rep(2, 84))
dat_t <- data.frame(dat_t[, 2:4], grp = as.factor(grp))

# fit models
fitted_models <- dat_t %>%
  group_by(PROVINCE) %>%
  do(model = glm(NEW_IN ~ grp, data = ., family = poisson))

fitted_models_tidy <- fitted_models %>%
  tidy(model)
# View(fitted_models_tidy)

# add p-values of fitted models to dataframe
dat_ratio <- cbind(
  dat_ratio,
  fitted_models_tidy[seq(from = 2, to = 24, by = 2), 6]
)

# create means for the 2 periods
dat_7mean <- aggregate(NEW_IN_divid ~ PROVINCE, dat_t7, mean)
dat_14mean <- aggregate(NEW_IN_divid ~ PROVINCE, dat_t14, mean)


# Create plot in dutch/fr
fig_trends <- ggplot(
  subset(dat, DATE >= "2020-05-01"), # subset data from May 1
  aes(x = DATE, y = NEW_IN_divid)
) +
  geom_point(
    size = 1L,
    colour = "steelblue"
  ) +
  labs(x = "", y = "Nombre d'hospitalisations (par 100,000 habitants) / Hospitalisaties (per 100,000 inwoners)") +
  theme_minimal() +
  facet_wrap(vars(PROVINCE),
    scales = "free"
  ) +
  geom_vline(
    xintercept = as.Date("2020-05-04"), linetype = "dashed",
    color = "darkgrey", size = 0.5
  ) +
  geom_text(aes(x = as.Date("2020-05-04"), label = "1a", y = 3),
    colour = "darkgrey", hjust = -0.1,
  ) +
  geom_vline(
    xintercept = as.Date("2020-05-11"), linetype = "dashed",
    color = "darkgrey", size = 0.5
  ) +
  geom_text(aes(x = as.Date("2020-05-11"), label = "1b", y = 3),
    colour = "darkgrey", hjust = -0.1,
  ) +
  geom_vline(
    xintercept = as.Date("2020-05-18"), linetype = "dashed",
    color = "darkgrey", size = 0.5
  ) +
  geom_text(aes(x = as.Date("2020-05-18"), label = "2", y = 3),
    colour = "darkgrey", hjust = -0.1,
  ) +
  annotate("rect",
    ymin = -Inf, ymax = Inf,
    xmin = as.Date("2020-05-01"), xmax = as.Date("2020-06-01"),
    alpha = 0
  ) +
  annotate("rect",
    ymin = -Inf, ymax = Inf,
    xmin = as.Date("2020-06-01"), xmax = as.Date("2020-06-05"),
    alpha = 0
  ) +
  labs(
    title = "Evolution des admissions hospitalières / Evolutie van de hospitalisaties - COVID-19"
  ) +
  scale_y_continuous(breaks = seq(from = 0, to = 10, by = 1), limits = c(0, 3)) +
  scale_x_date(labels = date_format("%d-%m")) +
  # geom_text(
  #   data = dat_ratio,
  #   mapping = aes(
  #     x = Sys.Date() + 3, y = 2.5,
  #     label = ifelse(p.value < 0.05, paste0("Change: ", round(ratio, 2)), NA)
  #   ),
  #   color = "darkgrey",
  #   size = 4
  # ) +
  geom_hline(
    yintercept = 1.1,
    linetype = "dotted",
    color = "darkgrey"
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
  geom_text(
    data = dat_7mean,
    mapping = aes(
      x = max(dat$DATE) - 1,
      y = NEW_IN_divid,
      label = round(NEW_IN_divid, 1),
      vjust = -0.5
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
      label = round(NEW_IN_divid, 1),
      vjust = -0.5
    ),
    color = "darkgrey",
    size = 4,
    fontface = "bold"
  )

# fig_trends

## adjust caption at the end of the trend figure
caption <- grobTree(
  textGrob(" * Lignes solides : moyennes 7 jours / Volle lijnen : ADD IN DUTCH \n * Lignes pointillées : phases de déconfinement 1a, 1b & 2 / Gestippelde lijnen: fases afbouw lockdown maatregelen 1a, 1b & 2",
    x = 0, hjust = 0, vjust = 0,
    gp = gpar(col = "darkgray", fontsize = 7, lineheight = 0.8)
  ),
  textGrob("Niko Speybroeck (@NikoSpeybroeck), Antoine Soetewey (@statsandr) & Angel Rosas (@arosas_aguirre) \n Data: https://epistat.wiv-isp.be/covid/  ",
    x = 1, hjust = 1, vjust = 0,
    gp = gpar(col = "black", fontsize = 7.5, lineheight = 1.2)
  ),
  cl = "ann"
)



# save plot
png(file = "Belgian_Hospitalisations_COVID-19_2weeks.png", width = 15 * 360, heigh = 7 * 360, units = "px", pointsize = 7, res = 300)
ggarrange(
  grid.arrange(fig_trends, bottom = caption),
  ncol = 1, widths = c(1, 1.5)
)
dev.off()
