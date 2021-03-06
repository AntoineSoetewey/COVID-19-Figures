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
library(tidyr)
library(purrr)
library(mgcv)
library(lubridate)
library(data.table)

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
    TOTAL_IN_ICU_divid = TOTAL_IN_ICU / population * 11431406
  )

dat$PROVINCE <- relevel(as.factor(dat$PROVINCE), ref = "Belgium")

# choose period
subdat <- subset(dat, DATE >= "2020-10-01")

subdat <- subset(subdat, PROVINCE == "Belgium")

# projection based on last two weeks of data

source("broom_gam.R")

twoweek_models <- dat %>%
  filter( (DATE > lubridate::today() - lubridate::days(15)) & (DATE < lubridate::today() - lubridate::days(7))) %>%
  # filter(DATE > lubridate::today() - lubridate::days(15)) %>%
  group_by(PROVINCE) %>%
  nest() %>%
  mutate(model = map(data, ~ gam(TOTAL_IN_ICU ~ DATE,
    offset = log(population / 100000),
    data = .,
    family = "nb"
  )))

# check everywhere why df=12
# Can replace qt with 0.975 fro 95% ci

twoweek_fit <- twoweek_models %>%
  mutate(
    fit = map(model, augment.gam, se_fit = TRUE),
    fit = map(fit, select, -c("TOTAL_IN_ICU", "DATE"))
  ) %>%
  select(-model) %>%
  unnest(cols = c("data", "fit")) %>%
  mutate(
    fit = exp(.fitted),
    lcl = exp(.fitted - .se.fit * qt(0.999, df = 6)),
    ucl = exp(.fitted + .se.fit * qt(0.999, df = 6)),
    fit_actual = fit * population / 100000,
    lcl_actual = lcl * population / 100000,
    ucl_actual = ucl * population / 100000
  )

twoweek_models %>%
  mutate(estimates = map(model, tidy.gam)) %>%
  unnest(cols = estimates) %>% # produces 2 rows per country, (intercept) and day100
  filter(term == "DATE") %>%
  select(PROVINCE, estimate, std.error) %>%
  knitr::kable(digits = 3)

twoweek_doubling_times <- twoweek_models %>%
  mutate(estimates = map(model, tidy.gam)) %>%
  unnest(cols = estimates) %>% # produces 2 rows per country, (intercept) and day100
  filter(term == "DATE") %>%
  select(PROVINCE, estimate, std.error) %>%
  mutate(
    var_b = std.error^2,
    t = log(2) / estimate,
    var_t = var_b * log(2)^2 / estimate^4,
    lcl_t = t - sqrt(var_t) * qt(0.999, 6),
    ucl_t = t + sqrt(var_t) * qt(0.999, 6),
    label = sprintf("%.2f [%.2f-%.2f]", t, lcl_t, ucl_t)
  )

twoweek_doubling_times %>%
  select(PROVINCE, label) %>%
  knitr::kable()

facet_labels <- twoweek_doubling_times %>%
  mutate(label = paste0(PROVINCE, "\n", label, " days")) %>%
  pull(label)
names(facet_labels) <- pull(twoweek_doubling_times, PROVINCE)

# The - 1 may need to be changed to -2 if data not available...
# make a projection forward 2 weeks
nd <- data.frame(DATE = seq(today(tz = "CET") - 8, today(tz = "CET") + 3, "days"))
# make a population dataframe for joining
belgium_pop <- dat %>%
  group_by(PROVINCE) %>%
  summarize(population = first(population))

# check: why df = 12
twoweek_proj <- twoweek_models %>%
  mutate(proj = map(model, augment.gam, se_fit = TRUE, newdata = nd)) %>%
  select(-model, -data) %>%
  unnest(cols = c("proj")) %>%
  left_join(belgium_pop) %>%
  mutate(
    fit = exp(.fitted),
    lcl = exp(.fitted - .se.fit * qt(0.999, df = 6)),
    ucl = exp(.fitted + .se.fit * qt(0.999, df = 6)),
    fit_actual = fit * population / 100000,
    lcl_actual = lcl * population / 100000,
    ucl_actual = ucl * population / 100000
  )

# Create plot in French
fig_trends <- ggplot(
  subdat,
  aes(x = DATE, y = TOTAL_IN_ICU_divid)
) +
  # geom_vline(
  #   xintercept = as.Date("2020-09-28"), linetype = "dashed",
  #   color = "lightgrey", size = 0.7
  # ) +
  # geom_text(aes(x = as.Date("2020-09-28"), y = 1700, label = "Nombre de cas COVID19 qui fait réagir l'Allemagne"),
  #   colour = "lightgrey", angle = 90, vjust = -0.5
  # ) +
  # geom_vline(
  #   xintercept = as.Date("2020-10-04"), linetype = "dashed",
  #   color = "lightgrey", size = 0.7
  # ) +
  # geom_text(aes(x = as.Date("2020-10-04"), y = 1700, label = "Nombre de cas COVID19 qui fait réagir l'Irlande"),
  #   colour = "lightgrey", angle = 90, vjust = -0.5
  # ) +
  geom_vline(
    xintercept = as.Date("2020-10-19"), linetype = "dashed",
    color = "lightgrey", size = 0.7
  ) +
  geom_text(aes(x = as.Date("2020-10-19"), y = 3000, label = "Belgian reaction: 19 October"),
    colour = "lightgrey", angle = 90, vjust = -0.5
  ) +
  geom_vline(
    xintercept = as.Date("2020-11-02"), linetype = "dashed",
    color = "lightgrey", size = 0.7
  ) +
  geom_text(aes(x = as.Date("2020-11-02"), y = 3000, label = "Belgian reaction: 2 November"),
    colour = "lightgrey", angle = 90, vjust = -0.5
  ) +
  annotate("rect",
    ymin = -Inf, ymax = 2000,
    xmin = as.Date(-Inf), xmax = as.Date(Inf),
    alpha = .011
  ) +
  # annotate("rect",
  #   ymin = 0.5, ymax = 1,
  #   xmin = as.Date(-Inf), xmax = as.Date(Inf),
  #   alpha = .1
  # ) +
  annotate("rect",
    ymin = 2000, ymax = Inf,
    xmin = as.Date(-Inf), xmax = as.Date(Inf),
    alpha = .06,
  ) +
  geom_point(
    size = 3L,
    colour = "steelblue",
    shape = 1
  ) +
  labs(x = "", y = "Patients in intensive care") +
  theme_minimal() +
  # facet_wrap(vars(PROVINCE),
  #   scales = "free",
  #   ncol = 5
  # ) +
  geom_smooth(
    se = FALSE,
    col = "steelblue",
    method = "gam",
    formula = y ~ s(x),
    size = 1.5L
  ) +
  geom_line(
    data = filter(twoweek_proj, PROVINCE == "Belgium"),
    mapping = aes(y = fit_actual),
    color = "grey",
    size = 1L
  ) +
  geom_ribbon(
    data = filter(twoweek_proj, PROVINCE == "Belgium"),
    mapping = aes(
      y = fit_actual,
      ymin = lcl_actual,
      ymax = ucl_actual
    ),
    fill = "grey",
    alpha = 0.25
  ) +
  geom_line(
    data = filter(twoweek_fit, PROVINCE == "Belgium"),
    mapping = aes(y = fit_actual),
    color = "grey",
    size = 1L
  ) +
  geom_ribbon(
    data = filter(twoweek_fit, PROVINCE == "Belgium"),
    mapping = aes(
      y = fit_actual,
      ymin = lcl_actual,
      ymax = ucl_actual
    ),
    fill = "grey",
    alpha = 0.25
  ) +
  labs(
    title = "Evolution of COVID19 patients in intensive care in Belgium"
  ) +
  #  scale_y_continuous(breaks = seq(from = 0, to = max(subdat$TOTAL_IN_ICU_divid) + 810, by = 200), limits = c(0, max(subdat$TOTAL_IN_ICU_divid) + 810)) +
  scale_x_date(labels = date_format("%d/%m"), date_breaks = "1 week") +
  theme(
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(5.5, 5.5, 20, 5.5), "points")
  )


## adjust caption at the end of the trend figure
caption <- grobTree(
  textGrob("* Blue line: curve fitted to observations\n* Grey line: predictions",
    x = 0, hjust = 0, vjust = 0,
    gp = gpar(col = "darkgray", fontsize = 8, lineheight = 1.2)
  ),
  textGrob("@NikoSpeybroeck @statsandr @arosas_aguirre & D Tyre \n Data: https://epistat.wiv-isp.be/covid/  ",
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

# agregated data to join with the map

## identify peak of occupied UCI beds
dat$PROVINCE <- as.character(dat$PROVINCE)
dat <- dat[order(-dat$TOTAL_IN_ICU_divid), ]
dat_ag <- filter(dat, DATE <= as.Date("2020-06-01") & PROVINCE != "Belgium")

setDT(dat_ag)[, rep := seq_len(.N), by = PROVINCE]
dat_ag <- filter(dat_ag, rep == 1)

dat_ag2 <- filter(dat, DATE == max(dat$DATE) & PROVINCE != "Belgium")

dat_ag <- dat_ag %>% left_join(dat_ag2, by = "PROVINCE")

names(dat_ag)[c(1, 5, 10)] <- c("date1", "per1", "per2")


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
                 breaks = c(0, 500, 1000, 1250, 1500, 2000, 6000),
                 include.lowest = TRUE,
                 labels = c("[ 0, 500 ]","[ 500, 1000 ]","] 1000, 1250 ]", "] 1250, 1500 ]", "] 1500, 2000 ]", " > 2000")
    ),
    class2 = cut(per2,
                 breaks = c(0, 500, 1000, 1250, 1500, 2000, 6000),
                 include.lowest = TRUE,
                 labels = c("[ 0, 500 ]", "[ 500, 1000 ]", "] 1000, 1250 ]", "] 1250, 1500 ]", "] 1500, 2000 ]", " > 2000")
    )
  )

###### MAPS WITH GGPLOT
points <- st_centroid(map)
points <- cbind(map, st_coordinates(st_centroid(map$geometry)))

points <- mutate(points,
                 num_1 = paste("(", format(round(per1, 0), nsmall = 0), ")"),
                 date_1 = paste(format(date1, format = "%d/%m")),
                 num_2 = paste("(", format(round(per2, 0), nsmall = 0), ")")
)


period1 <- paste0("Date indicated on map", "   ")
period2 <- paste0(
  "Date: ",
  format(max(dat$DATE), format = "%d/%m"), "   "
)

library(RColorBrewer)

# https://www.color-hex.com/
blues <- c("white", brewer.pal(5, "Blues"))
# blues <- c("white", "#F9FAFF", brewer.pal(6, "Blues")[2:5])

map1 <- ggplot(map) +
  geom_sf(aes(fill = class1)) +
  scale_fill_manual(values = blues, drop = FALSE) +
  geom_text(
    data = points, aes(x = X, y = Y + 0.1, label = PROVINCE), col = "black", size = 2.4, nudge_x = -0.07,
    check_overlap = TRUE
  ) +
  geom_text(
    data = points, aes(x = X, y = Y + 0.04, label = date_1), col = "black", size = 2.6, nudge_x = -0.07,
    check_overlap = TRUE
  ) +
  geom_text(
    data = points, aes(x = X, y = Y - 0.04, label = num_1), col = "black", size = 3, nudge_x = -0.07,
    check_overlap = TRUE
  ) +
  labs(fill = bquote(atop(NA, atop("Number of patients (max. first peak)\nin ICU (x belgian pop.)", bold(.(period1)))))) +
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
    data = points, aes(x = X, y = Y + 0.06, label = PROVINCE), col = "black", size = 2.4, nudge_x = -0.07,
    check_overlap = TRUE
  ) +
  geom_text(
    data = points, aes(x = X, y = Y - 0.02, label = num_2), col = "black", size = 3, nudge_x = -0.07,
    check_overlap = TRUE
  ) +
  labs(fill = bquote(atop(NA, atop("Number of patients\nin ICU (x belgian pop.)", bold(.(period2)))))) +
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
png(file = "Belgian_ICU_BEL_0111_peak.png", width = 15 * 360, height = 7 * 360, units = "px", pointsize = 7, res = 300)
ggarrange(ggarrange(map1, map2, ncol = 1),
          grid.arrange(fig_trends, bottom = caption),
          ncol = 2, widths = c(1, 1.5)
)
dev.off()
