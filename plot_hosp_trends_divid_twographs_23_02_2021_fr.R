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
library(ggthemes)

# import Sciensano hospitalizations data
dat <- read.csv("https://epistat.sciensano.be/Data/COVID19BE_HOSP.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)

# aggregate new intakes by province and date -

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

dat <- aggregate(NEW_IN ~ DATE + PROVINCE, dat, sum)


# add new intakes for Belgium as a whole

belgium <- aggregate(NEW_IN ~ DATE, dat, sum) %>%
  mutate(PROVINCE = "Belgium") %>%
  select(DATE, PROVINCE, NEW_IN)

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
    NEW_IN_divid = NEW_IN / population * 100000
  )

dat$PROVINCE <- relevel(as.factor(dat$PROVINCE), ref = "Belgium")

# choose period
period <- "2020-09-29"
subdat <- subset(dat, DATE >= period)
# subdat <- subset(dat, PROVINCE == "Belgium")

period2 <- min(dat$DATE) + (max(dat$DATE) - as.Date(period))
subdat2 <- subset(dat, DATE <= period2)
time_diff <- as.Date(period) - min(subdat2$DATE)
subdat2$DATE <- subdat2$DATE + time_diff

break.vec <- c(seq(
  from = as.Date(period), to = max(dat$DATE),
  by = "4 weeks"
))

# Create plot
fig_trends <- ggplot(
  subdat,
  aes(x = DATE, y = NEW_IN_divid)
) +
  # geom_vline(
  #   xintercept = as.Date("2020-07-01"), linetype = "dashed",
  #   color = "lightgrey", size = 0.5
  # ) +
  # geom_vline(
  #   xintercept = as.Date("2020-08-01"), linetype = "dashed",
  #   color = "lightgrey", size = 0.5
  # ) +
  # geom_vline(
  #   xintercept = as.Date("2020-09-01"), linetype = "dashed",
  #   color = "lightgrey", size = 0.5
  # ) +
  geom_point(
    data = subdat2,
    aes(x = DATE, y = NEW_IN_divid),
    col = "darkgrey",
    alpha = 0.35
  ) +
  geom_line(
    data = subdat2,
    aes(x = DATE, y = NEW_IN_divid),
    stat = "smooth",
    method = "gam",
    formula = y ~ s(x),
    col = "darkgrey",
    alpha = 0.5,
    size = 1L,
    linetype = 1
  ) +
  geom_point(
    size = 1L,
    colour = "steelblue"
  ) +
  labs(x = "", y = "Number of hospitalisations (per 100,000 inhabitants)") +
  theme_minimal() +
  # theme_economist() + scale_color_economist() +
  facet_wrap(vars(PROVINCE),
    scales = "free",
    ncol = 5
  ) +
  geom_smooth(
    se = FALSE,
    col = "steelblue",
    method = "gam",
    formula = y ~ s(x)
  ) +
  labs(
    # title = "Evolution des admissions hospitalières en Belgique - COVID-19"
    # ,
    # subtitle = paste0(format(as.Date(period), "%B %d"), " to ", format(max(dat$DATE), "%B %d"), " (en bleu) vs. ", format(min(dat$DATE), "%B %d"), " to ", format(period2, "%B %d"), " (en gris)")
  ) +
  scale_y_continuous(
    breaks = seq(from = 0, to = max(subdat$NEW_IN_divid), by = 2),
    limits = c(0, max(subdat$NEW_IN_divid))
  ) +
  scale_x_date(
    labels = date_format("%d/%m"),
    # date_breaks = "2 weeks",
    breaks = break.vec,
    sec.axis = sec_axis(~ . - time_diff,
      labels = date_format("%d/%m"),
      breaks = break.vec - time_diff
    )
  ) +
  theme(
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(colour = "steelblue"),
    axis.text.x.top = element_text(color = "darkgray"),
    strip.text = element_text(size = 12),
    strip.placement = "outside",
    plot.margin = unit(c(5.5, 5.5, 20, 5.5), "points")
  )

dat_peak <- data.frame(
  PROVINCE = as.factor(levels(dat$PROVINCE)),
  h_int = aggregate(y ~ PANEL, data = ggplot_build(fig_trends)[["data"]][[2]], max)$y,
  PANEL = 1:nlevels(dat$PROVINCE)
)

fig_trends <- fig_trends +
  geom_hline(
    data = dat_peak,
    aes(yintercept = h_int),
    linetype = "dashed",
    size = 0.75,
    color = "darkgrey",
    alpha = 0.5
  ) +
  facet_wrap(~PROVINCE,
    scales = "free",
    ncol = 5
  )

fig_trends

#  ggsave("fig_trends.png")

## adjust caption at the end of the trend figure
caption <- grobTree(
  textGrob(paste0("* ", format(as.Date(period), "%d/%m"), " to ", format(max(dat$DATE), "%d/%m"), " in blue; ", format(min(dat$DATE), "%d/%m"), " to ", format(period2, "%d/%m"), " in grey\n* Solid lines: fitted lines"),
    x = 0, hjust = 0, vjust = 0,
    gp = gpar(col = "darkgrey", fontsize = 8, lineheight = 1.2)
  ),
  textGrob("Niko Speybroeck (@NikoSpeybroeck), Antoine Soetewey (@statsandr) & Angel Rosas (@arosas_aguirre)  \nData: https://epistat.wiv-isp.be/covid/  ",
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
    "per1" = sum(NEW_IN_divid[DATE >= as.Date("2020-03-25") & DATE <= as.Date("2020-04-07")], na.rm = T) / 14,
    "per2" = sum(NEW_IN_divid[DATE >= mini & DATE <= maxi], na.rm = T) / divi
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
      breaks = c(0, 0.5, 1.0, 1.5, 3, 5, 9),
      include.lowest = TRUE,
      labels = c("[ 0.0, 0.5 ]", "] 0.5, 1.0 ]", "] 1.0, 1.5 ]", "] 1.5, 3.0 ]", "] 3.0, 5.0]", " > 5.0")
    ),
    class2 = cut(per2,
      breaks = c(0, 0.5, 1.0, 1.5, 3, 5, 9),
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


period1 <- paste0("Period: 25/03 - 07/04", "   ")
period2 <- paste0(
  "Period: ", format(mini, format = "%d/%m"), " - ",
  format(maxi, format = "%d/%m"), "   "
)


library(RColorBrewer)
reds <- brewer.pal(7, "Reds")
blues <- brewer.pal(7, "Blues")


map1 <- ggplot(map) +
  geom_sf(aes(fill = class1)) +
  scale_fill_manual(values = blues, drop = FALSE) +
  geom_text(
    data = points, aes(x = X, y = Y + 0.06, label = PROVINCE), col = "black", size = 2.4, nudge_x = -0.07,
    check_overlap = TRUE
  ) +
  geom_text(
    data = points, aes(x = X, y = Y, label = num_1), col = "black", size = 3, nudge_x = -0.07,
    check_overlap = TRUE
  ) +
  labs(fill = bquote(atop(NA, atop("Daily hospitalizations (x100,000 inh.)", bold(.(period1)))))) +
  theme_void() +
  theme(
    # Change legend
    legend.position = c(0.2, 0.2),
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
    data = points, aes(x = X, y = Y, label = num_2), col = "black", size = 3, nudge_x = -0.07,
    check_overlap = TRUE
  ) +
  labs(fill = bquote(atop(NA, atop("Daily hospitalizations (x100,000 inh.)", bold(.(period2)))))) +
  theme_void() +
  theme(
    # Change legend
    legend.position = c(0.2, 0.22),
    legend.key.size = unit(0.9, "line"),
    legend.title = element_text(size = 12, color = "black"),
    legend.text = element_text(color = "black"),
    plot.margin = unit(c(+0.2, 0, +0.5, 3), "cm")
  )

# save plot
# png(file = "Belgian_Hospitalizations_2710.png", width = 15 * 360, height = 7 * 360, units = "px", pointsize = 7, res = 300)
# ggarrange(ggarrange(map1, map2, ncol = 1),
#   grid.arrange(fig_trends, bottom = caption),
#   ncol = 2, widths = c(1, 1.5)
# )
# dev.off()

# save plot
png(file = "Belgian_Hospitalizations_2602.png", width = 15 * 360, height = 7 * 360, units = "px", pointsize = 7, res = 300)
ggarrange(grid.arrange(fig_trends, bottom = caption),
 widths = c(1, 1.5)
)
dev.off()


subdat <- subset(subdat, PROVINCE == "Belgium")

# Create Belgian plot only
fig_trends2_1 <- ggplot(
  subdat,
  aes(x = DATE, y = NEW_IN_divid)
) +
  # geom_vline(
  #   xintercept = as.Date("2020-07-01"), linetype = "dashed",
  #   color = "lightgrey", size = 0.5
  # ) +
  # geom_vline(
  #   xintercept = as.Date("2020-08-01"), linetype = "dashed",
  #   color = "lightgrey", size = 0.5
  # ) +
  # geom_vline(
  #   xintercept = as.Date("2020-09-01"), linetype = "dashed",
  #   color = "lightgrey", size = 0.5
# ) +


geom_point(
  data = subdat2,
  aes(x = DATE, y = NEW_IN_divid),
  col = "darkgrey",
  alpha = 0.35
) +
  geom_line(
    data = subdat2,
    aes(x = DATE, y = NEW_IN_divid),
    stat = "smooth",
    method = "gam",
    formula = y ~ s(x),
    col = "darkgrey",
    alpha = 0.5,
    size = 1L,
    linetype = 1
  ) +
  geom_point(
    size = 1L,
    colour = "steelblue"
  ) +
  labs(x = "", y = "Nombre d'hospitalisations (par 100,000 habitants)") +
  theme_minimal() +
   geom_smooth(
    se = FALSE,
    col = "steelblue",
    method = "gam",
    formula = y ~ s(x)
  ) +
  labs(
    title = "Evolution des admissions hospitalières en Belgique - COVID-19"
    # ,
    # subtitle = paste0(format(as.Date(period), "%B %d"), " to ", format(max(dat$DATE), "%B %d"), " (en bleu) vs. ", format(min(dat$DATE), "%B %d"), " to ", format(period2, "%B %d"), " (en gris)")
  ) +
  scale_y_continuous(
    breaks = seq(from = 0, to = max(subdat$NEW_IN_divid), by = 2),
    limits = c(0, max(subdat$NEW_IN_divid))
  ) +
  scale_x_date(
    labels = date_format("%d/%m"),
    # date_breaks = "2 weeks",
    breaks = break.vec,
    sec.axis = sec_axis(~ . - time_diff,
                        labels = date_format("%d/%m"),
                        breaks = break.vec - time_diff
    )
  ) +
  
  geom_vline(
    xintercept = as.Date("2020-10-06"), linetype = "dashed",
    color = "steelblue", size = 0.8
  ) +
  geom_text(aes(x = as.Date("2020-10-06"), y = 6, label = "6/10: comité de concertation: mesures plus strictes"),
            colour = "steelblue", angle = 90, vjust = -0.5
  ) +
  
  geom_vline(
    xintercept = as.Date("2020-12-23"), linetype = "dashed",
    color = "darkgray", size = 0.8
  ) +
  geom_text(aes(x = as.Date("2020-12-23"), y = 6.7, label = "8/06: Ecoles primaires reprennent"),
            colour = "darkgray", angle = 90, vjust = -0.5
  ) +

  geom_hline(
    yintercept = 75*100000/11431406, linetype = "dashed",
    color = "red", size = 0.8
  ) +
  
  # geom_text(aes(x = as.Date("2020-12-23"), y = 75*11431406/100000, label = ""),
  #           colour = "darkgray", angle = 90, hjust = -0.5
  # ) +
  
  
  theme(
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(colour = "steelblue"),
    axis.text.x.top = element_text(color = "darkgray"),
    strip.text = element_text(size = 12),
    strip.placement = "outside",
    plot.margin = unit(c(5.5, 5.5, 20, 5.5), "points")
  )


fig_trends2_1

subsubdat2 <- subset(subdat2, PROVINCE == "Belgium")


# Create Belgian plot only
fig_trends2_2 <- ggplot(
  subdat,
  aes(x = DATE, y = NEW_IN)
) +
  # geom_vline(
  #   xintercept = as.Date("2020-07-01"), linetype = "dashed",
  #   color = "lightgrey", size = 0.5
  # ) +
  # geom_vline(
  #   xintercept = as.Date("2020-08-01"), linetype = "dashed",
  #   color = "lightgrey", size = 0.5
  # ) +
  # geom_vline(
  #   xintercept = as.Date("2020-09-01"), linetype = "dashed",
  #   color = "lightgrey", size = 0.5
# ) +


geom_point(
  data = subsubdat2,
  aes(x = DATE, y = NEW_IN),
  col = "darkgrey",
  alpha = 0.35
) +
  geom_line(
    data = subsubdat2,
    aes(x = DATE, y = NEW_IN),
    stat = "smooth",
    method = "gam",
    formula = y ~ s(x),
    col = "darkgrey",
    alpha = 0.5,
    size = 1L,
    linetype = 1
  ) +
  geom_point(
    size = 1L,
    colour = "steelblue"
  ) +
  labs(x = "", y = "Hospitalisations",
       caption = "Niko Speybroeck (@NikoSpeybroeck), Antoine Soetewey (@statsandr) & Angel Rosas (@arosas_aguirre)\n Data: https://epistat.wiv-isp.be/covid/") +
  theme_minimal() +
  # theme_economist() + scale_color_economist() +
  geom_smooth(
    se = FALSE,
    col = "steelblue",
    method = "gam",
    formula = y ~ s(x)
  ) +
  labs(
    # title = "Evolution des admissions hospitalières en Belgique - COVID-19"
    # ,
    # subtitle = paste0(format(as.Date(period), "%B %d"), " to ", format(max(dat$DATE), "%B %d"), " (en bleu) vs. ", format(min(dat$DATE), "%B %d"), " to ", format(period2, "%B %d"), " (en gris)")
  ) +
  scale_y_continuous(
    breaks = seq(from = 0, to = max(subdat$NEW_IN), by = 200),
    limits = c(0, max(subdat$NEW_IN))
  ) +
  scale_x_date(
    labels = date_format("%d/%m"),
    # date_breaks = "2 weeks",
    breaks = break.vec,
    sec.axis = sec_axis(~ . - time_diff,
                        labels = date_format("%d/%m"),
                        breaks = break.vec - time_diff
    )
  ) +
  
  # geom_vline(
  #   xintercept = as.Date("2020-10-06"), linetype = "dashed",
  #   color = "steelblue", size = 0.8
  # ) +
  # geom_text(aes(x = as.Date("2020-10-06"), y = 6, label = "6/10: comité de concertation: mesures plus strictes"),
  #           colour = "steelblue", angle = 90, vjust = -0.5
  # ) +
  # 
  # geom_vline(
  #   xintercept = as.Date("2020-12-23"), linetype = "dashed",
  #   color = "darkgray", size = 0.8
  # ) +
  # geom_text(aes(x = as.Date("2020-12-23"), y = 6.7, label = "8/06: Ecoles primaires reprennent"),
  #           colour = "darkgray", angle = 90, vjust = -0.5
  # ) +
  # 
  # geom_hline(
  #   yintercept = 75*100000/11431406, linetype = "dashed",
  #   color = "red", size = 0.8
  # ) +
  
  # geom_text(aes(x = as.Date("2020-12-23"), y = 75*11431406/100000, label = ""),
  #           colour = "darkgray", angle = 90, hjust = -0.5
  # ) +
  
  
  theme(
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(colour = "steelblue"),
    axis.text.x.top = element_text(color = "darkgray"),
    strip.text = element_text(size = 12),
    strip.placement = "outside",
    plot.margin = unit(c(5.5, 5.5, 20, 5.5), "points")
  ) +
  geom_hline(
    yintercept = 75, linetype = "dashed",
    color = "red", size = 0.8
  ) +
  
  geom_text(aes(x = as.Date("2020-10-24"), y = 75+25, label = "Level of 75 hospitalisations/day"),
            colour = "red", angle = 0, size = 4
  ) +
  geom_hline(
    yintercept = 0, linetype = "dashed", size = 0.1
  ) 


fig_trends2_2
ggsave("fig_trends2_2.png",
       width = 31.75,
       height = 17.859375,
       units = "cm",
       dpi = 300)

subdat <- subdat %>%
  mutate(
    DATE = as.Date(DATE),
     )

# Create Belgian plot only
fig_trends3 <- ggplot(
  subdat,
  # aes(x = DATE, y = NEW_IN_divid)
  aes(x = DATE, y = NEW_IN_divid)
) +
   geom_point(
    size = 3L,
    colour = "steelblue",
    alpha=0.35
  ) +
  labs(x = "", y = "Nombre d'hospitalisations (par 100,000 habitants)"
       # ,
       # title = "Evolution du nombre d'hospitalisations en Belgique - COVID-19"
       ) +
  # theme_minimal() +
  theme_economist() + scale_color_economist() +
  geom_smooth(
    se = FALSE,
    col = "steelblue",
    method = "gam",
    formula = y ~ s(x)
  ) +
  # labs(
  #   title = "Evolution des admissions hospitalières en Belgique - COVID-19"
  #   # ,
  #   # subtitle = paste0(format(as.Date(period), "%B %d"), " to ", format(max(dat$DATE), "%B %d"), " (en bleu) vs. ", format(min(dat$DATE), "%B %d"), " to ", format(period2, "%B %d"), " (en gris)")
  # ) +
  scale_y_continuous(
    breaks = seq(from = 0, to = max(subdat$NEW_IN_divid), by = 2),
    limits = c(0, max(subdat$NEW_IN_divid))
  ) +
  scale_x_date(
    labels = date_format("%d/%m"),
    # date_breaks = "2 weeks",
    breaks = break.vec,
    # sec.axis = sec_axis(~ . - time_diff,
    #                     labels = date_format("%d/%m"),
    #                     breaks = break.vec - time_diff
    # )
  ) +
  
    geom_hline(
    yintercept = 75*100000/11431406, linetype = "dashed",
    color = "red", size = 0.8
  ) +
  
  geom_text(aes(x = as.Date("2020-11-5"), y = 75*100000/11431406+0.2, label = "Niveau de 75 hospitalisations/jour"),
            colour = "red", angle = 0, size = 5
  ) +
  geom_vline(
    xintercept = as.Date("2020-10-6"), linetype = "dashed",
    color = "darkgray", size = 0.8
  ) +
  geom_text(aes(x = as.Date("2020-10-6"), y = 5.2, label = "6/10 : comité de concertation : mesures plus strictes"),
            colour = "darkgray", angle = 90, vjust = -0.5, size = 5
  ) +
  
  theme(
    axis.title = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 16),
    axis.text.x = element_text(colour = "steelblue"),
    axis.text.x.top = element_text(color = "darkgray"),
    strip.text = element_text(size = 14),
    strip.placement = "outside",
    axis.title.y = 
          element_text(margin = 
                         margin(t = 0, r = 9, b = 0, l = 0))
  )


# fig_trends3
# ggsave("fig_trends3.png")


# Create Belgian plot only
fig_trends3_1 <- ggplot(
  subdat,
  # aes(x = DATE, y = NEW_IN_divid)
  aes(x = DATE, y = NEW_IN)
) +
  geom_point(
    size = 3L,
    colour = "steelblue",
    alpha=0.35
  ) +
  labs(x = "", y = "Hospitalisations per day in Belgium",
       caption = "Niko Speybroeck (@NikoSpeybroeck), Antoine Soetewey (@statsandr) & Angel Rosas (@arosas_aguirre)\n Data: https://epistat.wiv-isp.be/covid/"
       # ,
       # title = "Evolution du nombre d'hospitalisations en Belgique - COVID-19"
  ) +
  # theme_minimal() +
  theme_economist() + scale_color_economist() +
  geom_smooth(
    se = FALSE,
    col = "steelblue",
    method = "gam",
    formula = y ~ s(x)
    # ,
    # linetype = "dashed"
  ) +
  # labs(
  #   title = "Evolution des admissions hospitalières en Belgique - COVID-19"
  #   # ,
  #   # subtitle = paste0(format(as.Date(period), "%B %d"), " to ", format(max(dat$DATE), "%B %d"), " (en bleu) vs. ", format(min(dat$DATE), "%B %d"), " to ", format(period2, "%B %d"), " (en gris)")
  # ) +
  scale_y_continuous(
    breaks = seq(from = 0, to = max(subdat$NEW_IN), by = 200),
    limits = c(0, max(subdat$NEW_IN))
  ) +
  scale_x_date(
    labels = date_format("%d/%m"),
    # date_breaks = "2 weeks",
    breaks = break.vec,
    # sec.axis = sec_axis(~ . - time_diff,
    #                     labels = date_format("%d/%m"),
    #                     breaks = break.vec - time_diff
    # )
  ) +
  
  geom_hline(
    yintercept = 75, linetype = "dashed",
    color = "red", size = 0.8
  ) +
  
  geom_text(aes(x = as.Date("2020-11-3"), y = 75+25, label = "Level of 75 hospitalisations/day"),
            colour = "red", angle = 0, size = 5
  ) +
  # geom_vline(
  #   xintercept = as.Date("2020-10-6"), linetype = "dashed",
  #   color = "darkgray", size = 0.8
  # ) +
  # geom_text(aes(x = as.Date("2020-10-6"), y = 560, label = "6/10 - comité de concertation: mesures plus strictes"),
  #           colour = "darkgray", angle = 90, vjust = -0.5, size = 5
  # ) +
  
  theme(
    axis.title = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 16),
    axis.text.x = element_text(colour = "steelblue"),
    axis.text.x.top = element_text(color = "darkgray"),
    strip.text = element_text(size = 14),
    strip.placement = "outside",
    axis.title.y = 
      element_text(margin = 
                     margin(t = 0, r = 9, b = 0, l = 0))
  )

fig_trends3_1
ggsave("fig_trends3_1.png",
       width = 31.75,
       height = 17.859375,
       units = "cm",
       dpi = 300)

# # fig with gridlines
# # (opts() is deprecated, use theme())
# fig_trends4 <- fig_trends3 +
#   theme(
#     panel.grid.major = element_line(linetype = c("28")),
#     panel.grid.minor = element_line(linetype = c("28"))
#   )
# fig_trends4
# ggsave("fig_trends4.png")
# 
# fig_trends5 <- fig_trends3 +
#   theme(axis.line.y = element_line())
# fig_trends5
# ggsave("fig_trends5.png")
