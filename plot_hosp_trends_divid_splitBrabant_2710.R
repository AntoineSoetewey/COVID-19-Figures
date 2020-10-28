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


# import Sciensano hospitalizations data
dat <- read.csv("https://epistat.sciensano.be/Data/COVID19BE_HOSP.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)

dat <- filter(dat, PROVINCE == "BrabantWallon"
              | PROVINCE == "Brussels"
              | PROVINCE == "VlaamsBrabant")

# aggregate new intakes by province and date
dat <- dat %>%
  mutate(
    DATE = as.Date(DATE),
    PROVINCE2 = case_when(
      PROVINCE == "BrabantWallon" ~ "Brabant wallon",
      PROVINCE == "VlaamsBrabant" ~ "Vlaams-Brabant",
      !PROVINCE %in% c("BrabantWallon", "VlaamsBrabant") ~ PROVINCE
    ),
    PROVINCE = PROVINCE2
  )

dat <- aggregate(NEW_IN ~ DATE + PROVINCE, dat, sum)

# add new intakes for Brabant as a whole

brabant <- aggregate(NEW_IN ~ DATE, dat, sum) %>%
  mutate(PROVINCE = "Brabant") %>%
  select(DATE, PROVINCE, NEW_IN)

##

dat <- rbind(dat, brabant) %>%
  mutate(
    population = case_when(
      PROVINCE == "Brabant" ~ 403599 + 1208542 + 1146175,
      PROVINCE == "Brabant wallon" ~ 403599,
      PROVINCE == "Brussels" ~ 1208542,
      PROVINCE == "Vlaams-Brabant" ~ 1146175
      ),
    NEW_IN_divid = NEW_IN / population * 100000
  )

dat$PROVINCE <- relevel(as.factor(dat$PROVINCE), ref = "Brabant")

# choose period
period <- "2020-09-20"
subdat <- subset(dat, DATE >= period)

period2 <- min(dat$DATE) + (max(dat$DATE) - as.Date(period))
subdat2 <- subset(dat, DATE <= period2)
time_diff <- as.Date(period) - min(subdat2$DATE)
subdat2$DATE <- subdat2$DATE + time_diff

break.vec <- c(seq(
  from = as.Date(period), to = max(dat$DATE),
  by = "2 weeks"
))

# Create plot in English
fig_trends <- ggplot(
  subdat,
  aes(x = DATE, y = NEW_IN_divid)
) +
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
  labs(x = "", y = "Number of hospitalizations (per 100,00 inhabitants)") +
  theme_minimal() +
  facet_wrap(vars(PROVINCE),
    scales = "free",
    ncol = 2
  ) +
  geom_smooth(
    se = FALSE,
    col = "steelblue",
    method = "gam",
    formula = y ~ s(x)
  ) +
  labs(
    title = "Evolution of hospital admissions in Brabant - COVID-19",
    subtitle = paste0(format(as.Date(period), "%B %d"), " to ", format(max(dat$DATE), "%B %d"), " (in blue) vs. ", format(min(dat$DATE), "%B %d"), " to ", format(period2, "%B %d"), " (in gray)")
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
  PROVINCE = levels(dat$PROVINCE),
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
    ncol = 2
  )

fig_trends

## adjust caption at the end of the trend figure
caption <- grobTree(
  textGrob(paste0(" * ", format(as.Date(period), "%d/%m"), " to ", format(max(dat$DATE), "%d/%m"), " in blue; ", format(min(dat$DATE), "%d/%m"), " to ", format(period2, "%d/%m"), " in gray\n * Solid lines: curves fitted to observations"),
    x = 0, hjust = 0, vjust = 0,
    gp = gpar(col = "darkgrey", fontsize = 8, lineheight = 1.2)
  ),
  textGrob("Niko Speybroeck (@NikoSpeybroeck), Antoine Soetewey (@statsandr) & Angel Rosas (@arosas_aguirre)  \nData: https://epistat.wiv-isp.be/covid/  ",
    x = 1, hjust = 1, vjust = 0,
    gp = gpar(col = "black", fontsize = 10, lineheight = 1.2)
  ),
  cl = "ann"
)

# save plot
png(file = "Belgian_Hospitalizations_splitBrabant_2710.png", width = 15 * 360, height = 7 * 360, units = "px", pointsize = 7, res = 300)
ggarrange(grid.arrange(fig_trends, bottom = caption),
  ncol = 1
)
dev.off()
