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
library(tidyr)
library(purrr)


# import Sciensano hospitalisations data
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
dat <- subset(dat, DATE >= "2020-09-01")



all_models <- dat %>% 
  group_by(PROVINCE) %>% 
  nest() %>% 
  mutate(model = map(data, ~glm(NEW_IN ~ DATE, 
                               # offset=population,
                                data = .,
                                family = "poisson")))


all_fit <- all_models %>% 
  mutate(fit = map(model, augment, se_fit = TRUE),
         fit = map(fit, select, -c("NEW_IN_divid","DATE"))) %>% 
  select(-model) %>% 
  unnest(cols = c("data","fit")) %>% 
  mutate(fit = 10^.fitted,
         lcl = 10^(.fitted - .se.fit * qt(0.975, df = 10)),
         ucl = 10^(.fitted + .se.fit * qt(0.975, df = 10)),
         fitpc = fit,
         lclpc = lcl,
         uclpc = ucl)

all_models %>% 
  mutate(estimates = map(model, tidy)) %>% 
  unnest(cols = estimates) %>%  # produces 2 rows per country, (intercept) and day100
  filter(term == "DATE") %>% 
  select(PROVINCE, estimate, std.error) %>% 
  knitr::kable(digits = 3)

doubling_times <- all_models %>% 
  mutate(estimates = map(model, tidy)) %>% 
  unnest(cols = estimates) %>%  # produces 2 rows per country, (intercept) and day100
  filter(term == "DATE") %>% 
  select(PROVINCE, estimate, std.error) %>% 
  mutate(var_b = std.error^2,
         t = log10(2) / estimate,
         var_t = var_b * log10(2)^2 / estimate^4,
         lcl_t = t - sqrt(var_t)*qt(0.975, 12),
         ucl_t = t + sqrt(var_t)*qt(0.975, 12),
         label = sprintf("%.2f [%.2f-%.2f]", t, lcl_t, ucl_t))

doubling_times %>% 
  select(PROVINCE, label) %>% 
  knitr::kable()

facet_labels <- doubling_times %>% 
  mutate(label = paste0(PROVINCE,"\n Doubling time: ", label, " days")) %>% 
  pull(label)
names(facet_labels) <- pull(doubling_times, PROVINCE)


# Create plot in english
fig_trends <- ggplot(
  dat,
  aes(x = DATE, y = NEW_IN_divid)
) +
  geom_vline(
    xintercept = as.Date("2020-09-01"), linetype = "dashed",
    color = "lightgrey", size = 0.5
  ) +
  geom_vline(
    xintercept = as.Date("2020-10-01"), linetype = "dashed",
    color = "lightgrey", size = 0.5
  ) +
  annotate("rect",
    ymin = -Inf, ymax = 1,
    xmin = as.Date(-Inf), xmax = as.Date(Inf),
    alpha = .05
  ) +
  annotate("rect",
    ymin = 1, ymax = 2,
    xmin = as.Date(-Inf), xmax = as.Date(Inf),
    alpha = .1
  ) +
  annotate("rect",
           ymin = 2, ymax = 3,
           xmin = as.Date(-Inf), xmax = as.Date(Inf),
           alpha = .15
  ) +
  annotate("rect",
    ymin = 3, ymax = Inf,
    xmin = as.Date(-Inf), xmax = as.Date(Inf),
    alpha = .20
  ) +
  geom_point(
    size = 1L,
    colour = "steelblue"
  ) +
  labs(x = "", y = "Number of hospitalisations (per 100,00 inhabitants)") +
  theme_minimal() +
  facet_wrap(~PROVINCE, dir="v", labeller = labeller(PROVINCE = facet_labels),
             scales = "free",
             ncol = 5) + 
  geom_line(data = all_fit,
            mapping = aes(y = fitpc),
            color = "grey",
            size = 1.25) +
  geom_ribbon(data = all_fit,
              mapping = aes(ymin = lclpc, ymax = uclpc),
              alpha = 0.2) +
  # geom_line(data = all_predicted,
  #           mapping = aes(y = fitpc),
  #           linetype = 2) +
  # geom_ribbon(data = all_predicted,
  #             mapping = aes(ymin = lplpc, ymax = uplpc),
  #             alpha = 0.2)  +
  labs(
    title = "Evolution of hospital admissions in Belgium - COVID-19"
  ) +
  scale_y_continuous(breaks = seq(from = 0, to = max(dat$NEW_IN_divid), by = 1), limits = c(0, max(dat$NEW_IN_divid))) +
  scale_x_date(labels = date_format("%d/%m")) +
  theme(
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(5.5, 5.5, 20, 5.5), "points")
  )

fig_trends

## adjust caption at the end of the trend figure
caption <- grobTree(
  textGrob("* Solid lines: curves fitted to observations",
    x = 0, hjust = 0, vjust = 0,
    gp = gpar(col = "darkgray", fontsize = 8, lineheight = 1.2)
  ),
  textGrob("Niko Speybroeck (@NikoSpeybroeck), Antoine Soetewey (@statsandr) & Angel Rosas (@arosas_aguirre) \n Data: https://epistat.wiv-isp.be/covid/  ",
    x = 1, hjust = 1, vjust = 0,
    gp = gpar(col = "black", fontsize = 10, lineheight = 1.2)
  ),
  cl = "ann"
)

# save plot
png(file = "Belgian_Hospitalisations_1310.png", width = 15 * 360, height = 7 * 360, units = "px", pointsize = 7, res = 300)
grid.arrange(fig_trends, bottom = caption)
dev.off()
