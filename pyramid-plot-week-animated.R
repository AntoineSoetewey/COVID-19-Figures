# clean environment
remove(list = ls())
### COVID19BE // AGE SEX TRENDS GIF

## required packages
library(gganimate)
library(ggplot2)
library(zoo)

## import data
dta <-
  read.csv("https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv")
str(dta)

## collapse by date, age and sex
dta2 <- aggregate(CASES ~ DATE + AGEGROUP + SEX, dta, sum)
dta2$DATE <- as.Date(dta2$DATE)

## make full version - add zeroes
d <-
  expand.grid(
    stringsAsFactors = FALSE,
    DATE = unique(dta2$DATE),
    AGEGROUP = unique(dta2$AGEGROUP),
    SEX = unique(dta2$SEX))

dta3 <- merge(d, dta2, all = TRUE)
dta3[is.na(dta3)] <- 0

## calculate rolling incidences
a <- with(dta3, tapply(CASES, list(AGEGROUP, SEX), rollsum, 7))
n <- length(a[[1]])

## compile dataframe
df <-
  data.frame(
    CASES = unlist(a),
    AGEGROUP = rep(rep(unique(dta3$AGEGROUP), each = n), 2),
    SEX = rep(unique(dta3$SEX), each = n*10),
    DATE_FROM = rep(head(unique(dta3$DATE), -6), 2*10),
    DATE_TO = rep(tail(unique(dta3$DATE), -6), 2*10))
df$DATE_RANGE <-
  paste(format(df$DATE_FROM, "%d/%m/%Y"),
        format(df$DATE_TO, "%d/%m/%Y"),
        sep = "-")
df$DATE_RANGE <-
  factor(df$DATE_RANGE, unique(df$DATE_RANGE))

## add incidence
POP <- 
  structure(list(SEX = c("F", "M", "F", "M", "F", "M", "F", 
                         "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M"
  ), AGEGROUP = c("0-9", "0-9", "10-19", "10-19", "20-29", "20-29", 
                  "30-39", "30-39", "40-49", "40-49", "50-59", "50-59", "60-69", 
                  "60-69", "70-79", "70-79", "80-89", "80-89", "90+", "90+"), POP = c(624521, 
                                                                                      653781, 626569, 657008, 699447, 710611, 741647, 741911, 746138, 
                                                                                      760756, 791752, 801745, 677294, 648434, 487617, 415187, 326756, 
                                                                                      207895, 81437, 30900)), row.names = c(NA, -20L), class = "data.frame")
df <- merge(df, POP)
df$INC <- 1e5 * df$CASES / df$POP

## final steps
df$SEX <-
  factor(df$SEX, levels = c("M", "F"), labels = c("Men", "Women"))
head(df)


##
## ABSOLUTE CASES
##

lower_limit <- -1000
upper_limit <- 2000

gif <-
  ggplot(df) +
  geom_bar(aes(as.factor(AGEGROUP), CASES, group = SEX, fill = SEX),
           stat = "identity",
           subset(df, SEX == "Women")
  ) +
  geom_bar(aes(as.factor(AGEGROUP), -CASES, group = SEX, fill = SEX),
           stat = "identity",
           subset(df, SEX == "Men")
  ) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "COVID-19 cases by age group and sex in Belgium",
    subtitle = "Rolling 7 days window: {closest_state}",
    caption = "Niko Speybroeck (@NikoSpeybroeck), Antoine Soetewey (@statsandr) \n Data: https://epistat.wiv-isp.be/covid/",
    x = "Age group",
    y = "Number of cases per week") +
  scale_y_continuous(
    # limits = c(lower_limit, upper_limit),
    breaks = seq(lower_limit, upper_limit, 1000),
    labels = abs(seq(lower_limit, upper_limit, 1000))
  ) +
  theme(
    legend.position = c(.95, .15),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.title = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "bold")
  ) +
  transition_states(DATE_RANGE, transition_length = 2, state_length = 0) +
  # view_follow(fixed_x = TRUE) +
  ease_aes("linear")
# gif
animate(
  gif,
  fps = 10, duration = floor(((nrow(df) / 2 / 10) + 20) / 10), end_pause = 20, width = 800, height = 450)
anim_save(filename = "pyramid-plot-week-animated.gif")


##
## INCIDENCE PER 100,000
##

lower_limit <- -1000
upper_limit <- 1000

gif <-
  ggplot(df) +
  geom_bar(aes(as.factor(AGEGROUP), INC, group = SEX, fill = SEX),
           stat = "identity",
           subset(df, SEX == "Women")
  ) +
  geom_bar(aes(as.factor(AGEGROUP), -INC, group = SEX, fill = SEX),
           stat = "identity",
           subset(df, SEX == "Men")
  ) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Age and sex specific incidence per 100 000 of COVID19 cases in Belgium",
    subtitle = "Rolling 7 days window: {closest_state}",
    caption = "Niko Speybroeck (@NikoSpeybroeck), Antoine Soetewey (@statsandr) \n Data: https://epistat.wiv-isp.be/covid/",
    x = "Age group",
    y = "Number of cases per week per 100 000") +
  scale_y_continuous(
    # limits = c(lower_limit, upper_limit),
    breaks = seq(lower_limit, upper_limit, 1000),
    labels = abs(seq(lower_limit, upper_limit, 1000))
  ) +
  theme(
    legend.position = c(.95, .15),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.title = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "bold")
  ) +
  transition_states(DATE_RANGE, transition_length = 2, state_length = 0) +
  # view_follow(fixed_x = TRUE) +
  ease_aes("linear")
# gif
animate(
  gif,
  fps = 10, duration = floor(((nrow(df) / 2 / 10) + 20) / 10), end_pause = 20, width = 800, height = 450)
anim_save(filename = "pyramid-plot-week-animated-incidence.gif")
