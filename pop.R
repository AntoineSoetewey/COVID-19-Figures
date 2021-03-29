dat <- read.csv("~/Downloads/TF_SOC_POP_STRUCT_2020.csv")
View(dat)

dat$AGEGR <- cut(dat$CD_AGE,
                       breaks = c(0, 10,20,30,40,50,60,70,80,90, 120), # cut points
                       right = FALSE # closed on the left, open on the right
)

library(dplyr)
## Recoding dat$AGEGR into dat$AGEGR_rec
dat$AGEGR_rec <- recode_factor(dat$AGEGR,
  "[0,10)" = "0-9",
  "[10,20)" = "10-19",
  "[20,30)" = "20-29",
  "[30,40)" = "30-39",
  "[40,50)" = "40-49",
  "[50,60)" = "50-59",
  "[60,70)" = "60-69",
  "[70,80)" = "70-79",
  "[80,90)" = "80-89",
  "[90,120)" = "90+"
)

dat <- aggregate(MS_POPULATION ~ AGEGR_rec + CD_SEX, dat, sum)

write.csv(dat, "pop.csv", row.names = FALSE)
