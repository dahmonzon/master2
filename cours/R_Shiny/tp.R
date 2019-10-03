################ India trade data  between 2010-2018 ########


import <- read.csv("./tp_data/2018-2010_export.csv",header = TRUE,)
head(import[-2])

summary(import[import$country == "MALI"])


import[import$country == "MALI", -2]

import <- import[import$value,]


