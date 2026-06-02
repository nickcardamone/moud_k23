setwd("C://Users//Nick//Desktop")

traj <- read.csv("New Regression Data.csv")

traj$GROUP <- as.factor(traj$GROUP)




summary(aov(FIN~GROUP, data = traj))
summary(aov(REG~GROUP, data = traj))
summary(aov(STAFF~GROUP, data = traj))
summary(aov(PAT~GROUP, data = traj))
summary(aov(CENTER~GROUP, data = traj))
summary(aov(MISUSE~GROUP, data = traj))
summary(aov(BELIEF~GROUP, data = traj))

library(dplyr)
library(tidyverse)

traj <- traj %>% select(GROUP, REG, FIN, STAFF, PAT, CENTER, MISUSE, BELIEF)

traj <- traj %>% pivot_longer(c(REG, FIN, STAFF, PAT, CENTER, MISUSE, BELIEF), values_to = "rating", names_to = "code")

ggplot(data = traj, aes(fill = GROUP, y = rating, x = code)) + geom_boxplot()

traj <- traj %>% group_by(GROUP, code) %>% summarize(rating = mean(rating, na.rm = T))
