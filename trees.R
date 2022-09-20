library('here')
library('ggplot2')

trees <- read.csv(here('datasets/trees.csv'))
head(trees)

#plotting
hist(trees$height)
hist(trees$dbh)

plot(height ~ dbh, data = trees, las = 1)
ggplot(trees) + geom_point(aes(dbh, height))

#models
library('equatiomatic')
m1 <- lm(height ~ dbh, data = trees)
equatiomatic::extract_eq(m1, use_coefs = TRUE)

summary(m1)

library('performance')
check_model(m1)