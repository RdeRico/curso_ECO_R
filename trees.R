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

library('easystats')
model_dashboard(m1)


trees$height.pred <- fitted(m1)
trees$resid <- residuals(m1)
head(trees)



#Sex study
plot(height ~ as.factor(sex), data = trees)

m2 <- lm(height ~ sex, data = trees)
summary(m2)
check_model(m2)

#Area study
trees$site <- as.factor(trees$site)
plot(height ~ site, data = trees)
m3 <- lm(height ~ site, data = trees)
summary(m3)

check_model(m3)
plot(simulate_parameters(m3), stack = FALSE)
estimate_contrasts(m3)
