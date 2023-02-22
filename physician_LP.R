library(readxl)
library(tidyverse)
library(DataExplorer)
library(GGally)
library(reshape2)
library(car)
library(olsrr)

# load file
cdi <- read.csv("cdi.csv")
cdi$region <- as.factor(cdi$region)
cdi <- cdi %>%
  select(-id, -county, -state) %>%
  select(physician, everything())
summary(cdi)

# EDA
plot_intro(cdi)
plot_histogram(cdi)
plot_density(cdi)
plot_correlation(cdi)
plot_scatterplot(split_columns(cdi)$continuous, 
                 by = "physician")
ggpairs(cdi[, c(1:14)])

# model fit
model <- lm(physician ~ . - area, data = cdi)
summary(model)
ols_plot_resid_lev(model)

# The some VIF are high.
df <- data.frame(y = log(cdi[,1]),
                 fitted = fitted(model),
                 resid = model$residuals)
# Normality assumprion (failed)
ggplot(df, aes(sample = scale(resid))) +
  stat_qq() +
  geom_abline(intercept = 0, slope = 1,
              col = "gray") +
  theme_bw() +
  xlab("Theoretical") +
  ylab("Standardised Residuals") +
  ggtitle("QQ-plot") +
  theme_bw()
# Linearity assumption (failed)
ggplot(df, aes(x =y, y = fitted)) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = F, col = "red") +
  geom_abline(intercept = 0, slope = 1) +
  xlab("Observed values") +
  ylab("Fitted values") +
  ggtitle("Observed vs Fitted Plot") +
  theme_bw()
# Homoscedasticity assumption (failed)
ggplot(df, aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = F, col = "red") +
  geom_abline(intercept = 0, slope = 1) +
  xlab("Fitted values") +
  ylab("Residuals") +
  ggtitle("Residuals Plot") +
  theme_bw()
# Multicollinearity assumption (failed)
vif(model)
# Independence assumption (failed)
ggplot(df, aes(x = 1:length(df$y), y = resid)) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = F, col = "red") +
  xlab("Index") +
  ylab("Residuals") +
  ggtitle("Resisuals vs Time") +
  theme_bw()

m <- dim(cdi)[1]
x <- head(df$resid, m - 1)
y <- tail(df$resid, m - 1)
cor(x, y)
lag_df <- data.frame(x, y)
ggplot(lag_df, aes(x = x, y = y)) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  xlab(expression(hat(epsilon)[i])) +
  ylab(expression(hat(epsilon)[i+1])) +
  ggtitle("Successive Residuals Plot") +
  theme_bw()
durbinWatsonTest(model)

# In irder to address the assumption violations:
# get rid of outliers since LR is sensitive to their presence;
# make log transformation to address linearity and homoscedasticity problems;
# address collinearity problem.

#get rid of outliers
cdi <- cdi %>%
  filter(log(physician) < quantile(log(cdi$physician), 0.99)) %>%
  filter(log(bed) < quantile(log(cdi$bed), 0.99)) %>%
  filter(age65 < quantile(cdi$age65, 0.99)) %>%
  filter(age18 < quantile(cdi$age18, 0.99)) %>%
  filter(bachelor < quantile(cdi$bachelor, 0.99)) %>%
  filter(poverty < quantile(cdi$poverty, 0.99)) %>%
  filter(percapita < quantile(cdi$percapita, 0.99)) %>%
  filter(unemployed < quantile(cdi$unemployed, 0.99)) %>%
  filter(population < mean(cdi$population) + 3*sd(cdi$population))
# new model fit
model2 <- lm(log(physician) ~ log(population) + age18 + age65 +
              log(bed) + bachelor + poverty +
              unemployed + region + percapita, data = cdi[-381,])
summary(model2)
ols_plot_resid_lev(model2)
#Multicollinearity assumption (resolved)
vif(model2)
# Normality assumption (resolved)
df2 <- data.frame(y = log(cdi[-381,1]),
                 fitted = fitted(model2),
                 resid = model2$residuals)
ggplot(df2, aes(sample = scale(resid))) +
  stat_qq() +
  geom_abline(intercept = 0, slope = 1,
              col = "gray") +
  theme_bw() +
  xlab("Theoretical") +
  ylab("Standardised Residuals") +
  ggtitle("QQ-plot") +
  theme_bw()
# Linearity assumption (resolved)
ggplot(df2, aes(x =y, y = fitted)) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = F, col = "red") +
  geom_abline(intercept = 0, slope = 1) +
  xlab("Observed values") +
  ylab("Fitted values") +
  ggtitle("Observed vs Fitted Plot") +
  theme_bw()
# Homoscedasticity assumption (resolved)
ggplot(df2, aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = F, col = "red") +
  geom_abline(intercept = 0, slope = 1) +
  xlab("Fitted values") +
  ylab("Residuals") +
  ggtitle("Residuals Plot") +
  theme_bw()
# Independence assumption (resolved)
ggplot(df2, aes(x = 1:length(df2$y), y = resid)) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = F, col = "red") +
  xlab("Index") +
  ylab("Residuals") +
  ggtitle("Resisuals vs Time") +
  theme_bw()

m2 <- dim(cdi)[1]
x2 <- head(df$resid, m - 1)
y2 <- tail(df$resid, m - 1)
cor(x2, y2)
lag_df2 <- data.frame(x2, y2)
ggplot(lag_df2, aes(x = x2, y = y2)) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  xlab(expression(hat(epsilon)[i])) +
  ylab(expression(hat(epsilon)[i+1])) +
  ggtitle("Successive Residuals Plot") +
  theme_bw()
durbinWatsonTest(model2)





