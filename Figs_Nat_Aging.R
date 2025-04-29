library(ggplot2)

# Regression Women
hmdGPT_2020 <- subset(hmd_GPT, Year < 2021)
bestpW <- lm(Max_Female ~ Year, data = hmdGPT_2020)
summary(bestpW)

bestp2000 <- lm(Max_Female ~ Year, 
                data = hmdGPT_2020[hmdGPT_2020$Year>2000,])
summary(bestp2000)

# Regression Men
bestpM <- lm(Max_Male ~ Year, data = hmdGPT_2020)
summary(bestpM)
confint(bestpW)
bestpM2000 <- lm(Max_Male ~ Year, 
                 data = hmdGPT_2020[hmdGPT_2020$Year>2000,])
summary(bestpM2000)

# Figure 1

# 1) Fit models
mod_f <- lm(Max_Female ~ Year, data = hmdGPT_2020)
mod_m <- lm(Max_Male   ~ Year, data = hmdGPT_2020)

# 2) Make a grid of Years
year_grid <- data.frame(Year = seq(min(hmdGPT_2020$Year),
                                   max(hmdGPT_2020$Year),
                                   by = 1))

# 3) Get 95% prediction intervals
pred_f <- as.data.frame(predict(mod_f, year_grid, 
                                interval = "prediction", 
                                level    = 0.95))
pred_f$Year   <- year_grid$Year
pred_f$Gender <- "Women"

pred_m <- as.data.frame(predict(mod_m, year_grid, 
                                interval = "prediction", 
                                level    = 0.95))
pred_m$Year   <- year_grid$Year
pred_m$Gender <- "Men"

preds <- rbind(pred_f, pred_m)
# preds has columns: fit, lwr, upr, Year, Gender

# 4) Plot points, mean fits, and prediction bands
p1 <- ggplot() +
  # ribbon for the 95% prediction band
  geom_ribbon(data = preds,
              aes(x = Year, ymin = lwr, ymax = upr, fill = Gender),
              alpha = 0.2) +
  # mean regression lines
  geom_line(data = preds,
            aes(x = Year, y = fit, colour = Gender),
            size = 1) +
  # original points
  geom_point(data = hmdGPT_2020,
             aes(x = Year, y = Max_Female, colour = "Women"),
             shape = 1, size = 2) +
  geom_point(data = hmdGPT_2020,
             aes(x = Year, y = Max_Male, colour = "Men"),
             shape = 1, size = 2) +
  # manual scales for colour and fill
  scale_colour_manual(name  = "",
                      values = c(Women = "tomato", Men = "cyan3")) +
  scale_fill_manual(name   = "",
                    values = c(Women = "tomato", Men = "cyan3")) +
  labs(x = "Year",
       y = "Life expectancy at birth") +
  theme_minimal()

p2 <-p1 + geom_point(data = hmd_GPT[hmd_GPT$Year>2000 & hmd_GPT$Year<2021,],
                     aes(x = Year, y = Max_Female),
                     color = "tomato4",
                     size  = 2)

p2
p3 <- p2 + geom_point(data = hmd_GPT[hmd_GPT$Year>2000 & hmd_GPT$Year<2021,],
                      aes(x = Year, y = Max_Male),
                      color = "cyan4",
                      size  = 2)
p3



# Fig S1 annual rate females
rateW <- numeric(length(hmdGPT_2020$Year) - 1)
for (i in 1:(length(hmdGPT_2020$Year) - 1)) {
  rateW[i] <-  hmdGPT_2020$Max_Female[i + 1] - hmdGPT_2020$Max_Female[i]
}
rateW <- data.frame(rateW = rateW, Year = seq(1842, 2020, 1))
plot(rateW ~ Year, data = rateW, xlab = "Year",
     ylab = "Change in longevity (women)")
points(rateW ~ Year, 
       data = rateW[rateW$Year>2000,],
       col = "tomato3", pch = 19) 
abline(h=0)
summary(lm(rateW ~ Year, data = rateW))

# Fig S2 1940-2020
plot(rateW[hmd_GPT$Year>1939 & hmd_GPT$Year<2021] ~ 
       Year[hmd_GPT$Year>1939 & hmd_GPT$Year<2021], 
     data = rateW, xlab = "Year", 
     ylab = "Change in longevity (women)")
points(rateW ~ Year, 
       data = rateW[rateW$Year>2000,],
       col = "tomato3", pch = 19) 
abline(h=0)
mean(rateW$rateW[rateW$Year > 2000 & rateW$Year < 2021])


# Fig S3 annual rate males
rateM <- numeric(length(hmdGPT_2020$Year) - 1)
for (i in 1:(length(hmdGPT_2020$Year) - 1)) {
  rateM[i] <-  hmdGPT_2020$Max_Male[i + 1] - hmdGPT_2020$Max_Male[i]
}
rateM <- data.frame(rateM = rateM, Year = seq(1842, 2020, 1))
plot(rateM ~ Year, data = rateM,
     xlab = "Year",
     ylab = "Change in longevity (men)")

points(rateM ~ Year, 
       data = rateM[rateM$Year>2000,],
       col = "cyan4", pch = 19) 
abline(h=0)
summary(lm(rateW ~ Year, data = rateW))

# Fig S4 1940-2020
plot(rateM[hmd_GPT$Year>1939 & hmd_GPT$Year<2021] ~ 
       Year[hmd_GPT$Year>1939 & hmd_GPT$Year<2021], 
     data = rateM, 
     xlab = "Year",
     ylab = "Change in longevity (men)")

points(rateM ~ Year, 
       data = rateM[rateM$Year>2000,],
       col = "cyan4", pch = 19) 
abline(h=0)
mean(rateM$rateM[rateM$Year > 2000 & rateM$Year < 2021])
# 2001-2020 increase
(82.17-78.26)/2
# 1981-2000
(77.84-73.81)/2

# Fig S5 
# 10-year rates women
hmdGPT10 <- hmdGPT_2020[hmdGPT_2020$Year > 1939 
                        & hmdGPT_2020$Year %% 10 == 0, ]
rate10W <- numeric(length(hmdGPT10$Year) - 1)
for (i in 1:(length(hmdGPT10$Year) - 1)) {
  rate10W[i] <-  hmdGPT10$Max_Female[i + 1] - hmdGPT10$Max_Female[i]
}
rate10W <- data.frame(rate10W = rate10W, Year = seq(1950, 2020, 10))
plot(rate10W ~ Year, data = rate10W, ylim = c(-1, 5), type = "b",
     col = "tomato3", pch = 19,
     ylab = "Change in longevity (decade)")
abline(h=0)
summary(lm(rate10W ~ Year, data = rate10W))

# 10-year rate men
rate10M <- numeric(length(hmdGPT10$Year) - 1)
for (i in 1:(length(hmdGPT10$Year) - 1)) {
  rate10M[i] <-  hmdGPT10$Max_Male[i + 1] - hmdGPT10$Max_Male[i]
}
rate10M <- data.frame(rate10M = rate10M, Year = seq(1950, 2020, 10))
points(rate10M ~ Year, data = rate10M, type = "b",
       col = "cyan4", pch = 19)
summary(lm(rate10W ~ Year, data = rate10W))


# Fig S6

library(ggplot2)
library(viridis)

# shapes and colours
my_shapes <- setNames(c(15:20, 8, 13), levels(hmdGPT_2020$Country_Female))
my_cols   <- setNames(viridis(length(my_shapes)), levels(hmdGPT_2020$Country_Female))

ggplot(hmdGPT_2020, aes(Year, Max_Female)) +
  # points, mapping both aesthetics to the same variable
  geom_point(
    aes(shape = Country_Female, colour = Country_Female),
    size = 1
  ) +
  # regression line
  geom_smooth(
    inherit.aes = FALSE,
    aes(x = Year, y = Max_Female),
    method = "lm", se = FALSE, colour = "grey50", size = 0.5
  ) +
  # shape scale: no legend (we'll get it via colour)
  scale_shape_manual(
    name  = "", 
    values = my_shapes,
    guide  = "none"
  ) +
  # colour scale: single legend, but draw the shapes too
  scale_colour_manual(
    name  = "", 
    values = my_cols,
    guide = guide_legend(
      override.aes = list(shape = my_shapes)
    )
  ) +
  labs(
    x = "Year",
    y = "Life expectancy at birth (women)"
  ) +
  theme_minimal()

# Fig S7
my_shapesM <- setNames(c(0:9), levels(hmdGPT_2020$Country_Male))
my_cols   <- setNames(viridis(length(my_shapesM)), levels(hmdGPT_2020$Country_Male))

ggplot(hmdGPT_2020, aes(Year, Max_Male)) +
  # points, mapping both aesthetics to the same variable
  geom_point(
    aes(shape = Country_Male, colour = Country_Male),
    size = 1
  ) +
  # regression line
  geom_smooth(
    inherit.aes = FALSE,
    aes(x = Year, y = Max_Male),
    method = "lm", se = FALSE, colour = "grey50", size = 0.5
  ) +
  # shape scale: no legend (we'll get it via colour)
  scale_shape_manual(
    name  = "", 
    values = my_shapesM,
    guide  = "none"
  ) +
  # colour scale: single legend, but draw the shapes too
  scale_colour_manual(
    name  = "", 
    values = my_cols,
    guide = guide_legend(
      override.aes = list(shape = my_shapesM)
    )
  ) +
  labs(
    x = "Year",
    y = "Life expectancy at birth (men)"
  ) +
  theme_minimal()







