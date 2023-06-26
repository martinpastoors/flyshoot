# learn GAM
# https://noamross.github.io/gams-in-r-course
#
library(MASS)
library(mgcv)

mcycle <- MASS::mcycle

# Examine the mcycle data frame
head(mcycle)
plot(mcycle)

# Fit a linear model
lm_mod <- lm(accel ~ times, data = mcycle)

termplot(lm_mod, partial.resid = TRUE, se = TRUE)

# Fit the model
gam_mod <- gam(accel ~ s(times), data = mcycle)
plot(gam_mod, residuals = TRUE, pch = 1)

# extract the coefficients
coef(gam_mod)

# 6 setting complexity
gam_mod_k3 <- gam(accel ~ s(times, k = 3), data = mcycle); coef(gam_mod_k3)
gam_mod_k20 <- gam(accel ~ s(times, k = 20), data = mcycle); coef(gam_mod_k20)

par(mfrow = c(1, 2))
plot(gam_mod_k3, residuals = TRUE, pch = 1)
plot(gam_mod_k20, residuals = TRUE, pch = 1)

# 7 Smoothing parameters to avoid overfittting; Extract the smoothing parameter
gam_mod <- gam(accel ~ s(times), data = mcycle, method = "REML")
coef(gam_mod)
gam_mod_s1 <- gam(accel ~ s(times), data = mcycle, sp = 0.1)
gam_mod_s2 <- gam(accel ~ s(times), data = mcycle, sp = 0.0001)
par(mfrow = c(2, 1))
plot(gam_mod_s1, residuals = TRUE, pch = 1)
plot(gam_mod_s2, residuals = TRUE, pch = 1)

# 8. complexity and smoothing together
gam_mod_sk <- gam(accel ~ s(times, k=50), data = mcycle, sp=0.0001)
par(mfrow = c(2, 1))
plot(gam_mod, residuals = TRUE, pch = 1)
plot(gam_mod_sk, residuals = TRUE, pch = 1)

# 11 multivariate GAMS
data("mpg", package="gamair")
glimpse(mpg)
str(mpg)
mod_city <- gam(city.mpg ~ s(weight) + s(length) + s(price), 
                data = mpg, method = "REML")
plot(mod_city, pages = 1)

# 12 multivariate GAMS with factors
mod_city2 <- gam(city.mpg ~ s(weight) + s(length) + s(price) + fuel + drive + style, 
                 data = mpg, method = "REML")
plot(mod_city2, all.terms = TRUE, pages = 1)

# 13 Category level smooths
mod_city3 <- gam(city.mpg ~ s(weight, by=drive) + s(length, by=drive) + s(price, by=drive) + drive, 
                 data = mpg, method = "REML")
plot(mod_city3, all.terms = TRUE, pages = 1)

# Chapter 2 summarizing and interpreting

# 2 significance and linearity
mod_city4 <- gam(city.mpg ~ s(weight) + s(length) + s(price) + s(rpm) + s(width),
                 data = mpg, method = "REML")
plot(mod_city4, all.terms = TRUE, pages = 1)
summary(mod_city4)

# Family: gaussian 
# Link function: identity 
# 
# Formula:
#   city.mpg ~ s(weight) + s(length) + s(price) + s(rpm) + s(width)
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   25.201      0.188     134   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df      F  p-value    
# s(weight) 5.620  6.799 17.524  < 2e-16 ***
#   s(length) 2.943  3.759  0.904    0.421    
# s(price)  1.000  1.000 16.647 6.79e-05 ***
#   s(rpm)    7.751  8.499 16.486  < 2e-16 ***
#   s(width)  1.003  1.005  0.006    0.954    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# R-sq.(adj) =  0.831   Deviance explained = 84.7%
# -REML = 496.47  Scale est. = 7.0365    n = 199

# price is significant (p <0.05) and linear (edf near 1).

# 4 plotting motorcycle crash data
mod <- gam(accel ~ s(times), data = mcycle, method = "REML")
plot(mod, residuals = TRUE)
plot(mod, residuals = TRUE, pch = 1, cex = 1)

# 5 plotting multiple auto performance variables
# par(mfrow=c(1,1))
mod <- gam(hw.mpg ~ s(weight) + s(rpm) + s(price) + comp.ratio, 
           data = mpg, method = "REML")
plot(mod, select=3)
plot(mod, pages = 1, all.terms = TRUE)

# 6 visualizing uncertainty

# Plot the weight effect with colored shading
plot(mod, select = 1, shade=TRUE, shade.col = "hotpink")

# Make another plot adding the intercept value and uncertainty
par(mfrow=c(1,2))
plot(mod, select = 1, shade = TRUE, shade.col = "hotpink", 
     shift = coef(mod)[1])
plot(mod, select = 1, shade = TRUE, shade.col = "hotpink", 
     shift = coef(mod)[1], seWithMean = TRUE)
par(mfrow=c(1,1))

# Look up parameters for plot.gam
?plot.gam

# 8 reading model diagnostics
set.seed(0)
dat <- gamSim(1,n=200)
mod <- gam(y ~ s(x0, k = 5) + s(x1, k = 5) + s(x2, k = 5) + s(x3, k = 5),
           data = dat, method = "REML")

# Run the check function
gam.check(mod)

# Method: REML   Optimizer: outer newton
# full convergence after 10 iterations.
# Gradient range [-0.0001190691,0.0001259251]
# (score 460.9549 & scale 5.229925).
# Hessian positive definite, eigenvalue range [0.0001190726,97.53256].
# Model rank =  17 / 17 
# 
# Basis dimension (k) checking results. Low p-value (k-index<1) may
# indicate that k is too low, especially if edf is close to k'.
# 
#         k'  edf k-index p-value   
# s(x0) 4.00 2.54    0.99    0.47   
# s(x1) 4.00 2.25    0.95    0.18   
# s(x2) 4.00 3.94    0.84    0.01 **
# s(x3) 4.00 1.00    0.99    0.40   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# 9 fixing problems with k

dat <- mgcv::gamSim(1,n=600, scale=0.6, verbose=FALSE)
mod <- gam(y ~ s(x0, k = 3) + s(x1, k = 3) + s(x2, k = 3) + s(x3, k = 3),
           data = dat, method = "REML")

# Run the check function
gam.check(mod)

mod2 <- gam(y ~ s(x0, k = 3) + s(x1, k = 3) + s(x2, k = 10) + s(x3, k = 3),
           data = dat, method = "REML")

# Run the check function
gam.check(mod2)

# 11 examine overall concurvity

set.seed(0)
data("mpg", package="gamair", verbose=FALSE)

# Fit the model
mod <- gam(hw.mpg ~ s(length) + s(width) + s(height) + s(weight),
           data = mpg, method = "REML")

# Check overall concurvity
concurvity(mod, full = TRUE)
round(concurvity(mod, full = TRUE), 2)

# The least pre-determined variable generally has the lowest concurvity according to all three ways of measuring it.
# It is sometime helpful to wrap the concurvity() results in a round() function for easier inspection, like so: round(concurvity(mod, full = TRUE), 2).

# para s(length) s(width) s(height) s(weight)
# worst       0      0.93     0.93      0.67      0.96
# observed    0      0.75     0.88      0.49      0.88
# estimate    0      0.84     0.79      0.45      0.86

# height has relatively low concurvity. It isn’t too similar to any of the other variables.

# 12 examining concurvity between auto variables

mod <- gam(hw.mpg ~ s(length) + s(width) + s(height) + s(weight),
           data = mpg, method = "REML")

concurvity(mod, full = FALSE)
round(concurvity(mod, full = FALSE)$worst,2)
round(concurvity(mod, full = FALSE)$observed,2)
round(concurvity(mod, full = FALSE)$estimate,2)

# para s(length) s(width) s(height) s(weight)
# para         1      0.00     0.00      0.00      0.00
# s(length)    0      1.00     0.83      0.61      0.88
# s(width)     0      0.83     1.00      0.41      0.90
# s(height)    0      0.61     0.41      1.00      0.37
# s(weight)    0      0.88     0.90      0.37      1.00

# weight and width have worst-case concurvity of about 0.895

# Chapter 3 spatial GAMs and interactions

# 2 modeling soil pollution in the Netherlands

data(meuse, package="sp")
head(meuse)
str(meuse)

mod2d <- gam(cadmium ~ s(x,y), data = meuse, method = "REML")

summary(mod2d)
coef(mod2d)
plot(mod2d)

# 3 adding more variables

mod2da <- gam(cadmium ~ s(x,y) + s(elev) + s(dist), data = meuse, method = "REML")

summary(mod2da)
coef(mod2da)
plot(mod2da)  # one by one
plot(mod2da, pages = 1, all.terms = TRUE)

# 4 plotting and interpreting GAM interactions

# 5 Plotting the model surface

plot(mod2da, pages = 1)

plot(mod2da, scheme=1, pages = 1) # 3D map

plot(mod2da, scheme = 2, pages = 1) # coloured heatmap

# 6 customizing 3d plots

mod2d <- gam(cadmium ~ s(x,y), data=meuse, method = "REML")

# Make the perspective plot with error surfaces
vis.gam(mod2d, view = c("x", "y"), 
        plot.type = "persp", se = 2)

vis.gam(mod2d, view = c("x", "y"), 
        plot.type = "persp", se = 2, theta=135)
vis.gam(mod2d, view = c("x", "y"), 
        plot.type = "persp", se = 2, theta=135, ticktype="detailed")

# 7 extrapolations

# Make plot with 5% extrapolation
vis.gam(mod2d, view = c("x", "y"), 
        plot.type = "contour", too.far = 0.05)

# Overlay data
points(meuse)

# Make plot with 10% extrapolation
vis.gam(mod2d, view = c("x", "y"), 
        plot.type = "contour", too.far = 0.1)

# Overlay data
points(meuse)

# Make plot with 10% extrapolation
vis.gam(mod2d, view = c("x", "y"), 
        plot.type = "contour", too.far = 0.25)

# Overlay data
points(meuse)

# 8 visualizing categorical -continuous interactions

# 9 soil pollution in different land uses

mod_sep <- gam(copper ~ s(dist, by = landuse) + landuse,
               data = meuse, method = "REML")

summary(mod_sep)

mod_fs <- gam(copper ~ s(dist, landuse, bs = "fs"),
              data = meuse, method = "REML")

summary(mod_fs)

plot(mod_sep, pages = 1)
plot(mod_fs, pages = 1)

vis.gam(mod_sep, view = c("dist", "landuse"), plot.type = "persp", theta=135)
vis.gam(mod_fs, view = c("dist", "landuse"), plot.type = "persp", theta=135)

# 12 multi-scale interactions
# In this exercise, you’ll build a model that allows multiple variables to interact despite these different scales using a tensor smooth, te().

tensor_mod <- gam(cadmium ~ te(x, y, elev), 
                  data = meuse, method = "REML")

summary(tensor_mod)
plot(tensor_mod)

tensor_mod2 <- gam(cadmium ~ s(x, y) + s(elev) + ti(x, y, elev), 
                   data = meuse, method = "REML")

# Summarize and plot
summary(tensor_mod2)
plot(tensor_mod2, pages = 1)
plot(tensor_mod2, select=2)

