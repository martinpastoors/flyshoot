# Learn GAM Michael Clark

library(mgcv)

fn <- "https://raw.githubusercontent.com/m-clark/generalized-additive-models/master/data/pisasci2006.csv"
pisa = read_csv(fn)

my_fn <- function(data, mapping, method="loess", ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=method, ...)
  p
}

GGally::ggpairs(pisa, columns=2:ncol(pisa), lower = list(continuous = my_fn))

# linear model
mod_lm = gam(Overall ~ Income, data = pisa)
summary(mod_lm)

# GAM with cubic regression as basis
mod_gam1 = gam(Overall ~ s(Income, bs = "cr"), data = pisa)
summary(mod_gam1)
plot(mod_gam1)

# model comparison
anova(mod_lm, mod_gam1, test = "Chisq")

# Adding multiple features

mod_lm2 = gam(Overall ~ Income + Edu + Health, data = pisa)
summary(mod_lm2)

mod_gam2 = gam(Overall ~ s(Income) + s(Edu) + s(Health), data = pisa)
summary(mod_gam2)
plot(mod_gam2, page=1)

# Family: gaussian 
# Link function: identity 
# 
# Formula:
#   Overall ~ s(Income) + s(Edu) + s(Health)
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  471.154      2.772     170   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df     F  p-value    
# s(Income) 7.593  8.415 8.826 1.29e-06 ***
#   s(Edu)    6.204  7.178 3.308  0.00771 ** 
#   s(Health) 1.000  1.000 2.736  0.10679    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# R-sq.(adj) =  0.863   Deviance explained = 90.3%
# GCV = 573.83  Scale est. = 399.5     n = 52

# update the model by making health linear
mod_gam2B = update(mod_gam2, . ~ . - s(Health) + Health)
summary(mod_gam2B)

plot(ggeffects::ggpredict(mod_gam2), facets = TRUE)

# creating the predictions "manually" for Income, while keeping the others constant
# Note that mod_gam2$model is the data that was used in the modeling process, 
# so it will have NAs removed.
testdata = data.frame(
  Income = seq(.4, 1, length = 100),
  Edu    = mean(mod_gam2$model$Edu),
  Health = mean(mod_gam2$model$Health)
)

predictions = predict(
  mod_gam2,
  newdata = testdata,
  type = 'response',
  se = TRUE
)

df_preds = data.frame(testdata, predictions) %>%
  mutate(lower = fit - 1.96 * se.fit,
         upper = fit + 1.96 * se.fit)

ggplot(aes(x = Income, y = fit), data = df_preds) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = 'gray') +
  geom_line(color = '#56B4E9')

# plotting 2d smooths
vis.gam(mod_gam2, type = 'response', plot.type = 'contour')

# So let’s take a look at another approach, continuing the focus on visualization. It may not be obvious at all, 
# but one can utilize smooths of more than one feature, in effect, a smooth of the smooths of the variables that 
# go into it. This is akin to an interaction in typical model settings27. Let’s create a new model to play around 
# with this. After fitting the model, I provide both a visualization for comparison to the previous, as well as 
# a 3D view one can interactively rotate to their liking.

# Tensors are smooths of smooths

mod_gam3 = gam(Overall ~ te(Income, Edu), data = pisa)
summary(mod_gam3)
vis.gam(mod_gam3, type = 'response', plot.type = 'contour')
vis.gam(mod_gam3, type = 'response', plot.type = 'persp')
vis.gam(mod_gam3, type = 'response', plot.type = 'persp', phi=30)
vis.gam(mod_gam3, type = 'response', plot.type = 'persp', phi=30, theta=30)
vis.gam(mod_gam3, type = 'response', plot.type = 'persp', phi=30, theta=30, n.grid=100)
vis.gam(mod_gam3, type = 'response', plot.type = 'persp', phi=30, theta=30, n.grid=100, ticktype="detailed")

# Model comparison
anova(mod_lm2, mod_gam2, mod_gam2B, test = "Chisq")

# Diagnostics
gam.check(mod_gam2, k.rep = 1000)

# mod_gam2 = gam(Overall ~ s(Income) + s(Edu) + s(Health), data = pisa)
viz <- getViz(mod_gam2)
mgcViz::check.gamViz(viz,
      # a.qq = list(method = "tnorm",
      a.qq = list(method = "simul1", CI="normal",
                  a.cipoly = list(fill = "lightblue"),
                  a.qqpoi = list(shape=19, size=1)), 
      a.respoi = list(size = 1), 
      a.hist = list(bins = 20))

ck1 <- check1D(viz, "Income")
ck2 <- check1D(viz, "Edu")
ck3 <- check1D(viz, "Health")
gridPrint(ck1 + l_dens(type = "cond", alpha = 0.8) + l_rug(alpha = 0.2), 
          ck2 + l_points(shape=19) + l_rug(alpha = 0.2), 
          ck3 + l_points(shape=19) + l_rug(alpha = 0.2), 
          layout_matrix = rbind(c(1, 1, 1, 1),
                                c(2, 2, 3, 3))
          # ncol=3
)

ck1 + l_densCheck()

viz <- getViz(mod_gam2, nsim = 50)
gridPrint(check1D(viz, "Income") + l_gridCheck1D(gridFun = sd, showReps = TRUE), 
          check1D(viz, "Edu") + l_gridCheck1D(gridFun = sd, showReps = TRUE), 
          check1D(viz, "Health") + l_gridCheck1D(gridFun = sd, showReps = TRUE), 
          ncol = 3)
# Concurvity
round(concurvity(mod_gam2),2)

# Model comparison
mod_1d = gam(Overall ~ s(Income) + s(Edu), data = pisa)
mod_2d = gam(Overall ~ te(Income, Edu, bs = "tp"), data = pisa)

AIC(mod_1d, mod_2d)

# te produces a full tensor product smooth, while ti produces a tensor product interaction, 
# appropriate when the main effects (and any lower interactions) are also present.

mod_A = gam(Overall ~ s(Income, bs = "cr", k = 5) + s(Edu, bs = "cr", k = 5), data = pisa)

mod_B = gam(Overall ~ ti(Income, bs = "cr", k = 5) + ti(Edu, bs = "cr", k = 5) + ti(Income, Edu, bs = 'cr'), data = pisa)

anova(mod_A, mod_B, test = "Chisq")

