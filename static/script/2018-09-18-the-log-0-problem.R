library(purrr) # v. 0.2.5
library(dplyr) # v. 0.7.6
library(broom) # v. 0.5.0
library(ggplot2) # v. 3.0.0
library(ggridges) # v. 0.5.0


set.seed(16)
beta0 = 0 # intercept
beta1 = .75 # slope

# Sample size
n = 50

# Explanatory variable
x = runif(n, min = -7, max = 0)


true_y = exp(beta0 + beta1*x + rnorm(n, mean = 0, sd = 2))
summary(true_y)


y = round(true_y, digits = 2)
summary(y)


replicate(100, sum( round( exp(beta0 + beta1*runif(n, min = -7, max = 0) + 
                                  rnorm(n, mean = 0, sd = 2)), 2) == 0) )


( true = lm(log(true_y) ~ x) )


( fit1 = lm(log(y + 1) ~ x) )


( fitc = lm(log(y + min(y[y>0])/2) ~ x) )


( fitq = lm(log(y + quantile(y, .25)^2/quantile(y, .75) ) ~ x) )

logy_0 = function(beta0 = 0, beta1 = .75, n) {
     x = runif(n, -7, 0) # create expl var between -10 and 0
     true_y = exp(beta0 + beta1*x + rnorm(n, 0, 2))
     y = round(true_y, 2)
     
     while( sum(y == 0 ) == 0 | sum(y == 0) > n/4) {
          true_y = exp(beta0 + beta1*x + rnorm(n, 0, 2))
          y = round(true_y, 2)
     }
     
     true = lm(log(true_y) ~ x)
     fit1 = lm(log(y + 1) ~ x)
     fitc = lm(log(y + min(y[y>0])/2) ~ x)
     fitq = lm(log(y + quantile(y, .25)^2/quantile(y, .75) ) ~ x)
     
     setNames(list(true, fit1, fitc, fitq), 
              c("True model", "Add 1", "Add 1/2 minimum > 0", "Quartile method") )
}


set.seed(16)
logy_0(n = 50)


models = replicate(1000, logy_0(n = 50), simplify = FALSE)


results = map_dfr(flatten(models), 
              ~tidy(.x, conf.int = TRUE), 
              .id = "model")
head(results)


results_sl = filter(results, term == "x")


results_sl %>%
     group_by(model) %>%
     summarise(med_estimate = median(estimate),
               CI_coverage = mean(conf.low < .75 & .75 < conf.high) )


ggplot(results_sl, aes(x = estimate, y = model, fill = model) ) +
     geom_density_ridges2(show.legend = FALSE, rel_min_height = 0.005) +
     geom_vline(xintercept = .75, size = 1) +
     scale_fill_viridis_d() +
     theme_bw() +
     scale_x_continuous(name = "Estimated slope", 
                        expand = c(0.01, 0),
                        breaks = seq(0, 1.5, by = .25) ) +
     scale_y_discrete(name = NULL, expand = expand_scale(mult = c(.01, .3) ) )

