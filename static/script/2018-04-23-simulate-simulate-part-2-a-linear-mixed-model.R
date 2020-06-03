set.seed(16)

nstand = 5
nplot = 4
mu = 10
sds = 2
sd = 1


( stand = rep(LETTERS[1:nstand], each = nplot) )

( plot = letters[1:(nstand*nplot)] )

( standeff = rnorm(nstand, 0, sds) )

( standeff = rep(standeff, each = nplot) )

( ploteff = rnorm(nstand*nplot, 0, sd) )


( dat = data.frame(stand, standeff, plot, ploteff) )


( dat$resp = with(dat, mu + standeff + ploteff ) )


library(lme4) # v. 1.1-21


fit1 = lmer(resp ~ 1 + (1|stand), data = dat)
fit1


twolevel_fun = function(nstand = 5, nplot = 4, mu = 10, sigma_s = 2, sigma = 1) {
     standeff = rep( rnorm(nstand, 0, sigma_s), each = nplot)
     stand = rep(LETTERS[1:nstand], each = nplot)
     ploteff = rnorm(nstand*nplot, 0, sigma)
     resp = mu + standeff + ploteff
     dat = data.frame(stand, resp)
     lmer(resp ~ 1 + (1|stand), data = dat)
}


set.seed(16)
twolevel_fun()


sims = replicate(100, twolevel_fun(), simplify = FALSE )
sims[[100]]


library(broom) # v. 0.5.6
tidy(fit1)


tidy(fit1, effects = "fixed")


tidy(fit1, effects = "ran_pars", scales = "vcov")


library(purrr) # v. 0.3.3
suppressPackageStartupMessages( library(dplyr) ) # v. 0.8.3
library(ggplot2) # v. 3.2.1


stand_sims = c(5, 20, 100) %>%
     set_names() %>%
     map(~replicate(1000, twolevel_fun(nstand = .x) ) )


stand_vars = stand_sims %>%
     modify_depth(2, ~tidy(.x, effects = "ran_pars", scales = "vcov") ) %>%
     map_dfr(bind_rows, .id = "stand_num") %>%
     filter(group == "stand")
head(stand_vars)


ggplot(stand_vars, aes(x = estimate) ) +
     geom_density(fill = "blue", alpha = .25) +
     facet_wrap(~stand_num) +
     geom_vline(xintercept = 4)


stand_vars = mutate(stand_vars, stand_num = forcats::fct_inorder(stand_num) )


add_prefix = function(string) {
     paste("Number stands:", string, sep = " ")
}


groupmed = stand_vars %>%
     group_by(stand_num) %>%
     summarise(mvar = median(estimate) )


ggplot(stand_vars, aes(x = estimate) ) + 
     geom_density(fill = "blue", alpha = .25) +
     facet_wrap(~stand_num, labeller = as_labeller(add_prefix) ) +
     geom_vline(aes(xintercept = 4, linetype = "True variance"), size = .5 ) +
     geom_vline(data = groupmed, aes(xintercept = mvar, linetype = "Median variance"),
                size = .5) +
     theme_bw(base_size = 14) +
     scale_linetype_manual(name = "", values = c(2, 1) ) +
     theme(legend.position = "bottom",
           legend.key.width = unit(.1, "cm") ) +
     labs(x = "Estimated Variance", y = NULL)


stand_vars %>%
     group_by(stand_num) %>%
     summarise_at("estimate", 
                  list(min = min, mean = mean, med = median, max = max) )


stand_vars %>%
     group_by(stand_num) %>%
     summarise(mean(estimate < 4) )


nstand = 5
nplot = 4
b0 = -1
b1 = .005
b2 = .1
sds = 2
sd = 1


set.seed(16)
stand = rep(LETTERS[1:nstand], each = nplot)
standeff = rep( rnorm(nstand, 0, sds), each = nplot)
ploteff = rnorm(nstand*nplot, 0, sd)


( elevation = rep( runif(nstand, 1000, 1500), each = nplot) )

( slope = runif(nstand*nplot, 2, 75) )

( resp2 = b0 + b1*elevation + b2*slope + standeff + ploteff )


lmer(resp2 ~ elevation + slope + (1|stand) )
