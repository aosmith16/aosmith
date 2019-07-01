library(lme4) # v. 1.1-15
suppressPackageStartupMessages( library(dplyr) ) # v. 0.7.4
library(purrr) # 0.2.4
library(broom) # 0.4.3


glimpse(cbpp)


set.seed(16)

cbpp = mutate(cbpp, y1 = rnorm(56, 500, 100),
              y2 = runif(56, 0, 1),
              y3 = runif(56, 10000, 20000) )

glimpse(cbpp)


fit1 = glmer( cbind(incidence, size - incidence) ~ y1 + y2 + y3 + (1|herd),
              data = cbpp, family = binomial)


cbpp = mutate_at(cbpp, vars( y1:y3 ), funs(s = as.numeric( scale(.) ) ) )

glimpse(cbpp)


fit2 = glmer( cbind(incidence, size - incidence) ~ y1_s + y2_s + y3_s + (1|herd),
              data = cbpp, family = binomial)


coef_st = tidy(fit2, effects = "fixed",
     conf.int = TRUE,
     conf.method = "profile")

coef_st


map( select(cbpp, y1:y3), sd) %>% 
     stack()


sd_all = map( select(cbpp, y1:y3), sd) %>% 
     stack() %>%
     mutate(ind = paste(ind, "s", sep = "_") )

sd_all


coef_st %>%
     inner_join(., sd_all, by = c("term" = "ind") )


coef_st %>%
     inner_join(., sd_all, by = c("term" = "ind") ) %>%
     mutate_at( vars(estimate, conf.low, conf.high), funs(round( ./values, 4) ) )


coef_unst = coef_st %>%
     inner_join(., sd_all, by = c("term" = "ind") ) %>%
     mutate_at( vars(estimate, conf.low, conf.high), funs(round( ./values, 4) ) ) %>%
     select(-(std.error:p.value), -values)

coef_unst


round( fixef(fit1)[2:4], 4)


tidy(fit1, effects = "fixed",
     conf.int = TRUE,
     conf.method = "profile")

