library(lme4) # v. 1.1-23
suppressPackageStartupMessages( library(dplyr) ) # v. 1.0.0
library(purrr) # 0.3.4
library(broom.mixed) # 0.2.6


glimpse(cbpp)


set.seed(16)

cbpp = mutate(cbpp, 
              y1 = rnorm(56, 500, 100),
              y2 = runif(56, 0, 1),
              y3 = runif(56, 10000, 20000) )

glimpse(cbpp)


fit1 = glmer( cbind(incidence, size - incidence) ~ y1 + y2 + y3 + (1|herd),
              data = cbpp, family = binomial)


cbpp = mutate_at(cbpp, 
                 .vars = vars( y1:y3 ), 
                 .funs = list(s = ~as.numeric( scale(.) ) ) )

glimpse(cbpp)


fit2 = glmer( cbind(incidence, size - incidence) ~ y1_s + y2_s + y3_s + (1|herd),
              data = cbpp, family = binomial)


coef_st = tidy(fit2, 
               effects = "fixed",
               conf.int = TRUE,
               conf.method = "profile")

coef_st


cbpp %>%
     select(y1:y3) %>%
     map(sd) %>% 
     stack()


sd_all = cbpp %>%
     select(y1:y3) %>%
     map(sd) %>%
     stack() %>%
     rename(sd = values) %>%
     mutate(ind = paste(ind, "s", sep = "_") )

sd_all


coef_st %>%
     inner_join(., sd_all, by = c("term" = "ind") )


coef_st %>%
     inner_join(., sd_all, by = c("term" = "ind") ) %>%
     mutate_at( .vars = vars(estimate, conf.low, conf.high), 
                .funs = list(~round( ./sd, 4) ) )


coef_unst = coef_st %>%
     inner_join(., sd_all, by = c("term" = "ind") ) %>%
     mutate_at( .vars = vars(estimate, conf.low, conf.high), 
                .funs = list(~round( ./sd, 4) ) ) %>%
     select(-effect, -(std.error:p.value), -sd)

coef_unst


round( fixef(fit1)[2:4], 4)


tidy(fit1, 
     effects = "fixed",
     conf.int = TRUE,
     conf.method = "profile")

