library(purrr)
library(broom)
suppressMessages( library(dplyr) )
library(ggplot2)


set.seed(16)

ngroup = 2
nrep = 10
b0 = 5
b1 = -2
sd = 2


( group = rep( c("group1", "group2"), each = nrep) )

( eps = rnorm(ngroup*nrep, 0, sd) )

( growth = b0 + b1*(group == "group2") + eps )


growthfit = lm(growth ~ group)
summary(growthfit)


twogroup_fun = function(nrep = 10, b0 = 5, b1 = -2, sigma = 2) {
     ngroup = 2
     group = rep( c("group1", "group2"), each = nrep)
     eps = rnorm(ngroup*nrep, 0, sigma)
     growth = b0 + b1*(group == "group2") + eps
     growthfit = lm(growth ~ group)
     growthfit
}


set.seed(16)
twogroup_fun()


twogroup_fun(sigma = 1)


sims = rerun(1000, twogroup_fun() )


tidy(growthfit)


summary(growthfit)$sigma


sims %>%
     map_df(tidy) %>%
     filter(term == "groupgroup2") %>%
     ggplot( aes(estimate) ) +
          geom_density(fill = "blue", alpha = .5) +
          geom_vline( xintercept = -2)


sims %>%
     map_dbl(~summary(.x)$sigma) %>%
     data.frame(sigma = .) %>%
     ggplot( aes(sigma) ) +
          geom_density(fill = "blue", alpha = .5) +
          geom_vline(xintercept = 2)


sims %>%
     map_dbl(~summary(.x)$sigma) %>%
     {. < 2} %>%
     mean()


sims %>%
     map_df(tidy) %>%
     filter(term == "groupgroup2") %>%
     pull(p.value) %>%
     {. <  0.05} %>%
     mean()

