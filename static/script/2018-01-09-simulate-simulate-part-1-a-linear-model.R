library(purrr) # v. 0.3.4
library(broom) # v. 0.5.6
library(dplyr) # v. 1.0.0
library(ggplot2) # v. 3.3.1




set.seed(16)

ngroup = 2
nrep = 10
b0 = 5
b1 = -2
sd = 2


( group = rep( c("group1", "group2"), each = nrep) )

( eps = rnorm(n = ngroup*nrep, mean = 0, sd = sd) )

( growth = b0 + b1*(group == "group2") + eps )


dat = data.frame(group, growth)


growthfit = lm(growth ~ group, data = dat)
summary(growthfit)


twogroup_fun = function(nrep = 10, b0 = 5, b1 = -2, sigma = 2) {
     ngroup = 2
     group = rep( c("group1", "group2"), each = nrep)
     eps = rnorm(n = ngroup*nrep, mean = 0, sd = sigma)
     growth = b0 + b1*(group == "group2") + eps
     simdat = data.frame(group, growth)
     growthfit = lm(growth ~ group, data = simdat)
     growthfit
}


set.seed(16)
twogroup_fun()


twogroup_fun(sigma = 1)


sims = replicate(n = 1000, twogroup_fun(), simplify = FALSE )
sims[[1]]


tidy(growthfit)


summary(growthfit)$sigma


sims %>%
     map_df(tidy) %>%
     filter(term == "groupgroup2") %>%
     ggplot( aes(x = estimate) ) +
          geom_density(fill = "blue", alpha = .5) +
          geom_vline( xintercept = -2)


sims %>%
     map_dbl(~summary(.x)$sigma) %>%
     data.frame(sigma = .) %>%
     ggplot( aes(x = sigma) ) +
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

