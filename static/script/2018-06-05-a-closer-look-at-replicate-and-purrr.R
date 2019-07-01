library(purrr) # v. 0.2.5
suppressPackageStartupMessages( library(dplyr) ) # v. 0.7.5
library(ggplot2) # v 2.2.1


set.seed(16)
rnorm(5, mean = 0, sd = 1)


set.seed(16)
replicate(n = 3, rnorm(5, 0, 1), simplify = FALSE )


set.seed(16)
replicate(n = 3, rnorm(5, 0, 1) )


set.seed(16)
list1 = list() # Make an empty list to save output in
for (i in 1:3) { # Indicate number of iterations with "i"
    list1[[i]] = rnorm(5, 0, 1) # Save output in list for each iteration
}
list1


twogroup_fun = function(nrep = 10, b0 = 5, b1 = -2, sigma = 2) {
     ngroup = 2
     group = rep( c("group1", "group2"), each = nrep)
     eps = rnorm(ngroup*nrep, 0, sigma)
     growth = b0 + b1*(group == "group2") + eps
     growthfit = lm(growth ~ group)
     growthfit
}

twogroup_fun()


sim_lm = replicate(5, twogroup_fun(), simplify = FALSE )
length(sim_lm)


map(sim_lm, coef)


map_dbl(sim_lm, ~summary(.x)$r.squared)


map_dbl(sim_lm, function(x) summary(x)$r.squared)


map_dfr(sim_lm, broom::tidy)


map_dfr(sim_lm, broom::tidy, .id = "model")


map_dfr(sim_lm, broom::tidy, conf.int = TRUE)


sim_lm %>%
     map_dfr(broom::tidy) %>%
     filter(term == "(Intercept)") %>%
     qplot(x = estimate, data = ., geom = "histogram")

