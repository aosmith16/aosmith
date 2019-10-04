library(purrr) # 0.3.2
library(tidyr) # 1.0.0
library(dplyr) # 0.8.3


dat = structure(list(plot = structure(1:8, .Label = c("plot1", "plot2", 
"plot3", "plot4", "plot5", "plot6", "plot7", "plot8"), class = "factor"), 
    group = structure(c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L), .Label = c("g1", 
    "g2"), class = "factor"), num_dead = c(4L, 6L, 6L, 5L, 1L, 4L, 
    3L, 2L), total = c(5L, 7L, 9L, 7L, 8L, 10L, 10L, 7L)), class = "data.frame", row.names = c(NA, 
-8L))

dat


binary_dat = pmap_dfr(dat, 
                      function(group, plot, num_dead, total) {
                           data.frame(plot = plot,
                                      group = group,
                                      dead = c( rep(1, num_dead),
                                                rep(0, total - num_dead) ) )
                      }
)


head(binary_dat)

dat[1, ]


# function(group, plot, num_dead, total, ...)
# 

fit = glm( cbind(num_dead, total - num_dead) ~ group, 
           data = dat,
           family = binomial)
summary(fit)$coefficients


fit_binary = glm( dead ~ group, 
                  data = binary_dat,
                  family = binomial)
summary(fit_binary)$coefficients


binary_dat2 = dat %>%
     nest(data = c(num_dead, total) ) %>%
     mutate(dead = map(data, ~c( rep(1, .x$num_dead),
                                 rep(0, .x$total - .x$num_dead) ) ) ) %>%
     select(-data) %>%
     unnest(dead)
head(binary_dat2)
