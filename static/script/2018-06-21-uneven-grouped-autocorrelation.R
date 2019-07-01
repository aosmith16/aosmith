library(purrr) # v. 0.2.5
library(dplyr) # v. 0.7.5


set.seed(64)


dat = map_dfr(1:9, 
               ~tibble(unit = .x,
                     x = runif(10, 5, 10),
                     y = 1 + x + arima.sim(list(ar = .7), 10),
                     time = 1:10)
)

head(dat, 15)


dat = dat %>%
     filter(unit %in% 4:9 | time %in% c(1, 4, 7, 10) ) %>%
     filter(!unit %in% 4:6 | time %in% c(1, 10) )

head(dat, 15)


fit1 = lm(y ~ x, data = dat)

dat$res = residuals(fit1)


dat %>%
    count(unit)


dat %>%
    count(unit) %>%
    filter(n == max(n) )


dat = dat %>%
     arrange(unit, time)


library(tidyr) # v. 0.8.1


dat_expand = dat %>%
    group_by(unit) %>%
    complete(time = 1:20) 


filter(dat_expand, unit == 1)


acf(dat_expand$res, lag.max = 9, na.action = na.pass, ci = 0)


( nall = map_df(1:9, 
                ~dat %>%
                     group_by(unit) %>%
                     arrange(unit, time) %>%
                     summarise(lag = list( diff(time, lag = .x ) ) )
     ) %>%
       unnest(lag) %>%
       group_by(lag) %>%
       summarise(n = n() ) )


acf(dat_expand$res, lag.max = 9, na.action = na.pass, ci = 0)
lines(1:9,-qnorm(1-.025)/sqrt(nall$n), lty = 2)
lines(1:9, qnorm(1-.025)/sqrt(nall$n), lty = 2)

