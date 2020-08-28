library(purrr) # v. 0.3.4
library(lme4) # v. 1.1-23


dat = structure(list(group = c("a", "a", "a", "a", "a", "a", "b", "b", "b"), 
                     x = c("A", "A", "A", "B", "B", "B", "A", "A", "A"), 
                     y = c(10.9, 11.1, 10.5, 9.7, 10.5, 10.9, 13, 9.9, 10.3)), 
                class = "data.frame", 
                row.names = c(NA, -9L))
dat


dat_split = split(dat, dat$group)

map(dat_split, ~lm(y ~ x, data = .x) )


lm(y ~ x, data = dat, subset = group == "a")

lm(y ~ x, data = dat, subset = group == "b")


posslm1 = possibly(.f = lm, otherwise = "Error")

map(dat_split, ~posslm1(y ~ x, data = .x) )


posslm2 = possibly(.f = lm, otherwise = NULL)
( mods = map(dat_split, ~posslm2(y ~ x, data = .x) ) )


mods %>%
     keep(~is.null(.x) )


group_errs = mods %>%
     keep(~is.null(.x) ) %>%
     names()
group_errs


dat[dat$group %in% group_errs, ]

dat_split[group_errs]


compact(mods)


safelm = safely(.f = lm)
mods2 = map(dat_split, ~safelm(y ~ x, data = .x) )

mods2[[1]]

mods2[[2]]


map(mods2, "error")


mods2 %>%
     map("result") %>%
     compact()


twolevel_fun = function(nstand = 5, nplot = 4, mu = 10, sigma_s = 1, sigma = 1) {
     standeff = rep( rnorm(nstand, 0, sigma_s), each = nplot)
     stand = rep(LETTERS[1:nstand], each = nplot)
     ploteff = rnorm(nstand*nplot, 0, sigma)
     resp = mu + standeff + ploteff
     dat = data.frame(stand, resp)
     lmer(resp ~ 1 + (1|stand), data = dat)
}


set.seed(16)
sims = replicate(10, twolevel_fun(), simplify = FALSE )

sims[[2]]

sims[[2]]@optinfo$conv$lme4


qlmer = quietly(.f = lmer)
qtwolevel_fun = function(nstand = 5, nplot = 4, mu = 10, sigma_s = 1, sigma = 1) {
     standeff = rep( rnorm(nstand, 0, sigma_s), each = nplot)
     stand = rep(LETTERS[1:nstand], each = nplot)
     ploteff = rnorm(nstand*nplot, 0, sigma)
     resp = mu + standeff + ploteff
     dat = data.frame(stand, resp)
     qlmer(resp ~ 1 + (1|stand), data = dat)
}


set.seed(16)
sims2 = replicate(10, qtwolevel_fun(), simplify = FALSE)
sims2[[2]]


sims2 %>%
     map("messages") %>% 
     unlist()


sims2 %>%
     map(`[`, c("messages", "warnings") ) %>%
     unlist()

