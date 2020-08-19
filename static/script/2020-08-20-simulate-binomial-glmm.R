library(lme4) # v. 1.1-23
library(purrr) # v. 0.3.4
library(ggplot2) # v. 3.3.2




set.seed(16)

codds = .5/(1 - .5)
todds = .85/(1 - .85)
todds/codds

log(todds/codds)

log(codds)

b0 = 0
b1 = 1.735
site_var = 0.5
n_sites = 10


site = rep(LETTERS[1:n_sites], each = 2)
plot = paste(site, rep(1:2, times = n_sites), sep = "." )
treatment = rep( c("treatment", "control"), times = n_sites)
dat = data.frame(site, plot, treatment)
dat


( site_eff = rep( rnorm(n = n_sites, 
                        mean = 0, 
                        sd = sqrt(site_var) ), 
                  each = 2) )


( log_odds = with(dat, b0 + b1*(treatment == "treatment") + site_eff ) )

( prop = plogis(log_odds) )


dat$num_samp = 50



( dat$y = rbinom(n = n_sites*2, size = dat$num_samp, prob = prop) )


mod = glmer(cbind(y, num_samp - y) ~ treatment + (1|site), 
            data = dat,
            family = binomial(link = "logit") )
mod


bin_glmm_fun = function(n_sites = 10,
                        b0 = 0,
                        b1 = 1.735,
                        num_samp = 50,
                        site_var = 0.5) {
     site = rep(LETTERS[1:n_sites], each = 2)
     plot = paste(site, rep(1:2, times = n_sites), sep = "." )
     treatment = rep( c("treatment", "control"), times = n_sites)
     dat = data.frame(site, plot, treatment)           
     
     site_eff = rep( rnorm(n = n_sites, mean = 0, sd = sqrt(site_var) ), each = 2)
     
     log_odds = with(dat, b0 + b1*(treatment == "treatment") + site_eff)
     prop = plogis(log_odds)
     dat$num_samp = num_samp
     dat$y = rbinom(n = n_sites*2, size = num_samp, prob = prop)
     
     glmer(cbind(y, num_samp - y) ~ treatment + (1|site),
           data = dat,
           family = binomial(link = "logit") )
}



set.seed(16)
bin_glmm_fun()


sims = replicate(1000, bin_glmm_fun(), simplify = FALSE )
sims[[100]]


overdisp_fun = function(model) {
     sum( residuals(model, type = "pearson")^2)/df.residual(model)
}
overdisp_fun(mod)


alldisp = map_dfr(sims, ~data.frame(disp = overdisp_fun(.x) ) )


ggplot(alldisp, aes(x = disp) ) +
     geom_histogram(fill = "blue", 
                    alpha = .25, 
                    bins = 100) +
     geom_vline(xintercept = 1) +
     scale_x_continuous(breaks = seq(0, 2, by = 0.2) ) +
     theme_bw(base_size = 14) +
     labs(x = "Disperson",
          y = "Count")


mean(alldisp$disp > 1)

mean(alldisp$disp > 1.5)
