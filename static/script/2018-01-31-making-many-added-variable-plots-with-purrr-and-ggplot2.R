library(dplyr) # v. 1.0.7
library(ggplot2) # v. 3.3.5
library(purrr) # v. 0.3.4
library(broom) # v. 0.7.9


fit1 = lm( log(mpg) ~ disp + hp + drat + wt, data = mtcars)
summary(fit1)


( mod_vars = all.vars( formula(fit1) )[-1] )


preddat_fun = function(data, allvars, var) {
     sums = summarise_at(data, 
                         vars( one_of(allvars), -one_of(var) ), 
                         median) 
     cbind( select_at(data, var), sums)
}


head( preddat_fun(mtcars, mod_vars, "disp") )


pred_dats = mod_vars %>%
     set_names() %>%
     map( ~preddat_fun(mtcars, mod_vars, .x) )
str(pred_dats)


head( augment(fit1, newdata = pred_dats[[1]], se_fit = TRUE))


preds = pred_dats %>%
     map(~augment(fit1, newdata = .x, se_fit = TRUE) ) %>%
     map(~mutate(.x, 
                 lower = exp(.fitted - 2*.se.fit),
                 upper = exp(.fitted + 2*.se.fit),
                 pred = exp(.fitted) ) )


str(preds$disp)

ggplot(data = preds$disp, aes(x = disp, y = pred) ) +
     geom_line(size = 1) +
     geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .25) +
     geom_rug(sides = "b") +
     theme_bw(base_size = 14) +
     labs(x = "Displacement (cu.in.)",
          y = "Miles/(US) gallon") +
     ylim(10, 32)


xlabs = c("Displacement (cu.in.)", 
          "Gross horsepower",
          "Rear axle ratio", 
          "Weight (1000 lbs)")


pred_plot = function(data, variable, xlab) {
     ggplot(data, aes(x = .data[[variable]], y = pred) ) +
          geom_line(size = 1) +
          geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .25) +
          geom_rug(sides = "b") +
          theme_bw(base_size = 14) +
          labs(x = xlab,
               y = "Miles/(US) gallon") +
          ylim(10, 32)
}


pred_plot(preds[[1]], mod_vars[1], xlabs[1])


all_plots = pmap( list(preds, mod_vars, xlabs), pred_plot)
all_plots


cowplot::plot_grid(plotlist = all_plots,
          labels = "AUTO",
          align = "hv")
