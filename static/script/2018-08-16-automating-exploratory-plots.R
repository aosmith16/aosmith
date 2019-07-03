library(ggplot2) # v. 3.2.0
library(purrr) # v. 0.3.2


set.seed(16)
dat = data.frame(elev = round( runif(20, 100, 500), 1),
                 resp = round( runif(20, 0, 10), 1),
                 grad = round( runif(20, 0, 1), 2),
                 slp = round( runif(20, 0, 35),1),
                 lat = runif(20, 44.5, 45),
                 long = runif(20, 122.5, 123.1),
                 nt = rpois(20, lambda = 25) )
head(dat)


response = names(dat)[1:3]
expl = names(dat)[4:7]


response = set_names(response)
response


expl = set_names(expl)
expl


scatter_fun = function(x, y) {
     ggplot(dat, aes(x = .data[[x]], y = .data[[y]]) ) +
          geom_point() +
          geom_smooth(method = "loess", se = FALSE, color = "grey74") +
          theme_bw() +
          labs(x = x,
               y = y)
}


# scatter_fun = function(x, y) {
#      ggplot(dat, aes_string(x = x, y = y ) ) +
#           geom_point() +
#           geom_smooth(method = "loess", se = FALSE, color = "grey74") +
#           theme_bw()
# }
# 

scatter_fun("lat", "elev")


elev_plots = map(expl, ~scatter_fun(.x, "elev") )

elev_plots


all_plots = map(response,
                ~map(expl, scatter_fun, y = .x) )


all_plots$grad[1:2]

all_plots$grad$long

all_plots[[3]][[3]]


# pdf("all_scatterplots.pdf")
# all_plots
# dev.off()
# 

# iwalk(all_plots, ~{
#      pdf(paste0(.y, "_scatterplots.pdf") )
#      print(.x)
#      dev.off()
# })
# 

plotnames = imap(all_plots, ~paste0(.y, "_", names(.x), ".png")) %>%
     flatten()
plotnames


# walk2(plotnames, flatten(all_plots), ~ggsave(filename = .x, plot = .y,
#                                              height = 7, width = 7))
# 

cowplot::plot_grid(plotlist = all_plots[[1]])


response_plots = map(all_plots, ~cowplot::plot_grid(plotlist = .x))
response_plots

