library(ggplot2) # v. 3.1.1
library(tidyr) # v. 0.8.3
library(egg) # v. 0.4.2


set.seed(16)
dat = data.frame(elev = round( runif(20, 100, 500), 1),
                 resp = round( runif(20, 0, 10), 1),
                 grad = round( runif(20, 0, 1), 2),
                 slp = round( runif(20, 0, 35),1) )
head(dat)


datlong = gather(dat, key = "variable", value = "value", -resp)
head(datlong)


ggplot(datlong, aes(x = value, y = resp) ) +
     geom_point() +
     theme_bw() +
     facet_wrap(~variable, scales = "free_x")


ggplot(datlong, aes(x = value, y = resp) ) +
     geom_point() +
     theme_bw() +
     facet_wrap(~variable, scales = "free_x", strip.position = "bottom") +
     theme(strip.background = element_blank(),
           strip.placement = "outside") +
     labs(x = NULL)


elevplot = ggplot(dat, aes(x = elev, y = resp) ) +
     geom_point() +
     theme_bw()

gradplot = ggplot(dat, aes(x = grad, y = resp) ) +
     geom_point() +
     theme_bw() +
     scale_x_continuous(breaks = seq(0, 1, by = 0.2) )

slpplot = ggplot(dat, aes(x = slp, y = resp) ) +
     geom_point() +
     theme_bw() +
     scale_x_continuous(breaks = seq(0, 35, by = 5) )


cowplot::plot_grid(elevplot, 
                   gradplot, 
                   slpplot,
                   nrow = 1,
                   labels = "auto")


cowplot::plot_grid(elevplot, 
                   gradplot + theme(axis.text.y = element_blank(),
                                    axis.ticks.y = element_blank(),
                                    axis.title.y = element_blank() ), 
                   slpplot + theme(axis.text.y = element_blank(),
                                    axis.ticks.y = element_blank(),
                                    axis.title.y = element_blank() ),
                   nrow = 1,
                   labels = "auto")


cowplot::plot_grid(elevplot, 
                   gradplot + 
                        theme(axis.text.y = element_blank(),
                              axis.ticks.y = element_blank(),
                              axis.title.y = element_blank() ), 
                   slpplot + 
                        theme(axis.text.y = element_blank(),
                              axis.ticks.y = element_blank(),
                              axis.title.y = element_blank() ),
                   nrow = 1,
                   labels = "auto",
                   align = "v")


elevplot = ggplot(dat, aes(x = elev, y = resp) ) +
     geom_point() +
     theme_bw()

gradplot = ggplot(dat, aes(x = grad, y = resp) ) +
     geom_point() +
     theme_bw() +
     scale_x_continuous(breaks = seq(0, 1, by = 0.2) )

slpplot = ggplot(dat, aes(x = slp, y = resp) ) +
     geom_point() +
     theme_bw() +
     scale_x_continuous(breaks = seq(0, 35, by = 5) )


ggarrange(elevplot, 
          gradplot + 
               theme(axis.text.y = element_blank(),
                     axis.ticks.y = element_blank(),
                     axis.title.y = element_blank() ), 
          slpplot + 
               theme(axis.text.y = element_blank(),
                     axis.ticks.y = element_blank(),
                     axis.title.y = element_blank() ),
          nrow = 1)


ggarrange(elevplot +
               theme(axis.ticks.y = element_blank(),
                     plot.margin = margin(r = 1) ), 
          gradplot + 
               theme(axis.text.y = element_blank(),
                     axis.ticks.y = element_blank(),
                     axis.title.y = element_blank(),
                     plot.margin = margin(r = 1, l = 1) ), 
          slpplot + 
               theme(axis.text.y = element_blank(),
                     axis.ticks.y = element_blank(),
                     axis.title.y = element_blank(),
                     plot.margin = margin(l = 1)  ),
          nrow = 1)


ggarrange(tag_facet(elevplot +
                          theme(axis.ticks.y = element_blank(),
                                plot.margin = margin(r = 1) ) +
                          facet_wrap(~"elev"),
                     tag_pool = "a"), 
          tag_facet(gradplot + 
                          theme(axis.text.y = element_blank(),
                                axis.ticks.y = element_blank(),
                                axis.title.y = element_blank(),
                                plot.margin = margin(r = 1, l = 1) ) +
                          facet_wrap(~"grad"), 
                    tag_pool = "b" ), 
          tag_facet(slpplot + 
                          theme(axis.text.y = element_blank(),
                                axis.ticks.y = element_blank(),
                                axis.title.y = element_blank(),
                                plot.margin = margin(l = 1)  ) +
                          facet_wrap(~"slp"),
                     tag_pool = "c"),
          nrow = 1)
