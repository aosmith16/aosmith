res = structure(list(Diffmeans = c(-0.27, 0.11, -0.15, -1.27, -1.18
), Lower.CI = c(-0.63, -0.25, -0.51, -1.63, -1.54), Upper.CI = c(0.09, 
0.47, 0.21, -0.91, -0.82), plantdate = structure(c(1L, 2L, 2L, 
3L, 3L), .Label = c("January 2", "January 28", "February 25"), class = "factor"), 
    stocktype = structure(c(2L, 2L, 1L, 2L, 1L), .Label = c("bare", 
    "cont"), class = "factor")), .Names = c("Diffmeans", "Lower.CI", 
"Upper.CI", "plantdate", "stocktype"), row.names = c(NA, -5L), class = "data.frame")


library(ggplot2)

( g1 = ggplot(res, aes(x = plantdate, y = Diffmeans, group = stocktype) ) +
 	geom_point(position = position_dodge(width = .75) ) +
 	geom_errorbar( aes(ymin = Lower.CI, ymax = Upper.CI,
 	                   linetype = stocktype,
 	                   width = c(.2, .4, .4, .4, .4) ),
 	               position = position_dodge(width = .75) ) +
      theme_bw() + 
      labs(y = "Difference in Growth (cm)",
           x = "Planting Date") +
      geom_rect(xmin = -Inf, xmax = Inf, ymin = -.25, ymax = .25, 
			fill = "grey54", alpha = .05) +
      scale_y_continuous(breaks = seq(-1.5, .5, by = .25) ) + 
      coord_flip() +
      scale_linetype_manual(values = c("solid", "twodash"),
                            name = element_blank(),
                            labels = c("Bare root", "Container") ) )


res$stocktype = factor(res$stocktype, levels = c("cont", "bare") )


g1 %+% 
     res +
     scale_linetype_manual(values = c("solid", "twodash"),
                            name = element_blank(),
                            labels = c("Container", "Bare root") )




g1 %+% 
     res +
     scale_linetype_manual(values = c("solid", "twodash"),
                           name = element_blank(),
                           labels = c("Container", "Bare root"),
                           guide = guide_legend(reverse = TRUE) )

