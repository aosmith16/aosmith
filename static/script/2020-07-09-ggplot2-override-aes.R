library(ggplot2) # v. 3.3.2


ggplot(data = diamonds, aes(x = carat, y = price, color = cut) ) +
     geom_point(alpha = .25, size = 1)
     

# guides(color = guide_legend(override.aes = list(size = 3) ) )

ggplot(data = diamonds, aes(x = carat, y = price, color = cut) ) +
     geom_point(alpha = .25, size = 1) +
     guides(color = guide_legend(override.aes = list(size = 3) ) )
            

ggplot(data = diamonds, aes(x = carat, y = price, color = cut) ) +
     geom_point(alpha = .25, size = 1) +
     scale_color_viridis_d(option = "magma",
                           guide = guide_legend(override.aes = list(size = 3) ) )


ggplot(data = diamonds, aes(x = carat, y = price, color = cut) ) +
     geom_point(alpha = .25, size = 1) +
     scale_color_viridis_d(option = "magma",
                           guide = guide_legend(override.aes = list(size = 3,
                                                                    alpha = 1) ) )


points = structure(list(x = c(5L, 10L, 7L, 9L, 86L, 46L, 22L, 94L, 21L, 
6L, 24L, 3L), y = c(51L, 54L, 50L, 60L, 97L, 74L, 59L, 68L, 45L, 
56L, 25L, 70L), id = c("a", "a", "a", "a", "b", "b", "b", "b", 
"c", "c", "c", "c")), row.names = c(NA, -12L), class = "data.frame")

head(points)

box = data.frame(left = 1, right = 10, bottom = 50, top = 60, id = "a")
box

ggplot(data = points, aes(color = id) ) +
     geom_point(aes(x = x, y = y), size = 4) +
     geom_rect(data = box, aes(xmin = left,
                               xmax = right,
                               ymin = 50,
                               ymax = top),
               fill = NA, size = 1)


ggplot(data = points, aes(color = id) ) +
     geom_point(aes(x = x, y = y), size = 4) +
     geom_rect(data = box, aes(xmin = left,
                               xmax = right,
                               ymin = 50,
                               ymax = top),
               fill = NA, size = 1) +
     guides(color = guide_legend(override.aes = list(linetype = c(1, 0, 0) ) ) )


ggplot(data = mtcars, aes(x = mpg, y = wt, color = factor(am) ) ) +
     geom_point(size = 3) +
     geom_smooth(method = "lm", se = FALSE)
       

ggplot(data = mtcars, aes(x = mpg, y = wt, color = factor(am) ) ) +
     geom_point(aes(alpha = "Observed"), size = 3) +
     geom_smooth(method = "lm", se = FALSE, aes(alpha = "Fitted") ) +
     scale_alpha_manual(name = NULL,
                        values = c(1, 1),
                        breaks = c("Observed", "Fitted") )
                        


ggplot(data = mtcars, aes(x = mpg, y = wt, color = factor(am) ) ) +
     geom_point(aes(alpha = "Observed"), size = 3) +
     geom_smooth(method = "lm", se = FALSE, aes(alpha = "Fitted") ) +
     scale_alpha_manual(name = NULL,
                        values = c(1, 1),
                        breaks = c("Observed", "Fitted"),
                        guide = guide_legend(override.aes = list(linetype = c(0, 1),
                                                                  shape = c(16, NA),
                                                                  color = "black") ) )


dat = structure(list(g1 = structure(c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 
1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L), class = "factor", .Label = c("High", 
"Low")), g2 = structure(c(1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L, 1L, 
1L, 2L, 2L, 1L, 1L, 2L, 2L), class = "factor", .Label = c("Control", 
"Treatment")), x = c(0.42, 0.39, 0.56, 0.59, 0.17, 0.95, 0.85, 
0.25, 0.31, 0.75, 0.58, 0.9, 0.6, 0.86, 0.61, 0.61), y = c(-1.4, 
3.6, 1.1, -0.1, 0.5, 0, -1.8, 0.8, -1.1, -0.6, 0.2, 0.3, 1.1, 
1.6, 0.9, -0.6)), class = "data.frame", row.names = c(NA, -16L
))

head(dat)


ggplot(data = dat, aes(x = x, y = y, fill = g1, shape = g2) ) +
     geom_point(size = 5) +
     scale_fill_manual(values = c("#002F70", "#EDB4B5") ) +
     scale_shape_manual(values = c(21, 24) )


ggplot(data = dat, aes(x = x, y = y, fill = g1, shape = g2) ) +
     geom_point(size = 5) +
     scale_fill_manual(values = c("#002F70", "#EDB4B5") ) +
     scale_shape_manual(values = c(21, 24) ) +
     guides(fill = guide_legend(override.aes = list(shape = 21) ),
            shape = guide_legend(override.aes = list(fill = "black") ) )

