library(ggplot2) # v. 3.2.0


ggplot(mtcars, aes(mpg, hp) ) +
     geom_point() +
     geom_smooth(method = "lm", se = FALSE, color = "black") +
     geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "red") +
     geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, color = "blue")


ggplot(mtcars, aes(mpg, hp) ) +
     geom_point() +
     geom_smooth(method = "lm", se = FALSE, color = "black") +
     geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "red") +
     geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, color = "blue") +
     scale_color_manual(values = c("black", "red", "blue") )


ggplot(mtcars, aes(mpg, hp) ) +
     geom_point() +
     geom_smooth(method = "lm", se = FALSE, aes(color = "black") ) +
     geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, aes(color = "red") ) +
     geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, aes(color = "blue") )


ggplot(mtcars, aes(mpg, hp) ) +
     geom_point() +
     geom_smooth(method = "lm", se = FALSE, aes(color = "black") ) +
     geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, aes(color = "red") ) +
     geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, aes(color = "blue") ) +
     scale_color_identity(guide = "legend")


ggplot(mtcars, aes(mpg, hp) ) +
     geom_point() +
     geom_smooth(method = "lm", se = FALSE, aes(color = "black") ) +
     geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, aes(color = "red") ) +
     geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, aes(color = "blue") ) +
     scale_color_identity(name = "Model fit",
                          breaks = c("black", "red", "blue"),
                          labels = c("Linear", "Quadratic", "Cubic"),
                          guide = "legend")


ggplot(mtcars, aes(mpg, hp) ) +
     geom_point() +
     geom_smooth(method = "lm", se = FALSE, aes(color = "Linear") ) +
     geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, aes(color = "Quadratic") ) +
     geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, aes(color = "Cubic") )


ggplot(mtcars, aes(mpg, hp) ) +
     geom_point() +
     geom_smooth(method = "lm", se = FALSE, aes(color = "Linear") ) +
     geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, aes(color = "Quadratic") ) +
     geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, aes(color = "Cubic") ) +
     scale_color_manual(name = "Model fit",
                        breaks = c("Linear", "Quadratic", "Cubic"),
                        values = c("Cubic" = "blue", "Quadratic" = "red", "Linear" = "black") )

