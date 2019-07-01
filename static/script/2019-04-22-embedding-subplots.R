library(ggplot2) # 3.1.1
suppressPackageStartupMessages( library(dplyr) ) # 0.8.0.1
library(tidyr) # 0.8.3
library(purrr) # 0.3.2


cuteven = function(variable, ngroups) {
     seq_all = seq(min(variable), max(variable), length.out = ngroups + 1)
     cut(variable,
         breaks = seq_all,
         labels = paste(seq_all[-(ngroups + 1)], seq_all[-1], sep = ","),
         include.lowest = TRUE)
}


with(iris, cuteven(Sepal.Length, ngroups = 3) )


iris = mutate(iris,
                group_x = cuteven(Sepal.Length, 3),
                group_y = cuteven(Petal.Length, 4) )

glimpse(iris)


iris = iris %>%
     separate(group_x, into = c("min_x", "max_x"), 
              sep = ",", convert = TRUE) %>%
     separate(group_y, into = c("min_y", "max_y"), 
              sep = ",", convert = TRUE)

glimpse(iris)


iris %>%
     group_by(min_x, max_x, min_y, max_y, Species) %>%
     count() %>%
     ungroup() %>%
     filter(n == max(n) )


ggplot(data = filter(iris, max_x <= 5.5, max_y <= 2.475), 
       aes(x = Species, fill = Species) ) +
     geom_bar() +
     theme_void() +
     scale_x_discrete(limits = c("setosa", "versicolor", "virginica") ) +
     scale_fill_manual(values = c("setosa" = "#ED90A4", 
                                  "versicolor" = "#ABB150",
                                  "virginica" = "#00C1B2"),
                       guide  = "none") +
     theme(panel.border = element_rect(color = "grey",
                                       fill = "transparent") ) +
     ylim(0, 47)


barfun = function(data) {
     ggplot(data = data, 
            aes(x = Species, fill = Species) ) +
          geom_bar() +
          theme_void() +
          scale_x_discrete(limits = c("setosa", "versicolor", "virginica") ) +
          scale_fill_manual(values = c("setosa" = "#ED90A4", 
                                       "versicolor" = "#ABB150",
                                       "virginica" = "#00C1B2"),
                            guide  = "none") +
          theme(panel.border = element_rect(color = "grey",
                                            fill = "transparent") ) +
          ylim(0, 47) 
  
}

barfun(data = filter(iris, max_x <= 5.5, max_y <= 2.475) )


allplots = iris %>%
     group_by_at( vars( matches("min|max") ) ) %>%
     group_nest() %>%
     mutate(subplots = map(data, barfun) )

allplots

allplots$subplots[[1]]
allplots$subplots[[6]]

grobfun = function(min_x, max_x, min_y, max_y, subplots) {
     annotation_custom(ggplotGrob(subplots),
                       xmin = min_x, ymin = min_y,
                       xmax = max_x, ymax = max_y)
}


( allgrobs = allplots %>%
     select(-data) %>%
     mutate(grobs = pmap(., grobfun) ) )


( largeplot = ggplot(iris, aes(x = Sepal.Length, 
                               y = Petal.Length, 
                               fill = Species) ) +
       geom_blank() +
       geom_col( aes(Inf, Inf) ) +
       scale_fill_manual(values = c("setosa" = "#ED90A4", 
                                    "versicolor" = "#ABB150",
                                    "virginica" = "#00C1B2") ) )


largeplot +
     allgrobs$grobs +
     ylim(1, NA)


range(iris$Petal.Width)


ggplot(data = filter(iris, max_x <= 5.5, max_y <= 2.475),
       aes(x = Petal.Width, y = stat(ncount), fill = stat(x) ) ) +
     geom_histogram(binwidth = .2, center = .1) +
     theme_void(base_size = 14) +
     scale_x_continuous(limits = c(0.1 - .1, 2.5 + .1) ) +
     scale_fill_continuous(type = "viridis",
                           guide  = "none",
                           limits = c(.1, 2.5) ) +
     facet_wrap(~paste0("n = ", nrow(filter(iris, max_x <= 5.5, max_y <= 2.475) ) ) ) +
     theme(panel.border = element_rect(color = "grey",
                                       fill = "transparent") )


histfun = function(data) {
     ggplot(data = data,
            aes(x = Petal.Width, y = stat(ncount), fill = stat(x) ) ) +
          geom_histogram(binwidth = .2, center = .1) +
          theme_void(base_size = 14) +
          scale_x_continuous(limits = c(0.1 - .1, 2.5 + .1) ) +
          scale_fill_continuous(type = "viridis",
                                guide  = "none",
                                limits = c(.1, 2.5) ) +
          facet_wrap(~paste0("n = ", nrow(data) ) ) +
          theme(panel.border = element_rect(color = "grey",
                                            fill = "transparent") )
}


allgrobs_hist = iris %>%
     group_by_at( vars( matches("min|max") ) ) %>%
     group_nest() %>%
     mutate(subplots = map(data, histfun) ) %>%
     select(-data) %>%
     mutate(grobs = pmap(., grobfun) )


( largeplot2 = ggplot(iris, aes(x = Sepal.Length, 
                                y = Petal.Length, 
                                fill = Petal.Width) ) +
       geom_blank() +
       geom_col( aes(Inf, Inf) ) +
       scale_fill_continuous(type = "viridis",
                             limits = c(.1, 2.5),
                             breaks = seq(.1, 2.5, by = .8) ) )


largeplot2 +
     allgrobs_hist$grobs +
     ylim(1, NA)


ggplot(data = filter(iris, max_x <= 5.5, max_y <= 2.475),
       aes(x = Petal.Width, y = stat(ndensity), color = stat(x) ) ) +
     stat_density(geom = "line", size = 1.25) +
     theme_void(base_size = 14) +
     scale_x_continuous(limits = c(0.1, 2.5),
                        expand = c(0, 0) ) +
     scale_color_viridis_c(guide  = "none",
                           limits = c(.1, 2.5) ) +
     facet_wrap(~paste0("n = ", nrow(filter(iris, max_x <= 5.5, max_y <= 2.475) ) ) ) +
     theme(panel.border = element_rect(color = "grey",
                                       fill = "transparent") )


densfun = function(data) {
     ggplot(data = data,
            aes(x = Petal.Width, y = stat(ndensity), color = stat(x) ) ) +
          stat_density(geom = "line", size = 1.25) +
          theme_void(base_size = 14) +
          scale_x_continuous(limits = c(0.1, 2.5),
                             expand = c(0, 0) ) +
          scale_color_viridis_c(guide  = "none",
                                limits = c(.1, 2.5) ) +
          facet_wrap(~paste0("n = ", nrow(data) ) ) +
          theme(panel.border = element_rect(color = "grey",
                                            fill = "transparent") )
}


allgrobs_dens = iris %>%
     group_by_at( vars( matches("min|max") ) ) %>%
     group_nest() %>%
     mutate(subplots = map(data, densfun) ) %>%
     select(-data) %>%
     mutate(grobs = pmap(., grobfun) )

largeplot2 +
     allgrobs_dens$grobs +
     ylim(1, NA)


library(ggridges) # v 0.5.1
ggplot(data = filter(iris, max_x <= 5.5, max_y <= 2.475),
       aes(x = Petal.Width, y = 1, fill = stat(x) ) ) +
     geom_density_ridges_gradient() +
     theme_void(base_size = 14) +
     scale_x_continuous(limits = c(0.1, 2.5),
                        expand = c(0, 0) ) +
     scale_fill_viridis_c(guide  = "none",
                           limits = c(.1, 2.5) ) +
     facet_wrap(~paste0("n = ", nrow(filter(iris, max_x <= 5.5, max_y <= 2.475) ) ) ) +
     theme(panel.border = element_rect(color = "grey",
                                       fill = "transparent") )


densfun2 = function(data) {
  ggplot(data = data,
         aes(x = Petal.Width, y = 1, fill = stat(x) ) ) +
    geom_density_ridges_gradient() +
    theme_void(base_size = 14) +
    scale_x_continuous(limits = c(0.1, 2.5),
                       expand = c(0, 0) ) +
    scale_fill_viridis_c(guide  = "none",
                         limits = c(.1, 2.5) ) +
    facet_wrap(~paste0("n = ", nrow(data) ) ) +
    theme(panel.border = element_rect(color = "grey",
                                      fill = "transparent") )
}
allgrobs_dens2 = iris %>%
    group_by_at( vars( matches("min|max") ) ) %>%
    group_nest() %>%
    mutate(subplots = map(data, densfun2) ) %>%
    select(-data) %>%
    mutate(grobs = pmap(., grobfun) )

largeplot2 +
    allgrobs_dens2$grobs +
    ylim(1, NA) +
    theme_bw() +
    theme(legend.direction = "horizontal",
          legend.position = c(.8, .25),
          legend.background = element_blank() ) +
    guides(fill = guide_colorbar(title.position = "top") )
