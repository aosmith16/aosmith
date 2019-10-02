library(ggplot2) # v. 3.2.1
library(purrr) # v. 0.3.2

dat = structure(list(cov_plant = c(3.7, 1.8, 7.5, 0.4, 7.9, 1.2, 0.7, 
2.3, 6.9, 4.1, 17.7, 2.4, 0.9, 14.3, 4.9, 0, 4.1, 3.6, 1.1, 7.7, 
0, 1.5, 1.7, 11.5, 0.8, 12.3, 7.1, 6.9, 5.6, 2.7, 1, 2.5, 2, 
0.7, 0.7, 2.9, 4, 2.5, 2.9, 1.5, 0.5, 22.8, 2.8, 1.4, 1, 2.9, 
2.4, 4.1, 4.1, 1.9, 2.8, 5, 5.7, 5.6, 0, 4.6, 8.1, 0.5, 88.9, 
1), cov_oth = c(11.5, 63.2, 34, 65.5, 28.8, 8.6, 7.1, 65.5, 12.1, 
3, 23.6, 3.8, 24.9, 55.9, 24.2, 78.2, 81.1, 10.7, 30.7, 23.5, 
10.1, 4.6, 45.7, 37.6, 81.3, 39.1, 50.8, 75.8, 78.2, 23.9, 53, 
51.1, 2.5, 40.2, 15.9, 91.3, 44, 72.9, 82.7, 42.4, 94.1, 23, 
86.2, 50.1, 88.9, 80.5, 34.2, 68.7, 45, 13.9, 44.2, 85, 79.6, 
1, 45.3, 69.5, 89.6, 22.2, 1.3, 88), gap = c(2.8, 11.8, 0.3, 
17.2, 18.3, 1.4, 19.6, 19.4, 2.6, 66.3, 97.1, 17, 381.5, 15.7, 
8.3, 2.4, 3.8, 3.8, 246.6, 43.2, 16.7, 6.6, 3.1, 2.4, 3.2, 4.3, 
0.3, 2.1, 41.7, 68.9, 5.1, 5.7, 0.4, 35.5, 1.1, 10.8, 5, 11.8, 
75.5, 5.4, 12.6, 5.2, 11.4, 6.8, 5.3, 1.1, 3.2, 2.9, 5.2, 0.2, 
1.5, 0.6, 7.4, 18.6, 11.7, 1.6, 13.7, 7.1, 19.9, 16.8), year = structure(c(1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 
3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L), .Label = c("Year 1", 
"Year 2", "Year 3"), class = "factor"), trt = structure(c(1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 
2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L), .Label = c("a", 
"b"), class = "factor")), row.names = c(NA, -60L), class = "data.frame")

head(dat)


resp_dat = structure(list(variable = structure(c(2L, 1L, 3L), .Label = c("cov_oth", 
"cov_plant", "gap"), class = "factor"), description = structure(3:1, .Label = c("Gap size", 
"Other cover", "Plant cover"), class = "factor"), units = structure(c(1L, 
1L, 2L), .Label = c("%", "m"), class = "factor"), transformation = structure(c(2L, 
1L, 2L), .Label = c("identity", "log"), class = "factor"), constant = c(0.3, 
0, 0)), class = "data.frame", row.names = c(NA, -3L))

resp_dat


plot_fun = function(data = dat, respdata = resp_dat, response) {
     
     respvar = subset(respdata, variable == response)

     ggplot(data = data, aes(x = year, 
                             y = .data[[response]] + respvar$constant,
                             shape = trt, 
                             color = trt,
                             group = trt) ) +
          geom_point(position = position_dodge(width = 0.5),
                   alpha = 0.25,
                   size = 2, show.legend = FALSE) +
          stat_summary(fun.y = mean, geom = "point",
                       position = position_dodge(width = 0.5),
                       size = 4, show.legend = FALSE) +
          stat_summary(fun.y = mean, geom = "line",
                       position = position_dodge(width = 0.5),
                       size = 1, key_glyph = "rect") +
          theme_bw(base_size = 14) +
          theme(legend.position = "bottom",
                legend.direction = "horizontal",
                legend.box.spacing = unit(0, "cm"),
                legend.text = element_text(margin = margin(l = -.2, unit = "cm") ),
                panel.grid.minor.y = element_blank() ) +
          scale_color_grey(name = "",
                           label = c("A Treatment", "B Treatment"),
                           start = 0, end = 0.5) +
          labs(x = "Year since treatment",
               title = paste0(respvar$descrip, " (", respvar$units, ")"),
               y = NULL)
}

plot_fun(response = "cov_plant")


# if( grepl("log", respvar$transformation) ) {
#           g1 + scale_y_log10()
#      } else {
#           g1
#      }
# 

plot_fun2 = function(data = dat, respdata = resp_dat, response) {
     
     respvar = subset(respdata, variable == response)

     g1 = ggplot(data = data, aes(x = year, 
                                  y = .data[[response]] + respvar$constant,
                                  shape = trt, 
                                  color = trt,
                                  group = trt) ) +
          geom_point(position = position_dodge(width = 0.5),
                     alpha = 0.25,
                     size = 2, show.legend = FALSE) +
          stat_summary(fun.y = mean, geom = "point",
                       position = position_dodge(width = 0.5),
                       size = 4, show.legend = FALSE) +
          stat_summary(fun.y = mean, geom = "line",
                       position = position_dodge(width = 0.5),
                       size = 1, key_glyph = "rect") +
          theme_bw(base_size = 14) +
          theme(legend.position = "bottom",
                legend.direction = "horizontal",
                legend.box.spacing = unit(0, "cm"),
                legend.text = element_text(margin = margin(l = -.2, unit = "cm") ),
                panel.grid.minor.y = element_blank() ) +
          scale_color_grey(name = "",
                           label = c("A Treatment", "B Treatment"),
                           start = 0, end = 0.5) +
          labs(x = "Year since treatment",
               title = paste0(respvar$descrip, " (", respvar$units, ")"),
               y = NULL)
     
     if( grepl("log", respvar$transformation) ) {
          g1 + scale_y_log10()
     } else {
          g1
     }
}

plot_fun2(response = "cov_plant")

plot_fun2(response = "cov_oth")


# caption_text = {
#      if(respvar$constant != 0 ) {
#                paste0("Y axis on log scale ",
#                       "(added constant ",
#                       respvar$constant, ")")
#           } else if(!grepl("log", respvar$transformation) ) {
#                "Y axis on original scale"
#           } else {
#                "Y axis on log scale"
#           }
# }
# 

plot_fun3 = function(data = dat, respdata = resp_dat, response) {
     
     respvar = subset(respdata, variable == response)

     caption_text = {
          if(respvar$constant != 0 ) {
               paste0("Y axis on log scale ", 
                      "(added constant ", 
                      respvar$constant, ")")
        } else if(!grepl("log", respvar$transformation) ) {
                "Y axis on original scale"
        } else {
                "Y axis on log scale"
        }
     }
     
     g1 = ggplot(data = data, aes(x = year, 
                                  y = .data[[response]] + respvar$constant,
                                  shape = trt, 
                                  color = trt,
                                  group = trt) ) +
          geom_point(position = position_dodge(width = 0.5),
                     alpha = 0.25,
                     size = 2, show.legend = FALSE) +
          stat_summary(fun.y = mean, geom = "point",
                       position = position_dodge(width = 0.5),
                       size = 4, show.legend = FALSE) +
          stat_summary(fun.y = mean, geom = "line",
                       position = position_dodge(width = 0.5),
                       size = 1, key_glyph = "rect") +
          theme_bw(base_size = 14) +
          theme(legend.position = "bottom",
                legend.direction = "horizontal",
                legend.box.spacing = unit(0, "cm"),
                legend.text = element_text(margin = margin(l = -.2, unit = "cm") ),
                panel.grid.minor.y = element_blank() ) +
          scale_color_grey(name = "",
                           label = c("A Treatment", "B Treatment"),
                           start = 0, end = 0.5) +
          labs(x = "Year since treatment",
               title = paste0(respvar$descrip, " (", respvar$units, ")"),
               y = NULL,
               caption = caption_text)
     
     if( grepl("log", respvar$transformation) ) {
          g1 +
               scale_y_log10()
     } else {
          g1
     }
}

plot_fun3(response = "cov_plant")


vars = names(dat)[1:3]
vars


all_plots = map(vars, ~plot_fun3(response = .x) )


cowplot::plot_grid(plotlist = all_plots)

