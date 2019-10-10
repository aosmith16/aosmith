library(ggplot2) # 3.2.1
library(dplyr) # 0.8.3


xfun::embed_file(path = here::here("static", "data", "2017-08-21_sapsucker_morning_temp.csv"), 
                  name = "eclipse_temp.csv")

head(temp)

eclipse = data.frame(start = as.POSIXct("2017-08-21 09:05:10"),
                     end = as.POSIXct("2017-08-21 11:37:19") )

totality = data.frame(start = as.POSIXct("2017-08-21 10:16:55"),
                      end = as.POSIXct("2017-08-21 10:18:52") )


plottemp = filter(temp, between(datetime, as.POSIXct("2017-08-21 09:00:00"),
                                as.POSIXct("2017-08-21 12:00:00") ) )

ggplot(plottemp) +
     geom_line( aes(datetime, tempf), size = 1 ) +
     scale_x_datetime( date_breaks = "15 min",
                       date_labels = "%H:%M",
                       expand = c(0, 0) ) +
     coord_cartesian(xlim = c(eclipse$start, eclipse$end) ) +
     labs(y = expression( Temperature~(degree*F) ),
          x = NULL,
          title = "Temperature during 2017-08-21 solar eclipse",
          subtitle = expression(italic("Sapsucker Farm, 09:05:10 - 11:37:19 PDT") ),
          caption = "Eclipse: 2 hours 32 minutes 9 seconds\nTotality: 1 minute 57 seconds"
     ) +
     scale_y_continuous(sec.axis = sec_axis(~ (. - 32) * 5 / 9 , 
                                            name =  expression( Temperature~(degree*C)),
                                            breaks = seq(16, 24, by = 1)) ) +
     theme_bw(base_size = 14) +
     theme(panel.grid = element_blank() ) 

color_dat = data.frame(time = seq(eclipse$start, eclipse$end, by = "1 sec") )

color_dat = mutate(color_dat,
                   color = 0,
                   color = replace(color, 
                                   time < totality$start, 
                                   seq(100, 0, length.out = sum(time < totality$start) ) ),
                   color = replace(color, 
                                   time > totality$end, 
                                   seq(0, 100, length.out = sum(time > totality$end) ) ) )

g1 = ggplot(plottemp) +
     geom_segment(data = color_dat,
                  aes(x = time, xend = time,
                      y = -Inf, yend = Inf, color = color),
                  show.legend = FALSE) +
     geom_line( aes(datetime, tempf), size = 1 ) +
     scale_x_datetime( date_breaks = "15 min",
                       date_labels = "%H:%M",
                       expand = c(0, 0) ) +
     coord_cartesian(xlim = c(eclipse$start, eclipse$end) ) +
     labs(y = expression( Temperature~(degree*F) ),
          x = NULL,
          title = "Temperature during 2017-08-21 solar eclipse",
          subtitle = expression(italic("Sapsucker Farm, 09:05:10 - 11:37:19 PDT") ),
          caption = "Eclipse: 2 hours 32 minutes 9 seconds\nTotality: 1 minute 57 seconds"
     ) +
     scale_y_continuous(sec.axis = sec_axis(~ (. - 32) * 5 / 9 , 
                                            name =  expression( Temperature~(degree*C)),
                                            breaks = seq(16, 24, by = 1)) ) +
     theme_bw(base_size = 14) +
     theme(panel.grid = element_blank() ) 

g1


g1 + scale_color_gradient(low = gray.colors(1, .25),
                          high = gray.colors(1, 1) )

g2 = ggplot(temp) +
     geom_segment(data = color_dat,
                  aes(x = time, xend = time,
                      y = -Inf, yend = Inf, color = color),
                  show.legend = FALSE) +
     geom_line( aes(datetime, tempf), size = 1 ) +
     scale_x_datetime( date_breaks = "1 hour",
                       date_labels = "%H:%M",
                       expand = c(0, 0) ) +
     labs(y = expression( Temperature~(degree*F) ),
          x = NULL,
          title = "Temperature during 2017-08-21 solar eclipse",
          subtitle = expression(italic("Sapsucker Farm, Dallas, OR, USA") ),
          caption = "Eclipse: 2 hours 32 minutes 9 seconds\nTotality: 1 minute 57 seconds"
     ) +
     scale_y_continuous(sec.axis = sec_axis(~ (. - 32) * 5 / 9 , 
                                            name =  expression( Temperature~(degree*C)),
                                            breaks = seq(12, 24, by = 2)) ) +
     scale_color_gradient(low = gray.colors(1, .25),
                          high = gray.colors(1, 1) ) +
     theme_bw(base_size = 14) +
     theme(panel.grid.major.x = element_blank(),
           panel.grid.minor = element_blank() ) 

g2

g2 = g2 + 
     annotate("text", x = as.POSIXct("2017-08-21 08:15"),
                   y = 74.5, label = "Partial eclipse begins",
              color = "grey24") +
     annotate("text", x = as.POSIXct("2017-08-21 09:00"),
              y = 57, label = "Totality",
              color = "grey24")
g2


arrows = data.frame(x1 = as.POSIXct( c("2017-08-21 08:16",
                                      "2017-08-21 09:16") ),
                    x2 = c(eclipse$start, totality$start),
                    y1 = c(73.75, 57),
                    y2 = c(72, 61) )

g2 +
     geom_curve(data = arrows,
                aes(x = x1, xend = x2,
                    y = y1, yend = y2),
                arrow = arrow(length = unit(0.075, "inches"),
                              type = "closed"),
                curvature = 0.2)
