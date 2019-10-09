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
g1 = ggplot(plottemp) +
     geom_line( aes(datetime, tempf), size = 1 ) +
     scale_x_datetime( date_breaks = "15 min",
                       date_labels = "%H:%M",
                       expand = c(0, 0) ) +
     coord_cartesian(xlim = c(eclipse$start, eclipse$end) ) +
     labs(y = expression( Temperature~(degree*F) ),
          x = NULL,
          title = "Sapsucker Farm temperature during 2017-08-21 solar eclipse",
          subtitle = expression(italic("09:05:10 - 11:37:19 PDT") ),
          caption = "Eclipse: 2 hours 32 minutes 9 seconds\nTotality: 1 minute 57 seconds"
     ) +
     scale_y_continuous(sec.axis = sec_axis(~ (. - 32) * 5 / 9 , 
                                            name =  expression( Temperature~(degree*C)),
                                            breaks = seq(16, 24, by = 1)) ) +
     theme_bw(base_size = 14) +
     theme(panel.grid = element_blank() ) 

g1

color_dat = data.frame(time = seq(eclipse$start, eclipse$end, by = "1 sec") )

color_dat = mutate(color_dat,
                   color = 0,
                   color = replace(color, time < totality$start, seq(100, 0, length.out = sum(time < totality$start) ) ),
                   color = replace(color, time > totality$end, seq(0, 100, length.out = sum(time > totality$end) ) ) )
