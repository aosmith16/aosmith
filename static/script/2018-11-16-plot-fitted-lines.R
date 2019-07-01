library(nlme) # v. 3.1-137
library(ggplot2) # v. 3.1.0


theme_set(theme_bw())


dat = structure(list(block = structure(c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 
2L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 5L, 5L, 5L, 5L, 6L, 6L, 6L, 
6L, 7L, 7L, 7L, 7L, 8L, 8L, 8L, 8L, 9L, 9L, 9L, 9L, 10L, 10L, 
10L, 10L), .Label = c("A", "B", "C", "D", "E", "F", "G", "H", 
"I", "J"), class = "factor"), grp = structure(c(1L, 2L, 3L, 
4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 
4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 
4L, 1L, 2L, 3L, 4L), .Label = c("a", "b", "c", "d"), class = "factor"), 
    x1 = c(11.1, 7.9, 6.6, 7.1, 10.6, 8, 9.8, 8.2, 9.5, 5.4, 
    15, 15.3, 10.4, 5.5, 9.1, 15.3, 12.9, 9.9, 5, 7.4, 10.4, 
    8.2, 14.1, 4.7, 11.9, 12.5, 8.7, 7, 5.5, 5.7, 13.7, 11.8, 
    7, 14.8, 4.9, 14.3, 7.8, 15.4, 15.2, 12.2), x2 = c(109.9, 
    149.2, 187.4, 124.1, 190.7, 145, 110.1, 114.1, 119.9, 163.8, 
    192.7, 158.3, 180.5, 127.7, 133.1, 137.5, 167.8, 181.8, 156.4, 
    109.7, 143.9, 194.2, 139.1, 112.4, 194, 125.7, 127, 149.1, 
    117.8, 170.4, 167.3, 101.1, 128, 157.8, 139.7, 193.6, 121.1, 
    161.1, 112, 137.3), resp = c(86.5, 63.1, 10.5, 44.4, 61.9, 
    67.7, 64.1, 59.4, 66.1, 33.2, 91.6, 116.4, 59.4, 38.6, 44.6, 
    122.9, 87.1, 75.1, -0.8, 49.1, 70.2, 57.8, 96.4, 22.5, 74.7, 
    116.7, 46, 39.8, 28.3, 34.1, 87, 97.1, 37.3, 126.8, 2.2, 
    96.1, 45.3, 131.9, 107.6, 92.7)), class = "data.frame", row.names = c(NA, 
-40L))

head(dat)


ggplot(dat, aes(x = x1, y = resp, color = grp) ) +
     geom_point() +
     geom_smooth(method = "lm", se = FALSE)


ggplot(dat, aes(x = x1, y = resp, color = grp) ) +
     geom_point() +
     geom_smooth(method = "lm", alpha = .15, aes(fill = grp))


methods("predict")


fitlm = lm(resp ~ grp + x1, data = dat)

dat$predlm = predict(fitlm)


ggplot(dat, aes(x = x1, y = resp, color = grp) ) +
     geom_point() +
     geom_line(aes(y = predlm), size = 1)


predslm = predict(fitlm, interval = "confidence")
head(predslm)


datlm = cbind(dat, predslm)
head(datlm)


ggplot(datlm, aes(x = x1, y = resp, color = grp) ) +
     geom_point() +
     geom_ribbon( aes(ymin = lwr, ymax = upr, fill = grp, color = NULL), alpha = .15) +
     geom_line( aes(y = fit), size = 1)


head( seq(min(dat$x1), max(dat$x1), by = .1) )


newdat = expand.grid(x1 = seq(min(dat$x1), max(dat$x1), by = .1),
                     grp = unique(dat$grp) )


newdat$predlm = predict(fitlm, newdata = newdat)


ggplot(dat, aes(x = x1, y = resp, color = grp) ) +
     geom_point() +
     geom_line(data = newdat, aes(y = predlm), size = 1)


fitlme = lme(resp ~ grp + x1 + x2, 
             random = ~1|block,
             data = dat)


newdat.lme = data.frame(grp = dat$grp,
                        x1 = dat$x1,
                        x2 = median(dat$x2) )
head(newdat.lme)


newdat.lme$predlme = predict(fitlme, newdata = newdat.lme, level = 0)


ggplot(dat, aes(x = x1, y = resp, color = grp) ) +
     geom_rug(sides = "b", size = 1) +
     geom_line(data = newdat.lme, aes(y = predlme), size = 1)


des = model.matrix(formula(fitlme)[-2], newdat.lme)


predvar = diag( des %*% vcov(fitlme) %*% t(des) )


newdat.lme$lower = with(newdat.lme, predlme - 2*sqrt(predvar) )
newdat.lme$upper = with(newdat.lme, predlme + 2*sqrt(predvar) )


ggplot(dat, aes(x = x1, y = resp, color = grp) ) +
     geom_rug(sides = "b", size = 1) +
     geom_ribbon(data = newdat.lme, aes(y = NULL, ymin = lower, ymax = upper, 
                                        color = NULL, fill = grp),
                 alpha = .15) +
          geom_line(data = newdat.lme, aes(y = predlme), size = .75)

