library(ggplot2) # v.3.2.0




reformulate(termlabels = "am", response = "mpg")


as.formula( paste("mpg", "~ am") )


lm( reformulate("am", response = "mpg"), data = mtcars)


lm_fun = function(response) {
  lm( reformulate("am", response = response), data = mtcars)
}


lm_fun(response = "mpg")
lm_fun(response = "wt")


lm_fun2 = function(response) {
  resp = deparse( substitute( response) )
  lm( reformulate("am", response = resp), data = mtcars)
}


lm_fun2(response = mpg)


expl = c("am", "disp")
reformulate(expl, response = "mpg")


lm_fun_expl = function(expl) {
  form = reformulate(expl, response = "mpg")
  lm(form, data = mtcars)
}


lm_fun_expl(expl = c("am", "disp") )


lm_fun_expl2 = function(...) {
  form = reformulate(c(...), response = "mpg")
  lm(form, data = mtcars)
}


lm_fun_expl2("am", "disp")


lm_modfit = function(response) {
  resp = deparse( substitute( response) )
  mod = lm( reformulate("am", response = resp), data = mtcars)
  resvfit = qplot(x = mod$fit, y = mod$res) + theme_bw()
  resdist = qplot(x = "Residual", mod$res, geom = "boxplot") + theme_bw()
  list(resvfit, resdist, anova(mod) )
}

mpgfit = lm_modfit(mpg)

mpgfit[1:2]

mpgfit[[3]]
