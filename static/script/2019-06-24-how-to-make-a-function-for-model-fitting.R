library(ggplot2) # v.3.2.0




paste("mpg", "~ am")


as.formula( paste("mpg", "~ am") )


lm( as.formula( paste("mpg", "~ am") ), data = mtcars)


lm_fun = function(response) {
  lm( as.formula( paste(response, "~ am") ), data = mtcars)
}


lm_fun(response = "mpg")
lm_fun(response = "wt")


lm_fun2 = function(response) {
  resp = deparse( substitute( response) )
  lm( as.formula( paste(resp, "~ am") ), data = mtcars)
}


lm_fun2(response = mpg)


expl = c("am", "disp")
paste(expl, collapse = "+")


as.formula( paste("mpg ~", paste(expl, collapse = "+") ) )

lm_fun_expl = function(expl) {
  form = as.formula( paste("mpg ~ ", paste(expl, collapse = "+") ) )
  lm(form, data = mtcars)
}


lm_fun_expl(expl = c("am", "disp") )


lm_fun_expl2 = function(...) {
  form = as.formula( paste("mpg ~ ", paste( c(...), collapse = "+") ) )
  lm(form, data = mtcars)
}


lm_fun_expl2("am", "disp")


lm_modfit = function(response) {
  resp = deparse( substitute( response) )
  mod = lm( as.formula( paste(resp, "~ am") ), data = mtcars)
  resvfit = qplot(x = mod$fit, y = mod$res) + theme_bw()
  resdist = qplot(x = "Residual", mod$res, geom = "boxplot") + theme_bw()
  list(resvfit, resdist, anova(mod) )
}

mpgfit = lm_modfit(mpg)

mpgfit[1:2]

mpgfit[[3]]
