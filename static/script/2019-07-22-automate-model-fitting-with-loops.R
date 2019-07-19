library(purrr) # v. 0.3.2
library(ggplot2) # v. 3.2.0
library(patchwork) # v. 0.0.1, github only
library(broom) # v. 0.5.2


dat = structure(list(group = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
2L, 2L, 2L, 2L, 2L, 2L, 2L), .Label = c("a", "b"), class = "factor"), 
    resp = c(10.48, 9.87, 11.1, 8.56, 11.15, 9.53, 8.99, 10.06, 
    11.02, 10.57, 11.85, 10.11, 9.25, 11.66, 10.72, 8.34, 10.58, 
    10.47, 9.46, 11.13, 8.35, 9.69, 9.82, 11.47, 9.13, 11.53, 
    11.05, 11.03, 10.84, 10.22), slp = c(38.27, 46.33, 44.29, 
    35.57, 34.78, 47.81, 50.45, 46.31, 47.82, 42.07, 31.75, 65.65, 
    47.42, 41.51, 38.69, 47.84, 46.22, 50.66, 50.69, 44.09, 47.3, 
    52.53, 53.63, 53.38, 27.34, 51.83, 56.63, 32.99, 77.5, 38.24
    ), grad = c(0.3, 0.66, 0.57, 0.23, 0.31, 0.48, 0.5, 0.49, 
    2.41, 0.6, 0.27, 0.89, 2.43, 1.02, 2.17, 1.38, 0.17, 0.47, 
    1.1, 3.28, 6.14, 3.8, 4.35, 0.85, 1.13, 1.11, 2.93, 1.13, 
    4.52, 0.13)), class = "data.frame", row.names = c(NA, -30L) )
head(dat)


ttest_fun = function(response) {
  form = paste(response, "~ group")
  lm(as.formula(form), data = dat)
}

ttest_fun(response = "resp")


vars = names(dat)[2:4]
vars


vars = set_names(vars)
vars


models = vars %>%
     map(ttest_fun)

models


# vars %>%
#      set_names() %>%
#      map(ttest_fun)
# 

resid_plots = function(model, modelname) {
     output = augment(model)
     res.v.fit = ggplot(output, aes(x = .fitted, y = .resid) ) +
          geom_point() +
          theme_bw(base_size = 16)
     res.box = ggplot(output, aes(x = "", y = .resid) ) +
          geom_boxplot() +
          theme_bw(base_size = 16) +
          labs(x = NULL)
     res.v.fit + res.box +
          plot_annotation(title = paste("Residuals plots for", modelname) )
}

resid_plots(model = models[[1]], modelname = names(models)[1])


residplots = imap(models, resid_plots)

residplots[[3]]


gradmod = ttest_fun("log(grad)")


models$log_grad = gradmod

models$grad = NULL

models


models[!names(models) %in% "slp"]


res_anova = map_df(models, tidy, conf.int = TRUE, .id = "variable")
res_anova
