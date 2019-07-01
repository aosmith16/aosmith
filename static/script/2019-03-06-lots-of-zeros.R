library(ggplot2) # v. 3.1.0
library(HMMpa) # v. 1.0.1
library(MASS) # v. 7.3-51.1


set.seed(16)
dat = data.frame(Y = rnbinom(200, mu = 10, size = .05) )


ggplot(dat, aes(x = Y) ) +
	geom_histogram(binwidth = 5)  +
	theme_bw(base_size = 18) +
	labs(y = "Frequency",
	     title = "Negative binomial",
	     subtitle = "mean = 10, theta = 0.05" ) +
	annotate(geom = "text",
		    label = paste("Proportion 0:", mean(dat$Y == 0), 
		    		    "\nMax Count:", max(dat$Y) ),
		    		    x = 150, y = 100, size = 8)


set.seed(16)
dat = data.frame(Y = rgenpois(200, lambda1 = 0.5, lambda2 = 0.95) )


ggplot(dat, aes(x = Y) ) +
	geom_histogram(binwidth = 5)  +
	theme_bw(base_size = 18) +
	labs(y = "Frequency",
	     title = "Generalized Poisson",
	     subtitle = "lambda1 = 0.5, lambda2 = 0.95") +
	annotate(geom = "text",
		    label = paste("Proportion 0:", mean(dat$Y == 0), 
		    		    "\nMax Count:", max(dat$Y) ),
		    		    x = 600, y = 100, size = 8)


set.seed(16)
x = runif(200, 5, 10) # simulate explanatory variable
b0 = 1 # set value of intercept
b1 = 0.25 # set value of slope
means = exp(b0 + b1*x) # calculate true means
theta = 0.25 # true theta

y = rnbinom(200, mu = means, size = theta)


fit1 = glm.nb(y ~ x)


sum(y == 0)


preds = predict(fit1, type = "response") # estimated means
esttheta = summary(fit1)$theta # estimated theta


prop0 = dnbinom(x = 0, mu = preds, size = esttheta )

round( sum(prop0) )


fit2 = glm(y ~ x, family = poisson)

sum(y == 0)


round( sum( dpois(x = 0,
           lambda = predict(fit2, type = "response") ) ) )

