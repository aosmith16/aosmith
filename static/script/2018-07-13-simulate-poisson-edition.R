b0 = .5
b1 = .5
b2 = 5


set.seed(16)
x = runif(100, min = 0, max = 1)
head(x) # First six values of x


lambda = exp(b0 + b1*x + b2*x^2)

head(lambda)


y = rpois(100, lambda = lambda) 

head(y)


plot(x, log(y) )


plot(x, y)
