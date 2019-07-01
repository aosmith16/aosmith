library(emmeans) # v. 1.3.3
library(magrittr) # v. 1.5


dat = structure(list(sub.rate = structure(c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 
2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 4L, 5L, 5L, 5L, 
5L, 5L), .Label = c("A.1", "A.2", "B.1", "B.2", "control"), class = "factor"), 
    resp = c(5.5, 4.9, 6.1, 3.6, 6.1, 3.5, 3, 4.1, 5, 4.6, 7.3, 
    5.6, 4.8, 7.2, 6.2, 4.3, 6.6, 6.5, 5.5, 7.1, 5.4, 6.7, 6.8, 
    8.5, 6.1)), row.names = c(NA, -25L), class = "data.frame")

str(dat)


fit1 = lm(resp ~ sub.rate, data = dat)


emmeans(fit1, specs = trt.vs.ctrlk ~ sub.rate)


emm1 = emmeans(fit1, specs = ~ sub.rate)
emm1


A2 = c(0, 1, 0, 0, 0)

B2 = c(0, 0, 0, 1, 0)

contrast(emm1, method = list(A2 - B2) )


contrast(emm1, method = list("A2 - B2" = A2 - B2) )


emmeans(fit1, specs = pairwise ~ sub.rate, 
         at = list(sub.rate = c("A.2", "B.2") ) )


contrast(emm1, method = list("A2 - B2" = A2 - B2,
                             "B2 - A2" = B2 - A2) )


twocomp = contrast(emm1, method = list("A2 minus B2" = A2 - B2,
                             "B2 minus A2" = B2 - A2),
         adjust = "mvt") %>%
     confint()
twocomp


emm1

A1 = c(1, 0, 0, 0, 0)
A2 = c(0, 1, 0, 0, 0)
B1 = c(0, 0, 1, 0, 0)
B2 = c(0, 0, 0, 1, 0)


Aoverall = (A1 + A2)/2
Boverall = (B1 + B2)/2


contrast(emm1, method = list("A - B" = Aoverall - Boverall) ) 

