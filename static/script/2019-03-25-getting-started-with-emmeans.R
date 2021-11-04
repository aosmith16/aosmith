library(emmeans) # v. 1.7.0
library(magrittr) # v. 2.0.1


dat = data.frame(resp = c(1.6,0.3,3,0.1,3.2,0.2,0.4,0.4,2.8,
                          0.7,3.8,3,0.3,14.3,1.2,0.5,1.1,4.4,0.4,8.4),
                 f1 = factor(c("a","a","a","a","a",
                               "a","a","a","a","a","c","c","c","c","c",
                               "c","c","c","c","c")),
                 f2 = factor(c("1","c","1","c","1",
                               "c","1","c","1","c","1","c","1","c","1",
                               "c","1","c","1","c")))

str(dat)


fit1 = lm(log(resp) ~ f1 + f2 + f1:f2, data = dat)


emm1 = emmeans(fit1, specs = pairwise ~ f1:f2)


emm1$emmeans

emm1$contrasts


emmeans(fit1, specs = pairwise ~ f1:f2, type = "response")


emm1.1 = emmeans(fit1, specs = pairwise ~ f1:f2, type = "response", adjust = "none")
emm1.1


emm1.1$contrasts %>%
     confint()


emm1.1$contrasts %>%
     summary(infer = TRUE)


emm1.1$emmeans %>%
     as.data.frame()


emm2 = emmeans(fit1, specs = pairwise ~ f1|f2, type = "response")
emm2


emm2$contrasts %>%
     rbind() 


emmeans(fit1, specs = pairwise ~ f1)


emmeans(fit1, specs = trt.vs.ctrl ~ f1:f2)


emmeans(fit1, specs = trt.vs.ctrlk ~ f1:f2)


emmeans(fit1, specs = trt.vs.ctrlk ~ f1:f2, ref = 2)


emmeans(fit1, specs = trt.vs.ctrlk ~ f1:f2, ref = 2, reverse = TRUE)


emm3 = emmeans(fit1, specs = ~ f1:f2, type = "response")
emm3


contrast(emm3, method = "pairwise", adjust = "none")

