library(DHARMa) # version 0.1.5
library(glmmTMB) # version 0.1.1


fit_nbz = glmmTMB(count ~ spp + mined + (1|site), 
                  zi = ~spp + mined, 
                  family = nbinom2, data = Salamanders)


sim_nbz = simulate(fit_nbz, nsim = 10)

str(sim_nbz)


sim_nbz = do.call(cbind, sim_nbz)
head(sim_nbz)


sim_res_nbz = createDHARMa(simulatedResponse = sim_nbz, 
                           observedResponse = Salamanders$count,
                           fittedPredictedResponse = predict(fit_nbz),
                           integerResponse = TRUE)

plotSimulatedResiduals(sim_res_nbz)


library(pscl) # version 1.5.2
library(VGAM) # version 1.0-4


fit_zinb = zeroinfl(art ~ . | 1, 
                    data = bioChemists, 
                    dist = "negbin")


# Predicted probabilities
p = predict(fit_zinb, type = "zero")
# Predicted counts
mus = predict(fit_zinb, type = "count")

fit_zinb$theta


sim1 = rzinegbin(n = nrow(bioChemists),
                 size = fit_zinb$theta,
                 pstr0 = p,
                 munb = mus)


sim_zinb = replicate(10, rzinegbin(n = nrow(bioChemists),
                                   size = fit_zinb$theta,
                                   pstr0 = p,
                                   munb = mus) )

head(sim_zinb)


sim_res_zinb = createDHARMa(simulatedResponse = sim_zinb,
                            observedResponse = bioChemists$art,
                            fittedPredictedResponse = predict(fit_zinb, type = "response"),
                            integerResponse = TRUE)

plotSimulatedResiduals(sim_res_zinb)
