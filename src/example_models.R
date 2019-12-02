###Simulate data for fruit mass

PLE_N_abs <- rnorm(30, mean = 0.4, sd = 0.1)
PLE_N_pres <- rnorm(30, mean = 0.35, sd = 0.1)
MLBS_N_abs <- rnorm(30, mean = 0.34, sd = 0.1)
MLBS_N_pres <- rnorm(30, mean = 0.15, sd = 0.1)
TO_N_abs <- rnorm(30, mean = 0.32, sd = 0.1)
TO_N_pres <- rnorm(30, mean = 0.1, sd = 0.1)

#put it in a dataframe that is understandable to the lm function
dat <- data.frame("site" = c(rep("PLE", 60), rep("MLBS", 60), rep("TO", 60)), 
                  "nema_trt" = rep(c(rep("absent", 30), rep("present", 30)), 3), 
                  "fruit_mass" = c(PLE_N_abs, PLE_N_pres, MLBS_N_abs, MLBS_N_pres, TO_N_abs, TO_N_pres))

#run the full model for main effects of site and nematode treatment and interaction effect.
fitness_fullmodel <- lm(fruit_mass ~ site + nema_trt + site*nema_trt, data = dat)

#put results in an anova table. 
anova(fitness_fullmodel)

#How to specific random effects

lmer(fruit_mass ~ site + nema_trt + site*nema_trt + (1|block), data = dat )
