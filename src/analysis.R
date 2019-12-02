#set the working directory 
library(lme4)
library(car)

setwd("~/Box/Wood_Rotation/Truncatula_local_adaptation/")

#load in the data 
dat <- read.csv("./data/unique_IDs_lupulina_leaves112219.csv")

#dat is a raw data file, so we'll construct a dataframe with our two factors, site and nematode treatment 
#that will make it more ammendable to analysis with lm or lme4 

PLE <- grep("PLE*", dat$exp_id)
MLBS <- grep("MLBS*", dat$exp_id)
TO <- grep("TO*", dat$exp_id)

#Add a column with the site data
dat$site <- rep(NA,nrow(dat))
dat$site[PLE] <- "PLE"
dat$site[MLBS] <- "MLBS"
dat$site[TO] <- "TO"

#Add a column with the nematode treatment to it. Block_plot has the nematode treatment abended in the id, abs = absent, pres=present
#so we'll just grep those to get an index that can be used to generate a column with the presence absence treatment. 
#nematode absent
Nematode_abs <- grep("*abs", dat$block_plot)
#nematode present 
Nematode_pres <- grep("pres", dat$block_plot)
dat$nematode <- rep(NA,nrow(dat))
dat$nematode[Nematode_abs] <- "abs"
dat$nematode[Nematode_pres] <- "pres"

#Finally, we'll add a block factor to our dataframe using a similar application of grep as above. 
block1 <- grep("Block1*", dat$block_plot)
block2 <- grep("Block2*", dat$block_plot)
block3 <- grep("Block3*", dat$block_plot)
block4 <- grep("Block4*", dat$block_plot)
block5 <- grep("Block5*", dat$block_plot)
block6 <- grep("Block6*", dat$block_plot)
block7 <- grep("Block7*", dat$block_plot)
block8 <- grep("Block8*", dat$block_plot)
block9 <- grep("Block9*", dat$block_plot)
block10 <- grep("Block10*", dat$block_plot)

#add it to a new atomic vector 
dat$block <- rep(NA,nrow(dat))
dat$block[block1] <- 1
dat$block[block2] <- 2
dat$block[block3] <- 3
dat$block[block4] <- 4
dat$block[block5] <- 5
dat$block[block6] <- 6
dat$block[block7] <- 7
dat$block[block8] <- 8
dat$block[block9] <- 9
dat$block[block10] <- 10

#run the full model for main effects of site and nematode treatment and interaction effect.
fitness_fullmodel <- lm(fruit_mass ~ site + nema_trt + site*nema_trt, data = dat)

#put results in an anova table. 
anova(fitness_fullmodel)

#How to specific random effects

fullModel <- lmer(height_cm ~ site + nematode + site*nematode + (1|block), data = dat)
Anova(fullModel)



