#set the working directory 
library(lme4)
library(car)
library(effects)
##############################################################################################
#BE SURE TO SET THE WD TO THE NAME OF THIS REPO, E.G. /YOuR/PATH/Lupulina_local_adaptation
##############################################################################################
setwd("~/Box/Wood_Rotation/Lupulina_local_adaptation/")

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
dat$block_plot <- as.character(dat$block_plot)
block1 <- grep("Block1_*", dat$block_plot)
block2 <- grep("Block2_*", dat$block_plot)
block3 <- grep("Block3_*", dat$block_plot)
block4 <- grep("Block4_*", dat$block_plot)
block5 <- grep("Block5_*", dat$block_plot)
block6 <- grep("Block6_*", dat$block_plot)
block7 <- grep("Block7_*", dat$block_plot)
block8 <- grep("Block8_*", dat$block_plot)
block9 <- grep("Block9_*", dat$block_plot)
block10 <- grep("Block10_*", dat$block_plot)

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

#Now, we add genotype


MLBS_0202 <- grep("MLBS_02-02*", dat$exp_id)
MLBS_1003 <- grep("MLBS_10-03*", dat$exp_id)
MLBS_1407 <- grep("MLBS_14-07*", dat$exp_id)
MLBS_1708 <- grep("MLBS_17-08*", dat$exp_id)
PLE_0103 <- grep("PLE_01-03*", dat$exp_id)
PLE_0202 <- grep("PLE_02-02*", dat$exp_id)
PLE_0307 <- grep("PLE_03-07*", dat$exp_id)
PLE_0403 <- grep("PLE_04-03*", dat$exp_id)
TO_0405 <- grep("TO_04-05*", dat$exp_id)
TO_0510 <- grep("TO_05-10*", dat$exp_id)
TO_0705 <- grep("TO_07-05*", dat$exp_id)
TO_1503 <- grep("TO_15-03*", dat$exp_id)

dat$genotype <- rep(NA, nrow(dat))
dat$genotype[MLBS_0202] <- "MLBS_0202" 
dat$genotype[MLBS_1003] <- "MLBS_1003" 
dat$genotype[MLBS_1407] <- "MLBS_1407" 
dat$genotype[MLBS_1708] <- "MLBS_1708"
dat$genotype[PLE_0103] <- "PLE_0103" 
dat$genotype[PLE_0202] <- "PLE_0202" 
dat$genotype[PLE_0307] <- "PLE_0307" 
dat$genotype[PLE_0403] <- "PLE_0403" 
dat$genotype[TO_0405] <- "TO_0405"
dat$genotype[TO_0510] <- "TO_0510" 
dat$genotype[TO_0705] <- "TO_0705" 
dat$genotype[TO_1503] <- "TO_1503" 

sum(is.na(dat$height_cm[dat$genotype == "MLBS_0202"]))
sum(is.na(dat$height_cm[dat$genotype == "MLBS_1003"]))
sum(is.na(dat$height_cm[dat$genotype == "MLBS_1407"]))
sum(is.na(dat$height_cm[dat$genotype == "MLBS_1708"]))

sum(is.na(dat$height_cm[dat$genotype == "PLE_0103"]))
sum(is.na(dat$height_cm[dat$genotype == "PLE_0202"]))
sum(is.na(dat$height_cm[dat$genotype == "PLE_0307"]))
sum(is.na(dat$height_cm[dat$genotype == "PLE_0403"]))

sum(is.na(dat$height_cm[dat$genotype == "TO_0405"]))
sum(is.na(dat$height_cm[dat$genotype == "TO_0510"]))
sum(is.na(dat$height_cm[dat$genotype == "TO_0705"]))
sum(is.na(dat$height_cm[dat$genotype == "TO_1503"]))

#Check to ensure the data is normally distributied 
hist(dat$height_cm[dat$site=="TO" & dat$nematode=="abs"])
hist(dat$height_cm[dat$site=="TO" & dat$nematode=="pres"])
hist(dat$height_cm[dat$site=="PLE" & dat$nematode=="abs"])
hist(dat$height_cm[dat$site=="PLE" & dat$nematode=="pres"])
hist(dat$height_cm[dat$site=="MLBS" & dat$nematode=="abs"])
hist(dat$height_cm[dat$site=="MLBS" & dat$nematode=="pres"])

#The assumption of 

#First we'll run the classic two-way ANOVA as initially proposed, 

twoWayAnova <- lm(height_cm ~ site + nematode + site*nematode, data = dat, 
                  contrasts = list(site = contr.sum, nematode = contr.sum))

#Check the assumption of equal variance among all combinations of explanatory variables 
plot(resid(twoWayAnova))

#To print the 
Anova(twoWayAnova, type =  "III")

#We can also run a 
fullModel <- lmer(height_cm ~ site + nematode + site*nematode + (1|block) + (1|site:genotype), data = dat)
#Model_noNesting <- lmer(height_cm ~ site + nematode + site*nematode + (1|block), data = dat)

Anova(fullModel, type = "III")
#Anova(Model_noNesting, type = "III")

#output the results
out <- capture.output(Anova(twoWayAnova, type =  "III")
,Anova(fullModel, type = "III"))

cat("Results", out, file="./results/fullModel_summary.txt", sep="\n", append=FALSE)

#graph the results 
SE <- function(x){
  sd(x, na.rm=T)/sqrt(length(x))
}

TO_abs <- mean(dat$height_cm[dat$site == "TO" & dat$nematode == "abs"], na.rm = T)
TO_abs_SE <- SE(dat$height_cm[dat$site == "TO" & dat$nematode == "abs"])
TO_pres <- mean(dat$height_cm[dat$site == "TO" & dat$nematode == "pres"], na.rm = T)
TO_pres_SE <- SE(dat$height_cm[dat$site == "TO" & dat$nematode == "pres"])
PLE_abs <- mean(dat$height_cm[dat$site == "PLE" & dat$nematode == "abs"], na.rm = T)
PLE_abs_SE <- SE(dat$height_cm[dat$site == "PLE" & dat$nematode == "abs"])
PLE_pres <- mean(dat$height_cm[dat$site == "PLE" & dat$nematode == "pres"], na.rm = T)
PLE_pres_SE <- SE(dat$height_cm[dat$site == "PLE" & dat$nematode == "pres"])
MLBS_abs <- mean(dat$height_cm[dat$site == "MLBS" & dat$nematode == "abs"], na.rm = T)
MLBS_abs_SE <- SE(dat$height_cm[dat$site == "MLBS" & dat$nematode == "abs"])
MLBS_pres <- mean(dat$height_cm[dat$site == "MLBS" & dat$nematode == "pres"], na.rm = T)
MLBS_pres_SE <- SE(dat$height_cm[dat$site == "MLBS" & dat$nematode == "pres"])

fig_dat <- data.frame("nematode" = c(rep("pres", 3), rep("abs", 3)), "site" = rep(c("TO", "PLE", "MLBS"),2), 
                      "height_cm" = c(TO_pres, PLE_pres, MLBS_pres, TO_abs, PLE_abs, MLBS_abs), 
                      "SEs" = c(TO_pres_SE, PLE_pres_SE, MLBS_pres_SE, TO_abs_SE, PLE_abs_SE, MLBS_abs_SE))


pdf("./results/TwoWayANOVA.pdf")
ggplot(fig_dat, aes(x=site, y=height_cm, group=nematode, color=nematode)) + 
  geom_point(position = position_dodge(width = 0.8), shape=15, size=2.5)+
  #geom_line(data = as.data.frame(effects), aes(x=site, y=fit, color = nematode, fill=nematode), position = position_dodge(width = 0.8))+
  geom_line(position = position_dodge(width = 0.8))+
  geom_errorbar(aes(ymin=height_cm-SEs, ymax=height_cm+SEs), width = 0, position = position_dodge(width = 0.8))+
  geom_point(data=dat, aes(x=site, y=height_cm, group=nematode, color=nematode), 
             position=position_jitterdodge(dodge.width = 0.8, jitter.width = 0.2), alpha=1/3)+
  theme_classic()+
  ylab("Plant height (cm)")+
  xlab("Population")+ 
  ylim(0,5)
dev.off()
  



#First get the effects that we can plot

# effects <- effect("site*nematode", fullModel) 

# pdf("./results/FullModelFit.pdf")
# ggplot(dat, aes(x=site, y=height_cm, group=nematode, color=nematode)) + 
#   geom_point(position=position_jitterdodge(dodge.width = 0.8, jitter.width = 0.2), alpha=1/3)+
#   geom_line(data = as.data.frame(effects), aes(x=site, y=fit, color = nematode, fill=nematode), position = position_dodge(width = 0.8))+
#   #scale_fill_discrete(name = "Nematode treatment", labels = c("absent", "present"))+
#   geom_point(data = as.data.frame(effects), aes(x=site, y=fit, color = nematode, fill=nematode), position = position_dodge(width = 0.8),
#              shape=15, size=3.5)+
#   geom_errorbar(data = as.data.frame(effects), aes(x=site, y=fit, color = nematode, fill=nematode, ymin=lower, ymax=upper), 
#                 position = position_dodge(width = 0.8), 
#                 width=0)+
#   ylab("Plant height (cm)")+
#   xlab("Population")+ 
#   theme_classic()
# dev.off()


# interaction.plot(algae$herbivores, algae$height, response = predict(algaeFullModel), 
#                  ylim = range(algae$sqrtArea), trace.label = "Height", las = 1,
#                  ylab = "Square root surface area (cm)", xlab = "Herbivore treatment")
# adjustAmount = 0.05
# points(sqrtArea ~ c(jitter(as.numeric(herbivores), factor = 0.2) + adjustAmount), 
#        data = subset(algae, height == "low"))
# points(sqrtArea ~ c(jitter(as.numeric(herbivores), factor = 0.2) - adjustAmount), 
#        data = subset(algae, height == "mid"), pch = 16)

