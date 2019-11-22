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


dat$site <- for(i in 1:nrow(dat)){
  grep
}
