################################################################################################################################################
# Main module
#############

# Note: will need to check/update side encoding before running the full models
# Read in and clean transceiver data
source('C:/Users/19708/Desktop/functions-11-20.R', echo=TRUE)
library(readxl)
transceiver_data_raw <- read_excel("C:/Users/19708/Desktop/Herring/Modeling/Data/transceiver-data.xlsx", sheet = "herring 2017 and beyond jun 201")
transceiver_data_cleaned <- clean.transceiver.data(transceiver_data_raw)

# Add the 10-21-receiver data
transceiver_data_new <- read.csv("C:/Users/19708/Desktop/Herring/Modeling/Data/transceiver-data-10-21.csv", header=T)
full_transceiver_data <- add.transceiver.data(transceiver_data_cleaned, transceiver_data_new)

# Add the 11-7-receiver data
transceiver_data_new <- read.csv("C:/Users/19708/Desktop/Herring/Modeling/Data/transceiver-data-11-7.csv", header=T)
full_transceiver_data <- add.transceiver.data(full_transceiver_data, transceiver_data_new)

# Change the Station of VR3-UVM-156 to MB
full_transceiver_data <- adjust.VR3UVM156(full_transceiver_data)

# Remove the single detection at Profiler
full_transceiver_data <- full_transceiver_data[-which(full_transceiver_data$Region=="Profiler"),]

# To remove isolated (single) detections
# transceiver_data_filtered <- filter.single.detections(full_transceiver_data)
transceiver_data_filtered <- full_transceiver_data
save(transceiver_data_filtered, file="C:/Users/19708/Desktop/Herring/Modeling/Data/RData/transceiver-data-filtered.RData")
load("C:/Users/19708/Desktop/Herring/Modeling/Data/RData/transceiver-data-filtered.RData")

# Read in and clean tagging data
tagging_data_raw <- read_excel("C:/Users/19708/Desktop/Herring/Modeling/Data/tagging-data-full.xlsx", sheet = "2017 to 2019 tag data")
tagging_data_cleaned <- clean.tagging.data(tagging_data_raw)

# Encode the side information for the receiver arrays. 
{
  # Isolate receiver data
  receiver_data <- isolate.receiver.data(transceiver_data_filtered)
  
  # Need to check these
  # For the Hinchinbrook array
  hinch_array <- receiver_data[receiver_data$Region=="Hinchinbrook",]
  hinch_array[,6]<- rep(c("in", "out", "in","out","in","out"), c(1,2,3,2,26,1))
  names(hinch_array)[6]<-"Side"
  hinch_array <- hinch_array[,-c(4,5)]
  
  # For Montague
  mont_array <- receiver_data[receiver_data$Region=="Montague",]
  mont_array[,6]<- rep(c("in","out","in","out","in","out","in","out","in","out","in","out","in","out"),c(1,1,1,1,4,1,2,2,1,1,10,1,1,1))
  names(mont_array)[6]<-"Side"
  mont_array <- mont_array[,-c(4,5)]
  
  # For Elrington 8
  elring_array <- receiver_data[receiver_data$Region=="Elrington",]
  elring_array[,6]<- rep(c("in","out","in","out","in"),c(2,2,1,2,1))
  names(elring_array)[6]<-"Side"
  elring_array <- elring_array[,-c(4,5)]
  
  # For Prince of Wales 7
  pow_array <- receiver_data[receiver_data$Region=="Prince of Wales",]
  pow_array[,6]<- rep(c("out","in","out","in","out","in"),c(1,2,1,1,1,1))
  names(pow_array)[6]<-"Side"
  pow_array <- pow_array[,-c(4,5)]
  
  # For LaTouche 6
  latouche_array <- receiver_data[receiver_data$Region=="LaTouche",]
  latouche_array[,6] <- rep(c("in","out"),c(2,4))
  names(latouche_array)[6] <- "Side"
  latouche_array <- latouche_array[,-c(4,5)]
  
  # For Bainbridge 2
  bain_array <- receiver_data[receiver_data$Region=="Bainbridge",]
  bain_array[,6] <- c("in", "out")
  names(bain_array)[6]<-"Side"
  bain_array <- bain_array[,-c(4,5)]
  
  # For Gravina
  grav_array <- receiver_data[receiver_data$Region=="Gravina",]
  grav_array <- grav_array[,-c(4,5)]
  grav_array[,4] <- NA
  names(grav_array)[4] <- "Side"
  
  # For RedGravina
  red_grav_array <- receiver_data[receiver_data$Region=="RedGravina",]
  red_grav_array <- red_grav_array[,-c(4,5)]
  red_grav_array[,4] <- NA
  names(red_grav_array)[4] <- "Side"
  
  # For Hawkins
  hawk_array <- receiver_data[receiver_data$Region=="Hawkins",]
  hawk_array <- hawk_array[,-c(4,5)]
  hawk_array[,4] <- NA
  names(hawk_array)[4] <- "Side"
  
  # For NoMontague
  noMont_array <- receiver_data[receiver_data$Region=="NoMontague",]
  noMont_array <- noMont_array[,-c(4,5)]
  noMont_array[,4] <- NA
  names(noMont_array)[4] <- "Side"
  
  # For Glacier
  glacier_array <- receiver_data[receiver_data$Region=="Glacier",]
  glacier_array <- glacier_array[,-c(4,5)]
  glacier_array[,4] <- NA
  names(glacier_array)[4] <- "Side"
  
  # For Knight Island
  knight_array <- receiver_data[receiver_data$Region=="KnightIP",]
  knight_array <- knight_array[,-c(4,5)]
  knight_array[,4] <- NA
  names(knight_array)[4] <- "Side"
    
  # Summarize all of the array info in a data frame
  receiver_data <- rbind(grav_array, red_grav_array, hawk_array, glacier_array, noMont_array, hinch_array, mont_array, elring_array, pow_array, latouche_array, bain_array, knight_array)

  # Add this information to the main data.frame
  transceiver_data_side <- add.side(transceiver_data_filtered, receiver_data)
  save(transceiver_data_side, file="C:/Users/19708/Desktop/Herring/Modeling/Data/RData/transceiver_data_side.RData")
}

# Determine the state associated with each detection
transceiver_data_encoded <- add.observation.encoding(transceiver_data_side) # For main model

# When did the experiment start/end
start_time <- get.start.time(tagging_data_cleaned)
end_time <- as.POSIXct("2019/08/31", format="%Y/%m/%d", tz="UTC")

# Compute detection histories for each individual
detection_history_list <- get.detecton.history.list(transceiver_data_encoded, start_time, end_time)

# So that I don't need to re-run stuff
save(tagging_data_cleaned, file="C:/Users/19708/Desktop/Herring/Modeling/Data/RData/tagging-data-cleaned.RData")
save(detection_history_list, file="C:/Users/19708/Desktop/Herring/Modeling/Data/RData/detection-history-list.RData")

# To load processed data
load("C:/Users/19708/Desktop/Herring/Modeling/Data/RData/tagging-data-cleaned.RData")
load("C:/Users/19708/Desktop/Herring/Modeling/Data/RData/detection-history-list.RData")

# Prepare detection data to be used in Jags model
y <- get.y(detection_history_list)
missing_fish <- find.missing.fish(detection_history_list, tagging_data_cleaned)
for (fish in missing_fish){
  y<-add.fish.to.y(fish, y)
}
y <- y[,1:874]
y <- compress.y(y, 7)
y <- remove.incon.detect(y)

# Prepare dps vector
date_vector <- c("2017-04-09", "2017-08-31","2018-03-31","2018-08-31","2019-03-31", "2019-08-31")
dps <- get.days.per.season(date_vector)

side_counts_length_h <- get.side.counts(dps, detection_history_list, by="length", tagging_data_cleaned, entrance = 4)
side_counts_length_m <- get.side.counts(dps, detection_history_list, by="length", tagging_data_cleaned, entrance = 5)
side_counts_length_s <- get.side.counts(dps, detection_history_list, by="length", tagging_data_cleaned, entrance = 6)
side_counts_length <- list(side_counts_length_h, side_counts_length_m, side_counts_length_s)

side_counts_weight_h <- get.side.counts(dps, detection_history_list, by="weight", tagging_data_cleaned, entrance = 4)
side_counts_weight_m <- get.side.counts(dps, detection_history_list, by="weight", tagging_data_cleaned, entrance = 5)
side_counts_weight_s <- get.side.counts(dps, detection_history_list, by="weight", tagging_data_cleaned, entrance = 6)
side_counts_weight <- list(side_counts_weight_h, side_counts_weight_m, side_counts_weight_s)

side_counts_cond_h <- get.side.counts(dps, detection_history_list, by="condition", tagging_data_cleaned, entrance = 4)
side_counts_cond_m <- get.side.counts(dps, detection_history_list, by="condition", tagging_data_cleaned, entrance = 5)
side_counts_cond_s <- get.side.counts(dps, detection_history_list, by="condition", tagging_data_cleaned, entrance = 6)
side_counts_cond <- list(side_counts_cond_h, side_counts_cond_m, side_counts_cond_s)

side_counts_sex_h <- get.side.counts(dps, detection_history_list, by="sex", tagging_data_cleaned, entrance = 4)
side_counts_sex_m <- get.side.counts(dps, detection_history_list, by="sex", tagging_data_cleaned, entrance = 5)
side_counts_sex_s <- get.side.counts(dps, detection_history_list, by="sex", tagging_data_cleaned, entrance = 6)
side_counts_sex <- list(side_counts_sex_h, side_counts_sex_m, side_counts_sex_s)

side_counts_burden_h <- get.side.counts(dps, detection_history_list, by="burden", tagging_data_cleaned, entrance = 4)
side_counts_burden_m <- get.side.counts(dps, detection_history_list, by="burden", tagging_data_cleaned, entrance = 5)
side_counts_burden_s <- get.side.counts(dps, detection_history_list, by="burden", tagging_data_cleaned, entrance = 6)
side_counts_burden <- list(side_counts_burden_h, side_counts_burden_m, side_counts_burden_s)

side_counts_spawn_h <- get.side.counts(dps, detection_history_list, by="spawn", tagging_data_cleaned, entrance = 4)
side_counts_spawn_m <- get.side.counts(dps, detection_history_list, by="spawn", tagging_data_cleaned, entrance = 5)
side_counts_spawn_s <- get.side.counts(dps, detection_history_list, by="spawn", tagging_data_cleaned, entrance = 6)
side_counts_spawn <- list(side_counts_spawn_h, side_counts_spawn_m, side_counts_spawn_s)

dps <- adjust.dps(dps, 7)

# Get x_data for Jags Model
x_length <- get.length(y, tagging_data_cleaned) # Check this line
x_weight <- get.weight(y, tagging_data_cleaned)
x_cond <- get.condition(y, tagging_data_cleaned)
x_sex <- get.sex(y, tagging_data_cleaned)
x_burden <- get.burden(y, tagging_data_cleaned)
x_spawn <- get.spawned.out(y, tagging_data_cleaned)

# Get and prepare vector of times when tagged fish were released
t_0 <- get.tagging.time(y, tagging_data_cleaned)
t_0 <- adjust.t_0(t_0, 7)

# Get and prepare vector of tag lifes
tl <- get.tag.life(y, tagging_data_cleaned)
tl <- adjust.tl(tl, 7)

# Estimated proportion of fish that stay at each array location
prop_stay <- get.prop.stay(y, t_0, tl)

# Save all of this information
jags_data_length <- list(y, x_length, t_0, side_counts_length, dps, tl, prop_stay)
jags_data_weight <- list(y, x_weight, t_0, side_counts_weight, dps, tl, prop_stay)
jags_data_cond <- list(y, x_cond, t_0, side_counts_cond, dps, tl, prop_stay)
jags_data_sex <- list(y, x_sex, t_0, side_counts_sex, dps, tl, prop_stay)
jags_data_burden <- list(y, x_burden, t_0, side_counts_burden, dps, tl, prop_stay)
jags_data_spawn <- list(y, x_spawn, t_0, side_counts_spawn, dps, tl, prop_stay)

save(jags_data_length, file="C:/Users/19708/Desktop/Herring/Modeling/Data/RData/jags-data-length.RData")
save(jags_data_weight, file="C:/Users/19708/Desktop/Herring/Modeling/Data/RData/jags-data-weight.RData")
save(jags_data_cond, file="C:/Users/19708/Desktop/Herring/Modeling/Data/RData/jags-data-cond.RData")
save(jags_data_sex, file="C:/Users/19708/Desktop/Herring/Modeling/Data/RData/jags-data-sex.RData")
save(jags_data_burden, file="C:/Users/19708/Desktop/Herring/Modeling/Data/RData/jags-data-burden.RData")
save(jags_data_spawn, file="C:/Users/19708/Desktop/Herring/Modeling/Data/RData/jags-data-spawn.RData")

# To load it back up
load("C:/Users/19708/Desktop/Herring/Modeling/Data/RData/jags-data-length.RData")
load("C:/Users/19708/Desktop/Herring/Modeling/Data/RData/jags-data-weight.RData")
load("C:/Users/19708/Desktop/Herring/Modeling/Data/RData/jags-data-cond.RData")
load("C:/Users/19708/Desktop/Herring/Modeling/Data/RData/jags-data-sex.RData")
load("C:/Users/19708/Desktop/Herring/Modeling/Data/RData/jags-data-burden.RData")
load("C:/Users/19708/Desktop/Herring/Modeling/Data/RData/jags-data-spawn.RData")

rm(list=objects())
source('C:/Users/19708/Desktop/functions-11-20.R', echo=TRUE)
load("C:/Users/19708/Desktop/Herring/Modeling/Data/RData/jags-data-cond.RData")

# To run the jags model without covariate
y <- jags_data_cond[[1]]
t_0 <- jags_data_cond[[3]]
tl <- jags_data_cond[[6]]
y <- patch.y(y, t_0, tl)
x_data <- jags_data_cond[[2]]
x_data <- as.factor(rep(1,length(x_data)))
side_counts <- jags_data_cond[[4]]
for (i in 1:3){
  for (j in 1:2){
    side_counts[[i]][[j]] <- apply(side_counts[[i]][[j]], MARGIN=1, FUN=sum)
    side_counts[[i]][[j]] <- cbind(side_counts[[i]][[j]])
  }
}
dps <- jags_data_cond[[5]]
prop_stay <- jags_data_cond[[7]]

# Let's update this and run the model with the nested parameterization
file_name = "C:/Users/19708/Desktop/Herring/Modeling/R Multistate CJS/Jags/09-27-jags.bugs"

# source functions and jags model

model <- model(file_name, y, x_data, t_0, side_counts, prop_stay, dps, tl)

# Modify this to pull betapws as well
post_samples <- coda.samples(model,
                             n.burnin = 5000,
                             thin = 25,
                             variable.names=c("S4", "S8",
                                              "psi2","psi3","psi4","psi5","psi6","psi7","psi8",
                                              "betas4","betas8",
                                              "betap2", "betap3", "betap4","betap5","betap6","betap7", "betap8"),
                             n.iter = 50000)


post_samples_nc_1122 <- post_samples

# Save coda samples
save(post_samples_nc_1122, file="C:/Users/19708/Desktop/Herring/Modeling/Data/RData/post-samples/nc-1122.RData")
load("C:/Users/19708/Desktop/Herring/Modeling/Data/RData/post-samples/no-cond.RData")

# Save trace plots
jpeg("C:/Users/19708/Desktop/Herring/Images/traceplots-cond.jpg")
plot(post.samples[[1]])
dev.off()

# Extract the results from the posterior samples samples
library(coda)
load("C:/Users/19708/Desktop/Herring/Modeling/Data/RData/post-samples-cond.RData")
median_estimates_cond <- get.posterior.quantiles(cond_samples,n_x_vals=2, n_seasons=5, quantile=0.5)
lb_cred_int_cond <- get.posterior.quantiles(cond_samples, n_x_vals=2, n_seasons=5, quantile=0.05)
ub_cred_int_cond <- get.posterior.quantiles(cond_samples, n_x_vals=2, n_seasons=5, quantile=0.95)

# Code for model selection
dic.pD <- dic.samples(model, n.iter=200000, thin=25, type = "pD")
dic.popt <- dic.samples(model, n.iter=200000, thin=25, type="popt")