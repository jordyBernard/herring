# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Main Functions
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# Function to clean up transceiver data
clean.transceiver.data <- function(transceiver_data_raw){
  transceiver_data_raw <- as.data.frame(transceiver_data_raw)
  transceiver_data_cleaned <- transceiver_data_raw[,-c(1,2, 6:9, 14:19)]
  names(transceiver_data_cleaned) <- c("Time", "Receiver", "Transmitter", "Region", "Station","Lat","Lon")
  transceiver_data_cleaned$Receiver <- as.factor(transceiver_data_cleaned$Receiver) 
  transceiver_data_cleaned$Transmitter <- as.factor(transceiver_data_cleaned$Transmitter)
  transceiver_data_cleaned$Region<-as.factor(transceiver_data_cleaned$Region)
  transceiver_data_cleaned$Station<- as.factor(transceiver_data_cleaned$Station)
  transceiver_data_cleaned <- na.coord.to.0(transceiver_data_cleaned)
  return(transceiver_data_cleaned)
}

# -------------------------------------------------------------------------
# Function to add the 10-21 transceiver data to the main set
add.transceiver.data <- function(transceiver_data_cleaned, transceiver_data_new){
  transceiver_data_raw <- as.data.frame(transceiver_data_new)
  transceiver_data_new <- transceiver_data_raw[,-c(1,2, 6:9, 14:19)]
  library(plyr)
  # date_char <- ldply(strsplit(as.character(transceiver_data_new[,1]), split=" "))[[1]]
  date_char <- as.character(transceiver_data_new[,1])
  # transceiver_data_new[,1] <- as.POSIXct(date_char, format="%m/%d/%Y", tz="UTC")
  transceiver_data_new[,1] <- as.POSIXct(date_char, format="%m/%d/%Y %H:%M", tz="UTC")
  names(transceiver_data_new) <- c("Time", "Receiver", "Transmitter", "Region", "Station","Lat","Lon")
  return(rbind(transceiver_data_cleaned, transceiver_data_new))
}

# -------------------------------------------------------------------------
# Function to convert VR3-UVM-156 station to MB
adjust.VR3UVM156 <- function(full_transceiver_data){
  for (i in 1:nrow(full_transceiver_data)){
    if (full_transceiver_data$Receiver[i] == "VR3-UVM-156"){
      full_transceiver_data$Station[i] <- "MB"
    }
  }
  return(full_transceiver_data)
}

# -------------------------------------------------------------------------
# Function to clean up tagging data
clean.tagging.data <- function(tagging_data_raw){
  tagging_data_raw <- as.data.frame(tagging_data_raw)
  tagging_data_cleaned <- tagging_data_raw[,c(4,2,7,9,16,17,14,15,18,13)]
  names(tagging_data_cleaned) <- c("Transmitter", "Cohort","TagLife","ReleaseTime","Condition","Sex","Weight","Burden","SpawnedOut","Length")
  sex_vec <- tagging_data_cleaned$Sex
  for(i in 1:length(sex_vec)){
    if (sex_vec[i]=="Unk"){
      sex_vec[i]<-"UNK"
    }
  }
  sex_vec <- as.factor(as.character(sex_vec))
  tagging_data_cleaned$Sex <- sex_vec
  tagging_data_cleaned$Cohort <- as.factor(tagging_data_cleaned$Cohort)
  tagging_data_cleaned$SpawnedOut <- as.factor(tagging_data_cleaned$SpawnedOut)
  tagging_data_cleaned <- bin.condition(tagging_data_cleaned)
  tagging_data_cleaned <- bin.weight(tagging_data_cleaned)
  tagging_data_cleaned <- bin.burden(tagging_data_cleaned)
  tagging_data_cleaned <- bin.length(tagging_data_cleaned)
  return(tagging_data_cleaned)
}


# -------------------------------------------------------------------------
# Function to clean up upload data
clean.upload.data <- function(upload_data){
  upload_data <- na.omit(upload_data)
  deploy_date <- rep(NA, nrow(upload_data))
  upload_date <- rep(NA, nrow(upload_data))
  for(i in 1:nrow(upload_data)){
    deploy_date[i] <- substr(upload_data[i,2], start=1, stop=10)
    upload_date[i] <- substr(upload_data[i,3], start=1, stop=10)
  }
  return(data.frame(station=upload_data$STATION, deployDate=as.POSIXct(deploy_date), uploadDate=as.POSIXct(upload_date)))
}

# -------------------------------------------------------------------------
# Function to bin condition into 2 levels ("good" and "poor")
bin.condition <- function(tagging_data_cleaned){
  condition <- rep(NA, nrow(tagging_data_cleaned))
  condition[tagging_data_cleaned$Condition < median(tagging_data_cleaned$Condition)] <- "poor"
  condition[tagging_data_cleaned$Condition >= median(tagging_data_cleaned$Condition)] <-"good"
  tagging_data_cleaned$Condition <- as.factor(condition)
  return(tagging_data_cleaned)
}

# -------------------------------------------------------------------------
# Function to bin weight into 2 levels ("low" and "high")
bin.weight <- function(tagging_data_cleaned){
  weight <- rep(NA, nrow(tagging_data_cleaned))
  weight[tagging_data_cleaned$Weight < median(tagging_data_cleaned$Weight)] <- "low"
  weight[tagging_data_cleaned$Weight >= median(tagging_data_cleaned$Weight)] <- "high"
  tagging_data_cleaned$Weight <- as.factor(weight)
  return(tagging_data_cleaned)
}

# -------------------------------------------------------------------------
# Function to bin weight into 2 levels ("low" and "high")
bin.length <- function(tagging_data_cleaned){
  length <- rep(NA, nrow(tagging_data_cleaned))
  length[tagging_data_cleaned$Length < median(tagging_data_cleaned$Length)] <- "low"
  length[tagging_data_cleaned$Length >= median(tagging_data_cleaned$Length)] <- "high"
  tagging_data_cleaned$Length <- as.factor(length)
  return(tagging_data_cleaned)
}

# -------------------------------------------------------------------------
# Function to bin tag-burden into 2 tag burden into two levels ("low" and "high") 
bin.burden <- function(tagging_data_cleaned){
  burden <- rep(NA, nrow(tagging_data_cleaned))
  burden[tagging_data_cleaned$Burden < median(tagging_data_cleaned$Burden)] <- "low"
  burden[tagging_data_cleaned$Burden >= median(tagging_data_cleaned$Burden)] <- "high"
  tagging_data_cleaned$Burden <- as.factor(burden)
  return(tagging_data_cleaned)
}

# -------------------------------------------------------------------------
# Function to convert NA coordinates in transceiver data to 0 so that na.rm function can be utilized
na.coord.to.0 <- function(transceiver_data){
  for (i in 1:nrow(transceiver_data)){
    if (is.na(transceiver_data$Lat[i])){
      transceiver_data$Lat[i] <- 0
      transceiver_data$Lon[i] <- 0
    }
  }
  return(transceiver_data)
}

# -------------------------------------------------------------------------
# Function to remove isolated (single) detections
# Check to make sure that na.rm isn't removing anything unnecessarily
filter.single.detections <- function(transceiver_data_cleaned, time="hour"){
  transceiver_data <- transceiver_data_cleaned
  transceiver_data <- na.omit(transceiver_data)
  transceiver_data <- transceiver_data[order(transceiver_data$Time),]
  n<-nrow(transceiver_data)
  keepers <- rep(F, n)
  if (time=="hour"){
    spw <- 1800
  } else if (time=="day"){
    spw <- 43200
  } else if (time=="week"){
    spw <-302400
  }
  names(transceiver_data) <- c("Time", "Receiver", "Transmitter", "Region", "Station","Lat","Lon")
  for (i in 1:(n-1)){
    second_detect <- F
    j<-1
    while (second_detect==F & transceiver_data$Time[i+j]<=(transceiver_data$Time[i]+spw)){
      if (transceiver_data$Transmitter[i]==transceiver_data$Transmitter[i+j] & transceiver_data$Region[i]==transceiver_data$Region[i+j]){
        second_detect <- T
        break
      }
      j<-j+1
      if(i+j > n){
        break
      }
    }
    k<-1
    if (i-k>0 & second_detect==F){
      while (second_detect==F & transceiver_data$Time[i-k]>=(transceiver_data$Time[i]-spw)){
        if (transceiver_data$Transmitter[i]==transceiver_data$Transmitter[i-k] & transceiver_data$Region[i]==transceiver_data$Region[i-k]){
          second_detect <-T
          break
        }
        k<-k+1
        if (i-k < 1){
          break
        }
      }
    }
    keepers[i] <- second_detect
    if((i%%5000) == 0){
      print(round(i/n, 2))
    }
  }
  m <- sum(keepers)-n
  print(paste(m, "fish were detected only one time and moved from the returned data.frame", sep=" "))
  return(transceiver_data[keepers,])
}

# -------------------------------------------------------------------------
# Isolate receiver coordinate data
isolate.receiver.data <- function(transceiver_data_filtered){
  transceiver_data <- transceiver_data_filtered 
  library(dplyr)
  receiver_data <- data.frame(Receiver=transceiver_data$Receiver, Station=transceiver_data$Station, Region=transceiver_data$Region, Lat=transceiver_data$Lat, Lon=transceiver_data$Lon) %>% distinct()
  return(receiver_data)
}

# -------------------------------------------------------------------------
# Adds side information to the main df, removes duplicates so that there is a maximum of one detection for each time, and removes information that is no longer needed such as receiver info
add.side <- function(transceiver_data_filtered, receiver_data){
  library(dplyr)
  n<- nrow(transceiver_data_filtered)
  side <- rep(NA, n)
  for (i in 1:n){
    detection <- transceiver_data_filtered[i,]
    side[i] <- receiver_data$Side[receiver_data$Receiver==detection$Receiver & receiver_data$Station==detection$Station][1]
    if(i%%5000 == 0){
      print(round(i/n, 2))
    }
  }
  transceiver_data_side <- cbind(transceiver_data_filtered, side)
  names(transceiver_data_side)[8] <- "Side"
  transceiver_data_side <- transceiver_data_side[, -c(2,5,6,7)]
  transceiver_data_side$Side <- as.character(transceiver_data_side$Side)
  transceiver_data_side <- distinct(transceiver_data_side)
  n <- nrow(transceiver_data_side)
  side <- rep(NA, n)
  for (i in 1:n){
    detections <- transceiver_data_side[transceiver_data_side$Time == transceiver_data_side$Time[i] & transceiver_data_side$Transmitter == transceiver_data_side$Transmitter[i], ]
    detection_sides <- detections$Side
    if (length(unique(detection_sides)) == 1){
      side[i] <- detection_sides[1]
    }else{
      side[i] <- "both"
    }
    if(i%%5000 == 0){
      print(round(i/n, 2))
    }
  }
  transceiver_data_side$Side <- side
  transceiver_data_side$Side <- as.factor(transceiver_data_side$Side)
  return(transceiver_data_side)
}

# -------------------------------------------------------------------------
# Function to determine the observed data associated with each detection following the encoding below:
# 1 -- No detection
# 2 -- Gravina, RedGravina, or Hawkins detection
# 3 -- NoMontague or Mobile detection
# 4 -- Hinchinbrook detection
# 5 -- Montague detection
# 6 -- Latouche, Elrington, Prince of Wales or Bainbridge detection
add.observation.encoding <- function(transceiver_data_side){
  n <- nrow(transceiver_data_side)
  obs.enc <- rep(NA, n)
  for (i in 1:n){
    detection <- transceiver_data_side[i,]
    if (detection$Region == "Gravina" || detection$Region == "RedGravina" || detection$Region == "Hawkins"){
      obs.enc[i] <- 2
    }else if (detection$Region == "NoMontague" || detection$Region == "Mobile" || detection$Region == "Glacier" || detection$Region == "KnightIP"){
      obs.enc[i] <- 3
    }else if (detection$Region == "Hinchinbrook"){
      obs.enc[i] <- 4
    }else if (detection$Region == "Montague"){
      obs.enc[i] <- 5
    }else if (detection$Region == "LaTouche" || detection$Region == "Elrington" || detection$Region == "Prince of Wales" || detection$Region == "Bainbridge"){
      obs.enc[i] <- 6
    }
    if(i%%5000 == 0){
      print(round(i/n, 2))
    }
  }
  obs_data <- cbind(transceiver_data_side[,c(1,2,4)], obs.enc)
  names(obs_data)[4] <- "Obs" 
  return(obs_data)
}


# -------------------------------------------------------------------------
# Funtion to return the time when the receiver data was uploaded last
# Modify this function before running the models for real
get.end.time <- function(upload_data_cleaned){
  return(max(upload_data_cleaned$uploadDate))
}

# -------------------------------------------------------------------------
# Function to return the time the first fish was released
get.start.time <- function(tagging_data_cleaned){
  return(min(tagging_data_cleaned$ReleaseTime))
}

# -------------------------------------------------------------------------
# Compute the detection history for each individual
get.detecton.history.list <- function(transceiver_data_encoded, start_time, end_time){
  get.day <- function(time, start_time){
    day <- rep(NA, length(time))
    for (i in 1:length(time)){
      day[i] <- as.numeric(ceiling(difftime(time[i], start_time, units="days")))
      if (day[i] == 0){
        day[i] <- 1
      }
    }
    return(day)
  }
  num_days <- get.day(end_time, start_time)
  days <- get.day(transceiver_data_encoded$Time, start_time)
  detection_histories <- list()
  for (j in 1:length(levels(transceiver_data_encoded$Transmitter))){
    fish <- levels(transceiver_data_encoded$Transmitter)[j]
    obs <- rep(NA, num_days)
    direction <- rep(NA, num_days)
    for (day in 1:num_days){
      detections <- transceiver_data_encoded[transceiver_data_encoded$Transmitter==fish & days == day, ]
      if (nrow(detections)==0){
        obs[day] <- 1
      } else if (length(unique(detections$Obs))==1){
        obs[day] <- detections$Obs[1]
        if (length(unique(detections$Side)) != 1){
          first_side <- detections$Side[1]
          last_side <- detections$Side[nrow(detections)]
          if (first_side == "in" & last_side == "out"){
            direction[day] <- "io"
          } else if (first_side=="out" & last_side == "in"){
            direction[day] <- "oi"
          }
        }
      } else{
        obs[day] <- detections$Obs[1]
        print(paste("Fish", levels(transceiver_data_encoded$Transmitter)[j], "detected in different regions on day", day, sep=" "))
        print("using the first region where the fish was detected")
      }
    }
    detection_histories[[j]] <- data.frame(obs, direction)
    if(j%%10 == 0){
      print(round(j/length(levels(transceiver_data_encoded$Transmitter)), 2))
    }
  }
  names(detection_histories) <- levels(transceiver_data_encoded$Transmitter)
  return(detection_histories)
}

# -------------------------------------------------------------------------
# Function to convert the detection history infomation into the appropriate structure for the Jags model
get.y <- function(detection_history_list){
  y <- matrix(NA, nrow=length(detection_history_list), ncol=nrow(detection_history_list[[1]]))
  row.names(y) <- names(detection_history_list)
  for (i in 1:length(detection_history_list)){
    y[i,] <- detection_history_list[[i]][,1]
  }
  row.names(y) <- names(detection_history_list)
  return(y)
}

# -------------------------------------------------------------------------
# Function to determine the number of days in each season
get.days.per.season <- function(date_vector){
  new_date_vector <- as.POSIXct(date_vector)
  days <- rep(NA, (length(date_vector)-1))
  for (i in 1:(length(date_vector)-1)){
    days[i] <- as.numeric(round(difftime(date_vector[[i+1]], date_vector[[i]], units="days")))
  }
  return(days)
}

# -------------------------------------------------------------------------
# Function to extract io/oi counts at the entrance arrays
get.side.counts <- function(days_per_season, detection_history_list, by, tagging_data_cleaned, entrance){
  if (by=="length"){
    index <- 10
  }else if (by =="condition"){
    index <- 5
  }else if (by == "sex"){
    index <- 6
  }else if (by == "weight"){
    index <- 7
  }else if (by == "burden"){
    index <- 8
  }else if (by == "spawn"){
    index <- 9
  }
  io_mat <- matrix(0, nrow=length(days_per_season), ncol=length(levels(tagging_data_cleaned[,index])))
  oi_mat <- matrix(0, nrow=length(days_per_season), ncol=length(levels(tagging_data_cleaned[,index])))
  for(k in 1:length(levels(tagging_data_cleaned[,index]))){
    fish_ids <- tagging_data_cleaned$Transmitter[tagging_data_cleaned[,index]==levels(tagging_data_cleaned[,index])[k]]
    for (i in 1:length(detection_history_list)){
      if (is.element(names(detection_history_list)[i], tagging_data_cleaned$Transmitter)){
        if (is.element(names(detection_history_list)[i], fish_ids)){
          lb <- 1
          ub <- days_per_season[1]
          for (j in 1:length(days_per_season)){
            io_mat[j,k] <- io_mat[j,k] + sum(na.omit(detection_history_list[[i]][lb:ub,2]=="io" & detection_history_list[[i]][lb:ub,1]==entrance))
            oi_mat[j,k] <- oi_mat[j,k] + sum(na.omit(detection_history_list[[i]][lb:ub,2]=="oi" & detection_history_list[[i]][lb:ub,1]==entrance))
            lb <- lb + days_per_season[j]
            if (j != length(days_per_season)){
              ub <- ub + days_per_season[j+1]
            }
          }
        }
      }
    }
  }
  side_counts <- list("io"=io_mat,"oi"=oi_mat)
  for(i in 1:2){
    side_counts[[i]]<-side_counts[[i]]+1
  }
  return(side_counts)
}

# -------------------------------------------------------------------------
# Function to run the code with no constraint
side.count.no.cons <- function(side_counts){
  for(i in 1:length(side_counts)){
    side_counts[[i]]$io <- data.frame(apply(side_counts[[1]]$io, FUN=sum, MARGIN = 1)) 
    side_counts[[i]]$oi <- data.frame(apply(side_counts[[1]]$oi, FUN=sum, MARGIN = 1)) 
  }
  return(side_counts)
}

# -------------------------------------------------------------------------
# Function to estimate the proportion of fish that stay at each of the entrance arrays between consecutive time periods for each season
get.prop.stay <- function(y, t_0, tl){
  sh <- 0
  gh <- 0
  sm <- 0
  gm <- 0
  ss <- 0
  gs <- 0
  for (i in 1:nrow(y)){
    for (j in t_0[i]:(min(t_0[i]+tl[i], ncol(y))-1)){
      if (y[i,j]==4 & y[i,j+1]==4){
        sh <- sh + 1
      }else if (y[i,j]==4 & y[i,j+1]!=4){
        gh <- gh + 1
      }else if (y[i,j]==5 & y[i,j+1]==5){
        sm <- sm + 1
      }else if (y[i,j]==5 & y[i,j+1]!=5){
        gm <- gm + 1
      }else if (y[i,j]==6 & y[i,j+1]==6){
        ss <- ss + 1
      }else if (y[i,j]==6 & y[i,j+1]!=6){
        gs <- gs +1
      }
    }
  }
  prop_stay <- c(sh/(sh+gh), sm/(sm+gm), ss/(ss+gs))
  names(prop_stay) <- c("h", "m", "s")
  return(prop_stay)
}

# -------------------------------------------------------------------------
# Function to estimate stay counts for the informative dirichlet priors at the entrance arrays 
get.est.stay <- function(side_counts, prop_stay){
  num_go <- side_counts$io+side_counts$oi
  r <- prop_stay/(1-prop_stay)
  num_stay <- floor(r*num_go)
  return(num_stay)
}

# -------------------------------------------------------------------------
# Function to gather and supply the relevant information to the Jags model for the informative priors at the entrance arrays
prep.entrance.inits <- function(side_counts, prop_stay){
  stay_counts <- get.est.stay(side_counts, prop_stay)
  ent_inits <- array(NA, dim=c(3,nrow(side_counts$io),ncol(side_counts$io)))
  ent_inits[1,,] <- side_counts$oi
  ent_inits[2,,] <- stay_counts
  ent_inits[3,,] <- side_counts$io
  return(ent_inits)
}

# -------------------------------------------------------------------------
# Function to determine the fish that were never detected
find.missing.fish <- function(detection_history_list, tagging_data_cleaned){
  missing_fish <- rep(NA, length(tagging_data_cleaned$Transmitter)-length(names(detection_history_list)))
  j<-1
  for(i in 1:length(tagging_data_cleaned$Transmitter)){
    fish <- tagging_data_cleaned$Transmitter[i]
    if (!is.element(fish, names(detection_history_list))){
      missing_fish[j] <- fish
      j<-j+1
    }
  }
  return(missing_fish)
}

# -------------------------------------------------------------------------
# Function to supply detection histories for the fish that were never heard from again
add.fish.to.y <- function(id, y){
  dh <- rep(1, ncol(y))
  new_y <- rbind(y, dh)
  rownames(new_y)[nrow(new_y)]<-id
  return(new_y)
}

# -------------------------------------------------------------------------
# Function to extract condition information for Jags model. 1 -- good; 2 -- poor
get.condition <- function(y, tagging_data_cleaned){
  condition <- rep(NA, nrow(y))
  for (i in 1:length(rownames(y))){
    fish <- rownames(y)[i]
    condition[i] <- tagging_data_cleaned$Condition[tagging_data_cleaned$Transmitter==fish]
  }
  return(as.factor(condition))
}

# -------------------------------------------------------------------------
# Functions to extract sex information for Jags model. 1 -- Female; 2 -- Male; 3 -- Unknown
get.sex <- function(y, tagging_data_cleaned){
  sex <- rep(NA, nrow(y))
  for (i in 1:length(rownames(y))){
    fish <- rownames(y)[i]
    sex[i] <- as.character(tagging_data_cleaned$Sex[tagging_data_cleaned$Transmitter==fish])
  }
  return(as.factor(sex))
}

# -------------------------------------------------------------------------
# For length
get.length <- function(y, tagging_data_cleaned){
  length <- rep(NA, nrow(y))
  for (i in 1:length(rownames(y))){
    fish <- rownames(y)[i]
    length[i] <- as.character(tagging_data_cleaned$Length[tagging_data_cleaned$Transmitter==fish])
  }
  return(as.factor(length))
}

# -------------------------------------------------------------------------
# For weight
get.weight <- function(y, tagging_data_cleaned){
  weight <- rep(NA, nrow(y))
  for (i in 1:length(rownames(y))){
    fish <- rownames(y)[i]
    weight[i] <- as.character(tagging_data_cleaned$Weight[tagging_data_cleaned$Transmitter==fish])
  }
  return(as.factor(weight))
}

# -------------------------------------------------------------------------
# For tag-burden
get.burden <- function(y, tagging_data_cleaned){
  burden <- rep(NA, nrow(y))
  for (i in 1:length(rownames(y))){
    fish <- rownames(y)[i]
    burden[i] <- as.character(tagging_data_cleaned$Burden[tagging_data_cleaned$Transmitter==fish])
  }
  return(as.factor(burden))
}

# -------------------------------------------------------------------------
# For spawned state
get.spawned.out <- function(y, tagging_data_cleaned){
  spawnedOut <- rep(NA, nrow(y))
  for (i in 1:length(rownames(y))){
    fish <- rownames(y)[i]
    spawnedOut[i] <- as.character(tagging_data_cleaned$SpawnedOut[tagging_data_cleaned$Transmitter==fish])
  }
  return(as.factor(spawnedOut))
}

# -------------------------------------------------------------------------
# Function to determine the day when a fish was tagged
get.tagging.time <- function(y, tagging_data_cleaned){
  tt <- rep(NA, nrow(tagging_data_cleaned)) 
  first_tt <- min(tagging_data_cleaned$ReleaseTime)
  for(i in 1:nrow(tagging_data_cleaned)){
    tt[i] <- as.numeric(round(difftime(tagging_data_cleaned$ReleaseTime[i], first_tt, units="days")))
  }
  names(tt)<-tagging_data_cleaned$Transmitter
  new_tt <- rep(NA, length(tt))
  for (i in 1:length(rownames(y))){
    fish <- rownames(y)[i]
    new_tt[i] <- tt[names(tt)==fish]
  }
  names(new_tt) <- row.names(y)
  return(new_tt)
}

# -------------------------------------------------------------------------
# Function extract, clean, organize, and return taglife vector for Jags Model
get.tag.life <- function(y, tagging_data_cleaned){
  tl <- tagging_data_cleaned$TagLife
  # Tags with unknown Taglife are active regardless of taglife at time of analysis, I'm giving them an arbitrary taglife of 246 days
  unk_tags <- tagging_data_cleaned$Transmitter[tl=="UNK"]
  tagging_data_cleaned[is.element(tagging_data_cleaned$Transmitter,unk_tags),]
  for (i in 1:length(tl)){
    if (tl[i]=="UNK"){
      tl[i]<-"246"
    }
  }
  tl <- as.numeric(tl)
  names(tl)<-tagging_data_cleaned$Transmitter
  new_tl <- rep(NA, length(tl))
  for (i in 1:length(rownames(y))){
    fish <- rownames(y)[i]
    new_tl[i] <- tl[names(tl)==fish]
  }
  names(new_tl) <- row.names(y)
  return(new_tl)
}

# -------------------------------------------------------------------------
# Function to determine the season that cooresponding to each day
get.season <- function(dps){
  seasons <- c()
  for (i in 1:length(dps)){
    seasons <- c(seasons, rep(i,dps[i])) 
  }
  return(seasons)
}

# -------------------------------------------------------------------------
# Functions to compress detection histories for Jags model
compress.y <- function(y, bin_by){
  num_col <- floor(ncol(y)/bin_by)
  new_y <- matrix(1, nrow=nrow(y), ncol=num_col)
  for (r in 1:nrow(new_y)){
    for (i in 1:num_col){
      start_index <- (i-1)*bin_by
      for(j in 1:bin_by){
        dat <- y[r,start_index+j]
        if (dat == 2){
          new_y[r,i] <- 2
        }else if (dat == 3){
          new_y[r,i] <- 3
        }else if (dat == 4){
          new_y[r,i] <- 4
        }else if (dat == 5){
          new_y[r,i] <- 5
        }else if (dat == 6){
          new_y[r,i] <- 6
        }
      }
    }
  }
  row.names(new_y) <- row.names(y)
  return(new_y)
}

# -------------------------------------------------------------------------
# Function to check for inconsistent patterns in the detection histories
get.incon.detect <- function(y){
  num.incon <- 0
  for (i in 1:nrow(y)){
    for(j in 1:(ncol(y)-1)){
      if ((y[i,j] == 2 || y[i,j] == 3 || y[i,j] == 4 || y[i,j] == 5 || y[i,j] == 6) &
          (y[i,j+1] == 2 || y[i,j+1] == 3 || y[i,j+1] == 4 || y[i,j+1] == 5 || y[i,j+1] == 6) &
          (y[i,j]!=y[i,j+1])){
        num.incon <- num.incon + 1
      }
    }
  }
  incon <- rep(NA, num.incon)
  k <- 1
  for (i in 1:nrow(y)){
    for(j in 1:(ncol(y)-1)){
      if ((y[i,j] == 2 || y[i,j] == 3 || y[i,j] == 4 || y[i,j] == 5 || y[i,j] == 6) &
          (y[i,j+1] == 2 || y[i,j+1] == 3 || y[i,j+1] == 4 || y[i,j+1] == 5 || y[i,j+1] == 6) &
          (y[i,j]!=y[i,j+1])){
        incon[k] <- i
        k <- k+1
      }
    }
  }
  return(incon)
}

# -------------------------------------------------------------------------
# Function to remove the inconsistencies
remove.incon.detect <- function(y){
  incon_detect <- get.incon.detect(y)
  for(i in incon_detect){
    for (j in 1:(ncol(y)-1)){
      if (is.element(y[i,j], c(2,3,4,5,6)) & is.element(y[i,j+1],c(2,3,4,5,6)) & y[i,j]!=y[i,j+1]){
        y[i,j+1]<-1
      }
    }
  }
  return(y)
}


# -------------------------------------------------------------------------
# Function to adjust the tagging times to match compressed detection histories
adjust.t_0 <- function(t_0, bin_by){
  new_t_0 <- ceiling(t_0/bin_by)
  for(i in 1:length(new_t_0)){
    if (new_t_0[i]==0){
      new_t_0[i] <- 1
    }
  }
  return(new_t_0)
}


# -------------------------------------------------------------------------
# Function to adjust dps vector to match compressed detection histories
adjust.dps <- function(dps, bin_by){
  tps <- rep(NA, length(dps))
  for(i in 1:length(dps)){
    cum_days <- sum(dps[1:i])
    tps[i] <- floor(cum_days/bin_by)
  }
  for(i in 2:length(dps)){
    tps[i] <- tps[i]-sum(tps[1:i-1])
  }
  return(tps)
}


# -------------------------------------------------------------------------
# Function to adjust the tag-life vector to match compressed detection histories
adjust.tl <- function(tl, bin_by){
  new_tl <- floor(tl/bin_by)
  return(new_tl)
}

# -------------------------------------------------------------------------
# Function to initialize hidden state vector for Jags model
get.hidden.inits <- function(y, t_0, tl){
  z <- matrix(NA, nrow=nrow(y), ncol=ncol(y))
  for (i in 1:nrow(z)){
    last_det <- "p"
    state <- "pws"
    for (j in 1:(ncol(z)-1)){
      if (is.element(y[i,j], c(2,3))){
        z[i,j] <- y[i,j]
        last_det <- "p"
      }else if (is.element(y[i,j], c(4,5,6))){
        z[i,j] <- y[i,j]+1
        last_det <- "e"
        if (state == "pws" & y[i,j]!=y[i,j+1]){
          state <- "goa"
        }else if (state == "goa" & y[i,j]!=y[i,j+1]){
          state <- "pws"
        }
      }else if (y[i,j]==1){
        if (last_det == "p"){
          z[i,j] <- 4
        }else if (last_det =="e"){
          if (state=="pws"){
            z[i,j] <- 4
          }else if (state=="goa")
            z[i,j] <- 8
        }
      }
    }
    j <- ncol(y)
    if (is.element(y[i,j], c(2,3))){
      z[i,j] <- y[i,j]
    } else if (is.element(y[i,j], c(4,5,6))){
      z[i,j] <- y[i,j]+1
    } else{
      z[i,j] <- z[i, j-1]
    }
  }
  for(i in 1:nrow(z)){
    for(j in ncol(z):2){
      if (z[i,j]==2 & z[i, j-1]==8){
        k <- j-1
        while (z[i,k]==8){
          z[i,k] <- 4
          k <- k-1
        }
      }
    }
  }
  for(i in 1:nrow(z)){
    for(j in ncol(z):2){
      if (z[i,j]==3 & z[i, j-1]==8){
        k <- j-1
        while (z[i,k]==8){
          z[i,k] <- 4
          k <- k-1
        }
      }
    }
  }
  for(i in 1:nrow(z)){
    for(j in 1:ncol(z)){
      if (j < t_0[i]){
        z[i,j] <- NA
      }else if (j > t_0[i]+tl[i]-1){
        z[i,j] <- NA
      }
    }
  }
  return(z)
}

# -------------------------------------------------------------------------
# Code to put a little patch on y
patch.y <- function(y, t_0, tl){
  z <- get.hidden.inits(y,t_0,tl)
  for (i in 1:nrow(z)){
    j <- 1
    while (j<=ncol(z) & is.na(z[i,j])){
      j <- j+1
    }
    if (z[i,j]==3 || z[i,j]==5){
      print(i)
      y[i,j] <- 1
    }
  }
  return(y)
} 

# -------------------------------------------------------------------------
# Function to gather and return a list of the information needed in the Jags model
gather.data <- function(y, x_data, t_0, side_counts, prop_stay, dps, tl){
  side_counts_h <- side_counts[[1]]
  side_counts_m <- side_counts[[2]]
  side_counts_s <- side_counts[[3]]
  prop_stay_h <- prop_stay[1]
  prop_stay_m <- prop_stay[2]
  prop_stay_s <- prop_stay[3]
  data <- list(y_data = y, 
               x_data = x_data,
               t_0 = t_0,
               M = nrow(y),
               n_seasons = length(dps),
               n_x_vals = length(levels(x_data)),
               N = sum(dps),
               tl=tl,
               season=get.season(dps),
               h_data = prep.entrance.inits(side_counts_h, prop_stay_h),
               m_data = prep.entrance.inits(side_counts_m, prop_stay_m),
               s_data = prep.entrance.inits(side_counts_s, prop_stay_s))
  return(data)
}

# -------------------------------------------------------------------------
# Function to pick sensible initial values for the Jags model
inits <- function(y, dps, x_data, t_0, tl){
  library(rBeta2009)
  n_seasons <- length(dps)
  n_x_vals <- length(levels(x_data))
  S4 <- matrix(NA, n_seasons, n_x_vals)
  S8 <- matrix(NA, n_seasons, n_x_vals)
  psi2 <- array(NA, dim=c(2,n_seasons, n_x_vals))
  psi3 <- array(NA, dim=c(2,n_seasons, n_x_vals))
  psi4 <- array(NA, dim=c(6,n_seasons, n_x_vals)) 
  psi5 <- array(NA, dim=c(3,n_seasons, n_x_vals))
  psi6 <- array(NA, dim=c(3,n_seasons, n_x_vals))
  psi7 <- array(NA, dim=c(3,n_seasons, n_x_vals))
  psi8 <- array(NA, dim=c(4,n_seasons, n_x_vals))
  for (s in 1:n_seasons){
    for (x in 1:n_x_vals){
      S4[s,x] <- rbeta(1,1,1)
      S8[s,x] <- rbeta(1,1,1)
      psi2[1,s,x] <- rbeta(1,1,1)
      psi2[2,s,x] <- 1-psi2[1,s,x]
      psi3[1,s,x] <- rbeta(1,1,1)
      psi3[2,s,x] <- 1-psi3[1,s,x]
      psi4[,s,x] <- rdirichlet(1,c(2,2,2,2,2,2))
      psi5[,s,x] <- rdirichlet(1,c(2,2,2))
      psi6[,s,x] <- rdirichlet(1,c(2,2,2))
      psi7[,s,x] <- rdirichlet(1,c(2,2,2))
      psi8[,s,x] <- rdirichlet(1,c(2,2,2,2))
    }
  }
  z <- get.hidden.inits(y,t_0,tl)
  return(list(S4=S4,
              S8=S8,
              psi2=psi2,
              psi3=psi3,
              psi4=psi4,
              psi5=psi5,
              psi6=psi6,
              psi7=psi7,
              psi8=psi8,
              z=z
  ))
}

# -------------------------------------------------------------------------
# Function to initialize the Jags model
model <- function(file_path, y, x_data, t_0, side_counts, prop_stay, dps, tl){
  library(rjags)
  model = jags.model(file_path,
                     data=gather.data(y, x_data, t_0, side_counts, prop_stay, dps, tl),
                     inits=inits(y, dps, x_data, t_0, tl),
                     n.chains = 2,
                     n.adapt = 20000)
}


# -------------------------------------------------------------------------
# Function to pull median estimates and lower and upper bounds of credible interval from posterior samples
get.posterior.quantiles <- function(post_samples, n_x_vals, n_seasons, quantile){
  survival_pws <- matrix(NA, nrow=n_seasons, ncol=n_x_vals)
  survival_goa <- matrix(NA, nrow=n_seasons, ncol=n_x_vals)
  movement_spawning <- array(NA, dim=c(2,n_seasons, n_x_vals))
  movement_other <- array(NA, dim=c(2,n_seasons,n_x_vals))
  movement_pws <- array(NA, dim=c(6, n_seasons, n_x_vals))
  movement_hinch <-array(NA, dim=c(3, n_seasons, n_x_vals))
  movement_mont <-array(NA, dim=c(3, n_seasons, n_x_vals))
  movement_sw <-array(NA, dim=c(3, n_seasons, n_x_vals))
  movement_goa <-array(NA, dim=c(4, n_seasons, n_x_vals))
  movement_pe <- matrix(NA, nrow=n_seasons, ncol=n_x_vals) 
  movement_ge <- matrix(NA, nrow=n_seasons, ncol=n_x_vals) 
  beta_movement_spawning <- array(NA, dim=c(2,n_seasons, n_x_vals))
  beta_movement_other <- array(NA, dim=c(2,n_seasons,n_x_vals))
  beta_movement_pws <- array(NA, dim=c(6, n_seasons, n_x_vals))
  beta_movement_hinch <- array(NA, dim=c(3, n_seasons, n_x_vals))
  beta_movement_mont <- array(NA, dim=c(3, n_seasons, n_x_vals))
  beta_movement_sw <- array(NA,dim=c(3, n_seasons, n_x_vals))
  beta_movement_goa <- array(NA, dim=c(4, n_seasons, n_x_vals))
  beta_survival_pws <- matrix(NA, nrow=n_seasons, ncol=n_x_vals)
  beta_survival_goa <- matrix(NA, nrow=n_seasons, ncol=n_x_vals)
  beta_pe <- matrix(NA, nrow=n_seasons, ncol=n_x_vals) 
  beta_ge <- matrix(NA, nrow=n_seasons, ncol=n_x_vals) 
  k <-1
  for(x in 1:n_x_vals){
    for(s in 1: n_seasons){
      survival_pws[s,x] <- summary(post_samples[[1]], quantiles=quantile)[[2]][k]
      k <- k+1
    }
  }
  for(x in 1:n_x_vals){
    for(s in 1: n_seasons){
      survival_goa[s,x] <- summary(post_samples[[1]], quantiles=quantile)[[2]][k]
      k <- k+1
    }
  }
  for(x in 1:n_x_vals){
    for(s in 1: n_seasons){
      for(i in 1:2){
        beta_movement_spawning[i, s, x] <- summary(post_samples[[1]], quantiles=quantile)[[2]][k]
        k <- k+1
      }
    }
  }
  for(x in 1:n_x_vals){
    for(s in 1: n_seasons){
      for(i in 1:2){
        beta_movement_other[i, s, x] <- summary(post_samples[[1]], quantiles=quantile)[[2]][k]
        k <- k+1
      }
    }
  }
  for(x in 1:n_x_vals){
    for(s in 1: n_seasons){
      for(i in 1:6){
        beta_movement_pws[i,s,x] <- summary(post_samples[[1]], quantiles=quantile)[[2]][k]
        k <- k+1
      }
    }
  }
  for(x in 1:n_x_vals){
    for(s in 1: n_seasons){
      for(i in 1:3){
        beta_movement_hinch[i,s,x] <- summary(post_samples[[1]], quantiles=quantile)[[2]][k]
        k <- k+1
      }
    }
  }
  for(x in 1:n_x_vals){
    for(s in 1: n_seasons){
      for(i in 1:3){
        beta_movement_mont[i,s,x] <- summary(post_samples[[1]], quantiles=quantile)[[2]][k]
        k <- k+1
      }
    }
  }
  for(x in 1:n_x_vals){
    for(s in 1: n_seasons){
      for(i in 1:3){
        beta_movement_sw[i,s,x] <- summary(post_samples[[1]], quantiles=quantile)[[2]][k]
        k <- k+1
      }
    }
  }
  for(x in 1:n_x_vals){
    for(s in 1: n_seasons){
      for(i in 1:4){
        beta_movement_goa[i,s,x] <- summary(post_samples[[1]], quantiles=quantile)[[2]][k]
        k <- k+1
      }
    }
  }
  for(x in 1:n_x_vals){
    for(s in 1: n_seasons){
      beta_survival_pws[s,x] <- summary(post_samples[[1]], quantiles=quantile)[[2]][k]
      k <- k+1
    }
  }
  for(x in 1:n_x_vals){
    for(s in 1: n_seasons){
      beta_survival_goa[s,x] <- summary(post_samples[[1]], quantiles=quantile)[[2]][k]
      k <- k+1
    }
  }
  for(x in 1:n_x_vals){
    for(s in 1: n_seasons){
      for(i in 1:2){
        movement_spawning[i, s, x] <- summary(post_samples[[1]], quantiles=quantile)[[2]][k]
        k <- k+1
      }
    }
  }
  for(x in 1:n_x_vals){
    for(s in 1: n_seasons){
      for(i in 1:2){
        movement_other[i, s, x] <- summary(post_samples[[1]], quantiles=quantile)[[2]][k]
        k <- k+1
      }
    }
  }
  for(x in 1:n_x_vals){
    for(s in 1: n_seasons){
      for(i in 1:6){
        movement_pws[i,s,x] <- summary(post_samples[[1]], quantiles=quantile)[[2]][k]
        k <- k+1
      }
    }
  }
  for(x in 1:n_x_vals){
    for(s in 1: n_seasons){
      for(i in 1:3){
        movement_hinch[i,s,x] <- summary(post_samples[[1]], quantiles=quantile)[[2]][k]
        k <- k+1
      }
    }
  }
  for(x in 1:n_x_vals){
    for(s in 1: n_seasons){
      for(i in 1:3){
        movement_mont[i,s,x] <- summary(post_samples[[1]], quantiles=quantile)[[2]][k]
        k <- k+1
      }
    }
  }
  for(x in 1:n_x_vals){
    for(s in 1: n_seasons){
      for(i in 1:3){
        movement_sw[i,s,x] <- summary(post_samples[[1]], quantiles=quantile)[[2]][k]
        k <- k+1
      }
    }
  }
  for(x in 1:n_x_vals){
    for(s in 1: n_seasons){
      for(i in 1:4){
        movement_goa[i,s,x] <- summary(post_samples[[1]], quantiles=quantile)[[2]][k]
        k <- k+1
      }
    }
  }
  for(x in 1:n_x_vals){
    for(s in 1: n_seasons){
      movement_pe[s,x]
      k <- k+1
    }
  }
  for(x in 1:n_x_vals){
    for(s in 1: n_seasons){
      movement_ge[s,x]
      k <- k+1
    }
  }
  for(x in 1:n_x_vals){
    for(s in 1: n_seasons){
      beta_pe[s,x]
      k <- k+1
    }
  }
  for(x in 1:n_x_vals){
    for(s in 1: n_seasons){
      beta_ge[s,x]
      k <- k+1
    }
  }
  return(list("survivalPWS" = survival_pws, "betaSurvivalPWS" = beta_survival_pws,
              "survivalGOA" = survival_goa, "betaSurvival" = beta_survival_goa,
              "movementSpawning" = movement_spawning, "betaMovementSpawning" = beta_movement_spawning,
              "movementOther" = movement_other, "betaMovementOther" = beta_movement_other,
              "movementPWS" = movement_pws, "betaMovementPWS" = beta_movement_pws,
              "movementHinch" = movement_hinch, "betaMovementHinch" = beta_movement_hinch,
              "movementMont" = movement_mont, "betaMovementMont" = beta_movement_mont,
              "movementSW" = movement_sw, "betaMovementSW" = beta_movement_sw,
              "movementGOA" = movement_goa, "betaMovementGOA" = beta_movement_goa,
              "movementPe" = movement_pe, "betaPe" = beta_pe,
              "movementGe" = movement_ge, "betaGE" = beta_ge))
}




# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Functions used in diagnostics
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Function that returns passage times at a given receiver, station, or region and a histogram of the return times. Additional arguments allow for the specification of the calendar year, transmitter id, and histogram breaks points
plot.detection.times <- function(transceiver_data_filtered, 
                                 year="unspecified", 
                                 receiver="unspecified", 
                                 station="unspecified", 
                                 region="unspecified", 
                                 transmitter="unspecified", 
                                 breaks="days"){
  
  plot_data <- transceiver_data_filtered
  if (year!="unspecified"){
    plot_data <- plot_data[get.year(plot_data$Time)==year, ]
  }
  if (receiver!="unspecified"){
    plot_data <- plot_data[plot_data$Receiver==receiver, ]
  }
  if (station!="unspecified"){
    plot_data <- plot_data[plot_data$Station==station, ]
  }
  if(region!="unspecified"){
    plot_data <- plot_data[plot_data$Region==region, ]
  }
  if(transmitter!="unspecified"){
    plot_data <- plot_data[plot_data$Transmitter==transmitter, ]
  }
  if (nrow(plot_data) == 0){
    print("there were no detections")
  }else{
    detect_times <- plot_data$Time
    hist(detect_times, breaks)
    return(detect_times)
  }
}

# -------------------------------------------------------------------------
# Function to construct contingency table of the fish detected in a region by sex/condition
# May want to modify this so that there is just a "good" and "poor" condition
get.contingency.table <- function(transceiver_data_filtered, tagging_data_cleaned, region="Gravina", year=2017, sex=T, condition=F){
  detections <- transceiver_data_filtered[(transceiver_data_filtered$Region==region & get.year(transceiver_data_filtered$Time)==year), ]
  fish_detected <- levels(detections$Transmitter)
  if (sex == T & condition == F){
    n_male <- 0
    n_female <- 0
    n_unknown <- 0
    for (fish in fish_detected){
      fish_sex <- tagging_data_cleaned$Sex[tagging_data_cleaned$Transmitter == fish]
      if (length(fish_sex != 0)){
        if (fish_sex == "M"){
          n_male <- n_male + 1
        } else if (fish_sex =="F"){
          n_female <- n_female <- n_female + 1
        } else if (fish_sex == "UNK"){
          n_unknown <- n_unknown + 1
        }
      }
    }
    cont_table <- c(n_male, n_female, n_unknown)
    names(cont_table) <- c("Male", "Female", "Unknown")
    return(cont_table)
  }else if(sex == F & condition==T){
    tagging_data_cleaned <- tagging_data_cleaned[order(tagging_data_cleaned$Condition),]
    l_break <- tagging_data_cleaned$Condition[floor(nrow(tagging_data_cleaned)/3)]
    u_break <- tagging_data_cleaned$Condition[nrow(tagging_data_cleaned)-ceiling(nrow(tagging_data_cleaned)/3)]
    n_bad <- 0
    n_ok <- 0
    n_good <- 0
    for (fish in fish_detected){
      fish_condition <- tagging_data_cleaned$Condition[tagging_data_cleaned$Transmitter == fish]
      if (length(fish_condition != 0)){
        if (fish_condition <= l_break){
          n_bad <- n_bad + 1 
        }else if (fish_condition > l_break & fish_condition < u_break){
          n_ok <- n_ok + 1
        }else if (fish_condition >= u_break){
          n_good <- n_good + 1
        }
      }
    }
    cont_table <- c(n_bad, n_ok, n_good)
    names(cont_table) <- c("Bad", "OK", "Good")
    return(cont_table)
  }else if (sex == T & condition == T){
    tagging_data_cleaned <- tagging_data_cleaned[order(tagging_data_cleaned$Condition),]
    l_break <- tagging_data_cleaned$Condition[floor(nrow(tagging_data_cleaned)/3)]
    u_break <- tagging_data_cleaned$Condition[nrow(tagging_data_cleaned)-ceiling(nrow(tagging_data_cleaned)/3)]
    n_male_bad <- 0
    n_male_ok <- 0
    n_male_good <- 0
    n_female_bad <- 0
    n_female_ok <- 0
    n_female_good <- 0
    n_unknown_bad <- 0
    n_unknown_ok <- 0
    n_unknown_good <- 0
    for (fish in fish_detected){
      fish_sex <- tagging_data_cleaned$Sex[tagging_data_cleaned$Transmitter == fish]
      fish_condition <- tagging_data_cleaned$Condition[tagging_data_cleaned$Transmitter == fish]
      if (length(fish_sex != 0)){
        if (fish_condition <= l_break){
          if (fish_sex == "M"){
            n_male_bad <- n_male_bad + 1
          }else if (fish_sex == "F"){
            n_female_bad <- n_female_bad + 1
          }else if (fish_sex == "UNK"){
            n_unknown_bad <- n_unknown_bad + 1
          }
        }else if (fish_condition > l_break & fish_condition < u_break){
          if (fish_sex == "M"){
            n_male_ok <- n_male_ok + 1
          }else if (fish_sex == "F"){
            n_female_ok <- n_female_ok + 1
          }else if (fish_sex == "UNK"){
            n_unknown_ok <- n_unknown_ok + 1
          }
        }else if (fish_condition >= u_break){
          if (fish_sex == "M"){
            n_male_good <- n_male_good + 1
          }else if (fish_sex == "F"){
            n_female_good <- n_female_good + 1
          }else if (fish_sex == "UNK"){
            n_unknown_good <- n_unknown_good + 1
          }
        }
      }
    }
  }
  cont_table <- cbind(c(n_male_bad, n_male_ok, n_male_good), c(n_female_bad, n_female_ok, n_female_good), c(n_unknown_bad, n_unknown_ok, n_unknown_good))
  row.names(cont_table) <- c("Bad", "OK", "Good")
  colnames(cont_table) <- c("Male", "Female", "Unknown")
  return(cont_table)
}

# -------------------------------------------------------------------------
# To determine the percentage of double detections by region
num.double.detections <- function(transceiver_data_side){
  entrances <- c("Bainbridge", "Elrington", "Hinchinbrook", "LaTouche", "Mobile", "Montague", "Prince of Wales")
  double_detections <- rep(0, length(entrances))
  total_detections <- rep(0, length(entrances))
  names(double_detections) <- entrances
  names(total_detections) <- entrances
  for(i in 1:length(entrances)){
    array <- entrances[i]
    detections <- transceiver_data_side[transceiver_data_side$Region==array,]
    double_detections[i]<-sum(detections$Side=="both")
    total_detections[i]<- nrow(detections)
  }
  return(list(both=double_detections, total=total_detections, percentage=double_detections/total_detections))
}

# -------------------------------------------------------------------------
# A few functions to extract the year, month, and day from a POSIXct date
get.year <- function(POSIXt_time){
  return(as.numeric(format(POSIXt_time, format="%Y")))
}
get.month <- function(POSIXt_time){
  return(as.numeric(format(POSIXt_time, format="%m")))
}
get.day <- function(POSIXt_time){
  return(as.numeric(format(POSIXt_time, format="%d")))
}

# -------------------------------------------------------------------------
# A functions to return the region where a receiver or station is located
get.region <- function(data, station="unspecified", receiver="unspecified"){
  if (station != "unspecified"){
    return(data$Region[data$Station==station][1])
  }else if (receiver != unspecified){
    return(data$Region[data$Receiver==receiver][1])
  }
}

# -------------------------------------------------------------------------
# Function to return the number of detections of a given fish. Additional argument is added to allow for year specification
get.num.detect <- function(data, transmitter, year="unspecified"){
  if (year =="unspecified"){
    return(sum(data$Transmitter==transmitter))
  }else{
    return(sum(data$Transmitter==transmitter) & data$Year==year)
  }
}

# -------------------------------------------------------------------------
# Function returning a data.frame of times and stations where a given fish was located
get.detection.stations <- function(transmitter){
  return(data.frame(Time = data$Time[data$Transmitter==transmitter], Station = data$Station[data$Transmitter==transmitter]))
}

# -------------------------------------------------------------------------
# Function returning a data.frame of times and regions where a given fish was located
get.detection.regions <- function(transmitter){
  return(data.frame(Time = data$Time[data$Transmitter==transmitter], Region = data$Region[data$Transmitter==transmitter]))
}

# -------------------------------------------------------------------------
# Function to find the number of times that we get a 2-3 sequence in the detection histories
num.23 <- function(y){
  count <- 0
  indexes <- rep(NA, 56)
  for (i in 1:nrow(y)){
    for (j in 1:(ncol(y)-1)){
      if (y[i,j]==2 & y[i,j+1]==3){
        count <- count+1
        indexes[count] <- i
      }
    }
  }
  return(indexes)
}


# -------------------------------------------------------------------------
# Function to find the numeber of times that we get a 3-2 sequence in the detection histories
num.32 <- function(y){
  count <- 0
  indexes <- rep(NA, 56)
  for (i in 1:nrow(y)){
    for (j in 1:(ncol(y)-1)){
      if (y[i,j]==3 & y[i,j+1]==2){
        count <- count+1
        indexes[count] <- j
      }
    }
  }
  return(indexes)
}


# -------------------------------------------------------------------------
# To make sure we have good initial values for z
num.82 <- function(y, t_0, tl){
  z <- get.hidden.inits(y, t_0, tl)
  count <- 0
  for (i in 1:nrow(z)){
    for (j in 1:(ncol(z)-1)){
      if (!is.na(z[i,j]) & !is.na(z[i,j+1])){
        if (z[i,j] == 8 & z[i,j+1] == 2){
          count <- count+1
        }
      }
    }
  }
  return(count)
}

num.83 <- function(y, t_0, tl){
  z <- get.hidden.inits(y, t_0, tl)
  count <- 0
  for (i in 1:nrow(z)){
    for (j in 1:(ncol(z)-1)){
      if (!is.na(z[i,j]) & !is.na(z[i,j+1])){
        if (z[i,j] == 8 & z[i,j+1] == 3){
          count <- count+1
        }
      }
    }
  }
  return(count)
}



base.pws.plot <- function(main){
  library(rgdal)
  library(raster)
  library(rgeos)
  download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_coastline.zip", destfile="coastlines.zip")
  unzip(zipfile="coastlines.zip", exdir="ne-coastlines-10m")
  coastlines<-readOGR("ne-coastlines-10m/ne_10m_coastline.shp")
  pwsExtent<-extent(-148.829400, -145.376399, 59.522309, 61.399354)
  pwsCoast<-crop(coastlines, pwsExtent)
  landCoords <- rbind(c(-148.829400, 59.94281),c(-148.829400, 61.39354),c(-145.376399, 61.39354),c(-145.376399, 60.36151), coordinates(pwsCoast)[[14]][[1]])
  waterCoords <- rbind(c(-148.829400,59.95100),c(-148.829400,59.522309),c(-145.376399, 59.522309),c(-145.376399, 60.36151), coordinates(pwsCoast)[[14]][[1]])
  islandCoords <- coordinates(pwsCoast)[-14]
  landPolys <- Polygons(list(Polygon(landCoords)), ID="land")
  waterPolys <- Polygons(list(Polygon(waterCoords)), ID="water")
  islandPoly <- lapply(islandCoords, Polygon)
  islandPolys <- list()
  index <- 1:length(islandPoly)
  for(i in 1:length(islandPoly)){
    islandPolys[[i]] <- Polygons(islandPoly, ID=paste("island", index[i], sep=" "))
  }
  SPs <- SpatialPolygons(c(landPolys, waterPolys, islandPolys))
  names<-c("land","water", paste("island", index, sep=" "))
  SPDF = SpatialPolygonsDataFrame(SPs, data.frame(N = names, row.names = names))
  return(plot(SPDF, asp=2, col=c(colors()[232], colors()[251], rep(colors()[232], length(islandPoly))), main=main))
}

# Function to pull the locations of where fish were released into system
get.release.coords <- function(){
  library(readxl)
  tagging_data_raw <- read_excel("C:/Users/19708/Desktop/Herring/Modeling/Data/tagging-data-full.xlsx", sheet = "2017 to 2019 tag data")
  lat <- tagging_data_raw$`RELEASE_ LATITUDE`
  lon <- tagging_data_raw$`RELEASE_ LONGITUDE`
  year <- tagging_data_raw$`Tag year`
  return(data.frame(lon=lon, lat=lat, year=year))
}

