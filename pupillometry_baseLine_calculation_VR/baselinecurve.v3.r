library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)



analyse_VRpupil <- function(datadir, filename) {
  # extract information from filename
  mystudentid = str_sub(filename,1,2)
  mytrial = str_sub(filename,4,5)
  mylevel = str_sub(filename,7,7)
  
  # read data
  
  delay = 1; # delay in seconds after start of change in light levels
  
  data = read.csv(paste(datadir,filename,sep="\\"), sep= "\t")
  data <- data %>% select(-X) %>% mutate(flag = trimws(as.character(flag))) %>% 
    mutate(flag = ifelse(flag=="", NA, as.character(flag))) %>% fill(flag)

  
  # remove part with no flag
  data <- data[complete.cases(data),]
  # remove pupil.diameter zero
  data <- data %>% filter(pupil.diameter>0) 
  
  # separate baselinedata and videodata
  videodata <- data %>% filter(substr(flag,1,5)!="Light")
  baselinedata <- data %>% filter(substr(flag,1,5)=="Light")
  baselinedata <- baselinedata %>% mutate(flag = as.factor(flag))
  
  # remove delay periods from data 
  startpoint <- baselinedata %>% group_by(flag) %>% summarize(start = min(seconds)) %>% ungroup()
  baselinedata <- baselinedata %>% left_join(startpoint, by=c("flag"))
  baselinedata <- baselinedata %>% filter(seconds-start>delay) %>% select(-start)
  rm(startpoint)
  
  
  fit <- lm(pupil.diameter ~ poly(brightness,3), baselinedata)
  videodata$baseline <- predict(fit,videodata)
  videodata <- videodata %>% mutate(
    pupil.diameter.corrected = pupil.diameter - baseline,
    studentid = mystudentid,
    trial = mytrial,
    level = mylevel
  )
  
  return(videodata)
}


get_baselineplot <- function(datadir, filename) {
  
  # extract information from filename
  mystudentid = str_sub(filename,1,2)
  mytrial = str_sub(filename,4,5)
  mylevel = str_sub(filename,7,7)
  
  # read data
  
  delay = 1; # delay in seconds after start of change in light levels
  
  data = read.csv(paste(datadir,filename,sep="\\"), sep= "\t")
  data <- data %>% select(-X) %>% mutate(flag = trimws(as.character(flag))) %>% 
    mutate(flag = ifelse(flag=="", NA, as.character(flag))) %>% fill(flag)
  
  
  # remove part with no flag
  data <- data[complete.cases(data),]
  # remove pupil.diameter zero
  data <- data %>% filter(pupil.diameter>0) 
  
  # separate baselinedata and videodata
  baselinedata <- data %>% filter(substr(flag,1,5)=="Light")
  baselinedata <- baselinedata %>% mutate(flag = as.factor(flag))
  
  # remove delay periods from data 
  startpoint <- baselinedata %>% group_by(flag) %>% summarize(start = min(seconds)) %>% ungroup()
  baselinedata <- baselinedata %>% left_join(startpoint, by=c("flag"))
  baselinedata <- baselinedata %>% filter(seconds-start>delay) %>% select(-start)
  rm(startpoint)
  
  
  fit <- lm(pupil.diameter ~ poly(brightness,3), baselinedata)

  # plotting the fitted curve on top of baselinedata
  predicted.intervals <- predict(fit,baselinedata,interval='confidence',level=0.99)
  plotdata <- data.frame(brightness = baselinedata$brightness, 
                         pupil.diameter = baselinedata$pupil.diameter,
                         minpupil = predicted.intervals[,2],
                         predpupil = predicted.intervals[,1],
                         maxpupil = predicted.intervals[,3])
  
  return(ggplot(plotdata, aes(x=brightness, y=pupil.diameter)) + geom_point() +
    geom_line(aes(x=brightness, y=predpupil), color="red") +
    geom_line(aes(x=brightness, y=minpupil), color="red", lty=2) +
    geom_line(aes(x=brightness, y=maxpupil), color="red", lty=2) + 
    ggtitle(paste("Baseline plot ",str_sub(filename,1,7))) + ylim(0,6))
}  





get_baseline_fit <- function(datadir, filename) {
  
  # extract information from filename
  mystudentid = str_sub(filename,1,2)
  mytrial = str_sub(filename,4,5)
  mylevel = str_sub(filename,7,7)
  
  # read data
  
  delay = 1; # delay in seconds after start of change in light levels
  
  data = read.csv(paste(datadir,filename,sep="\\"), sep= "\t")
  data <- data %>% select(-X) %>% mutate(flag = trimws(as.character(flag))) %>% 
    mutate(flag = ifelse(flag=="", NA, as.character(flag))) %>% fill(flag)
  
  
  # remove part with no flag
  data <- data[complete.cases(data),]
  # remove pupil.diameter zero
  data <- data %>% filter(pupil.diameter>0) 
  
  # separate baselinedata and videodata
  baselinedata <- data %>% filter(substr(flag,1,5)=="Light")
  baselinedata <- baselinedata %>% mutate(flag = as.factor(flag))
  
  # remove delay periods from data 
  startpoint <- baselinedata %>% group_by(flag) %>% summarize(start = min(seconds)) %>% ungroup()
  baselinedata <- baselinedata %>% left_join(startpoint, by=c("flag"))
  baselinedata <- baselinedata %>% filter(seconds-start>delay) %>% select(-start)
  rm(startpoint)
  
  
  return(lm(pupil.diameter ~ poly(brightness,3), baselinedata))
  
}


# application


get_baselineplot(".","02-01-E_tracking_data.txt")
fit <- get_baseline_fit(".","02-01-E_tracking_data.txt")
summary(fit)

d <- analyse_VRpupil(".","02-01-E_tracking_data.txt")



ggplot(d, aes(x=seconds, y=pupil.diameter)) + geom_line() +
  geom_line(aes(x=seconds, y=pupil.diameter.corrected), color="red") + 
  geom_line(aes(x=seconds, y=brightness*10), color="green", lty=2) + 
  geom_line(aes(x=seconds, y=baseline), color="blue", lty=2) + 
  ggtitle("Videodata, corrected for fitted baseline")
  

get_baselineplot(".","02-02-D_tracking_data.txt")
fit <- get_baseline_fit(".","02-02-D_tracking_data.txt")
summary(fit)



d <- analyse_VRpupil(".","02-02-D_tracking_data.txt")

ggplot(d, aes(x=seconds, y=pupil.diameter)) + geom_line() +
  geom_line(aes(x=seconds, y=pupil.diameter.corrected), color="red") + 
  geom_line(aes(x=seconds, y=brightness*10), color="green", lty=2) + 
  geom_line(aes(x=seconds, y=baseline), color="blue", lty=2) + 
  ggtitle("Videodata, corrected for fitted baseline")

  