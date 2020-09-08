#ci<-function(m,s,n) {
#    e<-qt(0.975,df=n-1)*s/sqrt(n)
#}

# 95% rule
library(DescTools)
  
getBound <- function(file_name) {  
  fileName <- file_name
  df <- read.csv(fileName)
  df
  
  trial <- df$trial
  mean  <- df$mean
  sd    <- df$sd
  
  length<-length(trial)
  
  for (i in 1:length) {
    if (!is.na(sd[i])) { 
      intpr(c(mean[i] - 1.96*sd[i], mean[i] + 1.96*sd[i]))
    }
    else {
      print(c(NA,NA))
    }
  }
}

name = c("clinical_data/Age_1.csv", "clinical_data/BSA_1.csv", "clinical_data/CI_1.csv", "clinical_data/LVEF.csv")




getProportion <- function(file_name) {
  fileName <- file_name
  df <- read.csv(fileName)
  df
  
  trial <- df$trial
  yes   <- df$yes
  total <- df$total
  
  length<-length(trial)
  
  for (i in 1:length) {
    if (!is.na(yes[i])) { 
      a <- binom.test(yes[i], total[i],
                 0.5,
                 alternative="two.sided",
                 conf.level=0.95)
      print(a)
    }
    else {
      print(c(NA,NA))
    }
  }
}

file_name = "clinical_data/NYHA_IV.csv"
getProportion(file_name)


