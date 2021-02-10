
library(readxl)

#Input follow-up arrangements
data <- read_excel(".xlsx")
DelayTimeCal<-function(month,schedule,risk){
    cohort <- as.data.frame(cbind(month, schedule, rep(1, length(month))*risk), stringsAsFactors=F)
    colnames(cohort) <- c("Month", "Follow-up", "Patients")
    cohort <- as.data.frame(apply(cohort, 2, as.numeric))
    #Calculate the months of delay in detection
    followup.month <- cohort[which(cohort$`Follow-up`==1), 1] 
    g <- vector() 
    for(m in 2:length(followup.month)){   
      g[m] <- followup.month[m]-followup.month[m-1]
    }
    g[1] <- followup.month[1] 
    result <- c(0) 
    for(i in 1:length(followup.month)){
      h <- rep(1, g[i]) 
      k <- length(h)
      for(j in 1:k){
        month <- cohort$Patients[j+followup.month[i]-k+1-1]*(k-j)  
        result <- month+result
      }
    }
    return(result)
  }
#The delayed-detection months was defined as the duration from the failure occurrence to the next nearest follow-up.
#The input variable month represents the time, schedule represents the follow-up arrangement of each month, risk represents the probability of disease failure in each month
DelayTimeCal(month=data$Month, schedule=data$`Follow-up schedule`, risk=data$`Probability per month`)
  
