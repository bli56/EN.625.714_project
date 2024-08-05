
rm(list=ls(all=TRUE))
cat("\014")

options(max.print = 100000000)

library(readxl)
library(dplyr)
library(scatterplot3d)
library(diffusionMap)
library(writexl)

### Import data ###
proact_data <- read_xlsx(path = "C:\\Users\\LiBaini\\Documents\\2024_Summer\\EN.625.714.81.SU24 Introductory Stochastic Differential Equations with Applications\\714 project\\Data\\proact_data.xlsx")

data <- cbind(proact_data$ALSFRS_Delta,
              proact_data$Age,
              proact_data$Onset_Delta,
              proact_data$ALBUMIN,
              proact_data$ALKALINE,
              proact_data$BASOPHILS,
              proact_data$CALCIUM,
              proact_data$CREATININE,
              proact_data$HEMOGLOBIN,
              proact_data$POTASSIUM,
              proact_data$RBC,
              proact_data$SODIUM,
              proact_data$WBC,
              proact_data$RiluzoleFLN,
              proact_data$Riluzole_use_Delta,
              proact_data$DTHFLN,     
              proact_data$DTHDAY,
              proact_data$armfln,
              proact_data$slope_ALSFRS)

colnames(data) = c("ALSFRS_Delta", "Age", "Onset_Delta", "ALBUMIN", "ALKALINE", "BASOPHILS", "CALCIUM", "CREATININE", "HEMOGLOBIN", "POTASSIUM", "RBC", "SODIUM", "WBC", "RiluzoleFLN", "Riluzole_use_Delta", "DTHFLN", "DTHDAY", "armfln", "slope_ALSFRS")
data = as.data.frame(data)

# Remove observations with missing values
data <- data[complete.cases(data), ]
write_xlsx(data, "C:\\Users\\LiBaini\\Documents\\2024_Summer\\EN.625.714.81.SU24 Introductory Stochastic Differential Equations with Applications\\714 project\\Data\\data.xlsx")

# Drop useless predictors
data2 <- subset(data, select = -c(armfln))

# Standardize all variables
data_std <- data2 
data_std[] <-scale(data_std)

data_used <- data_std


#########################
### Parameter Finding ###
#########################


K=2 # number of clusters
upper_iter = 100
upper_seed = 1000
lower_seed_floor = 0
lower_seed = lower_seed_floor+1
ttl_raw = upper_iter*(upper_seed-lower_seed+1)
perf <- data.frame(matrix(ncol = 6, nrow = ttl_raw))
colnames(perf) <- c("iter", "seed", "p", "sensitivity", "specificity", "accuracy")

for(i_iter in 1: upper_iter){
  
  p_dis <- i_iter/upper_iter
  if (p_dis > 0.99){
    p_dis = 0.99
  }

  for(i_seed in lower_seed: upper_seed){
    # Show real time progress 
    Sys.sleep(0.1)
    print(paste0("Iter = ", i_iter))
    print(paste0("Seed = ", i_seed))
    
    break_signal = 0
    
    set.seed(i_seed)
    D = dist(data_used)

    eps_val <- epsilonCompute(D=D, p=p_dis)
    
    dmap <- diffuse(D=D, eps.val=eps_val)   ### Diffuse Compute diffusion map coordinates from pair-wise distance (page 7)
    dkmeans = diffusionKmeans(dmap=dmap, K=K, Niter=10)   ### Diffusion K-means (page 9)
    
    DTH_Pred <- dkmeans$part-1
    DTH_Real <- data$DTHFLN
    
    confmat_verti <- as.data.frame(cbind(DTH_Pred, DTH_Real))
    colnames(confmat_verti) = c("Prediction", "Reality")
    
    namevector <- c("Pt_Rt", "Pt_Rf", "Pf_Rt", "Pf_Rf")
    confmat_verti[, namevector] <- NA
    n_ttl <- as.integer(length(DTH_Pred))
    
    for(i in 1: n_ttl){
      if (confmat_verti[i, 1] == 1 & confmat_verti[i, 2] == 1) {
        confmat_verti[i, 3] = 1   # Pt_Rt
      } else if (confmat_verti[i, 1] == 1 & confmat_verti[i, 2] == 0) {
        confmat_verti[i, 4] = 1   # Pt_Rf
      } else if (confmat_verti[i, 1] == 0 & confmat_verti[i, 2] == 1) {
        confmat_verti[i, 5] = 1   # Pf_Rt
      } else if (confmat_verti[i, 1] == 0 & confmat_verti[i, 2] == 0) {
        confmat_verti[i, 6] = 1   # Pf_Rf
      }
    }
    
    confmat_verti[is.na(confmat_verti)] <- 0
    
    a <- sum(confmat_verti$Pt_Rt)
    b <- sum(confmat_verti$Pt_Rf)
    c <- sum(confmat_verti$Pf_Rt)
    d <- sum(confmat_verti$Pf_Rf)
    
    sensitivity <- a/(a+c)
    specificity <- d/(b+d)
    accuracy <- (a+d)/(a+b+c+d)
    
    row_num = (i_iter-1)*(upper_seed-lower_seed+1)+(i_seed - lower_seed_floor)
    
    perf[row_num,1] <- i_iter
    perf[row_num,2] <- i_seed
    perf[row_num,3] <- p_dis
    perf[row_num,4] <- sensitivity
    perf[row_num,5] <- specificity
    perf[row_num,6] <- accuracy
    # print(perf[row_num,])
    
    if (sensitivity>0.95 & specificity>0.95 & accuracy>0.95) {    # End the loop if a clearly good parameter combination has been found. 
      break_signal <<- 1;
      message("A clearly good parameter combination has been found!")
      print(perf[row_num,])
      break
    }
  }
  if (break_signal == 1) 
  { 
    break
  } 
}


filter <- function(sensi, speci, accur){
  perf <- perf[complete.cases(perf), ] # Remove rows with any missing values
  filtered_perf <<- perf[perf$sensitivity >= sensi &
                   perf$specificity >= speci &
                   perf$accuracy >= accur,]
  print(paste0("Number of eligible records: ", nrow(filtered_perf)))  # Return the number of eligible records 
  head(filtered_perf, 100)  # Show the first 100 rows
}

filter(0.6, 0.6, 0.6)


# Best p (for std data) = 0.9860 (no good; seed=777; perf_lev=0.57)
# Best p (for data) = 


##########################################################
### Diffusion K-means using the best parameter setting ###
##########################################################


set.seed(177)
D1 = dist(data_std)
eps_val1 <- epsilonCompute(D=D1, p=0.12)
dmap1 <- diffuse(D=D1, eps.val=eps_val1)
dkmeans1 = diffusionKmeans(dmap=dmap1, K=2, Niter=10)

col_dkmeans1=ifelse(dkmeans1$part==1, "red", "blue")
scatterplot3d(x = data_std$DTHDAY,
              y = data_std$slope_ALSFRS,
              z = data_std$Onset_Delta,
              xlab="Death day",
              ylab="ALSFRS-R slope",
              zlab="Onset delta",
              color = col_dkmeans1,
              main ="PRO-ACT Data, colored by diff. K-means class")




DTH_Pred <- dkmeans1$part-1
DTH_Real <- data$DTHFLN

confmat_verti <- as.data.frame(cbind(DTH_Pred, DTH_Real))
colnames(confmat_verti) = c("Prediction", "Reality")

namevector <- c("Pt_Rt", "Pt_Rf", "Pf_Rt", "Pf_Rf")
confmat_verti[, namevector] <- NA
n_ttl <- as.integer(length(DTH_Pred))

for(i in 1: n_ttl){
  if (confmat_verti[i, 1] == 1 & confmat_verti[i, 2] == 1) {
    confmat_verti[i, 3] = 1   # Pt_Rt
  } else if (confmat_verti[i, 1] == 1 & confmat_verti[i, 2] == 0) {
    confmat_verti[i, 4] = 1   # Pt_Rf
  } else if (confmat_verti[i, 1] == 0 & confmat_verti[i, 2] == 1) {
    confmat_verti[i, 5] = 1   # Pf_Rt
  } else if (confmat_verti[i, 1] == 0 & confmat_verti[i, 2] == 0) {
    confmat_verti[i, 6] = 1   # Pf_Rf
  }
}

confmat_verti[is.na(confmat_verti)] <- 0

a <- sum(confmat_verti$Pt_Rt)
b <- sum(confmat_verti$Pt_Rf)
c <- sum(confmat_verti$Pf_Rt)
d <- sum(confmat_verti$Pf_Rf)

sensitivity <- a/(a+c)
specificity <- d/(b+d)
accuracy <- (a+d)/(a+b+c+d)

sensitivity
specificity
accuracy






