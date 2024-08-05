
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

# Factorize categorical variables
data$RiluzoleFLN = factor(data$RiluzoleFLN, ordered = FALSE)
data$armfln = factor(data$armfln, ordered = FALSE)
data$DTHFLN = factor(data$DTHFLN, ordered = FALSE)

# Process other variables appropriately
data$ALSFRS_Delta = as.integer(data$ALSFRS_Delta)
data$Age = as.integer(data$Age)
data$Onset_Delta = as.integer(data$Onset_Delta)
data$ALBUMIN = as.integer(data$ALBUMIN)
data$ALKALINE = as.integer(data$ALKALINE)
data$HEMOGLOBIN = as.integer(data$HEMOGLOBIN)
data$RBC = as.integer(data$RBC)
data$SODIUM = as.integer(data$SODIUM)
data$Riluzole_use_Delta = as.integer(data$Riluzole_use_Delta)
data$DTHDAY = as.integer(data$DTHDAY)

# Drop useless predictors
data2 <- subset(data, select = -c(DTHDAY))  # Response variable DTHFLN is kept for RF 

# Standardize all variables
# data_std <- data2 
# data_std[] <-scale(data_std)

data_used <- data2



############################
### Build random forests ###
############################

library(ggplot2)
library(cowplot)
library(randomForest)

##### Fit 1: use all predictors and data that do not contain missing values
### RF modelling
# Start with default, i.e., ntree=500, then optimize the ntree setting
set.seed(777)
RF1 = randomForest(DTHFLN ~ ., data=data_used, ntree=10000)
RF1
print(RF1)


# Results evaluation
oob.error.data = data.frame(Trees=rep(1:nrow(RF1$err.rate), times=3),
                            Type=rep(c("OOB", "0", "1"), each=nrow(RF1$err.rate)),
                            Error=c(RF1$err.rate[,"OOB"], 
                                    RF1$err.rate[,"0"], 
                                    RF1$err.rate[,"1"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))


# Find the optimal number of variables at each internal node in the tree, i.e., "No. of variables tried at each split"
set.seed(777)
oob.values = vector(length=15)
for(i in 1:15) {
  RF1_iter = randomForest(DTHFLN ~ ., data=data_used, mtry=i, ntree=4000)
  oob.values[i] = RF1_iter$err.rate[nrow(RF1_iter$err.rate),1]
}
oob.values   # The optimal mtry=2


### Fitting and MDS plotting
set.seed(777)
RF1_optimal = randomForest(DTHFLN ~ ., data=data_used, mtry=2, ntree=4000, proximity=TRUE)
print(RF1_optimal) 


a <- RF1_optimal$confusion[4]  # True Positive (TP)
c <- RF1_optimal$confusion[2]  # False Negative (FN)
b <- RF1_optimal$confusion[3]  # False Positive (FP)
d <- RF1_optimal$confusion[1]  # True Negative (TN)

sensitivity <- a/(a+c)
specificity <- d/(b+d)
accuracy <- (a+d)/(a+b+c+d)
sensitivity
specificity
accuracy

# Convert the proximity matrix into a distance matrix.
distance.matrix = as.dist(1-RF1_optimal$proximity)
mds.stuff = cmdscale(distance.matrix, eig=TRUE, x.ret=TRUE)

# calculate the percentage of variation that each MDS axis accounts for
mds.var.per = round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)

# MDS plot
mds.values = mds.stuff$points
mds.data = data.frame(Sample=rownames(mds.values),
                      X=mds.values[,1],
                      Y=mds.values[,2],
                      Status=data_used$DTHFLN)

ggplot(data=mds.data, aes(x=X, y=Y, label=Sample)) + 
  geom_text(aes(color=Status)) +
  theme_bw() +
  xlab(paste("MDS 1 (Predictors): ", mds.var.per[1], "%", sep="")) +
  ylab(paste("MDS 2 (Response): ", mds.var.per[2], "%", sep="")) +
  ggtitle("MDS plot using (1 - Random Forest Proximities)")


### Investigate the importance of variables
varImpPlot(RF1_optimal,  
           sort = T,
           n.var = 17,
           main = "Variable Importance")






























