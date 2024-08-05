
rm(list=ls(all=TRUE))
cat("\014")

options(max.print = 100000000)
set.seed(777)

library(readxl)
library(dplyr)


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

# Standardize all variables
data_std <-scale(data)

data_used <- data


####################################
### Adaptive Regression (Page 4) ###
####################################

library(stats)
library(scatterplot3d)
library(diffusionMap)

# Create predictor matrix
X <- subset(data_std, select = -c(slope_ALSFRS))

# Create response vector
y <-  as.vector(subset(data_std, select = c(slope_ALSFRS)))

D = as.matrix(dist(X))

# leave-one-out cross-validation:
mmax = 100
AR = adapreg.m(epsilon = 24.38433, 
               D = D,
               y = y,
               mmax = mmax,
               nfolds = 10)

print(paste("optimal model size:",
            AR$mopt,
            "; min. CV risk:",
            round(AR$mincvrisk, mmax)))

plot(AR$cvrisks,
     typ='b',
     xlab="Model size",
     ylab="CV risk",
     cex.lab=1.5,
     cex.main=1.5,
     main="CV risk estimates")
plot(y,
     AR$y.hat,
     xlab="y",
     ylab="y_hat",
     cex.lab=1.5,
     cex.main=1.5,
     main="Predictions")
abline(coef = c(0,1), col="blue")

summary(AR)

# R^2
library(Metrics)
rmse(y, AR$y.hat)


sqrt(mean((y- AR$y.hat)^2))












