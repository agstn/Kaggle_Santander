#' Parameters
if (Sys.info()['sysname']=="Windows") {
  loc_in   <- "C:/Git/Kaggle_Santander/Data/Data/Derive"
  loc_out  <- "C:/Git/Kaggle_Santander/Data/Anly/S05"
} else {
  loc_in   <- "/home/acalatroni/Kaggle_Santander/Data/Derive"
  loc_out  <- "/home/acalatroni/Kaggle_Santander/Anly/s05"
}

#' Packages
pacman::p_load(pacman)
p_load(readr,dplyr)
p_load(h2o,h2oEnsemble)

#' Start h2o
h2o.init(nthreads=-1)

#' Import RDS files
train <- read.csv(paste0(loc_in,"/train.csv"))
test  <- read.csv(paste0(loc_in,"/test.csv"))

#' Check dimensions
dim(train)
dim(test)

#' target as factor
train$target <- as.factor(train$target)

#' Import Data
train_h2o <- as.h2o(train, destination_frame = "train.hex")
test_h2o  <- as.h2o(test,  destination_frame = "test.hex")

#' Setup
y      <- "target"
x      <- setdiff(names(train_h2o[,-1]), y)
family <- "binomial"

#' Specify the base learner & the metalearner
source(paste0(loc_out,"/",'_base_learners.R'))

learner <- c("h2o.glm.1","h2o.glm.2","h2o.glm.3",
             "h2o.rf.1","h2o.rf.2","h2o.rf.3","h2o.rf.4",
             "h2o.gbm.1","h2o.gbm.2","h2o.gbm.3","h2o.gbm.4","h2o.gbm.5","h2o.gbm.6","h2o.gbm.7","h2o.gbm.8",
             "h2o.dl.1","h2o.dl.2","h2o.dl.3","h2o.dl.4", "h2o.dl.5","h2o.dl.6","h2o.dl.7"
             )

metalearner <- "h2o.glm.wrapper"

#' Ensemble training
fit <- h2o.ensemble(x = x,
                    y = y,
                    training_frame   = train_h2o,
                    family = "binomial",
                    learner = learner,
                    metalearner = metalearner,
                    cvControl = list(V=5)
)

#' Results

L   <- length(fit$learner)
AUC  <- sapply(seq(L), function(l)  fit$basefits[[l]]@model$cross_validation_metrics@metrics$AUC)
data.frame(fit$metafit@model$coefficients_table[-1,],AUC)

#' Predict
p       <- predict.h2o.ensemble(fit,test_h2o)
p1      <- as.vector(p$pred[,"predict"])

submission <- data.frame(ID=test$ID,TARGET=p1)
write_csv(submission,paste0(loc_out,"/submission.csv"))

#' All done, shutdown H2O
# h2o.shutdown(prompt=FALSE)

