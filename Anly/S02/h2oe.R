#' Parameters
if (Sys.info()['sysname']=="Windows") {
  loc_in   <- "C:/Git/Kaggle_Santander/Data/Data/Raw"
  loc_out  <- "C:/Git/Kaggle_Santander/Data/Anly/S01"
} else {
  loc_in   <- "/home/acalatroni/Kaggle_Santander/Data/Raw"
  loc_out  <- "/home/acalatroni/Kaggle_Santander/Anly/S02"
}

#' Packages
pacman::p_load(pacman)
p_load(h2o,h2oEnsemble)

#' Start h2o
h2o.init(nthreads=-1)

#' Import Data
train_h2o <- as.h2o(train2, destination_frame = "train.hex")
test_h2o  <- as.h2o(test2,  destination_frame = "test.hex")

#' No Splits

#' Setup
y      <- "target"
x      <- setdiff(names(train_h2o[,-1]), y)
family <- "binomial"

#' Specify the base learner & the metalearner
learner <- c("h2o.glm.wrapper",
             "h2o.randomForest.wrapper",
             "h2o.gbm.wrapper",
             "h2o.deeplearning.wrapper")

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

#' Base learner test set AUC (for comparison)
#' Other metrics
L   <- length(fit$learner)
AUC  <- sapply(seq(L), function(l)  fit$basefits[[l]]@model$cross_validation_metrics@metrics$AUC)
fit$metafit@model$coefficients_table

#' Predict
p       <- predict.h2o.ensemble(fit,test_h2o)
p1      <- as.vector(p$pred[,"predict"])

submission <- data.frame(ID=test$ID,TARGET=p1)
write_csv(submission,paste0(loc_out,"/submission.csv"))
