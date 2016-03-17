#' Parameters
if (Sys.info()['sysname']=="Windows") {
  loc_in   <- "C:/Git/Kaggle_Santander/Data/Raw"
  loc_out  <- "C:/Git/Kaggle_Santander/Data/Derive"
} else {
  loc_in   <- "/home/acalatroni/Kaggle_Santander/Data/Raw"
  loc_out  <- "/home/acalatroni/Kaggle_Santander/Data/Derive"
}

#' libraries
pacman::p_load(pacman)
p_load(caret)
p_load(dplyr)

#' Import data
test  <- read.csv(paste0(loc_in,"/test.csv"))

train <- read.csv(paste0(loc_in,"/train.csv"))
target <- train$TARGET
train <- select(train,-TARGET)

#' Remove constant columns
train <- train[sapply(train, function(x) length(unique(na.omit(x)))) > 1]

#' Remove duplicate columns
train <- train[!duplicated(lapply(train,summary))]

#' Impute median/mode
train <- randomForest::na.roughfix(train)

#' Find and Remove linear combinations
ldv <- findLinearCombos(train)
train <- train[,-ldv$remove]

#' Count the # of Zeros
train$n0 <- apply(train, 1, function(x) sum(x == 0))
test$n0  <- apply(test, 1,  function(x) sum(x == 0))

#' Export
test <- test[,names(train)]
write.csv(test,paste0(loc_out,"/test.csv"))

train <- cbind(train,target)
write.csv(train,paste0(loc_out,"/train.csv"))
