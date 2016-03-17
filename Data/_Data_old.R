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
p_load(readr,dplyr)

#' Import data
test  <- read.csv(paste0(loc_in,"/test.csv"))

train <- read.csv(paste0(loc_in,"/train.csv"))
targer <- train$TARGET
id     <- train$ID
train <- filter(train,-TARGET,-ID)

#' Impute median/mode
train <- randomForest::na.roughfix(train)

#' Remove Near Zero
nzv <- nearZeroVar(train, freqCut = 1500, uniqueCut = 0.001, saveMetrics = T)
train <- train[,-nzv]
train <- train[,names(train)[nzv$zeroVar==FALSE]]

#' Identifying Correlated predictors
hcv <- findCorrelation(train, cutoff = 0.9999, exact=F)
train <- train[,-hcv]

#' Identify linear dependencies
ldv <- findLinearCombos(train)
train <- train[,-ldv$remove]

#' Create Factors (no need, all variables with 2 classes)
# l1 <- apply(train,2,function(x) length(unique(x)))
# train[,which(l1<10)] <- lapply(train[,which(l1<10)], as.factor)

#' Converts characters to factors
comb[sapply(comb, is.character)] <- 
  lapply(train[sapply(train, is.character)],as.factor)


#' Export
train2 <- comb[1:nrow(train),]
train2$target <- as.factor(train$TARGET)
saveRDS(train2,paste0(loc_out,"/train.rds"))

test2  <- comb[(nrow(train)+1):nrow(comb),]
saveRDS(test2,paste0(loc_out,"/test.rds"))
