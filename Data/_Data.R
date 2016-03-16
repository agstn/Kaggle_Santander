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
p_load(readr,dplyr)

#' Import data
test  <- read_csv(paste0(loc_in,"/test.csv.zip"))
train <- read_csv(paste0(loc_in,"/train.csv.zip"))

#' Combine
comb <- rbind(select(train,-TARGET),test)

#' Remove Near Zero
nzv <- caret::nearZeroVar(comb, freqCut = 99/1, uniqueCut = 2)
comb <- comb[,-nzv]

#' Create Factors
l1 <- apply(comb,2,function(x) length(unique(x)))
comb[,which(l1<10)] <- lapply(comb[,which(l1<10)], as.factor)

#' Converts characters to factors
comb[sapply(comb, is.character)] <- lapply(comb[sapply(comb, is.character)],as.factor)

#' Impute median/mode
comb <- randomForest::na.roughfix(comb)

#' Export
train2 <- comb[1:nrow(train),]
train2$target <- as.factor(train$TARGET)
saveRDS(train2,paste0(loc_out,"/train.rds"))

test2  <- comb[(nrow(train)+1):nrow(comb),]
saveRDS(test2,paste0(loc_out,"/test.rds"))
