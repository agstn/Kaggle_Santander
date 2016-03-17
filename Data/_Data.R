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
p_load(dplyr)

#' Load csv files
train <- read.csv(paste0(loc_in,"/train.csv"))
test  <- read.csv(paste0(loc_in,"/test.csv"))

#' 0 count per line
count0 <- function(x) {
  return( sum(x == 0) )
}
train$n0 <- apply(train, 1, FUN=count0)
test$n0 <- apply(test, 1,  FUN=count0)

#' Removing constant features
cat("\n## Removing the constants features.\n")
for (f in names(train)) {
  if (length(unique(train[[f]])) == 1) {
    cat(f, "is constant in train. We delete it.\n")
    train[[f]] <- NULL
    test[[f]]  <- NULL
  }
}

#' Removing identical features
features_pair <- combn(names(train), 2, simplify = F)
toRemove <- c()
for(pair in features_pair) {
  f1 <- pair[1]
  f2 <- pair[2]
  
  if (!(f1 %in% toRemove) & !(f2 %in% toRemove)) {
    if (all(train[[f1]] == train[[f2]])) {
      cat(f1, "and", f2, "are equals.\n")
      toRemove <- c(toRemove, f2)
    }
  }
}

#' Features to keep
feature.names <- setdiff(names(train), toRemove)
train <- train[, feature.names]
test  <-  test[, feature.names[-308]]

#' Export
write.csv(train,paste0(loc_out,"/train.csv"))
write.csv(train,paste0(loc_out,"/test.csv"))




