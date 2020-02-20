# This is used to calculated Brier Score,
# accuracy, precision, recall, f-measure, 
# and AUC under ROC.
# We would be able to use packages but we preffered manual implementation. 

require(zoo) # For rollmean

################################################################################
# Brier score
################################################################################
brier <- function(probs, testdata) {
  resids <- ifelse(testdata == T, 1, 0)-probs
  return(sum(resids^2)/length(testdata))
}

pred.probs <- function(fit, testdata) {
  odds <- predict(fit, testdata,type="response")

  if (class(fit)[2] == "randomForest") {
    odds <- predict(fit, testdata, type="prob")[,"1"]
  }

  probs <- exp(odds)/(1+exp(odds))
  probs[is.na(probs)] <- 1 # XXX: NAs are treated as buggy

  return(probs)
}
################################################################################
# AUC
################################################################################

auc <- function(fit, testdata) {
  probs <- pred.probs(fit, testdata)

  tpr <- c()
  fpr <- c()
  for (thresh in seq(0,1,by=0.01)) {
    preds <- probs >= thresh

    buggy_preds <- preds[testdata$bug]
    clean_preds <- preds[!testdata$bug]

    tp <- sum(ifelse(buggy_preds == T, 1, 0))
    fp <- sum(ifelse(clean_preds == T, 1, 0))
    tpr <- append(tpr, tp / length(buggy_preds))
    fpr <- append(fpr, fp / length(clean_preds))
  }

  #plot(fpr, tpr, xlim=c(0,1), ylim=c(0,1))

  return(-areauc(fpr, tpr))
}

areauc <- function(x, y) {
  return(sum(diff(x)*rollmean(y,2)))
}


################################################################################
# performance metrics
################################################################################
evalPredict <- function (act, pred, cutoff){
  pred[pred > cutoff] = 1
  pred[pred <= cutoff] = 0
  
  # make the table
  a=table(act, pred)
  
  res=c()

  if((ncol(a) == 2) && (nrow(a) == 2)){
    res$acc   = (a[1,1] + a[2,2]) / (a[1,1] +a[1,2]+ a[2,1]+a[2,2])
    res$type1 = a[1,2]/(a[1,2]+a[2,2])
    res$type2 = a[2,1] / (a[1,1] + a[2,1])
    res$precision = a[2,2]/(a[1,2]+a[2,2])
    res$recall = a[2,2]/(a[2,1]+a[2,2])
    res$f  = 2 * res$precision * res$recall / (res$precision + res$recall)
    res$t1 = a[1,2] / (a[1,1] + a[1,2])
    res$t2 = a[2,1] / (a[2,1] + a[2,2])
    res$table = a
  }else{ # exception
    if(ncol(a) == 1){
      # all commits are predicted as non-buggy
      if(sum(pred) == 0){
        res$acc = a[1] / (a[1] + a[2])
        res$type1 = 0
        res$type2 = a[2] / (a[1] + a[2])
        res$precision = 1
        res$recall = 0
        res$f = 0
        res$t1 = 0
        res$t2 = a[2] / (a[2])
      # all commits are predicted as buggy    
      }else{
        res$acc = a[2] / (a[1] + a[2])
        res$type1 = a[1] / (a[1] + a[2])
        res$type2 = 0
        res$precision = a[2] / (a[1] + a[2])
        res$recall = 1
        res$f  = 2 * res$precision * res$recall / (res$precision + res$recall)
        res$t1 = a[1] / (a[1])
        res$t2 = 0
      }
    }else{ # non-buggy commit
      res$acc = 0
      res$type1 = 0
      res$type2 = 0
      res$precision = 0
      res$recall = 0
      res$f = 0
      res$t1 = 0
      res$t2 = 0
    }
  }

  return (res)
}


################################################################################
# AUC
################################################################################
calcROC <- function(pred, actual) {
  sortedId <- order(pred, decreasing=TRUE)
  fp <- tp <- fp_prev <- tp_prev <- 0
  nF <- sum(actual == FALSE)
  nT <- sum(actual == TRUE)

  if(nF == 0 || nT == 0) {
    return (0)
  }

  pred_prev <- -Inf
  ber_min <- Inf
  area <- 0
  rx <- ry <- numeric(length(sortedId))
  n <- 0
  for (i in seq_along(sortedId)) {
    j <- sortedId[i]
    if (pred[j] != pred_prev) {
      area <- area + (fp - fp_prev) * (tp + tp_prev) / 2
      n <- n + 1
      rx[n] <- fp/nF
      ry[n] <- tp/nT
      ber <- (fp/nF + 1 - tp/nT)/2
      if (ber < ber_min) {
        ber_min <- ber
        th <- pred_prev
        rx_best <- fp/nF
        ry_best <- tp/nT
      }
      pred_prev <- pred[j]
      fp_prev <- fp
      tp_prev <- tp
    }
    if (actual[j] == TRUE) {
      tp <- tp + 1
    } else {
      fp <- fp + 1
    }
  }
  area <- area + (fp - fp_prev) * (tp + tp_prev) / 2
  return (area/(nF*nT))
}
