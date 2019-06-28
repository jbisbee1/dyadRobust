dyad.se.helper <- function(fit,dyad.mat,iUp,sw) {
  if(length(iUp) == 1) {
    clusUp <- as.numeric(iUp == dyad.mat$ego) + as.numeric(iUp == dyad.mat$alter)
    clusIndexUp <- clusUp*-99 + (1-clusUp)*1:nrow(dyad.mat)
  } else if(length(iUp) == nrow(sw)) {
    clusIndexUp <- iUp
  } else {
    stop("Dyad ID issue")
  }
  sw$cluster <- clusIndexUp
  dt <- data.table(sw)
  setkey(dt,cluster)
  uj_dt <- dt[,lapply(.SD,sum),by = cluster]
  uj <- as.matrix(uj_dt[,2:ncol(uj_dt)])
  rownames(uj) <- uj_dt$cluster
  return(sandwich(x = fit, meat = crossprod(uj)/length(clusIndexUp)))
}