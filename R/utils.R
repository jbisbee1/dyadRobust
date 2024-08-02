dyad.se.helper <- function (brd, dyad.mat, iUp, sw) 
{
  if (length(iUp) == 1) {
    
    clusUp <- as.numeric(iUp == dyad.mat$ego) + as.numeric(iUp == dyad.mat$alter)
    clusIndexUp <- clusUp * -99 + (1 - clusUp) * 1:nrow(dyad.mat)
    sw$cluster <- clusIndexUp
    setkey(sw, cluster)
    # Old stuff: trying different efficient solutions
    # uj_dt <- bind_rows(sw %>%
    #                      filter(cluster == -99) %>%
    #                      group_by(cluster) %>% summarise_all(sum),
    #                    sw %>% filter(cluster != -99))
    uj_dt <- sw[,lapply(.SD,sum),by = cluster]
    
  } else if (length(iUp) == nrow(sw)) {
    clusIndexUp <- iUp
    sw$cluster <- clusIndexUp
    setkey(sw, cluster)
    # Old stuff: trying different efficient solutions
    # uj_dt <- sw %>% group_by(cluster) %>% summarise_all(sum)
    uj_dt <- sw[,lapply(.SD,sum),by = cluster]
  } else {
    stop("Dyad ID issue")
  }
  uj <- as.matrix(uj_dt[, 2:ncol(uj_dt)])
  rownames(uj) <- uj_dt$cluster
  
  ### UPDATED CODE FOR CPU IMPROVEMENTS ###
  meat <- crossprod(uj)/length(clusIndexUp) # Time consuming but not nearly as taxing on the CPU
  return((1/nrow(sw) * (brd %*% meat %*% brd))) # Manually calculate sandwich
  ### UPDATED CODE FOR CPU IMPROVEMENTS ###
}