#' dyadRobust
#' 
#' This function calculates dyad-robust standard errors via multiway decomposition, proposed by Aronow, Peter M., Cyrus Samii, and Valentina A. Assenova. "Cluster-robust variance estimation for dyadic data." Political Analysis 23.4 (2015): 564-577 (\code{\href{https://arxiv.org/pdf/1312.3398.pdf}{ARXIV}}).
#' @param fit The model object. Can be of any class compatible with the \code{sandwich} package.
#' @param dat The dataset used to calculate the \code{fit} object.
#' @param dyadid Name of column containing the dyad ID. Must be a character.
#' @param egoid Name of column containing the ego ID. Must be a character.
#' @param alterid Name of column containing the alter ID. Must be a character.
#' @param parallel Logical for whether to run in parallel (number of cores = total - 1). Defaults to \code{FALSE}.
#' @return Returns a list containing the following components:
#' \item{bhat}{A \code{named vector} of coefficient estimates from the \code{fit} model object.}
#' \item{sehat}{A \code{named vector} of dyad-robust standard errors calculated via multiway decomposition.}
#' \item{Vhat}{A dyad-robust covariance \code{matrix}.}
#' @keywords dyad dyadic standard errors inference robust cluster
#' @examples 
#' data("dyad.sim")
#' m <- lm(dY ~ dX,dyad.sim)
#' out <- dyadRobust(fit = m,
#'            dat = dyad.sim,
#'            dyadid = "dyads",
#'            egoid = "dyad1",
#'            alterid = "dyad2")
#' @export dyadRobust

dyadRobust <- function(fit,dat,dyadid,egoid,alterid,parallel = F) {
  require(sandwich)
  fit = fit
  dyad.mat <- dat %>% dplyr::select(dyad = dyadid,
                                    ego = egoid,
                                    alter = alterid)
  if(!is.null(fit$na.action)) {
    dyad.mat <- dyad.mat[-fit$na.action,]
  }
  if(class(fit) == "felm" & "cX" %in% names(fit)) {
    xmat <- fit$cX
    xmat <- naresid(fit$na.action, xmat)
    if(any(alias <- is.na(coef(fit)))) xmat <- xmat[, !alias, drop = FALSE]
    wts <- weights(fit)
    if(is.null(wts)) wts <- 1
    res <- residuals(fit)
    sw <- data.table(as.vector(res) * wts * xmat)
  } else {
    sw <- data.table(estfun(x = fit))
  }
  
  index <- unique(c(dyad.mat$ego,dyad.mat$alter))
  
  dcrUp <- dyad.se.helper(fit,dyad.mat,iUp = index[1],sw)
  
  if(parallel) {
    require(doParallel)
    require(foreach)
    ncores <- parallel::detectCores()
    cl <- makeCluster(ncores - 1,outfile = "workerout")
    registerDoParallel(cl)
    tmp <- foreach(i = 2:length(index),.packages = c('data.table','sandwich')) %dopar% dyad.se.helper(fit,dyad.mat,index[i],sw)
    stopCluster(cl)
  } else {
    pb <- progress_bar$new(total = length(index))
    tmp <- vector(mode = "list",length = length(index)-1)
    for(i in 2:length(index)){
      tmp[[(i-1)]] <- dyad.se.helper(fit,dyad.mat,index[i],sw)
      pb$tick()
    }
  }
  for(i in 1:length(tmp)) {
    dcrUp <- dcrUp + tmp[[i]]
  }
  dcrUp2 <- dcrUp - dyad.se.helper(fit,dyad.mat,dyad.mat$dyad,sw)
  Vhat <- dcrUp2 - (length(index)-2)*vcovHC(fit,type="HC0")
  
  # standard errors
  sehat <- sqrt(diag(Vhat))
  bhat <- coef(fit)
  return(list(bhat = bhat,sehat = sehat,Vhat = Vhat))
}
