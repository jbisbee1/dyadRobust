#' dyadRobust
#' 
#' This function calculates dyad-robust standard errors via multiway decomposition, proposed by Aronow, Peter M., Cyrus Samii, and Valentina A. Assenova. "Cluster-robust variance estimation for dyadic data." Political Analysis 23.4 (2015): 564-577 (\code{\href{https://arxiv.org/pdf/1312.3398.pdf}{ARXIV}}).
#' @param fit The model object. Should be of type 'lm'.
#' @param dat The dataset used to calculate the \code{fit} object.
#' @param dyadid Name of column containing the dyad ID. Must be a character.
#' @param egoid Name of column containing the ego ID. Must be a character.
#' @param alterid Name of column containing the alter ID. Must be a character.
#' @return Returns a list containing the following components:
#' \item{bhat}{A \code{named vector} of coefficient estimates from the \code{fit} model object.}
#' \item{sehat}{A \code{named vector} of dyad-robust standard errors calculated via multiway decomposition.}
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

dyadRobust <- function(fit,dat,dyadid,egoid,alterid) {
  dyad.mat <- dat %>% dplyr::select(dyad = dyadid,
                                    ego = egoid,
                                    alter = alterid)
  if(!is.null(fit$na.action)) {
    dyad.mat <- dyad.mat[-fit$na.action,]
  }
  sw <- as_tibble(sandwich::estfun(fit))
  
  index <- unique(c(dyad.mat$ego,dyad.mat$alter))
  
  dcrUp <- dyad.se.helper(fit,dyad.mat,iUp = index[1],sw)
  
  pb <- progress_bar$new(total = length(index))
  for(i in 2:length(index)){
    dcrUp <- dcrUp + dyad.se.helper(fit,dyad.mat,index[i],sw)
    pb$tick()
  }
  dcrUp2 <- dcrUp - dyad.se.helper(fit,dyad.mat,dyad.mat$dyad,sw)
  Vhat <- dcrUp2 - (length(index)-2)*vcovHC(fit,type="HC0")
  
  # standard errors
  sehat <- sqrt(diag(Vhat))
  bhat <- coef(fit)
  return(list(bhat = bhat,sehat = sehat))
}