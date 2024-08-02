#' dyadRobust
#' 
#' This function calculates dyad-robust standard errors via multiway decomposition, proposed by Aronow, Peter M., Cyrus Samii, and Valentina A. Assenova. "Cluster-robust variance estimation for dyadic data." Political Analysis 23.4 (2015): 564-577 (\code{\href{https://arxiv.org/pdf/1312.3398.pdf}{ARXIV}}). It can work with \code{lm()} models, or fast fixed effects models estimated with either \code{feols()} from the \code{fixest} package or \code{felm()} from the \code{lfe} package. Please ensure that the \code{feols()} function is implemented with \code{lean=FALSE} and \code{demeaned=TRUE}. Please make sure that the \code{felm()} is implemented with \code{keepCX=TRUE}.
#' @param fit The model object. Can be of any class compatible with the \code{sandwich} package.
#' @param dat The dataset used to calculate the \code{fit} object.
#' @param dyadid Name of column containing the dyad ID. Must be a character.
#' @param egoid Name of column containing the ego ID. Must be a character.
#' @param alterid Name of column containing the alter ID. Must be a character.
#' @return Returns a list containing the following components:
#' \item{bhat}{A \code{named vector} of coefficient estimates from the \code{fit} model object.}
#' \item{sehat}{A \code{named vector} of dyad-robust standard errors calculated via multiway decomposition.}
#' \item{Vhat}{A dyad-robust covariance \code{matrix}.}
#' @keywords dyad dyadic standard errors inference robust cluster
#' @examples 
#' data("dyad.sim")
#' m <- lm(dY ~ dX,dyad.sim)
#' out <- dyadRobust(fit = m,
#'                   dat = dyad.sim,
#'                   dyadid = "dyads",
#'                   egoid = "dyad1",
#'                   alterid = "dyad2")
#'            
#' # Comparing lm() to feols() to felm()          
#' data("sdat")
#' fitLM <- lm(dec~amb+attr+intel + factor(iid), 
#'           data=sdat, 
#'           weights = sdat$wts,
#'           x=T)
#' require(fixest)
#' fitFIXEST <- feols(dec~amb+attr+intel | factor(iid), 
#'                    data=sdat, 
#'                    weights = sdat$wts,
#'                    lean = F,
#'                    demeaned = T)
#' require(lfe)
#' fitLFE <- felm(dec~amb+attr+intel | factor(iid), 
#'                data=sdat, 
#'                weights = sdat$wts,
#'                keepCX = T)
#'             
#' outLM <- dyadRobust(fit = fitLM,
#'                     dat = dyad.sim,
#'                     dyadid = "dyads",
#'                     egoid = "dyad1",
#'                     alterid = "dyad2")
#' outFIXEST <- dyadRobust(fit = fitFIXEST,
#'                         dat = dyad.sim,
#'                         dyadid = "dyads",
#'                         egoid = "dyad1",
#'                         alterid = "dyad2")
#' outLFE <- dyadRobust(fit = fitLFE,
#'                      dat = dyad.sim,
#'                      dyadid = "dyads",
#'                      egoid = "dyad1",
#'                      alterid = "dyad2")
#' @export dyadRobust

dyadRobust <- function (fit, dat, dyadid, egoid, alterid) 
{
  require(sandwich)
  dyad.mat <- dat %>% dplyr::select(dyad = dyadid, ego = egoid, 
                                    alter = alterid)
  if (!is.null(fit$na.action)) {
    dyad.mat <- dyad.mat[-fit$na.action, ]
  } else if(!is.null(fit$obs_selection$obsRemoved)) {
    dyad.mat <- dyad.mat[fit$obs_selection$obsRemoved,]
    fit$na.action <- abs(fit$obs_selection$obsRemoved)
  }
  
  # R works faster with integers than text
  dyad.mat <- dyad.mat %>%
    mutate(dyad = as.integer(factor(as.character(dyad))),
           ego = as.integer(factor(as.character(ego),
                                   levels = unique(c(as.character(ego),
                                                     as.character(alter))))),
           alter = as.integer(factor(as.character(alter),
                                     levels = unique(c(as.character(ego),
                                                       as.character(alter))))))
  
  ### UPDATED CODE FOR CPU IMPROVEMENTS / SPEED ###
  if(class(fit) == 'felm') {
    if(!any(names(fit) == 'cX')) {
      stop('Please re-estimate the lfe function setting `keepCX = T`')
    }
    xmat <- fit$cX # Get the sweep matrix from the fit object itself
    vcovTmp <- NULL #vcovHC(fit,type = 'HC') # Can do this faster below
    xmat <- naresid(fit$na.action, xmat) # Remainder manually calculates the estimating function
    if(any(alias <- is.na(coef(fit)))) xmat <- xmat[, !alias, drop = FALSE]
    wts <- weights(fit)
    if(is.null(wts)) wts <- 1
    res <- fit$residuals
    sw <- as.vector(res) * wts * xmat
    brd <- bread(fit)
    n <- NROW(xmat)
    df <- n - NCOL(xmat)
    res <- rowMeans(sw/xmat, na.rm = TRUE)
    if(any(is.na(res))) {
      res[apply(abs(sw) < .Machine$double.eps, 1L, all)] <- 0 # This can be super slow with large data
    }
    omega <- res^2
    rval <- sqrt(omega) * xmat
    rval <- crossprod(rval)/n
    
    # Back to vcovHC.default
    vcovTmp <- sandwich(fit,bread. = brd,meat. = rval)
    
    if(any(apply(vcovTmp,2,function(x) any(is.na(x))))) {
      vcovTmp <- vcovHC(fit,type = 'HC')
    }
    
    sw <- data.table(sw)
    ### END UPDATED CODE FOR CPU IMPROVEMENTS ###
  } else if(class(fit) == 'fixest') {
    if(!any(names(fit) == 'X_demeaned')) {
      stop('Please re-estimate the fixest function setting `demeaned = T` and `lean = F`')
    }
    xmat <- fit$X_demeaned # Get the sweep matrix from the fit object itself
    vcovTmp <- NULL #vcovHC(fit,type = 'HC') # Can do this faster below
    xmat <- naresid(fit$na.action, xmat) # Remainder manually calculates the estimating function
    if(any(alias <- is.na(coef(fit)))) xmat <- xmat[, !alias, drop = FALSE]
    wts <- weights(fit)
    if(is.null(wts)) wts <- 1
    res <- fit$residuals
    sw <- as.vector(res) * wts * xmat
    brd <- bread(fit)
    n <- NROW(xmat)
    df <- n - NCOL(xmat)
    res <- rowMeans(sw/xmat, na.rm = TRUE)
    if(any(is.na(res))) {
      res[apply(abs(sw) < .Machine$double.eps, 1L, all)] <- 0 # This is super slow
    }
    omega <- res^2
    rval <- sqrt(omega) * xmat
    rval <- crossprod(rval)/n
    
    # Back to vcovHC.default
    vcovTmp <- sandwich(fit,bread. = brd,meat. = rval)
    
    if(any(apply(vcovTmp,2,function(x) any(is.na(x))))) {
      warning('NA values in manually calculated vcovTmp object.')
      require(sandwich)
      require(lmtest)
      M <- length(unique(cluster))
      N <- length(cluster)
      K <- model$rank
      dfc <- 1
      uj <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum))
      vcovTmp <- dfc * sandwich(model, meat = crossprod(uj)/N)
    }
    sw <- data.table(sw)
  } else {
    sw <- data.table(estfun(x = fit))
    vcovTmp <- vcovHC(fit,type = 'HC')
    brd <- bread(fit)
  }
  
  index <- unique(c(dyad.mat$ego, dyad.mat$alter))
  dcrUp <- dyad.se.helper(brd, dyad.mat, iUp = index[1], sw)
  pb <- progress_bar$new(total = length(index))
  for (i in 2:length(index)) {
    dcrUp <- dcrUp + dyad.se.helper(brd, dyad.mat, iUp = index[i], sw)
    pb$tick()
  }
  dcrUp2 <- dcrUp - dyad.se.helper(brd, dyad.mat, iUp = dyad.mat$dyad, sw)
  
  Vhat <- dcrUp2 - (length(index) - 2) * vcovTmp
  if(any(diag(Vhat) < 0)) { warning('Some elements of Vhat are < 0') }
  sehat <- sqrt(diag(Vhat))
  bhat <- coef(fit)
  return(list(bhat = bhat, sehat = sehat, Vhat = Vhat))
}