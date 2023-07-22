#' @export
NBIC <- function(mod, intercept_mod, return.K = FALSE, nobs = NULL, ...){
  UseMethod("NBIC", mod)
}

#' @export
NBIC.default <- function(mod, intercept_mod, return.K = FALSE, nobs = NULL, ...){
  stop("\nFunction not yet defined for this object class\n")
}

#' @export
NBIC.lm <-
  function(mod, intercept_mod, return.K = FALSE, nobs = NULL, ...){
    
    if(identical(nobs, NULL)) {n <- length(mod$fitted)} else {n <- nobs}
    LL <- logLik(mod)[1]
    K <- attr(logLik(mod), "df")  #extract correct number of parameters included in model - this includes LM
    BIC <- -2*LL + K * log(n)
    if(return.K == TRUE) BIC[1] <- K #attributes the first element of BIC to K
    
    if(n < length(intercept_mod$fitted))
      NBIC <- -(BIC - useBIC(intercept_mod)) / (n - length(intercept_mod$fitted))
    else
      NBIC <- BIC - useBIC(intercept_mod)
    NBIC
  }

#' @export
NBIC.glm <-
  function(mod, intercept_mod, return.K = FALSE, nobs = NULL, c.hat = 1, ...){
    
    if(is.null(nobs)) {
      n <- length(mod$fitted)
    } else {n <- nobs}
    
    LL <- logLik(mod)[1]
    K <- attr(logLik(mod), "df")  #extract correct number of parameters included in model - this includes LM
    
    if(c.hat == 1) {
      BIC <- -2*LL + K * log(n)
    }
    if(c.hat > 1 && c.hat <= 4) {
      K <- K+1
      BIC <- -2*LL/c.hat + K * log(n)
    }
    
    if(c.hat > 4) stop("High overdispersion and model fit is questionable\n")
    if(c.hat < 1) stop("You should set \'c.hat\' to 1 if < 1, but values << 1 might also indicate lack of fit\n")
    
    ##check if negative binomial and add 1 to K for estimation of theta if glm( ) was used
    if(!is.na(charmatch(x="Negative Binomial", table=family(mod)$family))) {
      if(!identical(class(mod)[1], "negbin")) { #if not negbin, add + 1 because k of negbin was estimated glm.convert( ) screws up logLik
        K <- K+1
        BIC <- -2*LL + K * log(n)
      }
      if(c.hat != 1) stop("You should not use the c.hat argument with the negative binomial")
    }
    
    ##add 1 for theta parameter in negative binomial fit with glm( )
    
    ##check if gamma and add 1 to K for estimation of shape parameter if glm( ) was used
    if(identical(family(mod)$family, "Gamma") && c.hat > 1) stop("You should not use the c.hat argument with the gamma")
    
    ##an extra condition must be added to avoid adding a parameter for theta with negative binomial when glm.nb( ) is fit which estimates the correct number of parameters
    if(n < length(intercept_mod$fitted))
      NBIC <- -(BIC - useBIC(intercept_mod)) / (n - length(intercept_mod$fitted))
    else
      NBIC <- BIC - useBIC(intercept_mod)
    if(return.K == TRUE) NBIC[1] <- K #attributes the first element of BIC to K
    NBIC
  }