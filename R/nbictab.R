#' @export
nbictab <- function(cand.set, intercept_model, modnames = NULL, nobs = NULL, sort = TRUE, ...) {
  ##format list according to model class
  cand.set <- formatCands(cand.set)
  UseMethod("nbictab", cand.set)
}

#' @export
nbictab.AIClm <-
  function(cand.set, intercept_model, modnames = NULL, nobs = NULL, sort = TRUE, ...){
    
    ##check if named list if modnames are not supplied
    if(is.null(modnames)) {
      if(is.null(names(cand.set))) {
        modnames <- paste("Mod", 1:length(cand.set), sep = "")
        warning("\nModel names have been supplied automatically in the table\n")
      } else {
        modnames <- names(cand.set)
      }
    }
    
    
    ##add check to see whether response variable is the same for all models
    check.resp <- lapply(X = cand.set, FUN = function(b) formula(b)[2])
    if(length(unique(check.resp)) > 1) stop("\nYou must use the same response variable for all models\n")
    
    Results <- NULL
    Results <- data.frame(Modnames = modnames)                    #assign model names to first column
    Results$K <- unlist(lapply(X = cand.set, FUN = useBIC, return.K = TRUE,
                               nobs = nobs))     #extract number of parameters
    Results$BIC <- unlist(lapply(X = cand.set, FUN = useBIC, return.K = FALSE, 
                                 nobs = nobs))  #extract BIC                                      #
    Results$Delta_BIC <- Results$BIC - min(Results$BIC)            #compute delta BIC
    Results$ModelLik <- exp(-0.5*Results$Delta_BIC)                #compute model likelihood required to compute BIC weights
    Results$BICWt <- Results$ModelLik/sum(Results$ModelLik)        #compute BIC weights
    Results$NBIC <- unlist(lapply(X = cand.set, FUN = NBIC, intercept_mod=intercept_model, return.K = FALSE, 
                                  nobs = nobs))
    
    ##check if some models are redundant
    if(length(unique(Results$BIC)) != length(cand.set)) warning("\nCheck model structure carefully as some models may be redundant\n")
    
    ##extract LL
    Results$LL <- unlist(lapply(X = cand.set, FUN = function(i) logLik(i)[1]))
    
    if(sort)  {
      Results <- Results[order(Results[, 4]),] 	  #if sort=TRUE, models are ranked based on BIC weights
      Results$Cum.Wt <- cumsum(Results[, 6])                        #display cumulative sum of BIC weights
    } else {Results$Cum.Wt <- NULL}
    
    class(Results) <- c("bictab", "data.frame")
    return(Results)
  }

#' @export
nbictab.AICglm.lm <-
  function(cand.set, intercept_model, modnames = NULL, nobs = NULL, sort = TRUE, c.hat = 1, ...){
    
    ##check if named list if modnames are not supplied
    if(is.null(modnames)) {
      if(is.null(names(cand.set))) {
        modnames <- paste("Mod", 1:length(cand.set), sep = "")
        warning("\nModel names have been supplied automatically in the table\n")
      } else {
        modnames <- names(cand.set)
      }
    }
    
    
    ##add check to see whether response variable is the same for all models
    check.resp <- lapply(X = cand.set, FUN = function(b) formula(b)[2])
    if(length(unique(check.resp)) > 1) stop("\nYou must use the same response variable for all models\n")
    
    Results <- NULL
    Results <- data.frame(Modnames = modnames)                    #assign model names to first column
    Results$K <- unlist(lapply(X = cand.set, FUN = useBIC, return.K = TRUE, 
                               nobs = nobs, c.hat = c.hat))     #extract number of parameters
    Results$BIC <- unlist(lapply(X = cand.set, FUN = useBIC, return.K = FALSE,
                                 nobs = nobs, c.hat = c.hat))  #extract BIC                                      
    Results$Delta_BIC <- Results$BIC - min(Results$BIC)            #compute delta BIC
    Results$ModelLik <- exp(-0.5*Results$Delta_BIC)                #compute model likelihood required to compute BIC weights
    Results$BICWt <- Results$ModelLik/sum(Results$ModelLik)        #compute BIC weights
    Results$NBIC <- unlist(lapply(X = cand.set, FUN = NBIC, intercept_mod=intercept_model, return.K = FALSE, 
                                  nobs = nobs))
    
    ##check if some models are redundant
    if(length(unique(Results$BIC)) != length(cand.set)) warning("\nCheck model structure carefully as some models may be redundant\n")
    
    ##check if BIC and c.hat = 1
    if(c.hat == 1) {
      Results$LL <- unlist(lapply(X = cand.set, FUN=function(i) logLik(i)[1]))
    }
    
    ##rename correctly to QBIC and add column for c-hat
    if(c.hat > 1) {
      colnames(Results) <- c("Modnames", "K", "QBIC", "Delta_QBIC", "ModelLik", "QBICWt", "NBIC")
      LL <- unlist(lapply(X=cand.set, FUN = function(i) logLik(i)[1]))
      Results$Quasi.LL <- LL/c.hat
      Results$c_hat <- c.hat
    }      
    
    if(sort)  {
      Results <- Results[order(Results[, 4]),] 	  #if sort=TRUE, models are ranked based on BIC weights
      Results$Cum.Wt <- cumsum(Results[, 6])                        #display cumulative sum of BIC weights
    } else {Results$Cum.Wt <- NULL}
    
    class(Results) <- c("bictab", "data.frame")
    return(Results)
  }