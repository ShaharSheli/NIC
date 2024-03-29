#' @export
naictab <- function(cand.set, modnames = NULL, second.ord = TRUE, nobs = NULL, sort = TRUE, ...) {
  ##format list according to model class
  cand.set <- formatCands(cand.set)
  UseMethod("naictab", cand.set)
}

#' @export
naictab.AIClm <-
  function(cand.set, modnames = NULL, second.ord = TRUE, nobs = NULL, sort = TRUE, ...){

    ##check if named list if modnames are not supplied
    if(is.null(modnames)) {
      if(is.null(names(cand.set))) {
        modnames <- paste("Mod", 1:length(cand.set), sep = "")
        warning("\nModel names have been supplied automatically in the table\n")
      } else {
        modnames <- names(cand.set)
      }
    }


    # ##add check to see whether response variable is the same for all models
    # check.resp <- lapply(X = cand.set, FUN = function(b) formula(b)[2])
    # if(length(unique(check.resp)) > 1) stop("\nYou must use the same response variable for all models\n")

    Results <- NULL
    Results <- data.frame(Modnames = modnames)                    #assign model names to first column
    Results$K <- unlist(lapply(X = cand.set, FUN = AICc, return.K = TRUE,
                               second.ord = second.ord, nobs = nobs))     #extract number of parameters
    Results$AICc <- unlist(lapply(X = cand.set, FUN = AICc, return.K = FALSE,
                                  second.ord = second.ord, nobs = nobs))  #extract AICc                                      #
    Results$Delta_AICc <- Results$AICc - min(Results$AICc)            #compute delta AICc
    Results$ModelLik <- exp(-0.5*Results$Delta_AICc)                #compute model likelihood required to compute Akaike weights
    Results$AICcWt <- Results$ModelLik/sum(Results$ModelLik)        #compute Akaike weights
    Results$NAIC <- Results$AICc/unlist(lapply(X = cand.set, FUN = nobs))

    ##check if some models are redundant
    if(length(unique(Results$AICc)) != length(cand.set)) warning("\nCheck modelefully as some mod structure carels may be redundant\n")

    ##extract LL
    Results$LL <- unlist(lapply(X = cand.set, FUN = function(i) logLik(i)[1]))

    ##rename correctly to AIC
    if(second.ord == FALSE) {
      colnames(Results)[1:7] <- c("Modnames", "K", "AIC", "Delta_AIC", "ModelLik", "AICWt", "NAIC")
    }


    if(sort)  {
      Results <- Results[order(Results[, 4]),] 	  #if sort=TRUE, models are ranked based on Akaike weights
      Results$Cum.Wt <- cumsum(Results[, 6])                        #display cumulative sum of Akaike weights
    } else {Results$Cum.Wt <- NULL}


    class(Results) <- c("aictab", "data.frame")
    return(Results)
  }

#' @export
naictab.AICglm.lm <-
  function(cand.set, modnames = NULL, second.ord = TRUE, nobs = NULL, sort = TRUE, c.hat = 1, ...){
    
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
    Results$K <- unlist(lapply(X = cand.set, FUN = AICc, return.K = TRUE, second.ord = second.ord,
                               nobs = nobs, c.hat = c.hat))     #extract number of parameters
    Results$AICc <- unlist(lapply(X = cand.set, FUN = AICc, return.K = FALSE, second.ord = second.ord,
                                  nobs = nobs, c.hat = c.hat))  #extract AICc                                      
    Results$Delta_AICc <- Results$AICc - min(Results$AICc)            #compute delta AICc
    Results$ModelLik <- exp(-0.5*Results$Delta_AICc)                #compute model likelihood required to compute Akaike weights
    Results$AICcWt <- Results$ModelLik/sum(Results$ModelLik)        #compute Akaike weights
    Results$NAIC <- Results$AICc/unlist(lapply(X = cand.set, FUN = nobs))
    
    ##check if some models are redundant
    if(length(unique(Results$AICc)) != length(cand.set)) warning("\nCheck model structure carefully as some models may be redundant\n")
    
    ##check if AICc and c.hat = 1
    if(second.ord == TRUE && c.hat == 1) {
      Results$LL <- unlist(lapply(X = cand.set, FUN=function(i) logLik(i)[1]))
    }
    
    ##rename correctly to QAICc and add column for c-hat
    if(second.ord == TRUE && c.hat > 1) {
      colnames(Results) <- c("Modnames", "K", "QAICc", "Delta_QAICc", "ModelLik", "QAICcWt", "NAIC")
      LL <- unlist(lapply(X=cand.set, FUN = function(i) logLik(i)[1]))
      Results$Quasi.LL <- LL/c.hat
      Results$c_hat <- c.hat
    }      
    
    ##rename correctly to AIC
    if(second.ord == FALSE && c.hat == 1) {
      colnames(Results)<-c("Modnames", "K", "AIC", "Delta_AIC", "ModelLik", "AICWt", "NAIC")
      Results$LL <- unlist(lapply(X=cand.set, FUN=function(i) logLik(i)[1]))      
    }  
    
    ##rename correctly to QAIC and add column for c-hat
    if(second.ord == FALSE && c.hat > 1) {
      colnames(Results)<-c("Modnames", "K", "QAIC", "Delta_QAIC", "ModelLik", "QAICWt", "NAIC")
      LL <- unlist(lapply(X=cand.set, FUN=function(i) logLik(i)[1]))
      Results$Quasi.LL <- LL/c.hat
      Results$c_hat<-c.hat
    }
    
    
    if(sort)  {
      Results <- Results[order(Results[, 4]),] 	  #if sort=TRUE, models are ranked based on Akaike weights
      Results$Cum.Wt <- cumsum(Results[, 6])                        #display cumulative sum of Akaike weights
    } else {Results$Cum.Wt <- NULL}
    
    
    class(Results) <- c("aictab", "data.frame")
    return(Results)
  }