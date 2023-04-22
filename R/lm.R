# aictab.AIClm <-
#   function(cand.set, modnames = NULL, second.ord = TRUE, nobs = NULL, sort = TRUE, ...){
#     
#     ##check if named list if modnames are not supplied
#     if(is.null(modnames)) {
#       if(is.null(names(cand.set))) {
#         modnames <- paste("Mod", 1:length(cand.set), sep = "")
#         warning("\nModel names have been supplied automatically in the table\n")
#       } else {
#         modnames <- names(cand.set)
#       }
#     }
#     
#     
#     # ##add check to see whether response variable is the same for all models
#     # check.resp <- lapply(X = cand.set, FUN = function(b) formula(b)[2])
#     # if(length(unique(check.resp)) > 1) stop("\nYou must use the same response variable for all models\n")
#     
#     Results <- NULL
#     Results <- data.frame(Modnames = modnames)                    #assign model names to first column
#     Results$K <- unlist(lapply(X = cand.set, FUN = AICc, return.K = TRUE,
#                                second.ord = second.ord, nobs = nobs))     #extract number of parameters
#     Results$AICc <- unlist(lapply(X = cand.set, FUN = AICc, return.K = FALSE, 
#                                   second.ord = second.ord, nobs = nobs))  #extract AICc                                      #
#     Results$Delta_AICc <- Results$AICc - min(Results$AICc)            #compute delta AICc
#     Results$ModelLik <- exp(-0.5*Results$Delta_AICc)                #compute model likelihood required to compute Akaike weights
#     Results$AICcWt <- Results$ModelLik/sum(Results$ModelLik)        #compute Akaike weights
#     
#     ##check if some models are redundant
#     if(length(unique(Results$AICc)) != length(cand.set)) warning("\nCheck modelefully as some mod structure carels may be redundant\n")
#     
#     ##extract LL
#     Results$LL <- unlist(lapply(X = cand.set, FUN = function(i) logLik(i)[1]))
#     
#     ##rename correctly to AIC
#     if(second.ord == FALSE) {
#       colnames(Results)[1:6] <- c("Modnames", "K", "AIC", "Delta_AIC", "ModelLik", "AICWt")
#     }  
#     
#     
#     if(sort)  {
#       Results <- Results[order(Results[, 4]),] 	  #if sort=TRUE, models are ranked based on Akaike weights
#       Results$Cum.Wt <- cumsum(Results[, 6])                        #display cumulative sum of Akaike weights
#     } else {Results$Cum.Wt <- NULL}
#     
#     
#     class(Results) <- c("aictab", "data.frame")
#     return(Results)
#   }


X <- datasets::cars
set.seed(122)
speed.c = scale(cars$speed, center=TRUE, scale=FALSE)
lm1 <- lm(formula = dist ~ speed.c, data = cars)
summary(lm1)
