library(AICcmodavg)
library(NIC)

#creating some models
S.dot <- lm(Temp ~ 1, data = airquality)
S.1 <- lm(Temp ~ Wind + Day + Month, data = airquality)
S.2 <- lm(Temp ~ Wind + Day + Month + Ozone + Solar.R, data = airquality)

Cand.mods <- list("S.dot" = S.dot, 
                  "S.1" = S.1, 
                  "S.2" = S.2)

##compute comparing tables
AICc.table <- naictab(cand.set = Cand.mods, second.ord = FALSE)
AICc.table

AICc.table <- nbictab(cand.set = Cand.mods, intercept_model=S.dot, second.ord = FALSE)
AICc.table