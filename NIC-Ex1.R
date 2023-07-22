library(RMark)
library(AICcmodavg)
library(NIC)

# bring in the data file
fawns <- convert.inp("http://www.montana.edu/rotella/documents/502/lab02-fawns.inp",
                     covariates = c("area", "sex", "mass", "length"))

# preprocessing
fawns$length.sq <- (fawns$length)^2
head(fawns)
fawns$area <- factor(fawns$area, 
                     levels = c(0, 1), 
                     labels = c("control", "treatment"))
fawns$sex  <- factor(fawns$sex,
                     levels = c(0, 1),
                     labels = c("female", "male"))
fawns$fate <- factor(as.numeric(fawns$ch), 
                     levels = c(11, 10),
                     labels = c("died", "lived"))
fawns$Fate <- 2 - as.numeric(fawns$fate)
fawns2 = fawns[fawns$mass < 70, ]
summary(fawns2)

#creating some models
S.dot <- glm(fate ~ 1, data = fawns2, family = binomial)
S.area <- glm(fate ~ area, data = fawns2, family = binomial)
S.mass <- glm(fate ~ mass, data = fawns2, family = binomial)
S.length <- glm(fate ~ length, data = fawns2, family = binomial)
S.length.sq <- glm(fate ~ length + length.sq, data = fawns2, family = binomial)
S.sex <- glm(fate ~ sex, data = fawns2, family = binomial)
S.area.length <- glm(fate ~ area + length, data = fawns2, family = binomial)
S.length.mass <- glm(fate ~ length + mass, data = fawns2, family = binomial)
S.sex.length <- glm(fate ~ sex + length, data = fawns2, family = binomial)
S.sex.by.length <- glm(fate ~ sex:length, data = fawns2, family = binomial)
S.sex.x.length <- glm(fate ~ sex * length, data = fawns2, family = binomial)

##set up named list
Cand.mods <- list("S.dot" = S.dot, 
                  "S.area" = S.area, 
                  "S.mass" = S.mass, 
                  "S.length" = S.length,
                  "S.length.sq" = S.length.sq, 
                  "S.sex" = S.sex,
                  "S.area.length" = S.area.length,
                  "S.length.mass" = S.length.mass, 
                  "S.sex.length" = S.sex.length,
                  "S.sex.by.length" = S.sex.by.length,
                  "S.sex.x.length" = S.sex.x.length)

##compute comparing tables
AICc.table <- naictab(cand.set = Cand.mods, second.ord = FALSE)
AICc.table

AICc.table <- nbictab(cand.set = Cand.mods, intercept_model=S.dot, second.ord = FALSE)
AICc.table
