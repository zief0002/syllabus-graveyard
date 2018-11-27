
# Read in mnSchools.csv
mn = read.csv(file = "~/Google Drive/Documents/github/EPsy-8252/data/mnSchools.csv")
head(mn)


# Fit models
lm.a = lm(gradRate ~ 1 + public, data = mn)
lm.b = lm(gradRate ~ 1 + sat, data = mn)
lm.c = lm(gradRate ~ 1 + tuition, data = mn)
lm.d = lm(gradRate ~ 1 + public + sat, data = mn)
lm.e = lm(gradRate ~ 1 + tuition + sat, data = mn)
lm.f = lm(gradRate ~ 1 + public + tuition, data = mn)
lm.g = lm(gradRate ~ 1 + public + sat + tuition, data = mn)


AICc(lm.a)
AICc(lm.b)
AICc(lm.c)
AICc(lm.d)
AICc(lm.e)
AICc(lm.f)
AICc(lm.g)


library(AICcmodavg)

# AICc Table for Model Selection
myAIC = aictab(
  cand.set = list(lm.a, lm.b, lm.c, lm.d, lm.e, lm.f, lm.g),
  modnames = c("Model A", "Model B", "Model C", "Model D", "Model E", "Model F", "Model G")
)


# Compute evidence ratios
evidence(myAIC, model.high = "Model E", model.low = "Model G")
evidence(myAIC, model.high = "Model E", model.low = "Model D")
evidence(myAIC, model.high = "Model E", model.low = "Model B")
evidence(myAIC, model.high = "Model E", model.low = "Model F")
evidence(myAIC, model.high = "Model E", model.low = "Model C")
evidence(myAIC, model.high = "Model E", model.low = "Model A")