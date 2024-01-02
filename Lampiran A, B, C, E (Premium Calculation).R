library(CASdatasets)
library(pscl)
library(dplyr)
library(quantreg)
library(AER)

data(freMTPLfreq)
data(freMTPLsev)
df = freMTPLfreq
dfsev = freMTPLsev

dfsev = dfsev %>%
  group_by(PolicyID) %>%
  summarise(ClaimAmount=sum(ClaimAmount)) %>%
  as.data.frame(ClaimAmount)
df = merge(df, dfsev, by = "PolicyID", all.x = TRUE)
df$NewClaimAmount = df$ClaimAmount/df$ClaimNb

df$CarAge <- cut(df$CarAge, breaks = c(0, 5, 10, 15, 100),
                 labels = c(1, 2, 3, 4), include.lowest = TRUE)

df$DriverAge <- cut(df$DriverAge, breaks = c(18, 25, 35, 50, 65, 100),
                    labels = c(1, 2, 3, 4, 5), include.lowest = TRUE)

df$Density <- cut(df$Density, breaks = c(0, 100, 27000),
                  labels = c(1, 2), include.lowest = TRUE)

summary(df)

unique_regions = sort(unique(df$Region))
for (i in 1:length(unique_regions)) {
  assign(paste0("df", i), subset(df, Region == unique_regions[i]))
}

dfsev = df[df$ClaimNb!=0,]
for (i in 1:length(unique_regions)) {
  assign(paste0("dfsev", i), subset(dfsev, Region == unique_regions[i]))
}

#####################################################################################
# Kode di bawah adalah untuk Region 1.
# Untuk Region lainnya dapat mengubah data frame df dan dfsev, yakni sebagai berikut:
# df1 -> df2
# dfsev1 -> dfsev2
#####################################################################################

# Data Frame
CarAge = c(rep("1", 5), rep("2", 5), rep("3", 5), rep("4", 5))
DriverAge = rep(c("1", "2", "3", "4", "5"), 4)
RiskClass = data.frame("Kelas Risiko" = seq(20), CarAge, DriverAge)

Npolicies = NULL
for (i in 1:20) {
  Npolicies = c(Npolicies, nrow(df1[df1$CarAge == RiskClass$CarAge[i] &
                                      df1$DriverAge == RiskClass$DriverAge[i],]))
}
RiskClass$Npolicies = Npolicies

Nclaims = NULL
for (i in 1:20) {
  Nclaims = c(Nclaims, sum(df1$ClaimNb[df1$CarAge == RiskClass$CarAge[i] &
                                         df1$DriverAge == RiskClass$DriverAge[i]]))
}
RiskClass$Nclaims = Nclaims

## Tes adanya kelebihan nol (excess zeros)
hist(df1$ClaimNb)

ovrdisp = glm(ClaimNb ~ CarAge + DriverAge, data = df1, family = poisson)
summary(ovrdisp)
dispersiontest(ovrdisp, alternative = "greater")
## Akhir dari tes kelebihan nol

freq = zeroinfl(ClaimNb ~ offset(log(df1$Exposure)) + CarAge + DriverAge, data = df1)
summary(freq)

lambda = c(exp(freq[["coefficients"]][["count"]][["(Intercept)"]]),
           exp(freq[["coefficients"]][["count"]][["(Intercept)"]]+freq[["coefficients"]][["count"]][["DriverAge2"]]),
           exp(freq[["coefficients"]][["count"]][["(Intercept)"]]+freq[["coefficients"]][["count"]][["DriverAge3"]]),
           exp(freq[["coefficients"]][["count"]][["(Intercept)"]]+freq[["coefficients"]][["count"]][["DriverAge4"]]),
           exp(freq[["coefficients"]][["count"]][["(Intercept)"]]+freq[["coefficients"]][["count"]][["DriverAge5"]]),
           exp(freq[["coefficients"]][["count"]][["(Intercept)"]]+freq[["coefficients"]][["count"]][["CarAge2"]]),
           exp(freq[["coefficients"]][["count"]][["(Intercept)"]]+freq[["coefficients"]][["count"]][["CarAge2"]]+freq[["coefficients"]][["count"]][["DriverAge2"]]),
           exp(freq[["coefficients"]][["count"]][["(Intercept)"]]+freq[["coefficients"]][["count"]][["CarAge2"]]+freq[["coefficients"]][["count"]][["DriverAge3"]]),
           exp(freq[["coefficients"]][["count"]][["(Intercept)"]]+freq[["coefficients"]][["count"]][["CarAge2"]]+freq[["coefficients"]][["count"]][["DriverAge4"]]),
           exp(freq[["coefficients"]][["count"]][["(Intercept)"]]+freq[["coefficients"]][["count"]][["CarAge2"]]+freq[["coefficients"]][["count"]][["DriverAge5"]]),
           exp(freq[["coefficients"]][["count"]][["(Intercept)"]]+freq[["coefficients"]][["count"]][["CarAge3"]]),
           exp(freq[["coefficients"]][["count"]][["(Intercept)"]]+freq[["coefficients"]][["count"]][["CarAge3"]]+freq[["coefficients"]][["count"]][["DriverAge2"]]),
           exp(freq[["coefficients"]][["count"]][["(Intercept)"]]+freq[["coefficients"]][["count"]][["CarAge3"]]+freq[["coefficients"]][["count"]][["DriverAge3"]]),
           exp(freq[["coefficients"]][["count"]][["(Intercept)"]]+freq[["coefficients"]][["count"]][["CarAge3"]]+freq[["coefficients"]][["count"]][["DriverAge4"]]),
           exp(freq[["coefficients"]][["count"]][["(Intercept)"]]+freq[["coefficients"]][["count"]][["CarAge3"]]+freq[["coefficients"]][["count"]][["DriverAge5"]]),
           exp(freq[["coefficients"]][["count"]][["(Intercept)"]]+freq[["coefficients"]][["count"]][["CarAge4"]]),
           exp(freq[["coefficients"]][["count"]][["(Intercept)"]]+freq[["coefficients"]][["count"]][["CarAge4"]]+freq[["coefficients"]][["count"]][["DriverAge2"]]),
           exp(freq[["coefficients"]][["count"]][["(Intercept)"]]+freq[["coefficients"]][["count"]][["CarAge4"]]+freq[["coefficients"]][["count"]][["DriverAge3"]]),
           exp(freq[["coefficients"]][["count"]][["(Intercept)"]]+freq[["coefficients"]][["count"]][["CarAge4"]]+freq[["coefficients"]][["count"]][["DriverAge4"]]),
           exp(freq[["coefficients"]][["count"]][["(Intercept)"]]+freq[["coefficients"]][["count"]][["CarAge4"]]+freq[["coefficients"]][["count"]][["DriverAge5"]]))

pi = c(exp(freq[["coefficients"]][["zero"]][["(Intercept)"]]),
       exp(freq[["coefficients"]][["zero"]][["(Intercept)"]]+freq[["coefficients"]][["zero"]][["DriverAge2"]]),
       exp(freq[["coefficients"]][["zero"]][["(Intercept)"]]+freq[["coefficients"]][["zero"]][["DriverAge3"]]),
       exp(freq[["coefficients"]][["zero"]][["(Intercept)"]]+freq[["coefficients"]][["zero"]][["DriverAge4"]]),
       exp(freq[["coefficients"]][["zero"]][["(Intercept)"]]+freq[["coefficients"]][["zero"]][["DriverAge5"]]),
       exp(freq[["coefficients"]][["zero"]][["(Intercept)"]]+freq[["coefficients"]][["zero"]][["CarAge2"]]),
       exp(freq[["coefficients"]][["zero"]][["(Intercept)"]]+freq[["coefficients"]][["zero"]][["CarAge2"]]+freq[["coefficients"]][["zero"]][["DriverAge2"]]),
       exp(freq[["coefficients"]][["zero"]][["(Intercept)"]]+freq[["coefficients"]][["zero"]][["CarAge2"]]+freq[["coefficients"]][["zero"]][["DriverAge3"]]),
       exp(freq[["coefficients"]][["zero"]][["(Intercept)"]]+freq[["coefficients"]][["zero"]][["CarAge2"]]+freq[["coefficients"]][["zero"]][["DriverAge4"]]),
       exp(freq[["coefficients"]][["zero"]][["(Intercept)"]]+freq[["coefficients"]][["zero"]][["CarAge2"]]+freq[["coefficients"]][["zero"]][["DriverAge5"]]),
       exp(freq[["coefficients"]][["zero"]][["(Intercept)"]]+freq[["coefficients"]][["zero"]][["CarAge3"]]),
       exp(freq[["coefficients"]][["zero"]][["(Intercept)"]]+freq[["coefficients"]][["zero"]][["CarAge3"]]+freq[["coefficients"]][["zero"]][["DriverAge2"]]),
       exp(freq[["coefficients"]][["zero"]][["(Intercept)"]]+freq[["coefficients"]][["zero"]][["CarAge3"]]+freq[["coefficients"]][["zero"]][["DriverAge3"]]),
       exp(freq[["coefficients"]][["zero"]][["(Intercept)"]]+freq[["coefficients"]][["zero"]][["CarAge3"]]+freq[["coefficients"]][["zero"]][["DriverAge4"]]),
       exp(freq[["coefficients"]][["zero"]][["(Intercept)"]]+freq[["coefficients"]][["zero"]][["CarAge3"]]+freq[["coefficients"]][["zero"]][["DriverAge5"]]),
       exp(freq[["coefficients"]][["zero"]][["(Intercept)"]]+freq[["coefficients"]][["zero"]][["CarAge4"]]),
       exp(freq[["coefficients"]][["zero"]][["(Intercept)"]]+freq[["coefficients"]][["zero"]][["CarAge4"]]+freq[["coefficients"]][["zero"]][["DriverAge2"]]),
       exp(freq[["coefficients"]][["zero"]][["(Intercept)"]]+freq[["coefficients"]][["zero"]][["CarAge4"]]+freq[["coefficients"]][["zero"]][["DriverAge3"]]),
       exp(freq[["coefficients"]][["zero"]][["(Intercept)"]]+freq[["coefficients"]][["zero"]][["CarAge4"]]+freq[["coefficients"]][["zero"]][["DriverAge4"]]),
       exp(freq[["coefficients"]][["zero"]][["(Intercept)"]]+freq[["coefficients"]][["zero"]][["CarAge4"]]+freq[["coefficients"]][["zero"]][["DriverAge5"]]))

pi = pi / (1 + pi)

Prob = pi + (1 - pi)*exp(-lambda)
RiskClass$Prob = Prob

NewProb = (0.99 - Prob) / (1 - Prob)
RiskClass$NewProb = NewProb

RiskClass
# Akhir dari Lampiran A

#####################################################################################

# Lanjutan untuk ke Lampiran B
level = rank(RiskClass$NewProb)

qrmod = rq(log(ClaimAmount) ~ CarAge + DriverAge, data = dfsev1, tau = NewProb)
coef(qrmod)
# Akhir dari Lampiran B

#####################################################################################

# Lanjutan untuk ke Lampiran C
Quant = c(qrmod[["coefficients"]][[1,level[1]]],
          qrmod[["coefficients"]][[1,level[2]]]+qrmod[["coefficients"]][[5,level[2]]],
          qrmod[["coefficients"]][[1,level[3]]]+qrmod[["coefficients"]][[6,level[3]]],
          qrmod[["coefficients"]][[1,level[4]]]+qrmod[["coefficients"]][[7,level[4]]],
          qrmod[["coefficients"]][[1,level[5]]]+qrmod[["coefficients"]][[8,level[5]]],
          qrmod[["coefficients"]][[1,level[6]]]+qrmod[["coefficients"]][[2,level[6]]],
          qrmod[["coefficients"]][[1,level[7]]]+qrmod[["coefficients"]][[2,level[7]]]+qrmod[["coefficients"]][[5,level[7]]],
          qrmod[["coefficients"]][[1,level[8]]]+qrmod[["coefficients"]][[2,level[8]]]+qrmod[["coefficients"]][[6,level[8]]],
          qrmod[["coefficients"]][[1,level[9]]]+qrmod[["coefficients"]][[2,level[9]]]+qrmod[["coefficients"]][[7,level[9]]],
          qrmod[["coefficients"]][[1,level[10]]]+qrmod[["coefficients"]][[2,level[10]]]+qrmod[["coefficients"]][[8,level[10]]],
          qrmod[["coefficients"]][[1,level[11]]]+qrmod[["coefficients"]][[3,level[11]]],
          qrmod[["coefficients"]][[1,level[12]]]+qrmod[["coefficients"]][[3,level[12]]]+qrmod[["coefficients"]][[5,level[12]]],
          qrmod[["coefficients"]][[1,level[13]]]+qrmod[["coefficients"]][[3,level[13]]]+qrmod[["coefficients"]][[6,level[13]]],
          qrmod[["coefficients"]][[1,level[14]]]+qrmod[["coefficients"]][[3,level[14]]]+qrmod[["coefficients"]][[7,level[14]]],
          qrmod[["coefficients"]][[1,level[15]]]+qrmod[["coefficients"]][[3,level[15]]]+qrmod[["coefficients"]][[8,level[15]]],
          qrmod[["coefficients"]][[1,level[16]]]+qrmod[["coefficients"]][[4,level[16]]],
          qrmod[["coefficients"]][[1,level[17]]]+qrmod[["coefficients"]][[4,level[17]]]+qrmod[["coefficients"]][[5,level[17]]],
          qrmod[["coefficients"]][[1,level[18]]]+qrmod[["coefficients"]][[4,level[18]]]+qrmod[["coefficients"]][[6,level[18]]],
          qrmod[["coefficients"]][[1,level[19]]]+qrmod[["coefficients"]][[4,level[19]]]+qrmod[["coefficients"]][[7,level[19]]],
          qrmod[["coefficients"]][[1,level[20]]]+qrmod[["coefficients"]][[4,level[20]]]+qrmod[["coefficients"]][[8,level[20]]])

Quant = exp(Quant)

Quant
# Akhir dari Lampiran C

#####################################################################################

# Lanjutan untuk ke Lampiran E
sev = glm(NewClaimAmount ~ CarAge + DriverAge, data = dfsev1, family = inverse.gaussian(link = "inverse"))
summary(sev)

EX = c(sev[["coefficients"]][["(Intercept)"]],
       sev[["coefficients"]][["(Intercept)"]]+sev[["coefficients"]][["DriverAge2"]],
       sev[["coefficients"]][["(Intercept)"]]+sev[["coefficients"]][["DriverAge3"]],
       sev[["coefficients"]][["(Intercept)"]]+sev[["coefficients"]][["DriverAge4"]],
       sev[["coefficients"]][["(Intercept)"]]+sev[["coefficients"]][["DriverAge5"]],
       sev[["coefficients"]][["(Intercept)"]]+sev[["coefficients"]][["CarAge2"]],
       sev[["coefficients"]][["(Intercept)"]]+sev[["coefficients"]][["CarAge2"]]+sev[["coefficients"]][["DriverAge2"]],
       sev[["coefficients"]][["(Intercept)"]]+sev[["coefficients"]][["CarAge2"]]+sev[["coefficients"]][["DriverAge3"]],
       sev[["coefficients"]][["(Intercept)"]]+sev[["coefficients"]][["CarAge2"]]+sev[["coefficients"]][["DriverAge4"]],
       sev[["coefficients"]][["(Intercept)"]]+sev[["coefficients"]][["CarAge2"]]+sev[["coefficients"]][["DriverAge5"]],
       sev[["coefficients"]][["(Intercept)"]]+sev[["coefficients"]][["CarAge3"]],
       sev[["coefficients"]][["(Intercept)"]]+sev[["coefficients"]][["CarAge3"]]+sev[["coefficients"]][["DriverAge2"]],
       sev[["coefficients"]][["(Intercept)"]]+sev[["coefficients"]][["CarAge3"]]+sev[["coefficients"]][["DriverAge3"]],
       sev[["coefficients"]][["(Intercept)"]]+sev[["coefficients"]][["CarAge3"]]+sev[["coefficients"]][["DriverAge4"]],
       sev[["coefficients"]][["(Intercept)"]]+sev[["coefficients"]][["CarAge3"]]+sev[["coefficients"]][["DriverAge5"]],
       sev[["coefficients"]][["(Intercept)"]]+sev[["coefficients"]][["CarAge4"]],
       sev[["coefficients"]][["(Intercept)"]]+sev[["coefficients"]][["CarAge4"]]+sev[["coefficients"]][["DriverAge2"]],
       sev[["coefficients"]][["(Intercept)"]]+sev[["coefficients"]][["CarAge4"]]+sev[["coefficients"]][["DriverAge3"]],
       sev[["coefficients"]][["(Intercept)"]]+sev[["coefficients"]][["CarAge4"]]+sev[["coefficients"]][["DriverAge4"]],
       sev[["coefficients"]][["(Intercept)"]]+sev[["coefficients"]][["CarAge4"]]+sev[["coefficients"]][["DriverAge5"]])

EX = 1 / EX

EN = (1 - pi)*lambda

ES = EN*EX

Prem = data.frame("Kelas Risiko" = seq(20))
Prem$PurePrem = ES
Prem$Quant = Quant
Prem$"Q-PP" = Quant - ES
Prem$QPP = Prem$PurePrem + 0.03*Prem$`Q-PP`
alpha = sum(Prem$QPP) / sum(Prem$PurePrem) - 1
Prem$EVPP = (1 + alpha)*Prem$PurePrem

Prem
# Akhir dari Lampiran E
