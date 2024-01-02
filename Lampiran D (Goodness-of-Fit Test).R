library(CASdatasets)
library(pscl)
library(dplyr)
library(quantreg)

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

# Train Test
set.seed(4)
index <- sample(x = 1:2, size = nrow(df1), replace = TRUE, prob = c(0.8, 0.2))
train <- df1[index == 1,]
test <- df1[index == 2,]
summary(train)
summary(test)

# Data Frame
CarAge = c(rep("1", 5), rep("2", 5), rep("3", 5), rep("4", 5))
DriverAge = rep(c("1", "2", "3", "4", "5"), 4)
RiskClass = data.frame("Kelas Risiko" = seq(20), CarAge, DriverAge)

freq = zeroinfl(ClaimNb ~ offset(log(train$Exposure)) + CarAge + DriverAge, data = train)
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

level = rank(RiskClass$NewProb)

qrmod = rq(log(ClaimAmount) ~ CarAge + DriverAge, data = train, tau = NewProb)

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

sev = glm(NewClaimAmount ~ CarAge + DriverAge, data = train, family = inverse.gaussian(link = "inverse"))
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

#####################################################################################

GoFT = test[,c(5,6,11)]

GoFT$Aktual = ifelse(is.na(GoFT$ClaimAmount), 0, GoFT$ClaimAmount)
GoFT = GoFT[,-3]

GoFT <- GoFT %>% mutate(EVPPPred = case_when(
  CarAge == 1 & DriverAge == 1 ~ Prem$EVPP[1],
  CarAge == 1 & DriverAge == 2 ~ Prem$EVPP[2],
  CarAge == 1 & DriverAge == 3 ~ Prem$EVPP[3],
  CarAge == 1 & DriverAge == 4 ~ Prem$EVPP[4],
  CarAge == 1 & DriverAge == 5 ~ Prem$EVPP[5],
  CarAge == 2 & DriverAge == 1 ~ Prem$EVPP[6],
  CarAge == 2 & DriverAge == 2 ~ Prem$EVPP[7],
  CarAge == 2 & DriverAge == 3 ~ Prem$EVPP[8],
  CarAge == 2 & DriverAge == 4 ~ Prem$EVPP[9],
  CarAge == 2 & DriverAge == 5 ~ Prem$EVPP[10],
  CarAge == 3 & DriverAge == 1 ~ Prem$EVPP[11],
  CarAge == 3 & DriverAge == 2 ~ Prem$EVPP[12],
  CarAge == 3 & DriverAge == 3 ~ Prem$EVPP[13],
  CarAge == 3 & DriverAge == 4 ~ Prem$EVPP[14],
  CarAge == 3 & DriverAge == 5 ~ Prem$EVPP[15],
  CarAge == 4 & DriverAge == 1 ~ Prem$EVPP[16],
  CarAge == 4 & DriverAge == 2 ~ Prem$EVPP[17],
  CarAge == 4 & DriverAge == 3 ~ Prem$EVPP[18],
  CarAge == 4 & DriverAge == 4 ~ Prem$EVPP[19],
  CarAge == 4 & DriverAge == 5 ~ Prem$EVPP[20]
))

GoFT <- GoFT %>% mutate(QPPPred = case_when(
  CarAge == 1 & DriverAge == 1 ~ Prem$QPP[1],
  CarAge == 1 & DriverAge == 2 ~ Prem$QPP[2],
  CarAge == 1 & DriverAge == 3 ~ Prem$QPP[3],
  CarAge == 1 & DriverAge == 4 ~ Prem$QPP[4],
  CarAge == 1 & DriverAge == 5 ~ Prem$QPP[5],
  CarAge == 2 & DriverAge == 1 ~ Prem$QPP[6],
  CarAge == 2 & DriverAge == 2 ~ Prem$QPP[7],
  CarAge == 2 & DriverAge == 3 ~ Prem$QPP[8],
  CarAge == 2 & DriverAge == 4 ~ Prem$QPP[9],
  CarAge == 2 & DriverAge == 5 ~ Prem$QPP[10],
  CarAge == 3 & DriverAge == 1 ~ Prem$QPP[11],
  CarAge == 3 & DriverAge == 2 ~ Prem$QPP[12],
  CarAge == 3 & DriverAge == 3 ~ Prem$QPP[13],
  CarAge == 3 & DriverAge == 4 ~ Prem$QPP[14],
  CarAge == 3 & DriverAge == 5 ~ Prem$QPP[15],
  CarAge == 4 & DriverAge == 1 ~ Prem$QPP[16],
  CarAge == 4 & DriverAge == 2 ~ Prem$QPP[17],
  CarAge == 4 & DriverAge == 3 ~ Prem$QPP[18],
  CarAge == 4 & DriverAge == 4 ~ Prem$QPP[19],
  CarAge == 4 & DriverAge == 5 ~ Prem$QPP[20]
))

GoFT$"EVPP Square Error" = (GoFT$EVPPPred - GoFT$Aktual)^2
GoFT$"QPP Square Error" = (GoFT$QPPPred - GoFT$Aktual)^2

RMSE = data.frame("Kelas Risiko" = seq(20))

EVPP.MSE = c(mean(GoFT$`EVPP Square Error`[GoFT$CarAge == 1 & GoFT$DriverAge == 1]),
             mean(GoFT$`EVPP Square Error`[GoFT$CarAge == 1 & GoFT$DriverAge == 2]),
             mean(GoFT$`EVPP Square Error`[GoFT$CarAge == 1 & GoFT$DriverAge == 3]),
             mean(GoFT$`EVPP Square Error`[GoFT$CarAge == 1 & GoFT$DriverAge == 4]),
             mean(GoFT$`EVPP Square Error`[GoFT$CarAge == 1 & GoFT$DriverAge == 5]),
             mean(GoFT$`EVPP Square Error`[GoFT$CarAge == 2 & GoFT$DriverAge == 1]),
             mean(GoFT$`EVPP Square Error`[GoFT$CarAge == 2 & GoFT$DriverAge == 2]),
             mean(GoFT$`EVPP Square Error`[GoFT$CarAge == 2 & GoFT$DriverAge == 3]),
             mean(GoFT$`EVPP Square Error`[GoFT$CarAge == 2 & GoFT$DriverAge == 4]),
             mean(GoFT$`EVPP Square Error`[GoFT$CarAge == 2 & GoFT$DriverAge == 5]),
             mean(GoFT$`EVPP Square Error`[GoFT$CarAge == 3 & GoFT$DriverAge == 1]),
             mean(GoFT$`EVPP Square Error`[GoFT$CarAge == 3 & GoFT$DriverAge == 2]),
             mean(GoFT$`EVPP Square Error`[GoFT$CarAge == 3 & GoFT$DriverAge == 3]),
             mean(GoFT$`EVPP Square Error`[GoFT$CarAge == 3 & GoFT$DriverAge == 4]),
             mean(GoFT$`EVPP Square Error`[GoFT$CarAge == 3 & GoFT$DriverAge == 5]),
             mean(GoFT$`EVPP Square Error`[GoFT$CarAge == 4 & GoFT$DriverAge == 1]),
             mean(GoFT$`EVPP Square Error`[GoFT$CarAge == 4 & GoFT$DriverAge == 2]),
             mean(GoFT$`EVPP Square Error`[GoFT$CarAge == 4 & GoFT$DriverAge == 3]),
             mean(GoFT$`EVPP Square Error`[GoFT$CarAge == 4 & GoFT$DriverAge == 4]),
             mean(GoFT$`EVPP Square Error`[GoFT$CarAge == 4 & GoFT$DriverAge == 5]))

RMSE$EVPP = sqrt(EVPP.MSE)

QPP.MSE = c(mean(GoFT$`QPP Square Error`[GoFT$CarAge == 1 & GoFT$DriverAge == 1]),
            mean(GoFT$`QPP Square Error`[GoFT$CarAge == 1 & GoFT$DriverAge == 2]),
            mean(GoFT$`QPP Square Error`[GoFT$CarAge == 1 & GoFT$DriverAge == 3]),
            mean(GoFT$`QPP Square Error`[GoFT$CarAge == 1 & GoFT$DriverAge == 4]),
            mean(GoFT$`QPP Square Error`[GoFT$CarAge == 1 & GoFT$DriverAge == 5]),
            mean(GoFT$`QPP Square Error`[GoFT$CarAge == 2 & GoFT$DriverAge == 1]),
            mean(GoFT$`QPP Square Error`[GoFT$CarAge == 2 & GoFT$DriverAge == 2]),
            mean(GoFT$`QPP Square Error`[GoFT$CarAge == 2 & GoFT$DriverAge == 3]),
            mean(GoFT$`QPP Square Error`[GoFT$CarAge == 2 & GoFT$DriverAge == 4]),
            mean(GoFT$`QPP Square Error`[GoFT$CarAge == 2 & GoFT$DriverAge == 5]),
            mean(GoFT$`QPP Square Error`[GoFT$CarAge == 3 & GoFT$DriverAge == 1]),
            mean(GoFT$`QPP Square Error`[GoFT$CarAge == 3 & GoFT$DriverAge == 2]),
            mean(GoFT$`QPP Square Error`[GoFT$CarAge == 3 & GoFT$DriverAge == 3]),
            mean(GoFT$`QPP Square Error`[GoFT$CarAge == 3 & GoFT$DriverAge == 4]),
            mean(GoFT$`QPP Square Error`[GoFT$CarAge == 3 & GoFT$DriverAge == 5]),
            mean(GoFT$`QPP Square Error`[GoFT$CarAge == 4 & GoFT$DriverAge == 1]),
            mean(GoFT$`QPP Square Error`[GoFT$CarAge == 4 & GoFT$DriverAge == 2]),
            mean(GoFT$`QPP Square Error`[GoFT$CarAge == 4 & GoFT$DriverAge == 3]),
            mean(GoFT$`QPP Square Error`[GoFT$CarAge == 4 & GoFT$DriverAge == 4]),
            mean(GoFT$`QPP Square Error`[GoFT$CarAge == 4 & GoFT$DriverAge == 5]))

RMSE$QPP = sqrt(QPP.MSE)

RMSE
