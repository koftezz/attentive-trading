install.packages('quantmod')
install.packages("PerformanceAnalytics")
install.packages("PortfolioAnalytics")
install.packages("FactoMineR")
install.packages("dygraphs")
install.packages("riskParityPortfolio")


library(quantmod) 
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(riskParityPortfolio)
library(FactoMineR)
library(dygraphs)
library("readxl")



markowitz = function(x){
  port <- portfolio.spec(assets = colnames((x)))
  port <- add.constraint(portfolio = port, type = "full_investment")
  port <- add.constraint(portfolio = port, type = "long_only")
  port <- add.constraint(portfolio = port,type = "box", min = 0.0,max = 0.5)      
  port <- add.objective(portfolio = port, type = "return", name = "mean")
  port <- add.objective(portfolio = port, type = "risk", name = "StdDev")
  set.seed(Sys.time())
  minVarOpt = optimize.portfolio(R = x, portfolio = port, optimize_method = "random")
  markovitz <- Return.portfolio(x, weight = extractWeights(minVarOpt))
}

getIVP <- function(covMat) {
  invDiag <- 1/diag(as.matrix(covMat))
  weights <- invDiag/sum(invDiag)
  return(weights)
}

getClusterVar <- function(covMat, cItems) {
  covMatSlice <- covMat[cItems, cItems]
  weights <- getIVP(covMatSlice)
  cVar <- t(weights) %*% as.matrix(covMatSlice) %*% weights
  return(cVar)
}

getRecBipart <- function(covMat, sortIx) {
  # keeping track of w in the global environment
  assign("w", value = rep(1, ncol(covMat)), envir = .GlobalEnv)
  recurFun(covMat, sortIx)
  return(w)
}

recurFun <- function(covMat, sortIx) {
  subIdx <- 1:trunc(length(sortIx)/2)
  cItems0 <- sortIx[subIdx]
  cItems1 <- sortIx[-subIdx]
  cVar0 <- getClusterVar(covMat, cItems0)
  cVar1 <- getClusterVar(covMat, cItems1)
  alpha <- 1 - cVar0/(cVar0 + cVar1)
  
  # scoping mechanics using w as a free parameter
  w[cItems0] <<- w[cItems0] * alpha
  w[cItems1] <<- w[cItems1] * (1-alpha)
  
  if(length(cItems0) > 1) {
    recurFun(covMat, cItems0)
  }
  if(length(cItems1) > 1) {
    recurFun(covMat, cItems1)
  }
}

todaysDate = Sys.Date()

setwd(getwd())

asset_list <- read_excel("./AssetReturns.xlsx")
asset_list <- as.data.table(asset_list)

signals <- read_excel("./Signals.xlsx")
signals <- as.data.table(signals)


signals <- merge(asset_list, signals, by=c("date"))
signals <- signals[!is.na(GOLDpred)]

signals <- signals[, .(date,
               Gold_Signal = Gold_Index * GOLDpred,
               BIST_Signal = BIST_Index * BISTpred,
               DAX_Signal = DAX_Index * DAXpred,
               EWZ_Signal = EWZ_Index * EWZpred)]

# djList = read.csv("DowJonesList.csv")
# tickers=djList$ticker
tickers=c('TLT','QQQ','SPY','EWG','EEM','EMB','GLD','HYG')

myAssets <-lapply(tickers, function(x) {getSymbols(x, from = "2014-12-31", 
                                                   periodicity = "weekly",auto.assign=FALSE)} )

adjustedPrices <- lapply(myAssets, Ad)
adjustedPrices <- do.call(merge, adjustedPrices)

assetReturns <- Return.calculate(adjustedPrices)[-1]
assetReturns <- as.xts.data.table(asset_list)


equalWeightReturnList = xts()
minVarReturnList = xts()
inverseVolReturnList = xts()
rppReturnList = xts()
hrpReturnList = xts()
inversePcaReturnList = xts()
pcaReturnList = xts()

rppWeightList = data.frame()
hrpWeightList = data.frame()
inverseVolWeightList = data.frame()

colMeans(assetReturns)*52
apply(assetReturns, 2, sd)*sqrt(52)

# batuhan
rppWeightList_1 <- as.data.table(asset_list)
rppWeightList_1 <- rppWeightList_1[,.(date)]
rppWeightList_1 <- merge(rppWeightList_1, rppWeightList, by="date", all.x = TRUE)
rppWeightList_1 <- rppWeightList_1[date >= "2018-05-21",]
rppWeightList_1[, Gold_Index := na.locf(Gold_Index, fromLast = FALSE)]
rppWeightList_1[, BIST_Index := na.locf(BIST_Index, fromLast = FALSE)]
rppWeightList_1[, DAX_Index := na.locf(DAX_Index, fromLast = FALSE)]
rppWeightList_1[, EWZ_Index := na.locf(EWZ_Index, fromLast = FALSE)]

rppWeightList_1 <- merge(rppWeightList_1, signals, by="date")
rppWeightList_1 <- rppWeightList_1[, .(date,
                       Gold_Final = Gold_Index * Gold_Signal,
                       BIST_Final = BIST_Index * BIST_Signal,
                       DAX_Final = DAX_Index * DAX_Signal,
                       EWZ_Final = EWZ_Index * EWZ_Signal)]

rppWeightList_1 <- as.xts.data.table(rppWeightList_1[, .(date, portfolio.returns = Gold_Final + BIST_Final + DAX_Final + EWZ_Final)])


hrpWeightList_1 <- as.data.table(asset_list)
hrpWeightList_1 <- hrpWeightList_1[,.(date)]
hrpWeightList_1 <- merge(hrpWeightList_1, hrpWeightList, by="date", all.x = TRUE)
hrpWeightList_1 <- hrpWeightList_1[date >= "2018-05-21",]
hrpWeightList_1[, Gold_Index := na.locf(X1, fromLast = FALSE)]
hrpWeightList_1[, BIST_Index := na.locf(X2, fromLast = FALSE)]
hrpWeightList_1[, DAX_Index := na.locf(X3, fromLast = FALSE)]
hrpWeightList_1[, EWZ_Index := na.locf(X4, fromLast = FALSE)]

hrpWeightList_1 <- merge(hrpWeightList_1, signals, by="date")
hrpWeightList_1 <- hrpWeightList_1[, .(date,
                                       Gold_Final = Gold_Index * Gold_Signal,
                                       BIST_Final = BIST_Index * BIST_Signal,
                                       DAX_Final = DAX_Index * DAX_Signal,
                                       EWZ_Final = EWZ_Index * EWZ_Signal)]
hrpWeightList_1 <- as.xts.data.table(hrpWeightList_1[, .(date, portfolio.returns = Gold_Final + BIST_Final + DAX_Final + EWZ_Final)])



inverseVolWeightList_1 <- as.data.table(asset_list)
inverseVolWeightList_1 <- inverseVolWeightList_1[,.(date)]
inverseVolWeightList_1 <- merge(inverseVolWeightList_1, inverseVolWeightList, by="date", all.x = TRUE)
inverseVolWeightList_1 <- inverseVolWeightList_1[date >= "2018-05-21",]
inverseVolWeightList_1[, Gold_Index := na.locf(Gold_Index, fromLast = FALSE)]
inverseVolWeightList_1[, BIST_Index := na.locf(BIST_Index, fromLast = FALSE)]
inverseVolWeightList_1[, DAX_Index := na.locf(DAX_Index, fromLast = FALSE)]
inverseVolWeightList_1[, EWZ_Index := na.locf(EWZ_Index, fromLast = FALSE)]

inverseVolWeightList_1 <- merge(inverseVolWeightList_1, signals, by="date")
inverseVolWeightList_1 <- inverseVolWeightList_1[, .(date,
                                       Gold_Final = Gold_Index * Gold_Signal,
                                       BIST_Final = BIST_Index * BIST_Signal,
                                       DAX_Final = DAX_Index * DAX_Signal,
                                       EWZ_Final = EWZ_Index * EWZ_Signal)]
inverseVolWeightList_1 <- as.xts.data.table(inverseVolWeightList_1[, .(date, portfolio.returns = Gold_Final + BIST_Final + DAX_Final + EWZ_Final)])




dateIndex = nrow(assetReturns)

trainWindow = 8
testWindow = 2

while(dateIndex>(trainWindow+testWindow)){
  
  trainReturns = assetReturns[(dateIndex-testWindow-trainWindow):(dateIndex-testWindow),]
  testReturns = assetReturns[(dateIndex-testWindow+1):dateIndex,]
  
  port <- portfolio.spec(assets = colnames((trainReturns)))
  equalWeightReturns <- Return.portfolio(testReturns, weight = port$assets, rebalance_on = "months")
  equalWeightReturnList = append(equalWeightReturnList,equalWeightReturns)
  
  #Minimum Variance Optimization:
  # port <- add.constraint(portfolio = port, type = "full_investment")
  # port <- add.constraint(portfolio = port, type = "long_only")
  # port <- add.constraint(portfolio = port,type = "box", min = 0.0,max = 0.75)      
  # 
  # port <- add.objective(portfolio = port, type = "return", name = "mean")
  # port <- add.objective(portfolio = port, type = "risk", name = "StdDev")
  
  # set.seed(Sys.time())
  # minVarOpt <- optimize.portfolio(R = trainReturns, portfolio = port, optimize_method = "random")
  # minVarReturns <- Return.portfolio(testReturns, weight = extractWeights(minVarOpt), rebalance_on = "months")
  # minVarReturnList = append(minVarReturnList,minVarReturns)
  
  #Inverse Volatility:
  invVol = 1/apply(X = trainReturns,FUN = sd,MARGIN = 2)
  # invVol[2] = 0
  inverseVolReturns <- Return.portfolio(testReturns, weight = invVol/sum(invVol), rebalance_on = "months")
  inverseVolReturnList =append(inverseVolReturnList, inverseVolReturns)
  new <- data.frame(date = index(assetReturns[dateIndex]), t(invVol/sum(invVol)))
  inverseVolWeightList = rbind(inverseVolWeightList,new)
  
  # Risk Parity Portfolio:
  Sigma <- cov(trainReturns)
  rpp <- riskParityPortfolio(Sigma)
  rppReturns <- Return.portfolio(testReturns, weight = rpp$w, rebalance_on = "months")
  rppReturnList =append(rppReturnList, rppReturns)
  new <- data.frame(date = index(assetReturns[dateIndex]), t(rpp$w))
  rppWeightList=rbind(rppWeightList,new)
  
  #Hierarchical Risk Parity:
  covMat <- cov(trainReturns)
  corMat <- cor(trainReturns)
  
  clustOrder <- hclust(dist(corMat), method = 'centroid')$order
  hrpWeights <- getRecBipart(corMat, clustOrder)
  hrpReturns <- Return.portfolio(testReturns, weight = hrpWeights, rebalance_on = "months")
  hrpReturnList =append(hrpReturnList, hrpReturns)
  new <- data.frame(date = index(assetReturns[dateIndex]), t(hrpWeights))
  hrpWeightList=rbind(hrpWeightList,new)
  
  
  pca = PCA(trainReturns)
  pca_var = pca$var$contrib
  inversePcaVar = 1/pca_var
  # inversePcaVar[2, 1]
  invPcaWeights = inversePcaVar[,1]/100
  x = as.vector(invPcaWeights/sum(invPcaWeights))
  inversePcaReturns <- Return.portfolio(testReturns, weight = x, rebalance_on = "months")
  inversePcaReturnList =append(inversePcaReturnList, inversePcaReturns)
   
  pcaWeights = pca_var[,1]/100
  pcaReturns <- Return.portfolio(testReturns, weight = pcaWeights, rebalance_on = "months")
  pcaReturnList = append(pcaReturnList, pcaReturns)
  
  dateIndex = dateIndex - testWindow
}

plot(100*cumprod(1+equalWeightReturnList),type="l", lwd=5, 
     ylim = c(100*min(cumprod(1+equalWeightReturnList)),100*max(cumprod(1+equalWeightReturnList))+10), 
     main = "Cumulative Returns")
lines(100*cumprod(1+inverseVolReturnList),col = "red")
lines(100*cumprod(1+rppReturnList),col = "green")
lines(100*cumprod(1+hrpReturnList),col = "brown")
lines(100*cumprod(1+pcaReturnList),col = "blue")
lines(100*cumprod(1+inversePcaReturnList),col = "orange")

addLegend("topleft", on=1, 
          legend.names = c("Equal", "Inv. Vol.","RPP", "HRP", "PCA", "Inv. PCA"), 
          lty=c(1, 1), lwd=c(2, 1),
          col=c( "black", "red", "green", "brown","blue","orange"))

#Risk free rate i 2% aldim. Bunu daha iyi hesaplayabiliriz.
table.AnnualizedReturns(R = equalWeightReturnList, Rf = 0.02/252)
#table.AnnualizedReturns(R = minVarReturnList, Rf = 0.02/252)
table.AnnualizedReturns(R = inverseVolReturnList, Rf = 0.02/252)
table.AnnualizedReturns(R = rppReturnList, Rf = 0.02/252)
table.AnnualizedReturns(R = hrpReturnList, Rf = 0.02/252)
table.AnnualizedReturns(R = inversePcaReturnList, Rf = 0.02/252)
table.AnnualizedReturns(R = pcaReturnList, Rf = 0.02/252)


# install.packages("scales", dependencies=TRUE)
# library(scales)

plot(100*cumprod(1+equalWeightReturnList),type="l", lwd=5, 
     ylim = c(100*min(cumprod(1+equalWeightReturnList)),100*max(cumprod(1+equalWeightReturnList))+10), 
     main = "Cumulative Returns")
lines(100*cumprod(1+rppWeightList_1 ),col = "green")
lines(100*cumprod(1+hrpWeightList_1 ),col = "brown")
lines(100*cumprod(1+inverseVolWeightList_1),col = "blue")
# lines(100*cumprod(1+inversePcaReturnList),col = "orange")

addLegend("topleft", on=1, 
          legend.names = c("Equal","RPP", "HRP", "PCA"), 
          lty=c(1, 1), lwd=c(2, 1),
          col=c( "black", "green", "brown","blue"))

table.AnnualizedReturns(R = equalWeightReturnList, Rf = 0.02/252)
#table.AnnualizedReturns(R = minVarReturnList, Rf = 0.02/252)
table.AnnualizedReturns(R = rppWeightList_1, Rf = 0.02/252)
table.AnnualizedReturns(R = hrpWeightList_1, Rf = 0.02/252)
table.AnnualizedReturns(R = inverseVolWeightList_1, Rf = 0.02/252)



test1 <- as.data.table(equalWeightReturnList)
fwrite(test1, "./equalweight.csv")

test1 <- as.data.table(inverseVolWeightList_1)
fwrite(test1, "./inverseVolWeight.csv")