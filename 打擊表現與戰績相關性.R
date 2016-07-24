#建立安打，全壘打，保送與戰績的迴歸模型
#先從http://seanlahman.com/baseball-archive/statistics下載資料
teamStat<-read.csv("C:/Users/hippo/Desktop/CSV/球隊戰績.csv",header = TRUE,sep=",", stringsAsFactors = FALSE)
teamStatHit<-teamStat[,c(2,3,5,6,8,9,10,17,18,19,20,21)]
WinsPredictedByH<-data.frame(H = 1400)#安打數預測值為1400

# 建立一個安打數與勝場數的線性迴歸模型
lmWinsByH <- lm(formula = W ~ H, data = teamStatHit)#W=y，H=x，inputData=teamStatHit
predicted <- predict(lmWinsByH, newdata = WinsPredictedByH)
# 模型結論
summary(lmWinsByH)
# 作圖
plot(teamStatHit$W ~ teamStatHit$H, main = "依據安打數推測勝場數", xlab = "安打數", ylab = "勝場數" )#散佈圖
points(x = WinsPredictedByH$H, y = predicted, col="green", cex = 2, pch = 18)#預測點
abline(reg = lmWinsByH$coefficients, col = "red", lwd = 2)#迴歸方程式

# 建立一個全壘打與勝場數的線性迴歸模型
WinsPredictedByHR<-data.frame(HR = 175)#全壘打預測值為175
lmWinsByHR <- lm(formula = W ~ HR, data = teamStatHit)#W=y，HR=x，inputData=teamStatHit
predicted <- predict(lmWinsByHR, newdata = WinsPredictedByHR)
# 模型結論
summary(lmWinsByHR)
# 作圖
plot(teamStatHit$W ~ teamStatHit$HR, main = "依據全壘打推測勝場數", xlab = "全壘打", ylab = "勝場數" )
points(x = WinsPredictedByHR$HR, y = predicted, col="green", cex = 2, pch = 18)
abline(reg = lmWinsByHR$coefficients, col = "red", lwd = 2)

# 建立一個保送與勝場數的線性迴歸模型
WinsPredictedByBB<-data.frame(BB = 480)#全壘打預測值為175
lmWinsByBB <- lm(formula = W ~ BB, data = teamStatHit)#W=y，BB=x，inputData=teamStatHit
predicted <- predict(lmWinsByBB, newdata = WinsPredictedByBB)
# 模型結論
summary(lmWinsByBB)
# 作圖
plot(teamStatHit$W ~ teamStatHit$BB, main = "依據保送推測勝場數", xlab = "保送", ylab = "勝場數" )
points(x = WinsPredictedByBB$BB, y = predicted, col="green", cex = 2, pch = 18)
abline(reg = lmWinsByBB$coefficients, col = "red", lwd = 2)
