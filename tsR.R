set.seed(20)
n<-100
p<-0.5
u0<-rbinom(n,1,p)
u1<-(u0-0.5)*2
x<-c(0,cumsum(u1))
plot(diff(x),type="l",main="",xlab="时间",ylab="差分值",xlim=c(0,100),cex.axis=1.5,cex.lab=1.5)
library(TSA)
par(mfrow=c(2,1),cex.axis=1.5,cex.lab=1.5)

acf(x,main="随机游走模型")
#对于随机游走模型模拟的时间序列，自相关函数随着滞后期的增加缓慢衰减，说明随机游走模型模拟的时间序列为非平稳序列

acf(diff(x),main="一阶差分序列")
#模拟序列的一阶差分序列的为平稳的白噪声过程，其自相关在滞后各期均不显著，为平稳时间序列

par(mfrow=c(1,1))

#利用Box,test对差分序列的自相关性进行混合检验
Box.test(diff(x),lag=5,type=c("Box-Pierce"))
Box.test(diff(x),lag=5,type=c("Ljung-Box"))







