library(ggplot2)
library(plyr)
#选取各个颜色中carat最小的钻石
ddply(diamonds,.(color),subset,carat==min(carat))
#选取各个颜色中carat最小的前两个钻石
ddply(diamonds,.(color),subset,order(carat)<=2)
#选取各个颜色中carat百分前1的钻石
ddply(diamonds,.(color),subset,carat>quantile(carat,0.99))

dsmall<-diamonds[sample(nrow(diamonds),50),]
ddply(dsmall,.(color),subset)
ddply(dsmall,.(color),subset,carat==min(carat))
ddply(dsmall,.(color),subset,order(carat)<=2)

ddply(dsmall,.(color),transform,price1=scale(price))

nmissing<-function(x){sum(is.na(x))}
nmissing(msleep$name)
nmissing(msleep$brainwt)
nmissing(msleep)
#将原本对单行向量操作的函数转化为对竖向操作
nmissing_df<-colwise(nmissing)
nmissing_df(msleep)
#简化
colwise(nmissing)(msleep)
#numcolwise()
msleep2<-msleep[,-6]
numcolwise(median)(msleep2,na.rm=T)
numcolwise(quantile)(msleep2,na.rm=T)
numcolwise(quantile)(msleep2,probs=c(0.25,0.75),na.rm=T)

ddply(msleep2,.(vore),numcolwise(median),na.rm=T)
ddply(msleep2,.(vore),numcolwise(mean),na.rm=T)
####


#函数必须能对根据分类变量分类好的数据框进行计算
my_summary<-function(df){
  with(df,data.frame(
    pc_cor=cor(price,carat,method="spearman"),
    lpc_cor=cor(log(price),log(carat),method="spearman")
  ))
}
ddply(diamonds,.(cut),my_summary)
ddply(diamonds,.(color),my_summary)











data<-data.frame(x=c(2,3,2,4,5,6),y=c(2,4,6,7,5,3),z=c(1,2,1,2,1,2))
ddply(data,.(z),colwise(mean))
ddply(data,.(z),subset)
ddply(data,.(z),subset,y>3)

my_summary<-function(df){
  with(df,data.frame(
    pc_cor=cor(price,carat,method="spearman")
  ))
}
ddply(diamonds,.(cut),my_summary)


library(ggplot2)
qplot(carat,price,data=diamonds,geom="smooth",color=color)
qplot(carat,price,data=diamonds)+geom_smooth(aes(color=color))
dense<-subset(diamonds,carat<2)
qplot(carat,price,data=dense,geom="smooth",color=color,fullrange=T)



library(reshape2)
#
ggplot(economics,aes(date))+
  geom_line(aes(y=unemploy,color="unemploy"))+
  geom_line(aes(y=uempmed,color="uempmed"))+
  scale_color_hue("variable")
emp<-melt(economics,id="date",measure=c("unemploy","uempmed"))
qplot(date,value,data=emp,geom="line",color=variable)

range01<-function(x){
  xrng<-range(x);(x-xrng[1])/diff(xrng)
}
emp2<-ddply(emp,.(variable),transform,value=range01(value))
qplot(date,value,data=emp2,geom="line",color=variable,linetype=variable)

qplot(date,value,data=emp,geom="line")+facet_grid(variable~.,scales="free_y")



