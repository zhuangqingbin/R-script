#����help.start()���һ����������ڣ����ǿ������в鿴���ź͸߼��İ����ֲᡢ�������⼯���Լ��ο����ϡ�
#�ɺ���vignette()�������ص�vignette�ĵ�һ����PDF��ʽ��ʵ�ý��������¡��������������еİ����ṩ��vignette�ĵ���

#��������
dim1 <- c("A1", "A2")
dim2 <- c("B1", "B2", "B3")
dim3 <- c("C1", "C2", "C3", "C4")
z <- array(1:24, c(2, 3, 4), dimnames=list(dim1, dim2, dim3))

#with�����÷�
#�������Ҫ������with()�ṹ������ڵĶ���ʹ�����⸳ֵ��<<-�����׼��ֵ����<-�����ɣ����ɽ����󱣴浽with()֮���ȫ�ֻ����С���һ���ͨ�����´��������
with(mtcars, {
  nokeepstats <- summary(mpg)
  keepstats <<- summary(mpg)
})
nokeepstats
keepstats

#����
status <- c("Poor", "Improved", "Excellent", "Poor")
status <- factor(status, order=TRUE,levels=c("Poor", "Improved", "Excellent"))
#��������ordered=T,����levels�������˳����Ǵ�С���������˳��



ͨ��ִ����plot()��hist()������ֱ��ͼ����boxplot()�����ĸ߼���ͼ����������һ��
��ͼ��ʱ��ͨ���Ḳ�ǵ���ǰ��ͼ�Ρ���β��ܴ������ͼ�β���ʱ�鿴ÿһ���أ����������ɡ�
��һ�ַ�����������ڴ���һ����ͼ��֮ǰ��һ���µ�ͼ�δ��ڣ�
attach(mtcars)
plot(wt, mpg)
abline(lm(mpg~wt))
title("Regression of MPG on Weight")
detach(mtcars)
ÿһ����ͼ�ν����������һ�δ򿪵Ĵ����С�

�ڶ��ַ����������ͨ��ͼ���û��������鿴���ͼ�Ρ���Mac�ϣ������ʹ��Quartz�˵�
�еġ����ˡ���Back���͡�ǰ������Forward����������ͼ�Ρ���Windows�ϣ�������̷�Ϊ��
�����ڴ򿪵�һ��ͼ�δ����Ժ󣬹�ѡ����ʷ����History��������¼����Recording����Ȼ��ʹ�ò�
���еġ���һ������Previous���͡���һ������Next��������鿴�Ѿ����Ƶ�ͼ�Ρ�

������Ҳ�����һ�ַ����������ʹ�ú���dev.new()��dev.next()��dev.prev()��
dev.set(which=)��dev.off()ͬʱ�򿪶��ͼ�δ��ڣ���ѡ���ĸ�������͵��ĸ������С����ַ���ȫƽ̨���á��������ַ����ĸ���ϸ�ڣ���ο�help(dev.cur)��



#���Ӳ�����ִ��par()������һ�����е�ǰͼ�β������õ��б������Ӳ���no.readonly=TRUE��������һ�������޸ĵĵ�ǰͼ�β����б���
opar <- par(no.readonly=TRUE)
par(lty=2, pch=17)
plot(mtcars$mpg, mtcars$wt, type="b")
par(opar)

col                Ĭ�ϵĻ�ͼ��ɫ��ĳЩ��������lines��pie�����Խ���һ��������ɫֵ���������Զ�ѭ��ʹ�á����磬����趨col=c("red", "blue")����Ҫ���������ߣ����һ���߽�Ϊ��ɫ���ڶ�����Ϊ��ɫ�����������ֽ�Ϊ��ɫ
col.axis           ������̶����ֵ���ɫ
col.lab            �������ǩ�����ƣ�����ɫ
col.main           ������ɫ
col.sub            ��������ɫ
fg                 ͼ�ε�ǰ��ɫ
bg                 ͼ�εı���ɫ
cex                ��ʾ�����Ĭ�ϴ�С���ű�������ֵ��Ĭ�ϴ�СΪ1��1.5��ʾ�Ŵ�ΪĬ��ֵ��1.5����0.5��ʾ��СΪĬ��ֵ��50%���ȵ�
cex.axis           ������̶����ֵ����ű�����������cex
cex.lab            �������ǩ�����ƣ������ű�����������cex
cex.main           ��������ű�����������cex
cex.sub            ����������ű�����������cex
font               ����������ָ����ͼʹ�õ�������ʽ��1=���棬2=���壬3=б�壬4=��б�壬5=�������壨��Adobe���ű����ʾ��
font.axis          ������̶����ֵ�������ʽ
font.lab           �������ǩ�����ƣ���������ʽ
font.main          �����������ʽ
font.sub           �������������ʽ
ps                 �����ֵ��1��ԼΪ1/72Ӣ�磩���ı������մ�СΪ ps*cex
family             �����ı�ʱʹ�õ������塣��׼��ȡֵΪserif�����ߣ���sans���޳��ߣ���mono���ȿ���


ͼ�γߴ���߽�ߴ�
pin                ��Ӣ���ʾ��ͼ�γߴ磨���͸ߣ�
mai                ����ֵ������ʾ�ı߽��С��˳��Ϊ���¡����ϡ��ҡ�����λΪӢ��
mar                ����ֵ������ʾ�ı߽��С��˳��Ϊ���¡����ϡ��ҡ�����λΪӢ��*��Ĭ��ֵΪc(5, 4, 4, 2) + 0.1



�����ı����Զ����������ͼ��
dose <- c(20, 30, 40, 45, 60)
drugA <- c(16, 20, 27, 40, 60)
drugB <- c(15, 18, 25, 31, 40)
plot(dose, drugA, type="b",
     col="red", lty=2, pch=2, lwd=2,
     main="Clinical Trials for Drug A",
     sub="This is hypothetical data",
     xlab="Dosage", ylab="Drug Response",
     xlim=c(0, 60), ylim=c(0, 70))
#����
title(main="My Title", col.main="red",
      sub="My Sub-title", col.sub="blue",
      xlab="My X label", ylab="My Y label",
      col.lab="green", cex.lab=0.75)
#������
axis(side, at=, labels=, pos=, lty=, col=, las=, tck=, ...)
side   һ����������ʾ��ͼ�ε��ı߻��������ᣨ1=�£�2=��3=�ϣ�4=�ң�
at     һ����ֵ����������ʾ��Ҫ���ƿ̶��ߵ�λ��
labels һ���ַ�����������ʾ���ڿ̶����Աߵ����ֱ�ǩ�����ΪNULL����ֱ��ʹ��at�е�ֵ��
pos    �������߻���λ�õ����꣨������һ���������ཻλ�õ�ֵ��
lty    ��������
col    �����Ϳ̶�����ɫ
las    ��ǩ�Ƿ�ƽ���ڣ�=0����ֱ�ڣ�=2��������
tck    �̶��ߵĳ��ȣ�������ڻ�ͼ�����С�ķ�����ʾ����ֵ��ʾ��ͼ����࣬��ֵ��ʾ��ͼ���ڲ࣬0��ʾ���ÿ̶ȣ�1��ʾ���������ߣ���Ĭ��ֵΪ???0.01

x <- c(1:10)
y <- x
z <- 10/x
opar <- par(no.readonly=TRUE)
par(mar=c(5, 4, 4, 8) + 0.1)
plot(x, y, type="b",
     pch=21, col="red",
     yaxt="n", lty=3, ann=FALSE)
lines(x, z, type="b", pch=22, col="blue", lty=2)
axis(2, at=x, labels=x, col.axis="red", las=2)
axis(4, at=z, labels=round(z, digits=2),
     col.axis="blue", las=2, cex.axis=0.7, tck=-.01)
mtext("y=1/x", side=4, line=3, cex.lab=1, las=2, col="blue")
title("An Example of Creative Axes",xlab="X values",ylab="Y=X")
par(opar)

��Ҫ�̶���
ע�⣬�������������ͼ�ζ�ֻӵ�����̶��ߣ�ȴû�д�Ҫ�̶��ߡ�Ҫ������Ҫ�̶��ߣ���
��Ҫʹ��Hmisc���е�minor.tick()������
library(Hmisc)
minor.tick(nx=n, ny=n, tick.ratio=n)

��X���ÿ�������̶���֮������1����Ҫ�̶��ߣ�����Y���ÿ�������̶���֮������2����Ҫ�̶���
minor.tick(nx=2, ny=3, tick.ratio=0.5)

ͼ��
legend(location, title, legend, ...)
location        �����෽ʽ����ָ��ͼ����λ�á������ֱ�Ӹ���ͼ�����Ͻǵ�x��y���꣬Ҳ����ִ��locator(1)��Ȼ��ͨ����굥������ͼ����λ�ã�������ʹ�ùؼ���bottom��bottomleft��left��topleft��top��topright��right��bottomright��center����ͼ���������ʹ��������ĳ���ؼ��֣���ô����ͬʱʹ�ò���inset=ָ��ͼ����ͼ���ڲ��ƶ��Ĵ�С���Ի�ͼ�����С�ķ�����ʾ��
title           ͼ��������ַ�������ѡ��
legend          ͼ����ǩ��ɵ��ַ�������

#�ۺϻ�ͼ
dose <- c(20, 30, 40, 45, 60)
drugA <- c(16, 20, 27, 40, 60)
drugB <- c(15, 18, 25, 31, 40)
opar <- par(no.readonly=TRUE)
par(lwd=2, cex=1.5, font.lab=2)
plot(dose, drugA, type="b",pch=15, lty=1, col="red", ylim=c(0, 60),
     main="Drug A vs. Drug B",xlab="Drug Dosage", ylab="Drug Response")
lines(dose, drugB, type="b",pch=17, lty=2, col="blue")
abline(h=c(30), lwd=1.5, lty=2, col="gray")
library(Hmisc)
minor.tick(nx=3, ny=3, tick.ratio=0.5)
legend("topleft", inset=.05, title="Drug Type", c("A","B"),
       lty=c(1, 2), pch=c(15, 17), col=c("red", "blue"))
par(opar)


#�ı�
���ǿ���ͨ������text()��mtext()���ı����ӵ�ͼ���ϡ�text()�����ͼ�����ڲ�����
�ı�����mtext()����ͼ�ε��ĸ��߽�֮һ�����ı���
text(location, "text to place", pos, ...)
mtext("text to place", side, line=n, ...)
location          �ı���λ�ò�������Ϊһ��x,y���꣬Ҳ��ͨ��ָ��locationΪlocator(1)ʹ����꽻��ʽ��ȷ���ڷ�λ��
pos               �ı������λ�ò����ķ�λ��1=�£�2=��3=�ϣ�4=�ҡ����ָ����pos���Ϳ���ͬʱָ������offset=��Ϊƫ������������ڵ����ַ����ȵı�����ʾ
side              ָ�����������ı��ıߡ�1=�£�2=��3=�ϣ�4=�ҡ������ָ������line=�����ƻ������ı�������ֵ�����ӣ��ı������ơ�Ҳ��ʹ��adj=0���ı������¶��룬��ʹ��adj=1���϶���
attach(mtcars)
plot(wt, mpg,
     main="Mileage vs. Car Weight",
     xlab="Weight", ylab="Mileage",
     pch=18, col="blue")
text(wt, mpg,
     row.names(mtcars),
     cex=0.6, pos=4, col="red")
detach(mtcars)


mydates <- as.Date(c("2007-06-22", "2004-02-13"))
strDates <- c("01/05/1965", "08/16/1975")
dates <- as.Date(strDates, "%m/%d/%Y")
myformat <- "%m/%d/%y"
date <- as.Date(mydates, myformat)
Sys.Date()
date()
today <- Sys.Date()
format(today, format="%B %d %Y")
format(today, format="%A")

startdate <- as.Date("2004-02-13")
enddate <- as.Date("2011-01-22")
days <- enddate - startdate
days

today <- Sys.Date()
dob <- as.Date("1956-10-12")
difftime(today, dob, units="weeks")


������
Ҫ����ϲ��������ݿ����ݼ�������ʹ��merge()�������ڶ�������£��������ݿ���ͨ
��һ���������б�����������ģ���һ�������ᣬinner join��


���ʺ���
d = �ܶȺ�����density��
p = �ֲ�������distribution function��
q = ��λ��������quantile function��
r = ��������������ƫ�

Beta�ֲ� beta 
Logistic�ֲ� logis
����ֲ� binom 
����ֲ� multinom
�����ֲ� cauchy 
������ֲ� nbinom
�������ģ������ֲ� chisq 
��̬�ֲ� norm
ָ���ֲ� exp 
���ɷֲ� pois
F�ֲ� f 
Wilcoxon�����ȷֲ� signrank
Gamma�ֲ� gamma 
t�ֲ� t
���ηֲ� geom 
���ȷֲ� unif
�����ηֲ� hyper 
Weibull�ֲ� weibull
������̬�ֲ� lnorm 
Wilcoxon�Ⱥͷֲ� wilcox

###���ɷ��Ӷ�Ԫ��̬�ֲ�������
library(MASS)
options(digits=3)
set.seed(1234) 
mean <- c(230.7, 146.7, 3.6)
sigma<-matrix(c(15360.8,6721.2,-47.1,6721.2,4700.9,-16.5,-47.1,-16.5,0.3),nrow=3,ncol=3)
mydata <- mvrnorm(500, mean, sigma)
mydata <- as.data.frame(mydata)
names(mydata) <- c("y","x1","x2")
dim(mydata)
head(mydata, n=10)


�ַ���������
nchar(x) ����x�е��ַ�����
x <- c("ab", "cde","fghij")
length(x)#����ֵΪ 3
nchar(x[3])#����ֵΪ5

substr(x, start, stop) ��ȡ���滻һ���ַ������е��Ӵ�
x<-"abcdef"
substr(x, 2, 4)   #����ֵΪ"bcd"
substr(x, 2, 4)<-"22222"  #(x�����"a222ef")

grep(pattern, x, ignore. case=FALSE,fixed=FALSE)
#��x������ĳ��ģʽ����fixed=FALSE����patternΪһ���������ʽ����fixed=TRUE����patternΪһ���ı��ַ���������ֵΪƥ����±�
grep("A",c("b","A","c"),fixed=TRUE)   #����ֵΪ2

sub(pattern, replacement, x,ignore.case=FALSE, fixed=FALSE)
#��x������pattern�������ı�replacement�����滻����fixed=FALSE����patternΪһ���������ʽ����fixed=TRUE����patternΪһ���ı��ַ���
sub("\\s",".","Hello There")����ֵΪHello.There��
#ע�⣬"\s"��һ���������ҿհ׵��������ʽ��ʹ��"\\s"������"\"��ԭ���ǣ�������R�е�ת���ַ�

strsplit(x, split, fixed=FALSE) ��split���ָ��ַ�����x�е�Ԫ�ء���fixed=FALSE����
patternΪһ���������ʽ����fixed=TRUE����patternΪһ���ı��ַ���
y <- strsplit("abc", "") 
#������һ������1���ɷ֡�3��Ԫ�ص��б�������������Ϊ"a" "b" "c"
#unlist(y)[2]��sapply(y, "[", 2)���᷵��"b"

paste(��, sep="") �����ַ������ָ���Ϊsep
paste("x", 1:3,sep="")#����ֵΪc("x1", "x2", "x3")
paste("x",1:3,sep="M")#����ֵΪc("xM1","xM2" "xM3")
paste("Today is", date())#����ֵΪToday is Thu Jun25 14:17:32 2011�����޸����������������������ӽ���ǰ��ʱ�䣩

toupper(x) ��дת��
toupper("abc")����ֵΪ"ABC"

tolower(x) Сдת��
tolower("ABC")����ֵΪ"abc"


##swtich������
feelings <- c("sad", "afraid")
for (i in feelings)
  print(
    switch(i,
           happy = "I am glad you are happy",
           afraid = "There is nothing to fear",
           sad = "Cheer up",
           angry = "Calm down now"
    )
  )


##aggregate
attach(mtcars)
aggdata <-aggregate(mtcars, by=list(cyl,gear), FUN=mean, na.rm=TRUE)
aggdata



####reshape��
�ںϣ����ݼ����ں��ǽ����ع�Ϊ����һ�ָ�ʽ��ÿ������������ռһ�У����д���ҪΨһȷ�������������ı�ʶ��������
library(reshape)
md <- melt(mydata, id=(c("id", "time")))

������cast()������ȡ���ںϵ����ݣ���ʹ�����ṩ�Ĺ�ʽ��һ������ѡ�ģ������������ݵĺ����������ܡ����ø�ʽΪ��
newdata <- cast(md, formula, FUN)
���е�mdΪ���ںϵ����ݣ�formula��������Ҫ�����������FUN�ǣ���ѡ�ģ��������Ϻ���������ܵĹ�ʽ���磺
rowvar1 + rowvar2 + �� ~ colvar1 + colvar2 +
  ����һ��ʽ�У�rowvar1 + rowvar2 + ...������Ҫ�����ı������ϣ���ȷ�����е����ݣ�
��colvar1 + colvar2 + ...������Ҫ�����ġ�ȷ���������ݵı�������






###��sapply��������ͳ��
vars <- c("mpg", "hp", "wt")
mystats <- function(x, na.omit=FALSE){
  if (na.omit)
    x <- x[!is.na(x)]
  m <- mean(x)
  n <- length(x)
  s <- sd(x)
  skew <- sum((x-m)^3/s^3)/n
  kurt <- sum((x-m)^4/s^4)/n - 3
  return(c(n=n, mean=m, stdev=s, skew=skew, kurtosis=kurt))
}
sapply(mtcars[vars], mystats)

Hmisc���е�describe()�����ɷ��ر����͹۲��������ȱʧֵ��Ψһֵ����Ŀ��ƽ��ֵ��
��λ�����Լ��������ֵ�������С��ֵ��
library(Hmisc)
describe(mtcars[vars])

pastecs������һ����Ϊstat.desc()�ĺ����������Լ������෱���������ͳ������ʹ��
��ʽΪ��stat.desc(x, basic=TRUE, desc=TRUE, norm=FALSE, p=0.95)
���е�x��һ�����ݿ��ʱ�����С���basic=TRUE��Ĭ��ֵ�����������������ֵ����ֵ��ȱʧֵ���������Լ���Сֵ�����ֵ��ֵ�򣬻����ܺ͡���desc=TRUE��ͬ��Ҳ��Ĭ��ֵ�����������λ����ƽ������ƽ�����ı�׼��ƽ�������Ŷ�Ϊ95%���������䡢�����׼���Լ�����ϵ���������norm=TRUE������Ĭ�ϵģ����򷵻���̬�ֲ�ͳ����������ƫ�Ⱥͷ�ȣ��Լ����ǵ�ͳ�������̶ȣ���Shapiro�CWilk��̬������������ʹ����pֵ������ƽ�������������䣨Ĭ�����Ŷ�Ϊ0.95��
library(pastecs)
stat.desc(mtcars[vars])


###�������������ͳ����
aggregate(mtcars[vars], by=list(am=mtcars$am), mean)
aggregate(mtcars[vars], by=list(am=mtcars$am), sd)
ע��list(am=mtcars$am)��ʹ�á����ʹ�õ���list(mtcars$am)����am�н�����עΪGroup.1������am��

�ź����ǣ�aggregate()��������ÿ�ε�����ʹ��ƽ��������׼�������ĵ�����ֵ������
���޷�һ�η������ɸ�ͳ������Ҫ����������񣬿���ʹ��by()������
dstats <- function(x)(c(mean=mean(x), sd=sd(x)))
by(mtcars[vars], mtcars$am, dstats)


��doBy����summaryBy()������ʹ�ø�ʽΪ
var1 + var2 + var3 + ... + varN ~ groupvar1 + groupvar2 + �� + groupvarN
��~���ı�������Ҫ��������ֵ�ͱ��������Ҳ�ı���������͵ķ��������function
��Ϊ�κ��ڽ����û��Ա��R������
library(doBy)
summaryBy(mpg+hp+wt~am, data=mtcars, FUN=mystats)

###reshape��
library(reshape)
 dstats <- function(x)(c(n=length(x), mean=mean(x), sd=sd(x)))
 dfm <- melt(mtcars, measure.vars=c("mpg", "hp", "wt"),id.vars=c("am", "cyl"))
 cast(dfm, am + cyl + variable ~ ., dstats)


 Ƶ������������
 �����е���������vcd���е�Arthritis���ݼ�
 library(vcd)
 > head(Arthritis)
 ����Ƶ����
 table(var1, var2, ��, varN)     ʹ�� N ������ͱ��������ӣ�����һ�� N ά������
 xtabs(formula, data)           ����һ����ʽ��һ����������ݿ򴴽�һ�� N ά������
 prop.table(table, margins)     ��margins����ı߼��б���������Ŀ��ʾΪ������ʽ
 margin.table(table, margins)   ��margins����ı߼��б����������Ŀ�ĺ�
 addmargins(table, margins)     ��������margins��Ĭ������ͽ�����������
 ftable(table)                  ����һ�����յġ�ƽ�̡�ʽ������
 
 ����ʹ��table()�������ɼ򵥵�Ƶ��ͳ�Ʊ�
 mytable <- with(Arthritis, table(Improved))
 mytable
 
 ������prop.table()����ЩƵ��ת��Ϊ����ֵ
 prop.table(mytable)

 ��ʹ��prop.table()*100ת��Ϊ�ٷֱ�
 prop.table(mytable)*100
 
 ��ά������
 xtabs()��������ʹ�ù�ʽ�������봴������������ʽΪ��
 mytable <- xtabs(~ A + B, data=mydata)
 ���е�mydata��һ����������ݿ��ܵ���˵��Ҫ���н������ı���Ӧ�����ڹ�ʽ���Ҳࣨ��~���ŵ��ҷ�������+��Ϊ�ָ�������ĳ������д�ڹ�ʽ����࣬����Ϊһ��Ƶ���������������Ѿ�������ʱ�����ã���
mytable <- xtabs(~ Treatment+Improved, data=Arthritis)
mytable

�����ʹ��margin.table()��prop.table()�����ֱ����ɱ߼�Ƶ���ͱ������к����б�
�������������㣺
margin.table(mytable, 1)
prop.table(mytable, 1)

�����ʹ��addmargins()����Ϊ��Щ�������ӱ߼ʺ͡����磬���´��������˸��еĺ���
���еĺͣ�
addmargins(mytable)
addmargins(prop.table(mytable))

��ʹ��addmargins()ʱ��Ĭ����Ϊ��Ϊ�������еı��������߼ʺ�
addmargins(prop.table(mytable, 1), 2)
addmargins(prop.table(mytable, 2), 1)


##��ά�б���֮ǰ��xtabs����table�������ƹ㣬���⣬ftable()����������һ�ֽ��ն������˵ķ�ʽ�����ά������
mytable<-xtabs(~ Treatment+Sex+Improved, data=Arthritis)
ftable(mytable)
margin.table(mytable, 1)
margin.table(mytable, 2)
margin.table(mytable, 3)
margin.table(mytable, c(1, 3))


###�����Լ���
���������Լ���
�����ʹ��chisq.test()�����Զ�ά�����б������б������п��������Լ���
library(vcd)
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
chisq.test(mytable)
mytable <- xtabs(~Improved+Sex, data=Arthritis)
chisq.test(mytable)

Fisher��ȷ����
����ʹ��fisher.test()��������Fisher��ȷ���顣Fisher��ȷ�����ԭ�����ǣ��߽�̶�
�����������к������໥�����ġ�
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
fisher.test(mytable)

Cochran-Mantel��Haenszel����
mantelhaen.test()��������������Cochran��Mantel��Haenszel�������飬��ԭ�����ǣ�������������ڵ�����������ÿһ���ж������������ġ����д�����Լ�����������͸���������Ա��ÿһˮƽ���Ƿ�������˼�����費�������׽������ã��������������������Ա𣩡�
mytable <- xtabs(~Treatment+Improved+Sex, data=Arthritis)
mantelhaen.test(mytable)

����ԵĶ���
library(vcd)
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
assocstats(mytable)
������˵���ϴ��ֵ��ζ�Ž�ǿ�������

Pearson�������ϵ��������������������֮���������س̶ȡ�Spearman�ȼ����ϵ�����
���ּ��������֮�����س̶ȡ�Kendall��s Tau���ϵ��Ҳ��һ�ַǲ����ĵȼ���ض�����
  cor()�������Լ������������ϵ������cov()��������������Э����
x         ��������ݿ�
use       ָ��ȱʧ���ݵĴ�����ʽ����ѡ�ķ�ʽΪall.obs�����費����ȱʧ���ݡ�������ȱʧ����ʱ����������everything������ȱʧ����ʱ�����ϵ���ļ�����������Ϊmissing����complete.obs����ɾ�����Լ� pairwise.complete.obs���ɶ�ɾ����pairwise deletion��
method    ָ�����ϵ�������͡���ѡ����Ϊpearson��spearman��kendall
states<- state.x77[,1:6]
cov(states)
cor(states)
cor(states, method="spearman")

x <- states[,c("Population", "Income", "Illiteracy", "HS Grad")]
y <- states[,c("Life Exp", "Murder")]
cor(x,y)


ƫ���
ƫ�����ָ�ڿ���һ��������������ʱ������������������֮����໥��ϵ��
�����ʹ��ggm���е�pcor()��������ƫ���ϵ����ggm��û�б�Ĭ�ϰ�װ���ڵ�һ��ʹ��֮ǰ��Ҫ�Ƚ��а�װ���������ø�ʽΪ��
pcor(u, S)
���е�u��һ����ֵ������ǰ������ֵ��ʾҪ�������ϵ���ı����±꣬�������ֵΪ������������Ҫ�ų�Ӱ��ı��������±ꡣSΪ������Э������
library(ggm)
#�ڿ��������롢��ä�ʺ͸��б�ҵ��ʱ
#�˿ں�ıɱ�ʵ�ƫ���ϵ��
pcor(c(1,5,2,3,6), cov(states))


����Ե������Լ���
�ڼ�������ϵ���Ժ���ζ����ǽ���ͳ�������Լ�����
����ʹ��cor.test()�����Ե�����Pearson��Spearman��Kendall���ϵ�����м��顣�򻯺��ʹ�ø�ʽΪ��
cor.test(x, y, alternative = , method = )
���е�x��yΪҪ��������Եı�����alternative������ָ������˫�����򵥲���飨ȡֵ
Ϊ"two.side"��"less"��"greater"������method����ָ��Ҫ�����������ͣ�"pearson"��
"kendall"��"spearman"�������о��ļ���Ϊ��������ϵ��С��0ʱ����ʹ��alternative=
  "less"�����о��ļ���Ϊ��������ϵ������0ʱ��Ӧʹ��alternative="greater"��
cor.test(states[,3], states[,5])

cor.testÿ��ֻ�ܼ���һ����ع�ϵ�������˵��ǣ�psych�����ṩ��corr.test()��������һ������������
library(psych)
corr.test(states, use="complete")
����use=��ȡֵ��Ϊ"pairwise"��"complete"���ֱ��ʾ��ȱʧִֵ�гɶ�ɾ������ɾ����������method=��ȡֵ��Ϊ"pearson"��Ĭ��ֵ����"spearman"��"kendall"��

���ǹ�ע��ƫ���ϵ�����ڶ�Ԫ��̬�Եļ����£�psych���е�pcor.test()�����������������ڿ���һ�������������ʱ��������֮������������ԡ�ʹ�ø�ʽΪ
pcor.test(r, q, n)
���е�r����pcor()��������õ���ƫ���ϵ����qΪҪ���Ƶı�����������ֵ��ʾλ�ã���nΪ������С��


����������t����
��һ���������Ķ�������t����������ڼ�����������ľ�ֵ��ȵļ��衣����������������Ƕ����ģ������Ǵ���̬�����г�á�
library(MASS)
t.test(Prob ~ So, data=UScrime)


�Ƕ���������t����
library(MASS)
sapply(UScrime[c("U1","U2")], function(x)(c(mean=mean(x),sd=sd(x))))
with(UScrime, t.test(U1, U2, paired=TRUE))



###########�ǲ�������
###����
with(UScrime, by(Prob, So, median))
wilcox.test(Prob ~ So, data=UScrime)
#Wilcoxon�����ȼ����ǷǶ�������t�����һ�ַǲ������������������������ɶ����ݺ��޷���֤��̬�Լ�����龳�����ø�ʽ��Mann�CWhitney U������ȫ��ͬ���������������Ӳ���paired=TRUE
sapply(UScrime[c("U1","U2")], median)
with(UScrime, wilcox.test(U1, U2, paired=TRUE))
#��t����ļ������ʱ����������Ĺ�Ч��ǿ�������׷��ִ��ڵĲ��죩�����ǲ��������ڼ���ǳ�������ʱ������ڵȼ��������ݣ������á�

#��������ıȽ�
#����޷�����ANOVA��Ƶļ��裬��ô����ʹ�÷ǲ����������������Ĳ��졣��������������Kruskal��Wallis���齫��һ��ʵ�õķ�����������鲻���������ظ�������ƻ����������ƣ�����ôFriedman���������ʡ�
kruskal.test(y ~ A, data)
friedman.test(y ~ A | B, data)
#���е�y����ֵ�ͽ��������A��һ�������������B��һ�������϶�ƥ��۲�����������blocking variable
states <- as.data.frame(cbind(state.region, state.x77))
kruskal.test(Illiteracy ~ state.region, data=states)
class <- state.region
var <- state.x77[,c("Illiteracy")]
mydata <- as.data.frame(cbind(class, var))
rm(class, var)
library(npmc)
summary(npmc(mydata), type="BF")
aggregate(mydata, by=list(mydata$class), median)



















boxplot(mpg ~ cyl, data=mtcars,
        notch=TRUE,
        varwidth=TRUE,
        col="red",
        main="Car Mileage Data",
        xlab="Number of Cylinders",
        ylab="Miles Per Gallon")

dotchart(mtcars$mpg, labels=row.names(mtcars), cex=.7,
         main="Gas Mileage for Car Models",
         xlab="Miles Per Gallon")

x <- mtcars[order(mtcars$mpg),]
x$cyl <- factor(x$cyl)
x$color[x$cyl==4] <- "red"
x$color[x$cyl==6] <- "blue"
x$color[x$cyl==8] <- "darkgreen"
dotchart(x$mpg,
         labels = row.names(x),
         cex=.7,
         groups = x$cyl,
         gcolor = "black",
         color = x$color,
         pch=19,
         main = "Gas Mileage for Car Models\ngrouped by cylinder",
         xlab = "Miles Per Gallon")





library(car)
scatterplot(weight ~ height,
            data=women,
            spread=T, lty.smooth=2,
            pch=22,
            main="Women Age 30-39",
            xlab="Height (inches)",
            ylab="Weight (lbs.)")






states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
cor(states)
scatterplotMatrix(states, spread=FALSE, lty.smooth=2,main="Scatter Plot Matrix")
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost,
          data=states)
summary(fit)

fit <- lm(mpg ~ hp + wt + hp:wt, data=mtcars)
summary(fit)
#̽����������һ���������ȱ任��һ��������������Ĺ�ϵ
library(effects)
#plot(effect(term, mod, xlevels),
 #    multiline=TRUE)
library(effects)
plot(effect("hp:wt",fit,list(wt=c(2.2,3.2,4.2))),multiline=TRUE)


fit <- lm(weight ~ height, data=women)
par(mfrow=c(2,2))
plot(fit)
#�޸�
fit2 <- lm(weight ~ height + I(height^2), data=women)
par(mfrow=c(1,1))
plot(fit2)


#car�еĸ��ּ���
library(car)
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
qqPlot(fit, labels=row.names(states), id.method="identify",
       simulate=TRUE, main="Q-Q Plot")
#�۲⵽Nevada ��һ��Ⱥ��
states["Nevada",]
#����ԭ�����ع�����ݿ��������֣����Ժ���������ģ��֮���Ǵ������ֵ�����
fitted(fit)["Nevada"]
residuals(fit)["Nevada"]
rstudent(fit)["Nevada"]#��ȡѧ������ɾ���в�

residplot <- function(fit, nbreaks=10) {
  z <- rstudent(fit)
  hist(z, breaks=nbreaks, freq=FALSE,
       xlab="Studentized Residual",
       main="Distribution of Errors")
  rug(jitter(z), col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue", lwd=2)
  lines(density(z)$x, density(z)$y,
        col="red", lwd=2, lty=2)
  legend("topright",
         legend = c( "Normal Curve", "Kernel Density Curve"),
         lty=1:2, col=c("blue","red"), cex=.7)
}
residplot(fit)

#����в��Ƿ������
durbinWatsonTest(fit)

#��ͼָ������ģ�͵Ĳв����һ���Ա�������ع�ϵ���ĳ˻�vs���Ա�����ֱ������һ���Ա�������ع�ϵ���ĳ˻�vs���Ա������Դ˼��������ڲ�������
crPlots(fit)

#�����췽��
#ԭ����Ϊ�������췽��
ncvTest(fit)
spreadLevelPlot(fit)


#ȫ�ּ��飬�����Լ�������ģ�͵ļ���
library(gvlma)
gvmodel<-gvlma(fit)
summary(gvmodel)

#������ع�����
vif(fit)
sqrt(vif(fit)) > 2 # problem?

#�����쳣��
    #���Ч�����õĵ� outlier
outlierTest(fit)
   ##Cookֵ����4/(n-k-1)��˵��Ӱ���
cutoff <- 4/(nrow(states)-length(fit$coefficients)-2)
plot(fit, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")

influencePlot(fit, id.method="identify", main="Influence Plot",
              sub="Circle size is proportional to Cook��s distance")
#Nevada and Rhode Island are outliers; NewYork, California, Hawaii,and Washington have high leverage; and Nevada, Alaska, and Hawaii are influential observations


#�������������ʽ��������̬�Լ���
#�����Ȼ�ҳ�ָ��
summary(powerTransform(states$Murder))
#��LR test�е�lambda=��1�� ��pֵ���Կ���û��Ҫ���任 ˵��ԭ���̻��ǽϺõ������̬

#�������ڲ������費���ϣ����������Ա�������ʽ���ı�ָ����������
boxTidwell(Murder~Population+Illiteracy,data=states)
#˵��popluationָ����0.86��illiteracyָ����1.35������pֵ���Կ���û��Ҫ���任

#�����췽��
#ԭ����Ϊ�������췽��
ncvTest(fit)
spreadLevelPlot(fit)
#�������ˮƽ����ncvTest���鲻����ͬ�����Suggested power transformation���������


#��Ƕģ�͵ıȽ� anova
fit1 <- lm(Murder ~ Population + Illiteracy + Income + Frost,data=states)
fit2 <- lm(Murder ~ Population + Illiteracy, data=states)
anova(fit2, fit1)
#��������������
AIC(fit1,fit2)
#����СAIC����

#�������̵��Ա������࣬Ҫ����ķ���̫�࣬��Ӧ��ʹ��ǰ�������ˡ��𲽷���
library(MASS)
fit1 <- lm(Murder ~ Population + Illiteracy + Income + Frost,data=states)
stepAIC(fit, direction="backward")

#���������Ա����Ŀ����Ӽ����ع飬�÷�������һ������̫��ʱ��
library(leaps)
leaps<-regsubsets(Murder~Population+Illiteracy+Income+Frost,data=states,nbest=4)
plot(leaps, scale="adjr2")
library(car)
subsets(leaps, statistic="cp",main="Cp Plot for All Subsets Regression")
abline(1,1,lty=2,col="red")

#����ģ�͵�Ԥ������
#������������������ݹ�����ģ����ѡ����Ա�������ʵ�Ѿ����ԣ�����Ϊ�˸��õ�Ԥ����ʵ������������������Ҫ��ģ�͵�Ԥ����������һ���ķ���
#˼·�������ݷֽ��k���Ӽ���������һ����ΪԤ��ı�������ʣ�µ�k-1��Ϊ���������ع�ģ�ͣ�������R
shrinkage <- function(fit, k=10){
  require(bootstrap)
  theta.fit <- function(x,y){lsfit(x,y)}
  theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef}
  x <- fit$model[,2:ncol(fit$model)]
  y <- fit$model[,1]
  results <- crossval(x, y, theta.fit, theta.predict, ngroup=k)
  r2 <- cor(y, fit$fitted.values)^2
  r2cv <- cor(y, results$cv.fit)^2
  cat("Original R-square =", r2, "\n")
  cat(k, "Fold Cross-Validated R-square =", r2cv, "\n")
  cat("Change =", r2-r2cv, "\n")
}
fit <- lm(Murder ~ Population + Income + Illiteracy + Frost, data=states)
shrinkage(fit)

#�жϸ�����������Ҫ��
relweights <- function(fit,...){
  R <- cor(fit$model)
  nvar <- ncol(R)
  rxx <- R[2:nvar, 2:nvar]
  rxy <- R[2:nvar, 1]
  svd <- eigen(rxx)
  evec <- svd$vectors
  ev <- svd$values
  delta <- diag(sqrt(ev))
  lambda <- evec %*% delta %*% t(evec)
  lambdasq <- lambda ^ 2
  beta <- solve(lambda) %*% rxy
  rsquare <- colSums(beta ^ 2)
  rawwgt <- lambdasq %*% beta ^ 2
  import <- (rawwgt / rsquare) * 100
  lbls <- names(fit$model[2:nvar])
  rownames(import) <- lbls
  colnames(import) <- "Weights"
  barplot(t(import),names.arg=lbls,
          ylab="% of R-Square",
          xlab="Predictor Variables",
          main="Relative Importance of Predictor Variables",
          sub=paste("R-Square=", round(rsquare, digits=3)),
          ...)
  return(import)
}
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
relweights(fit, col="lightgrey")






















##�������
#ע��anova�����ڻ���������car���ǲ�һ���ģ�anova�ڻ������ǿ���ѡ�����ַ�����������е�һ�֣�aovֻ�ܷ�����һ��
#�����ط������

library(mvtnorm)
library(survival)
library(TH.data)
library(multcomp)
attach(cholesterol)
table(trt)
aggregate(response,by=list(trt),mean)
aggregate(response,by=list(trt),sd)
fit<-aov(response~trt)
summary(fit)
library(gplots)
plotmeans(response ~ trt, xlab="Treatment", ylab="Response",main="Mean Plot\nwith 95% CI")#����ÿ���ֵ������·ֲ�
detach(cholesterol)

#��TUkeyHSD������Щ���ֵ��ͬ�����ú�����HH���еĺ�����ͻ��������detach("package:HH")�����Ƴ�����·��
TukeyHSD(fit)
par(las=2)
par(mar=c(5,8,4,2))
plot(TukeyHSD(fit))

library(multcomp)
par(mar=c(5,4,6,2))
tuk <- glht(fit, linfct=mcp(trt="Tukey"))
plot(cld(tuk, level=.05),col="lightgrey")


#���������裬�������̬�ֲ����ڸ����з���һ��

###���鷽������ļ���ǰ��
##������̬�Լ���
library(car)
qqPlot(lm(response ~ trt, data=cholesterol),simulate=TRUE, main="Q-Q Plot", labels=FALSE)#�㶼����95%�Ĵ��У�������̬�Լ���
##���鷽������
bartlett.test(response ~ trt, data=cholesterol)
#ԭ���裬������ͬ
library(car)
outlierTest(fit)


##������Э�����������
#ʹ��litter���ݼ��������ݼ���¼�����鱻ע�벻ͬҩ�����Ļ���ĸ�������������������С������أ�Э�����ǻ���ʱ��
data(litter, package="multcomp")
attach(litter)
table(dose)
aggregate(weight, by=list(dose), FUN=mean)
fit <- aov(weight ~ gesttime + dose)
summary(fit)
#F�����ʾЭ����gesttime�ͳ��������йأ����ر���dose�ڿ���Э��������ʱ�ͳ��������й�
#Ϊ��õ���������ֵ����ȥ����Э������Ӱ�������ֵ
library(effects)
effect("dose", fit)
#��֪Ҫ�Ƚϵľ����ϵ������spss�еķ�������еĶ���ʽ
library(multcomp)
contrast <- rbind("no drug vs. drug" = c(3, -1, -1, -1))
summary(glht(fit, linfct=mcp(dose=contrast)))
#��������ҩ��ע���С�����������С��Ĳ��

#�������
library(multcomp)
fit2 <- aov(weight ~ gesttime*dose, data=litter)
summary(fit2)
#�����������֤����б����ȵļ��裬
#���ӻ����
library(lattice)
library(grid)
library(latticeExtra)
library(RColorBrewer)
library(gridExtra)
library(HH)
ancova(weight ~ gesttime + dose, data=litter)
       
###˫���ط������
attach(ToothGrowth)
table(supp, dose)
aggregate(len, by=list(supp, dose), FUN=mean)
aggregate(len, by=list(supp, dose), FUN=sd)
fit <- aov(len ~ supp*dose)
summary(fit)
interaction.plot(dose, supp, len, type="b",
                 col=c("red","blue"), pch=c(16, 18),
                 main = "Interaction between Dose and Supplement Type")
library(gplots)
plotmeans(len ~ interaction(supp, dose, sep=" "),
          connect=list(c(1,3,5),c(2,4,6)),
          col=c("red", "darkgreen"),
          main = "Interaction Plot with 95% CIs",
          xlab="Treatment and Dose Combination")
library(HH)
interaction2wt(len~supp*dose)


###�ظ������������
w1b1 <- subset(CO2, Treatment=='chilled')
fit <- aov(uptake~conc*Type + Error(Plant/conc), w1b1)
summary(fit)
par(las=2)
par(mar=c(10,4,4,2))
with(w1b1, interaction.plot(conc,Type,uptake,
              type="b", col=c("red","blue"), pch=c(16,18),
              main="Interaction Plot for Plant Type and Concentration"))
boxplot(uptake ~ Type*conc, data=w1b1, col=(c("gold", "green")),
          main="Chilled Quebec and Mississippi Plants",
          ylab="Carbon dioxide uptake rate (umol/m^2 sec)")


###�����ض�����������
library(MASS)
attach(UScereal)
y <- cbind(calories, fat, sugars)
aggregate(y, by=list(shelf), FUN=mean)
cov(y)
fit <- manova(y ~ shelf)
summary(fit)
summary.aov(fit)

##�����ض������������ļ��裺��̬����������
center <- colMeans(y)
n <- nrow(y)
p <- ncol(y)
cov <- cov(y)
d <- mahalanobis(y,center,cov)
coord <- qqplot(qchisq(ppoints(n),df=p),
                  d, main="Q-Q Plot Assessing Multivariate Normality",
                  ylab="Mahalanobis D2")
abline(a=0,b=1)
identify(coord$x, coord$y, labels=row.names(UScereal))#�������
#��������R��û��ʲô�õļ���

#����������費���㣬�������Ƚ��ĵ����ض�����������
library(robustbase)
library(rrcov)
Wilks.test(y,shelf,method="mcd")

##�����ط��������ع����
library(survival)
library(multcomp)
levels(cholesterol$trt)
fit.aov <- aov(response ~ trt, data=cholesterol)
summary(fit.aov)
250
