

# 上課前請先執行下列幾行


# -----------------------------------
install.packages("lme4")
library(lme4)
install.packages("nlme")
library(nlme)
install.packages("rio")
library(rio)
install.packages("psych")
library(psych)
install.packages("Matrix")
library(Matrix)
#如果Console沒有出現任何以"Error: ......"開頭的訊息就是OK拉
#如果是說 "suggested packages沒安裝.." 沒關係那不重要。
#就可以關起來了，不用特別儲存什麼

#-----------------------------------








#-----    R 的四個框框    ----------



#-----    程式語言的基本邏輯   -----

  #variable  e.g: a,b,c

  #function  e.g. + - , t-test() .... 

  #記憶體與儲存



#-----    R 的基本操作    ----------

  #數種資料結構：num數值 chr字串 boolean/logic布林值 
  #vector, factor, list, dataframe
a = 1
b = "b"
c = T

factor1 = c("大","中","小")     #只是三個chr 稱為Vector
factor1 = as.factor(factor1)

BigList = c(a,b,c)             #依然只是三個chr  也是Vector
BigList = list(a,b,c)

vec = c("a","b","c","d")
vec[1]
vec[2:4]
vec[-1]

dta = data.frame(
  Length1  = c(5.5,5.5,5.8,6.0,5.4,6.0,6.7,6.3,5.6,5.5),
  Length2 = c(6.1,5.8,5.0,5.6,5.7,5.7,6.2,5.1,5.7,5.5)
  )

View(dta)
dta
dta$Length1
dta$Length2



#-----    快速跑過以前會的統計    ----------



dta<-data.frame(gender=rep(c("F","M"),each=10),
                height=c(56,60,64,68,72,54,62,65,65,70,
                         64,68,72,76,80,62,69,74,75,82),
                weight=c(117,125,133,141,149,109,128,131,131,145,
                         211,223,235,247,259,201,228,245,241,269))
?rep
#相關
cor(dta$height,dta$weight)

#t檢定
x = dta$weight[dta$gender=="F"]
y = dta$weight[dta$gender=="M"]

t.test(x,y)  # 不知道怎麼放就打問號
?t.test

#迴歸
lm(height~weight,data = dta)
lm_result = lm(height~weight,data = dta)
summary(lm_result)

#anova
aov_result = aov(height~gender,data = dta)
summary(aov_result)

#-------------    HLM    --------------------
dta<-import("5L3.sav")
View(dta)  #整體看
names(dta) #看標題
head(dta)  #看前6
is.na(dta) #空值
sum(is.na(dta)) #是否有na
dta[dta=="1"]  #如果有的話(應該)
summary(dta)
sd(dta$achievement,na.rm = T)  #可以跳過NA



#量表處理以後再說吧
#HLM
 
#R與SPSS的HLM，不會特別分level，所以要從資料上著手 
#NULL MODEL
m1 <- lme(achievement~1,random = ~1|id2,data = dta)
summary(m1)
VarCorr(m1)


#level1 Random
m2 <- lme(achievement~minority+gender+ses,random = ~minority+gender+ses|id2,data = dta)
summary(m2)

#製作levle2變項
MeanSes_School <- tapply(dta$ses,dta$id2,mean)
MeanSes_lv1 <-MeanSes_School[as.character(dta$id2)]
dta$MeanSes_school <- MeanSes_lv1

m3 <- lme(achievement~minority+gender+ses+size+sector+meanses,random = ~minority+gender+ses|id2,data = dta,control =lmeControl(opt = "optim"))
summary(m3)




#因素分析
efadata <- read.table("http://www.obhrm.net/data/ex4.1a.dat")     
names(efadata) <-c(paste("y", 1:12, sep=""))                

fa.parallel(efadata, n.iter=100,main="Scatter Plot")   
f4 <- fa(efadata,4,rotate="varimax")                       #因素分析，四個因素，正交旋轉
load = loadings(f4)
print(load,sort=TRUE,digits=2)  
