###################################################
### Loading the Data into R
###################################################
#设置工作路径
setwd("D:\\R语言培训")
library(DMwR)
library(car)
library(rpart)
head(algae)
dim(algae)
#从本地读取数据
algae <- read.table('Analysis.txt',
          header=F,
          dec='.',
          col.names=c('season','size','speed','mxPH','mnO2','Cl',
          'NO3','NH4','oPO4','PO4','Chla','a1','a2','a3','a4',
          'a5','a6','a7'),
          na.strings=c('XXXXXXX'))#缺失值由XXXXXX表示


###################################################
### Data Visualization and Summarization
###################################################
summary(algae)#统计量

hist(algae$mxPH, prob=T)直方图

#下面是更为丰富的数据分析

par(mfrow=c(1,2))
hist(algae$mxPH, prob=T, xlab='',
      main='Histogram of maximum pH value',ylim=0:1)
lines(density(algae$mxPH,na.rm=T))
rug(algae$mxPH,side=1)
qqPlot(algae$mxPH,main='Normal QQ plot of maximum pH')




par(mfrow=c(1,1))
boxplot(algae$oPO4,ylab='Orthophosphate (oPO4)')
rug(algae$oPO4,side=2)
abline(h=mean(algae$oPO4,na.rm=T),lty=2)




plot(algae$NH4,xlab='')
abline(h=mean(algae$NH4,na.rm=T),lty=1)
abline(h=mean(algae$NH4,na.rm=T)+sd(algae$NH4,na.rm=T),lty=2)
abline(h=median(algae$NH4,na.rm=T),lty=3)
identify(algae$NH4)


plot(algae$NH4,xlab='')
clicked.lines <- identify(algae$NH4)



algae[algae$NH4 > 19000,]#索引数据框的一种方式
algae[!is.na(algae$NH4)&algae$NH4 > 19000,]

#箱图
boxplot(  a1~size, data=algae, ylab='River Size',xlab='Algal A1')

library(lattice)
bwplot(size ~ a1, data=algae,ylab='River Size',xlab='Algal A1')


library(Hmisc)
bwplot(size ~ a1, data=algae,panel=panel.bpplot, 
        probs=seq(.01,.49,by=.01), datadensity=TRUE,
        ylab='River Size',xlab='Algal A1')#实际观测值，分布函数的估计，分位数


###################################################
###Missing data###################################################
library(DMwR)
data(algae)


algae[!complete.cases(algae),]#哪些样本有缺失


nrow(algae[!complete.cases(algae),])#一共缺失多少样本


algae1 <- na.omit(algae)#把有缺失的删去


algae2 <- algae[-c(62,199),]#剔除个别的严重的样本


apply(algae,1,function(x) sum(is.na(x)))#看看样本缺失的情况，每个样本有多少个缺失


data(algae)
manyNAs(algae,0.2)#缺失变量的个数多于1/5变量的总数

algae <- algae[-manyNAs(algae),]

algae <- centralImputation(algae)#用中位数或者众数填补所有缺失值。这种方法不够准确
#只适用于大样本，有少量缺失的情形



cor(algae[,4:18],use="complete.obs")
symnum(cor(algae[,4:18],use="complete.obs"))#看变量间的相关性

data(algae)
algae <- algae[-manyNAs(algae),]
lm(PO4 ~ oPO4,data=algae)


algae[28,'PO4'] <- 42.897 + 1.293 * algae[28,'oPO4']#利用回归的方法填补


data(algae)
algae <- algae[-manyNAs(algae),]
fillPO4 <- function(oP) {
   if (is.na(oP)) return(NA)
   else return(42.897 + 1.293 * oP)
}
algae[is.na(algae$PO4),'PO4'] <- 
    sapply(algae[is.na(algae$PO4),'oPO4'],fillPO4)#构造函数实现回归填补




data(algae)#重来
algae <- algae[-manyNAs(algae),]


algae <- knnImputation(algae,k=10)#利用近邻法填补


#或者algae <- knnImputation(algae,k=10,meth='median')



###################################################
### Multiple Linear Regression
###################################################
data(algae)
algae <- algae[-manyNAs(algae), ]
clean.algae <- knnImputation(algae, k = 10)


lm.a1 <- lm(a1 ~ .,data=clean.algae[,1:12])


summary(lm.a1)



lm2.a1 <- update(lm.a1, . ~ . - season)


summary(lm2.a1)



final.lm <- step(lm.a1)


summary(final.lm)

#a.正态性的检验：
qqPlot(final.lm , labels = row.names(clean.algae), id.method="identify",
       simulate = TRUE, main = "Q-Q Plot") #请点击图上的离群点。

#b.误差的独立性的检验：
durbinWatsonTest(final.lm )  #p值不显著，说明无自相关性。
#c.线性：
crPlots(final.lm)
#d.同方差性：
ncvTest(final.lm)  #p值不显著，说明同方差。
#e.多重共线性：
vif(final.lm)
sqrt(vif(final.lm))>2  #多真则有多重共线性
#f.离群点：
outlierTest(final.lm)
#g:高杠杆值的点（若观测点的帽子值hii大于帽子均值的2或3倍，Cook统计量较大，是强影响点）：
#或者使用一个总的函数：
influencePlot(final.lm,main="Influence Plot",
              sub="Circle size is proportional to cook's distance")
#纵坐标不在|2|之间的是离群点，水平轴超过0.2的是高杠杆值的点，圆圈大小代表由库克距离反映的影响力大小。



subdata=clean.algae[-c(49,153,117,152),]

lm.r=update(final.lm,data=subdata)
influencePlot(lm.r,main="Influence Plot",
              sub="Circle size is proportional to cook's distance")
#########################################对数据变换看看效果

final.lm=update(lm.r,sqrt(a1)~.)
summary(final.lm)
qqPlot(final.lm , labels = row.names(clean.algae), id.method="identify",
       simulate = TRUE, main = "Q-Q Plot") #请点击图上的离群点。

#b.误差的独立性的检验：
durbinWatsonTest(final.lm ) 
###################################################
### Regression Trees
###################################################
library(rpart)
data(algae)
#回归树可处理缺失数据，只要不是缺失太多都可以
algae <- algae[-manyNAs(algae), ]
rt.a1 <- rpart(a1 ~ .,data=algae[,1:12])
#rpart可以构建回归树，也可以构建分类树（因变量为分类变量）
rt.a1
prettyTree(rt.a1)#画图展示结果
printcp(rt.a1)#cp统计量


rt2.a1 <- prune(rt.a1,cp=0.08)
rt2.a1
prettyTree(rt2.a1)




first.tree <- rpart(a1 ~ .,data=algae[,1:12],cp=0.01,minsplit=40,maxdepth=4)
#cp=0.04,minsplit=40,maxdepth=3这三个用一个即可，分别表示模型的lack of fit，节点包括的最少样本数，数的深度（几层）
prettyTree(first.tree)
printcp(first.tree)



second.tree <- rpart(a1 ~ .,data=algae[,1:12])
second.tree
third.tree=snip.rpart(second.tree,4)
par(mfrow=c(1,2))
prettyTree(second.tree)
prettyTree(third.tree)

prettyTree(second.tree)
snip.rpart(second.tree)
###自动修剪程序
set.seed(1234) # 种种子，保证所出结果相同
rt.a1 <- rpartXse(a1 ~ .,data=algae[,1:12])
set.seed(1234)
rt.a2=rpartXse(a1 ~ .,data=algae[,1:12], se = 1)#se:standard errors ，默认1
par(mfrow=c(1,2))
prettyTree(rt.a1)
prettyTree(rt.a2)

###################################################
### Model Evaluation and Selection
###################################################
lm.predictions.a1 <- predict(final.lm,clean.algae)
rt.predictions.a1 <- predict(rt.a1,algae)


(mae.a1.lm <- mean(abs(lm.predictions.a1-algae[,'a1'])))#平均绝对误差
(mae.a1.rt <- mean(abs(rt.predictions.a1-algae[,'a1'])))


(mse.a1.lm <- mean((lm.predictions.a1-algae[,'a1'])^2))#均方误差
(mse.a1.rt <- mean((rt.predictions.a1-algae[,'a1'])^2))


(nmse.a1.lm <- mean((lm.predictions.a1-algae[,'a1'])^2)/
                mean((mean(algae[,'a1'])-algae[,'a1'])^2))#标准化后的均方误差，越小越好
(nmse.a1.rt <- mean((rt.predictions.a1-algae[,'a1'])^2)/
                mean((mean(algae[,'a1'])-algae[,'a1'])^2))


regr.eval(algae[,'a1'],rt.predictions.a1,train.y=algae[,'a1'])#这个函数可直接输出模型拟合效果的统计量
#"mape": sum(|(t_i - p_i) / t_i|)/N 


#以下输出可视化的模型预测结果
old.par <- par(mfrow=c(1,2))
plot(lm.predictions.a1,algae[,'a1'],main="Linear Model",
     xlab="Predictions",ylab="True Values")
abline(0,1,lty=2)
plot(rt.predictions.a1,algae[,'a1'],main="Regression Tree",
     xlab="Predictions",ylab="True Values")
abline(0,1,lty=2)


#查看带样本点标记的预测结果
plot(lm.predictions.a1,algae[,'a1'],main="Linear Model",
     xlab="Predictions",ylab="True Values")
abline(0,1,lty=2)
algae[identify(lm.predictions.a1,algae[,'a1']),]


cv.rpart <- function(form,train,test,...) {
  m <- rpartXse(form,train,...)
  p <- predict(m,test)
  mse <- mean((p-resp(form,test))^2)
  c(nmse=mse/mean((mean(resp(form,train))-resp(form,test))^2))
}
cv.lm <- function(form,train,test,...) {
  m <- lm(form,train,...)
  p <- predict(m,test)
  p <- ifelse(p < 0,0,p)
  mse <- mean((p-resp(form,test))^2)#resp提取模型中所用数据的因变量的实际值
  c(nmse=mse/mean((mean(resp(form,train))-resp(form,test))^2))
}


res <- experimentalComparison(
            c(dataset(a1 ~ .,clean.algae[,1:12],'a1')),
            c(variants('cv.lm'), 
              variants('cv.rpart',se=c(0,0.5,1))),
            cvSettings(3,10,1234))

#variants函数利用所给参数组合成可选模型，例如rpart会分别对应se=0,0.5和1的三个回归树
#cVSettings的是第一个3是交叉验证的次数，10是留作测试的样本比例，1234是随便写的随机种子
summary(res)


plot(res)


getVariant('cv.rpart.v1',res)
##以下是对所有七个因变量分别建模并用交叉验证法进行模型比较

DSs <- sapply(names(clean.algae)[12:18],
         function(x,names.attrs) { 
           f <- as.formula(paste(x,"~ ."))
           dataset(f,clean.algae[,c(names.attrs,x)],x) 
         },
         names(clean.algae)[1:11])

res.all <- experimentalComparison(
                  DSs,
                  c(variants('cv.lm'),
                    variants('cv.rpart',se=c(0,0.5,1))
                   ),
                  cvSettings(5,10,1234))
plot(res.all)
plot(res.all,xlim=c(0,1))


bestScores(res.all)

#####以下是用随机森林建模
library(randomForest)
cv.rf <- function(form,train,test,...) {
  m <- randomForest(form,train,...)
  p <- predict(m,test)
  mse <- mean((p-resp(form,test))^2)
  c(nmse=mse/mean((mean(resp(form,train))-resp(form,test))^2))
}
res.all <- experimentalComparison(
                  DSs,
                  c(variants('cv.lm'),
                    variants('cv.rpart',se=c(0,0.5,1)),
                    variants('cv.rf',ntree=c(200,500,700))
                   ),
                  cvSettings(5,10,1234))


bestScores(res.all)


compAnalysis(res.all,against='cv.rf.v3',
               datasets=c('a1','a2','a4','a6'))
#结果中的显著性给出了相对于基准模型cv.rf.v3来说，其他各个模型与之相比有无显著差异
#标记+表示nmse显著高于基准模型，也就是效果差，―号含义相反







#######################对定性变量的建模
rpart和randomForest都可以，回归模型需要调用glm回归函数


library(nnet)
data(kyphosis)
fit.rp <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
fit.logit1= multinom(Kyphosis ~ Age + Number + Start, data = kyphosis)
fit.logit2=glm(Kyphosis ~ Age + Number + Start, data = kyphosis,family=binomial(link = "logit"))

pre1=predict(fit.logit1,data=kyphosis)
pre2=predict(fit.logit2,data=kyphosis,type = "response")
pre2.t=ifelse(pre2<0.5,"absent ","present")
table(pre,kyphosis$Kyphosis)
table(pre2.t,kyphosis$Kyphosis)
###########################################################
fit.rp <- rpart(Species ~ ., data =iris)
fit.logit= multinom(Species  ~ ., data = iris)
pre=predict(fit.logit,data=iris)
table(pre,iris$Species  )
######抑郁症和loan数据练习





