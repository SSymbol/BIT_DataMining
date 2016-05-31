#install.packages("DMwR")
library("DMwR")
algae<-read.table('Analysis.txt',
                  header = F, 
                  dec='.',
                  col.names = c('season','size','speed','mxPH','mn02','C1','NO3','NH4','oP04','P04','Chla','a1','a2','a3','a4','a5','a6','a7'),na.strings = c('XXXXXXX'))
                  
head(algae)
summary(algae)
hist(algae$mxPH,prob = T)
hist(algae$mxPH,prob = F)

#install.packages("car")
library(car)
par(mfrow=c(1,2))
hist(algae$mxPH, prob=T, xlab='',
     main='Histogram of maximum pH value',ylim=0:1)
lines(density(algae$mxPH, na.rm = T))
rug(jitter(algae$mxPH))
qq.plot(algae$mxPH, main='Normal QQ plot of maixmum pH')
par(mfrow=c(1,1))

boxplot(algae$mxPH, ylab="Orthophosphate(mxPH)")
rug(jitter(algae$mxPH),side = 2)
abline(h=mean(algae$mxPH,na.rm = T),lty=2)

plot(algae$mxPH,xlab = "")
abline(h=mean(algae$mxPH, na.rm = T), lty=1)
abline(h=mean(algae$mxPH, na.rm = T) + sd(algae$mxPH, na.rm = T)
       ,lty=2)
abline(h=median(algae$mxPH, na.rm = T), lty = 3)
identify(algae$mxPH)

plot(algae$mxPH, xlab="")
clicked.lines<-identify(algae$mxPH)
algae[clicked.lines,]

boxplot(algae$mn02, ylab="Orthophosphate(mn02)")
rug(jitter(algae$mn02),side = 2)
abline(h=mean(algae$mn02,na.rm = T),lty=2)

boxplot(algae$C1, ylab="Orthophosphate(C1)")
rug(jitter(algae$C1),side = 2)
abline(h=mean(algae$C1,na.rm = T),lty=2)

boxplot(algae$NO3, ylab="Orthophosphate(NO3)")
rug(jitter(algae$NO3),side = 2)
abline(h=mean(algae$NO3,na.rm = T),lty=2)

boxplot(algae$NH4, ylab="Orthophosphate(NH4)")
rug(jitter(algae$NH4),side = 2)
abline(h=mean(algae$NH4,na.rm = T),lty=2)

boxplot(algae$oP04, ylab="Orthophosphate(oP04)")
rug(jitter(algae$oP04),side = 2)
abline(h=mean(algae$oP04,na.rm = T),lty=2)

boxplot(algae$P04, ylab="Orthophosphate(P04)")
rug(jitter(algae$P04),side = 2)
abline(h=mean(algae$P04,na.rm = T),lty=2)

boxplot(algae$Chla, ylab="Orthophosphate(Chla)")
rug(jitter(algae$Chla),side = 2)
abline(h=mean(algae$Chla,na.rm = T),lty=2)

library(lattice)
bwplot(size ~a1, data=algae, ylab='River Size', xlab='Algal A1')
bwplot(season ~a1, data=algae, ylab='River Season', xlab='Algal A1')
bwplot(speed ~a1, data=algae, ylab='River Speed', xlab='Algal A1')

bwplot(size ~a2, data=algae, ylab='River Size', xlab='Algal A2')
bwplot(season ~a2, data=algae, ylab='River Season', xlab='Algal A2')
bwplot(speed ~a2, data=algae, ylab='River Speed', xlab='Algal A2')

bwplot(size ~a3, data=algae, ylab='River Size', xlab='Algal A3')
bwplot(season ~a3, data=algae, ylab='River Season', xlab='Algal A3')
bwplot(speed ~a3, data=algae, ylab='River Speed', xlab='Algal A3')

bwplot(size ~a4, data=algae, ylab='River Size', xlab='Algal A4')
bwplot(season ~a4, data=algae, ylab='River Season', xlab='Algal A4')
bwplot(speed ~a4, data=algae, ylab='River Speed', xlab='Algal A4')

bwplot(size ~a5, data=algae, ylab='River Size', xlab='Algal A5')
bwplot(season ~a5, data=algae, ylab='River Season', xlab='Algal A5')
bwplot(speed ~a5, data=algae, ylab='River Speed', xlab='Algal A5')

bwplot(size ~a6, data=algae, ylab='River Size', xlab='Algal A6')
bwplot(season ~a6, data=algae, ylab='River Season', xlab='Algal A6')
bwplot(speed ~a6, data=algae, ylab='River Speed', xlab='Algal A6')

bwplot(size ~a7, data=algae, ylab='River Size', xlab='Algal A7')
bwplot(season ~a7, data=algae, ylab='River Season', xlab='Algal A7')
bwplot(speed ~a7, data=algae, ylab='River Speed', xlab='Algal A7')

#将缺失部分剔除
algae[!complete.cases(algae),]
nrow(algae[!complete.cases(algae),])
algae_new<- na.omit(algae)
View(algae_new)
dim(algae_new)
write.csv(algae_new,"/Users/Michelle/Desktop/withoutNAs.csv")

#用最高频率值来填补缺失值
algae_new<- centralImputation(algae)
View(algae_new)
dim(algae_new)
write.csv(algae_new,"/Users/Michelle/Desktop/output1.csv")

#cor(algae[,4:18], use="complete.obs")
#histogram(~mxPH | season, data = algae)
#algae$season<-factor(algae$season, levels = c("spring","summer","autumn","winter"))

#通过属性的相关关系来填补缺失值
cor(algae[,4:18], use="complete.obs")
symnum(cor(algae[,4:18], use="complete.obs"))

dim(algae)
manyNAs(algae)
algae_new<-algae[-manyNAs(algae),]
lm(PO4 ~ oPO4, data = algae_new)
#algae_new[28, "PO4"]<-42.897 + 1.293*algae_new[28, "oPO4"]
fillPO4 <- function(oP){
  if(is.na(oP))
    return(NA)
  else return (42.897 + 1.293 * oP)
}
algae_new[is.na(algae_new$PO4),'PO4']<- 
  sapply(algae_new[is.na(algae_new$PO4),'oPO4'],fillPO4)
write.csv(algae_new,"/Users/Michelle/Desktop/output2.csv")

#通过数据对象之间的相似性来填补缺失值
algae_new<-algae[-manyNAs(algae),]
algae_new = knnImputation(algae,k=10)
write.csv(algae_new,"/Users/Michelle/Desktop/output3.csv")
