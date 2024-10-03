
str(winequality.red)
str(winequality.red$fixed.acidity)
str(winequality.red$sulphates)
table(winequality.red$fixed.acidity)
attach(winequality.red)

#===========================================================================================================
#=========================================================================================================
subqual8 <- subset(winequality.red,winequality.red$quality==8)
subqual7 <- subset(winequality.red,winequality.red$quality==7)
subqual6 <- subset(winequality.red,winequality.red$quality==6)
subqual5 <- subset(winequality.red,winequality.red$quality==5)
subqual4 <- subset(winequality.red,winequality.red$quality==4)
subqual3 <- subset(winequality.red,winequality.red$quality==3)

dim(subqual8)[1]
dim(subqual7)[1]
dim(subqual6)[1]
dim(subqual5)[1]
dim(subqual4)[1]
dim(subqual3)[1]
#we classify as low qulity 3 and 4, medium 5-6-7, and high quality 8
height<-c(63,1518,18)
barplot(height, names = c("low", "medium", "high"), col = "pink", main= "Sample quality distribution")


#fixed acidity (4.5<x<9)

boxplot((winequality.red$fixed.acidity), horizontal = TRUE, col = "yellow")
hist((subqual8$fixed.acidity), breaks= c(4,8,12,16), xlab = "Fixed acidity", main = "Fixed acidity for Quality 8", col = "yellow", freq = FALSE)
hist((subqualmeno8$fixed.acidity), breaks= c(4,8,12,16), xlab = "Fixed acidity", main = "Fixed acidity for Quality less than 8", col = "yellow", freq = FALSE)

subqual8 <- subset(winequality.red,winequality.red$quality==8)
subqualmeno8 <- subset(winequality.red,winequality.red$quality<8)
subqual8fix <- subset(subqual8$fixed.acidity,subqual8$fixed.acidity>9)
subqualmeno8fix <- subset(subqualmeno8$fixed.acidity,subqualmeno8$fixed.acidity>9)
length(subqual8fix)
length(subqualmeno8fix)
length(subqual8$fixed.acidity)

#Quality 8: 10/18 55.55% --> sta nel range più bevibilità c'è --> consumatore finale è più contento --> non deve essere artificiale. Troppo bassa,
#non ti prendono la seconda bottiglia --> bisogna prendere dal 70% in sù
#Quality 7: 114/199 57.29% --> sta nel range 
#Quality 6: 452/638 70.85% --> sta nel range 
#Quality 5: 531/681 77.97% --> sta nel range 
#Quality 4: 44/53 83.02% --> sta nel range 
#Quality 3: 7/10 70% --> sta nel range 

#There are no wine with quality lower than 8 that have a fixed acidity lower than 4.5
#However, the no of those that are within the range is 1148/1581 (72%)



#citric acid 

boxplot((winequality.red$citric.acid), horizontal = TRUE, col = "green")
hist((subqual8$citric.acid), breaks= c(0, 0.3,0.6,1), xlab = "Citric Acid", main = "Citric Acid for Quality 8", col = "green", freq = FALSE)
hist((subqualmeno8$citric.acid), breaks= c(0, 0.3,0.6,1), xlab = "Citric Acid", main = "Citric Acid for Quality less than 8", col = "green", freq = FALSE)

subqual8 <- subset(winequality.red,winequality.red$quality==8)
subqualmeno8 <- subset(winequality.red,winequality.red$quality<8)
subqual8cit <- subset(subqual8$citric.acid,subqual8$citric.acid==0)
subqualmeno8cit <- subset(subqualmeno8$citric.acid,subqualmeno8$citric.acid==0)
length(subqual8cit)
length((subqualmeno8cit))
length(winequality.red$citric.acid)


#8.25% doesn't have citric acids 
#acido citrico pù acidità al vino, quindi se ce n'è zero, vino completamente in maturazione 
#è complesso e più naturale --> più buono
#annata trppo calda e il vino non ha le caratteristiche organolettiche, aggiungono acido citrico
#è come se nella pasta non mettessi il sale
#Mosto/Vino 0 - 1 g/l
max(winequality.red$citric.acid)
min(winequality.red$citric.acid)

subqual8$citric.acid
summary(subqual8$citric.acid) #0.3911 annata troppo calda e il vino non ha le caratteristiche organolettiche


#8 --> 0 wine with 0 citric acid
#7 --> 8 wines with 0 citric acid
#6 --> 54 wines with 0 citric acid --> annate migliori infatti meno acido citirico, più naturali
#5 --> 57 wines with 0 citric acid --> annate migliori infatti meno acido citirico, più naturali
#4 --> 10 wines with 0 citric acid
#3 --> 3 wines with 0 citric acid


#pH 

boxplot((winequality.red$pH), horizontal = TRUE, col = "lightblue")
hist((subqual8$pH), breaks= c(0, 0.3,0.6,1), xlab = "pH", main = "pH for Quality 8", col = "lightblue", freq = FALSE)
hist((subqualmeno8$pH), breaks= c(0, 0.3,0.6,1), xlab = "pH Acid", main = "pH for Quality less than 8", col = "lightblue", freq = FALSE)

subqual8 <- subset(winequality.red,winequality.red$quality==8)
subqualmeno8 <- subset(winequality.red,winequality.red$quality<8)
subqual8pHmin <- subset(subqual8$pH,subqual8$pH<=3.2)
subqualmeno8pHmin <- subset(subqualmeno8$pH,subqualmeno8$pH<=3.2)
length(subqual8pHmin)
length(subqualmeno8pHmin)

subqual8pHmax <- subset(subqual8$pH,subqual8$pH>=3.6)
subqualmeno8pHmax <- subset(subqualmeno8$pH,subqualmeno8$pH>=3.6)
length(subqual8pHmax)
length(subqualmeno8pHmax)

#Quality 8: 6/18 33.3% --> è acido (percepisci durezze = quasi tannico); 1/18 5.56% --> è dolce, ma il tempo di invecchiamento è minore (quindi va bevuto prima); 61.11% sta nel range
#Quality 7: 55/199 27.64% --> è acido; 5/199 2.51% --> è meno acido, ma il tempo di conservazione è minore; 82.25% sta nel range
#Quality 6: 138/638 21.68% --> è acido; 23/638 3.61% --> è meno acido, ma il tempo di conservazione è minore; 74.76% sta nel range
#Quality 5: 182/681 26.73% --> è acido; 20/681 2.94% --> è meno acido, ma il tempo di conservazione è minore; 70.34% sta nel range
#Quality 4: 6/53 11.32% --> è acido; 5/53 9.43% --> è meno acido, ma il tempo di conservazione è minore; 79.25% sta nel range
#Quality 3: 1/10 10% --> è acido; 1/10 10% --> è meno acido, ma il tempo di conservazione è minore; 80% sta nel range

#nelle filtrazione c'è l'albumina, perché pesante e inodore --> toglie impurità di vino e quindi abbassa PH


#sulphates 

boxplot((winequality.red$sulphates), horizontal = TRUE, col = "orange")
hist((subqual8$sulphates), breaks= c(0, 0.3,0.6,1), xlab = "Sulphates", main = "Sulphates for Quality 8", col = "orange", freq = FALSE)
hist((subqualmeno8$sulphates), breaks= c(0, 0.3,0.6,1), xlab = "Sulphates Acid", main = "Sulphates for Quality less than 8", col = "orange", freq = FALSE)


#solfati aggiunti --> anidride solforosa <1.5 (5 -->3 di 5 e 2 di 7)

subqual8 <- subset(winequality.red,winequality.red$quality==8)
subqualmeno8 <- subset(winequality.red,winequality.red$quality<8)
subqual8sul <- subset(subqual8$sulphates,subqual8$sulphates<=1.5)
subqualmeno8sul <- subset(subqualmeno8$sulphates,subqualmeno8$sulphates<=1.5)

length(subqual8sul)
length(subqualmeno8sul)

#Gli unici vini che non hanno solfati inferiori a 1.5 sono quelli con qualità 5 (3) e 7 (2)
#Quest'ultimi, secondo la legislazione italiana, non sono possibili



#alcohol 
boxplot((winequality.red$alcohol), horizontal = TRUE, col = "purple")
hist((subqual8$alcohol), breaks= c(0, 0.3,0.6,1), xlab = "Alcohol", main = "Alcohol for Quality 8", col = "purple", freq = FALSE)
hist((subqualmeno8$alcohol), breaks= c(0, 0.3,0.6,1), xlab = "Alcohol Acid", main = "Alcohol for Quality less than 8", col = "purple", freq = FALSE)


subqual8 <- subset(winequality.red,winequality.red$quality==8)
subqualmeno8 <- subset(winequality.red,winequality.red$quality<8)
subqual8alcmin <- subset(subqual8$alcohol,subqual8$alcohol<=11)
subqualmeno8alcmin <- subset(subqualmeno8$alcohol,subqualmeno8$alcohol<=11)
length(subqual8alcmin)
length(subqualmeno8alcmin)

subqual8alcmax <- subset(subqual8$alcohol,subqual8$alcohol>=14)
subqualmeno8alcmax <- subset(subqualmeno8$alcohol,subqualmeno8$alcohol>=14)
length(subqual8alcmax)
length(subqualmeno8alcmax)


#Quality 8: 4/18 22.22% --> sotto l'11% (non si può chiamere vino = moscato rosa, bracchetto etc. --> bevanda alcolica a base di uva); 2/18 11.11% sopra il 14% (vino se non vengono aggiunti zuccheri); 66.67% sta nel range
#Quality 7: 72/199 36.18% --> sotto l'11%; 1/199 0.50% --> sopra il 14%; 63.32% sta nel range
#Quality 6: 433/638 67.87% --> sotto l'11%; 4/638 0.63% --> sopra il 14%; 31.50% sta nel range
#Quality 5: 631/681 92.66% --> sotto l'11%; 1/681 0.15% --> sopra il 14%; 7.20% sta nel range
#Quality 4: 41/53 77.36% --> sotto l'11%; 0/53 0% --> sopra il 14%; 22.64% sta nel range
#Quality 3: 10/10 100% --> sotto l'11%; 0/53 0% --> sopra il 14%; 0% sta nel range

#quando si beve non si percepisce l'alcol perché l'acidità è più alta va a coprire la sensazione alcolica di calore
#alcohol è un conservante quindi quelli con alcohol sotto l'11 devono essere consumati prima

#===========================================================================================================
#=========================================================================================================
  

#totale
summary(sulphates) 
summary(fixed.acidity) 

#divisi per quality per sulphates
tapply(sulphates, quality, summary) 
tapply(sulphates, quality, mean) 
#più è alta la qualità del vino più è alta la presenza di solfati


mean(winequality.red$sulphates)
median(winequality.red$sulphates)
boxplot(sulphates, horizontal = TRUE, col = "orange")$out
range(winequality.red$sulphates) #1.67
#calcolo outliers, osservazioni non regolari (59), sono right skewed. so as showed above mean > median  

#divisi per quality per fixed acidity
tapply(fixed.acidity, quality, summary)
tapply(fixed.acidity, quality, mean)
#l'acidità rimane ad un livello costante according to qualities. 

#The presence of acidity in wine is essential for maintaining its freshness and liveliness.
#It imparts a zesty and slightly tart flavor while playing a crucial role in the wine's overall harmony. 
#Moreover, acidity enhances a wine's compatibility with various food options and significantly influences its aging potential.

mean(winequality.red$fixed.acidity)
median(winequality.red$fixed.acidity)
boxplot(fixed.acidity, horizontal = TRUE, col = "yellow")$out
#calcolo outliers, osservazioni non regolari (49), sono right skewed. so as showed above mean > median
range(winequality.red$fixed.acidity) #11.3

#divisi per quality per tasso alcolemico
tapply(alcohol, quality, summary)
tapply(alcohol, quality, mean)
#più è alta la qualità più il vino è alcolico. infatti dalla media le qualità piu alte (7 e 8) si avvicinano a valori come 7: 11.465913 
# 8: 12.094444 
#la gradiazione alcolica di un vino perfetta è 13,6 gradi alcolici. 
# + curiosities 

mean(winequality.red$alcohol)
median(winequality.red$alcohol)
boxplot(alcohol, horizontal = TRUE, col = "purple")$out
#calcolo outliers, osservazioni non regolari (13), sono right skewed. so as showed above mean > median
range(winequality.red$alcohol) #6.5


#===========================================================================================================
#=========================================================================================================

#Confidence Intervals Sulphates con qualità 8

subqual8 <- subset(winequality.red,winequality.red$quality==8)
subqual8sulp <- subqual8$sulphates
m8al <- mean(subqual8sulp)
v8al <-var(subqual8sulp)
sd8al <- sqrt(v8al)
z8al <- qnorm(1-0.05/2)
n8al <- length(subqual8sulp)
LCL8al <- m8al-z8al*(sd8al/sqrt(n8al))
UCL8al <- m8al+z8al*(sd8al/sqrt(n8al))
CI8al <- c(LCL8al, UCL8al)
round(CI8al, 3)


#Confidence Intervals Sulphates con qualità minore di 8

subqualmeno8 <- subset(winequality.red,winequality.red$sulphates<8)
subqualmeno8sulp <- subqualmeno8$sulphates
mminal <- mean(subqualmeno8sulp)
vminal <-var(subqualmeno8sulp)
sdminal <- sqrt(vminal)
zminal <- qnorm(1-0.05/2)
nminal <- length(subqualmeno8sulp)
LCLminal <- mminal-zminal*(sdminal/sqrt(nminal))
UCLminal <- mminal+zminal*(sdminal/sqrt(nminal))
CIminal <- c(LCLminal, UCLminal)
round(CIminal, 3)

#===========================================================================================================
#=========================================================================================================

#Confidence Intervals Fixed Acidity con qualità 8

subqual8 <- subset(winequality.red, winequality.red$quality==8)
subfix8 <- subqual8$fixed.acidity
m8x <- mean(subfix8)
v8x <- var(subfix8)
sd8x <-sqrt(v8x)
alpha8x <- 0.05
z8x <- qnorm(1-alpha8x/2)
n8x <- length(subfix8)
LCL8x <- m8x - z8x*(sd8x/sqrt(n8x))
UCL8x <- m8x + z8x*(sd8x/sqrt(n8x))
CI8x <- c(LCL8x,UCL8x)
round(CI8x, 3)


#Confidence Intervals Fixed Acidity con qualità minore di 8

subqualmeno8 <- subset(winequality.red, winequality.red$quality<8)
subfix8 <- subqualmeno8$fixed.acidity
m8x <- mean(subfix8)
v8x <- var(subfix8)
sd8x <-sqrt(v8x)
alpha8x <- 0.05
z8x <- qnorm(1-alpha8x/2)
n8x <- length(subfix8)
LCL8x <- m8x - z8x*(sd8x/sqrt(n8x))
UCL8x <- m8x + z8x*(sd8x/sqrt(n8x))
CI8x <- c(LCL8x,UCL8x)
round(CI8x, 3)

#===========================================================================================================
#=========================================================================================================
  
#Confidence Intervals Alcohol con qualità 8

subaqual8 <- subset(winequality.red,winequality.red$quality==8)
subal8 <- subaqual8$alcohol
m8al <- mean(subal8)
v8al <-var(subal8)
sd8al <- sqrt(v8al)
z8al <- qnorm(1-0.05/2)
n8al <- length(subal8)
LCL8al <- m8al-z8al*(sd8al/sqrt(n8al))
UCL8al <- m8al+z8al*(sd8al/sqrt(n8al))
CI8al <- c(LCL8al, UCL8al)
round(CI8al, 3)


#Confidence Intervals Alcohol con qualità minore di 8

subqualmeno8 <- subset(winequality.red,winequality.red$quality<8)
subalmin <- subqualmeno8$alcohol
mminal <- mean(subalmin)
vminal <-var(subalmin)
sdminal <- sqrt(vminal)
zminal <- qnorm(1-0.05/2)
nminal <- length(subalmin)
LCLminal <- mminal-zminal*(sdminal/sqrt(nminal))
UCLminal <- mminal+zminal*(sdminal/sqrt(nminal))
CIminal <- c(LCLminal, UCLminal)
round(CIminal, 3)

#===========================================================================================================
#=========================================================================================================

#Confidence Intervals pH con qualità 8
  
subqua8l <- subset(winequality.red,winequality.red$quality==8)
subal8 <- subqua8l$pH
m8al <- mean(subal8)
v8al <-var(subal8)
sd8al <- sqrt(v8al)
z8al <- qnorm(1-0.05/2)
n8al <- length(subal8)
LCL8al <- m8al-z8al*(sd8al/sqrt(n8al))
UCL8al <- m8al+z8al*(sd8al/sqrt(n8al))
CI8al <- c(LCL8al, UCL8al)
round(CI8al, 3)

#Confidence Intervals pH con qualità minore di 8

subqualmeno8 <- subset(winequality.red,winequality.red$quality<8)
subalmin <- subqualmeno8$pH
mminal <- mean(subalmin)
vminal <-var(subalmin)
sdminal <- sqrt(vminal)
zminal <- qnorm(1-0.05/2)
nminal <- length(subalmin)
LCLminal <- mminal-zminal*(sdminal/sqrt(nminal))
UCLminal <- mminal+zminal*(sdminal/sqrt(nminal))
CIminal <- c(LCLminal, UCLminal)
round(CIminal, 3)

#===========================================================================================================
#=========================================================================================================

#Confidence Intervals Citric Acid con qualità 8

subqual8 <- subset(winequality.red,winequality.red$quality==8)
subal8 <- subqual8$citric.acid
m8al <- mean(subal8)
v8al <-var(subal8)
sd8al <- sqrt(v8al)
z8al <- qnorm(1-0.05/2)
n8al <- length(subal8)
LCL8al <- m8al-z8al*(sd8al/sqrt(n8al))
UCL8al <- m8al+z8al*(sd8al/sqrt(n8al))
CI8al <- c(LCL8al, UCL8al)
round(CI8al, 3)

#Confidence Intervals Citric Acid con qualità meno di 8

subqualmeno8 <- subset(winequality.red,winequality.red$quality<8)
subal8 <- subqualmeno8$citric.acid
m8al <- mean(subal8)
v8al <-var(subal8)
sd8al <- sqrt(v8al)
z8al <- qnorm(1-0.05/2)
n8al <- length(subal8)
LCL8al <- m8al-z8al*(sd8al/sqrt(n8al))
UCL8al <- m8al+z8al*(sd8al/sqrt(n8al))
CI8al <- c(LCL8al, UCL8al)
round(CI8al, 3)
#=====================================================================================================
#==============================================================================================

#grafico totale

str(winequality.red)
subqual <- subset(winequality.red, winequality.red$quality<9)
str(subqual)
subal<- subqual$alcohol

mal <- mean(subal)
val <-var(subal)
sdal <- sqrt(val)
alphaal <- 0.05
zal <- qnorm(1-alphaal/2)
nal <- length(subal)

LCLal <- mal - zal*(sdal/sqrt(nal))
UCLal <- mal + zal*(sdal/sqrt(nal))
CIal <- c(LCLal, UCLal)
round(CIal, 3)


#=========================================================================================================
#========================================================================================

#Comparison
LCL1 <- mean(subqualmeno8$fixed.acidity)-mean(subqual8$fixed.acidity) - qnorm(1-0.05/2)*sqrt(var(subqualmeno8$fixed.acidity)/dim(subqualmeno8)[1])+sqrt(var(subqual8$fixed.acidity)/dim(subqual8)[1])  
UCL1 <- mean(subqualmeno8$fixed.acidity)-mean(subqual8$fixed.acidity) + qnorm(1-0.05/2)*sqrt(var(subqualmeno8$fixed.acidity)/dim(subqualmeno8)[1])+sqrt(var(subqual8$fixed.acidity)/dim(subqual8)[1])

alpha <- 0.05 
qnorm(1-0.05/2)
LCL <- mean(subqualmeno8$sulphates)-mean(subqual8$sulphates) - qnorm(1-0.05/2)*sqrt(var(subqualmeno8$sulphates)/dim(subqualmeno8)[1])+sqrt(var(subqual8$sulphates)/dim(subqual8)[1]) #-0.09204396
UCL <-  mean(subqualmeno8$sulphates)-mean(subqual8$sulphates) + qnorm(1-0.05/2)*sqrt(var(subqualmeno8$sulphates)/dim(subqualmeno8)[1])+sqrt(var(subqual8$sulphates)/dim(subqual8)[1]) #-0.07531981
#since the value does not cross the 0 there is an huge difference between the average sulphates in wine with a quality < 8 and wine with quality == 8

#==============================================================================================================================================================================================================
alpha <- 0.05 
qnorm(1-0.05/2)
LCL <- mean(subqual8$alcohol)-mean(subqualmeno8$alcohol) - qnorm(1-0.05/2)*sqrt(var(subqualmeno8$alcohol)/dim(subqualmeno8)[1])+sqrt(var(subqual8$alcohol)/dim(subqual8)[1]) 
UCL <-  mean(subqualmeno8$alcohol)-mean(subqual8$alcohol) + qnorm(1-0.05/2)*sqrt(var(subqualmeno8$alcohol)/dim(subqualmeno8)[1])+sqrt(var(subqual8$alcohol)/dim(subqual8)[1]) 
#since the value do not cross the 0 there is an huge difference between the average alcohol in wine with a quality < 8 and wine with quality == 8

#===========================================================================================================
#=========================================================================================================


#Hypothesis testing sulphates of quality 8(x) and sulphates <8

str(winequality.red)
qualmin <- c(winequality.red$quality<8)
qual <- cqual <- c(winequality.red$quality)
sulp <- c(winequality.red$sulphates)
round(tapply(sulp, qualmin, mean)[2],3)
round(tapply(sulp, qual, mean)[6],3)

set.seed(123)
attach((winequality.red))
subqual8 <-subset(winequality.red, winequality.red$quality==8)
samqual8sulp <-sample(subqual8$sulphates,18)

subqualmin8 <-subset(winequality.red, winequality.red$quality<8)
samqualmin8sulp <-sample(subqualmin8$sulphates,1581)

x <-mean(samqual8sulp)
y <- mean(samqualmin8sulp)


varx <- var(samqual8sulp)
vary <- var(samqualmin8sulp)

s2p <-((17*varx)+(1580*vary))/(18+1581-2)

length(samqual8sulp)
length(samqualmin8sulp)

t <- (x-y)/sqrt(s2p/18+s2p/1581)

#Dimostarre che H1: mux-muy>0
#H0: mux-muy<=0 
#Reject H0 if t>tnx+ny-2, alpha = qt

qn <-qnorm(1-0.05/2)

#t < tnx+ny-2, alpha --> We don't reject H0 --> mux-muy>0 --> Wine with quality < 8 have less sulphates

#===========================================================================================================
#=========================================================================================================

#Hypothesis testing fixed acidity of quality 8(x) and  <8

subqual8 <-subset(winequality.red, winequality.red$quality==8)
samqual8fix <-sample(subqual8$fixed.acidity,18)


subqualmin8 <-subset(winequality.red, winequality.red$quality<8)
samqualmin8fix <-sample(subqualmin8$fixed.acidity,1581)

x <-mean(samqual8fix)
y <- mean(samqualmin8fix)

varx <- var(samqual8fix)
vary <- var(samqualmin8fix)

s2p <-((17*varx)+(1580*vary))/(18+1581-2)

length(samqual8sulp)
length(samqualmin8sulp)

t <- (x-y)/sqrt(s2p/18+s2p/1581)

#Dimostarre che H1: mux-muy>0
#H0: mux-muy<=0 
#Reject H0 if t>tnx+ny-2, alpha = qn

qn <-qnorm(1-0.05/2)

#t < tnx+ny-2, alpha --> We don't reject H0 --> mux-muy<0 --> Wine with 
#quality < 8 has a greater mean. However, data collected show that, 
#since variances of quality <8 and 
#quality =8 differ (1.476034), it's possible that many single values 
#taken from quality < 8 are lower than those from quality 8.
#--> we don't have enough evidence --> fixed acidity doesn't influence quality


#===========================================================================================================
#=========================================================================================================
  
#Hypothesis testing alcohol of quality 8(x) and <8

subqual8 <-subset(winequality.red, winequality.red$quality==8)
samqual8alc <-sample(subqual8$alcohol,18)


subqualmin8 <-subset(winequality.red, winequality.red$quality<8)
samqualmin8alc <-sample(subqualmin8$alcohol,1581)

x <-mean(samqual8alc)
y <- mean(samqualmin8alc)

varx <- var(samqual8alc)
vary <- var(samqualmin8alc)

s2p <-((17*varx)+(1580*vary))/(18+1581-2)

length(samqual8sulp)
length(samqualmin8sulp)

t <- (x-y)/sqrt(s2p/18+s2p/1581)

#Dimostarre che H1: mux-muy>0
#H0: mux-muy<=0 
#Reject H0 if t>tnx+ny-2, alpha = qn

qn <-qnorm(1-0.05/2)

#t > tnx+ny-2, alpha --> We reject H0 --> mux-muy>0 --> Wine with quality 8 
#has more alcohol. 

#===========================================================================================================
#=========================================================================================================

#Kurtosis

library(moments)
  
#calculate skewness
skewness(subqual8$sulphates)
#calculate kurtosis
kurtosis(subqual8$sulphates)
#Jarque test
jarque.test(subqual8$sulphates)

#Jarque-Bera Normality Test
#data:  subqual8$sulphates
#JB = 7.793, p-value = 0.02031
#alternative hypothesis: greater

#===================================================================================
#=======================================================================================

#alcohol
boxplot(subqualmeno8$alcohol, main = "alcohol", horizontal = TRUE, col = "violet", xlab = "alcohol" )
skewness(subqualmeno8$alcohol)
#kurtosis
kurtosis(subqualmeno8$alcohol)
#jarque test 
jarque.test(subqualmeno8$alcohol)


boxplot(subqual8$pH, main = "pH", horizontal = TRUE, col = "lightblue", xlab = "pH" )
skewness(subqualmeno8$pH)
#kurtosis
kurtosis(subqualmeno8$pH)
#jarque test 
jarque.test(subqualmeno8$pH)

boxplot(subqual8$fixed.acidity, main = "fixed acidity", horizontal = TRUE, col = "yellow", xlab = "fixed.acidity" )

skewness(subqual8$fixed.acidity)
#kurtosis
kurtosis(subqual8$fixed.acidity)
#jarque test 
jarque.test(subqual8$fixed.acidity)
#kurtosis
kurtosis(subqual8$fixed.acidity)
#jarque test 
jarque.test(subqual8$fixed.acidity)

library(vioplot)
vioplot(subqual8$sulphates, names=c("sulphates"), col="orange", horizontal = TRUE)
title("Violin Plots of Miles Per Gallon")

var(subqual8$sulphates)
sqrt(0.01331242)

range(subqual8$sulphates)
#kurtosis caluclation: 
library(moments)
#skewness
skewness(subqual8$sulphates)
#kurtosis
kurtosis(subqual8$sulphates)
#jarque test 
jarque.test(subqual8$sulphates)

boxplot(subqual8$citric.acid, main = "citric acid", horizontal = TRUE, col = "green", xlab = "citric acid" )
range(subqual8$citric.acid)
#kurtosis caluclation: 
library(moments)
#skewness
skewness(subqual8$citric.acid)
#kurtosis
kurtosis(subqual8$citric.acid)
#jarque test 
jarque.test(subqual8$citric.acid)

#kurtosis caluclation: 
library(moments)
#skewness
skewness(subqual8$fixed.acidity)
#kurtosis
kurtosis(subqual8$fixed.acidity)
#jarque test 
jarque.test(subqual8$fixed.acidity)


#histogram subqual8 PH
hist(subqual8$pH, freq= FALSE, xlim =c(2,5), main = "pH", col = "green" )
#kurtosis caluclation: 
library(moments)
#skewness
skewness(subqual8$pH)
#kurtosis
kurtosis(subqual8$pH)
#jarque test 
jarque.test(subqual8$pH)

#histogram subqual8 alcohol 
hist(subqual8$alcohol, freq= FALSE, xlim =c(7,16), main = "alcohol", col = "pink")
#kurtosis caluclation: 
library(moments)
#skewness
skewness(subqual8$alcohol)
#kurtosis
kurtosis(subqual8$alcohol)
#jarque test 
jarque.test(subqual8$alcohol)


#histogram subqual8 citric acid 
hist(subqual8$citric.acid, freq= FALSE, xlim =c(-1,2), main = "citric acid", col = "yellow")
#kurtosis caluclation: 
library(moments)
#skewness
skewness(subqual8$citric.acid)
#kurtosis
kurtosis(subqual8$citric.acid)
#jarque test 
jarque.test(subqual8$citric.acid)
  
#===================================================================================
#=======================================================================================
  
#Regression model

round(cor(winequality.red),3)
pairs(winequality.red)

?heatmap
heatmap(cor(winequality.red))


panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

pairs(winequality.red, gap=0, row1attop=FALSE)

#citric acid & fixed acidity  --> 0.67
#According to our dataset, is the second strongest relationship (r closest to 1)
#1st strongest pH and fixed acidity --> 0.68

#===================================================================================
#=======================================================================================

#Regression model Fixed acidity & Citric acid

fixed_citric <- lm(fixed.acidity ~ citric.acid, data = winequality.red)
plot(winequality.red$citric.acid, winequality.red$fixed.acidity)
abline(fixed_citric, col = "red") 
summary(fixed_citric) 
#1) r == 0.4512, variability ben spiegata dal modello 
#2) H0: b1= 0 ; H1: b1 =/ 0 --> H0 is rejeceted since P-value (2e-16 )< alpha (0.05), no need to rerun the model
predict.lm(fixed_citric, newdata = data.frame(citric.acid=c(0.5,0.7,0.9))) # 9.694599 10.895311 12.096023 
#positive related--> by increasing citric acid by 1 unit the fixed acidity will increase by 6.00356

#===================================================================================
#=======================================================================================

#Regression model Citric Acidity & pH
#ph: depdendent 
#citric : independent
citric_ph <- lm(pH ~ citric.acid, data = winequality.red)
plot(winequality.red$citric.acid, winequality.red$pH)
abline(citric_ph, col = "red")
summary(citric_ph) #0.2937 variability non ben spiegata dal modello

#===================================================================================
#=======================================================================================

#Regression model Alcohol & Density
#alcohol e density
#alcohol: depdendent 
#density : independent
alcohol_density <- lm(alcohol ~ density, data = winequality.red)
plot(winequality.red$density, winequality.red$alcohol, xlab = "density", ylab = "alcohol", main = "SIMPLE LINEAR REGRESSION MODEL")
abline(alcohol_density, col = "red") #--> inverse related
summary(alcohol_density) #0.2462 variability modello non spiegato bene

#===================================================================================
#=======================================================================================

#Regression model Fixed Acidity & Density

#fixed acidity e density
#fixed acidity: indepedendent 
#density : dependent

fixed.acidity_density<- lm(density ~ fixed.acidity, data = winequality.red)
plot(winequality.red$fixed.acidity, winequality.red$density, xlab = "fixed acity", ylab = "density", main = "SIMPLE LINEAR REGRESSION MODEL")
abline(fixed.acidity_density, col = "red")
summary(fixed.acidity_density) 
#1) r^2=0.4463 variability ben spiegata dal modello
#2) H0: b1= 0 ; H1: b1 =/ 0 --> H0 is rejeceted since P-value (2e-16 )< alpha (0.05), no need to rerun the model
predict.lm(fixed.acidity_density, newdata = data.frame(fixed.acidity=c(10, 14))) #0.9979635 1.0008602 
#positive related--> by increasing fixed acidity by 1 unit the density will increase by 7.242e-04. 

#===================================================================================
#=======================================================================================

#Regression model Fixed Acidity & pH
#pH = dependent 
#fixed = indeprendent
reg<- lm(pH~fixed.acidity, data=winequality.red)
plot(winequality.red$fixed.acidity,winequality.red$pH, xlab="fixed acidity", ylab = "pH", main= "Simple linear regression model")
abline(reg,col="red")
summary(reg)
#1) r^2= 0.4665 , variability well explained by the model
#2) H0: b1= 0 ; H1: b1 =/ 0 --> H0 is rejeceted since P-value (2e-16 )< alpha (0.05), no need to rerun the model
predict.lm(reg, newdata = data.frame(fixed.acidity=c(13,9,5)))
#negative related, as fixed increase PH decrease by -0.060

#===================================================================================
#====================================================================================

#Quality & Alcohol
boxplot(alcohol~quality, data = winequality.red, main = "Quality VS Alcohol", col="pink")
lm_model <- lm(alcohol~quality, data = winequality.red)
plot(winequality.red$quality,winequality.red$alcohol)
abline(lm_model, col = "red", lty = 11) #questo va messo per forza coi puntini

#Quality & Fixed acidity
boxplot(fixed.acidity~quality, data = winequality.red, main = "Quality VS Fixed Acidity", col="pink")
lm_model <- lm(fixed.acidity~quality, data = winequality.red)
plot(winequality.red$quality,winequality.red$fixed.acidity)
abline(lm_model, col = "red", lty = 11) #quasi parallela alle assi x

#Quality & pH

boxplot(pH~quality, data = winequality.red, main = "Quality VS Fixed Acidity", col="pink")
lm_model <- lm(pH~quality, data = winequality.red)
plot(winequality.red$quality,winequality.red$pH)
abline(lm_model, col = "red", lty = 11) #quasi parallela alle assi x

#Quality & Citric acid

boxplot(citric.acid~quality, data = winequality.red, main = "Quality VS Fixed Acidity", col="pink")
lm_model <- lm(citric.acid~quality, data = winequality.red)
plot(winequality.red$quality,winequality.red$citric.acid)
abline(lm_model, col = "red", lty = 11) 


#=================================
library(ggcorrplot)
# Visualize the correlation matrix
# --------------------------------
corr <- round(cor(winequality.red[,-c(8,9)]), 1)
# method = "square" (default)
ggcorrplot(corr)
# Reordering the correlation matrix
# --------------------------------
# using hierarchical clustering
ggcorrplot(corr, hc.order = TRUE, outline.col = "white")
# Types of correlogram layout
# --------------------------------
# Get the lower triangle
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           outline.col = "darkred")
# Get the upper triangle
ggcorrplot(corr, hc.order = TRUE, type = "upper",
           outline.col = "red")
# Change colors and theme
# --------------------------------
# Argument colors
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("red", "pink", "darkred"))
# Add correlation coefficients
# --------------------------------
# argument lab = TRUE
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE)





  
  
  
