## 1번 
a <- seq(1.0,3.0,0.2)
a 

b <- rep(1:3,times= 3)
b

## 2번
max.temp <- c(22,27,26,24,23,26,28)
max.temp

barplot(max.temp)

barplot(max.temp, main = '주간 최대 기온',
        xlab = '섭씨', ylab = '요일',
        col = rainbow(7),
        names =c('월','화','수','목','금','토','일'),
        horiz = TRUE)

## 3번
trees
str(trees)
head(trees)

gi <-trees$Girth
vo <-trees$Volume  

plot(gi, vo, main = '나무 둘레 용적 산점도',
     xlab = '둘레', ylab = '용적',
     col = 'sky blue',
     pch=16)  

## 4번

airquality
str(airquality)
head(airquality)

par(mfrow=c(2,2), mar=c(3,3,4,2)) 
par(mfrow=c(2,2))

hist(airquality$Ozone,
     main = '오존분포', col='skyblue', xlab ='오존', ylab = '횟수') 
hist(airquality$Temp,
     main = '기온분포', col='skyblue', breaks = 5) 
hist(airquality$Solar.R,
     main = '자외선분포', col='skyblue') 
hist(airquality$Wind,
     main = '풍량분포', col='skyblue', breaks = 5) 

par(mfrow=c(1,1), mar=c(5,4,4,2)+.1) # 화면복귀
par(mfrow=c(1,1))

## 5번
library(carData)
library(dplyr)
head(TitanicSurvival)
str(TitanicSurvival)

is.na(TitanicSurvival)
TitanicSurvival %>% filter(is.na(TitanicSurvival$age))
Data1<- TitanicSurvival %>% filter(!is.na(TitanicSurvival$age))
str(Data1)

room.class <- Data1$passengerClass
str(room.class)

room.class
tbl <- table(room.class)
sum(tbl)

barplot(tbl, main = '타이타닉 클래스별 생존자', xlab = '선실등급', ylab = '탑승객수',
        col = c('blue','green','yellow'), names = c('First','Second','Third'),
        legend.text = T)