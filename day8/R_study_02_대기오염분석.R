getwd()
setwd('C:/STUDY/StudyR/day8')
getwd()
files <- c('ds.2015.csv', 'ds.2016.csv', 'ds.2017.csv', 'ds.2018.csv',
           'ds.2019.csv')

ds <- NULL
for (f in files) {
  tmp <- read.csv(f, header = T)
  ds <- rbind(ds, tmp)
  print(f)
}

str(ds)
unique(ds$mdate)
tail(ds$mdate)
range(ds$mdate)

for (i in 3:8) {
  cat(names(ds)[i], sum(is.na(ds[,i])), sum(is.na(ds[,i]))/
        nrow(ds), '\n')
}
ds <- ds[, -8]
ds <- ds[complete.cases(ds),]
str(ds)

# ds의 mdata를 문자열로 변환해서 mdata에 선언
mdata <- as.character(ds$mdate)
head(mdata)
# substr을 사용하여 mdata를 연도, 월, 시간으로 나눠주기
ds$year <- as.numeric(substr(mdata, 1,4))
ds$month <- as.numeric(substr(mdata, 5,6))
ds$hour <- as.numeric(substr(mdata, 9,10))

# locname
ds$locname <- NA
# loc에서 코드에 맞게끔 locname을 한글로 저장
ds$locname[ds$loc==111123] <- '서울'
ds$locname[ds$loc==336111] <- '목포'
ds$locname[ds$loc==632132] <- '강릉'

head(ds)
unique(ds$locname)

boxplot(PM10~locname, data=ds,
        main='미세먼지 농도 분포',
         ylim=c(1,100))

library(ggplot2)

# 연도별
tmp.year <- aggregate(ds[,7],
by=list(year=ds$year, loc=ds$locname), FUN='mean')
tmp.year$loc = as.factor(tmp.year$loc)
head(tmp.year)

ggplot(tmp.year, aes(x=year,y=x, colour=loc, group=loc)) +
  geom_line( ) +
  geom_point(size=6, shape=19, alpha=0.5) +
  ggtitle('연도별 PM10 농도 변화') +
  ylab('농도')

# 월별
tmp.month <- aggregate(ds[,7],
                      by=list(month=ds$month, loc=ds$locname), FUN='mean')
tmp.month$loc = as.factor(tmp.month$loc)
head(tmp.month)

ggplot(tmp.month, aes(x=month,y=x, colour=loc, group=loc)) +
  geom_line( ) +
  geom_point(size=3, shape=19, alpha=0.5) +
  ggtitle('월별 PM10 농도 변화') +
  ylab('농도')

# 시간대별
tmp.hour <- aggregate(ds[,7],
                       by=list(hour=ds$hour, loc=ds$locname), FUN='mean')
tmp.hour$loc = as.factor(tmp.hour$loc)
head(tmp.hour)

ggplot(tmp.hour, aes(x=hour,y=x, colour=loc, group=loc)) +
  geom_line( ) +
  geom_point(size=3, shape=19, alpha=0.5) +
  ggtitle('시간대별 PM10 농도 변화') +
  ylab('농도')

# 오염물질 농도 간의 상관관계
set.seed(1234)
plot(ds[sample(nrow(ds),5000), 3:7], lower.panel=NULL)
cor(ds[,3:7])

#미세먼지 최고점과 최저점 확인
tmp.yml <- aggregate(ds[,7],
                     by=list(year=ds$year,month=ds$month,
                             loc=ds$locname), FUN='mean')
# 가장 미세먼지가 많았던 달
idx <- which(tmp.yml$x==max(tmp.yml$x))
tmp.yml[idx,]

# 가장 미세먼지가 적었던 달
idx <- which(tmp.yml$x==min(tmp.yml$x))
tmp.yml[idx,]
