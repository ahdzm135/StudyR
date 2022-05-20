install.packages('HSAUR')
library(HSAUR)
data('Forbes2000')
ds <- Forbes2000
ds[!complete.cases(ds),] # 결측값확인

str(ds)
head(ds)

table(ds$country)
tmp <- sort(table(ds$country), decreasing = T)
top.10.contry <- tmp[1:10]
top.10.contry

par(mar=c(8,4,4,2))
barplot(top.10.contry, main = '기업수 상위 10개국',
        col = rainbow(10),
        las=2)

par(mar=c(5,4,4,2))

table(ds$category)
tmp <- sort(table(ds$category), decreasing = T)
top.10.category <- tmp[1:10]
top.10.category

par(mar=c(10,4,4,2))
barplot(top.10.category, main = '기업수 상위 10개 업종',
        col = 'pink',
        las=2)

par(mar=c(5,4,4,2))

tmp <- ds[ds$category %in% names(top.10.category),]
levels(tmp$category)
tmp$category <- factor(tmp$category)
levels(tmp$category)

par(mar=c(10,4,4,2))
boxplot(assets~category, data=tmp,
        ylim=c(0,100),
        xlab = ' ',
        las=2)

par(mar=c(5,4,4,2))

tmp <- ds[order(ds$marketvalue, decreasing = T),]
tmp[1:10,c('name','country','category','marketvalue')]

korea <- subset(ds, country=='South Korea')
korea[,c('rank','name','category','marketvalue')]
str(korea)

tmp <- ds[,5:8]
tmp <- tmp[complete.cases(tmp),]
plot(tmp, lower.panel=NULL)
cor(tmp)
