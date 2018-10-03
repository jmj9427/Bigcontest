
###############################################################################################
########################################Data setting###########################################
###############################################################################################
## data load
setwd("C:/Users/samsung/Desktop/빅콘")
movie=read.csv("movie0920.csv")

## load packages
library(reshape)
library(reshape2)
library(ggplot2)
library(cluster)
library(plyr)

## data 확인
length(which(is.na(movie))) #0
str(movie)
dim(movie)





###############################################################################################
########################################Data processing########################################
###############################################################################################
## 날짜 필드 추가(엑셀작업)

## 영화코드번호 필드 추가(엑셀작업) #영화명이 중복되는 것이 있기 때문에, 영화별 고유값을 설정함

## 누적최대관객수 필드 추가
movie.List <- as.matrix(unique(movie$영화코드))
max.aud <- c()
for (i in 1:length(movie.List)){
  max.aud[i] <- max(movie[movie$영화코드==movie.List[i],"누적관객수"])
  if (i%%100==0) print(i)
}
aud <- data.frame(cbind(movie.List,max.aud))
names(aud)=c('영화코드','누적최대관객수')
movie <- join(movie, aud, by='영화코드')


## 누적최대관객수 100000명 이하 데이터 삭제 
movie.List <- unique(movie$영화코드)
movie.List[order(movie.List)]
movie.TotalSeen <- c()
for (i in 1:length(movie.List)){
  movie.TotalSeen[i]=max(movie[movie$영화코드==movie.List[i],"누적최대관객수"])
  if (i%%100==0) print(i)
}

movie.seen <- data.frame(as.matrix(movie.List),movie.TotalSeen)
movie.seen

sum(movie.seen[,2]>10^5)  
var <- NULL
for (i in 1:length(movie.seen[,2])){
  if(movie.seen[i,2] > 10^5)  var <- c(var,i)
}
movie.seen[var,]
movie.Clean <- as.matrix(movie.seen[var,1]) #추출한 영화코드번호와 매칭되는 데이터세트 구성
movie.Name <- c()
for(i in 1:length(movie.Clean)){
  movie.Name <- c(movie.Name, which(movie[,5]==movie.Clean[i]))
}
movie <- movie[movie.Name,]
## 요일구분 필드 추가
movie$요일 = weekdays(as.Date(movie$일자))
movie$요일 = factor(movie$요일)
head(movie$요일)
movie$요일구분 = movie$요일
movie$요일구분 = ifelse(movie$요일구분=="금요일",1,ifelse(movie$요일구분=="토요일"|movie$요일구분=="일요일",2,0))

## 일자구분 필드 추가(평일,주말,추석,설,공휴일)
str(movie)
movie$일자구분 =ifelse(movie$요일구분=="2","주말",ifelse(movie$일자=="2013-01-01","공휴일",
          ifelse(movie$일자=="2013-02-09","설",ifelse(movie$일자=="2013-03-01","공휴일",
          ifelse(movie$일자=="2013-05-17","공휴일",ifelse(movie$일자=="2013-06-06","공휴일",
          ifelse(movie$일자=="2013-08-15","공휴일",ifelse(movie$일자=="2013-09-18"|movie$일자=="2013-09-19"|movie$일자=="2013-09-20","추석",
          ifelse(movie$일자=="2013-10-03","공휴일",ifelse(movie$일자=="2013-10-09","공휴일",ifelse(movie$일자=="2013-12-25","공휴일",
          ifelse(movie$일자=="2014-01-01","공휴일",ifelse(movie$일자=="2014-01-30"|movie$일자=="2014-01-31","설",ifelse(movie$일자=="2014-05-05","어린이날",
          ifelse(movie$일자=="2014-05-06","공휴일",ifelse(movie$일자=="2014-06-06","공휴일",ifelse(movie$일자=="2014-08-15","공휴일",
          ifelse(movie$일자=="2014-09-08"|movie$일자=="2014-09-09"|movie$일자=="2014-09-10","추석",ifelse(movie$일자=="2014-10-03","공휴일",ifelse(movie$일자=="2014-10-09","공휴일",
          ifelse(movie$일자=="2014-12-25","공휴일",ifelse(movie$일자=="2015-01-01","공휴일",ifelse(movie$일자=="2015-02-18"|movie$일자=="2015-02-19"|movie$일자=="2015-02-20","설",
          ifelse(movie$일자=="2015-05-05","공휴일",ifelse(movie$일자=="2015-08-14","공휴일",ifelse(movie$일자=="2015-09-28"|movie$일자=="2015-09-29","추석",
          ifelse(movie$일자=="2015-10-09","공휴일",ifelse(movie$일자=="2015-12-25","공휴일",ifelse(movie$일자=="2016-01-01","공휴일",ifelse(movie$일자=="2016-02-08"|movie$일자=="2016-02-09"|movie$일자=="2016-02-10","설",
          ifelse(movie$일자=="2016-03-01","공휴일",ifelse(movie$일자=="2016-04-13","공휴일",ifelse(movie$일자=="2016-05-05","공휴일",ifelse(movie$일자=="2016-05-06","공휴일",
          ifelse(movie$일자=="2016-06-06","공휴일",ifelse(movie$일자=="2016-08-15","공휴일",ifelse(movie$일자=="2016-09-14"|movie$일자=="2016-09-15"|movie$일자=="2016-09-16","추석",
          ifelse(movie$일자=="2016-10-03","공휴일",ifelse(movie$일자=="2017-01-27"|movie$일자=="2017-01-30","설",ifelse(movie$일자=="2017-03-01","공휴일",ifelse(movie$일자=="2017-05-03","공휴일",
          ifelse(movie$일자=="2017-05-05","공휴일",ifelse(movie$일자=="2017-05-09","공휴일",ifelse(movie$일자=="2017-06-06","공휴일",ifelse(movie$일자=="2017-08-15","공휴일",
          ifelse(movie$일자=="2017-10-03","공휴일",ifelse(movie$일자=="2017-10-03"|movie$일자=="2017-10-04"|movie$일자=="2017-10-05"|movie$일자=="2017-10-06","추석",ifelse(movie$일자=="2017-10-09","공휴일","평일"))))))))))))))))))))))))))))))))))))))))))))))))

## 배우파워 필드 추가(eda 후 엑셀 작업)

# 배우1--관객수
melt = melt(movie, "배우1", "관객수")
head(melt,10)
movie_actor_1 = cast(melt, 배우1 ~ variable, fun=sum)
movie_actor_1 = movie_actor_1[c(order(-movie_actor_1$관객수)), ]
head(movie_actor_1)
movie_actor_1_20 = movie_actor_1[1:20,] #상위 20개 추출
ggplot(movie_actor_1_20, aes(배우1, 관객수))+geom_bar(stat="identity")

# 배우2--관객수
melt = melt(movie, "배우2", "관객수")
head(melt,10)
movie_actor_2 = cast(melt, 배우2 ~ variable, fun=sum)
movie_actor_2 = movie_actor_2[c(order(-movie_actor_2$관객수)), ]
head(movie_actor_2)
movie_actor_2_20 = movie_actor_2[1:20,] #상위 20개 추출
ggplot(movie_actor_2_20, aes(배우2, 관객수))+geom_bar(stat="identity")

# 배우3--관객수
melt = melt(movie, "배우3", "관객수")
head(melt,10)
movie_actor_3 = cast(melt, 배우3 ~ variable, fun=sum)
movie_actor_3 = movie_actor_3[c(order(-movie_actor_3$관객수)), ]
head(movie_actor_3)
movie_actor_3_20 = movie_actor_3[1:20,] #상위 20개 추출
ggplot(movie_actor_3_20, aes(배우3, 관객수))+geom_bar(stat="identity")

colnames(movie_actor_1)=c("배우","관객수")
colnames(movie_actor_2)=c("배우","관객수")
colnames(movie_actor_3)=c("배우","관객수")

#동명이인 확인
length(unique(movie_actor_1$배우))==length(movie_actor_1$배우) #TRUE
length(unique(movie_actor_2$배우))==length(movie_actor_2$배우) #TRUE
length(unique(movie_actor_3$배우))==length(movie_actor_3$배우) #TRUE

movie_actor_all = rbind(movie_actor_1, movie_actor_2, movie_actor_3)
colnames(movie_actor_all)=c("배우","관객수")
head(movie_actor_all)

# movie_actor_power
movie_actor_power<-ddply(movie_actor_all, '배우', summarise, 관객수=sum(관객수))
movie_actor_power = movie_actor_power[c(order(-movie_actor_power$관객수)), ]
head(movie_actor_power)
movie_actor_power=movie_actor_power[-1,] # 결측치 제거
head(movie_actor_power)
movie_actor_power_20 = movie_actor_power[1:20,] #상위 20개 추출
ggplot(movie_actor_power_20, aes(배우, 관객수))+geom_bar(stat="identity")
# write.csv(movie_actor_power, file="배우별누적관객.csv")
# 나머지 엑셀로 처리
# #N/A=0처리




## 일차(개봉후 경과일수) 필드 추가  (일자-개봉일)

for (i in 1:length(movie[,5])){
  movie[i,37] <- as.integer(movie[i,2] - movie[i,5])
  if (i%%100==0) print(i)
}
names(movie)[37]=c("일차")


## 일차 제곱 필드 추가

movie[38]=movie$일차^2
names(movie)[38]=c("일차2")

## 일차 세제곱 필드 추가

movie[39]=movie$일차^3
names(movie)[39]=c("일차3")



## 감독평균최대관객수(평균최대관객수로 감독 클러스터링) 

mean.dir <- aggregate(누적관객수~영화명+감독, data=movie, max)
director <- mean.dir$감독
mean.dir <- cbind(mean.dir, director)
mean.dir <- melt(mean.dir, measure.vars='director', variable.name='mean.dir', value.name='director')
director.mean <- ddply(mean.dir, .(director), summarize, ave=mean(누적관객수))   #감독별 누적관객수 평균
director.mean <- na.omit(director.mean)
names(director.mean)[1:2]=c('감독','감독평균최대관객수')   
movie <- join(movie, director.mean, by='감독')  

## 장르평균최대관객수(평균최대관객수로 장르 클러스터링)

mean.genre <- aggregate(누적관객수~영화명+장르, data=movie, max)
genre <- mean.genre$장르
mean.genre <- cbind(mean.genre, genre)
mean.genre <- melt(mean.genre, measure.vars='장르', variable.name='temp', value.name='genre')
genre.mean <- ddply(mean.genre,.(genre),summarize,ave=mean(누적관객수))
names(genre.mean)[1:2] <- c('장르','장르평균최대관객수')
movie <- join(movie, genre.mean, by='장르')


## 배급사평균최대관객수(평균최대관객수로 배급사 클러스터링)
mean.mar <- aggregate(누적관객수~영화명+배급사, data=movie, max)
mar.mean <- ddply(mean.mar,.(배급사),summarize,ave=mean(누적관객수)) #배급사별 누적관객수 평균
names(mar.mean)[1:2]=c('배급사','배급사평균최대관객수')   
movie <- join(movie, mar.mean,by="배급사") 

## 등급평균최대관객수(평균최대관객수로 등급 클러스터링) , 등급.group 필드 추가

mean.grade <- aggregate(누적관객수~영화명+등급, data=movie, max)
grade <- mean.grade$등급
mean.grade <- cbind(mean.grade, grade)
mean.grade <- melt(mean.grade, measure.vars='grade', variable.name='mean.grade', value.name='grade')
grade.mean <- ddply(mean.grade, .(grade), summarize, ave=mean(누적관객수))   #등급별 누적관객수 평균
grade.mean <- na.omit(grade.mean)
names(grade.mean)[1:2]=c('등급','등급평균최대관객수')   
movie <- join(movie, grade.mean, by='등급')   

## 공휴일평균관객수 필드 추가
melt = melt(movie, "일자구분", "관객수")
head(melt,10)
movie_day_category = dcast(melt, 일자구분 ~ variable, fun=mean)
movie_day_category = movie_day_category[c(order(-movie_day_category$관객수)), ]
movie_day_category
names(movie_day_category)=c('일자구분','공휴일평균관객수')
mean(movie_day_category[1:3,2])
movie_day_category[1:3,2] <- mean(movie_day_category[1:3,2])
head(movie_day_category)
movie=merge(movie,movie_day_category,by='일자구분')

## 공휴일총합 필드 추가

movie.List <- unique(movie$영화코드)
movie.List[order(movie.List)]
movie.TotalSeen <- c()
for (i in 1:length(movie.List)){
  movie.TotalSeen[i]=sum(movie[movie$영화코드==movie.List[i],"공휴일평균관객수"])
  if (i%%100==0) print(i)
}

movie.seen <- data.frame(as.matrix(movie.List),movie.TotalSeen)
movie.seen

names(movie.seen)=c('영화코드','공휴일총합')

movie <- join(movie, movie.seen, by='영화코드')

## 공휴일누적평균관객수 필드 추가(엑셀작업)

## 평점계 필드 추가

movie$평점계 <- movie$평점 * movie$코멘트수

## X0일차,X1일차 필드 추가(엑셀작업)


###############################################################################################
############################################ EDA ##############################################
###############################################################################################

## 상영 차수별 관객수
movie$일차 = factor(movie$일차)
melt = melt(movie, "일차", "관객수")
head(melt,10)
movie_days = cast(melt, 일차 ~ variable, fun=mean)
movie_days
ggplot(movie_days, aes(일차, 관객수))+geom_bar(stat="identity",fill="blue")+geom_vline(xintercept=15.5,col="red",lwd=1)

## 상영 차수(60일까지)
movie_days_60 = movie_days[1:60,]
ggplot(movie_days_60, aes(일차, 관객수))+geom_bar(stat="identity")

## 상영 차수(90일까지)
movie_days_90 = movie_days[1:90,]
ggplot(movie_days_90, aes(일차, 관객수))+geom_bar(stat="identity")

## 일자구분별 관객수
melt = melt(movie, "일자구분", "관객수")
head(melt,10)
movie_day_category = cast(melt, 일자구분 ~ variable, fun=mean)
movie_day_category = movie_day_category[c(order(-movie_day_category$관객수)), ]
movie_day_category
ggplot(movie_day_category, aes(일자구분, 관객수))+geom_bar(stat="identity",fill=c("blue","blue","red","blue","green"))



## 요일별 일일 평균 관객수 비율
install.packages("plotrix")
library(plotrix)
me<-melt(movie, "요일","관객수")
head(me)
me1<-dcast(me, 요일~variable, fun=mean)
head(me1)
str(me1)


daymean<-me1$관객수
lbls<-me1$요일
lbls
dayrate<-round(daymean/sum(daymean)*100, 2)
dayrate
lbls<-paste(lbls, dayrate)
lbls
lbls<-paste(lbls, "%", sep="")
lbls


piechart<-pie3D(dayrate, labels=lbls, labelcex=1.2, radius=1.2, explode=0.1, theta=0.7, main="요일별 일일 관객수 평균")



