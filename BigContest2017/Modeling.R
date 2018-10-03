setwd("C:/Users/samsung/Desktop/빅콘")


## 배열 만들기 (2^14) (이후 최종 엑셀처리) # 14개의 변수로 만들 수 있는 모든 경우의 수 Array 생성
array <- c()
Array <- c()
cnt.ar <- 0
for (a in 0:1){ for (b in 0:1){ for (c in 0:1){ for (d in 0:1){ for (e in 0:1){ 
  for (f in 0:1){ for (g in 0:1){ for(h in 0:1){ for(i in 0:1){ for(j in 0:1){
    for (k in 0:1){for (l in 0:1){for (m in 0:1){for (n in 0:1){
      array <- c(a,b,c,d,e,f,g,h,i,j,k,l,m,n)
      Array <- rbind(Array, array)
      cnt.ar <- cnt.ar+1
      rownames(Array)[cnt.ar] <- paste('array',cnt.ar)
      if(cnt.ar%%100==0) print(cnt.ar)
    } } } } } } } } } } } } } } 
head(Array)
tail(Array)

Array = as.data.frame(Array)

length.ar <- length(Array[,1])
colnames(Array)=c("배우파워", "평점", "코멘트수", "일차", "일차2", "일차3", "감독평균최대관객수", "장르평균최대관객수", "배급사평균최대관객수","등급평균최대관객수","공휴일평균관객수" ,"공휴일누적평균관객수", "공휴일총합", "기사수")
form <- matrix(0,length.ar,14)
colnames(form)=c("배우파워", "평점", "코멘트수", "일차", "일차2", "일차3", "감독평균최대관객수", "장르평균최대관객수", "배급사평균최대관객수","등급평균최대관객수","공휴일평균관객수" ,"공휴일누적평균관객수", "공휴일총합", "기사수")



for(z in 1:length.ar){
  if(Array[z,1]==1) form[z,1] = colnames(Array[1])
  if(Array[z,2]==1) form[z,2] = colnames(Array[2])
  if(Array[z,3]==1) form[z,3] = colnames(Array[3])
  if(Array[z,4]==1) form[z,4] = colnames(Array[4])
  if(Array[z,5]==1) form[z,5] = colnames(Array[5])
  if(Array[z,6]==1) form[z,6] = colnames(Array[6])
  if(Array[z,7]==1) form[z,7] = colnames(Array[7])
  if(Array[z,8]==1) form[z,8] = colnames(Array[8])
  if(Array[z,9]==1) form[z,9] = colnames(Array[9])
  if(Array[z,10]==1) form[z,10] = colnames(Array[10])
  if(Array[z,11]==1) form[z,11] = colnames(Array[11])
  if(Array[z,12]==1) form[z,12] = colnames(Array[12])
  if(Array[z,13]==1) form[z,13] = colnames(Array[13])
  if(Array[z,14]==1) form[z,14] = colnames(Array[14])
}        
form2=matrix(0,length.ar,1)

for(z in 1:length.ar){
  if(form[z,1]!=0) form2[z] = paste(form[z,1],sep=",") 
  if(form[z,2]!=0) form2[z] = paste(form2[z],form[z,2],sep=",")
  if(form[z,3]!=0) form2[z] = paste(form2[z],form[z,3],sep=",")
  if(form[z,4]!=0) form2[z] = paste(form2[z],form[z,4],sep=",")
  if(form[z,5]!=0) form2[z] = paste(form2[z],form[z,5],sep=",")
  if(form[z,6]!=0) form2[z] = paste(form2[z],form[z,6],sep=",")
  if(form[z,7]!=0) form2[z] = paste(form2[z],form[z,7],sep=",")
  if(form[z,8]!=0) form2[z] = paste(form2[z],form[z,8],sep=",")
  if(form[z,9]!=0) form2[z] = paste(form2[z],form[z,9],sep=",")
  if(form[z,10]!=0) form2[z] = paste(form2[z],form[z,10],sep=",")
  if(form[z,11]!=0) form2[z] = paste(form2[z],form[z,11],sep=",")
  if(form[z,12]!=0) form2[z] = paste(form2[z],form[z,12],sep=",")
  if(form[z,13]!=0) form2[z] = paste(form2[z],form[z,13],sep=",")
  if(form[z,14]!=0) form2[z] = paste(form2[z],form[z,14],sep=",")
  
}


head(form2)
write.csv(form2,file="Array0924.csv")

##7일차#################

library(glmnet)
rm(list=ls())
movie=read.csv("movie0924.csv") #새로운데이터
movie=subset(movie, movie$일차<=7) #새로운데이터 
Array <-read.csv("Array0924.csv")


target_list=list("몬스터 대학교", "섀도우 헌터스: 뼈의 도시", "슈퍼배드 2","킹스맨 : 시크릿 에이전트",
                 "퍼시 잭슨과 괴물의 바다", "컨저링", "남자가 사랑할 때", "수상한 그녀", 
                 "넛잡: 땅콩 도둑들", "조선미녀삼총사", "폴리스 스토리 2014", "피끓는 청춘", 
                 "미라클 벨리에", "아메리칸 울트라", "치외법권", "앤트맨", "오피스", 
                 "조선명탐정 : 사라진 놉의 딸", "도라에몽 : 스탠 바이 미", 
                 "명탐정 코난 : 코난 실종사건 - 사상 최악의 이틀", "오즈의 마법사: 돌아온 도로시", 
                 "이미테이션 게임", "뮨: 달의 요정", "서부전선", "에베레스트", "인턴", "탐정 : 더 비기닝", 
                 "쿵푸팬더3", "검사외전", "앨빈과 슈퍼밴드: 악동 어드벤처", "최강전사 미니특공대: 영웅의 탄생", 
                 "캐롤", "장난감이 살아있다", "거울나라의 앨리스", "고산자, 대동여지도", "달빛궁궐", 
                 "로빈슨 크루소", "드림 쏭", "매그니피센트 7", "벤허", "카페 소사이어티", "공조", "더 킹", 
                 "터닝메카드W: 블랙미러의 부활", "레지던트 이블: 파멸의 날", "짱구는 못말려 극장판: 폭풍수면! 꿈꾸는 세계 대돌격","밀정","관상"
)

a=data.frame()
b=seq(0,1,0.1)
len.ar =length(Array$변수) ;len.ar
#rm(list=ls())변수1당 5분 /12당 1시간/ 24당 2시간
#system.time(
for (k in 1:106){
  for (i in target_list){
    for (j in b){
      movie=read.csv("movie0924.csv")
      movie=subset(movie, movie$일차<=7)
      Array=read.csv("Array0924.csv")
      
      #log(data set)
      #관객수로 만든 변수 log / 수치가 큰 데이터(코멘트 수, 평점계) 변수 log
      movie$감독평균최대관객수 = log(movie$감독평균최대관객수)
      movie$장르평균최대관객수 = log(movie$장르평균최대관객수)
      movie$배우파워 = sqrt(movie$배우파워)
      movie$배급사평균최대관객수 = log(movie$배급사평균최대관객수)
      movie$등급평균최대관객수 = log(movie$등급평균최대관객수)
      movie$공휴일평균관객수 = log(movie$공휴일평균관객수)
      movie$공휴일누적평균관객수 = sqrt(movie$공휴일누적평균관객수)
      movie$공휴일총합 = log(movie$공휴일총합)
      
      
      #scale Data
      movie[ ,c("배우파워", "평점", "코멘트수", "일차", "일차2", "일차3", "감독평균최대관객수", "장르평균최대관객수", "배급사평균최대관객수", "등급평균최대관객수", "공휴일평균관객수", "공휴일누적평균관객수", "공휴일총합", "평점계", "기사수")] = scale(movie[ ,c("배우파워", "평점", "코멘트수", "일차", "일차2", "일차3", "감독평균최대관객수", "장르평균최대관객수", "배급사평균최대관객수", "등급평균최대관객수", "공휴일평균관객수", "공휴일누적평균관객수", "공휴일총합", "평점계", "기사수")])
      # +등급별 데이터 추가 
      #movie=scale(movie)
      
      un=unlist(strsplit(as.character(Array[k,2]),fixed=TRUE,split=","))
      x_train<-movie[,c('영화명',un)]
      y_train = movie[,c("영화명","일차","누적관객수")]
      
      
      x_test = x_train[x_train$영화명==i,]
      y_test = y_train[y_train$영화명==i,]
      x_train = x_train[x_train$영화명!=i,]
      y_train = y_train[y_train$영화명!=i,]
      x_test = x_test[,-1]
      y_test = y_test[,3]
      x_train = x_train[,-1]
      y_train = y_train[,3]
      y_test = log(y_test)
      y_train = log(y_train)
      

      #elastic-cv
      set.seed(1)
      cv.elastic = cv.glmnet(data.matrix(x_train), y_train, type.measure="mse", alpha =j, family="gaussian")
      bestlam = cv.elastic$lambda.min
      elastic.fit = glmnet(data.matrix(x_train), y_train, alpha =j, lambda=bestlam, family="gaussian")
      result_elastic=predict(elastic.fit, data.matrix(x_test))
      
      #exp
      result_elastic = round(exp(result_elastic),2)
      y_test = exp(y_test)
      
      rmse = sqrt(mean((result_elastic - y_test)^2))
      error = round(abs(result_elastic - y_test),2)
      accuracy = round((result_elastic/y_test)*100, 2)
      comparison3 = cbind(result_elastic, y_test,error,rmse, accuracy)
      colnames(comparison3) = c("predict","real","error","rmse","accuracy")
      last_result=cbind(k, j, i,tail(comparison3,1))
      a=rbind(a,last_result)
    }
  }
}
#)
write.csv(a,file="7일차.csv")

##14일차#################

setwd("C:/Users/samsung/Desktop/빅콘")
library(glmnet)
rm(list=ls())
movie=read.csv("movie0924.csv")  #새로운데이터
Array <-read.csv("Array0924.csv") #새로운데이터 

target_list=list("몬스터 대학교", "섀도우 헌터스: 뼈의 도시", "슈퍼배드 2","킹스맨 : 시크릿 에이전트",
                 "퍼시 잭슨과 괴물의 바다", "컨저링", "남자가 사랑할 때", "수상한 그녀", 
                 "넛잡: 땅콩 도둑들", "조선미녀삼총사", "폴리스 스토리 2014", "피끓는 청춘", 
                 "미라클 벨리에", "아메리칸 울트라", "치외법권", "앤트맨", "오피스", 
                 "조선명탐정 : 사라진 놉의 딸", "도라에몽 : 스탠 바이 미", 
                 "명탐정 코난 : 코난 실종사건 - 사상 최악의 이틀", "오즈의 마법사: 돌아온 도로시", 
                 "이미테이션 게임", "뮨: 달의 요정", "서부전선", "에베레스트", "인턴", "탐정 : 더 비기닝", 
                 "쿵푸팬더3", "검사외전", "앨빈과 슈퍼밴드: 악동 어드벤처", "최강전사 미니특공대: 영웅의 탄생", 
                 "캐롤", "장난감이 살아있다", "거울나라의 앨리스", "고산자, 대동여지도", "달빛궁궐", 
                 "로빈슨 크루소", "드림 쏭", "매그니피센트 7", "벤허", "카페 소사이어티", "공조", "더 킹", 
                 "터닝메카드W: 블랙미러의 부활", "레지던트 이블: 파멸의 날", "짱구는 못말려 극장판: 폭풍수면! 꿈꾸는 세계 대돌격","밀정","관상"
)

a=data.frame()
b=seq(0,1,0.1)
len.ar =length(Array$변수) ;len.ar
#rm(list=ls())변수1당 5분 /12당 1시간/ 24당 2시간
#system.time(
for (k in 1:106){
  for (i in target_list){
    for (j in b){
      movie=read.csv("movie0924.csv")
      Array=read.csv("Array0924.csv")
      
      #log(data set)
      movie$감독평균최대관객수 = log(movie$감독평균최대관객수)
      movie$장르평균최대관객수 = log(movie$장르평균최대관객수)
      movie$배우파워 = sqrt(movie$배우파워)
      movie$배급사평균최대관객수 = log(movie$배급사평균최대관객수)
      movie$등급평균최대관객수 = log(movie$등급평균최대관객수)
      movie$공휴일평균관객수 = log(movie$공휴일평균관객수)
      movie$공휴일누적평균관객수 = sqrt(movie$공휴일누적평균관객수)
      movie$공휴일총합 = log(movie$공휴일총합)
      
      
      #scale Data
      movie[ ,c("배우파워", "평점", "코멘트수", "일차", "일차2", "일차3", "감독평균최대관객수", "장르평균최대관객수", "배급사평균최대관객수", "등급평균최대관객수", "공휴일평균관객수", "공휴일누적평균관객수", "공휴일총합", "평점계", "기사수")] = scale(movie[ ,c("배우파워", "평점", "코멘트수", "일차", "일차2", "일차3", "감독평균최대관객수", "장르평균최대관객수", "배급사평균최대관객수", "등급평균최대관객수", "공휴일평균관객수", "공휴일누적평균관객수", "공휴일총합", "평점계", "기사수")])
      #movie=scale(movie)
      
      un=unlist(strsplit(as.character(Array[k,2]),fixed=TRUE,split=","))
      x_train<-movie[,c('영화명',un)]
      y_train = movie[,c("영화명","일차","누적관객수")]
      
      
      x_test = x_train[x_train$영화명==i,]
      y_test = y_train[y_train$영화명==i,]
      x_train = x_train[x_train$영화명!=i,]
      y_train = y_train[y_train$영화명!=i,]
      x_test = x_test[,-1]
      y_test = y_test[,3]
      x_train = x_train[,-1]
      y_train = y_train[,3]
      y_test = log(y_test)
      y_train = log(y_train)

      #elastic-cv
      set.seed(1)
      cv.elastic = cv.glmnet(data.matrix(x_train), y_train, type.measure="mse", alpha =j, family="gaussian")
      bestlam = cv.elastic$lambda.min
      elastic.fit = glmnet(data.matrix(x_train), y_train, alpha =j, lambda=bestlam, family="gaussian")
      result_elastic=predict(elastic.fit, data.matrix(x_test))
      
      #exp
      result_elastic = round(exp(result_elastic),2)
      y_test = exp(y_test)
      
      rmse = sqrt(mean((result_elastic - y_test)^2))
      error = round(abs(result_elastic - y_test),2)
      accuracy = round((result_elastic/y_test)*100, 2)
      comparison3 = cbind(result_elastic, y_test,error,rmse, accuracy)
      colnames(comparison3) = c("predict","real","error","rmse","accuracy")
      last_result=cbind(k, j, i,tail(comparison3,1))
      a=rbind(a,last_result)
    }
  }
}
#)
write.csv(a,file="14일차.csv")



###############################################################################################
####################################### Modeling ##############################################
###############################################################################################
# load packages
install.packages("glmnet")
library("glmnet")

## data load
setwd("C:/Users/samsung/Desktop/빅콘")
# movie=read.csv("킹스맨2_testset.csv") #킹스맨2
# moive=read.csv("남한산성_testset.csv") #남한산성
# movie=read.csv("넛잡2_testset.csv") #넛잡2
# movie=subset(movie, movie$일차<=7) #남한산성, 넛잡2 예측시
Array=read.csv("Array0924.csv")
head(movie)
str(movie)

#
a=data.frame()

# 8일차 남한산성 #사용 변수(log(누적관객수) ~ 일차 + 일차2 + 일차3 + log(감독평균최대관객수) + log(배급사평균최대관객수) + log(등급평균최대관객수) + log(공휴일평균관객수) + sqrt(공휴일누적평균관객수) + log(공휴일총합) + 평점 + 코멘트수 + 기사수)
k=7
i="남한산성"
j=0.9

# 8일차 넛잡2 #사용 변수(log(누적관객수) ~ 일차 + 일차2 + 일차3 + log(감독평균최대관객수) + log(배급사평균최대관객수) + log(등급평균최대관객수) + log(공휴일평균관객수) + sqrt(공휴일누적평균관객수) + log(공휴일총합) + 평점 + 코멘트수 + 기사수)
k=7    #변수
i="넛잡2"
j=0.9   #알파값


# 14일차 킹스맨2 #사용 변수(log(누적관객수) ~ 일차 + sqrt(배우파워) + log(감독평균최대관객수) + log(장르평균최대관객수) + log(배급사평균최대관객수) + log(등급평균최대관객수) + log(공휴일평균관객수) + sqrt(공휴일누적평균관객수) + log(공휴일총합) + 평점 + 코멘트수 + 기사수)
k=48    #변수
i="킹스맨2"
j=0.1   #알파값


#정규화(data set)
movie$감독평균최대관객수 = log(movie$감독평균최대관객수)
movie$장르평균최대관객수 = log(movie$장르평균최대관객수)
movie$배우파워 = sqrt(movie$배우파워)
movie$배급사평균최대관객수 = log(movie$배급사평균최대관객수)
movie$등급평균최대관객수 = log(movie$등급평균최대관객수)
movie$공휴일평균관객수 = log(movie$공휴일평균관객수)
movie$공휴일누적평균관객수 = sqrt(movie$공휴일누적평균관객수)
movie$공휴일총합 = log(movie$공휴일총합)


#movie=scale(movie)
movie[ ,c("배우파워", "평점", "코멘트수", "일차", "일차2", "일차3", "감독평균최대관객수", "장르평균최대관객수", "배급사평균최대관객수","등급평균최대관객수","공휴일평균관객수" ,"공휴일누적평균관객수", "공휴일총합", "기사수")] = scale(movie[ ,c("배우파워", "평점", "코멘트수", "일차", "일차2", "일차3", "감독평균최대관객수", "장르평균최대관객수", "배급사평균최대관객수","등급평균최대관객수","공휴일평균관객수" ,"공휴일누적평균관객수", "공휴일총합", "기사수")])

#variable selection
var=c("배우파워", "평점", "코멘트수", "일차", "일차2", "일차3", "감독평균최대관객수", "장르평균최대관객수", "배급사평균최대관객수","등급평균최대관객수","공휴일평균관객수" ,"공휴일누적평균관객수", "공휴일총합", "기사수")


un=unlist(strsplit(as.character(Array[k,2]),fixed=TRUE,split=","))
x_train<-movie[,c('영화명',un)]
y_train = movie[,c("영화명","일차","누적관객수")]

tail(x_train)
x_test = x_train[x_train$영화명==i,]
y_test = y_train[y_train$영화명==i,]
x_train = x_train[x_train$영화명!=i,]
y_train = y_train[y_train$영화명!=i,]
x_test = x_test[,-1]
y_test = y_test[,3]
x_train = x_train[,-1]
y_train = y_train[,3]
y_test = log(y_test)
y_train = log(y_train)


#elastic-cv
set.seed(1)
cv.elastic = cv.glmnet(data.matrix(x_train), y_train, type.measure="mse", alpha =j, family="gaussian")
bestlam = cv.elastic$lambda.min
elastic.fit = glmnet(data.matrix(x_train), y_train, alpha =j, lambda=bestlam, family="gaussian")
result_elastic = predict(elastic.fit, data.matrix(x_test))

#exp
result_elastic = round(exp(result_elastic),2)
result_elastic



# write.csv(result_elastic, file="Final_result_킹스맨2.csv")
# write.csv(result_elastic, file="Final_result_남한산성.csv")
# write.csv(result_elastic, file="Final_result_넛잡2.csv")