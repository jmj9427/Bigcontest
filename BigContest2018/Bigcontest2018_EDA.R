setwd("D:/MJ/bigcontest2018/champion_data/final")

if(!require(data.table)) install.packages("data.table"); library(data.table)
if(!require(caret)) install.packages("caret"); library(caret)
if(!require(geosphere)) install.packages("geosphere"); library(geosphere)
if(!require(psych)) install.packages("psych"); library(psych)
if(!require(mice)) install.packages("mice"); library(mice)
if(!require(VIM)) install.packages("VIM"); library(VIM)
if(!require(e1071)) install.packages("e1071"); library(e1071)
if(!require(ROCR)) install.packages("ROCR"); library(ROCR)
if(!require(readxl)) install.packages("readxl"); library(readxl)
if(!require(data.table)) install.packages("data.table"); library(data.table)
if(!require(randomForest)) install.packages("randomForest"); library(randomForest)
if(!require(dplyr)) install.packages("dplyr"); library(dplyr)
if(!require(reshape2)) install.packages("reshape2"); library(reshape2)

activity <- fread("train_activity.csv") ; activity <- as.data.frame(activity)
guild <- fread("train_guild.csv") ; guild <- as.data.frame(guild)
label <- fread("train_label.csv") ; label <- as.data.frame(label)
party <- fread("train_party.csv") ; party <- as.data.frame(party)
pay <- fread("train_payment.csv") ; pay <- as.data.frame(pay)
trade <- fread("train_trade.csv") ; trade <- as.data.frame(trade)
#### EDA _ all csv

# base <- fread(dataname.csv) ; 
base$label<-as.factor(base$label)
base<-data.frame(base)

for(i in 2:length(colnames(base))){
  imagename = paste0(colnames(base)[i],"_","label")
  path_1 = paste0("D:/MJ/bigcontest2018/champion_data/final/plot/",imagename,".","png")
  png(filename = path_1, width = 800, height=600)
  plot(base$label, base[,i], xlab="label", ylab=colnames(base)[i])
  dev.off()
}


###################################### pre-processing & make derived variable from EDA


#########################activity variable

##user 8week variable -> variable sum & mean
######train 

activity_unst <- activity

for(i in 12:36) {
  a <- sort(unique(activity_unst[,colnames(activity_unst)[i]]))
  sig <- 1 / (a[2] - a[1])
  mu <- 1 / (1 - a[2]/a[1])
  
  activity_unst[,i] <- round(activity_unst[,i] * sig + mu)
  
}

for(i in c(5, 9, 10, 11)) {
  a <- sort(unique(activity_unst[,colnames(activity_unst)[i]]))
  sig <- 1 / (a[2] - a[1])
  mu <- 1 / (1 - a[2]/a[1])
  
  activity_unst[,i] <- round(activity_unst[,i] * sig + mu)
  
}


for(i in 3:38) {
  activity1 <- dcast(activity_unst, acc_id ~ wk, value.var = colnames(activity_unst)[i], sum )
  colnames(activity1)[2:length(colnames(activity1))] <- paste0(colnames(activity_unst)[i], "_",1:8)
  for(j in 2:9) {
    if(i == 3) next
    activity1[which(activity1[,j] == 0),j] = min(activity_unst[,i])
  }
  
  activity1[,10] <- rowSums(activity1[,-1])
  activity1[,11] <- rowMeans(activity1[,-1])
  colnames(activity1)[c(10, 11)] <- paste0(colnames(activity)[3], "_", c("sum","mean"))
  train <- left_join(train, activity1, by = c("acc_id"))
  print(i)
}



###### cnt_dt_ratio
train$cnt_ratio = train$cnt_dt_sum/56
head(train$cnt_ratio)

########### ratio of week variable 

for(i in 12:36) {
  a <- sort(unique(activity_unst[,colnames(activity_unst)[i]]))
  sig <- 1 / (a[2] - a[1])
  mu <- 1 / (1 - a[2]/a[1])
  
  activity_unst[,i] <- round(activity_unst[,i] * sig + mu)
  
}

activity_dcast <- data.frame("acc_id" = train$acc_id)



for(i in 12:36) {
  activity1 <- dcast(activity_unst, acc_id ~ wk, value.var = colnames(activity)[i], sum)
  colnames(activity1)[2:length(colnames(activity1))] <- paste0(colnames(activity)[i], "_",1:8)
  
  activity1[,10] <- rowSums(activity1[,-1])
  
  colnames(activity1)[10] <- paste0(colnames(activity)[i], "_sum")
  activity_dcast <- left_join(activity_dcast, activity1, by = c("acc_id"))
  print(i)
}

ratio <- data.frame("acc_id" = train$acc_id)

for(i in 2:10) {
  a <- activity_dcast[,i+9] / activity_dcast[,i]
  ratio <- cbind(ratio, a)
  colnames(ratio)[i] <- paste0("duel_ratio_",i-1)
}

for(i in 20:28) {
  a <- activity_dcast[,i+9] / activity_dcast[,i]
  ratio <- cbind(ratio, a)
  colnames(ratio)[i-9] <- paste0("partybattle_ratio_",i-19)
}

for(i in 38:46) {
  a <- activity_dcast[,i+63] / activity_dcast[,i]
  ratio <- cbind(ratio, a)
  colnames(ratio)[i-18] <- paste0("inzone_solo_ratio_",i-37)
}

for(i in 47:55) {
  a <- activity_dcast[,i+63] / activity_dcast[,i]
  ratio <- cbind(ratio, a)
  colnames(ratio)[i-18] <- paste0("inzone_light_ratio_",i-46)
}

for(i in 56:64) {
  a <- activity_dcast[,i+63] / activity_dcast[,i]
  ratio <- cbind(ratio, a)
  colnames(ratio)[i-18] <- paste0("inzone_skilled_ratio_",i-55)
}

for(i in 65:73) {
  a <- activity_dcast[,i+63] / activity_dcast[,i]
  ratio <- cbind(ratio, a)
  colnames(ratio)[i-18] <- paste0("inzone_normal_ratio_",i-64)
}

for(i in 74:82) {
  a <- activity_dcast[,i+63] / activity_dcast[,i]
  ratio <- cbind(ratio, a)
  colnames(ratio)[i-18] <- paste0("raid_ratio_",i-73)
}

for(i in 83:91) {
  a <- activity_dcast[,i+63] / activity_dcast[,i]
  ratio <- cbind(ratio, a)
  colnames(ratio)[i-18] <- paste0("raid_light_ratio_",i-82)
}

for(i in 92:100) {
  a <- activity_dcast[,i+63] / activity_dcast[,i]
  ratio <- cbind(ratio, a)
  colnames(ratio)[i-18] <- paste0("bam_ratio_",i-91)
}

colnames(ratio)[seq(10, 82, 9)] <- c("duel_ratio_sum", "partybattle_ratio_sum", "inzone_solo_ratio_sum", "inzone_light_ratio_sum",
                                     "inzone_skilled_ratio_sum", "inzone_normal_ratio_sum", "raid_ratio_sum", "raid_light_ratio_sum",
                                     "bam_ratio_sum")


for(i in 2:82) {
  ratio[which(is.nan(ratio[,i])),i] <- 0
  
}

ratio$acc_id <- as.character(ratio$acc_id)

train <- left_join(train, ratio, by = c('acc_id'))



########################################### payment table
## pay_amount
head(pay)
pay_amount <- aggregate(pay[,3],by=list(acc_id=pay$acc_id),sum)
colnames(pay_amount) <- c("acc_id","pay_amount")
head(pay_amount)
train <- merge(train,pay_amount,by="acc_id")

## pay_wk_x, -0.1498985 = 0
b <- dcast(pay, acc_id ~ payment_week, value.var='payment_amount')
colnames(b) <- c("acc_id",paste0('pay_',1:8))
pay_wk_x <- b
head(pay_wk_x,100)
train <- merge(train,pay_wk_x,by="acc_id")


## pay_wk_factor
for (i in 1:nrow(b)) {
  for (j in 1:8) {
    if (b[i,j+1] > -0.1498984) {
      b[i,j+1] = 1
    }else {
      b[i,j+1] = 0
    }
  }
  print(i)
}
head(b,100) # 완성 
pay_wk_factor <- b
colnames(pay_wk_factor) <- c("acc_id","pay_fac_1","pay_fac_2","pay_fac_3","pay_fac_4","pay_fac_5","pay_fac_6",
                 "pay_fac_7","pay_fac_8")

## total_pay_count, 1.19917 = 0 
b.1 <- b[,-1]
head(b.1)
c.1 = apply(b.1,1,sum)
c <- data.frame(acc_id=b$acc_id,total_pay_count=c.1)
head(c,100)
total_pay_count <- c
pay_pil <- merge(pay_pil,total_pay_count,by="acc_id")

pay_wk_factor$pay_fac_1 <- as.factor(pay_wk_factor$pay_fac_1)
pay_wk_factor$pay_fac_2 <- as.factor(pay_wk_factor$pay_fac_2)
pay_wk_factor$pay_fac_3 <- as.factor(pay_wk_factor$pay_fac_3)
pay_wk_factor$pay_fac_4 <- as.factor(pay_wk_factor$pay_fac_4)
pay_wk_factor$pay_fac_5 <- as.factor(pay_wk_factor$pay_fac_5)
pay_wk_factor$pay_fac_6 <- as.factor(pay_wk_factor$pay_fac_6)
pay_wk_factor$pay_fac_7 <- as.factor(pay_wk_factor$pay_fac_7)
pay_wk_factor$pay_fac_8 <- as.factor(pay_wk_factor$pay_fac_8)

train <- merge(train,pay_wk_factor,by="acc_id")
train <- merge(train,total_pay_count,by="acc_id")

## many_pay
train$many_pay = ifelse(train$total_pay_count >= 4,1,0)

# write.csv(train,"pil.csv",row.names = F)

#### trade
trade_weapon <- trade[which(trade$item_type == "weapon"),]

a <- sort(unique(trade_weapon$item_amount))
sig <- 1/(a[2] - a[1])
mu <- (2 - a[2]/a[1]) / (1 - a[2]/a[1])

trade_un <- trade$item_amount * sig + mu

trade$item_amount_un <- trade_un


trade_money <- trade[which(trade$item_type == "money"),]
trade_item <- trade[-which(trade$item_type == 'money'),]
trade_weapon <- trade[which(trade$item_type == "weapon"),]
trade_accessory <- trade[which(trade$item_type == "accessory"),]
trade_grocery <- trade[which(trade$item_type == "grocery"),]
trade_gem <- trade[which(trade$item_type == "gem"),]

trade_costume <- trade[which(trade$item_type == "costume"),]

##### trade of 8 weeks 
source_length <- trade %>% group_by(source_acc_id) %>% summarise('source_length' = length(item_amount))
target_length <- trade %>% group_by(target_acc_id) %>% summarise('target_length' = length(item_amount))

label3 <- left_join(train, source_length, by = c('acc_id' = 'source_acc_id'))
label3 <- left_join(label3, target_length, by = c('acc_id' = 'target_acc_id'))

label3[is.na(label3$source_length),]$source_length <- 0
label3[is.na(label3$target_length),]$target_length <- 0


#### total trade amount of 8 weeks

source_amount <- trade %>% group_by(source_acc_id) %>% summarise('source_amount' = sum(item_amount))
target_amount <- trade %>% group_by(target_acc_id) %>% summarise('target_amount' = sum(item_amount))

label3 <- left_join(label3, source_amount, by = c('acc_id' = 'source_acc_id'))
label3 <- left_join(label3, target_amount, by = c('acc_id' = 'target_acc_id'))

label3[is.na(label3$source_amount),]$source_amount <- 0
label3[is.na(label3$target_amount),]$target_amount <- 0

label3$trade_amount <- label3$source_amount + label3$target_amount

label3$source_amount <- (label3$source_amount - mu) / sig
label3$target_amount <- (label3$target_amount - mu) / sig
label3$trade_amount <- (label3$trade_amount - mu) / sig

###### last trade week
source_last <- trade %>% group_by(source_acc_id) %>% summarise('last_trade_week_source' = max(trade_week))
target_last <- trade %>% group_by(target_acc_id) %>% summarise('last_trade_week_target' = max(trade_week))

label3 <- left_join(label3, source_last, by = c('acc_id' = 'source_acc_id'))
label3 <- left_join(label3, target_last, by = c('acc_id' = 'target_acc_id'))

label3[is.na(label3$last_trade_week_source),]$last_trade_week_source <- 0
label3[is.na(label3$last_trade_week_target),]$last_trade_week_target <- 0


####### trade of money 
source_money_length <- trade_money %>% group_by(source_acc_id) %>% summarise('source_money_length' = length(item_amount))
target_money_length <- trade_money %>% group_by(target_acc_id) %>% summarise('target_money_length' = length(item_amount))

label3 <- left_join(label3, source_money_length, by = c('acc_id' = 'source_acc_id'))
label3 <- left_join(label3, target_money_length, by = c('acc_id' = 'target_acc_id'))

label3[is.na(label3$source_money_length),]$source_money_length <- 0
label3[is.na(label3$target_money_length),]$target_money_length <- 0

label3$money_length <- label3$source_money_length + label3$target_money_length


###### total trade amount of money
source_money_amount <- trade_money %>% group_by(source_acc_id) %>% summarise('source_money_amount' = sum(item_amount_un))
target_money_amount <- trade_money %>% group_by(target_acc_id) %>% summarise('target_money_amount' = sum(item_amount_un))

label3 <- left_join(label3, source_money_amount, by = c('acc_id' = 'source_acc_id'))
label3 <- left_join(label3, target_money_amount, by = c('acc_id' = 'target_acc_id'))

label3[is.na(label3$source_money_amount),]$source_money_amount <- 0
label3[is.na(label3$target_money_amount),]$target_money_amount <- 0

label3$money_amount <- label3$source_money_amount + label3$target_money_amount

label3$source_money_amount <- (label3$source_money_amount - mu) / sig
label3$target_money_amount <- (label3$target_money_amount - mu) / sig
label3$money_amount <- (label3$money_amount - mu) / sig
#### trade of item
source_item_length <- trade_item %>% group_by(source_acc_id) %>% summarise('source_item_length' = length(item_amount))
target_item_length <- trade_item %>% group_by(target_acc_id) %>% summarise('target_item_length' = length(item_amount))

label3 <- left_join(label3, source_item_length, by = c('acc_id' = 'source_acc_id'))
label3 <- left_join(label3, target_item_length, by = c('acc_id' = 'target_acc_id'))

label3[is.na(label3$source_item_length),]$source_item_length <- 0
label3[is.na(label3$target_item_length),]$target_item_length <- 0

label3$item_length <- label3$source_item_length + label3$target_item_length


#### total trade amount of item
source_item_amount <- trade_item %>% group_by(source_acc_id) %>% summarise('source_item_amount' = sum(item_amount_un))
target_item_amount <- trade_item %>% group_by(target_acc_id) %>% summarise('target_item_amount' = sum(item_amount_un))

label3 <- left_join(label3, source_item_amount, by = c('acc_id' = 'source_acc_id'))
label3 <- left_join(label3, target_item_amount, by = c('acc_id' = 'target_acc_id'))

label3[is.na(label3$source_item_amount),]$source_item_amount <- 0
label3[is.na(label3$target_item_amount),]$target_item_amount <- 0

label3$item_amount <- label3$source_item_amount + label3$target_item_amount

label3$source_item_amount <- (label3$source_item_amount - mu) / sig
label3$target_item_amount <- (label3$target_item_amount - mu) / sig
label3$item_amount <- (label3$item_amount - mu) / sig

##### trade of grocery
source_grocery_length <- trade_grocery %>% group_by(source_acc_id) %>% summarise('source_grocery_length' = length(item_amount))
target_grocery_length <- trade_grocery %>% group_by(target_acc_id) %>% summarise('target_grocery_length' = length(item_amount))

label3 <- left_join(label3, source_grocery_length, by = c('acc_id' = 'source_acc_id'))
label3 <- left_join(label3, target_grocery_length, by = c('acc_id' = 'target_acc_id'))

label3[is.na(label3$source_grocery_length),]$source_grocery_length <- 0
label3[is.na(label3$target_grocery_length),]$target_grocery_length <- 0

label3$grocery_length <- label3$source_grocery_length + label3$target_grocery_length


#### total trade amount of grocery
source_grocery_amount <- trade_grocery %>% group_by(source_acc_id) %>% summarise('source_grocery_amount' = sum(item_amount_un))
target_grocery_amount <- trade_grocery %>% group_by(target_acc_id) %>% summarise('target_grocery_amount' = sum(item_amount_un))

label3 <- left_join(label3, source_grocery_amount, by = c('acc_id' = 'source_acc_id'))
label3 <- left_join(label3, target_grocery_amount, by = c('acc_id' = 'target_acc_id'))

label3[is.na(label3$source_grocery_amount),]$source_grocery_amount <- 0
label3[is.na(label3$target_grocery_amount),]$target_grocery_amount <- 0

label3$grocery_amount <- label3$source_grocery_amount + label3$target_grocery_amount

label3$source_grocery_amount <- (label3$source_grocery_amount - mu) / sig
label3$target_grocery_amount <- (label3$target_grocery_amount - mu) / sig
label3$grocery_amount <- (label3$grocery_amount - mu) / sig

##### trade of weapon
source_weapon_length <- trade_weapon %>% group_by(source_acc_id) %>% summarise('source_weapon_length' = length(item_amount))
target_weapon_length <- trade_weapon %>% group_by(target_acc_id) %>% summarise('target_weapon_length' = length(item_amount))

label3 <- left_join(label3, source_weapon_length, by = c('acc_id' = 'source_acc_id'))
label3 <- left_join(label3, target_weapon_length, by = c('acc_id' = 'target_acc_id'))

label3[is.na(label3$source_weapon_length),]$source_weapon_length <- 0
label3[is.na(label3$target_weapon_length),]$target_weapon_length <- 0

label3$weapon_length <- label3$source_weapon_length + label3$target_weapon_length

##### total trade amount of weapon
source_weapon_amount <- trade_weapon %>% group_by(source_acc_id) %>% summarise('source_weapon_amount' = sum(item_amount_un))
target_weapon_amount <- trade_weapon %>% group_by(target_acc_id) %>% summarise('target_weapon_amount' = sum(item_amount_un))

label3 <- left_join(label3, source_weapon_amount, by = c('acc_id' = 'source_acc_id'))
label3 <- left_join(label3, target_weapon_amount, by = c('acc_id' = 'target_acc_id'))

label3[is.na(label3$source_weapon_amount),]$source_weapon_amount <- 0
label3[is.na(label3$target_weapon_amount),]$target_weapon_amount <- 0

label3$weapon_amount <- label3$source_weapon_amount + label3$target_weapon_amount

label3$source_weapon_amount <- (label3$source_weapon_amount - mu) / sig
label3$target_weapon_amount <- (label3$target_weapon_amount - mu) / sig
label3$weapon_amount <- (label3$weapon_amount - mu) / sig

##### trade of costume
source_costume_length <- trade_costume %>% group_by(source_acc_id) %>% summarise('source_costume_length' = length(item_amount))
target_costume_length <- trade_costume %>% group_by(target_acc_id) %>% summarise('target_costume_length' = length(item_amount))

label3 <- left_join(label3, source_costume_length, by = c('acc_id' = 'source_acc_id'))
label3 <- left_join(label3, target_costume_length, by = c('acc_id' = 'target_acc_id'))

label3[is.na(label3$source_costume_length),]$source_costume_length <- 0
label3[is.na(label3$target_costume_length),]$target_costume_length <- 0

label3$costume_length <- label3$source_costume_length + label3$target_costume_length

##### total trade amount of costume
source_costume_amount <- trade_costume %>% group_by(source_acc_id) %>% summarise('source_costume_amount' = sum(item_amount_un))
target_costume_amount <- trade_costume %>% group_by(target_acc_id) %>% summarise('target_costume_amount' = sum(item_amount_un))

label3 <- left_join(label3, source_costume_amount, by = c('acc_id' = 'source_acc_id'))
label3 <- left_join(label3, target_costume_amount, by = c('acc_id' = 'target_acc_id'))

label3[is.na(label3$source_costume_amount),]$source_costume_amount <- 0
label3[is.na(label3$target_costume_amount),]$target_costume_amount <- 0

label3$costume_amount <- label3$source_costume_amount + label3$target_costume_amount

label3$source_costume_amount <- (label3$source_costume_amount - mu) / sig
label3$target_costume_amount <- (label3$target_costume_amount - mu) / sig
label3$costume_amount <- (label3$costume_amount - mu) / sig

###### trade of gem
source_gem_length <- trade_gem %>% group_by(source_acc_id) %>% summarise('source_gem_length' = length(item_amount))
target_gem_length <- trade_gem %>% group_by(target_acc_id) %>% summarise('target_gem_length' = length(item_amount))

label3 <- left_join(label3, source_gem_length, by = c('acc_id' = 'source_acc_id'))
label3 <- left_join(label3, target_gem_length, by = c('acc_id' = 'target_acc_id'))

label3[is.na(label3$source_gem_length),]$source_gem_length <- 0
label3[is.na(label3$target_gem_length),]$target_gem_length <- 0

label3$gem_length <- label3$source_gem_length + label3$target_gem_length

###### total trade amount of gem
source_gem_amount <- trade_gem %>% group_by(source_acc_id) %>% summarise('source_gem_amount' = sum(item_amount_un))
target_gem_amount <- trade_gem %>% group_by(target_acc_id) %>% summarise('target_gem_amount' = sum(item_amount_un))

label3 <- left_join(label3, source_gem_amount, by = c('acc_id' = 'source_acc_id'))
label3 <- left_join(label3, target_gem_amount, by = c('acc_id' = 'target_acc_id'))

label3[is.na(label3$source_gem_amount),]$source_gem_amount <- 0
label3[is.na(label3$target_gem_amount),]$target_gem_amount <- 0

label3$gem_amount <- label3$source_gem_amount + label3$target_gem_amount

label3$source_gem_amount <- (label3$source_gem_amount - mu) / sig
label3$target_gem_amount <- (label3$target_gem_amount - mu) / sig
label3$gem_amount <- (label3$gem_amount - mu) / sig

##### trade of accessory
source_accessory_length <- trade_accessory %>% group_by(source_acc_id) %>% summarise('source_accessory_length' = length(item_amount))
target_accessory_length <- trade_accessory %>% group_by(target_acc_id) %>% summarise('target_accessory_length' = length(item_amount))

label3 <- left_join(label3, source_accessory_length, by = c('acc_id' = 'source_acc_id'))
label3 <- left_join(label3, target_accessory_length, by = c('acc_id' = 'target_acc_id'))

label3[is.na(label3$source_accessory_length),]$source_accessory_length <- 0
label3[is.na(label3$target_accessory_length),]$target_accessory_length <- 0

label3$accessory_length <- label3$source_accessory_length + label3$target_accessory_length


###### total trade amount of accessory
source_accessory_amount <- trade_accessory %>% group_by(source_acc_id) %>% summarise('source_accessory_amount' = sum(item_amount_un))
target_accessory_amount <- trade_accessory %>% group_by(target_acc_id) %>% summarise('target_accessory_amount' = sum(item_amount_un))

label3 <- left_join(label3, source_accessory_amount, by = c('acc_id' = 'source_acc_id'))
label3 <- left_join(label3, target_accessory_amount, by = c('acc_id' = 'target_acc_id'))

label3[is.na(label3$source_accessory_amount),]$source_accessory_amount <- 0
label3[is.na(label3$target_accessory_amount),]$target_accessory_amount <- 0


label3$accessory_amount <- label3$source_accessory_amount + label3$target_accessory_amount

label3$source_accessory_amount <- (label3$source_accessory_amount - mu) / sig
label3$target_accessory_amount <- (label3$target_accessory_amount - mu) / sig
label3$accessory_amount <- (label3$accessory_amount - mu) / sig
###### transaction count
source_id_length <- trade %>% group_by(source_acc_id) %>% summarise('source_id_length' = length(unique(target_acc_id)))
target_id_length <- trade %>% group_by(target_acc_id) %>% summarise('target_id_length' = length(unique(source_acc_id)))

label3 <- left_join(label3, source_id_length, by = c('acc_id' = 'source_acc_id'))
label3 <- left_join(label3, target_id_length, by = c('acc_id' = 'target_acc_id'))

label3[is.na(label3$source_id_length),]$source_id_length <- 0
label3[is.na(label3$target_id_length),]$target_id_length <- 0



### guild
## guild_join
idx <- vector()
num = 100000
acc_id <- train$acc_id
guild.split <- strsplit(guild$guild_member_acc_id,",")
guild_id <- unlist(guild.split)
head(guild_id)
for (i in 1:num) {
  if (acc_id[i] %in% guild_id) {
    idx[i] <- i
  }
  print(i)
}

idx <- na.omit(unique(idx))
head(idx)
label3 <- label[idx,]
label4 <- label[-idx,]
table(label3$label)

train$guild_join <- 0
train[idx,]$guild_join <- 1
train$guild_join <- as.factor(train$guild_join)

## guild_num
guild.split <- strsplit(guild$guild_member_acc_id,",")
head(guild.split)

guild2 <- unlist(guild.split)
guild2 <- as.data.frame(guild2)
head(guild2)
guild_num <- as.vector(table(guild2)[table(guild2)>1])
acc_id <- names(table(guild2)[table(guild2)>1])
guild3 <- data.frame(acc_id,guild_num)
head(guild3)
guild3$acc_id <- as.character(guild3$acc_id)

train <- merge(train,guild3,by="acc_id",all.x = T)
sum(is.na(train))
train[,c("guild_num")][is.na(train[,c("guild_num")])] <- 0


# guild_max <- max number of guild
people = vector()
train$guild_max = 0
train <- as.data.frame(train)
train.name <- train$acc_id
train.name <- as.character(train.name)
train.name <- as.vector(train.name)
guild_max <- as.vector(train$guild_max)

for (i in 1:length(train.name)) {
  for (j in 1:9963) {
    if (train.name[i] %in% unlist(guild.split[j])) {
      people[j] = length(unlist(guild.split[j]))
      guild_max[i] = max(people,na.rm=T)
    } 
    people = vector()
  }
  print(i)
}
head(guild_max,100)

train$guild_max = guild_max

################################################### party
tparty <- fread("train_party.csv")
tact <- fread("train_activity.csv")

# 
tparty$day = tparty$party_end_day - tparty$party_start_day
tparty$day = ifelse(tparty$day < 0,tparty$day + 7,tparty$day)

# 
tparty = tparty[tparty$day == 0,]

tparty_start_time = substr(tparty$party_start_time,1,8)
tparty_end_time = substr(tparty$party_end_time,1,8)

tnewtime = c(strptime(tparty_start_time, format = "%H:%M:%S"))
tnewendtime = c(strptime(tparty_end_time, format = "%H:%M:%S"))

#### start time
tah = as.numeric(format(tnewtime, "%H"))
tam = as.numeric(format(tnewtime, "%M"))
tas = as.numeric(format(tnewtime, "%S"))

#### end time
tbh = as.numeric(format(tnewendtime, "%H"))
tbm = as.numeric(format(tnewendtime, "%M"))
tbs = as.numeric(format(tnewnedtime, "%S"))

## maintain time of party 
tday = c(tparty$day)
thour = ifelse(tbh - tah < 0, tbh - tah + 24, tbh - tah)
tminute = ifelse(tbm - tam < 0, tbm - tam + 60, tbm - tam)
tsecond = ifelse(tbs - tas < 0, tbs - tas + 60, tbs - tas)

summary(tday)
summary(thour)
summary(tminute)
summary(tsecond)

length(tday)
length(thour)
length(tminute)
length(tsecond)

tplaysecond = (tday * 24 * 60 * 60) + (thour * 60 * 60) + (tminute * 60) + tsecond
length(tplaysecond)
head(tplaysecond)


thash = tparty$hashed
tid = strsplit(thash,",")

taccid <- unique(tact$acc_id)
head(taccid)


tpsw = c(as.character(tparty$party_start_week))
tpsd = c(as.character(tparty$party_start_day))
tpswd = paste0(tpsw,tpsd)
tpswd = as.numeric(tpswd)



for(i in 1:8){
  for(j in 1:7){
    assign(paste0('tpt',i,j),rep(0,length(taccid)))
  }
}


lentid = length(tid)

# week, day, party time by id
for(i in 1:lentid){
  if(tpswd[i] == 11){tpt11[which(taccid %in% tid[[i]])] = tpt11[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 12){tpt12[which(taccid %in% tid[[i]])] = tpt12[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 13){tpt13[which(taccid %in% tid[[i]])] = tpt13[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 14){tpt14[which(taccid %in% tid[[i]])] = tpt14[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 15){tpt15[which(taccid %in% tid[[i]])] = tpt15[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 16){tpt16[which(taccid %in% tid[[i]])] = tpt16[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 17){tpt17[which(taccid %in% tid[[i]])] = tpt17[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  
  else if(tpswd[i] == 21){tpt21[which(taccid %in% tid[[i]])] = tpt21[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 22){tpt22[which(taccid %in% tid[[i]])] = tpt22[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 23){tpt23[which(taccid %in% tid[[i]])] = tpt23[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 24){tpt24[which(taccid %in% tid[[i]])] = tpt24[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 25){tpt25[which(taccid %in% tid[[i]])] = tpt25[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 26){tpt26[which(taccid %in% tid[[i]])] = tpt26[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 27){tpt27[which(taccid %in% tid[[i]])] = tpt27[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  
  else if(tpswd[i] == 31){tpt31[which(taccid %in% tid[[i]])] = tpt31[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 32){tpt32[which(taccid %in% tid[[i]])] = tpt32[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 33){tpt33[which(taccid %in% tid[[i]])] = tpt33[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 34){tpt34[which(taccid %in% tid[[i]])] = tpt34[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 35){tpt35[which(taccid %in% tid[[i]])] = tpt35[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 36){tpt36[which(taccid %in% tid[[i]])] = tpt36[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 37){tpt37[which(taccid %in% tid[[i]])] = tpt37[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  
  else if(tpswd[i] == 41){tpt41[which(taccid %in% tid[[i]])] = tpt41[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 42){tpt42[which(taccid %in% tid[[i]])] = tpt42[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 43){tpt43[which(taccid %in% tid[[i]])] = tpt43[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 44){tpt44[which(taccid %in% tid[[i]])] = tpt44[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 45){tpt45[which(taccid %in% tid[[i]])] = tpt45[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 46){tpt46[which(taccid %in% tid[[i]])] = tpt46[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 47){tpt47[which(taccid %in% tid[[i]])] = tpt47[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  
  else if(tpswd[i] == 51){tpt51[which(taccid %in% tid[[i]])] = tpt51[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 52){tpt52[which(taccid %in% tid[[i]])] = tpt52[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 53){tpt53[which(taccid %in% tid[[i]])] = tpt53[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 54){tpt54[which(taccid %in% tid[[i]])] = tpt54[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 55){tpt55[which(taccid %in% tid[[i]])] = tpt55[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 56){tpt56[which(taccid %in% tid[[i]])] = tpt56[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 57){tpt57[which(taccid %in% tid[[i]])] = tpt57[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  
  else if(tpswd[i] == 61){tpt61[which(taccid %in% tid[[i]])] = tpt61[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 62){tpt62[which(taccid %in% tid[[i]])] = tpt62[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 63){tpt63[which(taccid %in% tid[[i]])] = tpt63[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 64){tpt64[which(taccid %in% tid[[i]])] = tpt64[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 65){tpt65[which(taccid %in% tid[[i]])] = tpt65[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 66){tpt66[which(taccid %in% tid[[i]])] = tpt66[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 67){tpt67[which(taccid %in% tid[[i]])] = tpt67[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  
  else if(tpswd[i] == 71){tpt71[which(taccid %in% tid[[i]])] = tpt71[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 72){tpt72[which(taccid %in% tid[[i]])] = tpt72[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 73){tpt73[which(taccid %in% tid[[i]])] = tpt73[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 74){tpt74[which(taccid %in% tid[[i]])] = tpt74[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 75){tpt75[which(taccid %in% tid[[i]])] = tpt75[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 76){tpt76[which(taccid %in% tid[[i]])] = tpt76[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 77){tpt77[which(taccid %in% tid[[i]])] = tpt77[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  
  else if(tpswd[i] == 81){tpt81[which(taccid %in% tid[[i]])] = tpt81[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 82){tpt82[which(taccid %in% tid[[i]])] = tpt82[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 83){tpt83[which(taccid %in% tid[[i]])] = tpt83[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 84){tpt84[which(taccid %in% tid[[i]])] = tpt84[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 85){tpt85[which(taccid %in% tid[[i]])] = tpt85[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 86){tpt86[which(taccid %in% tid[[i]])] = tpt86[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  else if(tpswd[i] == 87){tpt87[which(taccid %in% tid[[i]])] = tpt87[which(taccid %in% tid[[i]])] + tplaysecond[i]}
  
  print(i)
}


for(i in 1:8){
  for(j in 1:7){
    filename = paste0('tpt',i,j,'.csv')
    write.csv(get(paste0('tpt',i,j)),filename)
  }
}



tpartyid = as.data.frame(taccid)

train<-left_join(train,tpartyid,by=c("acc_id"="acc_id"))



### remove Inf
train_ratio_p = vector()
name.ratio = colnames(train)[grepl("ratio",colnames(train))]
train_ratio = train[name.ratio]
for (i in 1:ncol(train_ratio)) {
  train_ratio_p <- train_ratio[,i]
  train_ratio_p[which(is.infinite(train_ratio_p))] = 0
  train_ratio[,i] = train_ratio_p
  print(i)
} 
head(train_ratio)

train[name.ratio] = train_ratio


write.csv(train,"dong1.csv")
