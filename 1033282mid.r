#設定工作目錄
setwd('C:\\Users\\ralan ni\\Desktop\\ecoinformatics-master(資訊生態學上課教材)\\1033282mid')

library(data.table)

#第一大題
#隨機產生 10000 組正整數儲存成 vector 格式
random10k <- c(sample(1:100000,size = 10000))
#並輸出成 random10k.csv
write.csv(random10k,file='random10k.csv')

#第二大題
#使用 for 迴圈列出 15 個費布納西(Fibonacci)數列
Fibonacci <- function(i){
  x <- numeric(i)
  if(i==1){ 
    return(0)
  }
  else if(i==2){
    return(c(0,1))
  }
  else{
    x[1] <- 0
    x[2] <- 1
    for(i in 3:i){
      x[i] <- x[i-2] + x[i-1]
    }
    return(x)
  }
}
Fibonacci(15)

#第三大題
#讀取sample_data.txt
dtsd<-data.table::fread('sample_data.txt')
# 將 yyyymmddhh 轉成 POSIXct 時間戳記格式
POSIXctsd<-as.POSIXct(strptime(as.character(dtsd$yyyymmddhh),'%Y %m %d %H'))
#新增為一個欄(variable)，命名為 timestamp
dtsd[, timestamp:=(POSIXctsd)]
#輸出為 sample_data_parsed.csv 
write.csv(dtsd,file='sample_data_parsed.csv')
#將-9996,-9997,-9998,-9999轉換成NA,方便計算
dtsd[dtsd==-9996]<-NA
dtsd[dtsd==-9997]<-NA
dtsd[dtsd==-9998]<-NA
dtsd[dtsd==-9999]<-NA
#將時間合併成年月日
formatdays<-format(POSIXctsd,format = '%Y %m %d')
#新增欄位days
dtsd[, days:=(formatdays)]
#將時間合併成年月
formatmonth<-format(POSIXctsd,format = '%Y %m')
#新增欄位month
dtsd[, month:=(formatmonth)]
#計算月平均氣溫
aggregate(dtsd$TX01,by=list(dtsd$month),FUN=mean,na.rm=TRUE)
#計算月平均濕度
aggregate(dtsd$RH01,by=list(dtsd$month),FUN=mean,na.rm=TRUE)
#計算月累積降水
aggregate(dtsd$PP01,by=list(dtsd$month),FUN=sum,na.rm=TRUE)
#2014 年至 2015 年這個測站的每月平均氣溫、每月平均濕度、每月累積降水， 並用表格呈現
dtmontht<-data.table::fread('montht.txt',header = TRUE)
#依據dtmontht表格:2014最冷月為1月,2015最冷月為1月
#依據dtmontht表格:2014最熱月為7月,2015最熱月為6月
#201501每日最低溫，201407每日最高溫
dtmontht2<-data.table::fread('montht2.txt',header = TRUE)
#依據dtmontht表格:2014/9月最濕，其月平均溫度為28.75
#月溫差=每月最高溫日溫度-每月最低溫日溫度(2014.2015兩年平均)
MTD<-data.table(aggregate(dtsd$TX01,by=list(dtsd$days),FUN=mean,na.rm=TRUE))
setnames(MTD,1:2,c('日期','溫度'))
#月溫差最大者為2月，溫差=10.525
#年溫差平均為11.275
#溫量指數
sum(dtmontht[4,2]:dtmontht[4,25])

#第四大題
dtpenghu_env<-data.table::fread('penghu_env.csv',header = TRUE)
#計算各島環境因子(total_cover, C, EC, ..., etc.) 的平均、 第一四分位數、
#中位數、第三四分位數、最大值及最小值以及標準差
#利用aggregate
aggregate(dtpenghu_env$rock_ratio,by=list(dtpenghu_env$island),FUN=summary,na.rm=TRUE)
aggregate(dtpenghu_env$rock_ratio,by=list(dtpenghu_env$island),FUN=sd,na.rm=TRUE)
#並建立表格
dtcpenghu_env<-data.table::fread('cpenghu_env.txt',header = TRUE)
#C最高的樣區-dg07
#EC最高的樣區-sg01
#K最高的樣區-dg01
#Na最高的樣區-sg01
#N最高的樣區-sy04
