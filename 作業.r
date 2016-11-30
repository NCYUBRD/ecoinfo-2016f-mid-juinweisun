setwd('C:\\Users\\ralan ni\\Desktop\\ecoinformatics-master(資訊生態學上課教材)\\氣象資料\\raw')

library(data.table)

dt <- data.table::fread('201507_auto_hr.txt',skip=75,header = FALSE)

#讀取多個檔案
df <- list.files('.')
#迴圈提取
dx <- list()
for(i in 1:115){
  print(i)
  dx[[i]] <- data.table::fread(df[i],skip=75,header=FALSE)
  subset(dx[[i]],stno=='C0M530')
  write.csv(subset(dx[[i]],stno=='C0M530'),file= paste(i,"C0M530.txt", sep=''))
}
#迴圈合併
C0M530 <- data.table()
for(i in 1:length(df)){
  x <- fread(df[i],skip = 1,header = FALSE)
  if(i==1){
    C0M530 <- x
  }else{
    C0M530 <- rbind(C0M530,x)
  }
}
# 列名稱設定
setnames(C0M530,1:10,c('','stno','yyyymmddhh','PS01','TX01','RH01','WD01','WD02','PP01','SS01'))
#存檔
write.csv(C0M530,file="C0M530.txt")

#讀取C0M530
C0M530<-data.table::fread('C0M530.txt',header = TRUE)

#將-9997,-9998,-9999轉成NA
C0M530[C0M530==-9997]<-NA
C0M530[C0M530==-9998]<-NA
C0M530[C0M530==-9999]<-NA

# 將 yyyymmddhh 轉成 POSIXct 時間戳記格式
POSIXctsd<-as.POSIXct(strptime(as.character(C0M530$yyyymmddhh),'%Y %m %d %H'))
#新增為一個欄(variable)，命名為 timestamp
C0M530[, timestamp:=(POSIXctsd)]
#將時間合併成年月日
formatdays<-format(POSIXctsd,format = '%Y %m %d')
#新增欄位days
C0M530[, days:=(formatdays)]
#將時間合併成年月
formatmonth<-format(POSIXctsd,format = '%Y %m')
#新增欄位month
C0M530[, month:=(formatmonth)]
#計算月平均氣溫
aggregate(C0M530$TX01,by=list(C0M530$month),FUN=mean,na.rm=TRUE)
#計算日平均氣溫
aggregate(C0M530$TX01,by=list(C0M530$days),FUN=mean,na.rm=TRUE)
