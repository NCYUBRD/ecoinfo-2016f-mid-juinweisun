setwd('C:\\Users\\ralan ni\\Desktop\\ecoinformatics-master(��T�ͺA�ǤW�ұЧ�)\\��H���\\raw')

library(data.table)

dt <- data.table::fread('201507_auto_hr.txt',skip=75,header = FALSE)

#Ū���h���ɮ�
df <- list.files('.')
#�j�鴣��
dx <- list()
for(i in 1:115){
  print(i)
  dx[[i]] <- data.table::fread(df[i],skip=75,header=FALSE)
  subset(dx[[i]],stno=='C0M530')
  write.csv(subset(dx[[i]],stno=='C0M530'),file= paste(i,"C0M530.txt", sep=''))
}
#�j��X��
C0M530 <- data.table()
for(i in 1:length(df)){
  x <- fread(df[i],skip = 1,header = FALSE)
  if(i==1){
    C0M530 <- x
  }else{
    C0M530 <- rbind(C0M530,x)
  }
}
# �C�W�ٳ]�w
setnames(C0M530,1:10,c('','stno','yyyymmddhh','PS01','TX01','RH01','WD01','WD02','PP01','SS01'))
#�s��
write.csv(C0M530,file="C0M530.txt")

#Ū��C0M530
C0M530<-data.table::fread('C0M530.txt',header = TRUE)

#�N-9997,-9998,-9999�নNA
C0M530[C0M530==-9997]<-NA
C0M530[C0M530==-9998]<-NA
C0M530[C0M530==-9999]<-NA

# �N yyyymmddhh �ন POSIXct �ɶ��W�O�榡
POSIXctsd<-as.POSIXct(strptime(as.character(C0M530$yyyymmddhh),'%Y %m %d %H'))
#�s�W���@����(variable)�A�R�W�� timestamp
C0M530[, timestamp:=(POSIXctsd)]
#�N�ɶ��X�֦��~���
formatdays<-format(POSIXctsd,format = '%Y %m %d')
#�s�W���days
C0M530[, days:=(formatdays)]
#�N�ɶ��X�֦��~��
formatmonth<-format(POSIXctsd,format = '%Y %m')
#�s�W���month
C0M530[, month:=(formatmonth)]
#�p��륭�����
aggregate(C0M530$TX01,by=list(C0M530$month),FUN=mean,na.rm=TRUE)
#�p��饭�����
aggregate(C0M530$TX01,by=list(C0M530$days),FUN=mean,na.rm=TRUE)
