#�]�w�u�@�ؿ�
setwd('C:\\Users\\ralan ni\\Desktop\\ecoinformatics-master(��T�ͺA�ǤW�ұЧ�)\\1033282mid')

library(data.table)

#�Ĥ@�j�D
#�H������ 10000 �ե�����x�s�� vector �榡
random10k <- c(sample(1:100000,size = 10000))
#�ÿ�X�� random10k.csv
write.csv(random10k,file='random10k.csv')

#�ĤG�j�D
#�ϥ� for �j��C�X 15 �ӶO���Ǧ�(Fibonacci)�ƦC
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

#�ĤT�j�D
#Ū��sample_data.txt
dtsd<-data.table::fread('sample_data.txt')
# �N yyyymmddhh �ন POSIXct �ɶ��W�O�榡
POSIXctsd<-as.POSIXct(strptime(as.character(dtsd$yyyymmddhh),'%Y %m %d %H'))
#�s�W���@����(variable)�A�R�W�� timestamp
dtsd[, timestamp:=(POSIXctsd)]
#��X�� sample_data_parsed.csv 
write.csv(dtsd,file='sample_data_parsed.csv')
#�N-9996,-9997,-9998,-9999�ഫ��NA,��K�p��
dtsd[dtsd==-9996]<-NA
dtsd[dtsd==-9997]<-NA
dtsd[dtsd==-9998]<-NA
dtsd[dtsd==-9999]<-NA
#�N�ɶ��X�֦��~���
formatdays<-format(POSIXctsd,format = '%Y %m %d')
#�s�W���days
dtsd[, days:=(formatdays)]
#�N�ɶ��X�֦��~��
formatmonth<-format(POSIXctsd,format = '%Y %m')
#�s�W���month
dtsd[, month:=(formatmonth)]
#�p��륭�����
aggregate(dtsd$TX01,by=list(dtsd$month),FUN=mean,na.rm=TRUE)
#�p��륭�����
aggregate(dtsd$RH01,by=list(dtsd$month),FUN=mean,na.rm=TRUE)
#�p���ֿn����
aggregate(dtsd$PP01,by=list(dtsd$month),FUN=sum,na.rm=TRUE)
#2014 �~�� 2015 �~�o�Ӵ������C�륭����šB�C�륭����סB�C��ֿn�����A �åΪ��e�{
dtmontht<-data.table::fread('montht.txt',header = TRUE)
#�̾�dtmontht���:2014�̧N�묰1��,2015�̧N�묰1��
#�̾�dtmontht���:2014�̼��묰7��,2015�̼��묰6��
#201501�C��̧C�šA201407�C��̰���
dtmontht2<-data.table::fread('montht2.txt',header = TRUE)
#�̾�dtmontht���:2014/9�����A��륭���ū׬�28.75
#��Ůt=�C��̰��Ť�ū�-�C��̧C�Ť�ū�(2014.2015��~����)
MTD<-data.table(aggregate(dtsd$TX01,by=list(dtsd$days),FUN=mean,na.rm=TRUE))
setnames(MTD,1:2,c('���','�ū�'))
#��Ůt�̤j�̬�2��A�Ůt=10.525
#�~�Ůt������11.275
#�Ŷq����
sum(dtmontht[4,2]:dtmontht[4,25])

#�ĥ|�j�D
dtpenghu_env<-data.table::fread('penghu_env.csv',header = TRUE)
#�p��U�q���Ҧ]�l(total_cover, C, EC, ..., etc.) �������B �Ĥ@�|����ơB
#����ơB�ĤT�|����ơB�̤j�Ȥγ̤p�ȥH�μзǮt
#�Q��aggregate
aggregate(dtpenghu_env$rock_ratio,by=list(dtpenghu_env$island),FUN=summary,na.rm=TRUE)
aggregate(dtpenghu_env$rock_ratio,by=list(dtpenghu_env$island),FUN=sd,na.rm=TRUE)
#�ëإߪ��
dtcpenghu_env<-data.table::fread('cpenghu_env.txt',header = TRUE)
#C�̰����˰�-dg07
#EC�̰����˰�-sg01
#K�̰����˰�-dg01
#Na�̰����˰�-sg01
#N�̰����˰�-sy04
