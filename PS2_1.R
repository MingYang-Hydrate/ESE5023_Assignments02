#1
library(tidyr)
library(dplyr)
library(ggplot2)
#1.1
data1 <-read.delim(file="signif.txt",header=T)
Sig_Eqs <- as_tibble(data1)
#1.2
TOT<-Sig_Eqs%>% 
  select(COUNTRY,DEATHS) %>% 
  group_by(COUNTRY) %>% 
  summarize(TOTALDEATH=sum(DEATHS,na.rm=T)) 
  TOP<-arrange(TOT,desc(TOTALDEATH))
print(TOP[1:10,])
#1.3
EQ<-Sig_Eqs%>% 
  select(YEAR,EQ_PRIMARY) %>%
  filter(EQ_PRIMARY >6) %>%
  group_by(YEAR) %>% 
  summarize(totalnumber_of_EQ=length(EQ_PRIMARY))
## using n() is also reconmended ##@MingYANG 
ggplot(EQ,aes(x=YEAR, y=totalnumber_of_EQ)) + 
  geom_line()
#观察到全球地震发生数在1700年左右急剧上升
#可能是人们对于地震的了解增多，开始有意识的记录数据
#和现代社会接近记录更容易保存下来
#1.4  跟姜浩同学讨论，他向我提供了创建函数的关键思路
CountEq_LargestEq<-function(country){
  Number_Eq<-Sig_Eqs%>% 
    filter(COUNTRY==country)%>% 
    nrow()
  Max_Eq<-Sig_Eqs%>%
    filter(COUNTRY==country)%>% 
    filter(EQ_PRIMARY==max(EQ_PRIMARY,na.rm=T))%>% 
    #paste函数是姜浩同学提供给我的关键思路
    mutate(date=paste(YEAR,MONTH,DAY,sep="-"))%>% 
  pull(date)
    list<-list(Number_Eq,Max_Eq)
  return(list)
}
country<-unique(Sig_Eqs$COUNTRY)
EQ_number<-c()
EQ_country<-c()
EQmax_date<-c()
for(i in country){
  a<-as.numeric(CountEq_LargestEq(i)[1])
  b<-i
  c<-as.character(CountEq_LargestEq(i)[2])
  EQ_number <-c(EQ_number,a)
  EQ_country<-c(EQ_country,b)
  EQmax_date<-c(EQmax_date,c)
}
df1<-data.frame(EQ_country,EQ_number ,EQmax_date)
tbl_new<-as_tibble(df1)
tbl_new %>% 
  arrange(desc(EQ_number))

# good work



