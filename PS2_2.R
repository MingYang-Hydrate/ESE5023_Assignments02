#2
library(tidyr)
library(dplyr)
library(ggplot2)

a_Data       <- read.csv(file = "2281305.csv", header = T)
a_Data <- as_tibble(a_Data)
a_Data%>% 
  select(WND,DATE) %>% 
  filter(substr(WND,1,3)!="999") %>% 
  filter(substr(WND,9,12)!="9999") %>% 
  filter(substr(WND,5,7)=="1,N")%>%
  filter(substr(WND,14,14)=="1")%>%
  #paste 函数是姜浩同学提供的思路
 mutate(Month=as.character(paste0(substr(DATE,1,4),"-",substr(DATE,6,7)))) %>%
  mutate(wind_speed=as.numeric(substr(WND,9,12)))%>%
  select(Month,wind_speed) %>% 
  group_by(Month) %>% 
  summarise(windspeed_month=mean(wind_speed,na.rm = T)) %>% 
  mutate(month = as.Date(paste0(Month,"-","15"))) %>%
  ggplot(aes(x=month, y=windspeed_month)) + 
  geom_line()
#通过观察可发现月平均风速呈现螺旋上升状态，在2020年风速尤其大

# It is required to explain how you filter the data in your report.
# And your explaination can be more explicit and detailed
