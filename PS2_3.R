#3
library(tidyr)
library(dplyr)
library(ggplot2)
b_Data       <- read.csv(file = "Flux-T_aYWW_20200925.csv", header = T)
b_Data <-as_tibble(b_Data) 
 b_Data%>%
  mutate(weight1=ifelse(weight>0,weight,NA))%>%
  mutate(Time_second=b_Data$锘縯ime)%>%
 select(weight1,Time_second)%>%
   ggplot(aes(x=Time_second,y=weight1))+
   geom_line()
# @MingYANG noticed:
# some thing wrong in "锘縯ime"
# the end
