library(dplyr)
library(ggplot2)
library(viridis)
library(hrbrthemes)

#country share hashrate
cshare <-  read.csv("export2.csv", header = TRUE)
str(cshare)
cshare <- cshare %>% 
  mutate(dateNew = as.Date(date,format="%Y/%m/%d")) %>% 
  group_by(date) %>% 
  mutate(country = forcats::fct_reorder(country,monthly_hashrate_.,.desc = TRUE))
cshare$monthly_hashrate_. <- as.numeric(sub("%", "",cshare$monthly_hashrate_.,fixed=TRUE))/100



ggplot(cshare, aes(x=dateNew, y=monthly_hashrate_., fill=country)) + 
  geom_area(alpha=0.6 , size=1, colour="black") + 
  geom_vline(xintercept = as.Date("2021/06/01") ,color="black", linetype = "dashed",size = 1)+
  labs(title = "Udzial panstw w hash-rate BTC",  x = "Data", y = "Udzial w globalnym hash-rate (srednia miesieczna)")

