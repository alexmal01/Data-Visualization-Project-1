library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(scales)


totalBitcoinConsumption <- read.csv("exportNew.csv")
totalBitcoinConsumption %>% 
  tail()

BP <- read.csv("bp-stats-review-2021-all-dataANSI.csv", header = TRUE)
BTCprice <- read.csv("BTCprice.csv")

str(BTCprice)


  

str(totalBitcoinConsumption)
View(totalBitcoinConsumption)

totalBitcoinConsumption <- 
  totalBitcoinConsumption %>% 
  mutate(DateFormattedNew = as.Date(DateFormated2, format = "%d/%m/%Y"))

BTCprice <- 
  BTCprice %>% 
  mutate(DateFormatted = as.Date(Date, format = "%Y-%m-%d"))


BTCprice <- 
  BTCprice %>% 
  mutate(meanPrice = (High+Low)/2)

str(BTCprice)


priceMeanLine <- 
  BTCprice %>% 
  select(DateFormatted, meanPrice) %>% 
  ggplot(aes(x = DateFormatted, y = meanPrice))+
  geom_line()

priceMeanLine

consumption1 <- 
  totalBitcoinConsumption %>%
  select(DateFormattedNew, Monthly.consumption) %>% 
  ggplot(aes(x = DateFormattedNew, y = Monthly.consumption)) +
  geom_col() + 
  xlim(as.Date("2013-01-01"), as.Date("2021-07-01")) + 
  labs(title = "Monthly consumption of energy by BTC mining", subtitle = "in TWh", x = "Year and Month", y = "Consumption in TWh")

consumption1


BTCprice <- 
  BTCprice %>% 
  select(-Date)

totalBitcoinConsumption<- 
  totalBitcoinConsumption %>% 
  select(-Date, -DateFormated2) %>% 
  rename(DateFormatted = DateFormattedNew)

totalBitcoinConsumption<- 
  totalBitcoinConsumption %>% 
  rename(DateFormatted = DateFormattedNew)

str(totalBitcoinConsumption)

BTC_consumption_price <- 
  inner_join(BTCprice, totalBitcoinConsumption)

BTC_consumption_price2 <- 
  BTC_consumption_price %>% 
  mutate(adjPrice = meanPrice/5000)



consAndPrice1 <- 
  ggplot(BTC_consumption_price2)+
  geom_col(aes(x = DateFormatted, y = Monthly.consumption, color = "Energy"), fill = "white", width = 10, size = 2.75)+
  geom_line(aes(x = DateFormatted, y = adjPrice, color = "Price/5000"), size = 2.75)+
  theme_bw()+
  scale_x_date(breaks = "1 months", labels=date_format("%Y-%m"), minor = "1 months", guide = guide_axis(n.dodge = 2))+
  theme(axis.text.x = element_text(angle = 90, size = 15, vjust = 0.5), axis.text.y = element_text(size = 15), title = element_text(size = 30))+
  labs(title = "Monthly consumption of energy by BTC mining", subtitle = "in relation to changes in BTC's price", x = "Year and Month", y = "Cumulative consumption [TWh]")+
  scale_color_manual(values = c("darkgreen", "red"))+
  theme(legend.title = element_text(color = "transparent"), legend.text = element_text(size = 25))

  

  
consAndPrice1

  




totalBitcoinConsumption %>%
  select(DateFormattedNew, Cumulative.consumption) %>% 
  ggplot(aes(x = DateFormattedNew, y = Cumulative.consumption)) +
  geom_col() + 
  xlim(as.Date("2013-01-01"), as.Date("2021-07-01")) + 
  labs(title = "Monthly consumption of energy by BTC mining", subtitle = "cumulative in TWh", x = "Year and Month", y = "Cumulative consumption in TWh")



BP <- 
  BP %>% 
  mutate(X1965 = as.numeric(X1965),
         X1966 = as.numeric(X1966),
         X1967 = as.numeric(X1967),
         X1968 = as.numeric(X1968),
         X1969 = as.numeric(X1969),
         X1970 = as.numeric(X1970),
         X1971 = as.numeric(X1971),
         X1972 = as.numeric(X1972),
         X1973 = as.numeric(X1973),
         X1974 = as.numeric(X1974),
         X1975 = as.numeric(X1975),
         X1976 = as.numeric(X1976),
         X1977 = as.numeric(X1977),
         X1978 = as.numeric(X1978),
         X1979 = as.numeric(X1979),
         X1980 = as.numeric(X1980),
         X1981 = as.numeric(X1981),
         X1982 = as.numeric(X1982),
         X1983 = as.numeric(X1983),
         X1984 = as.numeric(X1984),
         X1985 = as.numeric(X1985),
         X1986 = as.numeric(X1986),
         X1987 = as.numeric(X1987),
         X1988 = as.numeric(X1988),
         X1989 = as.numeric(X1989))

str(BP)


BP <- 
  BP %>% 
  pivot_longer(!Year, names_to = "consum", values_to = "energy") %>% 
  mutate(consum = str_sub(consum, 2,5)) %>% 
  mutate(consum = as.numeric(consum))
  
BP <- 
  BP %>% 
  rename(Country = Year, Year = consum)  
 
BP <- 
  BP %>%  mutate(EnergyTWh = energy*277.777778) %>% 
  mutate(AverageEnergyMonthlyTWh = EnergyTWh/12) %>% 
  rename(EnergyEJ = energy)

BP


kontynentyKraje = read.csv("krajeKontyn.csv")

BPkontinents = inner_join(BP, kontynentyKraje)
BPkontinents

BPkontinents %>% 
  filter(Year == 2020) %>% 
  ggplot(aes(x = EnergyEJ, y = Continent))+
  geom_violin() + 
  labs(title = "Energy consumption", subtitle = "by continents in 2020", x = "Energy in Exajoules")+
  xlim(0, 25)


