library(tidyverse)
library(lubridate)
washington_area <- c("Prince George's, Maryland, US", "Montgomery, Maryland, US", "Anne Arundel, Maryland, US", 
                     "Howard, Maryland, US", "Frederick, Maryland, US", "Charles, Maryland, US",
                     "Fairfax, Virginia, US", "Arlington, Virginia, US", "Prince William, Virginia, US", 
                     "Loudoun, Virginia, US", "Alexandria, Virginia, US", "Falls Church, Virginia, US", 
                     "Fairfax City, Virginia, US", "District of Columbia,District of Columbia,US")
cases <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv") %>%
  filter(Combined_Key %in% washington_area) %>% select(-(1:10)) %>% 
  gather(key="Date", value="Confirmed_Cases", -1) %>% mutate(Combined_Key=factor(Combined_Key), Date=mdy(Date)) %>%
  filter(Date>=as.Date("2020-03-17")) %>% group_by(Date) %>% summarise(Cases=sum(Confirmed_Cases))
deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv") %>%
  filter(Combined_Key %in% washington_area) %>% select(-(1:10)) %>% 
  gather(key="Date", value="Deaths", -1) %>% mutate(Combined_Key=factor(Combined_Key), Date=mdy(Date)) %>%
  filter(Date>=as.Date("2020-04-01")) %>% group_by(Date) %>% summarise(Deaths=sum(Deaths))



p1 <- cases %>% ggplot(aes(x=Date, y=Cases))+geom_line(lwd=1.5, col="darkblue")+
  scale_y_log10(labels=scales::comma)+theme_minimal()+scale_x_date(date_breaks = "1 week", date_labels = "%b %d")+
  geom_text(data =  cases[nrow(cases),], aes(x = Date, y = Cases, label = format(Cases, big.mark = ",")), vjust = 2.5)+
  labs(title = "COVID-19 Cases in Washington DC Area", x = element_blank(), y = element_blank())
p2 <- deaths %>% ggplot(aes(x=Date, y=Deaths))+geom_line(lwd=1.5, col="darkred")+
  scale_y_log10(labels=scales::comma)+theme_minimal()+scale_x_date(date_breaks = "1 week", date_labels = "%b %d")+
  geom_text(data =  deaths[nrow(deaths),], aes(x = Date, y = Deaths, label = format(Deaths, big.mark = ",")), vjust = 2.5)+
  labs(title = "COVID-19 Deaths in Washington DC Area", x = element_blank(), y = element_blank())

cases_g <- data.frame(Date=cases$Date[2:nrow(cases)], Cases_g=diff(log(cases$Cases))*100)

p3 <- cases_g %>% ggplot(aes(x=Date, y=Cases_g))+geom_line(lwd=1.5, lty=3, col="darkblue")+theme_minimal()+
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d")+
  geom_text(data =  cases_g[nrow(cases_g),], aes(x = Date, y = Cases_g, label = format(Cases_g, small.mark = ".")), vjust = 2.5)+
  labs(title = "COVID-19 Cases Daily Growth Rate in Washington DC Area", x = element_blank(), y = element_blank())

deaths_g <- data.frame(Date=deaths$Date[2:nrow(deaths)], Deaths_g=diff(log(deaths$Deaths))*100)

p4 <- deaths_g %>% ggplot(aes(x=Date, y=Deaths_g))+geom_line(lwd=1.5, lty=3, col="darkred")+theme_minimal()+
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d")+
  geom_text(data =  deaths_g[nrow(deaths_g),], aes(x = Date, y = Deaths_g, label = format(Deaths_g, small.mark = ".")), vjust = 2.5)+
  labs(title = "COVID-19 Deaths Daily Growth Rate in Washington DC Area", x = element_blank(), y = element_blank())

library(gridExtra)
grid.arrange(p1, p2, p3, p4, ncol=2)

