library (nycflights13)
library(tidyverse)
library(lubridate)
install.packages("nycflights13")
flights<-nycflights13::flights
colnames(vuelos)
flights
na.
vuelos$retraso<-vuelos$arr_delay(vuelos$arr_delay, na.rm=T)
na.omit(vuelos$arr_delay) vuelos$arr_delay[vuelos$arr_delay>60]
retraso<-na.omit(vuelos$arr_delay)
View(retraso)
sum(retraso>60)
sum(retraso)
dim()
any(is.na(vuelos$dest))
vuelos$dest[vuelos$dest== c("SFO","OAK")]
vuelos$carrier[vuelos$carrier== c("UA","AA")]
any(is.na(vuelos$month))
vuelos$month[vuelos$month== c(4,5,6)]
vuelos$month[vuelos$month==4]
vuelos$month[vuelos$month==c(5,6)]
vuelos$month[vuelos$month==6]
filter(vuelos, dest== "OAK"| dest=="SFO")
filter(vuelos, month == 1)
library(tidyverse)
filter(flights, month == 1, day == 1)
filter(vuelos, month == 1, day == 1)
filter(vuelos, dest == "SFO" | dest == "OAK")
filter(vuelos, carrier == "UA" |carrier == "AA")
is.na(vuelos)
filter(vuelos, arr_delay>60|is.na(vuelos))
any(is.na(vuelos$month))
filter(vuelos, month==4|month==5|month==6)
filter(vuelos, arr_delay>60&dep_delay<60)
filter(is.na(vuelos))
any(is.na(vuelos$dest))
filter(vuelos, hour==c(24: 7))
filter(vuelos, hour<=7)        
filter(vuelos, is.na(dep_time))
col(any(is.na(vuelos)))
apply(X = is.na(vuelos), MARGIN = 2, FUN = sum)
arrange(vuelos, desc(dep_delay))
arrange(vuelos, dep_delay)
?flights
vuelos$tiempototal<-(vuelos$hour*60)+vuelos$minute        
arrange(vuelos, desc(tiempototal))
arrange(vuelos, desc(distance))
arrange(vuelos,distance)
arrange(vuelos, desc(distance))
any(is.na(vuelos$distance))
horasalida<-transmute(vuelos, dep_time, horasalida= (dep_time %/% 100 * 60) + (dep_time %% 100))
vuelos$horasalida<-(vuelos$dep_time %/% 100 * 60) + (vuelos$dep_time %% 100)
vuelos$horasalest<-(vuelos$sched_dep_time %/% 100 * 60) + (vuelos$sched_dep_time %% 100)
vuelos$tiempototal<-(vuelos$arr_delay+vuelos$dep_delay)
arrange(vuelos, tiempototal)
apply(X = is.na(vuelos), MARGIN = 2, FUN = sum)
filter(vuelos, is.na(arr_delay)|is.na(tailnum))
group_by(vuelos, year, month, day)
vueloscanc<-transmute(vuelos, cancelados = (is.na (tailnum)), year, month, day)
summarise(cancelled_num = sum(vueloscanc$cancelados), flights_num = n(),)
vueloscanc <-  vuelos %>%
  mutate(cancelado = (is.na(tailnum))) %>%
  group_by(year, month, day) %>%
  summarise(cancelled_num = sum(cancelado), flights_num = n(),)
ggplot(cancelled_per_day) +
  geom_point(aes(x = flights_num, y = cancelled_num, col=flights_num)) 
proptard_canc <- 
  vuelos %>%
  mutate(cancelados = (is.na(tailnum))) %>%
  group_by(year, month, day) %>%
  summarise(cancelados_prop = mean(cancelados),med_dep_delay = mean(dep_delay, na.rm = TRUE),med_arr_delay = mean(arr_delay, na.rm = TRUE)) %>% ungroup()
ggplot(proptard_canc) +
  geom_point(aes(x = med_dep_delay, y = cancelados_prop, col=cancelados_prop))
ggplot(proptard_canc) +
  geom_point(aes(x = med_arr_delay, y = cancelados_prop, col=cancelados_prop))
?summarise
proptard_canc_aer <- 
  vuelos %>%
  mutate(cancelados = (is.na(tailnum))) %>%
  group_by(origin, dest) %>%
  summarise(cancelados_prop = mean(cancelados),med_dep_delay = mean(dep_delay, na.rm = TRUE),med_arr_delay = mean(arr_delay, na.rm = TRUE)) %>% ungroup()
ggplot(proptard_canc_aer) +
  geom_point(aes(x = med_dep_delay, y = cancelados_prop, col=cancelados_prop))
ggplot(proptard_canc_aer) +
  geom_point(aes(x = med_arr_delay, y = cancelados_prop, col=cancelados_prop))
vuelos %>%
  group_by(carrier) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE), arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(desc(dep_delay))
arrange(desc(arr_delay))
vuelos %>%
  group_by(hour) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  arrange(dep_delay)
 
 vuelos_semana<- vuelos %>% 
   group_by(year, month, day, dep_delay) %>%
   summarise(dep_delay = mean(dep_delay, na.rm = TRUE))

 flights %>% 
   select(year, month, day, hour) %>% 
   mutate(departure = make_datetime(year, month, day, hour))
 
 make_datetime_100 <- function(year, month, day, time) {
   make_datetime(year, month, day, time %/% 100, time %% 100)
 }
 
 flights_dt <- flights %>% 
   filter(!is.na(dep_time), !is.na(arr_time)) %>% 
   mutate(
     dep_time = make_datetime_100(year, month, day, dep_time),
     arr_time = make_datetime_100(year, month, day, arr_time),
     sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
     sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
   ) %>% 
   select(origin, dest, ends_with("delay"), ends_with("time"))
 
 flights_dt %>%
   mutate(dow = wday(sched_dep_time)) %>%
   group_by(dow) %>%
   summarise(
     dep_delay = mean(dep_delay),
     arr_delay = mean(arr_delay, na.rm = TRUE)
   ) %>%
   print(n = Inf)
 flights_dt %>%
   mutate(wday = wday(dep_time, label = TRUE)) %>% 
   group_by(wday) %>% 
   summarize(ave_dep_delay = mean(dep_delay, na.rm = TRUE)) %>% 
   ggplot(aes(x = wday, y = ave_dep_delay)) + 
   geom_bar(stat = "identity") 
 
 vuelos_totdel<-flights %>%
   filter(arr_delay > 0) %>%
   group_by(dest) %>%
   summarise(arr_delay = sum(arr_delay)) %>%
   group_by(dest) %>%
   mutate(
     arr_delay_prop = arr_delay / sum(arr_delay)
   ) %>%
   arrange(dest, desc(arr_delay_prop)) %>%
   select(carrier, flight, origin, dest, arr_delay_prop)
 
 vuelos_totdel<-
   mutate(
     arr_delay_prop = arr_delay / sum(arr_delay)
   )
 
 vuelos_prop<-flights %>%
   filter(arr_delay > 0)
 mutate(arr_delay_prop = arr_delay / sum(arr_delay))
 summarise(arr_delay = sum(arr_delay))
 group_by(dest)

vuelospro<- flights %>%
   filter(arr_delay > 0)
 mutate(arr_delay_prop = arr_delay / sum(arr_delay))
 mutate(arr_delay_tot= sum(arr_delay))
 
 vuelos_totpro<-vuelos %>%
   filter(arr_delay > 0) %>%
   group_by(dest) %>%
   mutate(arr_delay_prop = arr_delay / sum(arr_delay))%>%
   summarise(arr_delay = sum(arr_delay))
 
 flights %>%
   filter(arr_delay > 0) %>%
   group_by(dest, origin, carrier, flight) %>%
   summarise(arr_delay = sum(arr_delay)) %>%
   group_by(dest) %>%
   mutate(
     arr_delay_prop = arr_delay / sum(arr_delay)
   ) %>%
   arrange(dest, desc(arr_delay_prop)) %>%
   select(carrier, flight, origin, dest, arr_delay_prop) 
 
 flights_dt %>%
   mutate(sched_dep_hour = hour(sched_dep_time)) %>%
   group_by(sched_dep_hour) %>%
   summarise(dep_delay = mean(dep_delay)) %>%
   ggplot(aes(y = dep_delay, x = sched_dep_hour)) +
   geom_point() +
   geom_smooth()
 
 vuelos %>%
   group_by(carrier) %>%
   summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
   arrange(desc(arr_delay))
 
vuelos %>%
   group_by(carrier) %>%
   summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
   arrange(desc(dep_delay))

 