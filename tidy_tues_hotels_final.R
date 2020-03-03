library(tidyverse)
library(ggthemes)
library(lubridate)
library(tidyquant)
library(aod)
library(boot)


hotels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv')

#create date variable
hotels_tidy <- hotels %>% mutate(arrival_date = dmy(paste(arrival_date_day_of_month, arrival_date_month, arrival_date_year)))

xtabs(~is_canceled + customer_type, data=hotels)

hotel_learn <- hotels %>% filter(arrival_date_year < 2017)
hotel_test <- hotels %>% filter(arrival_date_year==2017)

hotel_logit <- glm(is_canceled ~ lead_time + adults + children + babies + 
                     market_segment + is_repeated_guest + stays_in_weekend_nights +
                     stays_in_week_nights + hotel + 
                     previous_cancellations + customer_type,
                   data=hotel_learn, family="binomial")

hotel_aug <- augment(hotel_logit, newdata=hotel_test)

ggplot(hotel_aug, aes(x=factor(is_canceled), y=inv.logit(.fitted))) + 
  geom_jitter(alpha=0.1) + geom_boxplot(col="red", width=0.5, alpha=0.5) + theme_tufte() +
  labs(title="Cancellation prediction model", y="Predicted probability of cancellation",
       x="Is cancelled (0 = not cancelled, 1 = cancelled", caption="Data: Antonio, Almeida and Nunes, 2019.") + facet_wrap(~hotel) 
ggsave("hotels.png")