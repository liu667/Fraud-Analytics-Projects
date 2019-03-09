library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(readxl)

data = read_excel("card transactions.xlsx")

##card number
data %>%
  group_by(Cardnum) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  slice(1:20) %>%
  ggplot(aes(x = reorder(as.factor(Cardnum),count), y = count))+
  geom_bar(stat = "identity",fill = "steelblue3")+
  labs(x = "Card Number", y = "Count", title = "Top 20 Card Number Counts")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))+
  theme(plot.title = element_text(face = "bold",size = 18,hjust = 0.5))+
  coord_flip()

##date
data$Date = ymd(data$Date)

daily = data %>%
  group_by(Date)%>%
  summarise(COUNT = n())

ggplot(daily, aes(x = Date, y = COUNT, group =1))+
  geom_line(color = "steelblue3")+
  expand_limits(y=0)+
  labs(title = "DAILY TRANSACTIONS",x = "DATE")+
  theme(plot.title = element_text(face = "bold",size = 18,hjust = 0.5))+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")


data$week = week(data$Date)
weekly = data%>%
  group_by(week) %>%
  summarise(COUNT = n())

ggplot(weekly, aes(x = week, y = COUNT, group =1))+
  geom_line(color = "steelblue3")+
  expand_limits(y=0)+
  labs(title = "WEEKLY TRANSACTIONS",x = "WEEK")+
  theme(plot.title = element_text(face = "bold",size = 18,hjust = 0.5))+
  theme(axis.text = element_text(size = 12))+
  scale_x_continuous(breaks = seq(1,53,4.5),labels = 
                       c("Jan","Feb","Mar","Apr","May","June","Jul","Aug","Sep",
                         "Oct","Nov","Dec"))

data$month = month(data$Date)

monthly = data%>%
  group_by(month) %>%
  summarise(COUNT = n())

ggplot(monthly, aes(x = as.factor(month), y = COUNT, group =1))+
  geom_line(color = "steelblue3")+
  expand_limits(y=0)+
  labs(title = "Monthly TRANSACTIONS",x = "MONTH")+
  theme(plot.title = element_text(face = "bold",size = 18,hjust = 0.5))+
  theme(axis.text = element_text(size = 12)) 
  scale_x_discrete(breaks = seq(1,12,1))

data %>%
    group_by(Date) %>%
    summarise(count = n()) %>%
    arrange(-count) %>%
    slice(1:20) %>%
    ggplot(aes(x = reorder(Date,count), y = count))+
    geom_bar(stat = "identity",fill = "steelblue3")+
    labs(x = "Date", y = "Count", title = "Top 20 Transaction Date Counts")+
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))+
    theme(plot.title = element_text(face = "bold",size = 18,hjust = 0.5))+
    coord_flip()

#Merchant num

data %>%
  group_by(Merchantnum) %>%
  filter(!is.na(Merchantnum)) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  slice(1:20) %>%
  ggplot(aes(x = reorder(Merchantnum,count), y = count))+
  geom_bar(stat = "identity",fill = "steelblue3")+
  labs(x = "Merchant Number", y = "Count", title = "Top 20 Merchant Number Counts")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))+
  scale_y_log10(breaks = c(0,10,100,1000),labels = c(0,10,100,1000))+
  theme(plot.title = element_text(face = "bold",size = 18,hjust = 0.5))+
  coord_flip()


NA1 = data %>%
  filter(is.na(Amount))

#mer description

data %>%
  group_by(Merchantnum) %>%
  filter(!is.na(Merchantnum)) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  slice(1:20) %>%
  ggplot(aes(x = reorder(Merchantnum,count), y = count))+
  geom_bar(stat = "identity",fill = "steelblue3")+
  labs(x = "Merchant Number", y = "Count", title = "Top 20 Merchant Number Counts")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))+
  scale_y_log10(breaks = c(0,10,100,1000),labels = c(0,10,100,1000))+
  theme(plot.title = element_text(face = "bold",size = 18,hjust = 0.5))+
  coord_flip()

length(unique(data$`Merch Description`))

data %>%
  group_by(`Merch Description`) %>%
  filter(!is.na(Merchantnum)) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  slice(1:20) %>%
  ggplot(aes(x = reorder(`Merch Description`,count), y = count))+
  geom_bar(stat = "identity",fill = "steelblue3")+
  labs(x = "Merchant Description", y = "Count", title = "Top 20 Merchant Description Counts")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))+
 # scale_y_log10(breaks = c(0,10,100,1000),labels = c(0,10,100,1000))+
  theme(plot.title = element_text(face = "bold",size = 18,hjust = 0.5))+
  coord_flip()

#state

data %>%
  group_by(`Merchant State`) %>%
  filter(!is.na(`Merchant State`)) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  slice(1:20) %>%
  ggplot(aes(x = reorder(`Merchant State`,count), y = count))+
  geom_bar(stat = "identity",fill = "steelblue3")+
  labs(x = "Merchant State", y = "Count", title = "Top 20 Merchant State Counts")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))+
  # scale_y_log10(breaks = c(0,10,100,1000),labels = c(0,10,100,1000))+
  theme(plot.title = element_text(face = "bold",size = 18,hjust = 0.5))+
  coord_flip()

state = data %>%
  group_by(`Merchant State`) %>%
  filter(!is.na(`Merchant State`)) %>%
  summarise(count = n()) %>%
  arrange(-count) 

#zip

data %>%
  group_by(`Merchant Zip`) %>%
  filter(!is.na(`Merchant Zip`)) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  slice(1:20) %>%
  ggplot(aes(x = reorder(`Merchant Zip`,count), y = count))+
  geom_bar(stat = "identity",fill = "steelblue3")+
  labs(x = "Merchant Zip", y = "Count", title = "Top 20 Merchant Zip Counts")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))+
  scale_y_log10(breaks = c(0,10,100,1000),labels = c(0,10,100,1000))+
  theme(plot.title = element_text(face = "bold",size = 18,hjust = 0.5))+
  coord_flip()

#type

data %>%
  group_by(Transtype) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  slice(1:20) %>%
  ggplot(aes(x = Transtype, y = count))+
  geom_bar(stat = "identity",fill = "steelblue3")+
  labs(x = "Transaction Type", y = "Count", title = "Transaction Type Counts")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))+
  scale_y_log10(breaks = c(0,10,100,1000,10000,100000),labels = c(0,10,100,1000,10000,100000))+
  theme(plot.title = element_text(face = "bold",size = 18,hjust = 0.5))


#Amount

data %>%
 ggplot(aes(x = Amount))+
  geom_histogram(stat = "count",fill = "steelblue3") +
  labs(title = "Amount COUNTS",x ="Amount",y = "Count")+
  theme(plot.title = element_text(face = "bold",size = 18,hjust = 0.5))+
  theme(axis.text = element_text(size = 12))

qplot(data$Amount, geom = "histogram",
      bins = 50)

data %>%
  group_by(Amount) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  slice(1:20) %>%
  ggplot(aes(x = reorder(as.factor(Amount),count), y = count))+
  geom_bar(stat = "identity",fill = "steelblue3")+
  labs(x = "Transaction Amount", y = "Count", title = "Top 20 Transaction Amount")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))+
  #scale_y_log10(breaks = c(0,10,100,1000,10000,100000),labels = c(0,10,100,1000,10000,100000))+
  theme(plot.title = element_text(face = "bold",size = 18,hjust = 0.5))+
  coord_flip()

ggplot(data,aes(x = Fraud))+
  geom_histogram(stat = "count",fill = "steelblue3")+
  geom_text(stat = "count", aes(label = ..count.., y = ..count..),vjust =2)+
  scale_x_continuous(breaks = c(0,1),labels = c(0,1))+
  labs(title = "FRAUD COUNTS",x ="FRAUD",y = "COUNT")+
  theme(plot.title = element_text(face = "bold",size = 18,hjust = 0.5))+
  theme(axis.text = element_text(size = 12))


data$weekend 

  











