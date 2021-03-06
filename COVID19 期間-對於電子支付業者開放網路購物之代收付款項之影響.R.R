#install.packages("plm")
#install.packages("rio")

##看相關係數顯著性的package
#install.packages("Hmisc")

library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)
library(plm)
library(Hmisc)
library(rio)


#import 2019 data
data_2019 = list()

for( n in c(1:9)){
  data_2019[[n]] <- read_excel(str_c("C:/Users/Oliver/Desktop/社會安全制度/final/1080", n,"_電子支付機構重要資訊揭露",".xlsx"), 
                               skip = 4, col_types = c("text","numeric","numeric","numeric","numeric","numeric"),
                               col_names = c("Name", "People", "transaction_thousand", "1", "2", "3")) %>%
  na.omit %>%
  select(1:3) %>%
  subset(!(Name %in% c("總計", "愛金卡股份有限公司", "悠遊卡股份有限公司"))) %>%
  mutate(month = str_c("108m0", n))
}

for(n in c(10:12)){
  data_2019[[n]] <- read_excel(str_c("C:/Users/Oliver/Desktop/社會安全制度/final/108", n,"_電子支付機構重要資訊揭露",".xlsx"), 
                               skip = 4, col_types = c("text","numeric","numeric","numeric","numeric","numeric"),
                               col_names = c("Name", "People", "transaction_thousand", "1", "2", "3")) %>%
    na.omit %>%
    select(1:3) %>%
    subset(!(Name %in% c("總計", "愛金卡股份有限公司", "悠遊卡股份有限公司"))) %>%
    mutate(month = str_c("108m", n))
}

data_2019.m <- bind_rows(data_2019[1:12]) %>%
  mutate(transaction = transaction_thousand *1000,
         ln_transaction = log(transaction))%>%
  select(-transaction_thousand)
data_2019.m$Name <- gsub("\\s","",data_2019.m$Name) ##取代掉金融機構名稱中的空白

View(data_2019.m)



#import 2020_01-10 data
data = list()

for( n in c(1:9)){
  data[[n]] <- read_excel(str_c("C:/Users/Oliver/Desktop/社會安全制度/final/1090", n,"_電子支付機構重要資訊揭露",".xlsx"), 
             skip = 2, col_types = c("text","numeric","numeric","numeric","numeric","numeric"),
             col_names = c("Name", "People", "transaction_thousand", "1", "2", "3")) %>% 
    na.omit() %>%
    select(1:3) %>%
    subset(!(Name %in% c("總計", "愛金卡股份有限公司", "悠遊卡股份有限公司"))) %>%
    mutate(month = str_c("109m0", n))
}

data_10 <- read_excel("C:/Users/Oliver/Desktop/社會安全制度/final/10910_電子支付機構重要資訊揭露.xlsx", 
                   skip = 2, col_types = c("text","numeric","numeric","numeric","numeric","numeric"),
                   col_names = c("Name", "People", "transaction_thousand", "1", "2", "3")) %>%
  na.omit%>%
  select(1:3) %>%
  subset(!(Name %in% c("總計", "愛金卡股份有限公司", "悠遊卡股份有限公司"))) %>%
  mutate(month = str_c("109m10"))
#View(data_10)


data_2020.m <- bind_rows(data[1:9], data_10) %>%
  mutate( transaction = transaction_thousand *1000,
         ln_transaction = log(transaction)) %>%
          select(-transaction_thousand)
data_2020.m$Name <- gsub("\\s","",data_2020.m$Name) ##取代掉金融機構名稱中的空白

data.m <- bind_rows(data_2019.m , data_2020.m)
View(data.m)

# Import the growth of online users
online_users <- read_excel("C:/Users/Oliver/Desktop/社會安全制度/final/行動寬頻用戶淨增加數.xlsx") %>%
  filter(month != "108m1")


# Calculate the growth of users
data_growth <- group_by(data.m, Name) %>%
  mutate(growth_user = People - dplyr::lag(People, default = People[1])) %>%
  subset(month != "108m01") %>%
  select(Name, month, growth_user)

View(data_growth)

# Calculate 每家金控每月的代收付金額占比
last_percent_money <- data.m %>%
  group_by(month) %>%
  summarise(last_percent_m = transaction / sum(transaction) * 100,
            Name) %>%
  filter(month != "109m10") %>%
  select(last_percent_m)

View(last_percent_money)


# Calculate Last month people
data.c <- subset(data.m, month != "109m10") %>%
  select(Name ,People) %>%
  rename(last_people = People)
  
data.c$Name <- gsub("\\s","",data.c$Name) ##取代掉金融機構名稱中的空白

View(data.c)

#先剔除 data.m 中 10801 的資料, 再合併上個月使用人數 & 上個月的市占率
data.m <- filter(data.m, data.m$month != "108m01")
data.mm <- bind_cols(data.m, last_percent_money, data.c, data_growth)
data.mmm <- data.mm %>% 
  select(-c(month...6, Name...8, Name...10, month...11)) %>%
  rename(Name = Name...1,
         month = month...3)
  
View(data.mmm)

#合併資料 & 將 ln_transaction 無法正常顯的值修正掉 & 上一期的代收付比例觀察log 是否出現怪怪的

data.mmmm <- left_join(data.mmm, online_users, by = c("month" = "month"))
data.mmmm$ln_transaction[is.infinite(data.mmmm$ln_transaction)] <- 0
data.mmmm <- data.mmmm %>% mutate(ln_last_percent_m = log(last_percent_m + 1))
data.mmmm$ln_last_percent_m[is.infinite(data.mmmm$ln_last_percent_m)] <- 0
data.mmmm <- data.mmmm %>%
  mutate(ln_last_people = log(last_people))

View(data.mmmm)

##產生八大公股行庫虛擬變數
data.mmmm <- data.mmmm %>%
  mutate(government = ifelse(Name %in% c("臺灣銀行", "臺灣土地銀行", "臺灣中小企業銀行", "合作金庫商業銀行",
                                         "兆豐國際商業銀行", "第一商業銀行", "華南商業銀行", "彰化商業銀行"), 1,0))

##先測試後疫情時代，故疫情虛擬變數 post_COVID19 10905 之後為 1
##將疫情嚴重發生期(109m1-109m5) COVID19 = 1
##整個疫情都有影響的 2020 年 產生 all_COVID19 = 1

data.mmmm <- data.mmmm %>%
  mutate(post_COVID19 = ifelse(month %in% c("109m06", "109m07","109m08","109m09","109m10"), 1, 0))
data.mmmm <- data.mmmm %>%
  mutate(COVID19 = ifelse(month %in% c("109m01", "109m02","109m03","109m04","109m05"), 1, 0),
        all_COVID19 = ifelse(month %in% c("109m01", "109m02", "109m03", "109m04", "109m05",
                                                  "109m06", "109m07","109m08","109m09","109m10"), 1, 0))


####產生有開放網路購物的電子支付業者：玉山, 街口, 一卡通為 1 之虛擬變數，作為實驗組
data.mmmm <- data.mmmm %>%
  mutate(online_shop = ifelse(Name %in% c("玉山商業銀行", "一卡通票證股份有限公司", "街口電子支付股份有限公司"), 1,0))


#畫圖區喔
##總計代收付金額趨勢圖

data.mmmm %>% 
  group_by(month) %>%
  summarise(money = sum(transaction)/100000000) %>%
  ggplot(aes(x = factor(month), y = money, label = round(money, digits = 2))) + 
  geom_line(group = 1, size = 1, colour = "blue") +
  geom_point(size = 3, colour = "black") +
  geom_text(vjust = -1.5) +
  xlab("") + ylab("Total_代收付金額(億元)") +
  theme_bw()

ggsave("總額.png", width = 30, height = 24)

##各家業者在每個月的代收付金額市占率趨勢圖
data.mmmm %>% 
  ggplot(aes(x = factor(month), y = last_percent_m, 
             label = round(last_percent_m, digits = 2),
             group = Name)) +
  geom_line(size = 1) +
  facet_wrap(~Name, nrow = 3) +
  labs(x = "", y = "上個月之代收付金額佔比") +
  scale_x_discrete(breaks = c("108m06", "108m12", "109m06","109m10")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -30))

##各家業者在每個月的實質代收付金額趨勢圖
data.mmmm %>% 
  ggplot(aes(x = factor(month), y = transaction/100000000, 
             label = round(last_percent_m, digits = 2),
             group = Name)) +
  geom_line(size = 1) +
  facet_wrap(~Name, nrow = 3) +
  labs(x = "", y = "代理收付實質交易款項金額(億元)") +
  scale_x_discrete(breaks = c("108m06", "108m12", "109m06","109m10")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -30))


##transaction 分布 建議取log
ggplot(data.mmmm, aes(x = ln_transaction, group = Name)) +
  geom_histogram(bins = 30, fill = "#6495ED") 

##last_people 分布 建議取 log 可以取
ggplot(data.mmmm, aes(x = ln_last_people, group = Name)) +
  geom_histogram(bins = 30, fill = "#6495ED")

## growth_user 分布 建議取 log 但有負值故不取
ggplot(data.mmmm, aes(x = growth_user, group = Name)) +
  geom_histogram(bins = 30)

#Panel Data

##敘述性統計
summary(data.mmmm)

##檢查共線性 Model_1a
correlation <- data.mmmm %>%
  select(last_percent_m, ln_last_people, growth_user, online_users, post_COVID19, COVID19, government)
rc <- rcorr(as.matrix(correlation), type = "pearson")
#install.packages("corrplot")
library(corrplot)
corrplot(rc[[1]], type = "upper", method = "number",
         tl.col = "black", tl.srt = 45)

##檢查共線性 Model_1b
correlation2 <- data.mmmm %>%
  select(last_percent_m, ln_last_people, growth_user, online_users, all_COVID19, government)
rc2 <- rcorr(as.matrix(correlation2), type = "pearson")
corrplot(rc2[[1]], type = "upper", method = "number",
         tl.col = "black", tl.srt = 45)



options(scipen=999) ## Cancel 掉科學符號
data.mmmm <- data.frame(data.mmmm)
data.panel <- pdata.frame(data.mmmm, index = c("Name", "month"))

# #單根檢定
# ##llc test 時間長度過短 故無法 test 且時間短大多呈現定態
# install.packages("tseries")
# library(tseries)
# purtest(data.panel$transaction, test= "levinlin")


#一般Panel Data Model_1a(COVID19 + post_COVID19)
re <- plm(formula = ln_transaction ~ ln_last_people + growth_user + post_COVID19 + COVID19 + online_users + last_percent_m + government, 
          data = data.panel,
          model = "random",
          index = c("Name", "month"))

summary(re)

fe <- plm(formula = ln_transaction ~ ln_last_people + growth_user + post_COVID19 + COVID19  + online_users + last_percent_m + government, 
          data = data.panel,
          model = "within",
          index = c("Name", "month"))

summary(fe)

# Hausman test：不拒絕虛無假設 → 使用 RE，進一步做 LM TEST
phtest(fe, re)

#LM test：若拒絕虛無假設，使用 RE
pool <- plm(formula = ln_transaction ~ ln_last_people + growth_user + post_COVID19 + COVID19 + online_users + last_percent_m + government, 
            data = data.panel,
            model = "pooling",
            index = c("Name", "month"))
summary(pool)

plmtest(pool, type=c("bp"))

#一般Panel Data Model_1b(all_COVID19)
re <- plm(formula = ln_transaction ~ ln_last_people + growth_user + all_COVID19 + online_users + last_percent_m + government, 
          data = data.panel,
          model = "random",
          index = c("Name", "month"))

summary(re)

fe <- plm(formula = ln_transaction ~ ln_last_people + growth_user + all_COVID19 + online_users + last_percent_m + government, 
          data = data.panel,
          model = "within",
          index = c("Name", "month"))

summary(fe)

# Hausman test：不拒絕虛無假設 → 使用 RE，進一步做 LM TEST
phtest(fe, re)

#LM test：若拒絕虛無假設，使用 RE
pool <- plm(formula = ln_transaction ~ ln_last_people + growth_user + all_COVID19 + online_users + last_percent_m + government, 
            data = data.panel,
            model = "pooling",
            index = c("Name", "month"))
summary(pool)

plmtest(pool, type=c("bp"))





#DiD 差異中的差異_後疫情時代(109m06-109m10)_有開放網路購物的行動支付是否可以增加代收付金額
re_did <- plm(formula = ln_transaction ~ ln_last_people + post_COVID19*online_shop + growth_user + online_users + last_percent_m + COVID19 + government, 
          data = data.panel,
          model = "random")

summary(re_did)

fe_did <- plm(formula = ln_transaction ~ ln_last_people + post_COVID19*online_shop + growth_user + online_users + last_percent_m + COVID19 + government, 
          data = data.panel,
          model = "within",
          effect = "twoways")

summary(fe_did)

# Hausman test：不拒絕虛無假設 → 使用 RE，進一步做 LM TEST
phtest(fe_did, re_did)

#LM test：拒絕虛無假設，使用 RE
pool_did <- plm(formula = ln_transaction ~ ln_last_people + post_COVID19*online_shop + growth_user + online_users + last_percent_m + COVID19 + government, 
          data = data.panel,
          model = "pooling")
summary(pool_did)

plmtest(pool_did, type=c("bp"))


####DiD 差異中的差異_疫情爆發期間(109m01-109m05)_有開放網路購物的行動支付是否可以增加代收付金額
re_did2 <- plm(formula = ln_transaction ~ ln_last_people + COVID19*online_shop + growth_user + online_users + last_percent_m + post_COVID19 + government, 
              data = data.panel,
              model = "random")

summary(re_did2)

fe_did2 <- plm(formula = ln_transaction ~ ln_last_people + COVID19*online_shop + growth_user + online_users + last_percent_m + post_COVID19 + government, 
              data = data.panel,
              model = "within")

summary(fe_did2)

# Hausman test：不拒絕虛無假設 → 使用 RE，進一步做 LM TEST
phtest(fe_did2, re_did2)

#LM test：拒絕虛無假設，使用 RE
pool_did2 <- plm(formula = ln_transaction ~ ln_last_people + COVID19*online_shop + growth_user + online_users + last_percent_m + post_COVID19 + government,  
                data = data.panel,
                model = "pooling")
summary(pool_did2)

plmtest(pool_did2, type=c("bp"))




### DiD 差異中的差異_疫情期間(109m01-109m10)_有開放網路購物的行動支付是否可以增加代收付金額

re_did3 <- plm(formula = ln_transaction ~ ln_last_people + all_COVID19*online_shop + growth_user + online_users + last_percent_m + government, 
               data = data.panel,
               model = "random")

summary(re_did3)


fe_did3 <- plm(formula = ln_transaction ~ ln_last_people + all_COVID19*online_shop + growth_user + online_users + last_percent_m + government, 
               data = data.panel,
               model = "within")


summary(fe_did3)

# Hausman test：不拒絕虛無假設 → 使用 RE，進一步做 LM TEST
phtest(fe_did3, re_did3)

#LM test：拒絕虛無假設，使用 RE
pool_did3 <- plm(formula = ln_transaction ~ ln_last_people + all_COVID19*online_shop + growth_user + online_users + last_percent_m + government,  
                 data = data.panel,
                 model = "pooling")
summary(pool_did3)

plmtest(pool_did3, type=c("bp"))
