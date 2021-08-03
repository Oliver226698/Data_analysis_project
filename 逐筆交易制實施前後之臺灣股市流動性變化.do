clear
cd C:\Users\USER\Desktop\計量經濟\econometrics_2\期末
*import excel 2020大盤.xlsx, firstrow
import excel 2018-2021大盤.xlsx, firstrow 

*cd C:\Users\USER\Desktop\計量經濟\econometrics_2\期末
*import excel 2020_01-12_day_return.xlsx, firstrow
*save "return.dta"
*merge 1:1 年月日 using "return.dta" ,nogen

// import excel 20180101-20210101_day_return.xlsx, firstrow
// save "2018-2021_return.dta"

merge 1:1 年月日 using "2018-2021_return.dta" ,nogen

// cd C:\Users\USER\Desktop\計量經濟\econometrics_2\期末
// * 最後揭示買價 有兩個空值；最後揭示賣價 有八個空值
// * 2020/08/18 聯發科 因為華為被美國制裁 聯發科被影響 故當天跌停 bid = .
// * 2020/01/30 鴻海 因為鴻海在大陸的工廠受武漢肺炎影響 當天跌停 bid =.
// * 2020/12/04 聯電, 11/23 聯電, 9/28 聯電, 9/07 聯電, 7/27 台積電和聯電, 7/23 聯電, 3/20 聯發科。上述因為當天漲停到收盤 故 ask = . 
// 還有其他因漲跌停板，所以未有最後揭示買賣報價
// * 賣價有 19 筆缺失值 買價有 9 筆缺失 成交值占比有 32 筆
// import excel 20180101-20210101_ask_bid.xlsx, firstrow
// * 直接刪除缺失值 共 48 筆資料 (Total: 7340 筆) 
// drop if 最後揭示買價 == "-"
// drop if 最後揭示賣價 == "-"
// drop if 成交值比重 == "-"
// * 將 string 轉成 numeric
// destring 最後揭示買價, replace
// destring 最後揭示賣價, replace
// destring 成交值比重, replace
//
// gen bid_ask_spread = 最後揭示買價 - 最後揭示賣價
// gen weight_bid_ask = bid_ask_spread * 成交值比重
// egen ag_bid_ask = total(weight_bid_ask), by(年月日)
// egen weight_volume = total(成交值比重), by(年月日)
// gen agg_bid_ask = ag_bid_ask / weight_volume * 100
// keep 年月日 agg_bid_ask
// by 年月日, sort: drop if _n > 1
// save "2018-2021_bid_ask_spread.dta", replace
merge 1:1 年月日 using "2018-2021_bid_ask_spread.dta" , nogen


// *三大法人資料檔
// clear
// cd C:\Users\USER\Desktop\計量經濟\econometrics_2\期末
// import excel 20180101-20210101_three.xlsx, firstrow
// save "three.dta"
merge 1:1 年月日 using "three.dta", nogen

encode 年月日, gen(date)
tsset date
summarize
*抓出月份
gen month = substr(年月日, 6,2)

gen order_imbalance = (總委買筆數  - 總委賣筆數)/100000 
gen abs_order = abs(order_imbalance)
gen order_gr = (order_imbalance - L.order_imbalance)/L.order_imbalance
*買賣報價價差每日變化
gen abs_bid_ask = abs(agg_bid_ask)
gen agg_bid_ask_growth = (abs_bid_ask - L.abs_bid_ask)/ L.abs_bid_ask
*gen agg_bid_ask_growth = (agg_bid_ask - L.agg_bid_ask)/L.agg_bid_ask
// *確定 order imbalance 是否有確定性趨勢(?)
// reg order_imbalance date
// predict ehat, residual
// dfuller ehat, noconstant regress

*單根檢定
* order imbalance 都定態
dfuller order_imbalance
dfuller abs_order
dfuller order_gr
* 日報酬 定態
dfuller 日報酬率

* 買賣報價價差 都定態
dfuller agg_bid_ask
dfuller abs_bid_ask
dfuller agg_bid_ask_growth

* 三大法人 都定態
dfuller 外資買賣超張
dfuller 投信買賣超張
dfuller 自營買賣超張

*Correlogram
ac order_imbalance, lags(20)
ac 日報酬率, lags(20)
ac abs_order, lags(50)
ac abs_bid_ask, lags(50)
ac agg_bid_ask, lags(50)
ac agg_bid_ask_growth, lags(50)
ac 成交筆數, lags(50)
* 11
ac 外資買賣超張, lags(50)
* 10
ac 投信買賣超張, lags(50)
* 5
ac 自營買賣超張, lags(50)

* 時間趨勢平方
gen date_sq = date^2
* 逐筆交易 2020/3/23
gen continuous = 1 if date >= 539
replace continuous = 0 if continuous == .
* 零股交易 2020/10/26
gen odd_share = 1 if date >= 686
replace odd_share = 0 if odd_share == .
* 禮拜一至禮拜四虛擬變數
gen monday = 1 if 星期 == "1"
replace monday = 0 if monday == .
gen tuesday = 1 if 星期 == "2"
replace tuesday = 0 if tuesday == .
gen wednesday = 1 if 星期 == "3"
replace wednesday = 0 if wednesday == .
gen thursday = 1 if 星期 == "4"
replace thursday = 0 if thursday == .

*元月效應，過年會橫跨二月
gen jan = 1 if month == "01"
replace jan = 0 if jan == .
gen feb = 1 if month == "02"
replace feb = 0 if feb == .

*COVID-19:2020/01/21-2020/05/25 爆發, 2020/06後為後疫情
gen covid_19 = 1 if date >= 503 & date <= 581
replace covid_19 = 0 if covid_19 == .
gen post_covid = 1 if date >= 582
replace post_covid = 0 if post_covid == .

* 2018年10月貿易戰風暴
gen crisis_2018 = 1 if date >= 183 & date <= 204
replace crisis_2018 = 0 if crisis_2018 == .

* 敘述性統計
summarize agg_bid_ask 日報酬率 成交筆數 order_imbalance 外資買賣超張 投信買賣超張 自營買賣超張 continuous monday tuesday wednesday thursday odd_share jan feb covid_19 crisis_2018

* 安裝 ardl 套件
*ssc install ardl
tsline agg_bid_ask
tsline abs_bid_ask 

*共線性檢定
pwcorr L(1/3).abs_bid_ask continuous monday tuesday wednesday thursday odd_share jan feb covid_19 crisis_2018 date date_sq, st(.05)
* continuous & date_sq 0.8571 顯著相關, continuous & date 0.7663 顯著相關, date & date_sq 0.9683 顯著相關
*進一步做 VIF 
reg abs_bid_ask L(1/3).abs_bid_ask continuous monday tuesday wednesday thursday odd_share jan feb covid_19 crisis_2018 date date_sq
estat vif
reg abs_bid_ask L(1/3).abs_bid_ask continuous monday tuesday wednesday thursday odd_share jan feb covid_19 crisis_2018 date
estat vif
*除了 date_sq 沒有共線性發生 故留下 continuous & date 



*AIC BIC 下 推薦落後3期
ardl abs_bid_ask ,exog(continuous monday tuesday wednesday thursday odd_share jan feb covid_19 crisis_2018) aic bic trendvar(date)

reg abs_bid_ask L(1/3).abs_bid_ask continuous monday tuesday wednesday thursday odd_share jan feb covid_19 crisis_2018 date
*看殘差序列相關
predict ehat, residual 
*看圖 殘差無序列相關
ac ehat, lags(50)

*LM test 一階殘差無序列相關, 後幾期有序列相關
reg abs_bid_ask L(1/3).abs_bid_ask continuous monday tuesday wednesday thursday odd_share jan feb covid_19 crisis_2018 date
estat bgodfrey, lags(1/20) 
*選擇 lag 3 期 測試殘差項是否為AR(x)模型 Ans:與lag 1 & lag 2 有統計顯著性
reg ehat L(1/3).abs_bid_ask continuous monday tuesday wednesday thursday odd_share jan feb covid_19 crisis_2018 date L(1/3).ehat

* abs_bid_ask 選擇1-12期 LM test 下 20期殘差才皆無自我相關
reg abs_bid_ask L(1/12).abs_bid_ask continuous monday tuesday wednesday thursday odd_share jan feb covid_19 crisis_2018 date
predict ehat_12, residual
ac ehat_12, lags(50)
estat bgodfrey, lags(1/20)
reg ehat_12 L(1/12).abs_bid_ask continuous monday tuesday wednesday thursday odd_share jan feb covid_19 crisis_2018 date L(1/12).ehat_12

* 最後確定迴歸式 + robust
reg abs_bid_ask L(1/12).abs_bid_ask continuous monday tuesday wednesday thursday odd_share jan feb covid_19 crisis_2018 date, vce(robust)

// *殘差序列相關 DW 不能用於 AR ARDL
// estat durbinalt, small
// estat dwa


// ardl abs_bid_ask abs_order 日報酬率 外資買賣超張 投信買賣超張 自營買賣超張,exog(continuous monday tuesday wednesday thursday odd_share jan feb covid_19 crisis_2018) aic bic trendvar(date)
//
// tsline agg_bid_ask_growth
//
// reg agg_bid_ask_growth L(1/8).agg_bid_ask_growth L(1/44).abs_order L(1/2).日報酬率 L(1/47).成交筆數 date continuous monday tuesday wednesday thursday odd_share jan feb covid_19 crisis_2018
//
// ardl agg_bid_ask_growth order_gr 日報酬率 外資買賣超張 投信買賣超張 自營買賣超張,exog(continuous monday tuesday wednesday thursday odd_share jan feb covid_19 crisis_2018) aic bic noconstant trendvar(date)

// * chow test : something wrong
// ardl agg_bid_ask_growth if continuous == 0 , aic bic
// scalar b1 = _b[L.agg_bid_ask_growth ]
// ardl agg_bid_ask_growth if continuous == 1 , aic bic
// scalar b2 = _b[L.agg_bid_ask_growth ]
// test b1 = b2, notest

* 交易量圖
graph twoway tsline 成交筆數, tline(539 686)
* 定態
dfuller 成交筆數
ardl 成交筆數,exog(continuous monday tuesday wednesday thursday odd_share jan feb covid_19 crisis_2018) aic bic

* 委託單不平衡 定態10%顯著水準下定態 成長率 1%
dfuller abs_order
dfuller order_gr
graph twoway tsline abs_order, tline(539 686)
ardl abs_order, exog(continuous monday tuesday wednesday thursday odd_share jan feb covid_19 crisis_2018) aic bic trendvar(date)

* 季資料：2011Q4-2020Q4 共37筆資料
* VAR 可以設定外生變數
clear
cd C:\Users\USER\Desktop\計量經濟\econometrics_2\期末
import excel quarter.xlsx, firstrow
encode date, gen(time)
tsset time

summarize agg_bid_ask return order_imbalance 外資買賣超張 投信買賣超張 自營買賣超張 RealGDP over_hour industry_index export_moneythousandsNT

*gen 總體經濟變數成長率
gen GDP_gr = (RealGDP - L.RealGDP)/L.RealGDP
gen over_hour_gr = (over_hour - L.over_hour)/L.over_hour
gen industry_index_gr = (industry_index - L.industry_index)/L.industry_index
gen export_moneythousandsNT_gr = (export_moneythousandsNT - L.export_moneythousandsNT)/ L.export_moneythousandsNT

*gen others
gen abs_order = abs(order_imbalance)
gen abs_bid_ask = abs(agg_bid_ask)
gen agg_bid_ask_gr = (abs_bid_ask - L.abs_bid_ask)/L.abs_bid_ask
gen 外資_gr = (外資買賣超張-L.外資買賣超張)/L.外資買賣超張
gen 投信_gr = (投信買賣超張-L.投信買賣超張)/L.投信買賣超張
gen 自營_gr = (自營買賣超張-L.自營買賣超張)/L.自營買賣超張
gen return_gr = (return-L.return)/L.return
gen order_gr = (order_imbalance-L.order_imbalance)/L.order_imbalance

*單根檢定
* GDP 非定態, 成長率 定態
dfuller RealGDP
dfuller GDP_gr
* 加班工時 定態, 成長率 定態
dfuller over_hour
dfuller over_hour_gr
* 工業生產指數 非定態, 成長率 定態
dfuller industry_index
dfuller industry_index_gr
* 海關出口 非定態, 成長率 定態
dfuller export_moneythousandsNT
dfuller export_moneythousandsNT_gr
* 買賣報價價差 非定態, 成長率 定態
dfuller abs_bid_ask
dfuller agg_bid_ask_gr
* 外資買賣超張 , 成長率 都定態
dfuller 外資買賣超張
dfuller 外資_gr
* 投信買賣超張 , 成長率 都定態
dfuller 投信買賣超張
dfuller 投信_gr
* 自營買賣超張 , 成長率 都定態
dfuller 自營買賣超張
dfuller 自營_gr
* 市場報酬 , 成長率 都定態
dfuller return
dfuller return_gr
* 委託單不平衡 非定態, 成長率 定態
dfuller order_imbalance
dfuller order_gr

*逐筆交易制
gen continuous = 1 if time >= 35
replace continuous = 0 if continuous == .
// *COVID 爆發
// gen covid_19 = 1 if time == 34 | time == 35
// replace covid_19 = 0 if covid_19 == .

*找出最適期數
varsoc agg_bid_ask GDP_gr over_hour_gr industry_index_gr export_moneythousandsNT_gr 外資買賣超張 投信買賣超張 自營買賣超張 return order_gr

varsoc agg_bid_ask_gr GDP_gr over_hour_gr industry_index_gr export_moneythousandsNT_gr 外資買賣超張 投信買賣超張 自營買賣超張 return order_gr

* 選兩期
var abs_bid_ask GDP_gr over_hour_gr industry_index_gr export_moneythousandsNT_gr 外資買賣超張 投信買賣超張 自營買賣超張 return abs_order, exog(continuous) lags(1/2)

* 選兩期
var agg_bid_ask_gr GDP_gr over_hour_gr industry_index_gr export_moneythousandsNT_gr 外資_gr 投信_gr 自營_gr return_gr order_gr, exog(continuous) lags(1/2)

var order_gr agg_bid_ask_gr GDP_gr over_hour_gr industry_index_gr export_moneythousandsNT_gr 外資_gr 投信_gr 自營_gr return_gr order_gr, exog(continuous) lags(1/2)