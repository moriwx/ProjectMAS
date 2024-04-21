rm(list=ls())
##=============================
####### 1、数据爬取 ##########
##=============================
#分两步
#先爬取具体某个股票市场的股票代码，注意，不是开高低收
#"A","SH","SZ","USA","HK"
get_code_market=function(category){
  library("rvest") 
  library("dplyr")
  if(category %in% c("A","SH","SZ","USA","HK")){
    market=data.frame(category=c("A","SH","SZ","USA","HK"),time_id=c("m:0+t:6,m:0+t:80,m:1+t:2,m:1+t:23","m:1+t:2,m:1+t:23","m:0+t:6,m:0+t:80","m:105,m:106,m:107","m:128+t:3,m:128+t:4,m:128+t:1,m:128+t:2"))
    max_page_num = 100
    page_size = 200
    time_id=market[market$category==category,]$time_id
    for(ii in 1:max_page_num){
      page_num=ii
      base_url = paste0("http://54.push2.eastmoney.com/api/qt/clist/get?pn=",page_num,"&pz=",page_size,"&po=1&np=1&fltt=2&invt=2&fid=f3&fs=",time_id,"&fields=f12,f14")
      get=read_html(base_url,encoding = "UTF-8") %>% html_nodes('p') %>% html_text()
      get_str=strsplit(strsplit(strsplit(get,":\\[\\{")[[1]][2],"\\}\\]\\}\\}")[[1]][1],"\\},\\{")[[1]]
      for(jj in 1:length(get_str)){
        get_ = unlist(strsplit(get_str[jj],'"'))
        one_code=t(as.data.frame(c(get_[4],get_[8])))
        if(ii==1&jj==1){all_code=one_code}else{all_code=rbind(all_code,one_code)}
      }
    }
    all_code=as.data.frame(all_code)
    colnames(all_code)=c("Code","Name")
    all_code=all_code[!duplicated(all_code$Code),]
    all_code=all_code[-nrow(all_code),]
    rownames(all_code)=all_code[,1]
    return(all_code)
  }else{
    stop(paste0(category," dose not belong to the set (A,SH,SZ,USA,HK)"))
  }
}

#再根据具体股票代码去爬取开高低收
get_stock_data=function(market,code,start_time,end_time){
  #install.packages('pedquant')
  #library(quantstrat)
  library(tidyverse) 
  library(pedquant) 
  if(market %in% c("A","USA","SH","SZ","HK")){
    code=as.character(code)
  }else{
    stop(paste0(market," dose not belong to the set (A,USA,SH,SZ,HKA)"))
  }
  pd163_ <- md_stock(symbol=code,from=start_time,to=end_time,source="163",adjust=NULL)
  pd163_tidy <- pd163_[[1]]%>%
    as_tibble()#%>%
  #select(symbol,date,open,high,low,close,volume,amount)
  #pd163_tidy$volume=log(as.numeric(pd163_tidy$volume),10)#成交量
  #pd163_tidy$amount=log(as.numeric(pd163_tidy$amount),10)#成交额
  #pd163_tidy=pd163_tidy[,-1]
  #colnames(pd163_tidy)[c(6,7)]=c("log_volume","log_amount")
  return(pd163_tidy)
}

market <- get_code_market(category="HK")#获取港股代码

#code形式
## A股如 000001
## 美股 如 KTEC
## 港股 如 09988.hk
HK_select <- get_stock_data(market="HK",code="09988.hk",start_time="2021-06-01",end_time="2022-07-01")
#which(market[,1]=="09988")
#market[732,]
##=============================
####### 2、时序数据生成 #######
##=============================
#ts数据
data <- ts(c(5, 5, 4, 6, 4, 3, 3, 3, 4, 5, 5, 4), start=c(2001,1), frequency=12)
data <- ts(c(5, 5, 4, 6, 4, 3, 3, 3, 4, 5, 5, 4), start=c(2001), frequency=1)
data <- ts(c(5, 5, 4, 6, 4, 3, 3, 3, 4, 5, 5, 4), start=c(2001,1), frequency=4)
ts(data = 1:10,start = 2001,end = 2010)
ts(data = 1:10,start = 2001,frequency = 4)
ts(data = 1:400,start = c(2001,2),frequency = 365)

#xts数据
library(xts)
dates <- seq(as.Date("2016-01-01"),length = 10,by = "days")
smith <- xts(x = rnorm(10),order.by = dates)#xts要求行名必须是日期

##==================================
####### 3、读取数据+日期处理 #######
##==================================
# 读取txt文件
setwd("D:/Study/R.program/rdata/Teaching assistant data")
read_table("return.txt")#tidyverse package
return <- read_table(
  "return.txt", col_types=cols(
    .default=col_double(),#默认列的类型是双浮点
    date=col_date(format="%Y%m%d")))
# 读取csv文件
ibm <- read_csv(
  "ibm.csv",
  col_types=cols(
    .default=col_double(),#默认列的类型是双浮点
    date=col_date(format="%Y%m%d")))
#字符型转日期型
a <- "2018-07-02"
class(a)
lubridate::ymd("2018-07-02")
#字符型向量转换为日期
lubridate::make_date(2018, 7, 2)

##==================================
####### 4、plot #######
##==================================
##### 4.1 读取sun_spots数据 #######
library(timetk)
library(tibbletime) 
library(lubridate)
sun_spots <- datasets::sunspot.month %>%
  tk_tbl() %>% #将ts数据转换为tibble数据
  mutate(index = as_date(index)) %>%#lubridate package
  as_tbl_time(index = index)#tbl_time，为 tibble 对象添加时间轴，赋予处理时间的能力
sun_spots
# 可视化
library(tidyquant)
p1 <- sun_spots %>%
  ggplot(aes(index, value)) +
  geom_point(
    color = palette_light()[[3]], alpha = 0.2) +#palette_light()可显示所以的颜色,tidyquant package
  theme_tq() +
  labs(title = "From 1749 to 2013 (Full Data Set)")+#左上角添加标题
  geom_line()

p2 <- sun_spots %>%
  filter_time("start" ~ "1800") %>%#选取前50年数据绘图
  ggplot(aes(index, value)) +
  geom_line() +
  geom_point(color = palette_light()[[3]],alpha =0.2) +#alpha透明度
  geom_smooth(method = "loess", span = 0.2, color = palette_light()[[2]],se = FALSE) +
  theme_tq() +
  labs(
    title = "1749 to 1800 (Zoomed In To Show Cycle)",
    caption = "datasets::sunspot.month")# 图中右下角添加注释

library(cowplot)
p_title <- ggdraw() + #设置一个图形只显示标签
  draw_label(
    "Sunspots",#具体的显示，为文本
    size = 18,
    fontface = "bold",#字体选择
    colour = palette_light()[[1]])
plot_grid(
  p_title, p1, p2,#上述三个图形
  ncol = 1,#显示为1列
  rel_heights = c(0.1, 1, 1))#p_title, p1, p2的行高

############# 4.2 SPX #############
#读取数据函数
get_data <- function(load){
  data <- read_csv(
    load,
    col_types = cols(
      trade_date = col_date(format="%Y%m%d"),
      .default = col_double()
    )
  )
  data <- data[,c(3,4,5,6,7)]
  data<- data %>%
    arrange(trade_date)#对日期进行升序
  return(data)
}
#+++++++++数据++++++++++++++
spx <- get_data(load="D:/Study/python.program/data/SPX20000103-20131231.csv")

# 转成时序数据
open_xts <- xts(spx $open, order.by =spx $trade_date, frequency = 365)# 转换为时序ts数据，根据日期
close_xts <- xts(spx $close, order.by = spx $trade_date, frequency = 365)
high_xts <- xts(spx $high, order.by = spx $trade_date, frequency = 365)
low_xts <- xts(spx $low, order.by = spx $trade_date, frequency = 365)
# creates a combined xts object
spx_xts <- cbind(open_xts, close_xts, high_xts, low_xts)

# plots the indices
# 交互式绘制
#dygraph交互式时序绘图函数
library(dygraphs)
ts_graph = dygraph(spx_xts, ylab = 'spx Price', 
                   main = ' Prices of spx stocks') %>%
  dySeries('open_xts', label = 'open') %>%
  dySeries('close_xts', label = 'close') %>%
  dySeries('high_xts', label = 'high') %>%
  dySeries('low_xts', label = 'low') %>%
  dyOptions(colors = c('blue', 'orange', 'red', 'green'))

# save html to png
library(htmlwidgets)
saveWidget(ts_graph, "temp.html", selfcontained = FALSE)# 保存为网页版，setwd路径下

##==================================
####### 5、数据平稳性检验 #######
##==================================
library(forecast)
attach (spx)
Acf(open, lag.max = NULL, 
    type = c("correlation", "covariance", "partial"), plot = TRUE,
    na.action = na.contiguous, demean = TRUE)
Acf(diff(open), lag.max = NULL,
    type = c("correlation", "covariance","partial"), plot = TRUE,
    na.action = na.contiguous, demean = TRUE)
Pacf(diff(open))
#单位根检验进一步验证
library(aTSA)
adf.test(open, output = TRUE)# 拒绝原假设：非平稳
adf.test(diff(open), output = TRUE)

##==================================
####### 6、回测策略 #######
##==================================
library(recipes) 
library(rsample)
cv <- function(load,periods_train,periods_test,skip_span){
  rolling_origin_resamples <- rolling_origin(
    data=get_data(load=load),
    initial    = periods_train,
    assess     = periods_test,
    cumulative = FALSE,#允许平移起始点，确保较近期数据上的模型相较那些不太新近的数据没有不公平的优势（使用更多的观测数据）
    skip       = skip_span)
  return(rolling_origin_resamples)
}
#++++++++++++++回测样本(训练集、测试集)+++++++++++++++
rolling_origin_resamples <- cv(load="D:/Study/python.program/data/SPX20000103-20131231.csv",
                               periods_train=1400,periods_test=600,skip_span=300)
spx_split    <- rolling_origin_resamples$splits[[06]]
spx_split_id <- rolling_origin_resamples$id[[06]]

spx_trn <- training(spx_split)
spx_tst <- testing(spx_split)
#将所有交叉验证样本绘制图形方便观察
library(glue)
# Plotting function for a single split绘制单一split
plot_split <- function(split,
                       expand_y_axis = TRUE,
                       alpha = 1,
                       size = 1,
                       base_size = 14) {
  
  # Manipulate data操纵数据
  #获取训练集和测试集数据并进行合并，添加列等操作方便ggplot绘图,分为时间，值，key(训练集、测试集)
  train_tbl <- training(split) %>%#这里split为rolling_origin_resamples$splits[[i]]),i表示spilt 1-11;training函数返回split i 的训练值
    add_column(key = "training") 
  
  test_tbl  <- testing(split) %>%
    add_column(key = "testing") 
  
  data_manipulated <- bind_rows(
    train_tbl, test_tbl) %>%#行合并
    mutate(
      key = fct_relevel(
        key, "training", "testing"))#将字符型数据转换成Levels: training testing
  
  # Collect attributes
  train_time_summary <- train_tbl %>%
    tk_index() %>%#提取tibble型时间序列数据时间列
    tk_get_timeseries_summary()#Get date features from a time-series index；index列
  
  test_time_summary <- test_tbl %>%
    tk_index() %>%
    tk_get_timeseries_summary()
  
  # Visualize
  g <- data_manipulated %>%
    ggplot(
      aes(x = trade_date,
          y = open,
          color = key)) +#颜色按照key的类型分开
    geom_line(size = size, alpha = alpha) +
    theme_tq(base_size = base_size) +
    scale_color_tq() +
    labs(
      title    = glue("Split: {split$id}"),#添加主标题
      subtitle = glue(
        "{train_time_summary$start} to {test_time_summary$end}"),#添加副标题
      y = "", x = "") +
    theme(legend.position = "none") 
  
  if (expand_y_axis) {
    
    sun_spots_time_summary <- get_data(load="D:/Study/python.program/data/SPX20000103-20131231.csv") %>% 
      tk_index() %>% 
      tk_get_timeseries_summary()#获取spx的时间特征
    
    g <- g +
      scale_x_date(
        limits = c(
          get_data(load="D:/Study/python.program/data/SPX20000103-20131231.csv")$start,
          get_data(load="D:/Study/python.program/data/SPX20000103-20131231.csv")$end))
  }
  
  return(g)
}
#使用 purrr 和 cowplot 将 plot_split() 函数应用到所有样本上
# Plotting function that scales to all splits 
plot_sampling_plan <- function(sampling_tbl, 
                               expand_y_axis = TRUE, 
                               ncol = 3,
                               alpha = 1,
                               size = 1,
                               base_size = 14, 
                               title = "Sampling Plan") {
  
  # Map plot_split() to sampling_tbl
  sampling_tbl_with_plots <- sampling_tbl %>%
    mutate(
      gg_plots = map(
        splits, plot_split, 
        expand_y_axis = expand_y_axis,
        alpha = alpha,
        base_size = base_size))
  
  # Make plots with cowplot
  plot_list <- sampling_tbl_with_plots$gg_plots 
  
  p_temp <- plot_list[[1]] + theme(legend.position = "bottom")
  legend <- get_legend(p_temp)
  
  p_body  <- plot_grid(
    plotlist = plot_list, ncol = ncol)
  
  p_title <- ggdraw() + 
    draw_label(
      title,
      size = 18, 
      fontface = "bold",
      colour = palette_light()[[1]])
  
  g <- plot_grid(
    p_title,
    p_body,
    legend,
    ncol = 1,
    rel_heights = c(0.05, 1, 0.05))
  
  return(g)
}
# rolling_origin_resamples$splits[[1]] %>%
#   plot_split(expand_y_axis = TRUE) +
#   theme(legend.position = "bottom")

#+++++++++++++++++可视化整个回测图+++++++++++++++++++++++
plot_all_split <- function(load,periods_train,periods_test,skip_span){
  cv(load,periods_train,periods_test,skip_span) %>%
    plot_sampling_plan(
      expand_y_axis = T,
      ncol = 3, alpha = 1,
      size = 1, base_size = 10, 
      title = "Backtesting Strategy: Rolling Origin Sampling Plan")
}
plot_all_split_spx <- plot_all_split(load="D:/Study/python.program/data/SPX20000103-20131231.csv",
                                     periods_train=1400,periods_test=600,skip_span=300)
