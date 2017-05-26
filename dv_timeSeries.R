# install packages
pkgs.needs <- c("ggplot2", "xts", "dygraphs", "stringr", "XML", "dplyr")
pkgs.installed <- installed.packages()[,"Package"] 
new.pkgs <- pkgs.needs[!(pkgs.needs %in% pkgs.installed)]
if(length(new.packages)) install.packages(new.pkgs)                         
library(ggplot2)  # plot 
library(xts)      # time data 
library(dygraphs) # dynamic graphs 
library(stringr)  # strsplit
library(XML)      # readHTMLTable
library(dplyr)    # data manipulation & pipe line

# define functions
## get Taiwan market monthly data (Stock Exchange Market only)
getMartData <- function(yyyy, mm){
  mm <- mm %>% str_pad(2, pad = "0") %>% as.character
  url <- paste0("http://www.twse.com.tw/exchangeReport/FMTQIK?response=html&date=",yyyy,mm,"01")
  tb = url %>% readHTMLTable %>% .[[1]] 
  return(tb)
}

df <- data.frame(日期= character(0), 成交股數= integer(0), 成交金額= integer(0), 成交筆數= integer(0), 發行量加權股價指數= integer(0), 漲跌點數= character(0))

## progressBar
pb <- txtProgressBar(2010, 2015, style=3)

## get data 
for(i in c(2010:2015)){
  yyyy <- i
  for(j in c(1:12)){
    mm <- j
    data <- getMartData(yyyy, mm)
    df <- rbind(df,data)
  }
  setTxtProgressBar(pb, i)
}

## trans form date format
y1 <- seq(1,105,1)
y2 <- seq(1912,2016,1)
y <- cbind(y1,y2) %>% data.frame
names(y) <- c("民國","西元")
df_date <- as.character(df$日期)
numt <- length(df_date)
for (i in 1:numt) {
  d = as.character(df_date[i]) %>% strsplit("/") %>% unlist
  yp <- y[which(y$民國 == str_trim(d[1])), ] 
  d[1] <- yp$西元                               
  df_date[i] <- paste(d[1],d[2],d[3],sep = "/")
}
df <- cbind(df_date,df)
names(df) <- c("日期_西元","日期_民國","成交股數","成交金額","成交筆數","發行量加權股價指數","漲跌點數")
df$成交金額 <- as.numeric(gsub(",","", df$成交金額))
mart = df %>% transform(成交金額_億 = 成交金額/100000000)
mart_xts <- xts(mart$成交金額_億,order.by=as.Date(mart$日期_西元),frequency=365)

## graphing
dygraph(mart_xts, main="Taiwan Stock Vol.") %>%
  dySeries(label="Stock Vol", color="black") %>%
  
  dyShading(from="2010-1-1", to="2010-4-30", color="#99FFFF") %>%
  dyShading(from="2010-5-1", to="2010-7-31", color="#33FFFF") %>%
  dyShading(from="2010-8-1", to="2010-12-31", color="#99FFFF") %>%
  
  dyShading(from="2011-1-1", to="2011-4-30", color="#99FF99") %>%
  dyShading(from="2011-5-1", to="2011-7-31", color="#33FF33") %>%
  dyShading(from="2011-8-1", to="2011-12-31", color="#99FF99") %>%
  
  dyShading(from="2012-1-1", to="2012-4-30", color="#99FFFF") %>%
  dyShading(from="2012-5-1", to="2012-7-31", color="#33FFFF") %>%
  dyShading(from="2012-8-1", to="2012-12-31", color="#99FFFF") %>%
  
  dyShading(from="2013-1-1", to="2013-4-30", color="#99FF99") %>%
  dyShading(from="2013-5-1", to="2013-7-31", color="#33FF33") %>%
  dyShading(from="2013-8-1", to="2013-12-31", color="#99FF99") %>%
  
  dyShading(from="2014-1-1", to="2014-4-30", color="#99FFFF") %>%
  dyShading(from="2014-5-1", to="2014-7-31", color="#33FFFF") %>%
  dyShading(from="2014-8-1", to="2014-12-31", color="#99FFFF") %>%
  
  dyShading(from="2015-1-1", to="2015-4-30", color="#99FF99") %>%
  dyShading(from="2015-5-1", to="2015-7-31", color="#33FF33") %>%
  dyShading(from="2015-8-1", to="2015-12-31", color="#99FF99") %>%
  dyRangeSelector()