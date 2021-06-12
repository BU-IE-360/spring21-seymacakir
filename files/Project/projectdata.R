# kullandýðýnýz paketleri ekleyebilirsiniz ben bunlarý kullandýðým için bunlar var 
require(jsonlite)
require(httr)
require(data.table)
library(lubridate)
library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)
library(tidyr)
library(tidyverse)
library(scales)
library(ggcorrplot)
library(forecast)
library(urca)
library(zoo)
library(reshape)
library(GGally)
library(PerformanceAnalytics)

# Hocanýn datayý almak için yazdýðý kod buralarý deðiþtirmeyin


get_token <- function(username, password, url_site){
  
  post_body = list(username=username,password=password)
  post_url_string = paste0(url_site,'/token/')
  result = POST(post_url_string, body = post_body)
  
  # error handling (wrong credentials)
  if(result$status_code==400){
    print('Check your credentials')
    return(0)
  }
  else if (result$status_code==201){
    output = content(result)
    token = output$key
  }
  
  return(token)
}

get_data <- function(start_date='2020-03-20', token, url_site){
  
  post_body = list(start_date=start_date,username=username,password=password)
  post_url_string = paste0(url_site,'/dataset/')
  
  header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
  result = GET(post_url_string, header, body = post_body)
  output = content(result)
  data = data.table::rbindlist(output)
  data[,event_date:=as.Date(event_date)]
  data = data[order(product_content_id,event_date)]
  return(data)
}


send_submission <- function(predictions, token, url_site, submit_now=F){
  
  format_check=check_format(predictions)
  if(!format_check){
    return(FALSE)
  }
  
  post_string="list("
  for(i in 1:nrow(predictions)){
    post_string=sprintf("%s'%s'=%s",post_string,predictions$product_content_id[i],predictions$forecast[i])
    if(i<nrow(predictions)){
      post_string=sprintf("%s,",post_string)
    } else {
      post_string=sprintf("%s)",post_string)
    }
  }
  
  submission = eval(parse(text=post_string))
  json_body = jsonlite::toJSON(submission, auto_unbox = TRUE)
  submission=list(submission=json_body)
  
  print(submission)
  # {"31515569":2.4,"32737302":2.4,"32939029":2.4,"4066298":2.4,"48740784":2.4,"6676673":2.4, "7061886":2.4, "73318567":2.4, "85004":2.4} 
  
  if(!submit_now){
    print("You did not submit.")
    return(FALSE)      
  }
  
  
  header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
  post_url_string = paste0(url_site,'/submission/')
  result = POST(post_url_string, header, body=submission)
  
  if (result$status_code==201){
    print("Successfully submitted. Below you can see the details of your submission")
  } else {
    print("Could not submit. Please check the error message below, contact the assistant if needed.")
  }
  
  print(content(result))
  
}

check_format <- function(predictions){
  
  if(is.data.frame(predictions) | is.data.frame(predictions)){
    if(all(c('product_content_id','forecast') %in% names(predictions))){
      if(is.numeric(predictions$forecast)){
        print("Format OK")
        return(TRUE)
      } else {
        print("forecast information is not numeric")
        return(FALSE)                
      }
    } else {
      print("Wrong column names. Please provide 'product_content_id' and 'forecast' columns")
      return(FALSE)
    }
    
  } else {
    print("Wrong format. Please provide data.frame or data.table object")
    return(FALSE)
  }
  
}

# this part is main code
subm_url = 'http://46.101.163.177'

u_name = "Group3"
p_word = "LaQjwxkIGSGhBrRj"
submit_now = TRUE

username = u_name
password = p_word

token = get_token(username=u_name, password=p_word, url=subm_url)
data = get_data(token=token,url=subm_url)

predictions=unique(data[,list(product_content_id)])
predictions[,forecast:=2.3]


# Predictioný her çalýþtýrmada göndermesin diye burayý yoruma aldým, gönderceðimiz zaman en son yukarýya yazýp burayý çalýþtýrcaz 
#send_submission(predictions, token, url=subm_url, submit_now=T)



# Rawdata ile güncel datayý burda birleþtirdim en son rawdata 2020-0-05 ten güncel güne kadar sýralanmýþ bir þekilde olmalý 
# read csv kodunun içini kendi file pathiniz olarak yazmanýz lazým csv okumadýysanýz excel olarak deðiþtirebilirsiniz 
# belki sorun olabilir hocanýn moodle a koyduðu datayý inidirebilirsiniz yeniden 

rawdata <- read.csv("C:/Users/seyma/OneDrive/Belgeler/GitHub/spring21-seymacakir/files/project/ProjectRawData.csv", header = TRUE,sep = ",")
rawdata <- as.data.table(rawdata)
colnames <- c( "price","event_date","product_content_id","sold_count" , "visit_count" , "favored_count" , "basket_count","category_sold",     "category_brand_sold", "category_visits"  ,   "ty_visits"  ,         "category_basket",    "category_favored" )
rawdata <- rawdata[,..colnames]
rawdata <- rawdata[!is.na(sold_count)]
rawdata$event_date <- as.Date(rawdata$event_date, format = "%Y-%m-%d")
rawdata$product_content_id <- as.factor(rawdata$product_content_id)
data$product_content_id <- as.factor(data$product_content_id)

rawdata <- rbind(rawdata,data)

rawdata %>% distinct(event_date, product_content_id, .keep_all = TRUE)
rawdata <- rawdata %>% arrange(event_date)

# Hafta ve Ayý ekledim 

rawdata[,w_day:= wday(event_date)]
rawdata[,mon:= month(event_date)]


# productlarý birden dokuza kadar listenin içinde tuttum products[[1]] rawdatanýn product 1 için olan tüm kýsýmlarýný içeriyor olucak 


product_id <- unique(rawdata$product_content_id)
products <- list()
for ( i in 1:length(product_id)){
  products[[i]] <- as.data.table(rawdata[product_content_id == product_content_id[i]])
}


# burada her bir product için kullanýþlý olabilecek grafikleri çizdirdim, datada herhangi bir düzenleme yapmadan 


for(i in 1:length(products)){
  
  
  g1 <-ggplot(products[[i]], aes( x = event_date, y = sold_count)) + geom_line() + ggtitle(paste("product",i)) +theme_minimal()  
  g2 <-ggplot(products[[i]], aes( y = sold_count, fill= factor(w_day))) + geom_boxplot() + 
    ggtitle(paste("product",i))+ theme_minimal() 
  g3 <-ggplot(products[[i]], aes( y = sold_count, fill= factor(mon))) + geom_boxplot()+
    ggtitle(paste("product",i))+ theme_minimal() 
  g4 <- ggplot(products[[i]], aes( x = sold_count)) + geom_histogram() + ggtitle(paste("product",i)) + theme_minimal()+
    facet_wrap(~factor(w_day))
  g5 <-ggplot(products[[i]], aes( x = sold_count )) + geom_histogram() + theme_minimal()+ ggtitle(paste("product",i))
  facet_wrap(~factor(mon))
  
  
  print(g1)
  print(g2)
  print(g3)
  print(g4)
  print(g5)
  ACF <- acf( products[[i]]$sold_count, plot = FALSE)
  plot(ACF, main = paste("ACF OF Product",i))
  PACF <- pacf( products[[i]]$sold_count, plot = FALSE)
  plot(PACF, main = paste("PACF OF Product",i))
  
}




# Bundan sonrasýnda kendi rawdatanýzý nasýl manipüle ettiyseniz öyle devam edebilirsiniz 
# rawdata küçükten büyüðe doðru sýralandýðý için rawdata[x:y] gibi bir seçim yaptýysanýz sorun yaþayabilirsiniz 
# ek olarak w_day ve mon columnu ekledim bu da w_day haftanýn gününü mon ayý temsil ediyor bunlarý kullanabilirsiniz kendi kodunuzda sýkýntý yaratýyorsa çýkarabilirsiniz de 
# aþaðýdaki kodu çalýþtýmanýz lazým 
# rawdata  <- rawdata[,1:13]
# bundan sonrasýnda kendi kodunuzda rawdata olarak adlandýrmadýysanýz kendi verdiðiniz adý koyarsanýz eski kodunuz sorunsuz çalýþýr sanýrým 
# product datanýzý da þöyle yapabilirsizniz

# kendikodunuzdakiproductismi <- products[[i]][,1:13] burdaki i attýðým sýralamada hangisine denk geliyorsa o oluyor benim çalýþtýklarý 7,8,9 du 










