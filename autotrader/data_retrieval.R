library(rvest)
library(tidyverse)

#pulls ~ 16 per second. 39000 should take 45 mins

df_base<-data.frame(model=character(),year=character(),miles=character(),
                    gearshift=character(),engine_size=character(),location=character(),
                    location=character())

rooturl<-'https://www.autotrader.co.uk/car-search?sort=sponsored&radius=1500&exclude-writeoff-categories=on&onesearchad=Used&postcode=cv136bp&year-from='
yearto<-"&year-to="
pageurl<-"&page="

for (i in 1990:2020){
  for(j in 1:1){
  tryCatch({
  url_string<-paste(rooturl,i,yearto,i,pageurl,j,sep='')
  tpage<-read_html(url_string)
  df1<-data.frame(model=html_text(html_nodes(tpage,'.listing-title.title-wrap')),
    year=html_text(html_nodes(tpage,'.listing-key-specs li:nth-child(1)')),
    miles=html_text(html_nodes(tpage,'.listing-key-specs li:nth-child(3)')),
    gearshift=html_text(html_nodes(tpage,'.listing-key-specs li:nth-child(4)')),
    engine_size=html_text(html_nodes(tpage,'.listing-key-specs li:nth-child(5)')),
    location=html_text(html_nodes(tpage,'.seller-location')),
    price=html_text(html_nodes(tpage,'.vehicle-price'))
  )
  df_base<-rbind(df_base,df1)
  },error=function(e){})
  }
}

df_base %>%
  filter(!grepl("*miles away*", location)) %>%
  filter(grepl("*HP", engine_size)) %>%
  mutate(price = gsub("\\£", "", price),
         price = gsub(",", "", price),
         price = as.numeric(price),
         miles = gsub(" miles", "", miles),
         miles = gsub(",", "", miles),
         miles = as.numeric(miles),
         year = gsub(" \\(.*","",year),
         year = as.numeric(year),
         engine_size = gsub("BHP", "", engine_size),
         engine_size = gsub("HP", "", engine_size),
         engine_size = as.numeric(engine_size)) %>%
  rename(engine_size_bhp = engine_size,
         price_gbp = price) %>%
  separate(col = location, into = c("location", "distance"), sep = " - ") %>%
  select(-distance) %>%
  distinct() %>%
  janitor::clean_names() %>%
  write_csv("autotrader_posts.csv")

###cleaning up
rm(df_base,tpage,df1,endtime,i,j,pageurl,rooturl,starttime,url_string,yearto)