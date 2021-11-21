library(jsonlite)
library(rvest)
library(pbapply)
library(data.table)
library(tidyverse)

start_page=1L
end_page=7L
one_page=1L

link_from_krishakz="https://krisha.kz/prodazha/kvartiry/almaty/?_txt_=%D1%82%D0%B0%D1%83%D0%BD%D1%85%D0%B0%D1%83%D1%81&page="
preview_html="div.a-card__text-preview"
price_html="div.a-card__price"
target_html="a.a-card__title"
address_html="div.a-card__subtitle"

author="Nurly Kuzdikbay"

krisha_townhouse=data.frame()

for (page_result in seq(from=start_page, to=end_page, by=one_page)) {
  link=paste0(link_from_krishakz,page_result,"")
  krisha=read_html(link)
  preview=krisha %>% 
    html_nodes(preview_html) %>% 
    html_text2()
  price<-krisha %>% 
    html_nodes(price_html) %>% 
    html_text2()
  target<-krisha %>% 
    html_nodes(target_html) %>% 
    html_text2()
  address<-krisha %>% 
    html_nodes(address_html) %>% 
    html_text2()
  krisha_townhouse<-rbind(krisha_townhouse,data.frame(preview,price,target,address,stringsAsFactors = FALSE)) 
  print(paste("Page:",page_result))
}

krisha1<-krisha_townhouse %>% 
  separate(target,into=c('rooms','area'),sep=', ') %>% 
  separate(address,into=c('region','address1','address2'),sep=',') %>% 
  unite('address',address1,address2,sep = ' ') %>% 
  separate(preview,into=c('Name','floors','year_of_explotation'),sep=',') 

#download our raw result for further edits in Excel
write.csv(krisha1,"\\Users\\dias\\Desktop\\Krisha_townhouse.csv", row.names = FALSE)

Townhouses_Krisha1 <- read_csv("Downloads/Townhouses - Krisha1.csv")

Townhouses_Krisha1 %>% 
  rename("price_per_area_m2"="price/area") %>% 
  group_by(year_of_explotation,region) %>% 
  summarise(mean_price=mean(price_per_area_m2)) %>% 
  ggplot()+
  geom_col()+aes(x=year_of_explotation,y=mean_price)+
  labs(title = "Year vs price per m2",subtitle = "Classified by regions",caption=author)+
  xlab(label="Year")+
  facet_wrap(~region)+
  ylab(label="Price per Area m2")+
  theme(axis.text.x = element_text(angle = 90))

Townhouses_Krisha1 %>% 
  rename("price_per_area_m2"="price/area") %>% 
  group_by(region) %>% 
  summarise(mean_price=mean(price_per_area_m2)) %>% 
  ggplot()+
  geom_col(aes(x=region,y=mean_price))+
  labs(title = "Region vs price per m2",caption=author)+
  xlab(label="Region")+
  ylab(label="Price per Area m2")+
  theme(axis.text.x = element_text(angle = 90))

