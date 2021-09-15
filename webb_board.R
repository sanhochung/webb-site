library(rvest)
library(dplyr)
library(tidyverse)

memb = data.frame()
memb_empty = 0

#131535
#this is for membership info
for (number in seq(from = 1, to = 131535, by = 1)) {
  link = paste0("https://webb-site.com/dbpub/positions.asp?p=", 
                number)
  page = read_html(link)
  
  #check if the page existed or not
  if (identical(tryCatch(page %>% html_nodes(".colHide2+ td , .total .colHide2 , .nowrap , td:nth-child(4) , td:nth-child(3)") %>% html_text(), 
                         error = function(e) e), character(0)) == FALSE){

    name = page %>% html_nodes("h2") %>% html_text()
    
    list = bio_page %>% html_nodes(".colHide2+ td , .total .colHide2 , .nowrap , td:nth-child(4) , td:nth-child(3)") %>% html_text()
          
    memb = rbind(memb, data.frame(name, list,
                                    stringsAsFactors = FALSE))
      
    print(paste("Scraped:", number))}  
    
      else 
        print(paste("Profile ", number, " is empty."))
        emeb_empty = memb_empty + 1
}


info <- rep(c("org_award","position","start_date", "end_date"), time = nrow(memb)/4)

group <- list()
for(number in seq(from = 1, to = nrow(memb)/4, by = 1)) {
  group = c(group, number, number, number, number)

}
memb_full <- data.frame(memb, info, group, stringsAsFactors = FALSE)



#This is for bio info
bio = data.frame()
bio_empty = 0

for (number in seq(from = 1, to = 131535, by = 1)) {
  bio_link = paste0("https://webb-site.com/dbpub/natperson.asp?p=", 
                    number)
  bio_page = read_html(bio_link)
  
  #scrape the name
  name = bio_page %>% html_nodes("h2") %>% html_text()
  
  #check if the page existed or not
  if (identical(tryCatch(page %>% html_nodes(".colHide2+ td , .total .colHide2 , .nowrap , td:nth-child(4) , td:nth-child(3)") %>% html_text(), 
                         error = function(e) e), character(0)) == FALSE){
  
  sex = bio_page %>% html_nodes("tr:nth-child(1) td+ td") %>% html_text()
  
  #check if birth date data is available
  if (identical(tryCatch(bio_page %>% html_nodes(".opltable tr:nth-child(2) td+ td") %>% html_text(), 
                         error = function(e) e), character(0)) == FALSE){
    birth = bio_page %>% html_nodes(".opltable tr:nth-child(2) td+ td") %>% html_text()}
    else{birth = NA}
  
  #check if he/she is dead
  if (identical(tryCatch(bio_page %>% html_nodes("tr:nth-child(4) td+ td") %>% html_text(), 
                         error = function(e) e), character(0)) == FALSE){
    death = bio_page %>% html_nodes("tr:nth-child(4) td+ td") %>% html_text()}
    else{death = NA}
  
  #check if he/she has non-lineal relatives
  if (identical(tryCatch(bio_page %>% html_nodes(".txtable td") %>% html_text(), 
                         error = function(e) e), character(0)) == FALSE){
    relatives = bio_page %>% html_nodes(".txtable td") %>% html_text()}
    else{relatives = NA}
  
  #check if he/she has descendants
  if (identical(tryCatch(bio_page %>% html_nodes("h4+ table tr+ tr td+ td") %>% html_text(), 
                         error = function(e) e), character(0)) == FALSE){
    descendants = bio_page %>% html_nodes("h4+ table tr+ tr td+ td") %>% html_text()}
    else{descendants = NA}
  
  bio = rbind(bio, data.frame(name, sex, birth, death, relatives, descendants,
                              stringsAsFactors = FALSE))
  
  print(paste("Scraped:", number))}  
  
  else 
    print(paste("Profile ", number, " is empty."))
    bio_empty = bio_empty + 1

}