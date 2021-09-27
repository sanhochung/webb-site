library(rvest)
library(dplyr)
library(tidyverse)
library(reshape2)

#################1. Membership information##################




#(a) scraping
memb = data.frame()
memb_empty = 0

#131535
#this is for membership info
for (number in seq(from = 1, to = 131535, by = 1)) {
  link = paste0("https://webb-site.com/dbpub/positions.asp?p=",  #looping so from 1 to 131535
                number)
  page = read_html(link)
  
  #check if the page is empty or not
  if (identical(tryCatch(page %>% html_nodes(".colHide2+ td , .total .colHide2 , .nowrap , td:nth-child(4) , td:nth-child(3)") %>% html_text(), 
                         error = function(e) e), character(0)) == FALSE){
    #If it exists then we will...
    #scrape the name
    name = page %>% html_nodes("h2") %>% html_text()
    
    #scrape the membership list with dates
    list = page %>% html_nodes(".colHide2+ td , .total .colHide2 , .nowrap , td:nth-child(4) , td:nth-child(3)") %>% html_text()
          
    memb = rbind(memb, data.frame(name, list,
                                    stringsAsFactors = FALSE))
      
    print(paste("Scraped:", number))}  
    
      else #if the profile is empty, then we will just count the empty profile
        print(paste("Profile ", number, " is empty."))
        memb_empty = memb_empty + 1
}






#(b) cleaning

#create information column for each entry
info <- rep(c("org_award","position","start_date", "end_date"), time = nrow(memb)/4)

#create group code for each entry
group <- list()
for(number in seq(from = 1, to = nrow(memb)/4, by = 1)) {
  group = c(group, number, number, number, number)

}

#merge all of them together
memb_merge <- data.frame(memb, info, as.character(group), stringsAsFactors = FALSE)

#rename the column
names(memb_merge)[names(memb_merge) == 'as.character.group.'] <- 'group'

#restructure the dataset so that 1 row = 1 membership record
memb_full <- dcast(memb_merge, group + name  ~ info, value.var = "list")
memb_full <- memb_full[c("group","name","org_award","position","start_date","end_date")]

#reorder the rows based on "group"
memb_full$group <- as.numeric(memb_full$group)
memb_full %>% arrange(as.numeric(group))
rownames(memb_full) <- memb_full$group


#turn all the blanks in "org_award" into the cell(organization name) above
empty_org = 0
memb_full$org_award[memb_full$org_award == ""] <- NA

for (i in seq(from = 1, to = nrow(memb_full), by = 1)) {
  
  if (is.na(memb_full[memb_full$group ==  i, 3]) == TRUE) {
    
    #since some of the group has been removed
    memb_full[memb_full$group ==  i, 3] <- memb_full[memb_full$group ==  (i-1), 3]
    empty_org = empty_org + 1
    
    print(paste("detected", empty_org, "empty cells"))
  }
    else
      print("nope")
}

#remove all the "average" in "org_award"
memb_full <- memb_full[memb_full$org_award != "Average",] 
rownames(memb_full) <- as.numeric(rownames(memb_full))

#remove column "group"
memb_full <- memb_full[c("name","org_award","position","start_date","end_date")]

#reordered based on name
memb_full <- memb_full[order(memb_full$name),]

#recreate row index
rownames(memb_full) <- 1:nrow(memb_full)

#export the list
write.csv(memb_full, "webb_orgs_awards_20210924.csv")













#################2. biographical (sex, birth and death) information################
bio = data.frame()
bio_empty = 0

for (number in seq(from = 1, to = 131535, by = 1)) {
  bio_link = paste0("https://webb-site.com/dbpub/natperson.asp?p=", 
                    number)
  bio_page = read_html(bio_link)
  
  #If the profile exists start scraping the name and the following checks...
  if (identical(tryCatch(bio_page %>% html_nodes("h2") %>% html_text(), 
                           error = function(e) e), character(0)) == FALSE){
  
    #scrape the name
    name = bio_page %>% html_nodes("h2") %>% html_text()
  
    #check if gender existed or not
    if (identical(tryCatch(bio_page %>% html_nodes(".opltable td") %>% html_text(), 
                         error = function(e) e), character(0)) == FALSE){
      bio_info = bio_page %>% html_nodes(".opltable td") %>% html_text()}
      else{bio_info <- NA}
    
    #bind them together
    bio = rbind(bio, data.frame(rep(name, time = length(bio_info)), bio_info,
                                stringsAsFactors = FALSE))
      
    
    print(paste("Scraped:", number))}
  
  else { 
    print(paste("Profile ", number, " is empty."))
    bio_empty = bio_empty + 1
  }
}

#################3. relatives (non-lineal, ascendants and descendants) information################
rela = data.frame()
rela_empty = 0


for (number in seq(from = 2, to = 2, by = 1)) {
  rela_link = paste0("https://webb-site.com/dbpub/natperson.asp?p=", 
                    number)
  rela_page = read_html(rela_link)
  
  #If the profile exists start scraping the name and the following checks...
  if (identical(tryCatch(bio_page %>% html_nodes("h2") %>% html_text(), 
                         error = function(e) e), character(0)) == FALSE){
    
    #scrape the name
    name = bio_page %>% html_nodes("h2") %>% html_text()

    #check if he/she has non-lineal relatives
    if (identical(tryCatch(bio_page %>% html_nodes("h4+ .txtable td") %>% html_text(), 
                         error = function(e) e), character(0)) == FALSE) {
      
      non_lin = bio_page %>% html_nodes("h4+ .txtable td") %>% html_text()
      non_lin_info = rep("non_lin", time = length(non_lin))
      non_lin_full = data.frame(non_lin, non_lin_info)
      
    }
      else {non_lin_full <- NA}
    
    #check if he/she has ascendants
    if (identical(tryCatch(bio_page %>% html_nodes("table:nth-child(20) td") %>% html_text(), 
                         error = function(e) e), character(0)) == FALSE) {
      ascen = bio_page %>% html_nodes("table:nth-child(20) td") %>% html_text()
      ascen_info = rep("ascen", time = length(ascen))
      ascen_full = data.frame(ascen, ascen_info)
    }
      else {ascen_full <- NA}
  
    #check if he/she has descendants
    if (identical(tryCatch(bio_page %>% html_nodes("table:nth-child(22) td+ td") %>% html_text(), 
                         error = function(e) e), character(0)) == FALSE){
      descen = bio_page %>% html_nodes("table:nth-child(22) td+ td") %>% html_text()
      descen_info = rep("non_lin", time = length(descen))
      descen_full = data.frame(descen, descen_info)
    }
      else{descen_full <- NA}
    
    #bind them together
    rela_part = merge(non_lin_full , ascen_full, descen_full)
  
    rela = rbind(rela, data.frame(rep(name, time = rnow(rela_part), rela_part),
                                stringsAsFactors = FALSE))
    
    
    print(paste("Scraped:", number))}
  
  else { 
    print(paste("Profile ", number, " is empty."))
    rela_empty = rela_empty + 1
  }
}

