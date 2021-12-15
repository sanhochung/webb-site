#For HPC only
#install.packages(c("rvest", "dplyr", "tidyverse", "reshape2"), lib = "~/R/library")

#load packages
library(rvest)
library(dplyr)
library(tidyverse)
library(reshape2)
library(naniar)

#################2. biographical (sex, birth and death) information################
bio = data.frame()
bio_empty = 0

#(a) scraping

#WARNING: It will take approximately a whole day to finish running the whole script. 
#It is recommended to run the script in the High Performance Computing (HPC) system. 
#If you wish to test the script, please change the sequence number below from "131535" to a smaller number (e.g. 300)

#scrape the bio info
for (number in seq(from = 1, to = 131535, by = 1)) {
  bio_link = paste0("https://webb-site.com/dbpub/natperson.asp?p=", 
                    number)
  options(timeout= 400)
  download.file(bio_link, destfile = 'whatever.html')
  bio_page = read_html('whatever.html')
  
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
    
    #record in the console that one profile is scrapped
    print(paste("Scrapped:", number, name))}
  
  #if the profile is empty, then we will just count the empty profile
  else { 
    #Tell the console that this profile is empty
    print(paste("Profile", number, "is empty."))
    
    #count the empty profiles
    bio_empty = bio_empty + 1
  }
}

#save the raw dataframe as csv file
write_csv(bio, "webb_bio_raw.csv")

#(b) cleaning


#categorize each entry
info_bio <- rep(c("var","value"), time = nrow(bio)/2)

#give number to each entry
num_bio <- list()
for(number in seq(from = 1, to = nrow(bio)/2, by = 1)) {
  num_bio = c(num_bio, number, number)
  print(number)
}

#reorganize each entry
bio_merge <- tibble(bio, info_bio, num_bio)
bio_merge <- rename(bio_merge, 
                    name = rep.name..time...length.bio_info.., 
                    data = bio_info, 
                    type = info_bio, 
                    num = num_bio)
bio_full <-bio_merge %>% pivot_wider(names_from = type, 
                                     values_from = data)

#remove ":"
bio_full <- bio_full %>% mutate_if(is.character, 
                                   str_replace_all, 
                                   pattern = ":", 
                                   replace = "")

#filter out "HKID" and "SFC ID" entries
bio_full <- filter(bio_full, var != "HKID")


#delete column "number"
bio_full1 <- bio_full[, c("name","var","value"), drop = TRUE]

#de-duplicate repeated items
bio_full1 <- distinct(bio_full1)

#convert "var" into columns
bio_full2 <- bio_full1 %>% pivot_wider(names_from = var,
                                     values_from = value)

#save the cleaned file
write_csv(bio_full2,"webb_bio.csv")
