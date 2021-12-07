#For HPC only
#install.packages(c("rvest", "dplyr", "tidyverse", "reshape2"), lib = "~/R/library")

#load the packages
library(rvest)
library(dplyr)
library(tidyverse)
library(reshape2)

#################1. Membership information##################
#(a) scraping
memb = data.frame()
memb_empty = 0

#131535
#scrape membership info
#WARNING: It will take approximately a whole day to finish running the whole script. 
#It is recommended to run the script in the High Performance Computing (HPC) system. 
#If you wish to test the script, please change the sequence number below from "131535" to a smaller number (e.g. 300)
for (number in seq(from = 1, to = 131535, by = 1)) {
  link = paste0("https://webb-site.com/dbpub/positions.asp?p=",  #looping so from 1 to 131535
                number)
  download.file(link, destfile = 'whatever.html')
  page = read_html('whatever.html')
  
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

#save the raw dataframe as csv file
write_csv(bio, "webb_memb_raw.csv")

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
write.csv(memb_full, "webb_memb.csv")