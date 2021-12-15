#For HPC only
#install.packages(c("lubridate", "dplyr", "tidyverse", "reshape2"), lib = "~/R/library")

#load package
library(tidyverse)
library(lubridate)
library(reshape2)
library(dplyr)
library(ggplot2)

#load the data
memb <- tibble(read.csv("webb_memb.csv"))


#####Extracting the start and end year of each membership

#extract the start year of each membership based on their start date

#load the function that can convert multiple formats into one
multidate <- function(data, formats){
  a<-list()
  for(i in 1:length(formats)){
    a[[i]]<- as.Date(data,format=formats[i])
    a[[1]][!is.na(a[[i]])]<-a[[i]][!is.na(a[[i]])]
  }
  a[[1]]
}

memb$start_year <- multidate(memb$start_date, 
                             c("%Y-%m-%d","%Y-%m","%Y")) %>% year()

#extract the end year of each membership based on their end date

#First, change all the "U" (i.e. uncertain) into blank
#memb$end_date[memb$end_date == "U"] <- ""

#do the same thing
memb$end_year <- multidate(memb$end_date, 
                           c("%Y-%m-%d","%Y-%m","%Y")) %>% year()

#for those memberships with a start year but without an end year (+ not "U" i.e. uncertain), I will set their end year as this year (2021). 

memb$end_year[is.na(memb$start_year) == FALSE %% is.na(memb$end_year) %% (memb$end_date != "U") %% (memb$start_year <= 2021)] <- 2021

#fill the NAs in start year or end year with the only year that exists in either column. 

memb$end_year[is.na(memb$end_year)] <- memb$start_year[is.na(memb$end_year)]
memb$start_year[is.na(memb$start_year)] <- memb$end_year[is.na(memb$start_year)]

#####Active membership graph

#remove all the NAs (those memberships that do not have a date)

memb_g <- memb[is.na(memb$end_year) == FALSE,]

#generate the a range of years that each membership covered
#e.g. from 2001 to 2004, it would be c(2001, 2002, 2003, 2004)
#WARNING: it will take about 10 mins to run this loop. 
#To skip this part you can just load the file "memb_year_graph.csv" as "custom")
custom <- c()
for (i in seq_along(along.with = memb_g$start_year))
{
  custom <- c(custom, memb_g$start_year[i]:memb_g$end_year[i])
  print(paste("done:", i))
}

#write_csv(as.data.frame(custom), "memb_year_graph.csv") #save the list

#plot the histogram, but it seems that the range is too large
hist(custom)

#so I limit the range to years after 1960
custom1960 <- custom[custom >= 1960]

#and plot it

options(scipen=999)#to avoid scientific notation

#this is exported as "memb_summary.pdf"
ggplot(as.data.frame(custom1960), aes(x = as.data.frame(custom1960)[,1])) + 
  geom_histogram(binwidth = 1,
                 color="black", 
                 fill="white") + 
  xlim(1960, 2021) + 
  labs(title = "Active Memberships of 'Who's Who' from 1960",
       x = "Year",
       y = "Number of Active Memberships",
       caption = "Source: Webb-site 'Who's Who' database") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 5, face = "bold"), #central + heighten + bold the title
        plot.margin = unit(c(1, 1, 1, 1), "cm"),#create margin for the graph
        axis.title.x = element_text(vjust= -1), #lower the label of x-axis
        axis.title.y = element_text(vjust= 8), #shift the label of y-axis to the left
        plot.caption = element_text(vjust= -3, face = "italic")) #lower the caption + change caption's font into italic

