#For HPC only
#install.packages(c("lubridate", "dplyr", "tidyverse", "reshape2"), lib = "~/R/library")

#load package
library(tidyverse)
library(lubridate)
library(reshape2)
library(dplyr)

###Data preparation#######

#load the data
bio <- tibble(read.csv("webb_bio.csv"))

#remove those profiles that cannot tell their ages
bio1 <- bio[is.na(bio$Estimated.age) == FALSE,]

#only keep useful columns: Gender, birthday and death-day
bio_g <- bio1[c("Gender","Estimated.date.of.birth","Estimated.date.of.death")]

#rename the columns
bio_g <- rename(bio_g,
                gender = Gender,
                bday = Estimated.date.of.birth,
                dday = Estimated.date.of.death)

#function that can convert multiple formats into one
multidate <- function(data, formats){
  a<-list()
  for(i in 1:length(formats)){
    a[[i]]<- as.Date(data,format=formats[i])
    a[[1]][!is.na(a[[i]])]<-a[[i]][!is.na(a[[i]])]
  }
  a[[1]]
}



###converting bday into byear######

#Since "%B" cannot be used without "%d", we add a fake day for all the "%B-%Y" so that they become "&d-%B-%Y"
#ignore the warnings btw
for (i in seq(from = 1, to = nrow(bio_g), by = 1)) {
  if (substr(bio_g$bday[i],1,1) %>% as.numeric %>% is.na) { #if the first letter of bday is not numeric
    bio_g$bday[i] <- paste0("11-", bio_g$bday[i]) #we will add "11-" at the beginning of it
    print(paste0("corrected:",i)) 
  }
    else {
      print(paste0("clear:",i))
  }
}

#We also add fake day + month for "%Y" anly. 
#again ignore the warnings
for (i in seq(from = 1, to = nrow(bio_g), by = 1)) {
  if (substr(bio_g$bday[i],1,4) %>% as.numeric %>% is.na == FALSE) { #if the first 4 letters of bday is numeric (aka year)
    bio_g$bday[i] <- paste0("11-Jan-", bio_g$bday[i]) #we will add "11-Jan-" at the beginning of it
    print(paste0("corrected:",i)) 
  }
  else {
    print(paste0("clear:",i))
  }
}

#now we can covert all "bday"s into "byear"s
bio_g$byear <- multidate(bio_g$bday,
                             c("%d-%b-%Y", "%d-%B-%Y")) %>% year()


###converting dday into dyear######(basically same as bday -> byear)

#Since "%B" cannot be used without "%d", we add a fake day for all the "%B-%Y" so that they become "&d-%B-%Y"
#ignore the warnings btw
for (i in seq(from = 1, to = nrow(bio_g), by = 1)) {
  if (substr(bio_g$dday[i],1,1) %>% as.numeric %>% is.na) { #if the first letter of dday is not numeric
    bio_g$dday[i] <- paste0("11-", bio_g$dday[i]) #we will add "11-" at the beginning of it
    print(paste0("corrected:",i)) 
  }
  else {
    print(paste0("clear:",i))
  }
}

#We also add fake day + month for "%Y" anly. 
#again ignore the warnings
for (i in seq(from = 1, to = nrow(bio_g), by = 1)) {
  if (substr(bio_g$dday[i],1,4) %>% as.numeric %>% is.na == FALSE) { #if the first 4 letters of bday is numeric (aka year)
    bio_g$dday[i] <- paste0("11-Jan-", bio_g$dday[i]) #we will add "11-Jan-" at the beginning of it
    print(paste0("corrected:",i)) 
  }
  else {
    print(paste0("clear:",i))
  }
}

#now we can covert all "bday"s into "byear"s
bio_g$dyear <- multidate(bio_g$dday,
                         c("%d-%b-%Y", "%d-%B-%Y")) %>% year()

######subsetting the data##########

#We only need those people since 20th century
bio_g1 <- bio_g[bio_g$byear >= 1900,]

#assume those with no death year are alive, we put 2021 on them
for (i in 1:nrow(bio_g1)) {
 if (bio_g1$dyear[i] %>% is.na()) {
   bio_g1$dyear[i] <- 2021
}
}

#divide them into male and female
bio_m <- bio_g1[bio_g1$gender == "M",][c("byear","dyear")] #male only, and we only keep byear and dyear
bio_f <- bio_g1[bio_g1$gender == "F",][c("byear","dyear")] #female only, and we only keep byear and dyear

#generate the a range of years that each profile covered
#e.g. from 2001 to 2004, it would be c(2001, 2002, 2003, 2004)
#WARNING: it will take about 5 mins to run thses loop. 
#To skip this part you can just load the file "bio_m_graph.csv" and "bio_f_graph.csv" as "male" and "female" respectively)

#do the male first
male <- c()
for (i in seq_along(along.with = bio_m$byear))
{
  male <- c(male, bio_m$byear[i]:bio_m$dyear[i])
  print(paste("done:", i))
}

#write_csv(as.data.frame(male), "bio_m_graph.csv") #save the list

#and then female
female <- c()
for (i in seq_along(along.with = bio_f$byear))
{
  female <- c(female, bio_f$byear[i]:bio_f$dyear[i])
  print(paste("done:", i))
}
#write_csv(as.data.frame(female), "bio_f_graph.csv")  #save the list

#Now we need to reconstruct the dataframe with column "gender" and "ayear" (ayear = alive/active year)
#Each entry represent one profile being active in that particular year

gender_m <- rep('M', times = length(male))  #create a list with full of M (i.e. "male")
bio_gm <- tibble(gender_m, male) #merge them together as a male active year dataframe "bio_gm"
bio_gm <- rename(bio_gm,
                 gender = gender_m,
                 year = male) #rename the columns


gender_f <- rep('F', times = length(female)) #create a list with full of F (i.e. "female")
bio_gf <- tibble(gender_f, female) #merge them together as a female active year dataframe "bio_gf"
bio_gf <- rename(bio_gf,
                 gender = gender_f,
                 year = female) #rename the columns

bio_plot <- rbind(bio_gm, bio_gf) #merge "bio_gm" and "bio_gf" together

#########plotting graph#########

options(scipen=999)#to avoid scientific notation

#this is exported as "bio_summary.pdf"
ggplot(bio_plot, aes(x = year)) +
  geom_histogram(bio_plot[bio_plot$gender == "M",], #this is for male
                  mapping = aes(fill = "blue", #color of interior
                     color = "blue"), #color of line
                 binwidth = 1, #scale of bin: 1 = normal
                 alpha = 0.1, #transparency
                 position="identity") + #I don't know what "identity" mean honestly...^_^
  geom_histogram(bio_plot[bio_plot$gender == "F",], # this is for female, this comes later because it has to be on top
                 mapping = aes(fill = "red", 
                     color = "red"),
                 binwidth = 1,
                 alpha = 0.1,
                 position="identity") +
  scale_fill_manual(name = "Gender",
                    values = c("blue", "red"),
                    labels = c("Male", "Female")) +
  scale_color_manual(name = "Gender",
                    values = c("blue", "red"),
                    labels = c("Male", "Female")) + #these are for legends
  xlim(1900, 2021) + #set the limit of x-scale
  labs(title = "Active (Alive) Profiles of 'Who's Who' from 1900", #heading of the graph
       x = "Year", #name of x-axis
       y = "Number of Active Profiles", #name of y-axis
       caption = "Source: Webb-site 'Who's Who' database") + #note on bottom-left
  theme(plot.title = element_text(hjust = 0.5, vjust = 5, face = "bold"), #central + heighten + bold the title
        plot.margin = unit(c(1, 1, 1, 1), "cm"),  #create margin for the graph
        axis.title.x = element_text(vjust= -2), #lower the label of x-axis
        axis.title.y = element_text(vjust= 8), #shift the label of y-axis to the left
        plot.caption = element_text(vjust= -3, face = "italic")) #lower the caption + change caption's font into italic




