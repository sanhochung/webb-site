Webb-site "Who's who" data scraping

Brief description of files in this repository

R scripts: 

"1_membership.R" and "2_bio.R" will show you how I scrape the data of membership of individuals and individual profiles from webb-site.com. (WARNING: these two R scripts will take approximately a whole day (respectively) to finish running the whole script. It is recommended to replicate the script on the High Performance Computing (HPC) system. If you wish to replicate the script with small samples, please change the sequence number of the first for-lope from "131535" to a smaller number (e.g. 300)

"1b_summary_graph.R" and "2b_summary_graph.R" will show you how I plot summary graphs of membership and profiles respectively. (be ware of WARNINGs within the script while replicating)

csv files:

"webb_memb_raw.csv" is the fresh, uncleaned data of memberships of each individual.

"webb_memb.csv" is the cleaned membership data.

"webb_bio_raw.csv" is the fresh, uncleaned data of individual profiles. 

"webb_memb.csv" is the cleaned profile data.

(The rest of them will be needed by R scripts that plot summary graphs.)

pdfs:

"memb_summary.pdf" is the summary of membership data, showing the number of active membership each year from 1960 till now. 

"bio_summary.pdf" is the summary of profile data, showing the number of alive profile each year from 1900 till now. 