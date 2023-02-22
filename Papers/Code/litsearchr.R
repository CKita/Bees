################################################################################
#### Ecological Synthesis Lab (SintECO): https://marcomellolab.wordpress.com

#### Authors: Cristina A. Kita, Laura C. Leal & Marco A. R. Mello

#### See README for further info: https://github.com/CKita/Bees#readme
################################################################################


################################################################################
########################### SYSTEMATIC REVIEW ################################## 
################################################################################

#Let's get ready for running the code. 

#Set the working directory to the source of this script file.  
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#Delete all previous objects.
rm(list= ls())

#Load or install the required packages.

if(!require(litsearchr)){
  remotes::install_github("elizagrames/litsearchr", ref="main")
  library(litsearchr)
}

if(!require(devtools)){
  install.packages("devtools")
  library(devtools)
}

if(!require(revtools)){
  install.packages("revtools")
  library(revtools)
}

if(!require(stringi)){
  install.packages("stringi")
  library(stringi)
}

if(!require(stringr)){
  install.packages("stringr")
  library(stringr)
}

############################ DATABASES #########################################

#First, let's import and check the data.

#records from Scopus
scopus <- read.csv("records_scopus.csv", header = T, sep = ";")
str(scopus)
write.csv(scopus, "scopus.csv")

#records from Web of Science
wos <- read.csv("records_wos.csv", header = T, sep = ";")
str(wos)
write.csv(wos, "wos.csv")

#let's merge them using litsearchr package 
records <- litsearchr::import_results(file = c("scopus.csv", "wos.csv"))
str(records)

#Finally, remove the duplicates

dedupli<- litsearchr::remove_duplicates(records, field="title", method="exact")

write.csv(dedupli, "../Data/duplicate_records_removed.csv")
