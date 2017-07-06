setwd("/Users/Personas/My Cloud/ch2/paper/quality/agglomeration/data/raw/2016")

# Load data from CQC ratings 
# Data accessible in the care directory with ratings files CQC

# note: depending the year data do not follow the same structure
#       data are referred to a) Locations and b) Providers
#       data duration: November 2015 - June 2017

# Libraries

library(readxl)
library(dplyr)
library(data.table)
library(stringr)
library(rio)
library(forcats)
library(dtplyr)



################
# Data from 2016
#################



file.list <- list.files(pattern='*.xlsx')

# data without considering january - it does not have information about the providers
file.loc = c("01 April 2016 Latest ratings.xlsx", "01 February 2016 Latest ratings.xlsx" ,   
"01 March 2016 Latest ratings.xlsx","1 August Latest ratings.xlsx", "1 December 2016 HSCA Latest ratings.xlsx",
"1 July 2016 Latest ratings.xlsx" ,"1 June 2016 Latest ratings.xlsx",         
 "1 November 2016 HSCA latest ratings.xlsx", "1 September 2016 Latest ratings.xlsx",   
 "3 May 2016 Latest ratings.xlsx" , "4 October 2016 Latest ratings.xlsx")

#----------
# Locations
#----------

loc = lapply(file.loc, function(i){
  x = read_xlsx(i, sheet= "Locations")
  
  # Get the columns
  x = x[, c("Location ID", "Location Name",	"Care Home?",	"Location Type", "Location Post Code", 	"Location Local Authority",
            "Location Region","Service / Population Group","Publication Date","Key Question", "Latest Rating", "Provider ID", "Provider Name")]
  
  # Add a column to say which file they're from
  x$file = i
  # Return data
  x
})

# Transform the list into a data frame and drop repeated observations (some months repeat observations) ----------
    ratings_locations_2016<- rbindlist(loc)
    
    head(ratings_locations_2016)
   
    # select observations that change only 
    ratings_2016 =  ratings_locations_2016 %>% select(-file) %>% unique()
    
    ratings_2016 = ratings_2016 %>% arrange(`Location ID`, `Key Question`, `Publication Date`)
    
    ratings_2016 = ratings_2016 %>% arrange(`Publication Date`)
    
# January ---------------------------------------------------------------------------------------------------------
# it does not have information regarding providers
    
    
    file.january = c("04 January Latest ratings.xlsx")
    
    
    loc.january =  loc = lapply(file.january, function(i){
      x = read_xlsx(i, sheet= "Locations")
      
      # Get the columns
      x = x[, c("Location ID", "Location Name",	"Care Home?",	"Location Type", "Location Post Code", 	"Location Local Authority",
                "Location Region","Service / Population Group","Publication Date","Key Question", "Latest Rating")]
      
      # You may want to add a column to say which file they're from
      x$file = i
      # Return your data
      x
    })
                                             
# Create a data frame 
    
    ratings_locations_january<- rbindlist(loc.january)
    
    ratings_jan =  ratings_locations_january %>% select(-file) %>% unique()
    
   new.ids =  setdiff(ratings_jan$`Location ID`, ratings_2016$`Location ID`) # check whether there are different care homes
    
   # select those are not in other dataset
   
   new_january = ratings_jan %>% filter(`Location ID` %in% new.ids)
   
   new_january = new_january %>% arrange(`Location ID`, `Key Question`, `Publication Date`)
   
   # get information about providers - use data from the active and inactive locations
   
  cqc =  read_xlsx("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/raw/20160927_Active and inactive care homes.xlsx", 
                   sheet = 2)
   
  cqc_red = cqc %>% select(`Location ID`, `Provider ID`, `Provider Name`)
  
  cqc.ids = cqc_red %>% filter(`Location ID` %in% new.ids)
  
  ratings_janone = left_join(ratings_jan, cqc.ids, by = c("Location ID" = "Location ID"))
                                                                            
#------------
# Providers
#-----------

  
    
    prov = lapply(file.loc, function(i){
      x = read_xlsx(i, sheet= "Providers")
      
      # Get the columns
      x = x[, c("Provider ID", "Provider Name",	"Provider Type", "Provider Post Code", 	"Provider Local Authority",
                "Provider Region","Service / Population Group","Publication Date","Key Question", "Latest Rating")]
      
      # You may want to add a column to say which file they're from
      x$file = i
      # Return your data
      x
    })
    
    # Transform the list into a data frame and drop repeated observations (some months repeat observations)
    ratings_providers_2016<- rbindlist(prov)
    head(ratings_providers_2016)
    
    ratings_providers_2016 =  ratings_providers_2016 %>% select(-file) %>% unique()
    
    ratings_providers_2016 = ratings_providers_2016 %>% arrange(`Provider ID`, `Key Question`, `Publication Date`)
    
    ratings_providers = ratings_providers_2016 %>% select(`Provider ID`, `Provider Name`,
                                                          `Provider Type`, `Provider Post Code`,
                                                          `Provider Local Authority`)
    
    
    # link information on locations and providers 
    
    rat_2016 = left_join(ratings_2016, ratings_providers, by = c("Provider ID"))
    
    
    write.csv(ratings_2016, "/Users/Personas/My Cloud/ch2/paper/quality/agglomeration/data/processed/ratings_2016.csv", row.names = FALSE)
    
#################
# Data from 2017
#################
    
    setwd("/Users/Personas/My Cloud/ch2/paper/quality/agglomeration/data/raw/2016")
    
    rm(list = ls())
    
    setwd("/Users/Personas/Downloads/2017")
    
  file.list <- list.files(pattern='*.xlsx')
    
#----------
# Locations
#----------
  
  loc = lapply(file.list, function(i){
    x = read_xlsx(i, sheet= "Locations")
    
    # Get the columns
    x = x[, c("Location ID", "Location Name",	"Care Home?",	"Location Type", "Location Post Code", 	"Location Local Authority",
              "Location Region","Service / Population Group","Publication Date","Key Question", "Latest Rating", "Provider ID", "Provider Name")]
    
    # Add a column to say which file they're from
    x$file = i
    # Return data
    x
  })
  
  # Transform the list into a data frame and drop repeated observations (some months repeat observations) ----------
  ratings_locations_2017<- rbindlist(loc)
  
  head(ratings_locations_2017)
  
  # select observations that change only 
  ratings_2017 =  ratings_locations_2017 %>% select(-file) %>% unique()
  
  ratings_2017 = ratings_2017 %>% arrange(`Location ID`, `Key Question`, `Publication Date`)
  
  write.csv(ratings_2017, "/Users/Personas/My Cloud/ch2/paper/quality/agglomeration/data/processed/ratings_2017.csv", row.names = FALSE)
  
  

  
  ratings_2016 = import("/Users/Personas/My Cloud/ch2/paper/quality/agglomeration/data/processed/ratings_2016.csv")
  
  ratings_2017 = import("/Users/Personas/My Cloud/ch2/paper/quality/agglomeration/data/processed/ratings_2017.csv")
  
  
  
  ratings_2016 = ratings_2016 %>% mutate_each(funs(as.Date), `Publication Date`)
  ratings_2017 = ratings_2017 %>% mutate_each(funs(as.Date), `Publication Date`)
  
  
  
  # link ratings from both years 
  ratings_total = rbind(ratings_2016, ratings_2017) %>% arrange(`Location ID`, `Key Question`)
  
  # clean some strings
  
  ratings_total = ratings_total %>% mutate(`Location Name` = gsub("Limited", "Ltd", `Location Name`), 
                                           `Location Name` = str_trim(`Location Name`, "both"))
  
  
  ratings_total_clean = unique(ratings_total)
  

  ratings_total_clean = ratings_total_clean %>% arrange(`Location ID`, `Key Question`, `Publication Date`)
  
  
  write.csv(ratings_total_clean, "/Users/Personas/My Cloud/ch2/paper/quality/agglomeration/data/processed/ratings_2016_2017.csv", row.names = FALSE)
  
#################
# Data from 2015
#################
  
  setwd("/Users/Personas/My Cloud/ch2/paper/quality/agglomeration/data/raw/2015")
  
  # Data including all the dimensions are from November 
  
  file.list <- list.files(pattern='*.xlsx')
  
  #----------
  # Locations
  #----------
  
  loc = lapply(file.list, function(i){
    x = read_xlsx(i, sheet= "Locations")
    
    # Get the columns
    x = x[, c("Location ID", "Location Name",	"Care Home?",	"Location Type", "Location Post Code", 	"Location Local Authority",
              "Location Region","Service / Population Group","Publication Date","Key Question", "Latest Rating")]
    
    # Add a column to say which file they're from
    x$file = i
    # Return data
    x
  })
  
  
  
# Transform the list into a data frame and drop repeated observations (some months repeat observations) ----------
  ratings_locations_2015<- rbindlist(loc)
  
  head(ratings_locations_2015)
  
  
# Select observations that change only ----------------------------------------------
  ratings_2015 =  ratings_locations_2015 %>% select(-file) %>% unique()
  
  ratings_2015 = ratings_2015 %>% mutate_each(funs(as.Date), `Publication Date`) %>% arrange(`Location ID`, `Key Question`, `Publication Date`)
  
  
# Add information regarding providers ----------------------------------------------------
  
  rat_2015 = ratings_2015 %>% select(`Location ID`, `Publication Date`:`Latest Rating`)
  
  
# Link data from 2016/17 with data from 2015 
ratings_total = import("/Users/Personas/My Cloud/ch2/paper/quality/agglomeration/data/processed/ratings_2016_2017.csv") %>% mutate_each(funs(as.Date), `Publication Date`)
  
  

  rat_total = left_join(ratings_total, rat_2015, by = c("Location ID", "Publication Date","Key Question","Latest Rating"))

  rat_total  = unique(rat_total)
  
# Filter information regarding care homes -------------------------------------------------------
  
  ratings_ch = rat_total %>% filter(`Care Home?` == "Y")
  
  write.csv(ratings_ch, "/Users/Personas/My Cloud/ch2/paper/quality/agglomeration/data/processed/ratings_ch_2015_2017.csv", row.names = FALSE)
  
  
    