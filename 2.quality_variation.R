# Quality variation




library(ggplot2)
library(scales)
library(rio)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(forcats)
library(RColorBrewer)
library(janitor)
library(purrr)
library(lubridate)



ratings_total = import("/Users/Personas/My Cloud/ch2/paper/quality/agglomeration/data/processed/ratings_ch_2015_2017.csv") %>%
  mutate_each(funs(as.Date), `Publication Date`)


  data = ratings_total %>% select(`Location ID`, `Publication Date`:`Latest Rating`, `Provider ID`)

# remove duplicates 
#     - mainly because of duplicate names of locations and providers 

  data = data %>% unique() # duplicates
                            

# create a time variable to determine the order of the inspections
df= data %>%
  group_by(`Location ID`, `Key Question`, `Publication Date`) %>%
  arrange(`Location ID`, `Key Question`, `Publication Date`) %>%
  ungroup() %>%
  group_by(`Location ID`, `Key Question`)%>% 
  mutate(time = row_number(), total.inspections = n())

# ---------------------------------------------
# Visualise the changes in categories over time
# ---------------------------------------------


  # divide into the categories that are evaluated:

 care = df %>% filter(`Key Question` == "Caring")
 effective = df %>%  filter(`Key Question` == "Effective")
 safe = df %>%  filter(`Key Question` == "Safe")
 responsive = df %>%  filter(`Key Question` == "Responsive")
 led = df %>%  filter(`Key Question` == "Well-led")

 
# function for ordering the values of the categories depending on the date - objective: create data frame with the number and frequencies for each category
 
 # date 1 
 date1 = function(data_frame){
   x = data_frame %>% select(`Location ID`, `Latest Rating`, time) %>% 
     filter(time == 1) %>% rename(value1 = `Latest Rating`) %>% 
     select(-time)
   return(x)
 }

 # date 2
 date2 = function(data_frame){
   x = data_frame %>% select(`Location ID`, `Latest Rating`, time) %>% 
     filter(time == 2) %>% rename(value2 = `Latest Rating`) %>% 
     select(-time)
   return(x)
 }
 
 
# data_final  
 
 data_final = function(data_frame){
   
   val1 =  date1(data_frame)
   val2 = date2(data_frame)
   dataset = full_join(val1, val2, by = c("Location ID", "Key Question"))
   return(dataset)
 
   }
 
 
cares = data_final(care)
effectives = data_final(effective)
safes = data_final(safe)
responsives = data_final(responsive)
wellled = data_final(led)
 
 
 
 
data_clean = function(data_frame){
   
   datatest = data_frame %>% crosstab(value1, value2) %>% adorn_crosstab("row")
   
   # create data as long
   data_long = datatest %>% gather(value2, freq, `Good`:`NA_`) %>% 
     separate(freq, c("Percentage", "Number"), sep = "%") %>%
     mutate(Number= str_trim(Number, "both"))%>%
     mutate(number= parse_number(Number)) %>% select(-Number) 
    
   # percentages as numeric
   data_long$Percentage = as.numeric(data_long$Percentage)
   
   # values 1 and 2 as factors and categories reordered
   data_long = data_long %>% mutate_at(vars(matches("value")),funs(factor))
   
   # reorder the levels of the category 
       # value1 
       data_long$value1 = fct_rev(fct_relevel(data_long$value1, "Outstanding", "Good", "Requires improvement", "Inadequate"))
       
       
       #value2
       data_long$value2 = fct_relevel(data_long$value2, "Outstanding", "Good", "Requires improvement", "Inadequate")
   
   return(data_long)
 }
 

 # long data
care_long = data_clean(cares)
 effective_long = data_clean(effectives)
 safe_long = data_clean(safes)
 responsive_long = data_clean(responsives)
 well_long = data_clean(wellled)
 
# create identifying variables
 care_long$category = "Care"
 effective_long$category = "Effective"
 safe_long$category = "Safe"
 responsive_long$category =  "Responsive"
 well_long$category = "Well-led"
 
# create a whole dataset and recode  NA_
 datafinal = rbind(care_long, effective_long, safe_long, responsive_long, well_long)
 
 datafinal = datafinal %>% mutate(value2 = fct_recode(value2, "Not Assessed" = "NA_"))
 
 
 
 rm(care_long, effective_long, safe_long, responsive_long, well_long)  
 
 
 
 
 # Plot 
 
 g <- ggplot(datafinal, aes(value2, value1)) + 
   geom_point(aes(size = Percentage), colour = "#deebf7") + 
   theme_bw() + xlab("Date2") + ylab("Date1")
 
 
 g + scale_size_continuous(range=c(7.75,15)) + 
   geom_text(aes(label = Percentage), size = 2.75) + 
   #theme(axis.text.x = element_text(angle = 0, hjust = 1)) + 
   facet_wrap( ~ category, nrow = 5, ncol = 1)
 
 # ------------ #
 # From brexit  #
 # ------------ #
 
 data = data %>% mutate(month = month(`Publication Date`), year = year(`Publication Date`))
 
 counts = data %>% group_by(month, year, `Key Question`, `Latest Rating`) %>% tally() %>% arrange(year, month) %>% mutate(month_year = paste(month, year, sep="-"))
 
 counts =  counts %>% mutate_each(funs(as.factor), `Key Question`, `Latest Rating`)
 
 counts$month_year = fct_inorder(counts$month_year)
 
 d = counts %>% ggplot(aes(fct_inorder(month_year), n, colour = `Key Question`)) + 
   geom_point(aes(group=`Key Question`)) + 
   facet_grid( `Latest Rating` ~ . , scales = "free") + 
   theme(axis.text.x = element_text(angle = 90)) 
 d
 