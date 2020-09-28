

library(tidyverse)
library(here)

## Read in the data
data <- read_csv("survey_results_public.csv")


## Make a data set that makes the data "long"
## Each column is now a row for every Respondent
explain <- data %>%
  select(-Respondent) %>%
  gather(Column, Value) %>%
  unique(.) %>%
  arrange(Column, Value)

## Make an alt version of above without ConvertedSalary, the Y variable
explain2 <- explain %>%
  filter(Column!="ConvertedSalary")

## Summarize each column, getting a count of 
explain.summary <- explain2 %>%
  group_by(Column) %>%
  summarize(Count = n()) %>%
  arrange(desc(Count))



## Key function for determining whether or not to make custom dummy variables
## The reason this is here is because multi-punch answers get appended with a
## ";".  Therefore, traditional methods for making dummies can't be used since
## we need to split all options first, then determine whether someone chose
## that option.

nutella_spreader <- function(datum){
  
  ## get a chill list of Respondents to join to
  starter_pack <- datum %>%
    select(Respondent) %>%
    arrange(Respondent)
  
  ## skip first column, but loop over all others from orig dataset
  for (i in seq(2,ncol(datum)) ){ 
    
    
    ## if there is a semi-colon within the values, initiate splitting procedure
    if (max(grepl(";", datum[,i]))==1){
      
      ## Get a list of unique values from column i, split by ";".
      ## This will ignore NA values and not include them
      cols <- tibble(Values = unique(unlist(strsplit(datum[,i, drop = T], ";")))) %>%
        filter(complete.cases(.)) %>%
        mutate(Values = gsub("\\+", "plus", Values)) %>%
        mutate(Values = gsub("&", "and", Values)) %>%
        mutate(Values = gsub("\\/", "or", Values))
      
      ## Subset down to just the i'th column, rename it so it's easy to reference.
      sub <- datum[,i]
      original <- names(sub)
      names(sub) <- "Column"
      
      ## Clean up the names a bit so that regular expressions work.
      sub <- sub %>%
        mutate(Column = gsub("\\+", "plus", Column)) %>%
        mutate(Column = gsub("&", "and", Column)) %>%
        mutate(Column = gsub("\\/", "or", Column))
      
      ## Now, loop over all possible answers to the question.
      for (j in seq(1, nrow(cols)) ){
        
        ## For each possible response the i'th question, get the j'th
        ## possible response and search each row of the original dataset for
        ## whether a respondent chose that option.
        hmm <- sapply(sub$Column, grepl, cols$Values[j], USE.NAMES = F)
        dang <- tibble(Binary = ifelse(hmm==T, 1, 0))
        names(dang) <- cols$Values[j]
        
        ## Add these new binary columns to a data.frame with the original column.
        sub <- bind_cols(sub, dang)
        
      }
      
      ## Now that the binary columns exist, remove the original column.
      sub <- sub %>%
        select(-Column)
      
      ## Since each column of sub is just a value, append the original name
      ## of the column to the front so we know where the responses came from.
      names(sub) <- paste(original, names(sub), sep = "_")
      
      starter_pack <- bind_cols(starter_pack, sub)
      
      gc()
      
    } else {
      
      ## If not a multi-punch column, just add it to the new data.frame.
      starter_pack <- bind_cols(starter_pack, datum[,i]) 
      
    }
    
    print(i)
    
    gc()
    
  }
    
   return(starter_pack)
  
}


## Because there are nested for loops, which is not ideal for R, this process
## will take a few minutes.

widened_data <- nutella_spreader(data)

write_csv(widened_data, "survey_widened.csv")




