#laod libaries
library (readxl)
library (tidyverse)

#set working dir

#create a function that reads file
readfile <- function(file){
result <- read_xlsx (file, skip = 7)
  return (result)
}

#read file
GE <- list.files (pattern = "^GE_OB")

GE_2 <- lapply (GE, readfile) 

#append
GE_3 <- bind_rows(GE_2) %>%  arrange (CLIENTID )

#save file
write.csv (GE_3, "GE_merged.csv", row.names = F, na = "" ) 
