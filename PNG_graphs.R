
library (stringr)
library (lubridate)
library (readxl)
library (tidyverse)
library (janitor)


PNG_a <- str_subset ( list.files (pattern = "SHECC.xlsx" ), pattern = "^line_list")
PNG_b <- lapply (PNG_a, read_xlsx)
PNG_c <- bind_rows (PNG_b)

df <- PNG_c


dummy <- seq (ymd ("20200101"), today(), 1)

dummy_2 <- as_tibble (x = dummy) 
names (dummy_2) <- "NOTIFICATION_DATE"

#function to  cut dates
create_date_periods <- function(dates, time_period = 28) {
  # TODO: add some error checking
  
  # create a vector to hold the results
  return_vector <- structure(rep(NA_real_, length(dates)), class = "Date")
  
  # if any date in the vector is still missing, keep going
  while(any(is.na(return_vector))) {
    
    # set minimum date amongst the values that are missing
    min_date <- min(dates[is.na(return_vector)])
    
    # if the date falls in range of interest, set it to the minimum date
    return_vector[dates >= min_date & dates <= min_date + time_period] <- min_date
  }
  
  return(return_vector)
}


df_sample <- bind_rows (dummy_2, df ) 

test_3 <- df_sample %>% 
  select (NOTIFICATION_DATE, NOTF_ID, SEX, AGEGRP5, LINEAGE, ACQUISITION_PLACE, PNG_Resid, PNG_group) %>% 
  arrange (NOTIFICATION_DATE) %>% 
  filter (NOTIFICATION_DATE >= ymd("2021-01-01")) %>% 
  mutate(NOTIFICATION_DATE = as.Date(NOTIFICATION_DATE),
         group = create_date_periods (NOTIFICATION_DATE, time_period = 6),
         group_2 = format (group + 6, "%d/%m/%Y"), 
         group = format (group, "%d/%m/%Y"), 
         group_cat = str_c (group, "-", group_2 ),
         group_cat = factor (group_cat, levels = unique (group_cat), ordered = TRUE)) %>% 
  select (-group, - group_2, - NOTIFICATION_DATE) %>% 
  filter (!is.na(NOTF_ID)) %>% 
  mutate (value = 1)


test_3 %>% 
  # mutate (Travel = case_when (ACQUISITION_PLACE == "Papua New Guinea" ~ "PNG", 
  #                             ACQUISITION_PLACE != "Papua New Guinea" ~ "Other",
  #                             is.na (ACQUISITION_PLACE) ~ "Handle",
  #                             TRUE ~ ACQUISITION_PLACE),
  #         SEX = factor (SEX, levels = c("M", "F"), labels = c("Male", "Female"))) %>% 
  # filter (Travel %in% c("PNG", "Other")) %>% 
  group_by (group_cat ) %>%
  summarise (Total = n()) %>% 
  ungroup() %>% 
  ggplot (aes(x = group_cat, y = Total, group = 1)) +
  geom_point () +
  geom_line () +
  theme(axis.text.x=element_text(angle=90, hjust=1))


test_3 %>% 
  mutate (category = "Sex",
          SEX = factor (SEX, levels = c("M", "F"), labels = c("Male", "Female"))) %>% 
  group_by (SEX, group_cat ) %>% 
  summarise (N = sum(n)) 
