#potentially set the working directory to a public folder which is will be universal to everyone???
#only works for wide format

library (haven)
library (tidyverse)
library (lubridate)
library (stringr)
library (stringi)

#library (readxl)


df <- read_dta ("test.dta")

df_1 <- df %>%
  mutate (Uniqueid = as.factor(Uniqueid)) %>%
  select (
    DateAdded,
    Uniqueid,
    FirstName,
    MiddleName,
    LastName,
    PhoneNumber1:PhoneNumber7,
    Email1,
    Email2,
    FlightNo,
    SeatNumber,
    DepartureDate,
    ApartmentUnit,
    AddressNumberandStreet,
    PostalCode,
    Suburb,
    State,
    everything()
  )

names <- names (df_1)

if ("DateofBirth" %in% names == TRUE &
    length (unique (df_1$DateofBirth)) != 1) {
  df_2 <- df_1 %>%
    group_by (Uniqueid, DateofBirth) %>%
    mutate (id = row_number ()) %>%
    filter (id == 1) %>%
    ungroup () %>%
    select (-id)
  df_4 <- as_tibble (df_2)
} else{
  df_3 <- df_1 %>%
    group_by (Uniqueid, SeatNumber) %>%
    mutate (id = row_number ()) %>%
    filter (id == 1) %>%
    ungroup () %>%
    select (-id)
  df_4 <- as_tibble (df_3)
}

#create a function to clean phone numbers
clean_phone <- function (x) {
  str_trim (str_replace_all (x, "[[:punct:]]", ""))
}

#2
clean_phone_2 <- function (x) {
  str_trim (str_replace_all (x, "`", ""))
}

#3
clean_phone_3 <- function (x) {
  str_trim (str_replace_all (x, "\\+", ""))
}

#4
clean_phone_4 <- function (x) {
  str_trim (str_replace_all (x, "^6161", "61"))
}

df_4$gen = as.factor (stri_rand_strings (nrow(df_4), 10))

df_5 <- df_4 %>%
  mutate (gen = as.factor (str_c (Uniqueid , gen, sep = "-")))
#test
df_6 <- df_5 %>%
  select (Uniqueid, gen, starts_with("phone")) %>%
  mutate_if (is.character, clean_phone) %>%
  mutate_if (is.character, clean_phone_2) %>%
  mutate_if (is.character, clean_phone_3) %>%
  mutate_if (is.character, clean_phone_4)


df_7 <-
  inner_join (df_6, select (df_5,-starts_with("Phone")), by = c("Uniqueid", "gen"))
df_8 <- df_7 %>%  select (-gen)

df_9 <- df_8 %>%
  select (
    DateAdded,
    Uniqueid,
    FirstName,
    MiddleName,
    LastName,
    DateofBirth,
    SeatNumber,
    PhoneNumber1:PhoneNumber7,
    Email1,
    Email2,
    ApartmentUnit,
    AddressNumberandStreet,
    Suburb,
    State,
    PostalCode,
    FlightNo,
    DepartureDate,
    DeparturePort,
    ArrivalPort,
    everything()
  )

write_dta (df_9, "test2.dta")


# df_6 %>%
#   #group_by_if (is.character) %>%
#   group_by(Uniqueid, across(where(is.character))) %>%
#   count () %>%
#   ungroup () %>%
#   select (-n) %>%
#   gather


# left_join (
#   nsw %>%
#     select (name, seatnumber, phonenumber1: phonenumber7) %>% #type = all columns to be gathered; number = columns to put the values of type
#     gather ( "type", "number", "phonenumber1", "phonenumber2", "phonenumber3", "phonenumber4", "phonenumber5", "phonenumber6", "phonenumber7") %>%
#     arrange (name, number, seatnumber) %>%
#     select (name, seatnumber, number) %>%
#     distinct (name, seatnumber, number) %>%
#     group_by (name, seatnumber) %>%
#     mutate (contact = "phonenumber",
#             id = row_number(),
#             contact_n = str_c (contact, id, sep = " ")) %>%
#     ungroup () %>%
#     select (name, seatnumber, contact_n, number, -contact, -id) %>%
#     spread (contact_n, number),
#   select (nsw, -(phonenumber1:phonenumber7)), by = c("name", "seatnumber"))
#

# clean <- function (df){
#   for (i in 2:length (df)){
#  out <-  mutate (i = str_trim (str_replace_all (i, "[[:punct:]]", " ")))
# }
# }
#
#
#
#
# for (i in 2:length (test)){
#   print (i)
# }
#
#
# #clean phone numbers at spcific columns
# df_4 %>%
#   across (.cols = starts_with("phone"), .fns = as.integer())
#
# df_4 %>%
#   mutate_at (.vars = starts_with("phone"), .funs = as.integer())
#
