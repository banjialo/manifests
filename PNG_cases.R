
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
  select (-group, - group_2, - NOTIFICATION_DATE)

#unique (test_3$group_cat)

#gender
PNG_summary <- bind_rows (
  test_3 %>% 
  mutate (category = "Sex",
          SEX = factor (SEX, levels = c("M", "F"), labels = c("Male", "Female"))) %>% 
  group_by (category, SEX, group_cat ) %>% 
  count () %>% 
  spread (group_cat, n) %>% 
  adorn_totals ("col") %>% 
  filter (!is.na(SEX )) %>%
  rename (Variable = SEX),
test_3 %>% 
  filter (!is.na(NOTF_ID )) %>% 
  mutate (category = "Age at diagnosis") %>%
  group_by (category, AGEGRP5 , group_cat ) %>% 
  count () %>% 
  spread (group_cat, n) %>% 
  adorn_totals ("col") %>% 
  filter (!is.na(AGEGRP5 )) %>%
  rename (Variable = AGEGRP5),
test_3 %>% 
  filter (!is.na(NOTF_ID )) %>% 
  mutate (category = "Lineage") %>%
  group_by (category, LINEAGE, group_cat ) %>% 
  count () %>% 
  spread (group_cat, n) %>% 
  adorn_totals ("col")  %>% 
  filter (!is.na(LINEAGE )) %>%
  rename (Variable = LINEAGE),
test_3 %>% 
  filter (!is.na(NOTF_ID )) %>% 
  mutate (category = "Likely place of acquisition/exposure setting") %>%
  group_by (category, PNG_group , group_cat ) %>% 
  count () %>% 
  spread (group_cat, n) %>% 
  adorn_totals ("col") %>% 
  filter (!is.na(PNG_group )) %>%
  rename (Variable = PNG_group))

#extras
test2 <- as_tibble(
  test_3 %>% 
      mutate (SEX = factor (SEX, levels = c("M", "F"), labels = c("Male", "Female"))) %>% 
      group_by (SEX,  group_cat ) %>% 
      summarise (n = n()) %>% 
      spread (group_cat, n) %>% 
    ungroup () %>% 
    filter (!is.na(SEX )) %>% 
    select (-SEX)
)

PNG_weekly <- data.frame (map_int (test2, function (x) sum(x, na.rm = T))) 
names (PNG_weekly) <- "Total"
PNG_weekly <- as_tibble (rownames_to_column(PNG_weekly, "Week"))
PNG_weekly <- PNG_weekly %>%  mutate (Week = factor (Week, levels = Week ))

#write.xlsx (PNG_weekly, "test.xlsx", append = TRUE )
#color = #636363 "#a6bddb"

p <- PNG_weekly %>% 
  ggplot (aes(x = Week, y = Total, group = 1)) +
  theme_minimal () +
  geom_point () +
  geom_line (col=rgb(0.2,0.4,0.6,0.8), linetype = 1, size = 1.5) +  
  theme(axis.text.x=element_text(angle= 90)) +  
  geom_label(aes(label = Total), size = 4.5, vjust = 1, nudge_y = 0.6, nudge_x = 0.1, colour = "#636363" ) +
  ylab ("Number of COVID-19 Cases") +
  xlab ("Notification Week") +
  ggtitle("Number of cases of COVID-19 notified to Queensland Health per week since 1 January 2021 with a recent travel history of Papua New Guinea") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  expand_limits(y = 0)


ggsave(filename = "PNG_Chart.png", width = 14, height = 8, dpi = 300, units = "in", device='png')


#paste0 (PNG_weekly$Week[1], ": ", PNG_weekly$Total[1])

#p + theme_classic()
#p + expand_limits(y = 0)
#p + scale_y_continuous(expand = c(0, 0))

#p + theme (axis.text.x=element_text(angle=45, hjust=0, vjust=1), panel.background = element_rect(fill='white', colour='black'))

#p + scale_y_continuous(expand = c(0, 0), breaks = seq(0, 40, by = 10))
library (plotly)
library (tidyverse)

fig <- plot_ly(PNG_weekly,  x = ~Week, y = ~Total, name = "Gaps", type = 'scatter',  text = ~Total, mode = 'lines+markers+text',
               textposition = "top right")

fig <- fig %>% 
  layout(title = "Number of cases of COVID-19 notified to Queensland Health per week since 1 January 2021 with a recent travel history of Papua New Guinea",
         xaxis = list(title = "Notification Week",
             gridcolor = 'rgb(255,255,255)',
             showgrid = TRUE,
             showline = FALSE,
             showticklabels = TRUE,
             tickcolor = 'rgb(127,127,127)',
             ticks = 'outside',
             zeroline = FALSE),
         yaxis = list(title = "Number of COVID-19 Cases",
             gridcolor = 'rgb(255,255,255)',
             showgrid = TRUE,
             showline = FALSE,
             showticklabels = TRUE,
             tickcolor = 'rgb(127,127,127)',
             ticks = 'outside',
             zeroline = FALSE))
fig


#starts here
library (plotly)

PNG_short <- as_tibble(
  test_3 %>% 
    mutate (SEX = factor (SEX, levels = c("M", "F"), labels = c("Male", "Female"))) %>% 
    group_by (SEX,  group_cat ) %>% 
    summarise (n = n()) %>% 
    spread (group_cat, n) %>% 
    ungroup () %>% 
    filter (!is.na(SEX )) %>% 
    select (-SEX)
)

PNG_weekly <- data.frame (map_int (PNG_short, function (x) sum(x, na.rm = T))) 
names (PNG_weekly) <- "Total"
PNG_weekly <- as_tibble (rownames_to_column(PNG_weekly, "Week"))
PNG_weekly <- PNG_weekly %>%  mutate (Week = factor (Week, levels = Week ))


fig <- plot_ly(PNG_weekly,  x = ~Week, y = ~Total, name = "Gaps", type = 'scatter',  text = ~Total, mode = 'lines+markers+text',
               textposition = "top right")

fig <- fig %>% 
  layout(title = "Number of cases of COVID-19 notified to Queensland Health per week since 1 January 2021 with a recent travel history of Papua New Guinea",
         xaxis = list(title = "Notification Week",
                      gridcolor = 'rgb(255,255,255)',
                      showgrid = TRUE,
                      showline = FALSE,
                      showticklabels = TRUE,
                      tickcolor = 'rgb(127,127,127)',
                      ticks = 'outside',
                      zeroline = FALSE),
         yaxis = list(title = "Number of COVID-19 Cases",
                      gridcolor = 'rgb(255,255,255)',
                      showgrid = TRUE,
                      showline = FALSE,
                      showticklabels = TRUE,
                      tickcolor = 'rgb(127,127,127)',
                      ticks = 'outside',
                      zeroline = FALSE))


