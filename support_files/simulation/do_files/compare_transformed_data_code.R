# Create empty work space

rm(list=ls())

# Load library 

library(tidyverse)

# Step 1: Create data 

df_a <- data.frame(
        pid = c(1,1,1,1,1,1),
        year = c(1,2,3,4,5,6),
        wage = c(50,90,110,130,140,150),
        temp = c(1,0,1,0,1,1),
        perm = c(0,1,0,1,0,0)
)

# Step 2: Identify transitions 

df_b <- df_a %>%
        arrange(pid, year) %>%
        group_by(pid) %>%
        # identify type of transition
        mutate(event_t_p_yes = ifelse(perm == 1 & lag(temp,1) == 1 & row_number()>1, yes = 1, no = NA),
               event_p_t_yes = ifelse(temp == 1 & lag(perm,1) == 1 & row_number()>1, yes = 1, no = NA),
        ) %>%
        ungroup() %>%
        # identify whether trans is whether transition occurred
        mutate(trans = rowSums(select(., contains("event_")), na.rm = TRUE)) %>%
        group_by(pid) %>%
        # transseq is the cumulative count for number of transitions
        mutate(transseq = cumsum(ifelse(is.na(trans), 0, trans))) %>%
        ungroup()

# Step 3: Create new data set, 1 row for each transition at time of transition.  

df_c <- df_b %>%
        filter(trans==1) %>%
        group_by(pid,transseq) %>%
        filter(row_number()==1) %>%
        # identify year transition took place
        mutate(eventtime=0,
               eventyear=year) %>%
        ungroup() %>%
        select(pid, year, transseq, matches("event"))

# Step 4: Generate sequence indicator for years pre/post transition 

df_d <- data.frame()
event <- c(-4,-3,-2,-1,1,2,3,4,5,6)
for (e in event) {
        df_test <- df_c %>%
                # create eventtime for years pre/post transition
                mutate(eventtime=e) 
        df_d <- rbind(df_d,df_test)
}                

# Step 5: Append data frame from step 2 and 3

df_e <- rbind(df_c,df_d) %>%
        arrange(pid,transseq,eventtime) %>%
        mutate(year = year+eventtime) %>%
        arrange(pid,transseq,year)

# Step 6: Merge wage data

df_f <- merge(df_e,df_a) %>%
        arrange(pid,transseq,year) %>%
        mutate(pidseq=pid*100+transseq) %>%  # new identifier
        select(pid, year, pidseq, transseq, wage, temp, perm, eventyear, eventtime, everything())

# Step 7: Code events

df_g <- df_f %>%
  arrange(pidseq,year) %>%
  group_by(pidseq) %>%
  # temp to perm
  mutate(event_t_p_time = ifelse(event_t_p_yes == 1, yes = year - eventyear, no = 0),
         event_t_p_time = ifelse(event_t_p_time < -2, yes = -3, # lower bound
                                 ifelse(event_t_p_time >= 5, yes = 5, no = event_t_p_time)), # upper bound
         event_t_p_time_pos = ifelse(event_t_p_yes == 1, yes = event_t_p_time + 3, no = 0), # make positive
  ) %>%
  # perm to temp
  mutate(event_p_t_time = ifelse(event_p_t_yes == 1, yes = year - eventyear, no = 0),
         event_p_t_time = ifelse(event_p_t_time < -2, yes = -3, # lower bound
                                 ifelse(event_p_t_time >= 5, yes = 5, no = event_p_t_time)), # upper bound
         event_p_t_time_pos = ifelse(event_p_t_yes == 1, yes = event_p_t_time + 3, no = 0), # make positive
  ) %>%
  replace(is.na(.), 0) %>% 
  ungroup()

# Print original and final data frame
print(df_a)

print(df_g)
