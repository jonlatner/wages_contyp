df_sample_2 <- df_sample %>%
        filter(pid == 9527) %>%#
        select(pid, year) %>%
        arrange(pid, year) %>%
        group_by(pid) %>%
        mutate(start = ifelse(year - lag(year) == 1, yes = NA, no = 1),
               start = ifelse(row_number()==1, yes = 1, no = start),
               spell = cumsum(ifelse(is.na(start), 0, start))) %>%
        ungroup() %>%
        group_by(pid, spell) %>%
        mutate(count=row_number(),
               max = max(count)) %>%
        ungroup()

View(df_sample_2)
