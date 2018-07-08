library(tidyverse)
library(rvest)

page <- read_html("https://en.wikipedia.org/wiki/LGBT_rights_by_country_or_territory")

lgbt_rights <- page %>% 
    html_table(fill = TRUE) %>% # generates a list
    map_df(function(x) {
        if (ncol(x) == 8 & names(x)[1] == "LGBT rights in:") {
            names(x) <- str_replace(names(x), ".*serve openly.*", "Allowed to serve openly in military")
            if (any(str_detect(names(x), "Recognition of relationships"))) {
                x <- x %>% 
                    mutate(`Recognition of same-sex unions` = if_else(!is.na(`Recognition of relationships`),
                                                                             paste("Relationships: ", `Recognition of relationships`),
                                                                             NA_character_)) %>% 
                    select(-`Recognition of relationships`)
            }
            x1 <- x
        } else {
            x1 <- NULL
        }
        
        return(x1)
    }) %>% 
    janitor::clean_names() %>% 
    mutate(mm_legal = if_else(!str_detect(same_sex_sexual_activity,
                                          "Illegal|(Male illegal)"),
                              if_else(str_detect(same_sex_sexual_activity, "Legal (No laws against same-sex sexual activity have ever existed in the country)"),
                                      1900,
                                      as.numeric(str_extract(same_sex_sexual_activity,
                                                             "(?<=Legal since )\\d{4}"))),
                                      0)) %>% 
    select(same_sex_sexual_activity, mm_legal, everything())

