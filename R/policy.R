library(tidyverse)
library(rvest)

page <- read_html("https://en.wikipedia.org/wiki/LGBT_rights_by_country_or_territory")

lgbt_rights <- page %>% 
    html_table(fill = TRUE) %>% # generates a list
    map_df(function(x) {
        if (ncol(x) == 8 & names(x)[1] == "LGBT rights in:") {
            names(x) <- str_replace(names(x), ".*serve openly.*", "serve_openly")
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
    mutate(country = lgbt_rights_in %>% 
               str_replace("[\\(\\[].*[\\)\\]]", "") %>% 
               str_trim(),
           mm_legal = if_else(!str_detect(same_sex_sexual_activity,
                                          "Illegal(?! in practice in Chechnya)|Male illegal|[Dd]e facto illegal"),
                              if_else(str_detect(same_sex_sexual_activity, "^Legal\\s*\\(No laws.*have|has ever existed|^Legal$|^Legal\\s*[\\[+]"),
                                      1800,
                                      str_extract(same_sex_sexual_activity,
                                                  "(Male legal|Legal)( nationwide| in England and Wales| in East Germany| in some states since 1962,nationwide| in some states and territories since 1975, nationwide)? (after|since|from)( the)?\\s\\d{4}") %>% 
                                          str_extract(., "\\d{4}") %>% 
                                          as.numeric()),
                              if_else(str_detect(same_sex_sexual_activity, "(Male )?[Ii]llegal((?!since).)*Penalty|Illegal\\s*\\(Decriminalization proposed\\)|Illegal under Article 534"),
                                      -1800,
                                      str_extract(same_sex_sexual_activity,
                                                  "[Ii]llegal( under federal law)? since( the)? \\d{4}") %>% 
                                          str_extract("\\d{4}") %>% 
                                          as.numeric() * -1)),
           ff_legal = if_else(str_detect(same_sex_sexual_activity, "[Ff]emale"),
                              if_else(str_detect(same_sex_sexual_activity, "[Ff]emale( always)? legal(?! since)"),
                                      1800,
                                      if_else(str_detect(same_sex_sexual_activity, "[Ff]emale uncertain"),
                                              mm_legal,
                                              NA_real_)),
                              mm_legal),
           unions_recognized = str_replace_all(recognition_of_same_sex_unions, '(Marriage\\s([Ss]ince |[Ff]rom )\\d{4})|June |July 3, |(\\"Stable unions\\" legal in some states since)', "") %>% 
               str_extract("(?<=[Ss]ince |[Ff]rom )\\d{4}") %>% 
               as.numeric(),
           marriage_legal = if_else(str_detect(same_sex_marriage, "Legal"),
                                    if_else(str_detect(same_sex_marriage, "nationwide"),
                                            if_else(str_detect(country, "Mexico"),
                                                    "2015",
                                                    str_extract(same_sex_marriage, "(?<=nationwide since )\\d{4}")),
                                            str_extract(same_sex_marriage, "(?<=since )(May 24, )?\\d{4}")),
                                    NA_character_) %>% 
               str_extract("\\d{4}") %>% 
               as.numeric(),
           constitutional_ban = str_extract(same_sex_marriage, "(?<=ban(ned)? since )(a )?\\d{4}") %>% 
               str_extract("\\d{4}") %>% 
               as.numeric(),
           adoption_by_couple = if_else(str_detect(adoption_by_same_sex_couples, "[Jj]oint adoption since"),
                                        str_extract(adoption_by_same_sex_couples, "(?<=[Jj]oint adoption since )\\d{4}"),
                                        if_else(str_detect(adoption_by_same_sex_couples, "Legal"),
                                                if_else(str_detect(adoption_by_same_sex_couples, "nationwide"),
                                                        str_extract(adoption_by_same_sex_couples, "(?<=nationwide since )\\d{4}"),
                                                        str_extract(adoption_by_same_sex_couples, "(?<=since )\\d{4}")),
                                                NA_character_)) %>% 
               str_extract("\\d{4}") %>% 
               as.numeric()) %>% 
    select(starts_with("adoption"), everything())

