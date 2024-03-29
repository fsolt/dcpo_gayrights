library(tidyverse)
library(rvest)
library(countrycode)

page <- read_html("https://en.wikipedia.org/wiki/LGBT_rights_by_country_or_territory")

# Revise countrycode::countrycode to work better with custom names in cc_dcpo
body(countrycode)[[2]] <- substitute(
    if (is.null(custom_dict) | as.list(match.call())[["custom_dict"]] == "cc_dcpo") {
        if (origin == "country.name") {
            origin <- "country.name.en"
        }
        if (destination == "country.name") {
            destination <- "country.name.en"
        }
        if (origin %in% c("country.name.en", "country.name.de")) {
            origin <- paste0(origin, ".regex")
            origin_regex <- TRUE
        }
        else {
            origin_regex <- FALSE
        }
    } 
)
cc_dcpo <- DCPO::cc_dcpo

lgbt_rights <- page %>% 
    html_table(fill = TRUE) %>% # generates a list
    map_df(function(x) {
        if (ncol(x) == 8 & str_detect(names(x)[1], "LGBT rights in")) {
            names(x) <- str_replace(names(x), ".*serve openly.*", "military_openly")
            names(x) <- str_replace(names(x), ":", "")
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
               str_trim() %>%
               countrycode("country.name", "dcpo.name", custom_dict = cc_dcpo),
           mm_legal = if_else(!str_detect(same_sex_sexual_activity,
                                          "Illegal(?! in practice in Chechnya| in the provinces)|Male illegal|[Dd]e facto illegal"),
                              if_else(str_detect(same_sex_sexual_activity, "^Legal\\s*\\(No laws.*have|has ever existed|^Legal$|^Legal\\s*[\\[+]|^Legal nationwide, except;"),
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
                                      if_else(str_detect(same_sex_sexual_activity, "[Ff]emale legal since\\s\\d{4}"),
                                              str_extract(same_sex_sexual_activity, "(?<=[Ff]emale legal since\\s)\\d{4}") %>% 
                                                  as.numeric(),     
                                              if_else(str_detect(same_sex_sexual_activity, "[Ff]emale uncertain"),
                                                      mm_legal,
                                                      NA_real_))),
                                      mm_legal),
           civ_union = str_replace_all(recognition_of_same_sex_unions, '(Marriage\\s([Ss]ince |[Ff]rom )\\d{4})|June |July 3, |(\\"Stable unions\\" legal in some states since)', "") %>% 
               str_extract("(?<=[Ss]ince |[Ff]rom )\\d{4}") %>% 
               as.numeric(),
           marry = if_else(str_detect(same_sex_marriage, "Legal"),
                                    if_else(str_detect(same_sex_marriage, "nationwide"),
                                            if_else(str_detect(country, "Mexico"),
                                                    "2015",
                                                    str_extract(same_sex_marriage, "(?<=nationwide since )\\d{4}")),
                                            str_extract(same_sex_marriage, "(?<=since )(May 24, )?\\d{4}")),
                                    NA_character_) %>% 
               str_extract("\\d{4}") %>% 
               as.numeric(),
           con_ban = str_extract(same_sex_marriage, "(?<=ban(ned)? since )(a )?\\d{4}") %>% 
               str_extract("\\d{4}") %>% 
               as.numeric(),
           adopt = if_else(str_detect(adoption_by_same_sex_couples, "[Jj]oint adoption since"),
                                        str_extract(adoption_by_same_sex_couples, "(?<=[Jj]oint adoption since )\\d{4}"),
                                        if_else(str_detect(adoption_by_same_sex_couples, "Legal"),
                                                if_else(str_detect(adoption_by_same_sex_couples, "nationwide"),
                                                        str_extract(adoption_by_same_sex_couples, "(?<=nationwide since )\\d{4}"),
                                                        str_extract(adoption_by_same_sex_couples, "(?<=since )\\d{4}")),
                                                NA_character_)) %>% 
               as.numeric(),
           serve = str_extract(military_openly, "\\d{4}") %>% 
               as.numeric()) %>% 
    select(country, ff_legal, mm_legal, civ_union, marry, con_ban, adopt, serve) %>% 
    filter(!is.na(country)) %>% 
    bind_rows(tibble(country = "Northern Ireland",
                     ff_legal = 1800,
                     mm_legal = 1982,
                     civ_union = 2005,
                     marry = NA_real_,
                     con_ban = NA_real_,
                     adopt = 2013,
                     serve = 2000)) %>% 
    distinct()

lgbt_rights1 <- crossing(country = unique(x$country),      # all possible combinations of these two variables
                         year = min(x$year):max(x$year)) %>% 
    left_join(x %>% 
                  group_by(country) %>% 
                  summarize(ccode = first(ccode)) %>% 
                  ungroup(),
              by = "country") %>% 
    mutate(tcode = as.integer(year - min(year) + 1),
           ktcode = as.integer((ccode-1)*max(tcode)+tcode)) %>% 
    left_join(lgbt_rights, by = c("country")) %>%
    group_by(country) %>% 
    mutate(ff_legal = if_else(ff_legal > 0, as.numeric(year >= ff_legal), 1 - as.numeric(year >= -ff_legal)),
           mm_legal = if_else(mm_legal > 0, as.numeric(year >= mm_legal), 1 - as.numeric(year >= -mm_legal)),
           civ_union = if_else(!is.na(civ_union), as.numeric(year >= civ_union), 0),
           marry = if_else(!is.na(marry), as.numeric(year >= marry), 0),
           con_ban = if_else(!is.na(con_ban), as.numeric(year >= con_ban), 0),
           adopt = if_else(!is.na(adopt), as.numeric(year >= adopt), 0),
           serve = if_else(!is.na(serve), as.numeric(year >= serve), 0)) %>% 
    ungroup() %>% 
    arrange(ktcode) %>% 
    mutate(marry = if_else((country == "Mexico" & year >=2015), 1, marry))

save(lgbt_rights1, file = "data/lgbt_rights1.rda")
