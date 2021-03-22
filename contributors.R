# retrieve deworming repo contributions: adapted from Hadley Wickham's scripts to credit contributors to R for Data Science
## contributors.R (https://github.com/hadley/r4ds/blob/master/contributors.R)
## index.Rmd (https://github.com/hadley/r4ds/blob/master/index.rmd)
## R for Data Science license: https://creativecommons.org/licenses/by-nc-nd/3.0/us/

library(tidyverse)
library(gh)
library(glue)
library(here)

#JSON format info on contributors to deworming repo
deworming_json <- gh::gh("/repos/:owner/:repo/contributors",
                    owner = "BITSS-OPA",
                    repo = "opa-deworming",
                    .limit = Inf)
deworming_contribs <- tibble(
  login = deworming_json %>% map_chr("login"),
  n = deworming_json %>% map_int("contributions")
)

#retrieve names of contributors
more_info_json <- map(
  deworming_contribs$login,
  ~ gh::gh("/users/:username", username = .x)
)
more_info <- tibble(
  login = map_chr(more_info_json, "login", .default = NA),
  name = map_chr(more_info_json, "name", .default = NA)
)

# combine contributor information from both tables
deworming_contribs_all <- deworming_contribs%>%
  left_join(more_info) %>%
  arrange(desc(n)) #arranged contributors alphabetically by number of contributions

# contributors' names with links
names_with_links <- deworming_contribs_all %>% 
  mutate(
    link = glue::glue("[\\@{login}](https://github.com/{login})"), #add link to github profile
    desc = ifelse(is.na(name), link, glue::glue("{name} ({link})")) #if no name is provided, show link only
  )


write_csv(names_with_links, here("contributors.csv"))
