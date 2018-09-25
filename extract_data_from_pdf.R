library("tidyverse")
library(dslabs)
library("pdftools")

#Download Example
temp_file <- tempfile()
url <- "http://www.pnas.org/content/suppl/2015/09/16/1510159112.DCSupplemental/pnas.201510159SI.pdf"
#If you are using windows you should force the transfer mode to binary (mode="wb") 
#otherwise your pdf file gets corrupted (when you open it with a pdf reader, it contains some text sections and 
#the searched table is empty)
download.file(url, temp_file, mode="wb")
txt <- pdf_text(temp_file)
file.remove(temp_file)



#we notice that it is a character vector with an entry for each page
raw_data_research_funding_rates <- txt[2]


raw_data_research_funding_rates %>% head
#Each line on the page, including the table rows, is separated by the symbol for newline: \n

tab <- str_split(raw_data_research_funding_rates, "\n")
tab <- tab[[1]]
tab %>% head

the_names_1 <- tab[3]
the_names_2 <- tab[4]

the_names_1 %>% head()

the_names_1 <- the_names_1 %>%
  str_trim() %>%
  str_replace_all(",\\s.", "") %>%
  str_split("\\s{2,}", simplify = TRUE)
the_names_1

the_names_2

the_names_2 <- the_names_2 %>%
  str_trim() %>%
  str_split("\\s+", simplify = TRUE)
the_names_2


tmp_names <- str_c(rep(the_names_1, each = 3), the_names_2[-1], sep = "_")
the_names <- c(the_names_2[1], tmp_names) %>%
  str_to_lower() %>%
  str_replace_all("\\s", "_")
the_names

new_research_funding_rates <- tab[6:14] %>%
  str_trim %>%
  str_split("\\s{2,}", simplify = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(the_names) %>%
  mutate_at(-1, parse_number)
new_research_funding_rates %>% head()

identical(research_funding_rates, new_research_funding_rates)
