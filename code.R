install.packages("readr")
install.packages("readxl")

library(readr)
library(readxl)

project_dir <- getwd()
data_dir <- file.path(project_dir,"data")
dslab_data_path <- system.file("extdata", package = "dslabs")

#packedge readr <- functions to read txt files
#packedge readxl <- funcitons to excel files

filename<- file.path(data_dir,"murders.csv")
url <- "http://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"

read_lines(filename, n_max=3)

dat <- read_csv(filename) #results in a Tibble
dat2 <- read.csv(filename,stringsAsFactors = F) #results in a data.frame
dat3 <- read_csv(url)
download.file(url,"murders.csv")

head(dat)
head(dat2)
head(dat3)

class(dat)
class(dat2)
class(dat3)

filename_xls<- file.path(data_dir,"murders.xlsx")

sheet_names <- excel_sheets(filename_xls)
dat_xls <- read_xlsx(filename_xls,sheet=sheet_names[1])

head(dat_xls)

#download as a temp file
tmp_filename <- tempfile()
download.file(url,tmp_filename)
dat4<-read_csv(tmp_filename)
file.remove(tmp_filename)

#From Wide to Tidy
library(tidyverse)

filename <- file.path(data_dir,"fertility-two-countries-example.csv")
wide_data <- read_csv(filename) #this is a wide data. a Tidy data needs to have 1 row for each data
new_tidy_data <- wide_data %>% gather(year,fertility, `1960`:`2015`,convert = TRUE)
head(new_tidy_data)
new_tidy_data %>% ggplot(aes(year,fertility,color=country))+geom_point()
new_wide_format <- new_tidy_data %>% spread(year,fertility)
head(new_wide_format)

test <- read_csv(file.path(data_dir,"test.csv"))
test %>% spread(key=var,value=people)


raw_dat <- read_csv(file.path(data_dir,"life-expectancy-and-fertility-two-countries-example.csv"))
select(raw_dat,1:5)
dat <- raw_dat %>% gather(key,value,-country)
head(dat)
dat <- dat %>% separate(key, c("year","variable_name"),sep="_",extra="merge")
head(dat)
dat <- dat %>% spread(variable_name,value)

library(dslabs)
library(ggrepel) # ==> for geom_text_repel

data(murders)
data(polls_us_election_2016)
head(results_us_election_2016)  # note we use results_

# combine our 2 datasets : we now have a abb column
tab <- left_join(murders, results_us_election_2016, by="state")
head(tab)

# plot
tab %>% ggplot(aes(population/10^6, electoral_votes, label = abb)) +
  geom_point() +
  geom_text_repel() +
  scale_x_continuous(trans="log2") +
  scale_y_continuous(trans="log2") +
  geom_smooth(method="lm", se=FALSE) +
  theme_bw()


tab1 <- slice(murders,1:6) %>% select(state,population)
tab2 <- slice(results_us_election_2016,c(1:3,5,7:8)) %>% select(state,electoral_votes)

tab1 %>% left_join(tab2)
tab1 %>% right_join(tab2)
tab1 %>% inner_join(tab2)
tab1 %>% full_join(tab2) #UNION

tab1 %>% semi_join(tab2)
tab1 %>% anti_join(tab2)


bind_cols(a=1:3,b=4:6)

tab1 <- tab[1:5,]
tab2<-tab[3:7,]
intersect(tab1,tab2)
tab2[3,5]<-tab2[3,5]+1
intersect(tab1,tab2)

union(tab1,tab2)
setequal(tab1,tab2)

df1 <- matrix( 
     c("a","a","b","a"), # the data elements 
     nrow=2,              # number of rows 
     ncol=2,              # number of columns 
     byrow = TRUE)        # fill matrix by rows 
df1
df2 <- matrix( 
  c("a","a","a","b"), # the data elements 
  nrow=2,              # number of rows 
  ncol=2,              # number of columns 
  byrow = TRUE)        # fill matrix by rows 
df2
union(df1, df2)
setdiff(df1, df2)
setdiff(df2, df1)
intersect(df1, df2)

df1 <- data.frame( 
  x=c("a","b"),y=c("a","a"),stringsAsFactors = FALSE)
df1
df2 <- data.frame( 
  x=c("a","a"),y=c("a","b"),stringsAsFactors = FALSE)
df2
union(df1, df2)
setdiff(df1, df2)
setdiff(df2, df1)
intersect(df1, df2)


#web Scraping
library(rvest)
url <- "https://en.wikipedia.org/wiki/Gun_violence_in_the_United_States_by_state"
h<-read_html(url)
class(h)

tab <- h %>% html_nodes("table")
tab <- tab[[2]]

tab <- tab %>% html_table
class(tab)
tab <- tab %>% setNames(c("state","population","total","murders",
                          "gun_murders","gun_ownership","total_rate","murder_rate","gun_murder_rate"))
head(tab)

h <- read_html("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")
recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
prep_time <- h %>% html_node(".o-RecipeInfo__a-Description--Total") %>% html_text()
ingredients <- h %>% html_nodes(".o-Ingredients__a-ListItemText") %>% html_text()
guacamole <- list(recipe, prep_time, ingredients)
guacamole
#use SelectorGaget - Chrome extention - to find the <references>

get_recipe <- function(url){
  h <- read_html(url)
  recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
  prep_time <- h %>% html_node(".o-RecipeInfo__a-Description--Total") %>% html_text()
  ingredients <- h %>% html_nodes(".o-Ingredients__a-ListItemText") %>% html_text()
  return(list(recipe = recipe, prep_time = prep_time, ingredients = ingredients))
}
get_recipe("http://www.foodnetwork.com/recipes/food-network-kitchen/pancakes-recipe-1913844")

s<-'10"'
s
cat(s)

s<-"5'"
s
cat(s)

s<-'5\'10"'
cat(s)

s<-"5'10\""
cat(s)



url <- "https://en.wikipedia.org/wiki/Gun_violence_in_the_United_States_by_state"
h<-read_html(url)
class(h)

tab <- h %>% html_nodes("table")
tab <- tab[[2]]

tab <- tab %>% html_table
class(tab)
murder_raw <- tab %>% setNames(c("state","population","total","murders",
                          "gun_murders","gun_ownership","total_rate","murder_rate","gun_murder_rate"))
head(murder_raw)

commas <- function(x) any(str_detect(x, ","))

murder_raw %>% summarise_all(funs(commas))
test1 <- str_replace_all(murder_raw$population, ",","")
test1 <- as.numeric(test1)
test2 <- parse_number(murder_raw$population)
identical(test1,test2)
murders_new <- murder_raw %>% mutate_at(2:3,parse_number)
head(murders_new)


library(dslabs)
data("reported_heights")
reported_heights %>% mutate(new_height = as.numeric(height)) %>% filter(is.na(new_height)) %>% head(n=10)

not_inches <- function(x,smallest=50,tallest=84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches<smallest |inches>tallest
  ind
}

problems <- reported_heights %>% filter(not_inches(height)) %>% .$height
length(problems)

pattern <- "^\\d\\s*'\\s*\\d{1,2}\\.*\\d*'*\"*$"
str_subset(problems,pattern) %>% head(n=10) %>% cat

pattern2 <- "^[4-6]\\s*[\\.|,]\\s*([0-9]|10|11)$"
str_subset(problems,pattern2) %>% head(n=10) %>% cat

ind <- which(between(suppressWarnings(as.numeric(problems))/2.54,54,81))
ind <- ind[!is.na(ind)]
problems[ind] %>% head(n=10) %>% cat

yes <- c("180 cm", "70 inches")
no <- c("180","70")
s <- c(yes,no)
s

str_detect(s,"cm") | str_detect(s,"inches")
str_detect(s,"cm|inches")

library("tidyverse")
library("htmlwidgets")

yes <- c("5", "6", "5'10", "5 feet", "4'11")
no <- c("", ".", "Five", "six", "feet 5")
s <- c(yes, no)
pattern <- "\\d"
str_detect(s, pattern)

str_view(s,pattern)
str_view_all(s,pattern)

str_view_all(s,"[57]")


str_view_all(s,"\\d{2}")
yes <- c("5'7\"", "6'2\"",  "5'12\"")
no <- c("6,2\"", "6.2\"","I am 5'11\"", "3'2\"", "64")
s<-c(yes,no)
pattern <- "^[4-7]'\\d{1,2}\"$"
str_view_all(s, pattern)


library("tidyverse")
library("htmlwidgets")
library('dslabs')
data("reported_heights")
reported_heights %>% mutate(new_height = as.numeric(height)) %>% filter(is.na(new_height)) %>% head(n=10)

not_inches <- function(x,smallest=50,tallest=84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches<smallest |inches>tallest
  ind
}

problems <- reported_heights %>% filter(not_inches(height)) %>% .$height
length(problems)

pattern <- "^[4-7]'\\d{1,2}$"
problems %>% str_replace("feet|ft|foot", "'") %>%
  str_replace("inches|in|''|\"","") %>%
  str_detect(pattern) %>% sum

pattern2 <- "^[4-7]'\\s\\d{1,2}\"$"
str_subset(problems,pattern2)

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
problems %>% str_replace("feet|ft|foot", "'") %>%
  str_replace("inches|in|''|\"","") %>%
  str_detect(pattern) %>% sum

pattern_without_groups <- "^[4-7],\\d*$"
pattern_with_groups <- "^([4-7]),(\\d*)$"
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
str_detect(s, pattern_without_groups)
str_detect(s, pattern_with_groups)
str_match(s,pattern_with_groups)
a<-str_match(s,pattern_without_groups)
b<-str_extract(s,pattern_with_groups)

str_replace(s,pattern_with_groups,"\\1'\\2")

pattern_with_groups <- "^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"
#^ Start of String, 
#([4-7]) 1 digit between 4 and 7 and extract this value,
#\\s* none or more white spaces, 
#[,\\.\\s+] 1 character that can be: comma, dot or at least one space, 
#\\s* none or more white spaces,
#(\\d*) none or more digits and extract this value,
#$ End of the String

str_subset(problems,pattern_with_groups) %>% str_replace(pattern_with_groups,"\\1'\\2")%>% head

library(tidyverse)
library(dslabs)
problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,\\.\\s](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")

not_inches_or_cm <- function(x,smallest=50,tallest=84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- !is.na(inches) &
    ((inches>=smallest & inches<=tallest) | (inches/2.54>=smallest & inches/2.54<=tallest))
  !ind
}

data("reported_heights")
problems <- reported_heights %>% filter(not_inches_or_cm(height)) %>% .$height
length(problems)

converted <- problems %>% 
  str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
  str_replace("inches|in|''|\"", "") %>%  #remove inches symbols
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") ##change format

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)
mean(index)
converted[!index]

yes <- c("5 feet 7inches", "5 7")
no <- c("5ft 9 inches", "5 ft 9 inches")
s <- c(yes, no)

converted <- s %>% 
  str_replace("\\s*feet|foot|ft", "'") %>% 
  str_replace("\\s*inches|in|''|\"", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
str_detect(converted, pattern)

converted <- s %>% 
  str_replace("\\s+feet|foot|ft\\s+", "'") %>% 
  str_replace("\\s+inches|in|''|\"\\s+", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")
str_detect(converted, pattern)

converted <- s %>% 
  str_replace("\\s*|feet|foot|ft", "'") %>% 
  str_replace("\\s*|inches|in|''|\"", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") 
str_detect(converted, pattern)

converted <- s %>% 
  str_replace_all("\\s", "") %>% 
  str_replace("\\s|feet|foot|ft", "'") %>% 
  str_replace("\\s|inches|in|''|\"", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") 
str_detect(converted, pattern)

converted <- s %>% 
  str_replace("\\s*(feet|foot|ft)\\s*", "'") %>% 
  str_replace("\\s*(inches|in|''|\")\\s*", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")
str_detect(converted, pattern)
converted

yes <- c("5", "6", "5")
no <- c("5'", "5''", "5'4")
s <- c(yes, no)
str_replace(s, "^([4-7])$", "\\1'0")
str_replace(s, "^([56])'?$", "\\1'0")


yes <- c("1,7", "1, 8", "2, " )
no <- c("5,8", "5,3,2", "1.7")
s <- c(yes, no)
str_replace(s, "^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2")

str_trim("5 ' 9 ")

s <- c("Five feet eight inches")
str_to_lower(s)

convert_format <- function(s){
  s %>%
    str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
    str_replace_all("inches|in|''|\"|cm|and", "") %>%  #remove inches and other symbols
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") %>% #change x.y, x,y x y
    str_replace("^([56])'?$", "\\1'0") %>% #add 0 when to 5 or 6
    str_replace("^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2") %>% #change european decimal
    str_trim() #remove extra space
}

words_to_numbers <- function(s){
  str_to_lower(s) %>%  
    str_replace_all("zero", "0") %>%
    str_replace_all("one", "1") %>%
    str_replace_all("two", "2") %>%
    str_replace_all("three", "3") %>%
    str_replace_all("four", "4") %>%
    str_replace_all("five", "5") %>%
    str_replace_all("six", "6") %>%
    str_replace_all("seven", "7") %>%
    str_replace_all("eight", "8") %>%
    str_replace_all("nine", "9") %>%
    str_replace_all("ten", "10") %>%
    str_replace_all("eleven", "11")
}

converted <- problems %>% words_to_numbers %>% convert_format
remaining_problems <- converted[not_inches_or_cm(converted)]
pattern <- "^[4-7]\\s*'\\s*\\d+\\.?\\d*$"
index <- str_detect(remaining_problems, pattern)
remaining_problems[!index]

s <- c("5'10", "6'1\"", "5'8inches", "5'7.5")
tab <- data.frame(x = s)
extract(data = tab, col = x, into = c("feet", "inches", "decimal"), 
        regex = "(\\d)'(\\d{1,2})(\\.\\d+)?")  



not_inches <- function(x,smallest=50,tallest=84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches<smallest |inches>tallest
  ind
}
pattern <- "^([4-7])\\s*'\\s*(\\d+\\.?\\d*)$"

smallest <- 50
tallest <- 84
new_heights <- reported_heights %>% 
  mutate(original = height, 
         height = words_to_numbers(height) %>% convert_format()) %>%
  extract(height, c("feet", "inches"), regex = pattern, remove = FALSE) %>% 
  mutate_at(c("height", "feet", "inches"), as.numeric) %>%
  mutate(guess = 12*feet + inches) %>%
  mutate(height = case_when(
    !is.na(height) & between(height, smallest, tallest) ~ height, #inches 
    !is.na(height) & between(height/2.54, smallest, tallest) ~ height/2.54, #centimeters
    !is.na(height) & between(height*100/2.54, smallest, tallest) ~ height*100/2.54, #meters
    !is.na(guess) & inches < 12 & between(guess, smallest, tallest) ~ guess, #feet'inches
    TRUE ~ as.numeric(NA))) %>%
  select(-guess)

new_heights %>%
  filter(not_inches(original)) %>%
  select(original, height) %>% 
  arrange(height) %>%
  View()
new_heights %>% arrange(height) %>% head(n=10)


filename <- system.file("extdata/murders.csv", package = "dslabs")
lines <- readLines(filename)
lines %>% head()

library("purrr")
x <- str_split(lines, ",") 
x %>% head()
col_names <- x[[1]]
x <- x[-1]
x %>% head()
map(x, function(y) y[1]) %>% head()
map(x, 1) %>% head()

dat <- data.frame(map_chr(x, 1),  
                  map_chr(x, 2),
                  map_chr(x, 3),
                  map_chr(x, 4),
                  map_chr(x, 5)) %>%
  mutate_all(parse_guess) %>%
  setNames(col_names)
dat %>% head

dat <- x %>%
  transpose() %>%
  map( ~ parse_guess(unlist(.))) %>%
  setNames(col_names) %>% 
  as.data.frame() 
dat %>% head

x <- str_split(lines, ",", simplify = TRUE) 
col_names <- x[1,]
x <- x[-1,]
x %>% as_data_frame() %>%
  setNames(col_names) %>%
  mutate_all(parse_guess)


data("gapminder")

  gapminder %>% 
  filter(region=="Caribbean") %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()
  
  gapminder %>% 
    filter(region=="Caribbean") %>%
    filter(str_length(country) >= 12) %>%
    distinct(country)
  
  gapminder %>% filter(region=="Caribbean") %>%
    mutate(country = recode(country, 
                            `Antigua and Barbuda`="Barbuda",
                            `Dominican Republic` = "DR",
                            `St. Vincent and the Grenadines` = "St. Vincent",
                            `Trinidad and Tobago` = "Trinidad")) %>%
    ggplot(aes(year, life_expectancy, color = country)) +
    geom_line()  
  
  
data("polls_us_election_2016")
class(polls_us_election_2016$startdate)
head(polls_us_election_2016$startdate)
as.numeric(polls_us_election_2016$startdate) %>% head()  

polls_us_election_2016 %>% filter(pollster=="Ipsos" & state == "U.S.") %>% ggplot(aes(startdate,rawpoll_trump))+geom_line()

set.seed(2)
dates <- sample(polls_us_election_2016$startdate,10) %>% sort
dates

library(lubridate)
data.frame(date = days(dates),
          month = month(dates),
          day = day(dates),
          year = year(dates))
month(dates,label=T)
x <- c(20090101, "2009-01-02", "2009 01 03", "2009-1-4",
       "2009-1, 5", "Created on 2009 1 6", "200901 !!! 07")
ymd(x)
x<-"09/01/02"
ymd(x)
mdy(x)

now()
now("GMT")
OlsonNames()

x <- "Nov/2/2012 12:34:56"
mdy_hms(x)
