library(RSQLite)
con <- dbConnect(drv    = SQLite(), 
                          dbname = "00_data/01_raw_data/02_chinook/Chinook_Sqlite.sqlite")
#return names of tables available in database
dbListTables(con)

tbl(con, "Album")

album_tbl <- tbl(con, "Album") %>% collect()

dbDisconnect(con)
con

library(glue)
name <- "Fred"
glue('My name is {name}.')

library(httr)
GET('https://swapi.dev/api/people/?page=2')

library(httr)
resp <- GET("https://swapi.dev/api/people/1/")
sw_api <- function(path) {
  url <- modify_url(url = "https://swapi.dev", path = glue("/api{path}"))
  resp <- GET(url)
  stop_for_status(resp) # automatically throws an error if a request did not succeed
}

resp <- sw_api("/people/1")
resp

rawToChar(resp$content)

library(jsonlite)
l <- fromJSON(rawToChar(resp$content))
l
toJSON(l)


data_list <- list(strings= c("string1", "string2"), 
                  numbers = c(1,2,3), 
                  TRUE, 
                  100.23, 
                  tibble(
                    A = c(1,2), 
                    B = c("x", "y")
                  )
)

library(jsonlite)
resp %>% 
  .$content %>% 
  rawToChar() %>% 
  fromJSON()

GET ("https://www.alphavantage.co/query?function=GLOBAL_QUOTE&symbol=WDI.DE&apikey=demo") %>% 
  content(as="parsed")

#authentication
alphavantage_api_url <- "https://www.alphavantage.co/query"
ticker               <- "WDI.DE"
# You can pass all query parameters as a list to the query argument of GET()
GET(alphavantage_api_url, query = list('function' = "GLOBAL_QUOTE",
                                       symbol     = ticker,
                                       apikey     = Sys.getenv('TOKEN'))
)

install.packages("keyring")
library(keyring)
keyring::key_set("token")
GET(alphavantage_api_url, query = list('function' = "GLOBAL_QUOTE",
                                       symbol     = ticker,
                                       apikey     = key_get("token"))
)

install.packages("rstudioapi")
library("rstudioapi")
GET(alphavantage_api_url, query = list('function' = "GLOBAL_QUOTE",
                                       symbol     = ticker,
                                       apikey     = askForPassword("token"))
)

#exercise
# get the URL for the wikipedia page with all S&P 500 symbols
url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
# use that URL to scrape the S&P 500 table using rvest
library(rvest)
sp_500 <- url %>%
  # read the HTML from the webpage
  read_html() %>%
  # Get the nodes with the id
  html_nodes(css = "#constituents") %>%
  # html_nodes(xpath = "//*[@id='constituents']"") %>% 
  # Extract the table and turn the list into a tibble
  html_table() %>% 
  .[[1]] %>% 
  as_tibble()

#exercise
url  <- "https://www.imdb.com/chart/top/?ref_=nv_mv_250"
html <- url %>% 
  read_html()
rank <-  html %>% 
  html_nodes(css = ".titleColumn") %>% 
  html_text() %>% 
  # Extrag all digits between " " and ".\n" The "\" have to be escaped
  # You can use Look ahead "<=" and Look behind "?=" for this
  stringr::str_extract("(?<= )[0-9]*(?=\\.\\n)")%>% 
  # Make all values numeric
  as.numeric()

#exercise
title <-  html %>% 
  html_nodes(css = ".titleColumn > a") %>% 
  html_text()

#exercise
library(stringr)
year <- html %>% 
  html_nodes(css = ".titleColumn .secondaryInfo") %>%
  html_text()%>%
  str_extract(pattern = "[0-9]+")%>%
  as.numeric()

#exercise
people <- html %>% 
  html_nodes(css = ".titleColumn > a") %>% 
  html_attr("title")

#exercise
num_ratings <- html %>% 
  html_nodes(css = ".ratingColumn > strong") %>%
  html_attr("title") %>% 
  stringr::str_extract("(?<=based on ).*(?=\ user ratings)" ) %>% 
  stringr::str_replace_all(pattern = ",", replacement = "") %>% 
  as.numeric()

imdb_tbl <- tibble(rank, title, year, people, rating, num_ratings)

#avoid translating of movie names with q defining the acceptable translating quality
resp_diff_lang <- GET(url = "https://www.imdb.com/chart/top/?ref_=nv_mv_250",  
            add_headers('Accept-Language' = "en-US, en;q=0.5")) 
html_diff_lang <- content(resp_diff_lang)


numbers <- c(1:5)
for (i in numbers) {
  print(i)
}
numbers_list <- map(numbers, print)

bike_data_lst <- fromJSON("src/ch3/bike_data.json")
bike_data_lst %>%
  purrr::pluck("productDetail", "variationAttributes", "values", 1, "displayValue")
