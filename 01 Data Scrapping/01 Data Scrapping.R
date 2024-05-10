#libraries needed to scrape data
library(rvest)
library(tidyverse)
library(stringr)

##==========================================================================================================================================

##Scrapping data of Alt BAlaji from IMDB:  

#loading site
website <- read_html("https://www.imdb.com/search/title/?companies=co0642697")

#loading chuck of code for different web series individually
main <- website %>% html_elements(".lister-item.mode-advanced")

#defining blank vectors for the columns needed
name = codes = URL = certificate = genre = vector(length = length(main))
s.year = runtime = ov.rating = ov.votes = numeric(length = length(main))

#for loop for scrapping entries one by one with individual web series
for (i in 1 : length(main))
{
  #Scrapping name:
  name[i] <- main[i] %>% html_elements(".lister-item-header a") %>% html_text()
  
  #scrapping codes and individual site: 
  codes[i] <- main[i] %>% html_elements(".loadlate") %>% html_attr("data-tconst")
  URL[i] <- paste("https://www.imdb.com/title/", codes[i], "/", sep = "")
  
  #scrapping starting year:
  s.year[i] <- main[i] %>% html_elements(".lister-item-year.text-muted.unbold") %>% html_text() %>% str_sub(2,5) %>% as.numeric()
  
  #scrapping certificate:
  if (  length(  main[i] %>% html_elements(".text-muted .certificate") %>% html_text()  ) ==  0 ) #if else for handeling missing values
  {
    certificate[i] <- NA
  }
  else
  {
    certificate[i] <- main[i] %>% html_elements(".text-muted .certificate") %>% html_text()
  }
  
  #scrapping runtime:
  if (  length(main[i] %>% html_elements(".text-muted .runtime") %>% html_text() %>% str_sub(1,2) %>% as.numeric()) ==  0 )  #if else for handeling missing values
  {
    runtime[i] <- NA
  }
  else
  {
    runtime[i] <- main[i] %>% html_elements(".text-muted .runtime") %>% html_text() %>% str_sub(1,2) %>% as.numeric()
  }
  
  #scrapping genre:
  genre[i] <- main[i] %>% html_elements(".text-muted .genre") %>% html_text() %>% str_sub(2,) %>% str_trim()
  
  #scrapping overall rating:
  ov.rating[i] <- main[i] %>% html_elements(".inline-block.ratings-imdb-rating strong") %>% html_text() %>% as.numeric()
  
  #scrapping overall votes:
  dummy <- main[i] %>% html_elements(".sort-num_votes-visible span") %>% html_text()
  ov.votes[i] <- dummy[2] %>% str_replace_all(",", "") %>% as.numeric()
  
}

#creating vector for Serial No:
serial <- seq(1:length(main))

#creating vector for Platform:
plat <- rep("Alt Balaji", length(main))

#Data frame for Alt Balaji:
alt_balaji <- data.frame(serial, name, plat, URL, s.year, certificate, runtime, genre, ov.rating, ov.votes, stringsAsFactors = F)


##==========================================================================================================================================

##Scrapping data of Amazon Prime from IMDB:

#loading site
website <- read_html("https://www.imdb.com/list/ls099967158/?st_dt=&mode=detail&page=1&sort=moviemeter,asc")

#loading chuck of code for different web series individually
main <- website %>% html_elements(".lister-item.mode-detail")

#defining blank vectors for the columns needed
name = codes = URL = certificate = genre = vector(length = length(main))
s.year = runtime = ov.rating = ov.votes = numeric(length = length(main))

#for loop for scrapping entries one by one with individual web series
for (i in 1:length(main))
{
  #Scrapping name:
  name[i] <- main[i] %>% html_elements(".lister-item-header a") %>% html_text()
  
  #Scrapping codes and individual site: 
  codes[i] <- main[i] %>% html_elements(".loadlate") %>% html_attr("data-tconst")
  URL[i] <- paste("https://www.imdb.com/title/", codes[i], "/", sep = "")
  
  #Scrapping year:
  s.year[i] <- main[i] %>% html_elements(".lister-item-year.text-muted.unbold") %>% html_text() %>% str_sub(2,5) %>% as.numeric()
  
  #Scrapping certificate:
  certificate[i] = main[i] %>% html_elements(".text-muted .certificate") %>% html_text()
  
  #Scrapping runtime:
  if (  length(main[i] %>% html_elements(".text-muted .runtime") %>% html_text() %>% str_sub(1,2) %>% as.numeric()) ==  0 ) #if else for handeling missing values
  {
    runtime[i] <- NA
  }
  else
  {
    runtime[i] <- main[i] %>% html_elements(".text-muted .runtime") %>% html_text() %>% str_sub(1,2) %>% as.numeric()
  }
  
  #Scrapping genre:
  genre[i] <- main[i] %>% html_elements(".text-muted .genre") %>% html_text() %>% str_sub(2,) %>% str_trim()
  
  #Scrapping overall rating:
  ov.rating[i] <- main[i] %>% html_elements(".ipl-rating-star.small .ipl-rating-star__rating") %>% html_text() %>% as.numeric()
  
  #Scrapping overall votes:
  ov.votes <- ((main[i] %>% html_elements(".text-muted.text-small"))[3] %>% html_elements("span") %>% html_attr("data-value"))[2] %>% as.numeric()
  
}

#creating vector for Serial No:
serial <- seq(1:length(main))

#creating vector for Platform:
plat <- rep("Amazon Prime", length(main))

#Data frame for Amazon Prime:
amazon_prime <- data.frame(serial, name, plat, URL, s.year, certificate, runtime, genre, ov.rating, ov.votes, stringsAsFactors = F)


##==========================================================================================================================================

####Scrapping data of voot from IMDB: 

#loading site
website <- read_html("https://www.imdb.com/search/title/?companies=co0603531")

#loading chuck of code for different web series individually
main <- website %>% html_elements(".lister-item.mode-advanced")

#defining blank vectors for the columns needed
name = codes = URL = certificate = genre = vector(length = length(main))
s.year = runtime = ov.rating = ov.votes = numeric(length = length(main))

#for loop for scrapping entries one by one with individual web series
for (i in 1:length(main))
{
  #Scrapping name:
  name[i] <- main[i] %>% html_elements(".lister-item-header a") %>% html_text()
  
  #Scrapping codes and individual sites:
  codes[i] <- main[i] %>% html_elements(".loadlate") %>% html_attr("data-tconst")
  URL[i] <- paste("https://www.imdb.com/title/", codes[i], "/", sep = "")
  
  #Scrapping starting year:
  s.year[i] <- main[i] %>% html_elements(".lister-item-year.text-muted.unbold") %>% html_text() %>% str_sub(2,5) %>% as.numeric()
  
  #Scrapping certificate:
  if (  length(  main[i] %>% html_elements(".text-muted .certificate") %>% html_text()  ) ==  0 ) #if else for handeling missing values
  {
    certificate[i] <- NA
  }
  else
  {
    certificate[i] <- main[i] %>% html_elements(".text-muted .certificate") %>% html_text()
  }
  
  #Scrapping runtime:
  if (  length(main[i] %>% html_elements(".text-muted .runtime") %>% html_text() %>% str_sub(1,2) %>% as.numeric()) ==  0 ) #if else for handeling missing values
  {
    runtime[i] <- NA
  }
  else
  {
    runtime[i] <- main[i] %>% html_elements(".text-muted .runtime") %>% html_text() %>% str_sub(1,2) %>% as.numeric()
  }
  
  #Scrapping genre
  genre[i] <- main[i] %>% html_elements(".text-muted .genre") %>% html_text() %>% str_sub(2,) %>% str_trim()
  
  #Scrapping overall ratings:
  if (  length(  main[i] %>% html_elements(".inline-block.ratings-imdb-rating strong") %>% html_text() %>% as.numeric()  ) ==  0 ) #if else for handeling missing values
  {
    ov.rating[i] <- NA
  }
  else
  {
    ov.rating[i] <- main[i] %>% html_elements(".inline-block.ratings-imdb-rating strong") %>% html_text() %>% as.numeric()
  }
  
  #Scrapping overall votes:
  dummy3 <- main[i] %>% html_elements(".sort-num_votes-visible span") %>% html_text()
  ov.votes[i] <- dummy3[2] %>% str_replace_all(",", "") %>% as.numeric()
  
}

#creating vector for Serial No:
serial <- seq(1:length(main))

#creating vector for Platform:
plat <- rep("Voot Select", length(main))

#Data frame for Voot Select:
voot_select <- data.frame(serial, name, plat, URL, s.year, certificate, runtime, genre, ov.rating, ov.votes, stringsAsFactors = F)


##==========================================================================================================================================

####Scrapping data of MX Player from IMDB: 

#loading site
website <- read_html("https://www.imdb.com/search/title/?companies=co0721599")

#loading chuck of code for different web series individually
main <- website %>% html_elements(".lister-item.mode-advanced")

#defining blank vectors for the columns needed
name = codes = URL = certificate = genre = vector(length = length(main))
s.year = runtime = ov.rating = ov.votes = numeric(length = length(main))

#for loop for scrapping entries one by one with individual web series
for (i in 1:length(main))
{
  #Scrapping name:
  name[i] <- main[i] %>% html_elements(".lister-item-header a") %>% html_text()
  
  #Scrapping codes and individual sites:
  codes[i] <- main[i] %>% html_elements(".loadlate") %>% html_attr("data-tconst")
  URL[i] <- paste("https://www.imdb.com/title/", codes[i], "/", sep = "")
  
  #Scrapping starting year:
  dummy1 <- main[i] %>% html_elements(".lister-item-header .lister-item-year.text-muted.unbold") %>% html_text()
  s.year[i] <- as.numeric(  str_sub(  str_trim(  str_replace_all(  str_replace_all(dummy1, "[[:punct:]]", "")  , "I", "")  )  , 1, 4)   )
  
  #Scrapping certificate:
  if (  length(  main[i] %>% html_elements(".text-muted .certificate") %>% html_text()  ) ==  0 ) #if else for handeling missing values
  {
    certificate[i] <- NA
  }
  else
  {
    certificate[i] <- main[i] %>% html_elements(".text-muted .certificate") %>% html_text()
  }
  
  #Scrapping runtime:
  if (  length(main[i] %>% html_elements(".text-muted .runtime") %>% html_text() %>% str_sub(1,2) %>% as.numeric()) ==  0 ) #if else for handeling missing values
  {
    runtime[i] <- NA
  }
  else
  {
    runtime[i] <- main[i] %>% html_elements(".text-muted .runtime") %>% html_text() %>% str_sub(1,2) %>% as.numeric()
  }
  
  #Scrapping genre
  genre[i] <- main[i] %>% html_elements(".text-muted .genre") %>% html_text() %>% str_sub(2,) %>% str_trim()
  
  #Scrapping overall ratings:
  if (  length(  main[i] %>% html_elements(".inline-block.ratings-imdb-rating strong") %>% html_text() %>% as.numeric()  ) ==  0 ) #if else for handeling missing values
  {
    ov.rating[i] <- NA
  }
  else
  {
    ov.rating[i] <- main[i] %>% html_elements(".inline-block.ratings-imdb-rating strong") %>% html_text() %>% as.numeric()
  }
  
  #Scrapping overall votes:
  if (  length(  main[i] %>% html_elements(".sort-num_votes-visible span") %>% html_text()  ) ==  0 ) #if else for handeling missing values
  {
    ov.votes[i] <- NA
  }
  else
  {
    dummy3 <- main[i] %>% html_elements(".sort-num_votes-visible span") %>% html_text()
    ov.votes[i] <- dummy3[2] %>% str_replace_all(",", "") %>% as.numeric()
  }
  
}

#creating vector for Serial No:
serial <- seq(1:length(main))

#creating vector for Platform:
plat <- rep("MX Player", length(main))

#Data frame for MX Player:
mx_player <- data.frame(serial, name, plat, URL, s.year, certificate, runtime, genre, ov.rating, ov.votes, stringsAsFactors = F)


##==========================================================================================================================================

####Scrapping data of Sony LIV from IMDB: 

#loading site
website <- read_html("https://www.imdb.com/search/title/?companies=co0546496")

#loading chuck of code for different web series individually
main <- website %>% html_elements(".lister-item.mode-advanced")

#defining blank vectors for the columns needed
name = codes = URL = certificate = genre = vector(length = 48)
s.year = runtime = ov.rating = ov.votes = numeric(length = 48)

#for loop for scrapping entries one by one with individual web series
for (i in 1:48)
{
  #Scrapping name:
  name[i] <- main[i] %>% html_elements(".lister-item-header a") %>% html_text()
  
  #Scrapping codes and individual sites:
  codes[i] <- main[i] %>% html_elements(".loadlate") %>% html_attr("data-tconst")
  URL[i] <- paste("https://www.imdb.com/title/", codes[i], "/", sep = "")
  
  #Scrapping starting year:
  dummy1 <- main[i] %>% html_elements(".lister-item-header .lister-item-year.text-muted.unbold") %>% html_text()
  s.year[i] <- as.numeric(  str_sub(  str_trim(  str_remove_all(  str_remove_all(  str_remove_all(  str_replace_all(dummy1, "[[:punct:]]", "")  , "X")  , "V")  , "I")  )  , 1, 4)   )
  
  #Scrapping certificate:
  if (  length(  main[i] %>% html_elements(".text-muted .certificate") %>% html_text()  ) ==  0 ) #if else for handeling missing values
  {
    certificate[i] <- NA
  }
  else
  {
    certificate[i] <- main[i] %>% html_elements(".text-muted .certificate") %>% html_text()
  }
  
  #Scrapping runtime:
  if (  length(  main[i] %>% html_elements(".text-muted .runtime") %>% html_text() %>% str_sub(1,2) %>% as.numeric()  ) ==  0 ) #if else for handeling missing values
  {
    runtime[i] <- NA
  }
  else
  {
    runtime[i] <- main[i] %>% html_elements(".text-muted .runtime") %>% html_text() %>% str_sub(1,2) %>% as.numeric()
  }
  
  #Scrapping genre:
  if (  length(  main[i] %>% html_elements(".text-muted .genre") %>% html_text() %>% str_sub(2,) %>% str_trim()  ) ==  0 ) #if else for handeling missing values
  {
    genre[i] <- NA
  }
  else
  {
    genre[i] <- main[i] %>% html_elements(".text-muted .genre") %>% html_text() %>% str_sub(2,) %>% str_trim()
  }
  
  #Scrapping overall ratings:
  if (  length(  main[i] %>% html_elements(".inline-block.ratings-imdb-rating strong") %>% html_text() %>% as.numeric()  ) ==  0 ) #if else for handeling missing values
  {
    ov.rating[i] <- NA
  }
  else
  {
    ov.rating[i] <- main[i] %>% html_elements(".inline-block.ratings-imdb-rating strong") %>% html_text() %>% as.numeric()
  }
  
  #Scrapping overall votes:
  if (  length(  main[i] %>% html_elements(".sort-num_votes-visible span") %>% html_text()  ) ==  0 ) #if else for handeling missing values 
  {
    ov.votes[i] <- NA
  }
  else
  {
    dummy3 <- main[i] %>% html_elements(".sort-num_votes-visible span") %>% html_text()
    ov.votes[i] <- dummy3[2] %>% str_replace_all(",", "") %>% as.numeric()
  }
  
}

#creating vector for Serial No:
serial <- seq(1:48)

#creating vector for Platform:
plat <- rep("Sony LIV", 48)

#Data frame for Sony LIV:
sony_liv <- data.frame(serial, name, plat, URL, s.year, certificate, runtime, genre, ov.rating, ov.votes, stringsAsFactors = F)


##==========================================================================================================================================

####Scrapping data of Hotstar from IMDB:

#loading site
website <- read_html("https://www.imdb.com/search/title/?companies=co0800964")

#loading chuck of code for different web series individually
main <- website %>% html_elements(".lister-item.mode-advanced")

#defining blank vectors for the columns needed
name = codes = URL = certificate = genre = vector(length = 10)
s.year = runtime = ov.rating = ov.votes = numeric(length = 10)

#for loop for scrapping entries one by one with individual web series
for (i in 1:10)
{
  #Scrapping name:
  name[i] <- main[i] %>% html_elements(".lister-item-header a") %>% html_text()
  
  #Scrapping codes and individual sites:
  codes[i] <- main[i] %>% html_elements(".loadlate") %>% html_attr("data-tconst")
  URL[i] <- paste("https://www.imdb.com/title/", codes[i], "/", sep = "")
  
  #Scrapping starting year:
  dummy1 <- main[i] %>% html_elements(".lister-item-header .lister-item-year.text-muted.unbold") %>% html_text()
  s.year[i] <- as.numeric(  str_sub(  str_trim(  str_remove_all(  str_remove_all(  str_remove_all(  str_replace_all(dummy1, "[[:punct:]]", "")  , "X")  , "V")  , "I")  )  , 1, 4)   )
  
  #Scrapping certificate:
  certificate[i] <- main[i] %>% html_elements(".text-muted .certificate") %>% html_text()
  
  #Scrapping runtime:
  if (  length(  main[i] %>% html_elements(".text-muted .runtime") %>% html_text() %>% str_sub(1,2) %>% as.numeric()  ) ==  0 ) #if else for handeling missing values
  {
    runtime[i] <- NA
  }
  else
  {
    runtime[i] <- main[i] %>% html_elements(".text-muted .runtime") %>% html_text() %>% str_sub(1,2) %>% as.numeric()
  }
  
  #Scrapping genre:
  genre[i] <- main[i] %>% html_elements(".text-muted .genre") %>% html_text() %>% str_sub(2,) %>% str_trim()
  
  #Scrapping overall ratings:
  ov.rating[i] <- main[i] %>% html_elements(".inline-block.ratings-imdb-rating strong") %>% html_text() %>% as.numeric()
  
  #Scrapping overall votes:
  dummy3 <- main[i] %>% html_elements(".sort-num_votes-visible span") %>% html_text()
  ov.votes[i] <- dummy3[2] %>% str_replace_all(",", "") %>% as.numeric()
  
}

#creating vector for Serial No:
serial <- seq(1:10)

#creating vector for Platform:
plat <- rep("Hotstar", 10)

#Data frame for Hotstar:
hotstar <- data.frame(serial, name, plat, URL, s.year, certificate, runtime, genre, ov.rating, ov.votes, stringsAsFactors = F)


##==========================================================================================================================================

####Scrapping data of zee 5 from IMDB: 

#loading site
website <- read_html("https://www.imdb.com/search/title/?companies=co0692549&sort=user_rating,desc")

#loading chuck of code for different web series individually
main <- website %>% html_elements(".lister-item.mode-advanced")

#defining blank vectors for the columns needed
name = codes = URL = certificate = genre = vector(length = length(main))
s.year = runtime = ov.rating = ov.votes = numeric(length = length(main))

#for loop for scrapping entries one by one with individual web series
for (i in 1:length(main))
{
  #Scrapping name:
  name[i] <- main[i] %>% html_elements(".lister-item-header a") %>% html_text()
  
  #Scrapping codes and individual sites:
  codes[i] <- main[i] %>% html_elements(".loadlate") %>% html_attr("data-tconst")
  URL[i] <- paste("https://www.imdb.com/title/", codes[i], "/", sep = "")
  
  #Scrapping starting year:
  s.year[i] <- main[i] %>% html_elements(".lister-item-year.text-muted.unbold") %>% html_text() %>% str_sub(2,5) %>% as.numeric()
  
  #Scrapping certificate:
  if (  length(  main[i] %>% html_elements(".text-muted .certificate") %>% html_text()  ) ==  0 ) #if else for handeling missing values
  {
    certificate[i] <- NA
  }
  else
  {
    certificate[i] <- main[i] %>% html_elements(".text-muted .certificate") %>% html_text()
  }
  
  #Scrapping runtime:
  if (  length(  main[i] %>% html_elements(".text-muted .runtime") %>% html_text() %>% str_sub(1,2) %>% as.numeric()  ) ==  0 ) #if else for handeling missing values
  {
    runtime[i] <- NA
  }
  else
  {
    runtime[i] <- main[i] %>% html_elements(".text-muted .runtime") %>% html_text() %>% str_sub(1,2) %>% as.numeric()
  }
  
  #Scrapping genre:
  genre[i] <- main[i] %>% html_elements(".text-muted .genre") %>% html_text() %>% str_sub(2,) %>% str_trim()
  
  #Scrapping overall ratings:
  ov.rating[i] <- main[i] %>% html_elements(".inline-block.ratings-imdb-rating strong") %>% html_text() %>% as.numeric()
  
  #Scrapping overall votes:
  dummy3 <- main[i] %>% html_elements(".sort-num_votes-visible span") %>% html_text()
  ov.votes[i] <- dummy3[2] %>% str_replace_all(",", "") %>% as.numeric()
  
}

#creating vector for Serial No:
serial <- seq(1:length(main))

#creating vector for Platform:
plat <- rep("Zee5", length(main))

#Data frame for Zee5:
zee5 <- data.frame(serial, name, plat, URL, s.year, certificate, runtime, genre, ov.rating, ov.votes, stringsAsFactors = F)


##==========================================================================================================================================

####Scrapping data of tvf play from IMDB: 

#loading site
website <- read_html("https://www.imdb.com/search/keyword/?keywords=tvf")

#loading chuck of code for different web series individually
main <- website %>% html_elements(".lister-item.mode-detail")

#defining blank vectors for the columns needed
name = codes = URL = certificate = genre = vector(length = length(main))
s.year = runtime = ov.rating = ov.votes = numeric(length = length(main))

#for loop for scrapping entries one by one with individual web series
for (i in 1:length(main))
{
  #Scrapping name:
  name[i] <- main[i] %>% html_elements(".lister-item-header a") %>% html_text()
  
  #Scrapping codes and individual sites:
  codes[i] <- main[i] %>% html_elements(".loadlate") %>% html_attr("data-tconst")
  URL[i] <- paste("https://www.imdb.com/title/", codes[i], "/", sep = "")
  
  #Scrapping starting year:
  s.year[i] <- main[i] %>% html_elements(".lister-item-year.text-muted.unbold") %>% html_text() %>% str_sub(2,5) %>% as.numeric()
  
  #Scrapping certificate:
  if (  length(  main[i] %>% html_elements(".text-muted .certificate") %>% html_text()  ) ==  0 ) #if else for handeling missing values
  {
    certificate[i] <- NA
  }
  else
  {
    certificate[i] <- main[i] %>% html_elements(".text-muted .certificate") %>% html_text()
  }
  
  #Scrapping runtime:
  if (  length(  main[i] %>% html_elements(".text-muted .runtime") %>% html_text() %>% str_sub(1,2) %>% as.numeric()  ) ==  0 ) #if else for handeling missing values
  {
    runtime[i] <- NA
  }
  else
  {
    runtime[i] <- main[i] %>% html_elements(".text-muted .runtime") %>% html_text() %>% str_sub(1,2) %>% as.numeric()
  }
  
  #Scrapping genre:
  genre[i] <- main[i] %>% html_elements(".text-muted .genre") %>% html_text() %>% str_sub(2,) %>% str_trim()
  
  #Scrapping overall ratings:
  ov.rating[i] <- main[i] %>% html_elements(".inline-block.ratings-imdb-rating strong") %>% html_text() %>% as.numeric()
  
  #Scrapping overall votes:
  dummy <- main[i] %>% html_elements(".text-muted.text-small span") %>% html_text()
  ov.votes[i] <- dummy[length(dummy)] %>% str_remove_all(",") %>% as.numeric()
  
}

#creating vector for Serial No:
serial <- seq(1:length(main))

#creating vector for Platform:
plat <- rep("TVF", length(main))

#Data frame for TVF:
tvf <- data.frame(serial, name, plat, URL, s.year, certificate, runtime, genre, ov.rating, ov.votes, stringsAsFactors = F)


#=======================================================================================================================
#=======================================================================================================================

#Creating a dataframe with 10 columns:

data <- rbind(alt_balaji, amazon_prime, hotstar, mx_player, sony_liv, tvf, voot_select, zee5)
data[ , 1] <- 1:dim(data)[1]

#=======================================================================================================================

#####scrapping ov.rating and ov.votes w.r.t. male and female :

#creating vector of ratings page of each website
rating_URL <- paste(  data$URL, "ratings/", sep = ""  )

#creating blank vectors for the remaining columns
m.ov.rating = m.ov.votes = f.ov.rating = f.ov.votes = numeric(length = dim(data)[1])

#for loop to go to ratings webpage of each webseries
for (i in 1:length(rating_URL))
{
  #refernces for not getting bored
  print(paste("Entry no", i, sep = " "))
  
  #loading site of i-th web series
  website <- read_html(rating_URL[i])
  
  #extracting tables of the page
  tables <- website %>% html_table()
  
  #if else in case of unavailability of any table
  if (length(tables) == 0)
  {
    m.ov.rating[i] = NA
    m.ov.votes[i] = NA
    f.ov.rating[i] = NA
    f.ov.votes[i] = NA
  }
  else
  {
    #going to 2nd table of the page
    table <- as.data.frame( tables[[2]])
    
    #Scrapping male overall ratings:
    if (table[2,2] == "-") #if else for handeling missing values
    {
      m.ov.rating[i] = NA
    }
    else
    {
      m.ov.rating[i] = as.numeric(str_sub(table[2,2], 1,3))
    }
    
    #Scrapping male overall votes:
    if (table[2,2] == "-") #if else for handeling missing values
    {
      m.ov.votes[i] = NA
    }
    else
    {
      m.ov.votes[i] = as.numeric(str_remove_all(str_sub(table[2,2], 55, ), ","))
    }
    
    #Scrapping female overall ratings:
    if (table[3,2] == "-") #if else for handeling missing values
    {
      f.ov.rating[i] = NA
    }
    else
    {
      f.ov.rating[i] = as.numeric(str_sub(table[3,2], 1,3)) 
    }
    
    #Scrapping female overall votes:
    if (table[3,2] == "-") #if else for handeling missing values
    {
      f.ov.votes[i] = NA
    }
    else
    {
      f.ov.votes[i] = as.numeric(str_remove_all(str_sub(table[3,2], 55, ), ","))
    }
  }
  
}


#=======================================================================================================================
#=======================================================================================================================

#The final dataframe:

#adding the four scrapped columns
data$m.ov.rating <- m.ov.rating
data$m.ov.votes <- m.ov.votes
data$f.ov.rating <- f.ov.rating
data$f.ov.votes <- f.ov.votes

#Naming the columns of the dataframe
colnames(data) <- c("Serial No", "Name", "Platform", "Individual Website", "Starting Year", "Certificate", "Run Time", "Genre", "Overall Rating", "Overall Votes", "Male Rating", "Male Votes", "Female Rating", "Female Votes") 

#Saving the data frame in data.Rdata file in working directory
save(data, file = "data.Rdata")

