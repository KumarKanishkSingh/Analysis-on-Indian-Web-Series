#loading data.Rdata file
load("data.Rdata")

#####assigning certificate category :

#crearting blank vector for certificate category columns
c_cer <- vector(length = dim(data)[1])

#for loop to create certificate category of each web series
for (i in 1:dim(data)[1]) 
{
  if (is.na(data$Certificate[i])) #if else for handeling missing values
  {
    c_cer[i] <- NA
  }
  else 
  {
    #if else if else condition for catgorising
    if(data$Certificate[i] == "12+"||data$Certificate[i] =="All"||data$Certificate[i] =="UA 7+"||data$Certificate[i] =="U"||data$Certificate[i] =="7+")
    {
      c_cer[i]="Family and Kids"
    }
    else if(data$Certificate[i] == "UA 16+"||data$Certificate[i] =="UA"||data$Certificate[i] =="U/A"||data$Certificate[i] =="PG"||data$Certificate[i] =="16+"||data$Certificate[i] =="16"||data$Certificate[i] =="13"||data$Certificate[i] =="15+"||data$Certificate[i] =="UA 13+")
    {
      c_cer[i]="Teenagers"
    }
    else if(data$Certificate[i] == "18+"||data$Certificate[i] =="18"||data$Certificate[i] =="A")
    {
      c_cer[i]="Adults"
    }
    else
    {
      c_cer[i]=NA
    }
  }
}

#####assigning length category :

#crearting blank vector for length category columns
len.web <- vector(length = dim(data)[1])

#for loop to create length category of each web series
for (i in 1:dim(data)[1])
{
  if (is.na(data$`Run Time`[i])) #if else for handeling missing values
  {
    len.web[i] <- NA
  }
  else 
  {
    #if else if else condition for catgorising
    if(data$`Run Time`[i] %in% c(10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20))
    {
      len.web[i]="Short"
    }
    else if(data$`Run Time`[i] %in% c(21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 32, 35, 36, 38, 39, 40))
    {
      len.web[i]="Medium"
    }
    else if(data$`Run Time`[i] %in% c(42, 43, 45, 49, 50, 51, 54, 55, 57, 60, 80, 86, 98))
    {
      len.web[i]="Long"
    }
    else
    {
      len.web[i]=NA
    }
  }
}


#=======================================================================================================================
#=======================================================================================================================

#The final dataframe:

#adding the two columns scrapped
data$c_cer <- c_cer
data$len.web <- len.web

#naming the columns of the final data frame
colnames(data) <- c("Serial No", "Name", "Platform", "Individual Website", "Starting Year", "Certificate", "Run Time", "Genre", "Overall Rating", "Overall Votes", "Male Rating", "Male Votes", "Female Rating", "Female Votes", "Certificate Category", "Length Category") 

#Saving the final data frame in the finaldata.Rdata file
save(data, file = "finaldata.Rdata")
