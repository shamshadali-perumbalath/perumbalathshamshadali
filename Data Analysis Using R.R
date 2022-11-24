################################################################################ 1 install packages 
# install.packages("tidyverese")
library(tidyverse)

# before starting make sure your working directory is set to grasp the files using 
# getwd()
# setwd()


################################################################################ 2 importing data into r 
## Read and Write Data into files 

# Reading web files

page = read_html("https://en.wikipedia.org/w/index.php?title=Template:COVID-19_testing_by_country")
page
tables= html_nodes(page, "table")
tables
table1= html_table(tables, header= TRUE, fill= TRUE)[[1]]
table1
table2=  html_table(tables, header= TRUE, fill= TRUE)[[2]]
table2

preprocess_covid_data_frame <- function(data_frame) {
  
  shape <- dim(data_frame)
  
  # Remove the World row
  data_frame<-data_frame[!(data_frame$`Country or region`=="World"),]
  # Remove the last row
  data_frame <- data_frame[1:172, ]
  
  # We dont need the Units and Ref columns, so can be removed
  data_frame["Ref."] <- NULL
  data_frame["Units[b]"] <- NULL
  
  # Renaming the columns
  names(data_frame) <- c("country", "date", "tested", "confirmed", "confirmed.tested.ratio", "tested.population.ratio", "confirmed.population.ratio")
  
  # Convert column data types
  data_frame$country <- as.factor(data_frame$country)
  data_frame$date <- as.factor(data_frame$date)
  data_frame$tested <- as.numeric(gsub(",","",data_frame$tested))
  data_frame$confirmed <- as.numeric(gsub(",","",data_frame$confirmed))
  data_frame$'confirmed.tested.ratio' <- as.numeric(gsub(",","",data_frame$`confirmed.tested.ratio`))
  data_frame$'tested.population.ratio' <- as.numeric(gsub(",","",data_frame$`tested.population.ratio`))
  data_frame$'confirmed.population.ratio' <- as.numeric(gsub(",","",data_frame$`confirmed.population.ratio`))
  
  return(data_frame)
}







#Reading csv files
    # download file from web(as csv) and read  or

      # url where the data is located
      url = "https://dax-cdn.cdn.appdomain.cloud/dax-airline/1.0.1/lax_to_jfk.tar.gz"
      # download the file
      # download.file(url, destfile = "lax_to_jfk.tar.gz")
      # untar the file so we can get the csv only
      untar("lax_to_jfk.tar.gz", tar = "internal")
      #  read_csv only 
      sub_airline <- read_csv("lax_to_jfk/lax_to_jfk.csv",
                        col_types = cols(
                          'DivDistance' = col_number(),
                          'DivArrDelay' = col_number()
                        ))

      sub_airline


    # stored csv file and read 

      # save the SCV file into your working directory folder as "AirLines_DataSet_CSV" or the name you wish
      sub_airline2 <- read_csv("AirLines_DataSet_CSV.csv")
      sub_airline2

  #write these files into CSV 
  write_csv(sub_airline2, "sub_airline2CSV.csv")
  #fine a new files called sub_airline2CSV in folder
  
  
  #write these files into excel
  # install.packages("writexl")
  library(writexl)
  write_xlsx(sub_airline2, "sub_airline2Excel.xlsx" )
  #fine a new files called sub_airline2Excel in folder 
  
  
#reading excel Files
  #stored excel (from csv write) and read
  # install.packages("readxl")
  library(readxl)
  sub_airline3= read_excel("sub_airline2Excel.xlsx")
  sub_airline3
  
  # if your data in sheet 2 specify it using an additional command
  # sub_airline3= read_excel("sub_airline2Excel.xlsx", sheet = 2)
  # sub_airline3
  
  # sub_airline3= read_excel("sub_airline2Excel.xlsx", sheet = "AirData")
  # sub_airline3
  






# the final data set for analysis from CSV file
  
  # url where the data is located
  # 1. url <- "https://dax-cdn.cdn.appdomain.cloud/dax-airline/1.0.1/airline_2m.tar.gz"
  # this include million data set
  
  # download the file --------------------------------- This may take more time wait------------------ 
  #  better to download data set (https://dax-cdn.cdn.appdomain.cloud/dax-airline/1.0.1/airline_2m.tar.gz) into folder manually and read the file
  # else it will take much longer time to get accesses 
  
  # 2. download.file(url, destfile = "airline_2m.tar.gz")
  

  
  
  # --------------------- after saving big file into folder ------------------------- or 
  # after step 1. and 2.
  
  # untar the file so we can get the csv only
  untar("airline_2m.tar.gz") #if its not working then extract file to working directory
  # read_csv only 
  
  airlines_data1= read.csv("airline_2m.csv")
  str(airlines_data1) #every data are chr 
  
  
  airlines_data = read_csv("airline_2m.csv",
                           col_types = cols(
                             'DivDistance' = col_number(),
                             'Div1Airport' = col_character(),
                             'Div1AirportID' = col_character(),
                             'Div1AirportSeqID' = col_character(),
                             'Div1WheelsOn' = col_character(),
                             'Div1TotalGTime' = col_number(),
                             'Div1LongestGTime' = col_number(),
                             'DivReachedDest' = col_number(),
                             'DivActualElapsedTime' = col_number(),
                             'DivArrDelay' = col_number(),
                             'Div1WheelsOff'= col_character(),
                             'Div1TailNum' = col_character(),
                             'Div2Airport' = col_character(),
                             'Div2AirportID' = col_character(),
                             'Div2AirportSeqID' = col_character(),
                             'Div2WheelsOn' = col_character(),
                             'Div2TotalGTime' = col_number(),
                             'Div2LongestGTime' = col_number(),
                             'Div2WheelsOff' = col_character(),
                             'Div2TailNum' = col_character()
                           ))
  str(airlines_data)
  # We are going to be focusing on flights from  LAX to JFK and we will exclude
  # flights that got cancelled or diverted
  # we are also going to get only useful columns
  sub_airline <- airlines %>% 
    filter(Origin == "LAX", Dest == "JFK", 
           Cancelled != 1, Diverted != 1) %>% 
    select(Month, DayOfWeek, FlightDate, 
           Reporting_Airline, Origin, Dest, 
           CRSDepTime, CRSArrTime, DepTime, 
           ArrTime, ArrDelay, ArrDelayMinutes, 
           CarrierDelay, WeatherDelay, NASDelay,
           SecurityDelay, LateAircraftDelay, DepDelay, 
           DepDelayMinutes, DivDistance, DivArrDelay)



################################################################################ 3 Understand the data frame
# dim()
# str()
# glipmse()
# head()
# tail()
# colnames()