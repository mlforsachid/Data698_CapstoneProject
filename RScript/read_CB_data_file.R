#### Function to load up a CitiBike datafile
read_CB_data_file = function(f){
  Startloadtime = Sys.time()
  #print(paste("reading data file   ", f, " at ", Startloadtime))
  
  ### Extract the year and month from the datafile.  Needed below for inconsistent date/time formats by month.
  YYYYMM <- sub("^.*/","",f) %>% sub("-citibike-tripdata.csv","",.)
  #print(paste("YYYYMM = ", YYYYMM))
  
  
  ### Read the datafile according to the format specifications
  datafile = read_csv(f,skip = 1,
                      ## The column names have slight format differences across months.  So, replace all column names with these:
                      col_names=c("trip_duration",            # in seconds
                                  "s_time",                   # start date/time
                                  "e_time",                   # end date/time
                                  "s_station_id",             # station ID for beginning of trip 
                                  "s_station_name", 
                                  "s_lat",                    # start station latitude
                                  "s_long",                   # start station longitude
                                  "e_station_id",             # station ID for end of trip
                                  "e_station_name",
                                  "e_lat",                    # latitude
                                  "e_long",                   # longitude
                                  "bike_id",                  # every bike has a 5-digit ID number
                                  "user_type",                # Annual Subscriber or Daily Customer
                                  "birth_year",               # Can infer age from this
                                  "gender")                   # 1=Male,2=Female,0=unknown
                      #                  ,col_types = "dTTffddffddifif"    # d=decimal; T=datetime; f=factor; i=integer
                      ### specify the data type for each of the above columns
                      ### Note: because of changes in the format across months, we will  have to read the date/time as char for now
                      ### also we will have to read the birth_year as char for now because of missing data (either "\\N" or "NULL")
                      ,col_types = "dccffddffddifcf"    # d=decimal; c=character; f=factor; i=integer
  )
  Endloadtime = Sys.time()
  #print(paste("done reading data file ",  f, " at ", Endloadtime))
  Totalloadtime = round(Endloadtime - Startloadtime, 2)
  #print(paste("Totaltime = ", Totalloadtime))
  
  ## Fix format changes on time and birth_year variables 
  s_time <- pull(.data=datafile, var = "s_time")
  e_time <- pull(.data=datafile, var = "e_time")
  
  ### Early and recent files use format "%Y-%m-%d %H:%M:%OS"
  if (YYYYMM < "201409" | YYYYMM > "201609") timeformat="%Y-%m-%d %H:%M:%OS"
  
  ### time between the months uses format "%m/%d/%Y %H:%M:%OS"
  if (YYYYMM >= "201409" & YYYYMM <= "201609") timeformat="%m/%d/%Y %H:%M:%OS"
  ### except for the first 3 months of 2015, time is only HH:MM -- no seconds!
  if (YYYYMM >= "201501" & YYYYMM <= "201503") timeformat="%m/%d/%Y %H:%M"
  ### Same for June 2015, time is only HH:MM -- no seconds!
  if (YYYYMM == "201506") timeformat="%m/%d/%Y %H:%M"
  
  datafile[,"s_time"] <- as.POSIXct(s_time, format=timeformat)
  datafile[,"e_time"] <- as.POSIXct(e_time, format=timeformat)
  
  #### note:  on the first Sunday of November, clocks move back 1 hour.
  #### This means that the hour 1am-2am EDT is followed by the hour 1am-2am EST.
  #### If a bicycle was rented during this hour "EDT",
  #### but returned during the subsequent hour "EST",
  #### then the trip duration could appear negative.
  #### This is because the default loader will assume all times on this date are EST.
  #### In this case, the below will force such start-times back an hour:
  
  iii = which(datafile$s_time>datafile$e_time)
  if(length(iii)>0) {
    #print("***DAYLIGHT SAVINGS PROBLEM***")
    #print(datafile[iii,])
    #print("**Start times:")
    #print(pull(datafile[iii,2]))
    #print(pull(datafile[iii,2]) %>% as.numeric())
    #print(unclass(datafile[iii,2])$s_time)
    #print("**End times:")
    #print(pull(datafile[iii,3]))
    #print(pull(datafile[iii,3]) %>% as.numeric())
    #print(unclass(datafile[iii,3])$e_time)
    
    
    #print("***CHANGING s_time backward***")
    new_s_time <- ifelse(datafile$s_time>datafile$e_time,
                         datafile$s_time-60*60,  # pushes back 1 hour from EST to EDT 
                         datafile$s_time) %>% as.POSIXct(., origin= "1970-01-01")
    #print("***CHANGING e_time forward***")
    new_e_time <- ifelse(datafile$s_time>datafile$e_time,
                         datafile$e_time+60*60,  # pushes forward 1 hour from EDT to EST 
                         datafile$e_time) %>% as.POSIXct(., origin= "1970-01-01")
    before_diff <-  datafile[iii,3] - datafile[iii,2] 
    #print(paste("BEFORE difference: ", before_diff))
    
    datafile[,"s_time"] <- new_s_time
    datafile[,"e_time"] <- new_e_time
    
    #print("**AFTER CHANGE**")
    #print(datafile[iii,])
    #print("**Start times**")
    #print(pull(datafile[iii,2]))
    #print(pull(datafile[iii,2]) %>% as.numeric())
    #print(unclass(datafile[iii,2])$s_time)
    #print("**End times**")
    #print(pull(datafile[iii,3]))
    #print(pull(datafile[iii,3]) %>% as.numeric())
    #print(unclass(datafile[iii,3])$e_time)
    
    after_diff <-  datafile[iii,3] - datafile[iii,2] 
    #print(paste("AFTER difference: ", after_diff))
    
    
  }  
  
  ##
  ## set missing birth years to NA  
  birth_year <- pull(.data=datafile, var = "birth_year")
  ## Fix missing birth year on early data (occurs when YYYYMM < "201409")
  birth_year[birth_year=='\\N']<-NA
  ## Fix missing birth year on 2017 (occurs when "201704" YYYYMM < "201712")
  birth_year[birth_year=='NULL']<-NA
  ## Convert the available birth_years to their integer equivalents (while retaining above NAs)
  datafile[,"birth_year"] <- as.integer(birth_year)
  
  ## There are numerous cases between 201610 and 201703 where the usertype is not specified.
  ## (It should be "Subscriber" or "Customer", but in such cases it is blank.)  
  ## We will set it to "UNKNOWN"  
  
  #library(forcats)   # loaded above
  datafile$user_type<-fct_explicit_na(datafile$user_type, "UNKNOWN")
  
  ## There was a trial of DOCKLESS BIKES in the Bronx starting from August 2018:
  ## https://nyc.streetsblog.org/2018/08/16/a-hit-and-miss-debut-for-dockless-citi-bikes-in-the-bronx/
  ## https://d21xlh2maitm24.cloudfront.net/nyc/bronx-service-area-map.png?mtime=20180809110452
  ## https://webcache.googleusercontent.com/search?q=cache:9Xz02WSdeOYJ:https://www.citibikenyc.com/how-it-works/dockless-faqs+
  
  ## For these trips, the latitute and longitude of the bike start and stop is given, but
  ## the start and end station ID and station name are set to "NULL" in the input datafiles.
  ## For clarity, we will change such station_id values to 0 and station_name values to "DOCKLESS" :
  levels(datafile$s_station_id)[levels(datafile$s_station_id)=="NULL"] <- 0
  levels(datafile$s_station_name)[levels(datafile$s_station_name)=="NULL"] <- "DOCKLESS"
  levels(datafile$e_station_id)[levels(datafile$e_station_id)=="NULL"] <- 0
  levels(datafile$e_station_name)[levels(datafile$e_station_name)=="NULL"] <- "DOCKLESS"
  
  
  ## for certain months, the datafile is not sorted on s_time (instead it is sorted on s_station_id then s_time)
  ## ensure that this month's data is sorted on s_time
  #datafile <- datafile[order(datafile$s_time),]
  return(datafile)
}
