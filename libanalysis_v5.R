GSR1_spot <- 65 #61 #Pivotal position, it may be necessary to update this, if there are some changes in the capacity of the library

importdata <- function(name) { #Imports raw data, deletes unused columns, proper column names are on first row
  library(readr) #Importing a required library for reading .csv file
  setwd("C:/Users/ABRA/Desktop/Senior 2020-2021/Student Associates/Library/Data Analysis")
  lib <- read_csv(name)
  lib <- lib[-c(2),c(5:8,19:ncol(lib))] #Deleting unnecessary parts
  lib <- lib[lib$`Finished`!= "False",] #Deleting uncompleted responses
  lib[1,5] <- "Weekday or weekend?"; lib[1,6] <- "WeekendTime" #Shortening the writings
  for (i in 7:(GSR1_spot-1)) {
    a <- as.character(lib[1,i])
    a <- substr(a,nchar(a)-10,nchar(a))
    lib[1,i] <- a
  }
  lib[1,GSR1_spot] <- "GSR1"; lib[1,GSR1_spot+1] <- "GSR2"; lib[1,GSR1_spot+2] <- "GSR3"
  for (i in (GSR1_spot+3):(ncol(lib)-6)) {
    a <- as.character(lib[1,i])
    a <- substr(a,nchar(a)-10,nchar(a))
    lib[1,i] <- a
  }
  #Changing the place of 8 new IS_L1 so that they come before SoftL
  lib <- lib[,c(1:52,57:64,53:56,65:ncol(lib))]
  lib[1,ncol(lib)-5] <- "GSR4"; lib[1,ncol(lib)-4] <- "GSR5"; lib[1,ncol(lib)-3] <- "GSR6";
  lib[1,ncol(lib)-2] <- "GSR7"; lib[1,ncol(lib)-1] <- "CompLab"; 
  lib[1,ncol(lib)] <- "WeekdayTime"
  colnames(lib) <- c(1:ncol(lib)) #Renaming cloumns with numbers
  return(lib)
}

specific_month <- function(lib,year,month) { #Creates a data frame for a specific month(string number)
  date <- paste(year,"-",month, sep = "")
  libhead <- lib[1,]
  lib <- lib[substr(lib$`4`,1,7)== date,]
  lib <- rbind(libhead,lib)
  row.names(lib) <- NULL
  return(lib)
}

##### WE ARE HERE !!!!!

weekend_data <- function(lib) {#Filters for weekend data
  libhead <- lib[1,]
  lib2 <- lib[lib$`5`== "Weekend",]
  lib3 <- rbind(libhead,lib2)
  return(lib3)
}
weekday_data <- function(lib) {#Filters for weekday data
  libhead <- lib[1,]
  lib2 <- lib[lib$`5`== "Weekday",]
  lib3 <- rbind(libhead,lib2)
  return(lib3)
}
monthfinder <- function(num) { #Returns the characters version of month number
  switch (num,
          "01" = return("January"),
          "02" = return("February"),
          "03" = return("March"),
          "04" = return("April"),
          "05" = return("May"),
          "06" = return("June"),
          "07" = return("July"),
          "08" = return("August"),
          "09" = return("September"),
          "10" = return("October"),
          "11" = return("November"),
          "12" = return("December"),
          message("Something is wrong_monthfinder")
  )
}

find_double_entry <- function(lib) {#Finds double entries and prints its row and col number
  Found <- FALSE
  a <- lib$`6` #weekend check
  prev <- ""
  for (i in 1:length(a)) { 
    if (!is.na(a[i]) & !is.na(prev) & a[i]==prev) {
      print("Duplicate found!")
      print(paste("Row number:", i))
      print(substr(lib[i,4],1,20))
      print(substr(lib[i,6],1,20))
      print("")
      Found <- TRUE
    }
    prev <- a[i]
  }
  b <- unname(unlist(lib[,ncol(lib)])) #weekday check
  prev <- ""
  for (i in 1:length(b)) { 
    if (!is.na(b[i]) & !is.na(prev) & b[i]==prev) {
      print("Duplicate found!")
      print(paste("Row number:", i))
      print(substr(lib[i,4],1,20))
      print(substr(lib[i,ncol(lib)],1,20))
      print("")
      Found <- TRUE
    }
    prev <- b[i]
  }
  if (!Found) {
    print("No duplicates found!")
  }
}

numcheck <- function(d1) {#Checks for NA or unvalid input
  select <- function(e1,e2) {
    if (is.na(e1)) {return(e2)}
    else {return(e1)}
  }
  data <- d1[-c(1),]
  no_mistake <- TRUE
  for (j in 1:nrow(data)) { 
    for (i in c(c(GSR1_spot:(GSR1_spot+2)),c((ncol(d1)-5):(ncol(d1)-1)))){
      if (is.na(as.numeric(data[j,i]))){
        no_mistake <- FALSE
        message(sprintf("Row number:%d Column Number:%d (%s)",(j+1),i,d1[1,i]))
        message(sprintf("Date: %s Shift: %s",data[j,4],select(data[j,6],data[j,ncol(d1)])))
        message(sprintf("Entered: %s\n-",data[j,i]))
      }
    }
  }
  if(no_mistake) {
    message("All inputs are valid!")
  }
} 

capacitycheck <- function(d1, showplace = FALSE) { # Checks for capacity limits
  capacity <- function(place) {
    if (place == GSR1_spot) {return(4)}
    else if (place == (GSR1_spot+1)) {return(4)}
    else if (place == (GSR1_spot+2)) {return(4)}
    else if (place == (ncol(d1)-5)) {return(5)}
    else if (place == (ncol(d1)-4)) {return(5)}
    else if (place == (ncol(d1)-3)) {return(7)}
    else if (place == (ncol(d1)-2)) {return(7)}
    else if (place == (ncol(d1)-1)) {return(26)}
    else {message("Inputs are wrong!")}
  }
  capacityCovid <- function(place) {
    if (place == GSR1_spot) {return(2)}
    else if (place == (GSR1_spot+1)) {return(2)}
    else if (place == (GSR1_spot+2)) {return(2)}
    else if (place == (ncol(d1)-5)) {return(2)}
    else if (place == (ncol(d1)-4)) {return(2)}
    else if (place == (ncol(d1)-3)) {return(3)}
    else if (place == (ncol(d1)-2)) {return(3)}
    else if (place == (ncol(d1)-1)) {return(26)}
    else {message("Inputs are wrong!")}
  }
  select <- function(e1,e2) {
    if (is.na(e1)) {return(e2)}
    else {return(e1)}
  }
  data <- d1[-c(1),]
  no_over <- TRUE
  for (j in 1:nrow(data)) { 
    for (i in c(c(GSR1_spot:(GSR1_spot+2)),c((ncol(d1)-5):(ncol(d1)-1)))){
      if (!is.na(as.numeric(data[j,i]))){
        amount <- as.numeric(data[j,i])
        cap <- capacityCovid(i)
        if(amount > cap) {
          no_over <- FALSE
          if(showplace) {
            message(sprintf("Row number:%d Column Number:%d",(j+1),i))
          }
          message(sprintf("(%s)",d1[1,i]))
          message(sprintf("Date: %s Shift: %s",data[j,4],select(data[j,6],data[j,ncol(d1)])))
          message(sprintf("Capacity: %d Entered: %d \n-",cap,amount))
        }
      }
    }
  }
  if(no_over) {
    message("No over capacity detected!")
  }
}

findmissing <- function(input) {#Finds missing entries
  weekend <- c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm","WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
  weekday <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm","WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
  dataset <- input[-c(1),]
  days <- c()
  a <- 1
  expected <- 0
  missing <- 0
  weekendmissing <- 0
  for (item in dataset$`4`) {
    day <- substr(item,9,10)
    if (day %in% days ) {
      next
    }
    else {
      days[a] <- day
      a <- a+1
      sublib <- dataset[substr(dataset$`4`,9,10)==day,]
      type <- toString(sublib[1,5])
      if (type == "Weekday") {
        expected <- expected + 9
        if (dim(sublib)[1] == 9){
        }
        else {
          subday <- c()
          b <- 1
          subsub <- unname(unlist(sublib[,ncol(sublib)])) #weekday check
          for (shift in subsub){
            subday[b] <- shift
            b <- b+1
          }
          print(day)
          diff <- setdiff(weekday, subday)
          print(diff)
          missing <- missing + length(diff)
          
        }
      }
      else { #if weekend
        expected <- expected + 8
        if (dim(sublib)[1] == 8){
        }
        else {
          subday <- c()
          b <- 1
          for (shift in sublib$`6`){
            subday[b] <- shift
            b <- b+1
          }
          print(day)
          diff <- setdiff(weekend, subday)
          print(diff)
          missing <- missing + length(diff)
          weekendmissing <- weekendmissing + length(diff)
        }
      }
    }
  }
  message(sprintf("The expected number of entry is: %i", expected))
  message(sprintf("The actual number of entry is: %i", (expected-missing)))
  message(sprintf("The number of missing entry is: %i", missing))
  perc <- 100*(missing/expected)
  message(sprintf("The percentage of missing entry is: %f",perc))
  message(sprintf("The number of missing entry on weekend is: %i", weekendmissing))
}

count_aggregate <- function(lib) {#For each space, counts the amount of like/dislike/neutral
  #Easy to remove/add new sections - you may have to edit impordata tho
  # First row(column row) is filled with dummy variables which will be replaced by actual names
  initial_ncol <- ncol(lib)
  # Main Stack (MS)
  temp <- data.frame("MS_Like" = rep(0,nrow(lib)), "MS_Dislike" = rep(0,nrow(lib)), "MS_Neutral" = rep(0,nrow(lib))) #Creating additional columns to store the accumulators
  lib <- cbind(lib,temp) #Combinig accumulator column with actual data
  startpos <- 7 #Start index for specific section
  capacity <- 16 #Capacity for specific section
  endpos <- startpos+capacity-1 #End index for specific section
  for (i in 1:nrow(lib)) {
    for (j in startpos:endpos) {
      if (lib[i,j] == "Neutral") {
        lib[i,ncol(lib)] = as.numeric(lib[i,ncol(lib)]) + 1
      }
      else if (lib[i,j] == "Dislike") {
        lib[i,ncol(lib)-1] = as.numeric(lib[i,ncol(lib)-1]) + 1
      }
      else {
        lib[i,ncol(lib)-2] = as.numeric(lib[i,ncol(lib)-2]) + 1
      }
    }
  }
  # Individual Seat Level 1 (IS_L1)
  temp <- data.frame("IS_L1_Like" = rep(0,nrow(lib)), "IS_L1_Dislike" = rep(0,nrow(lib)), "IS_L1_Neutral" = rep(0,nrow(lib)))
  lib <- cbind(lib,temp)
  startpos <- endpos + 1 
  capacity <- 6 
  endpos <- startpos+capacity-1 
  for (i in 1:nrow(lib)) {
    for (j in startpos:endpos) {
      if (lib[i,j] == "Neutral") {
        lib[i,ncol(lib)] = as.numeric(lib[i,ncol(lib)]) + 1
      }
      else if (lib[i,j] == "Dislike") {
        lib[i,ncol(lib)-1] = as.numeric(lib[i,ncol(lib)-1]) + 1
      }
      else {
        lib[i,ncol(lib)-2] = as.numeric(lib[i,ncol(lib)-2]) + 1
      }
    }
  }
  # Shared Seat Level 1 (SS_L1)
  temp <- data.frame("SS_L1_Like" = rep(0,nrow(lib)), "SS_L1_Dislike" = rep(0,nrow(lib)), "SS_L1_Neutral" = rep(0,nrow(lib)))
  lib <- cbind(lib,temp)
  startpos <- endpos + 1 
  capacity <- 32 
  endpos <- startpos+capacity-1
  for (i in 1:nrow(lib)) {
    for (j in startpos:endpos) {
      if (lib[i,j] == "Neutral") {
        lib[i,ncol(lib)] = as.numeric(lib[i,ncol(lib)]) + 1
      }
      else if (lib[i,j] == "Dislike") {
        lib[i,ncol(lib)-1] = as.numeric(lib[i,ncol(lib)-1]) + 1
      }
      else {
        lib[i,ncol(lib)-2] = as.numeric(lib[i,ncol(lib)-2]) + 1
      }
    }
  }
  # Soft Seat Left Level 1 (SoftL_L1)
  temp <- data.frame("SoftL_L1_Like" = rep(0,nrow(lib)), "SoftL_L1_Dislike" = rep(0,nrow(lib)), "SoftL_L1_Neutral" = rep(0,nrow(lib)))
  lib <- cbind(lib,temp)
  startpos <- endpos + 1 
  capacity <- 4 
  endpos <- startpos+capacity-1
  for (i in 1:nrow(lib)) {
    for (j in startpos:endpos) {
      if (lib[i,j] == "Neutral") {
        lib[i,ncol(lib)] = as.numeric(lib[i,ncol(lib)]) + 1
      }
      else if (lib[i,j] == "Dislike") {
        lib[i,ncol(lib)-1] = as.numeric(lib[i,ncol(lib)-1]) + 1
      }
      else {
        lib[i,ncol(lib)-2] = as.numeric(lib[i,ncol(lib)-2]) + 1
      }
    }
  }
  # Shared Seat Level 2 (SS_L2)
  temp <- data.frame("SS_L2_Like" = rep(0,nrow(lib)), "SS_L2_Dislike" = rep(0,nrow(lib)), "SS_L2_Neutral" = rep(0,nrow(lib)))
  lib <- cbind(lib,temp)
  startpos <- endpos + 1 + 3 #GSR1-3 entries are in between, thus +3 added
  capacity <- 18 
  endpos <- startpos+capacity-1
  for (i in 1:nrow(lib)) {
    for (j in startpos:endpos) {
      if (lib[i,j] == "Neutral") {
        lib[i,ncol(lib)] = as.numeric(lib[i,ncol(lib)]) + 1
      }
      else if (lib[i,j] == "Dislike") {
        lib[i,ncol(lib)-1] = as.numeric(lib[i,ncol(lib)-1]) + 1
      }
      else {
        lib[i,ncol(lib)-2] = as.numeric(lib[i,ncol(lib)-2]) + 1
      }
    }
  }
  # Soft Individual Seat Level 2 (SoftIS_L2)
  temp <- data.frame("SoftIS_L2_Like" = rep(0,nrow(lib)), "SoftIS_L2_Dislike" = rep(0,nrow(lib)), "SoftIS_L2_Neutral" = rep(0,nrow(lib)))
  lib <- cbind(lib,temp)
  startpos <- endpos + 1 
  capacity <- 2 
  endpos <- startpos+capacity-1
  for (i in 1:nrow(lib)) {
    for (j in startpos:endpos) {
      if (lib[i,j] == "Neutral") {
        lib[i,ncol(lib)] = as.numeric(lib[i,ncol(lib)]) + 1
      }
      else if (lib[i,j] == "Dislike") {
        lib[i,ncol(lib)-1] = as.numeric(lib[i,ncol(lib)-1]) + 1
      }
      else {
        lib[i,ncol(lib)-2] = as.numeric(lib[i,ncol(lib)-2]) + 1
      }
    }
  }
  # Soft Shared Seat Level 2 (SoftSS_L2)
  temp <- data.frame("SoftSS_L2_Like" = rep(0,nrow(lib)), "SoftSS_L2_Dislike" = rep(0,nrow(lib)), "SoftSS_L2_Neutral" = rep(0,nrow(lib)))
  lib <- cbind(lib,temp)
  startpos <- endpos + 1 
  capacity <- 8 
  endpos <- startpos+capacity-1
  for (i in 1:nrow(lib)) {
    for (j in startpos:endpos) {
      if (lib[i,j] == "Neutral") {
        lib[i,ncol(lib)] = as.numeric(lib[i,ncol(lib)]) + 1
      }
      else if (lib[i,j] == "Dislike") {
        lib[i,ncol(lib)-1] = as.numeric(lib[i,ncol(lib)-1]) + 1
      }
      else {
        lib[i,ncol(lib)-2] = as.numeric(lib[i,ncol(lib)-2]) + 1
      }
    }
  }
  # Individual Seat 24H (IS_24H)
  temp <- data.frame("IS_24H_Like" = rep(0,nrow(lib)), "IS_24H_Dislike" = rep(0,nrow(lib)), "IS_24H_Neutral" = rep(0,nrow(lib)))
  lib <- cbind(lib,temp)
  startpos <- endpos + 1 
  capacity <- 4 
  endpos <- startpos+capacity-1
  for (i in 1:nrow(lib)) {
    for (j in startpos:endpos) {
      if (lib[i,j] == "Neutral") {
        lib[i,ncol(lib)] = as.numeric(lib[i,ncol(lib)]) + 1
      }
      else if (lib[i,j] == "Dislike") {
        lib[i,ncol(lib)-1] = as.numeric(lib[i,ncol(lib)-1]) + 1
      }
      else {
        lib[i,ncol(lib)-2] = as.numeric(lib[i,ncol(lib)-2]) + 1
      }
    }
  }
  # Soft Individual Seat 24H (SoftIS_24H)
  temp <- data.frame("SoftIS_24H_Like" = rep(0,nrow(lib)), "SoftIS_24H_Dislike" = rep(0,nrow(lib)), "SoftIS_24H_Neutral" = rep(0,nrow(lib)))
  lib <- cbind(lib,temp)
  startpos <- endpos + 1 
  capacity <- 4 
  endpos <- startpos+capacity-1
  for (i in 1:nrow(lib)) {
    for (j in startpos:endpos) {
      if (lib[i,j] == "Neutral") {
        lib[i,ncol(lib)] = as.numeric(lib[i,ncol(lib)]) + 1
      }
      else if (lib[i,j] == "Dislike") {
        lib[i,ncol(lib)-1] = as.numeric(lib[i,ncol(lib)-1]) + 1
      }
      else {
        lib[i,ncol(lib)-2] = as.numeric(lib[i,ncol(lib)-2]) + 1
      }
    }
  }
  # Soft Shared Seat 24H (SoftSS_24H)
  temp <- data.frame("SoftSS_24H_Like" = rep(0,nrow(lib)), "SoftSS_24H_Dislike" = rep(0,nrow(lib)), "SoftSS_24H_Neutral" = rep(0,nrow(lib)))
  lib <- cbind(lib,temp)
  startpos <- endpos + 1 
  capacity <- 4
  endpos <- startpos+capacity-1
  for (i in 1:nrow(lib)) {
    for (j in startpos:endpos) {
      if (lib[i,j] == "Neutral") {
        lib[i,ncol(lib)] = as.numeric(lib[i,ncol(lib)]) + 1
      }
      else if (lib[i,j] == "Dislike") {
        lib[i,ncol(lib)-1] = as.numeric(lib[i,ncol(lib)-1]) + 1
      }
      else {
        lib[i,ncol(lib)-2] = as.numeric(lib[i,ncol(lib)-2]) + 1
      }
    }
  }
  # Shared Seat 24H (SS_24H)
  temp <- data.frame("SS_24H_Like" = rep(0,nrow(lib)), "SS_24H_Dislike" = rep(0,nrow(lib)), "SS_24H_Neutral" = rep(0,nrow(lib)))
  lib <- cbind(lib,temp)
  startpos <- endpos + 1 
  capacity <- 42 
  endpos <- startpos+capacity-1
  for (i in 1:nrow(lib)) {
    for (j in startpos:endpos) {
      if (lib[i,j] == "Neutral") {
        lib[i,ncol(lib)] = as.numeric(lib[i,ncol(lib)]) + 1
      }
      else if (lib[i,j] == "Dislike") {
        lib[i,ncol(lib)-1] = as.numeric(lib[i,ncol(lib)-1]) + 1
      }
      else {
        lib[i,ncol(lib)-2] = as.numeric(lib[i,ncol(lib)-2]) + 1
      }
    }
  }
  lib <- lib[,-c(7:(GSR1_spot-1),(GSR1_spot+3):(initial_ncol-6))]
  names <- colnames(lib)[16:ncol(lib)]
  lib[1,16:ncol(lib)] <- names
  colnames(lib) <- c(1:ncol(lib))
  return(lib)
}

convert_previous <- function(lib) { #Finds the total number of occupancy in 4 main spaces
  #Input lib is the output of count_aggregate function!
  #You have to check column numbers if any edit necessary in future
  #Stacks Total Occupancy
  temp <- as.numeric(lib[-c(1),16]) + as.numeric(lib[-c(1),17])
  temp[length(temp)+1] <- "Stacks_Total"
  temp <- temp[c(length(temp),1:length(temp)-1)]
  lib[,ncol(lib)+1] <- temp
  #Stacks Dislike
  temp <- as.numeric(lib[-c(1),17]) 
  temp[length(temp)+1] <- "Stacks_DL"
  temp <- temp[c(length(temp),1:length(temp)-1)]
  lib[,ncol(lib)+1] <- temp
  #Level 1 Total Occupancy
  temp <- {as.numeric(lib[-c(1),19]) + as.numeric(lib[-c(1),20]) +
      as.numeric(lib[-c(1),22]) + as.numeric(lib[-c(1),23]) +
      as.numeric(lib[-c(1),25]) + as.numeric(lib[-c(1),26])}
  temp[length(temp)+1] <- "Level1_Total"
  temp <- temp[c(length(temp),1:length(temp)-1)]
  lib[,ncol(lib)+1] <- temp
  #Level 1 Dislike
  temp <- as.numeric(lib[-c(1),20]) + as.numeric(lib[-c(1),23]) + as.numeric(lib[-c(1),26])
  temp[length(temp)+1] <- "Level1_DL"
  temp <- temp[c(length(temp),1:length(temp)-1)]
  lib[,ncol(lib)+1] <- temp
  #Level 2 Total Occupancy
  temp <- {as.numeric(lib[-c(1),28]) + as.numeric(lib[-c(1),29]) +
      as.numeric(lib[-c(1),31]) + as.numeric(lib[-c(1),32]) +
      as.numeric(lib[-c(1),34]) + as.numeric(lib[-c(1),35]) }
  temp[length(temp)+1] <- "Level2_Total"
  temp <- temp[c(length(temp),1:length(temp)-1)]
  lib[,ncol(lib)+1] <- temp
  #Level 2 Dislike
  temp <- as.numeric(lib[-c(1),32]) + as.numeric(lib[-c(1),35]) + as.numeric(lib[-c(1),29])
  temp[length(temp)+1] <- "Level2_DL"
  temp <- temp[c(length(temp),1:length(temp)-1)]
  lib[,ncol(lib)+1] <- temp
  #24 H Total Occupancy
  temp <- {as.numeric(lib[-c(1),40]) + as.numeric(lib[-c(1),41]) +
      as.numeric(lib[-c(1),43]) + as.numeric(lib[-c(1),44]) +
      as.numeric(lib[-c(1),46]) + as.numeric(lib[-c(1),47]) +
      as.numeric(lib[-c(1),37]) + as.numeric(lib[-c(1),38])}
  temp[length(temp)+1] <- "24H_Total"
  temp <- temp[c(length(temp),1:length(temp)-1)]
  lib[,ncol(lib)+1] <- temp
  #24 H Dislike
  temp <- as.numeric(lib[-c(1),41]) + as.numeric(lib[-c(1),44]) + as.numeric(lib[-c(1),47]) + as.numeric(lib[-c(1),38])
  temp[length(temp)+1] <- "24H_DL"
  temp <- temp[c(length(temp),1:length(temp)-1)]
  lib[,ncol(lib)+1] <- temp
  c <- lib[,c(1:6,15,7:14,49:ncol(lib))]
  colnames(c) <- c(1:ncol(c))
  return(c)
}
#RIGHT NOW YOU ARE HERE (BELOW)
addcolumn <- function(lib) { #Adds GSR13, GSR45,GSR67, Total columns
  lib2 <- lib
  for (i in 2:nrow(lib2)) {
    #Creating GSR13 column 
    item <- c() 
    a <- 1
    for (j in 8:10) {
      item[a] <- as.numeric(lib2[i,j])
      a <- a + 1
    }
    if (NA %in% item) {
      lib2[i,24] <- NA
    }
    else {
      lib2[i,24] <- sum(item)
    }
    #Creating GSR45 column
    item <- c() 
    a <- 1
    for (j in 11:12) {
      item[a] <- as.numeric(lib2[i,j])
      a <- a + 1
    }
    if (NA %in% item) {
      lib2[i,25] <- NA
    }
    else {
      lib2[i,25] <- sum(item)
    }
    #Creating GSR67 column
    item <- c() 
    a <- 1
    for (j in 13:14) {
      item[a] <- as.numeric(lib2[i,j])
      a <- a + 1
    }
    if (NA %in% item) {
      lib2[i,26] <- NA
    }
    else {
      lib2[i,26] <- sum(item)
    }
    #Creating Total column
    item <- c() 
    a <- 1
    for (j in c(8:16,18,20,22)) {
      item[a] <- as.numeric(lib2[i,j])
      a <- a + 1
    }
    if (NA %in% item) {
      lib2[i,27] <- NA
    }
    else {
      lib2[i,27] <- sum(item)
    }
    #Creating Total without GSR/CL column
    item <- c() 
    a <- 1
    for (j in c(16,18,20,22)) {
      item[a] <- as.numeric(lib2[i,j])
      a <- a + 1
    }
    if (NA %in% item) {
      lib2[i,28] <- NA
    }
    else {
      lib2[i,28] <- sum(item)
    }
  }
  libhead <- c("GSR1-3","GSR4-5","GSR6-7","Total","Total_NO GSR/CL")
  lib2[1,c(24:28)] <- libhead
  colnames(lib2) <- c(1:28)
  return(lib2)
}

datafinder <- function(mon,section,time) { #Gives 7-8 average values for given month, section, and time
  hours <- c()
  mon <- switch(time, 
                "weekday" = {
                  hours <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm","WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm");
                  colnames(mon)[7] <- "hour";
                  weekday_data(mon)
                },
                "weekend" = {
                  hours <- c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm","WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm");
                  colnames(mon)[6] <- "hour";
                  weekend_data(mon)
                },
                message("Wrong time input datafinder"))
  
  if (section == "All") {colnames(mon)[27] <- "this"}
  else if (section == "4main") {colnames(mon)[28] <- "this"}
  else if (section == "L1") {colnames(mon)[18] <- "this"}
  else if (section == "Stacks") {colnames(mon)[16] <- "this"}
  else if (section == "GSR13") {colnames(mon)[24] <- "this"}
  else if (section == "GSR45") {colnames(mon)[25] <- "this"}
  else if (section == "GSR67") {colnames(mon)[26] <- "this"}
  else if (section == "L2") {colnames(mon)[20] <- "this"}
  else if (section == "CLab") {colnames(mon)[15] <- "this"}
  else if (section == "24H") {colnames(mon)[22] <- "this"}
  else { message("Wrong section input datafinder")}
  
  result <- c() #This will store the average ocupancy of the given time
  for(i in 1:length(hours)){
    shift <- hours[i]
    subdata <- mon[mon$"hour" == shift,]
    a <- as.numeric(subdata$"this")
    s <- sum(a, na.rm = TRUE)
    len <- length(a[!is.na(a)])
    avg <- s/len
    result[i] <- avg
  }
  return(result)
}
#######################ÇALIÞMA ALANI START ##################################
GraphOverallDay <- function(mons) { #Takes a list of months, returns average occupancy (weekdays) per hour for each month
  capacity <- 120
  months <- c()
  for (i in 1:length(mons)) {
    months[i] <- monthfinder(substr(mons[[i]][4,4],6,7))
  }
  for (i in 1:length(mons)) {
    mons[[i]] <- datafinder(mons[[i]],"All","weekday")
  }
  b <- data.frame(mons)
  colnames(b) <- months
  rownames(b) <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
                   "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
  c <- (b/capacity)*100
  row.names(c) <- c("10am","12pm","2pm","4pm","6pm","7pm","8pm","9pm","10pm")
  d <- rbind(b,c)
  return(d)
}
GraphOverallEnd <- function(mons) { #Takes a list of months, returns average occupancy (weekends) per hour for each month
  capacity <- 120
  months <- c()
  for (i in 1:length(mons)) {
    months[i] <- monthfinder(substr(mons[[i]][4,4],6,7))
  }
  for (i in 1:length(mons)) {
    mons[[i]] <- datafinder(mons[[i]],"All","weekend")
  }
  b <- data.frame(mons)
  colnames(b) <- months
  rownames(b) <-  c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
                    "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
  c <- (b/capacity)*100
  row.names(c) <- c("11am","12pm","1pm","2pm","3pm","4pm","5pm","6pm")
  d <- rbind(b,c)
  return(d)
}

GraphOverallDay2 <- function(mons) { #Takes a list of months, returns average occupancy (weekdays) per hour for each month
  months <- c()                      #Only 4 main spaces
  capacity <- 84
  for (i in 1:length(mons)) {
    months[i] <- monthfinder(substr(mons[[i]][4,4],6,7))
  }
  for (i in 1:length(mons)) {
    mons[[i]] <- datafinder(mons[[i]],"4main","weekday")
  }
  b <- data.frame(mons)
  colnames(b) <- months
  rownames(b) <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
                   "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
  c <- (b/capacity)*100
  row.names(c) <- c("10am","12pm","2pm","4pm","6pm","7pm","8pm","9pm","10pm")
  d <- rbind(b,c)
  return(d)
}
GraphOverallEnd2 <- function(mons) { #Takes a list of months, returns average occupancy (weekends) per hour for each month
  months <- c()                      #Only 4 main spaces
  capacity <- 84
  for (i in 1:length(mons)) {
    months[i] <- monthfinder(substr(mons[[i]][4,4],6,7))
  }
  for (i in 1:length(mons)) {
    mons[[i]] <- datafinder(mons[[i]],"4main","weekend")
  }
  b <- data.frame(mons)
  colnames(b) <- months
  rownames(b) <-  c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
                    "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
  c <- (b/capacity)*100
  row.names(c) <- c("11am","12pm","1pm","2pm","3pm","4pm","5pm","6pm")
  d <- rbind(b,c)
  return(d)
}
##Alandýþý start
GraphSpacesDay <- function(mon) {
  c1 <- datafinder(mon,"L1","weekday") 
  c2 <- datafinder(mon,"Stacks","weekday")  
  c3 <- datafinder(mon,"GSR13","weekday")  
  c4 <- datafinder(mon,"GSR45","weekday")  
  c5 <- datafinder(mon,"GSR67","weekday")  
  c6 <- datafinder(mon,"L2","weekday")  
  c7 <- datafinder(mon,"CLab","weekday")  
  c8 <- datafinder(mon,"24H","weekday")  
  row <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
           "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
  b <- data.frame(row.names = row,c1,c2,c3,c4,c5,c6,c7,c8)
  colnames(b) <- c("Level1","Stacks","GSR1-3","GSR4-5",
                   "GSR6-7","Level2","Comp Lab","24H")
  c1 <- (c1/24)*100
  c2 <- (c2/16)*100
  c3 <- (c3/6)*100
  c4 <- (c4/4)*100
  c5 <- (c5/4)*100
  c6 <- (c6/15)*100
  c7 <- (c7/26)*100
  c8 <- (c8/29)*100
  row <- c("10am","12pm","2pm","4pm","6pm","7pm","8pm","9pm","10pm")
  c <- data.frame(row.names = row,c1,c2,c3,c4,c5,c6,c7,c8)
  colnames(c) <- c("Level1","Stacks","GSR1-3","GSR4-5",
                   "GSR6-7","Level2","Comp Lab","24H")
  d <- rbind(b,c)
  return(d)
}
GraphSpacesEnd <- function(mon) {
  c1 <- datafinder(mon,"L1","weekend") 
  c2 <- datafinder(mon,"Stacks","weekend") 
  c3 <- datafinder(mon,"GSR13","weekend") 
  c4 <- datafinder(mon,"GSR45","weekend") 
  c5 <- datafinder(mon,"GSR67","weekend")
  c6 <- datafinder(mon,"L2","weekend") 
  c7 <- datafinder(mon,"CLab","weekend") 
  c8 <- datafinder(mon,"24H","weekend") 
  row <- c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
           "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
  b <- data.frame(row.names = row,c1,c2,c3,c4,c5,c6,c7,c8)
  colnames(b) <- c("Level1","Stacks","GSR1-3","GSR4-5",
                   "GSR6-7","Level2","Comp Lab","24H")
  c1 <- (c1/24)*100
  c2 <- (c2/16)*100
  c3 <- (c3/6)*100
  c4 <- (c4/4)*100
  c5 <- (c5/4)*100
  c6 <- (c6/15)*100
  c7 <- (c7/26)*100
  c8 <- (c8/29)*100
  row <- c("11am","12pm","1pm","2pm","3pm","4pm","5pm","6pm")
  c <- data.frame(row.names = row,c1,c2,c3,c4,c5,c6,c7,c8)
  colnames(c) <- c("Level1","Stacks","GSR1-3","GSR4-5",
                   "GSR6-7","Level2","Comp Lab","24H")
  d <- rbind(b,c)
  return(d)
}
##Alandýþý end
GraphLevel1Day <- function(mons) {
  capacity <- 24
  months <- c()
  for (i in 1:length(mons)) {
    months[i] <- monthfinder(substr(mons[[i]][4,4],6,7))
  }
  for (i in 1:length(mons)) {
    mons[[i]] <- datafinder(mons[[i]],"L1","weekday")
  }
  b <- data.frame(mons)
  colnames(b) <- months
  rownames(b) <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
                   "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
  c <- (b/capacity)*100
  row.names(c) <- c("10am","12pm","2pm","4pm","6pm","7pm","8pm","9pm","10pm")
  d <- rbind(b,c)
  return(d)
} 
GraphLevel1End <- function(mons) {
  capacity <- 24
  months <- c()
  for (i in 1:length(mons)) {
    months[i] <- monthfinder(substr(mons[[i]][4,4],6,7))
  }
  for (i in 1:length(mons)) {
    mons[[i]] <- datafinder(mons[[i]],"L1","weekend")
  }
  b <- data.frame(mons)
  colnames(b) <- months
  rownames(b) <-  c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
                    "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
  c <- (b/capacity)*100
  row.names(c) <- c("11am","12pm","1pm","2pm","3pm","4pm","5pm","6pm")
  d <- rbind(b,c)
  return(d)
} 

GraphStacksDay <- function(mons) {
  capacity <- 16
  months <- c()
  for (i in 1:length(mons)) {
    months[i] <- monthfinder(substr(mons[[i]][4,4],6,7))
  }
  for (i in 1:length(mons)) {
    mons[[i]] <- datafinder(mons[[i]],"Stacks","weekday")
  }
  b <- data.frame(mons)
  colnames(b) <- months
  rownames(b) <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
                   "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
  c <- (b/capacity)*100
  row.names(c) <- c("10am","12pm","2pm","4pm","6pm","7pm","8pm","9pm","10pm")
  d <- rbind(b,c)
  return(d)
} 
GraphStacksEnd <- function(mons) {
  capacity <- 16
  months <- c()
  for (i in 1:length(mons)) {
    months[i] <- monthfinder(substr(mons[[i]][4,4],6,7))
  }
  for (i in 1:length(mons)) {
    mons[[i]] <- datafinder(mons[[i]],"Stacks","weekend")
  }
  b <- data.frame(mons)
  colnames(b) <- months
  rownames(b) <-  c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
                    "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
  c <- (b/capacity)*100
  row.names(c) <- c("11am","12pm","1pm","2pm","3pm","4pm","5pm","6pm")
  d <- rbind(b,c)
  return(d)
} 

GraphGSR13Day <- function(mons) {
  capacity <- 6
  months <- c()
  for (i in 1:length(mons)) {
    months[i] <- monthfinder(substr(mons[[i]][4,4],6,7))
  }
  for (i in 1:length(mons)) {
    mons[[i]] <- datafinder(mons[[i]],"GSR13","weekday")
  }
  b <- data.frame(mons)
  colnames(b) <- months
  rownames(b) <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
                   "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
  c <- (b/capacity)*100
  row.names(c) <- c("10am","12pm","2pm","4pm","6pm","7pm","8pm","9pm","10pm")
  d <- rbind(b,c)
  return(d)
} 
GraphGSR13End <- function(mons) {
  months <- c()
  capacity <- 6
  for (i in 1:length(mons)) {
    months[i] <- monthfinder(substr(mons[[i]][4,4],6,7))
  }
  for (i in 1:length(mons)) {
    mons[[i]] <- datafinder(mons[[i]],"GSR13","weekend")
  }
  b <- data.frame(mons)
  colnames(b) <- months
  rownames(b) <-  c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
                    "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
  c <- (b/capacity)*100
  row.names(c) <- c("11am","12pm","1pm","2pm","3pm","4pm","5pm","6pm")
  d <- rbind(b,c)
  return(d)
} 

GraphGSR45Day <- function(mons) {
  capacity <- 4
  months <- c()
  for (i in 1:length(mons)) {
    months[i] <- monthfinder(substr(mons[[i]][4,4],6,7))
  }
  for (i in 1:length(mons)) {
    mons[[i]] <- datafinder(mons[[i]],"GSR45","weekday")
  }
  b <- data.frame(mons)
  colnames(b) <- months
  rownames(b) <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
                   "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
  c <- (b/capacity)*100
  row.names(c) <- c("10am","12pm","2pm","4pm","6pm","7pm","8pm","9pm","10pm")
  d <- rbind(b,c)
  return(d)
} 
GraphGSR45End <- function(mons) {
  capacity <- 4
  months <- c()
  for (i in 1:length(mons)) {
    months[i] <- monthfinder(substr(mons[[i]][4,4],6,7))
  }
  for (i in 1:length(mons)) {
    mons[[i]] <- datafinder(mons[[i]],"GSR45","weekend")
  }
  b <- data.frame(mons)
  colnames(b) <- months
  rownames(b) <-  c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
                    "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
  c <- (b/capacity)*100
  row.names(c) <- c("11am","12pm","1pm","2pm","3pm","4pm","5pm","6pm")
  d <- rbind(b,c)
  return(d)
} 

GraphGSR67Day <- function(mons) {
  capacity <- 4
  months <- c()
  for (i in 1:length(mons)) {
    months[i] <- monthfinder(substr(mons[[i]][4,4],6,7))
  }
  for (i in 1:length(mons)) {
    mons[[i]] <- datafinder(mons[[i]],"GSR67","weekday")
  }
  b <- data.frame(mons)
  colnames(b) <- months
  rownames(b) <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
                   "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
  c <- (b/capacity)*100
  row.names(c) <- c("10am","12pm","2pm","4pm","6pm","7pm","8pm","9pm","10pm")
  d <- rbind(b,c)
  return(d)
} 
GraphGSR67End <- function(mons) {
  capacity <- 4
  months <- c()
  for (i in 1:length(mons)) {
    months[i] <- monthfinder(substr(mons[[i]][4,4],6,7))
  }
  for (i in 1:length(mons)) {
    mons[[i]] <- datafinder(mons[[i]],"GSR67","weekend")
  }
  b <- data.frame(mons)
  colnames(b) <- months
  rownames(b) <-  c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
                    "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
  c <- (b/capacity)*100
  row.names(c) <- c("11am","12pm","1pm","2pm","3pm","4pm","5pm","6pm")
  d <- rbind(b,c)
  return(d)
}

GraphLevel2Day <- function(mons) {
  capacity <- 15
  months <- c()
  for (i in 1:length(mons)) {
    months[i] <- monthfinder(substr(mons[[i]][4,4],6,7))
  }
  for (i in 1:length(mons)) {
    mons[[i]] <- datafinder(mons[[i]],"L2","weekday")
  }
  b <- data.frame(mons)
  colnames(b) <- months
  rownames(b) <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
                   "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
  c <- (b/capacity)*100
  row.names(c) <- c("10am","12pm","2pm","4pm","6pm","7pm","8pm","9pm","10pm")
  d <- rbind(b,c)
  return(d)
} 
GraphLevel2End <- function(mons) {
  capacity <- 15
  months <- c()
  for (i in 1:length(mons)) {
    months[i] <- monthfinder(substr(mons[[i]][4,4],6,7))
  }
  for (i in 1:length(mons)) {
    mons[[i]] <- datafinder(mons[[i]],"L2","weekend")
  }
  b <- data.frame(mons)
  colnames(b) <- months
  rownames(b) <-  c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
                    "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
  c <- (b/capacity)*100
  row.names(c) <- c("11am","12pm","1pm","2pm","3pm","4pm","5pm","6pm")
  d <- rbind(b,c)
  return(d)
} 

GraphCompDay <- function(mons) {
  capacity <- 26
  months <- c()
  for (i in 1:length(mons)) {
    months[i] <- monthfinder(substr(mons[[i]][4,4],6,7))
  }
  for (i in 1:length(mons)) {
    mons[[i]] <- datafinder(mons[[i]],"CLab","weekday")
  }
  b <- data.frame(mons)
  colnames(b) <- months
  rownames(b) <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
                   "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
  c <- (b/capacity)*100
  row.names(c) <- c("10am","12pm","2pm","4pm","6pm","7pm","8pm","9pm","10pm")
  d <- rbind(b,c)
  return(d)
} 
GraphCompEnd <- function(mons) {
  capacity <- 26
  months <- c()
  for (i in 1:length(mons)) {
    months[i] <- monthfinder(substr(mons[[i]][4,4],6,7))
  }
  for (i in 1:length(mons)) {
    mons[[i]] <- datafinder(mons[[i]],"CLab","weekend")
  }
  b <- data.frame(mons)
  colnames(b) <- months
  rownames(b) <-  c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
                    "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
  c <- (b/capacity)*100
  row.names(c) <- c("11am","12pm","1pm","2pm","3pm","4pm","5pm","6pm")
  d <- rbind(b,c)
  return(d)
} 

Graph24HDay <- function(mons) {
  capacity <- 29
  months <- c()
  for (i in 1:length(mons)) {
    months[i] <- monthfinder(substr(mons[[i]][4,4],6,7))
  }
  for (i in 1:length(mons)) {
    mons[[i]] <- datafinder(mons[[i]],"24H","weekday")
  }
  b <- data.frame(mons)
  colnames(b) <- months
  rownames(b) <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
                   "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
  c <- (b/capacity)*100
  row.names(c) <- c("10am","12pm","2pm","4pm","6pm","7pm","8pm","9pm","10pm")
  d <- rbind(b,c)
  return(d)
} 
Graph24HEnd <- function(mons) {
  capacity <- 29
  months <- c()
  for (i in 1:length(mons)) {
    months[i] <- monthfinder(substr(mons[[i]][4,4],6,7))
  }
  for (i in 1:length(mons)) {
    mons[[i]] <- datafinder(mons[[i]],"24H","weekend")
  }
  b <- data.frame(mons)
  colnames(b) <- months
  rownames(b) <-  c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
                    "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
  c <- (b/capacity)*100
  row.names(c) <- c("11am","12pm","1pm","2pm","3pm","4pm","5pm","6pm")
  d <- rbind(b,c)
  return(d)
} 
#########################ÇALIÞMA ALANI END ################################
seatminmax <- function(lib) { #Creates a list of individual seat according to their occupancy (descending order)
  #Editing name of the space is lengthy
  edit <- lib[,c(7:(GSR1_spot-1),(GSR1_spot+3):(ncol(lib)-6))]
  res <- data.frame(0,1,2,3,4, stringsAsFactors = FALSE)
  colnames(res) <- c("Seat","Like","Dislike","Neutral","Total")
  colnames(edit) <- c(1:ncol(edit))
  for (i in 1:ncol(edit)) {
    like <- 0
    dislike <- 0
    neutral <- 0
    v <- as.vector(unlist(edit[-c(1),i]))
    if(i < 10) {
      res[i,1] <- substr(edit[1,i],8,11)
    }
    else if(i < 17) {
      res[i,1] <- substr(edit[1,i],7,11)
    }
    else if(i < 32) {
      res[i,1] <- substr(edit[1,i],5,11)
    }
    else if(i < 55) {
      res[i,1] <- substr(edit[1,i],4,11)
    }
    else if(i < 59) {
      res[i,1] <- edit[1,i]
    }
    else if(i < 67) {
      res[i,1] <- substr(edit[1,i],5,11)
    }
    else if(i == 67) {
      res[i,1] <- substr(edit[1,i],4,11)
    }
    else if(i == 68) {
      res[i,1] <- substr(edit[1,i],5,11)
    }
    else if(i < 77) {
      res[i,1] <- substr(edit[1,i],4,11)
    }
    else if(i < 87) {
      res[i,1] <- edit[1,i]
    }
    else if(i < 91) {
      res[i,1] <- substr(edit[1,i],4,11)
    }
    else if(i < 99) {
      res[i,1] <- paste("S",edit[1,i], sep = "")
    }
    else if(i < 109) {
      res[i,1] <- substr(edit[1,i],4,11)
    }
    else if(i <= ncol(edit)) {
      res[i,1] <- substr(edit[1,i],3,11)
    }
    else {
      message("Oh shit_seatminmax")
      return()
    }
    for (item in v) {
      if (item == "Neutral") {
        neutral <- neutral + 1
      }
      else if (item == "Like") {
        like <- like + 1
      }
      else if (item == "Dislike") {
        dislike <- dislike + 1
      }
      else {
        message("What the f***_seatminmax")
        return()
      }
    }
    res[i,2] <- like
    res[i,3] <- dislike
    res[i,4] <- neutral
    res[i,5] <- like + dislike
  }
  res <- res[order(res$Total, decreasing = TRUE),]
  return(res)
}

convertnumeric <- function(v1) {
  result <- c()
  for (i in 1:length(v1)) {
    result[i] <- as.numeric(v1[i])
  }
  return(result)
}

BarMonthsDay <- function(mons) { #All months, like dislike, bar graph, weekday
  capacity <- 84
  months <- c()
  for (i in 1:length(mons)) {
    months[i] <- monthfinder(substr(mons[[i]][4,4],6,7))
  }
  
  for(i in 1:length(mons)) {
    mons[[i]] <- weekday_data(mons[[i]])
    colnames(mons[[i]]) <- as.character(mons[[i]][1,])
    mons[[i]] <- mons[[i]][-c(1),-c(1,2,3,5,6)]
    for (j in 3:22) {
      mons[[i]][,j] <- convertnumeric(as.vector(unlist(mons[[i]][,j])))
    }
  }
  
  T1 <- mean(mons[[1]][,11]) + mean(mons[[1]][,13]) + mean(mons[[1]][,15]) + mean(mons[[1]][,17])
  D1 <- mean(mons[[1]][,12]) + mean(mons[[1]][,14]) + mean(mons[[1]][,16]) + mean(mons[[1]][,18])
  d <- data.frame(T1,D1)
  
  if(length(mons) > 1) {
    for(i in 2:length(mons)) {
      T1 <- mean(mons[[i]][,11]) + mean(mons[[i]][,13]) + mean(mons[[i]][,15]) + mean(mons[[i]][,17])
      D1 <- mean(mons[[i]][,12]) + mean(mons[[i]][,14]) + mean(mons[[i]][,16]) + mean(mons[[i]][,18])
      d <- rbind(d, data.frame(T1,D1))
    }
  }
  colnames(d) <- c("Total Occupancy","Occupied by Belongings")
  months_total <- c()
  for (i in 1:length(months)) {
    months_total[i] <- paste(months[i],"Total")
  }
  rownames(d) <- months_total
  e <- d / capacity
  rownames(e) <- months
  d <- rbind(d,e)
  return(d)
}
BarMonthsEnd <- function(mons) { #All months, like dislike, bar graph, weekend
  capacity <- 84
  months <- c()
  for (i in 1:length(mons)) {
    months[i] <- monthfinder(substr(mons[[i]][4,4],6,7))
  }
  
  for(i in 1:length(mons)) {
    mons[[i]] <- weekend_data(mons[[i]])
    colnames(mons[[i]]) <- as.character(mons[[i]][1,])
    mons[[i]] <- mons[[i]][-c(1),-c(1,2,3,5,6)]
    for (j in 3:22) {
      mons[[i]][,j] <- convertnumeric(as.vector(unlist(mons[[i]][,j])))
    }
  }
  
  T1 <- mean(mons[[1]][,11]) + mean(mons[[1]][,13]) + mean(mons[[1]][,15]) + mean(mons[[1]][,17])
  D1 <- mean(mons[[1]][,12]) + mean(mons[[1]][,14]) + mean(mons[[1]][,16]) + mean(mons[[1]][,18])
  d <- data.frame(T1,D1)
  
  if(length(mons) > 1) {
    for(i in 2:length(mons)) {
      T1 <- mean(mons[[i]][,11]) + mean(mons[[i]][,13]) + mean(mons[[i]][,15]) + mean(mons[[i]][,17])
      D1 <- mean(mons[[i]][,12]) + mean(mons[[i]][,14]) + mean(mons[[i]][,16]) + mean(mons[[i]][,18])
      d <- rbind(d, data.frame(T1,D1))
    }
  }
  colnames(d) <- c("Total Occupancy","Occupied by Belongings")
  months_total <- c()
  for (i in 1:length(months)) {
    months_total[i] <- paste(months[i],"Total")
  }
  rownames(d) <- months_total
  e <- d /capacity
  rownames(e) <- months
  d <- rbind(d,e)
  
  return(d)
}

Bar4spaceDay <- function(mon) { #4 main spaces, like dislike, bar graph, weekday
  t <- c(mean(mon[,11]), mean(mon[,13]), mean(mon[,15]), mean(mon[,17]))
  d <- c(mean(mon[,12]), mean(mon[,14]), mean(mon[,16]), mean(mon[,18]))
  d <- data.frame(t,d)
  colnames(d) <- c("Total Occupancy","Occupied by Belongings")
  r1 <- d[1,] / 16
  r2 <- d[2,] / 24
  r3 <- d[3,] / 15
  r4 <- d[4,] / 29
  d <- rbind(d,r1,r2,r3,r4)
  rownames(d) <- c("Stacks_Total", "Level1_Total", "Level2_Total", "24H_Total","Stacks", "Level1", "Level2", "24H")
  return(d)
}
Bar4spaceEnd <- function(mon) { #4 main spaces, like dislike, bar graph, weekend
  t <- c(mean(mon[,11]), mean(mon[,13]), mean(mon[,15]), mean(mon[,17]))
  d <- c(mean(mon[,12]), mean(mon[,14]), mean(mon[,16]), mean(mon[,18]))
  d <- data.frame(t,d)
  colnames(d) <- c("Total Occupancy","Occupied by Belongings")
  r1 <- d[1,] / 16
  r2 <- d[2,] / 24
  r3 <- d[3,] / 15
  r4 <- d[4,] / 29
  d <- rbind(d,r1,r2,r3,r4)
  rownames(d) <- c("Stacks_Total", "Level1_Total", "Level2_Total", "24H_Total","Stacks", "Level1", "Level2", "24H")
  return(d)
}

bargraph_specific_section <- function(mon,section,time) {
  mon <- switch(time, #Cleans data for weekday or weekend
                "weekday" = weekday_data(mon),
                "weekend" = weekend_data(mon),
                message("Wrong time input bargraph_specific_section"))
  if (section == "L1") {
    IS_L1 <- c(mean(as.numeric(mon[-c(1),19])) + mean(as.numeric(mon[-c(1),20])),mean(as.numeric(mon[-c(1),20])))
    SS_L1 <- c(mean(as.numeric(mon[-c(1),22])) + mean(as.numeric(mon[-c(1),23])),mean(as.numeric(mon[-c(1),23])))
    SoftL_L1 <- c(mean(as.numeric(mon[-c(1),25])) + mean(as.numeric(mon[-c(1),26])),mean(as.numeric(mon[-c(1),26])))
    d <- rbind.data.frame(IS_L1,SS_L1,SoftL_L1)
    colnames(d) <- c("Total Occupancy","Occupied by Belongings")
    #Capacities here - adjusted for covid!!
    r1 <- d[1,] / 6 #No change here
    r2 <- d[2,] / 16
    r3 <- d[3,] / 2
    d <- rbind(d,r1,r2,r3)
    rownames(d) <- c("IS_L1_Total","SS_L1_Total","SoftL_L1_Total","IS_L1","SS_L1","SoftL_L1")
    return(d)
  }
  else if (section == "24H") {
    IS_24h <- c(mean(as.numeric(mon[-c(1),37])) + mean(as.numeric(mon[-c(1),38])),mean(as.numeric(mon[-c(1),38])))
    SS_24h <- c(mean(as.numeric(mon[-c(1),46])) + mean(as.numeric(mon[-c(1),47])),mean(as.numeric(mon[-c(1),47])))
    SoftIS_24h <- c(mean(as.numeric(mon[-c(1),40])) + mean(as.numeric(mon[-c(1),41])),mean(as.numeric(mon[-c(1),41])))
    SoftSS_24h <- c(mean(as.numeric(mon[-c(1),43])) + mean(as.numeric(mon[-c(1),44])),mean(as.numeric(mon[-c(1),44])))
    d <- rbind.data.frame(IS_24h,SS_24h,SoftIS_24h,SoftSS_24h)
    colnames(d) <- c("Total Occupancy","Occupied by Belongings")
    #Capacities here - covid adjusted
    r1 <- d[1,] / 4
    r2 <- d[2,] / 21
    r3 <- d[3,] / 2
    r4 <- d[4,] / 2
    d <- rbind(d,r1,r2,r3,r4)
    rownames(d) <- c("IS_24h_Total","SS_24h_Total","SoftIS_24h_Total","SoftSS_24h_Total","IS_24h","SS_24h","SoftIS_24h_L1","SoftSS_24h")
    return(d)
  }
  else if (section == "L2") {
    SS_L2 <- c(mean(as.numeric(mon[-c(1),28])) + mean(as.numeric(mon[-c(1),29])),mean(as.numeric(mon[-c(1),29])))
    SoftIS_L2 <- c(mean(as.numeric(mon[-c(1),31])) + mean(as.numeric(mon[-c(1),32])),mean(as.numeric(mon[-c(1),32])))
    SoftSS_L2 <- c(mean(as.numeric(mon[-c(1),34])) + mean(as.numeric(mon[-c(1),35])),mean(as.numeric(mon[-c(1),35])))
    d <- rbind.data.frame(SS_L2,SoftIS_L2,SoftSS_L2)
    colnames(d) <- c("Total Occupancy","Occupied by Belongings")
    #Capacities here - covid adjusted
    r1 <- d[1,] / 9
    r2 <- d[2,] / 2
    r3 <- d[3,] / 4
    d <- rbind(d,r1,r2,r3)
    rownames(d) <- c("SS_L2_Total","SoftIS_L2_Total","SoftSS_L2_Total","SS_L2","SoftIS_L2","SoftSS_L2")
    return(d)
  }
  else {
    message("Wrong section input bargraph_specific_section")
  }
}

monthmaxmin1 <- function(a) { #Finds individual min/max for a month (all data, column 27)
  #Covid adjusted
  libcapacity <- 120
  b <- a[!is.na(a$`27`),]
  head <- b[1,]
  b <- b[c(-1),]
  b[,27] <- convertnumeric(b[,27])
  b <- b[order(b$`27`, decreasing = TRUE),]
  b <- rbind(head,b)
  wend <- weekend_data(b)
  wday <- weekday_data(b)
  m <- monthfinder(substr(b[2,4],6,7))
  message(sprintf("%s Max:",m))
  for (i in 2:6) {
    occupancy <- round(as.numeric(wday[i,27])/libcapacity,4)*100
    d <- substr(wday[i,4],9,10)
    h <- if (substr(wday[i,7],14,14) == " ") substr(wday[i,7],11,13) else substr(wday[i,7],11,14)
    message(sprintf("%s %s %s (%.2f%%)",d,m,h,occupancy))
  }
  message(sprintf("%s Min:",m))
  for (i in (nrow(wday)-4):nrow(wday)) {
    occupancy <- round(as.numeric(wday[i,27])/libcapacity,4)*100
    d <- substr(wday[i,4],9,10)
    h <- if (substr(wday[i,7],14,14) == " ") substr(wday[i,7],11,13) else substr(wday[i,7],11,14)
    message(sprintf("%s %s %s (%.2f%%)",d,m,h,occupancy))
  }
  message(sprintf("%s Max:",m))
  for (i in 2:6) {
    occupancy <- round(as.numeric(wend[i,27])/libcapacity,4)*100
    d <- substr(wend[i,4],9,10)
    h <- if (substr(wend[i,6],14,14) == " ") substr(wend[i,6],11,13) else substr(wend[i,6],11,14)
    message(sprintf("%s %s %s (%.2f%%)",d,m,h,occupancy))
  }
  message(sprintf("%s Min:",m))
  for (i in (nrow(wend)-4):nrow(wend)) {
    occupancy <- round(as.numeric(wend[i,27])/libcapacity,4)*100
    d <- substr(wend[i,4],9,10)
    h <- if (substr(wend[i,6],14,14) == " ") substr(wend[i,6],11,13) else substr(wend[i,6],11,14)
    message(sprintf("%s %s %s (%.2f%%)",d,m,h,occupancy))
  }
}
monthmaxmin2 <- function(a) { #Finds individual min/max for a month (4 main space, column 28)
  #Covid adjusted capacity
  libcapacity <- 84
  b <- a[!is.na(a$`28`),]
  head <- b[1,]
  b <- b[c(-1),]
  b[,28] <- convertnumeric(b[,28])
  b <- b[order(b$`28`, decreasing = TRUE),]
  b <- rbind(head,b)
  wend <- weekend_data(b)
  wday <- weekday_data(b)
  m <- monthfinder(substr(b[2,4],6,7))
  message(sprintf("%s Max:",m))
  for (i in 2:6) {
    occupancy <- round(as.numeric(wday[i,28])/libcapacity,4)*100
    d <- substr(wday[i,4],9,10)
    h <- if (substr(wday[i,7],14,14) == " ") substr(wday[i,7],11,13) else substr(wday[i,7],11,14)
    message(sprintf("%s %s %s (%.2f%%)",d,m,h,occupancy))
  }
  message(sprintf("%s Min:",m))
  for (i in (nrow(wday)-4):nrow(wday)) {
    occupancy <- round(as.numeric(wday[i,28])/libcapacity,4)*100
    d <- substr(wday[i,4],9,10)
    h <- if (substr(wday[i,7],14,14) == " ") substr(wday[i,7],11,13) else substr(wday[i,7],11,14)
    message(sprintf("%s %s %s (%.2f%%)",d,m,h,occupancy))
  }
  message(sprintf("%s Max:",m))
  for (i in 2:6) {
    occupancy <- round(as.numeric(wend[i,28])/libcapacity,4)*100
    d <- substr(wend[i,4],9,10)
    h <- if (substr(wend[i,6],14,14) == " ") substr(wend[i,6],11,13) else substr(wend[i,6],11,14)
    message(sprintf("%s %s %s (%.2f%%)",d,m,h,occupancy))
  }
  message(sprintf("%s Min:",m))
  for (i in (nrow(wend)-4):nrow(wend)) {
    occupancy <- round(as.numeric(wend[i,28])/libcapacity,4)*100
    d <- substr(wend[i,4],9,10)
    h <- if (substr(wend[i,6],14,14) == " ") substr(wend[i,6],11,13) else substr(wend[i,6],11,14)
    message(sprintf("%s %s %s (%.2f%%)",d,m,h,occupancy))
  }
}

exportxlsx <- function(...) {
  library("xlsx")
  a <- list(...)
  
  specific <- seatminmax(a[[length(a)]])
  
  b <- count_aggregate(a[[length(a)]])
  
  fign1 <- round(bargraph_specific_section(b,"L1","weekday"), digits = 4)
  fign2 <- round(bargraph_specific_section(b,"L1","weekend"), digits = 4)
  
  fign3 <- round(bargraph_specific_section(b,"24H","weekday"), digits = 4)
  fign4 <- round(bargraph_specific_section(b,"24H","weekend"), digits = 4)
  
  fign5 <- round(bargraph_specific_section(b,"L2","weekday"), digits = 4)
  fign6 <- round(bargraph_specific_section(b,"L2","weekend"), digits = 4)
  
  for(i in 1:length(a)) {
    a[[i]] <- count_aggregate(a[[i]])
    a[[i]] <- convert_previous(a[[i]])
    a[[i]] <- addcolumn(a[[i]])
  }
  
  data_print <- a[[1]]
  
  if(length(a) > 1) {
    for(i in 2:length(a)) {
      data_print <- rbind(data_print, a[[i]])
    }
  }
  monthmaxmin1(a[[length(a)]])
  message(sprintf("---------4 Main---------"))
  monthmaxmin2(a[[length(a)]])
  
  data_weekday <- weekday_data(data_print)
  colnames(data_weekday) <- as.character(data_weekday[1,])
  data_weekday <- data_weekday[-c(1),-c(1,2,3,5,6)]
  for (i in 3:ncol(data_weekday)) {
    data_weekday[,i] <- convertnumeric(data_weekday[,i])
  }
  
  fig1 <- round(GraphOverallDay(a), digits = 2)
  fig1_4 <- round(GraphOverallDay2(a), digits = 2)
  
  data_weekend <- weekend_data(data_print)
  colnames(data_weekend) <- data_weekend[1,]
  data_weekend <- data_weekend[-c(1),-c(1,2,3,5,7)]
  for (i in 3:ncol(data_weekend)) {
    data_weekend[,i] <- convertnumeric(data_weekend[,i])
  }
  
  fig2 <- round(GraphOverallEnd(a), digits = 2)
  fig2_4 <- round(GraphOverallEnd2(a), digits = 2)
  
  fig21 <- round(BarMonthsDay(a), digits = 4)
  fig22 <- round(BarMonthsEnd(a), digits = 4)
  
  month_weekday <- weekday_data(a[[length(a)]])
  colnames(month_weekday) <- month_weekday[1,]
  month_weekday <- month_weekday[-c(1),-c(1,2,3,5,6)]
  for (i in 3:ncol(month_weekday)) {
    month_weekday[,i] <- convertnumeric(month_weekday[,i])
  }
  
  fig3 <- round(GraphSpacesDay(a[[length(a)]]), digits = 2)
  
  month_weekend <- weekend_data(a[[length(a)]])
  colnames(month_weekend) <- month_weekend[1,]
  month_weekend <- month_weekend[-c(1),-c(1,2,3,5,7)]
  for (i in 3:ncol(month_weekend)) {
    month_weekend[,i] <- convertnumeric(month_weekend[,i])
  }
  
  fig4 <- round(GraphSpacesEnd(a[[length(a)]]), digits = 2)
  
  fig41 <- round(Bar4spaceDay(month_weekday), digits = 4)
  fig42 <- round(Bar4spaceEnd(month_weekend), digits = 4)
  
  write.xlsx(as.data.frame(specific), file = "data.xlsx",sheetName = "Seats", append = FALSE,
             col.names = TRUE, row.names = FALSE)
  write.xlsx(as.data.frame(data_weekday), file = "data.xlsx",sheetName = "Data_Figure1", append = TRUE,
             col.names = TRUE, row.names = FALSE)
  write.xlsx(fig1, file = "data.xlsx",sheetName="OverallDay", append=TRUE)
  write.xlsx(fig1_4, file = "data.xlsx",sheetName="OverallDay_4Main", append=TRUE)
  write.xlsx(as.data.frame(data_weekend), file = "data.xlsx",sheetName="Data_Figure2", append=TRUE,
             col.names = TRUE, row.names = FALSE)
  write.xlsx(fig2, file = "data.xlsx",sheetName="OverallEnd", append=TRUE)
  write.xlsx(fig2_4, file = "data.xlsx",sheetName="OverallEnd_4Main", append=TRUE)
  write.xlsx(fig21, file = "data.xlsx",sheetName="BarMonthsDay", append=TRUE)
  write.xlsx(fig22, file = "data.xlsx",sheetName="BarMonthsEnd", append=TRUE)
  write.xlsx(as.data.frame(month_weekday), file = "data.xlsx",sheetName="Data_Figure3", append=TRUE,
             col.names = TRUE, row.names = FALSE)
  write.xlsx(fig3, file = "data.xlsx",sheetName="SpacesDay", append=TRUE)
  write.xlsx(as.data.frame(month_weekend), file = "data.xlsx",sheetName="Data_Figure4", append=TRUE,
             col.names = TRUE, row.names = FALSE)
  write.xlsx(fig4, file = "data.xlsx",sheetName="SpacesEnd", append=TRUE)
  write.xlsx(fig41, file = "data.xlsx",sheetName="Bar4spaceDay", append=TRUE)
  write.xlsx(fig42, file = "data.xlsx",sheetName="Bar4spaceEnd", append=TRUE)
  
  write.xlsx(fign1, file = "data.xlsx",sheetName="BarMonthL1Day", append=TRUE)
  write.xlsx(fign2, file = "data.xlsx",sheetName="BarMonthL1End", append=TRUE)
  write.xlsx(fign3, file = "data.xlsx",sheetName="BarMonth24HDay", append=TRUE)
  write.xlsx(fign4, file = "data.xlsx",sheetName="BarMonth24HEnd", append=TRUE)
  write.xlsx(fign5, file = "data.xlsx",sheetName="BarMonthL2Day", append=TRUE)
  write.xlsx(fign6, file = "data.xlsx",sheetName="BarMonthL2End", append=TRUE)
  
  fig5 <- round(GraphLevel1Day(a), digits = 2)
  fig6 <- round(GraphLevel1End(a), digits = 2)
  
  write.xlsx(fig5, file = "data.xlsx",sheetName="Level1Day", append=TRUE)
  write.xlsx(fig6, file = "data.xlsx",sheetName="Level1End", append=TRUE)
  
  fig7 <- round(GraphStacksDay(a), digits = 2)
  fig8 <- round(GraphStacksEnd(a), digits = 2)
  
  write.xlsx(fig7, file = "data.xlsx",sheetName="StacksDay", append=TRUE)
  write.xlsx(fig8, file = "data.xlsx",sheetName="StacksEnd", append=TRUE)
  
  fig9 <- round(GraphGSR13Day(a), digits = 2)
  fig10 <- round(GraphGSR13End(a), digits = 2)
  
  write.xlsx(fig9, file = "data.xlsx",sheetName="GSR13Day", append=TRUE)
  write.xlsx(fig10, file = "data.xlsx",sheetName="GSR13End", append=TRUE)
  
  fig11 <- round(GraphGSR45Day(a), digits = 2)
  fig12 <- round(GraphGSR45End(a), digits = 2)
  
  write.xlsx(fig11, file = "data.xlsx",sheetName="GSR45Day", append=TRUE)
  write.xlsx(fig12, file = "data.xlsx",sheetName="GSR45End", append=TRUE)
  
  fig13 <- round(GraphGSR67Day(a), digits = 2)
  fig14 <- round(GraphGSR67End(a), digits = 2)
  
  write.xlsx(fig13, file = "data.xlsx",sheetName="GSR67Day", append=TRUE)
  write.xlsx(fig14, file = "data.xlsx",sheetName="GSR67End", append=TRUE)
  
  fig15 <- round(GraphLevel2Day(a), digits = 2)
  fig16 <- round(GraphLevel2End(a), digits = 2)
  
  write.xlsx(fig15, file = "data.xlsx",sheetName="Level2Day", append=TRUE)
  write.xlsx(fig16, file = "data.xlsx",sheetName="Level2End", append=TRUE)
  
  fig17 <- round(GraphCompDay(a), digits = 2)
  fig18 <- round(GraphCompEnd(a), digits = 2)
  
  write.xlsx(fig17, file = "data.xlsx",sheetName="CompDay", append=TRUE)
  write.xlsx(fig18, file = "data.xlsx",sheetName="CompEnd", append=TRUE)
  
  fig19 <- round(Graph24HDay(a), digits = 2)
  fig20 <- round(Graph24HEnd(a), digits = 2)
  
  write.xlsx(fig19, file = "data.xlsx",sheetName="24HDay", append=TRUE)
  write.xlsx(fig20, file = "data.xlsx",sheetName="24HEnd", append=TRUE)
  
  
} ####Çalýþma sahasý
