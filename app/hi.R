library('usmap')

# Calculates the average AQI over a range of years
averageAQICalculator <- function(startYear, endYear) {
  
  # biglist: 1 = county code
  #          2 = average AQI
  
  county <- c()
  aqi <- c()
  years <- c()
  
  for (i in startYear:endYear) {
    
    name = paste("annual_aqi_by_county_", i, ".csv", sep = "")
    aqi_data <- read.csv(file = name)
    
    for (j in 1:nrow(aqi_data)) {
      
      if (determineCounty(aqi_data[j, 1], aqi_data[j, 2])) {
        
        countyCode = fips(aqi_data[j, 1], aqi_data[j, 2])
        if (length(which(county == countyCode)) > 0) {
          
          aqi[[which(county == countyCode)[[1]]]] <- aqi[[which(county == countyCode)[[1]]]] + aqi_data[j, 13]
          years[[which(county == countyCode)[[1]]]] <- years[[which(county == countyCode)[[1]]]]+ 1
          
        } else {
          if (i == startYear) {
            county <- append(county, countyCode)
            aqi <- append(aqi, aqi_data[j, 13])
            years <- append(years, 1)
          } 
        }
      }
    }
  }
  
  delete <- which(years < endYear - startYear + 1)
  aqi <- aqi[-c(delete)]
  county <- county[-c(delete)]
  
  aqi <- aqi / (endYear - startYear + 1)
  biglist <- list(county, aqi)
  print(biglist)
  
  return(biglist)
  
}

# Calculates the average number of bad days over a range of years
averageBadDayCalculator <- function(startYear, endYear) {
  
  # biglist: 1 = county code
  #          2 = average bad days
  
  county <- c()
  aqi <- c()
  years <- c()
  additionalDays <- 0
  
  for (i in startYear:endYear) {
    
    name = paste("annual_aqi_by_county_", i, ".csv", sep = "")
    aqi_data <- read.csv(file = name)
    days <- 365
    if (i %% 4 == 0) {
      days <- 366
      additionalDays <- additionalDays + 1
    }
    
    for (j in 1:nrow(aqi_data)) {
      
      if (determineCounty(aqi_data[j, 1], aqi_data[j, 2])) {
        
        countyCode = fips(aqi_data[j, 1], aqi_data[j, 2])
        if (length(which(county == countyCode)) > 0) {
          
          aqi[[which(county == countyCode)[[1]]]] <- aqi[[which(county == countyCode)[[1]]]] + (aqi_data[j, 8] + aqi_data[j, 9]) * (days/aqi_data[j, 4])
          years[[which(county == countyCode)[[1]]]] <- years[[which(county == countyCode)[[1]]]]+ 1
          
        } else {
          if (i == startYear) {
            county <- append(county, countyCode)
            aqi <- append(aqi, (aqi_data[j, 8] + aqi_data[j, 9]) * (days/aqi_data[j, 4]))
            years <- append(years, 1)
          } 
        }
      }
    }
  }
  
  delete <- which(years < endYear - startYear + 1)
  aqi <- aqi[-c(delete)]
  county <- county[-c(delete)]
  
  aqi <- aqi / (((endYear - startYear + 1) * 365 + additionalDays) / 365)
  biglist <- list(county, aqi)
  print(biglist)
  
  return(biglist)
  
}

# Finds the average amount of pollution by dividing sum of data by total days
averagePollutant <- function(pollutant, startYear, endYear) {
  # biglist: index 1 = county code
  #          index 2 = average concentration per day
  #          index 3 = number of days
  countyCode <- c()
  averageConcentration <- c()
  numOfDays <- c()
  years <- c()
  die <- c()
  for (k in startYear:endYear) {
    name = paste("daily_", pollutant, "_", k, ".csv", sep = "")
    csv_data <- read.csv(file = name)
  for (i in 1:nrow(csv_data)) {
    if(determineNumber(csv_data[i, 1])) {
      county =  as.numeric(csv_data[i, 1]) * 1000 + as.numeric(csv_data[i, 2])
      if (length(which(countyCode == county)) > 0) {
        averageConcentration[[which(countyCode == county)[[1]]]] <- averageConcentration[[which(countyCode == county)[[1]]]] + csv_data[i, 17]
        numOfDays[[which(countyCode == county)[[1]]]] <- numOfDays[[which(countyCode == county)[[1]]]] + 1
        die[[which(countyCode == county)[[1]]]] = 1
      } else {
        countyCode <- append(countyCode, county)
        averageConcentration <- append(averageConcentration, csv_data[i, 17])
        numOfDays <- append(numOfDays, 1)
        die <- append(die, 1)
        years <- append(years, 0)
      }
    } else {
      break
    }
  }
    years <- years + die
    die <- rep(c(0), each = length(numOfDays))
  }
  delete <- which(years < (endYear - startYear + 1))
  averageConcentration <- averageConcentration[-c(delete)]
  numOfDays <- numOfDays[-c(delete)]
  countyCode <- countyCode[-c(delete)]
  averageConcentration <- averageConcentration / numOfDays
  biglist <- list(countyCode, averageConcentration, numOfDays)
  return(biglist)
}

# Find average pollution by assuming the entire year is the same and dividing by year
averagePollutantYear <- function(pollutant, startYear, endYear) {
  # biglist: index 1 = county code
  #          index 2 = average concentration per day
  #          index 3 = number of days
  countyCode <- c()
  averageConcentration <- c()
  numOfDays <- c()
  years <- c()
  die <- c()
  yearDays <- c()
  yearAverage <- c()
  for (k in startYear:endYear) {
    name = paste("daily_", pollutant, "_", k, ".csv", sep = "")
    csv_data <- read.csv(file = name)
  for (i in 1:nrow(csv_data)) {
    if(determineNumber(csv_data[i, 1])) {
      county =  as.numeric(csv_data[i, 1]) * 1000 + as.numeric(csv_data[i, 2])
      if (length(which(countyCode == county)) > 0) {
        yearAverage[[which(countyCode == county)[[1]]]] <- yearAverage[[which(countyCode == county)[[1]]]] + csv_data[i, 17]
        numOfDays[[which(countyCode == county)[[1]]]] <- numOfDays[[which(countyCode == county)[[1]]]] + 1
        die[[which(countyCode == county)[[1]]]] = 1
      } else {
        countyCode <- append(countyCode, county)
        averageConcentration <- append(averageConcentration, 0)
        numOfDays <- append(numOfDays, 1)
        die <- append(die, 1)
        yearDays <- append(yearDays, 0)
        yearAverage <- append(yearAverage, csv_data[i, 17])
      }
    } else {
      break
    }
  }
    days <- 365
    if (k %% 4 == 0) {
      days <- 366
    }
    yearDays <- yearDays + days * die
    yearAverage <- yearAverage * die * days / numOfDays
    averageConcentration <- averageConcentration + yearAverage
    yearsAverage <- rep(c(0), each = length(averageConcentration))
    die <- rep(c(0), each = length(numOfDays))
  }
  delete <- which(yearDays < (endYear - startYear + 1) * 365)
  averageConcentration <- averageConcentration[-c(delete)]
  countyCode <- countyCode[-c(delete)]
  averageConcentration <- averageConcentration / (yearDays %/% 365)
  biglist <- list(countyCode, averageConcentration)
  return(biglist)
}

# Sums the amount of days with high concentrations of a particular air pollutant
amountHighPollutant <- function(csv_data) {
  
  # biglist: index 1 = county code
  #          index 2 = average concentration per day
  #          index 3 = number of days
  countyCode <- c()
  averageConcentration <- c()
  numOfDays <- c()
  
  for (i in 1:nrow(csv_data)) {
    
    if(determineNumber(csv_data[i, 1])) {
      
      county =  as.numeric(csv_data[i, 1]) * 1000 + as.numeric(csv_data[i, 2])
      if (length(which(countyCode == county)) > 0) {
      
        if (csv_data[i, 17]>35.5) {
          averageConcentration[[which(countyCode == county)[[1]]]] <- averageConcentration[[which(countyCode == county)[[1]]]] + csv_data[i, 17]
        }
        numOfDays[[which(countyCode == county)[[1]]]] <- numOfDays[[which(countyCode == county)[[1]]]] + 1
      } else {
        countyCode <- append(countyCode, county)
        if (csv_data[i, 17] > 35.5) {
          averageConcentration <- append(averageConcentration, csv_data[i, 17])
        }
        numOfDays <- append(numOfDays, 1)
      }
    
    } else {
      break
    }
    
  }
  
  days <- 365
  if (biggerlist[[i]][[1]] %% 4 == 0) {
    days <- 366
  }

for (i in 1:length(biglist)) {
  biglist[[i]][[2]] <- biglist[[i]][[2]] * (days / biglist[[i]][[3]])
  biglist[[i]][[3]] <- days
}
  
}

totalDaysMultipleYears <- function(biggerlist) {
  # biggerlist: index 1: year
  #             index 2: biglist (biggerlist[[i]][[2]] is biglist)
  
  # collectivelist: index 1: countycode
  #                 index 2: average
  #                 index 3: number of days calculated
  
  # stupidvector: list of the index of counties
  
  collectivelist <- list()
  stupidvector <- c()
  
  # set up stupidvector and collectivelist to have all the counties from the first biglist
  for (i in 1:length(biggerlist[[1]][[2]])) {
    
    sublist <- list(biggerlist[[1]][[2]][[i]][[1]], 0, 0)
    collectivelist <- append(collectivelist, list(sublist))
    stupidvector <- append(stupidvector, biggerlist[[1]][[2]][[i]][[1]])
    
  }
  
  # loop through each biglist
  for (i in 1:length(biggerlist)) {
    
    # loop through each county in biglist
    for (j in 1:length(biggerlist[[i]][[2]])) {
      
      if (length(which(stupidvector == biggerlist[[i]][[2]][[j]][[1]])) > 0) {
        
        index = which(stupidvector == biggerlist[[i]][[2]][[j]][[1]])[1]
        collectivelist[[index]][[2]] <- collectivelist[[index]][[2]] + biggerlist[[i]][[2]][[j]][[2]]
        collectivelist[[index]][[3]] <- collectivelist[[index]][[3]] + biggerlist[[i]][[2]][[j]][[3]]
        
      }
      
    }
    
  }
    
  # code that kills whichever counties did not have data in all the years
    for (i in length(collectivelist):1) {
      
      if (collectivelist[[i]][[3]] >= 365 * length(biggerlist)) {
        
      } else {
        collectivelist[[i]] <- NULL
      }
      
    }
  
  return(collectivelist)
  
}

averagePollutantMultipleYears <- function(biggerlist) {
  # biggerlist: index 1: year
  #             index 2: biglist (biggerlist[[i]][[2]] is biglist)
  
  # collectivelist: index 1: countycode
  #                 index 2: average
  #                 index 3: number of days calculated
  
  # stupidvector: list of the index of counties
  
  collectivelist <- list()
  stupidvector <- c()
  
  # set up stupidvector and collectivelist to have all the counties from the first biglist
  for (i in 1:length(biggerlist[[1]][[2]])) {
    
    sublist <- list(biggerlist[[1]][[2]][[i]][[1]], 0, 0)
    collectivelist <- append(collectivelist, list(sublist))
    stupidvector <- append(stupidvector, biggerlist[[1]][[2]][[i]][[1]])
    
  }
  
  # loop through each biglist
  for (i in 1:length(biggerlist)) {
    
    # determine if leap year
    days <- 365
    if (biggerlist[[i]][[1]] %% 4 == 0) {
      days <- 366
    } 
    
    # loop through each county in biglist
    for (j in 1:length(biggerlist[[i]][[2]])) {
      
      if (length(which(stupidvector == biggerlist[[i]][[2]][[j]][[1]])) > 0) {
        
        index = which(stupidvector == biggerlist[[i]][[2]][[j]][[1]])[1]
        collectivelist[[index]][[2]] <- collectivelist[[index]][[2]] + biggerlist[[i]][[2]][[j]][[2]] * days
        collectivelist[[index]][[3]] <- collectivelist[[index]][[3]] + days
        
      }
      
    }
    
  }
    
    for (i in 1:length(collectivelist)) {
      collectivelist[[i]][[2]] <- collectivelist[[i]][[2]] / collectivelist[[i]][[3]]
    }
    
  # code that kills whichever counties did not have data in all the years
    for (i in length(collectivelist):1) {
      
      if (collectivelist[[i]][[3]] >= 365 * length(biggerlist)) {
        
      } else {
        collectivelist[[i]] <- NULL
      }
      
    }
  
  return(collectivelist)
  
}

determineNumber <- function(x) {
  
  tryCatch(
    
    {
      as.numeric(x)
      return(TRUE)
    },
    warning = function(w) {
      return(FALSE)
    }
    
  )
  
}

determineCounty <- function(a, b) {
  
  tryCatch(
    
    {
      fips(a, b)
      return(TRUE)
    },
    error = function(e) {
      return(FALSE)
    }
    
  )
  
}

cancer <- function(pollution, states) {
  
  listy <- list()
  vecy <- c()
  for (i in 1:length(pollution)) {
    listy <- append(listy, list(list(pollution[[i]][[1]], pollution[[i]][[2]],-1, 0)))
    vecy <- append(vecy, pollution[[i]][[1]])
  }
  
  cancer <- read.csv('nonhodgkinlymphoma.csv')
  for (i in 2:nrow(cancer)) {
    
    # looks through the dataset until it gets to the counties without data
    if (determineNumber(cancer[i, 3])) { 
    
    # adds cancer data to listy
    if (length(which(vecy == as.numeric(cancer[i, 2]))) > 0) { 
      
      # removes counties with big/small populations
      #if (as.numeric(cancer[i, 3]) * 100000 / as.numeric(cancer[i, 9]) <= 50000) {
      
      listy[[which(vecy == as.numeric(cancer[i, 2]))[[1]]]][[3]] <- as.numeric(cancer[i, 3])
      
      #}
      
    }
      
    } else {
      break
    }
    
  }
  
  # removes stuff from listy that is not good enough
  for (i in length(listy):1) {
    
    if (listy[[i]][[3]] == -1) {
      listy[[i]] <- NULL # removes the ones that did not have data in all the years
    } #else if (length(which(states==(listy[[i]][[1]] %/% 1000))) == 0) {
      #listy[[i]] <- NULL # removes everything not in the desired state
    #} 
    
  }
  
  # listy: 1 = fips
  #        2 = pollution average
  #        3 = cancer incidence
  #        4 = carcinogen
  
  return(listy)
  
}

smoking <- function(listy) {
  
  # making a vector of all the counties in listy. it should only contain the states we want
  stupidvector <- c()
  for (i in 1:length(listy)) {
    stupidvector <- append(stupidvector, listy[[i]][[1]])
  }
  
  smokingData <- read.csv("pennsylvania-2016-adult-smoking-place-sort.csv")
  
  # loops through the smoking data
  for (i in 2:nrow(smokingData)) {
    
    x <- fips(smokingData[1, 1], smokingData[i, 1])
    if (length(which(stupidvector == x)) > 0) {
      
      # if x% of the population smokes, assume that x% of the cancer is caused by smoking
      listy[[which(stupidvector == x)]][[3]] <- listy[[which(stupidvector == x)]][[3]] * (1-as.numeric(sub("%","",smokingData[i, 2]))/100)
    }
    
  }
  print(listy)
  
  return(listy)
  
}

# Puts cancer and pollution data into one list
aqiCancer <- function(pollution) {
  
  county <- pollution[[1]]
  pollutionData <- pollution[[2]]
  cancerRate <- rep(c(0), each = length(county))
  cancer <- read.csv('7776714/youreka/incd.csv')
  
  for (i in 2:nrow(cancer)) {
    
    # looks through the dataset until it gets to the counties without data
    if (determineNumber(cancer[i, 3])) { 
    
      # adds cancer data to listy
      if (length(which(county == as.numeric(cancer[i, 2]))) > 0) { 
      
        # removes counties with big/small populations
        #if (as.numeric(cancer[i, 3]) * 100000 / as.numeric(cancer[i, 9]) <= 50000) {
      
          cancerRate[[which(county == as.numeric(cancer[i, 2]))[[1]]]] <- as.numeric(cancer[i, 3])
      
        #}
      
      }
      
    } else {
      break
    }
    
  }
  
  # removes stuff from listy that is not good enough
  delete <- which(cancerRate == 0)
  county <- county[-c(delete)]
  pollutionData <- pollutionData[-c(delete)]
  cancerRate <- cancerRate[-c(delete)]
  listy <- list(county, pollutionData, cancerRate)
  
  return(listy)
  
}

influenzaData <- function() {
  
  county <- c()
  influenzaRate <- c()
  influenza <- read.csv('C:/Users/Sheena/Downloads/7776714/7776714/youreka/HPAI Detections in Wild Birds.csv')
  #for (i in 1:nrow(influenza)) {
  for (i in 1:500) {
    # looks through the dataset until it gets to the counties without data
    #if (determineNumber(influenza[i, 3])) { 

print(i)
    # INSERT OTHER CONDITIONS HERE
    if (determineCounty(influenza[i, 1], influenza[i, 2])) {
    
      # adds cancer data to listy
      if (length(which(county == fips(influenza[i, 1], influenza[i, 2]))) > 0) { 
        # removes counties with big/small populations
        #if (as.numeric(cancer[i, 3]) * 100000 / as.numeric(cancer[i, 9]) <= 50000) {
          previousNum <- influenzaRate[[which(county == fips(influenza[i, 1], influenza[i, 2]))[[1]]]] + 1
          influenzaRate[[which(county == fips(influenza[i, 1], influenza[i, 2]))[[1]]]] <- previousNum
      
        #}
      
      } else {

        county <- append(county, fips(influenza[i, 1], influenza[i, 2]))
        influenzaRate <- append(influenzaRate, 1);

      }

    }
      
    #} else {
      #break
    #}
    
  }
  
  listy <- list(county, influenzaRate)
  
  return(listy)

}

# Puts cancer and pollution data into one list
aqiInfluenza <- function(pollution) {
  
  county <- pollution[[1]]
  pollutionData <- pollution[[2]]
  influenzaRate <- rep(c(0), each = length(county))
  influenza <- read.csv('C:/Users/Sheena/Downloads/7776714/7776714/youreka/HPAI Detections in Wild Birds.csv')
  print(nrow(influenza))
  for (i in 2:nrow(influenza)) {
    # looks through the dataset until it gets to the counties without data
    #if (determineNumber(influenza[i, 3])) { 
    if (determineCounty(influenza[i, 1], influenza[i, 2])) {
      print(1)
    
      # adds cancer data to listy
      if (length(which(county == fips(influenza[i, 1], influenza[i, 2]))) > 0) { 
      
      print(2)
        # removes counties with big/small populations
        #if (as.numeric(cancer[i, 3]) * 100000 / as.numeric(cancer[i, 9]) <= 50000) {
          previousNum <- influenzaRate[[which(county == fips(influenza[i, 1], influenza[i, 2]))[[1]]]] + 1
          influenzaRate[[which(county == fips(influenza[i, 1], influenza[i, 2]))[[1]]]] <- previousNum
      
        #}
      
      }

    }
      
    #} else {
      #break
    #}
    
  }
  
  # removes stuff from listy that is not good enough
  delete <- which(influenzaRate == 0)
  county <- county[-c(delete)]
  pollutionData <- pollutionData[-c(delete)]
  influenzaRate <- influenzaRate[-c(delete)]
  listy <- list(county, pollutionData, influenzaRate)
  
  return(listy)
  
}

uvThing <- function() {
  
  lowerLimit <- c(3000, 3200, 3400, 3600, 3800, 4000, 4200, 4400, 4600, 4800, 5000, 5200, 5400, 5600, 5800)
  list <- list(c(), c(), c(), c(), c(), c(), c(), c(), c(), c(), c(), c(), c(), c(), c(), c(), c(), c(), c(), c(), c(), c(), c(), c(), c(), c(), c(), c(), c())
  
  data <- read.csv("7776714/youreka/uv-county.csv")
  for (i in 1:nrow(data)) {
    
    uv <- data[i, 5]
    index <- which(lowerLimit == (uv %/% 200) * 200)
    list[[index]] <- append(list[[index]], data[i, 3])
    
  }
  
  return(list)
  
}

uvAndCancer <- function(data, listy) {
  
  # data is cancer + pollution
  # list is the uv list
  print(length(listy))
  for (i in 1:length(listy)) {
    
    pollution <- c()
    cancer <- c()
    counties <- listy[[i]]
    county <- c()
    
    for (j in 1:length(counties)) {
      if (length(which(data[[1]] == counties[[j]])) > 0) {
        pollution <- append(pollution, data[[2]][[which(data[[1]] == counties[[j]])[[1]]]])
        cancer <- append(cancer, data[[3]][[which(data[[1]] == counties[[j]])[[1]]]])
        county <- append(county, counties[[j]])
      }
      
    }
    
    if (length(pollution) > 1) {
        print("-----------------------------------------------")
        print(paste("list number ", i))
        print(county)
        print(mean(pollution))
        print(mean(cancer))
        print(paste("amount of data: ", length(pollution)))
        betterCoordinates(list(county, pollution, cancer))
        print("-----------------------------------------------")
    }

  }
  
}

betterCoordinates <- function(listy) {
  
  # makes a graph and prints the correlation thing
  x <- listy[[2]]
  y <- listy[[3]]
  
  plot(x, y)
  abline(lm(y ~ x))
  print(cor(x, y))
  
}

multipleRegression <- function(pollutants, startYear, endYear) {


  # turn dataset into vector
  #data <- read.csv("7776714/youreka/uv-county.csv")
  #uv <- list(data[["COUNTY_FIPS"]], data[[5]])
  # does intervalCalculation of all pollutants
  precip <- read.csv('C:/Users/Sheena/Downloads/7776714/7776714/youreka/data (4).csv')
  temp <- read.csv('C:/Users/Sheena/Downloads/7776714/7776714/youreka/data (3).csv')
  precipitate <- list()
  temperature <- list()
  countp <- list()
  countt <- list()
  for (i in 6:nrow(precip)) {
    countp <- append(countt, as.numeric(fips(precip[i, 3], precip[i, 2])))
    precipitate <- append(precipitate, as.numeric(precip[i, 4]))
  }
  for (i in 6:nrow(temp)) {
    countt <- append(countt, as.numeric(fips(temp[i, 3], temp[i, 2])))
    temperature <- append(temperature, as.numeric(prectempip[i, 4]))
  }
  biglist <- list()
  for (i in 1:length(pollutants)) {
    biglist <- append(biglist, list(intervalCalculation(pollutants[[i]], startYear, endYear)))
  }
  print(biglist)

  finalList <- c()

  for (i in 1:length(biglist[[1]][[1]])) {
    print(biglist[[1]][[1]][[i]])
    check = 0
    if (length(biglist) > 1) {
    for (j in 2:length(biglist)) {
      if (length(which(biglist[[j]][[1]] == biglist[[1]][[1]][[i]])) > 0) {
        check <- check + 1
      }
    }
    }
    # if (length(which(uv[[1]] == biglist[[1]][[1]][[i]])) > 0) {
    #   check <- check + 1
    # }
    # if (length(which(countt == biglist[[1]][[1]][[i]])) > 0) {
    #   check <- check + 1
    # }
    # if (length(which(countp == biglist[[1]][[1]][[i]])) > 0) {
    #   check <- check + 1
    # }
    if (check == length(biglist) + 1) {
      print("check!!")
      finalList <- append(finalList, biglist[[1]][[1]][[i]])
    }
  }

  print(finalList)

  finalbiglist <- list()

  cancerRate <- rep(c(0), each = length(finalList))
  cancer <- read.csv('C:/Users/Sheena/Downloads/7776714/7776714/youreka/LD_Case_Counts_by_County_2023_updated (1).csv')
  
  for (i in 2:nrow(cancer)) {
    
    # looks through the dataset until it gets to the counties without data
    #if (determineNumber(cancer[i, 4])) { 
    #if (cancer[i, 28] != 0) {
    
      # adds cancer data to listy
      if (length(which(finalList == 1000 * cancer[i, 4] + cancer[i, 5])) > 0) { 
        print(1000 * cancer[i, 4] + cancer[i, 5])
      
        # removes counties with big/small populations
        #if (as.numeric(cancer[i, 3]) * 100000 / as.numeric(cancer[i, 9]) <= 50000) {
      
          cancerRate[[which(finalList == 1000 * cancer[i, 4] + cancer[i, 5])[[1]]]] <- cancer[i, 28]
      
        #}
      
      }
      
    #} else {
      #break
    #}
    
  }

  print(cancerRate)

  pop = read.csv('C:/Users/Sheena/Downloads/7776714/7776714/youreka/co-est2024-alldata (1).csv') 

  for (i in 2:nrow(pop)) {
    
    # looks through the dataset until it gets to the counties without data
    #if (determineNumber(cancer[i, 4])) { 
    if (pop[i, 12] != 0) {
    
      # adds cancer data to listy
      if (length(which(finalList == 1000 * pop[i, 4] + pop[i, 5])) > 0) { 
      
        # removes counties with big/small populations
        #if (as.numeric(cancer[i, 3]) * 100000 / as.numeric(cancer[i, 9]) <= 50000) {
      
          cancerRate[[which(finalList == 1000 * pop[i, 4] + pop[i, 5])[[1]]]] <- cancerRate[[which(finalList == 1000 * pop[i, 4] + pop[i, 5])[[1]]]] / pop[i, 12]
      
        #}
      
      }
      
    } else {
      #break
    }
    
  }

  print(finalList)
  print(cancerRate)
  
  # removes stuff from listy that is not good enough
  delete <- which(cancerRate == 0)
  finalList <- finalList[-c(delete)]
  cancerRate <- cancerRate[-c(delete)]
  finalbiglist <- append(finalbiglist, list(cancerRate))

  #finalbiglist <- append(finalbiglist, list("UV", uv[[2]][which(uv[[1]] %in% finalList)]))
  #print(uv)  
  #finalbiglist <- append(finalbiglist, list("Precipitation", precipitate[which(countp[[1]] %in% finalList)]))


  for (i in 1:length(pollutants)) {
    pollutantNum = ""
  if (pollutants[[i]] == 44201) {
    pollutantNum = "Ozone"
  } else if (pollutants[[i]] == 42602) {
    pollutantNum = "Nitrogen Dioxide"
  } else if (pollutants[[i]] == 42401) {
    pollutantNum = "Sulfur Dioxide"
  } else if (pollutants[[i]] == 42101) {
    pollutantNum = "Carbon Monoxide"
  } else if (pollutants[[i]] == 81102) {
    pollutantNum = "PM 10"
  } else if (pollutants[[i]] == 88101) {
    pollutantNum = "PM 2.5"
  }
    finalbiglist <- append(finalbiglist, list(pollutantNum, biglist[[i]][[2]][which(biglist[[i]][[1]] %in% finalList)]))
  }

  print(finalList)
  print(finalbiglist)

  model <- lm(finalbiglist[[1]] ~ finalbiglist[[3]] + finalbiglist[[5]] + finalbiglist[[7]] + finalbiglist[[9]]) #+ finalbiglist[[11]] + finalbiglist[[13]])
  print(summary(model))

  return(finalbiglist)

}

intervalCalculation <- function(pollutant, startYear, endYear) {
  
  list <- list()
  finalCounties <- c()
  finalPollution <- c()
  #for (i in startYear:endYear) {
    i = startYear
    sublist <- list()
    if (allPollutionData$has(paste(pollutant, i, sep = ""))) {
      sublist <- allPollutionData$get(paste(pollutant, i, sep = ""))
    } else {
      name <- paste("C:/Users/Sheena/Downloads/7776714/7776714/youreka/daily_", pollutant, "_", i, ".csv", sep = "")
      csv_data <- read.csv(file = name)
      countyCode <- c()
      averageConcentration <- c()
      numOfDays <- c()
      die <- c()
  for (j in 1:nrow(csv_data)) {
    if(determineNumber(csv_data[j, 1])) {
      county =  as.numeric(csv_data[j, 1]) * 1000 + as.numeric(csv_data[j, 2])
      if (length(which(countyCode == county)) > 0) {
        averageConcentration[[which(countyCode == county)[[1]]]] <- averageConcentration[[which(countyCode == county)[[1]]]] + csv_data[j, 17]
        numOfDays[[which(countyCode == county)[[1]]]] <- numOfDays[[which(countyCode == county)[[1]]]] + 1
        die[[which(countyCode == county)[[1]]]] = 1
      } else {
        countyCode <- append(countyCode, county)
        averageConcentration <- append(averageConcentration, csv_data[j, 17])
        numOfDays <- append(numOfDays, 1)
        die <- append(die, 1)
      }
    } else {
      break
    }
  }
      
      delete <- which(die == 0)
      if (length(delete) > 0) {
        countyCode <- countyCode[-c(delete)]
        averageConcentration <- averageConcentration[-c(delete)]
        numOfDays <- numOfDays[-c(delete)]
      }
      averageConcentration <- averageConcentration / numOfDays
      sublist <- list(countyCode, averageConcentration)
      allPollutionData[paste(pollutant, i, sep = "")] <- sublist
    }
    
    # FOR CANCER
    # max <- endYear - startYear - 3
    # if (max > 5) {
    #   max <- 5
    # }
    
    # multiply <- max
    # if (i == startYear | i == endYear) {
    #   multiply <- 1
    # } else if ((max > 2) & (i == startYear + 1 | i == endYear - 1)) {
    #   multiply <- 2
    # } else if ((max > 3) & (i == startYear + 2 | i == endYear - 2)) {
    #   multiply <- 3
    # } else if ((max > 4) & (i == startYear + 3 | i == endYear - 3)) {
    #   multiply <- 4
    # }
    
    # sublist[[2]] <- sublist[[2]] * multiply
    list <- append(list, list(sublist))
    
  #}
  
   for (i in 1:length(list[[1]][[1]])) {
  #   canInclude <- TRUE
     sum <- list[[1]][[2]][[i]]
  #   for (j in 2:length(list)) {
  #     if (length(which(list[[j]][[1]] == list[[1]][[1]][[i]])) == 0) {
  #       canInclude <- FALSE
  #       break
  #     } else {
  #       sum <- sum + list[[j]][[2]][[which(list[[j]][[1]] == list[[1]][[1]][[i]])[[1]]]]
  #     }
  #   }
  #   if (canInclude) {
       finalCounties <- append(finalCounties, list[[1]][[1]][[i]])
       finalPollution <- append(finalPollution, sum)
  #   }
   }
  
  #finalPollution <- finalPollution / (endYear-startYear-3) / 5
  print(finalCounties)
  print("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
  print(finalPollution)
  return(list(finalCounties, finalPollution))
  
}

influenzaAndPollution <- function(influenza, pollution) {

  pollValue <- rep(c(0), each = length(pollution[[1]]))
  for (i in 1:length(influenza[[1]])) {
    if (length(which(pollution[[1]] == influenza[[1]][[i]])) > 0) {
      pollValue[[which(pollution[[1]] == influenza[[1]][[i]])]][[1]] <- influenza[[2]][[i]]
    }
  }

  delete <- which(pollValue == 0)
  noDelete <- which(pollValue != 0)
  
  return(list(pollution[[1]], pollution[[2]], pollValue, delete, noDelete))

}

influenzaTTest <- function(biglist) {

countyF <- biglist[[1]][-c(biglist[[4]])]
pollutionF <- biglist[[2]][-c(biglist[[4]])]
influenza <- biglist[[3]][-c(biglist[[4]])]
countyN <- biglist[[1]][-c(biglist[[5]])]
pollutionN <- biglist[[2]][-c(biglist[[5]])]

print(countyF)
print("CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC")
print(pollutionF)
print("PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP")
print(influenza)
print("IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII")
# print(countyN)
# print("CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC")
# print(pollutionN)
# print("PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP")

#print(wtd.t.test(x = pollutionF, y = pollutionN, weight = influenza))
#print(t.test(pollutionF, pollutionN))

}

lymeCalculate <- function() {

  county <- c()
  lymeRate <- c()
  lyme <- read.csv('C:/Users/Sheena/Downloads/7776714/7776714/youreka/LD_Case_Counts_by_County_2023_updated (1).csv')

  for (i in 2:nrow(lyme)) {
    print(as.numeric(lyme[i, 28]) == 0)
      if (lyme[i, 28] != 0) {
      county <- append(county, 1000 * as.numeric(lyme[i, 4]) + as.numeric(lyme[i, 5]))
      lymeRate <- append(lymeRate, as.numeric(lyme[i, 28]))
      }
  }

  population <- read.csv('C:/Users/Sheena/Downloads/7776714/7776714/youreka/co-est2024-alldata (1).csv') 
  delete <- rep(c(0), each = length(lymeRate))
  print(lymeRate)

  for (i in 1:nrow(population)) {
    countyCode <- 1000*as.numeric(population[i, 4])+as.numeric(population[i, 5])
    if (length(which(county == countyCode)) > 0) {
      delete[[which(county == countyCode)[[1]]]] <- 1
      lymeRate[[which(county == countyCode)[[1]]]] <- lymeRate[[which(county == countyCode)[[1]]]] / population[i, 12]
    }
  }
  delete <- which(delete != 1)
  #county <- county[-c(delete)]
  #lymeRate <- lymeRate[-c(delete)]

  return(list(county, lymeRate))

}

# install.packages("Rmpfr", repos="http://R-Forge.R-project.org")
# library("Rmpfr")
# x <- mpfr(numbers,200)
install.packages("Dict")
library(Dict)
install.packages("weights")
library(weights)
allPollutionData <- dict(hi = 0)
# format: 

# <- averageAQICalculator(2011, 2016)
#c <- aqiCancer(b, 2)
#betterCoordinates(c)

# asking user for what pollutant and what year to gather all the databases and put them into biggerlist


pollutant <- 1
k <- lymeCalculate()

while (pollutant != 0) {

mode = readline(prompt = "Linear or multi or compare: ")
if (mode == "linear") {
  pollutant = readline(prompt = "Which pollutant: ")
  pollutantNum = 0
  if (pollutant == "ozone") {
    pollutantNum = 44201
  } else if (pollutant == "nitrogen dioxide") {
    pollutantNum = 42602
  } else if (pollutant == "sulfur dioxide") {
    pollutantNum = 42401
  } else if (pollutant == "carbon monoxide") {
    pollutantNum = 42101
  } else if (pollutant == "pm10") {
    pollutantNum = 81102
  } else if (pollutant == "pm2.5") {
    pollutantNum = 88101
  }
  one = readline(prompt = "Start year: ")
  one = as.integer(one)
  two = readline(prompt = "End year: ")
  two = as.integer(two)

  p <- intervalCalculation(pollutantNum, one, two)
  s <- influenzaAndPollution(k, p)
  influenzaTTest(s)

  
  #uvData <- uvThing()
  #a <- uvAndCancer(s, uvData)

} else if (mode == "compare") {
  list <- c()
  for(i in 1:2){
    # update for i
    pollutant = readline(prompt = "enter pollutant: ")
    if (pollutant == "ozone") {
      list <- append(list, 44201)
    } else if (pollutant == "nitrogen dioxide") {
      list <- append(list, 42602)
    } else if (pollutant == "sulfur dioxide") {
      list <- append(list, 42401)
    } else if (pollutant == "carbon monoxide") {
      list <- append(list, 42101)
    } else if (pollutant == "pm10") {
      list <- append(list, 81102)
    } else if (pollutant == "pm2.5") {
      list <- append(list, 88101)
    } else if (pollutant == "uv"){
      list <- append(list, 31415)
    }
  }
  one = readline(prompt = "Start year: ")
  one = as.integer(one)
  two = readline(prompt = "End year: ")
  two = as.integer(two)

  p1 <- c()
  p2 <- c()
  
  #uv <- read.csv("7776714/youreka/uv-county.csv")
  if(list[1] == 31415){
    p2 <- intervalCalculation(list[2], one, two)
    p1 <- list(uv[[3]][which(uv[[3]] %in% p2[[1]])], uv[[5]][which(uv[[3]] %in% p2[[1]])])
  } else if(list[2] == 31415){
    p1 <- intervalCalculation(list[1], one, two)
    p2 <- list(uv[[3]][which(uv[[3]] %in% p1[[1]])], uv[[5]][which(uv[[3]] %in% p1[[1]])])
  } else{
    p1 <- intervalCalculation(list[1], one, two)
    p2 <- intervalCalculation(list[2], one, two)
  }
  
  goodCounty <- c()
  for (i in 1:length(p1[[1]])) {
    if (length(which(p2[[1]] == p1[[1]][[i]])) > 0) {
      goodCounty <- append(goodCounty, p1[[1]][[i]])
    }
  }
  
  p1[[2]] <- p1[[2]][which(p1[[1]] %in% goodCounty)]
  p2[[2]] <- p2[[2]][which(p2[[1]] %in% goodCounty)]
  
  print("p1 begin")
  print(p1)
  print("p1 end")
  print("p2 begin")
  print(p2)
  print("p2 end")
  
  print(cor(p1[[2]], p2[[2]]))
  plot(p1[[2]], p2[[2]])
  
} else {
  w = as.integer(readline(prompt = "How many pollutants: "))
  list <- c()
  for (i in 1:w) {
    pollutant = readline(prompt = "Which pollutant: ")
    if (pollutant == "ozone") {
      list <- append(list, 44201)
    } else if (pollutant == "nitrogen dioxide") {
      list <- append(list, 42602)
    } else if (pollutant == "sulfur dioxide") {
      list <- append(list, 42401)
    } else if (pollutant == "carbon monoxide") {
      list <- append(list, 42101)
    } else if (pollutant == "pm10") {
      list <- append(list, 81102)
    } else if (pollutant == "pm2.5") {
      list <- append(list, 88101)
    }
  }

  one = readline(prompt = "Start year: ")
  one = as.integer(one)
  two = readline(prompt = "End year: ")
  two = as.integer(two)

  multipleRegression(list, one, two)

}
}