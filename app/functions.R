# stupid function!!
multipleRegression <- function(pollutants, startYear, endYear) {
  
  # turn dataset into vector
  # data <- read.csv("7776714/youreka/uv-county.csv")
  data <- read.csv("csv-dataset/unzipped/uv-county.csv")
  uv <- list(data[["COUNTY_FIPS"]], data[[5]])
  # does intervalCalculation of all pollutants
  biglist <- list()
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
    if (length(which(uv[[1]] == biglist[[1]][[1]][[i]])) > 0) {
      check <- check + 1
    }
    if (check == length(biglist)) {
      print("check!!")
      finalList <- append(finalList, biglist[[1]][[1]][[i]])
    }
  }
  
  print(finalList)
  
  finalbiglist <- list()
  
  cancerRate <- rep(c(0), each = length(finalList))
  # cancer <- read.csv('7776714/youreka/incd.csv')
  cancer <- read.csv("csv-dataset/unzipped/incd.csv")
  
  for (i in 2:nrow(cancer)) {
    
    # looks through the dataset until it gets to the counties without data
    if (determineNumber(cancer[i, 3])) { 
      
      # adds cancer data to listy
      if (length(which(finalList == as.numeric(cancer[i, 2]))) > 0) { 
        
        # removes counties with big/small populations
        #if (as.numeric(cancer[i, 3]) * 100000 / as.numeric(cancer[i, 9]) <= 50000) {
        
        cancerRate[[which(finalList == as.numeric(cancer[i, 2]))[[1]]]] <- as.numeric(cancer[i, 3])
        
        #}
        
      }
      
    } else {
      break
    }
    
  }
  
  # removes stuff from listy that is not good enough
  delete <- which(cancerRate == 0)
  finalList <- finalList[-c(delete)]
  cancerRate <- cancerRate[-c(delete)]
  finalbiglist <- append(finalbiglist, list(cancerRate))
  
  finalbiglist <- append(finalbiglist, list("UV", uv[[2]][which(uv[[1]] %in% finalList)]))
  print(uv)
  
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
  
  model <- lm(finalbiglist[[1]] ~ finalbiglist[[3]] + finalbiglist[[5]] + finalbiglist[[7]])
  print(summary(model))
  
  return(finalbiglist)
  
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

# Puts cancer and pollution data into one list
aqiCancer <- function(pollution) {
  
  county <- pollution[[1]]
  pollutionData <- pollution[[2]]
  cancerRate <- rep(c(0), each = length(county))
  # cancer <- read.csv('7776714/youreka/incd.csv')
  cancer <- read.csv('csv-dataset/unzipped/incd.csv')
  
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

uvThing <- function() {
  
  lowerLimit <- c(3000, 3200, 3400, 3600, 3800, 4000, 4200, 4400, 4600, 4800, 5000, 5200, 5400, 5600, 5800)
  list <- list(c(), c(), c(), c(), c(), c(), c(), c(), c(), c(), c(), c(), c(), c(), c(), c(), c(), c(), c(), c(), c(), c(), c(), c(), c(), c(), c(), c(), c())
  
  # data <- read.csv("7776714/youreka/uv-county.csv")
  data <- read.csv("csv-dataset/unzipped/uv-county.csv")
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
  
  #specify path to save PDF to
  destination = "C:/work/project/git-repo/youreka/app/figures/test.pdf"
  
  #open PDF
  pdf(file=destination)
  
  # par( mfrow= c(0.5,0.5) ) 
  
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
      print(paste("avg pollution: ", mean(pollution)))
      print(paste("avg cancer: ", mean(cancer)))
      print(paste("amount of data: ", length(pollution)))
      betterCoordinates(list(county, pollution, cancer, i))
      print("-----------------------------------------------")
    }
    
  }
  
  #turn off PDF plotting
  dev.off()
  
}


betterCoordinates <- function(listy) {
  
  # Makes a graph and prints the correlation
  x <- listy[[2]]
  y <- listy[[3]]
  
  plot(x, y, 
       main = paste(csv_data[2, 9], " vs. Melanoma (UV Intensity ", 2800 + 200 * listy[[4]], "-", 2800 + 200 * listy[[4]] + 200, "Wh/m²)", sep = ""),
       xlab = paste(csv_data[2, 9], " Concentration (", csv_data[2, 13], ")", sep = ""), 
       ylab = "Melanoma Incidence (Per 100,000 population)")
  
  abline(lm(y ~ x))
  mtext(paste("r =", cor(x, y)), side=3)
  print(paste("correlation: ", cor(x, y)))
  
}

plotRatesByEthnicity <- function(){
  
  # Create the data for the chart.
  x <- c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)
  white <- c(26.5, 26.9, 27.8, 28.9, 30.0, 30.1, 30.5, 29.8, 30.6, 26.1)
  black <- c(1.1, 1.0, 1.1, 1.1, 1.1, 1.0, 1.0, 0.9, 1.0, 0.8)
  indigenous <- c(8.3, 8.3, 8.9, 8.2, 9.7, 8.8, 8.7, 8.6, 8.4, 7.3)
  asian <- c(1.4, 1.3, 1.3, 1.5, 1.4, 1.3, 1.3, 1.3, 1.2, 1.2)
  hispanic <- c(4.5, 4.4, 4.5, 4.8, 4.6, 4.9, 4.8, 4.5, 4.5, 3.8)
  
  #specify path to save PDF to
  destination = "C:/work/project/git-repo/youreka/app/figures/test.pdf"
  #open PDF
  pdf(file=destination)
  
  # Plot the bar chart.
  plot(x, white, type = "o", col = "red",
       xlab = "Year", ylab = "Melanoma Rate (per 100,000 population)",
       main = "Melanoma Rates by Ethnicity", xlim=c(2011, 2020), ylim=c(0, 45), 
       yaxs="i", yaxp=c(0, 45, 9), xaxp=c(2011, 2020, 9))
  
  lines(x, black, type = "o", col = "purple")
  lines(x, indigenous, type = "o", col = "orange")
  lines(x, asian, type = "o", col = "green")
  lines(x, hispanic, type = "o", col = "blue")
  
  legend(x = "topleft", box.col = "brown", 
         box.lwd = 2 , title="LEGEND",  
         legend=c("White", "Black", "Indigenous", "Asian and Pacific Islander", "Hispanic"),  
         fill = c("red","purple", "orange", "green", "blue")) 
  
  #turn off PDF plotting
  dev.off()
  
}


intervalCalculation <- function(pollutant, startYear, endYear, interval) {
  
  list <- list()
  finalCounties <- c()
  finalPollution <- c()
  for (i in startYear:endYear) {
    
    sublist <- list()
    if (allPollutionData$has(paste(pollutant, i, sep = ""))) {
      sublist <- allPollutionData$get(paste(pollutant, i, sep = ""))
    } else {
      # name <- paste("7776714/youreka/daily_", pollutant, "_", i, ".csv", sep = "")
      name <- paste("csv-dataset/unzipped/daily_", pollutant, "_", i, ".csv", sep = "")
      csv_data <<- read.csv(file = name)
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
    
    max <- endYear - startYear - 3
    if (max > 5) {
      max <- 5
    }
    
    multiply <- max
    if (i == startYear | i == endYear) {
      multiply <- 1
    } else if ((max > 2) & (i == startYear + 1 | i == endYear - 1)) {
      multiply <- 2
    } else if ((max > 3) & (i == startYear + 2 | i == endYear - 2)) {
      multiply <- 3
    } else if ((max > 4) & (i == startYear + 3 | i == endYear - 3)) {
      multiply <- 4
    }
    
    sublist[[2]] <- sublist[[2]] * multiply
    list <- append(list, list(sublist))
    
  }
  
  for (i in 1:length(list[[1]][[1]])) {
    canInclude <- TRUE
    sum <- list[[1]][[2]][[i]]
    for (j in 2:length(list)) {
      if (length(which(list[[j]][[1]] == list[[1]][[1]][[i]])) == 0) {
        canInclude <- FALSE
        break
      } else {
        sum <- sum + list[[j]][[2]][[which(list[[j]][[1]] == list[[1]][[1]][[i]])[[1]]]]
      }
    }
    if (canInclude) {
      finalCounties <- append(finalCounties, list[[1]][[1]][[i]])
      finalPollution <- append(finalPollution, sum)
    }
  }
  
  finalPollution <- finalPollution / (endYear-startYear-3) / 5
  # print(finalCounties)
  print("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
  # print(finalPollution)
  return(list(finalCounties, finalPollution))
  
}
