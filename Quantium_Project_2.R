# Load the required libraries
library(data.table)
library(ggplot2)
library(ggmosaic)
library(readr)
library(readxl)  # This library won't be needed for reading the csv file
library(scales)

# Define the file paths
path1 <- "/Users/JumpMan/Downloads/Fall 2023 Work/TheForge_Projects/Quantuim Project/QVI_data.csv"

# Read the files into R using read_csv
data <- fread(path1)


#### Calculate these measures over time for each store
#### Create a month ID
data[, YEARMONTH := year(DATE)*100 + month(DATE)]



# Assuming your data is already loaded into the 'data' data.table
# Convert DATE to a Date class if it's not already
data[, DATE := as.Date(DATE, format="%Y-%m-%d")]

# Create a 'YEARMONTH' column for monthly grouping
data[, YEARMONTH := year(DATE)*100 + month(DATE)]

# Define measures to calculate
measureOverTime <- data[, .(
  totSales = sum(TOT_SALES),
  nCustomers = uniqueN(LYLTY_CARD_NBR),
  nTxnPerCust = uniqueN(TXN_ID)/uniqueN(LYLTY_CARD_NBR),
  nChipsPerTxn = sum(PROD_QTY)/uniqueN(TXN_ID),
  avgPricePerUnit = sum(TOT_SALES)/sum(PROD_QTY)
),
by = .(STORE_NBR, YEARMONTH)
][order(STORE_NBR, YEARMONTH)]

# Filter the data for the pre-trial period and for stores with full 12 months of observations
storesWithFullObs <- unique(measureOverTime[, .N, by=.(STORE_NBR)][N == 12, STORE_NBR])
preTrialMeasures <- measureOverTime[YEARMONTH < 201902 & STORE_NBR %in% storesWithFullObs,]

calculateCorrelation <- function(inputTable, metricCol, storeComparison) {
  # Initialize an empty data.table
  calcCorrTable <- data.table(Store1 = numeric(), Store2 = numeric(), corr_measure = numeric())
  
  # Extract unique store numbers
  storeNumbers <- unique(inputTable$STORE_NBR)
  
  # Loop through each store and calculate the correlation
  for (i in storeNumbers) {
    # Filter the data for the two stores being compared
    store1_data <- inputTable[STORE_NBR == storeComparison, get(metricCol)]
    store2_data <- inputTable[STORE_NBR == i, get(metricCol)]
    
    # Calculate the correlation
    correlation_value <- cor(store1_data, store2_data)
    
    # Bind the calculated value to the results table
    calcCorrTable <- rbind(calcCorrTable, data.table(Store1 = storeComparison, Store2 = i, corr_measure = correlation_value))
  }
  
  return(calcCorrTable)
}

calculateMagnitudeDistance <- function(inputTable, metricCol, storeComparison) {
  
  calcDistTable <- data.table(Store1 = numeric(), Store2 = numeric(), YEARMONTH = numeric(), measure = numeric())
  
  storeNumbers <- unique(inputTable$STORE_NBR)
  
  for (i in storeNumbers) {
    calculatedMeasure <- data.table(
      Store1 = storeComparison,
      Store2 = i,
      YEARMONTH = inputTable[STORE_NBR == storeComparison, YEARMONTH],
      measure = abs(inputTable[STORE_NBR == storeComparison, get(metricCol)] - inputTable[STORE_NBR == i, get(metricCol)])
    )
    calcDistTable <- rbind(calcDistTable, calculatedMeasure)
  }
  
  # Standardise the magnitude distance
  minMaxDist <- calcDistTable[, .(minDist = min(measure), maxDist = max(measure)), by = .(Store1, YEARMONTH)]
  distTable <- merge(calcDistTable, minMaxDist, by = c("Store1", "YEARMONTH"))
  distTable[, magnitudeMeasure := 1 - (measure - minDist) / (maxDist - minDist)]
  finalDistTable <- distTable[, .(mag_measure = mean(magnitudeMeasure)), by = .(Store1, Store2)]
  
  return(finalDistTable)
}

#### Use the functions for calculating correlation
trial_store <- 77
corr_nSales <- calculateCorrelation(preTrialMeasures, quote(totSales),
                                    trial_store)
corr_nCustomers <- calculateCorrelation(preTrialMeasures, quote(nCustomers),
                                         trial_store)
#### Use the functions for calculating magnitude
magnitude_nSales <- calculateMagnitudeDistance(preTrialMeasures,
                                               quote(totSales), trial_store)
magnitude_nCustomers <- calculateMagnitudeDistance(preTrialMeasures,
                                                    quote(nCustomers), trial_store)

#### Create a combined score composed of correlation and magnitude
corr_weight <- 0.5
score_nSales <- merge(corr_nSales, magnitude_nSales, by = c("Store1",
                                                            "Store2"))[, scoreNSales := corr_measure * corr_weight + mag_measure * 
                                                            (1-corr_weight)]

score_nCustomers <- merge(corr_nCustomers, magnitude_nCustomers, by =
                            c("Store1", "Store2"))[, scoreNCust := corr_measure * corr_weight +
                                                     mag_measure * (1 - corr_weight)]

#### Combine scores across the drivers
score_Control <- merge(score_nSales, score_nCustomers, by = c("Store1",
                                                               "Store2"))
score_Control[, finalControlScore := scoreNSales * 0.5 + scoreNCust * 0.5]

#### Select control stores based on the highest matching store (closest to 1 but

#### not the store itself, i.e. the second ranked highest store)
#### Select control store for trial store 77
control_store <- score_Control[Store1 == trial_store,
                                ][order(-finalControlScore)][2, Store2]
control_store

#### Visual checks on trends based on the drivers
measureOverTimeSales <- measureOverTime
pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR ==
                                                            trial_store, "Trial",
                                                         ifelse(STORE_NBR == control_store,
                                                                "Control", "Other stores"))
][, totSales := mean(totSales), by = c("YEARMONTH",
                                        "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/%
                                         100, YEARMONTH %% 100, 1, sep = "‐"), "%Y‐%m‐%d")
][YEARMONTH < 201903 , ]
ggplot(pastSales, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_line() +
  labs(x = "Month of operation", y = "Total sales", title = "Total sales by
 month")

#### Visual checks on trends based on the drivers
measureOverTimeCusts <- measureOverTime
pastCustomers <- measureOverTimeCusts[, Store_type := ifelse(STORE_NBR ==
                                                               trial_store, "Trial",
                                                             ifelse(STORE_NBR == control_store,
                                                                     "Control", "Other stores"))
][, numberCustomers := mean(nCustomers), by =
     c("YEARMONTH", "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/%
                                         100, YEARMONTH %% 100, 1, sep = "‐"), "%Y‐%m‐%d")
][YEARMONTH < 201903 , ]
ggplot(pastCustomers, aes(TransactionMonth, numberCustomers, 
    color =Store_type)) + geom_line() + labs(x = "Month of operation",
    y = "Total number of customers" , title = "Total number of customers by month")
    

# Scale pre-trial control sales to match pre-trial trial store sales
scalingFactorForControlSales <- preTrialMeasures[STORE_NBR == trial_store & YEARMONTH < 201902, sum(totSales)] / preTrialMeasures[STORE_NBR == control_store & YEARMONTH < 201902, sum(totSales)]

# Apply the scaling factor
measureOverTimeSales <- measureOverTime
scaledControlSales <- measureOverTimeSales[STORE_NBR == control_store, ][, controlSales := totSales * scalingFactorForControlSales]

# Calculate the percentage difference between scaled control sales and trial sales
percentageDiff <- merge(scaledControlSales[, .(YEARMONTH, controlSales)], measureOverTime[STORE_NBR == trial_store, .(totSales, YEARMONTH)], by = "YEARMONTH")[, percentageDiff := abs(controlSales - totSales) / controlSales]


# Calculate standard deviation based on the scaled percentage difference in the pre-trial period
stdDev <- sd(percentageDiff[YEARMONTH < 201902, percentageDiff])

# Degrees of freedom for the pre-trial period
degreesOfFreedom <- 7

# Calculate t-values and add transaction month
percentageDiff[, tValue := (percentageDiff - 0) / stdDev]
percentageDiff[, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")]
result <- percentageDiff[YEARMONTH < 201905 & YEARMONTH > 201901, .(TransactionMonth, tValue)]

#### Find the 95th percentile of the t distribution with the appropriate
#### degrees of freedom to compare against
qt(0.95, df = degreesOfFreedom)
    
measureOverTimeSales <- measureOverTime

# Define Trial, Control, and Other stores
pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                                                         ifelse(STORE_NBR == control_store, "Control", "Other stores"))]
pastSales[, totSales := mean(totSales), by = c("YEARMONTH", "Store_type")]
pastSales[, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")]
pastSales <- pastSales[Store_type %in% c("Trial", "Control")]

# Control store 95th percentile
pastSales_Controls95 <- pastSales[Store_type == "Control", ][, totSales := totSales * (1 + stdDev * 2)]
pastSales_Controls95[, Store_type := "Control 95th % confidence interval"]

# Control store 5th percentile
pastSales_Controls5 <- pastSales[Store_type == "Control", ][, totSales := totSales * (1 - stdDev * 2)]
pastSales_Controls5[, Store_type := "Control 5th % confidence interval"]

trialAssessment <- rbind(pastSales, pastSales_Controls95, pastSales_Controls5)

# Plot
ggplot(trialAssessment, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_rect(data = trialAssessment[YEARMONTH < 201905 & YEARMONTH > 201901, ],
            aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), ymin = 0, ymax = Inf, color = NULL), 
            show.legend = FALSE) +
  geom_line() +
  labs(x = "Month of operation", y = "Total sales", title = "Total sales by month")





# Scale pre-trial control customers to match pre-trial trial store customers
scalingFactorForControlCust <- preTrialMeasures[STORE_NBR == trial_store & YEARMONTH < 201902, sum(nCustomers)] / 
  preTrialMeasures[STORE_NBR == control_store & YEARMONTH < 201902, sum(nCustomers)]

# Apply the scaling factor
measureOverTimeCusts <- measureOverTime
scaledControlCustomers <- measureOverTimeCusts[STORE_NBR == control_store, ]
scaledControlCustomers[, controlCustomers := nCustomers * scalingFactorForControlCust]
scaledControlCustomers[, Store_type := ifelse(STORE_NBR == trial_store, "Trial", 
                                              ifelse(STORE_NBR == control_store, "Control", "Other stores"))]

# Merge scaledControlCustomers and measureOverTimeCusts based on YEARMONTH
percentageDiff <- merge(scaledControlCustomers[, .("YEARMONTH", "controlCustomers")], 
                        measureOverTimeCusts[STORE_NBR == trial_store, .("nCustomers", "YEARMONTH")], 
                        by = "YEARMONTH", all.x = TRUE)

# Calculate the percentage difference between scaled control sales and trial sales
percentageDiff[, percentageDiff := abs(controlCustomers - nCustomers) / controlCustomers]

# Standard deviation
stdDev <- sd(percentageDiff[YEARMONTH < 201902 , percentageDiff])
degreesOfFreedom <- 7

# Trial and control store number of customers
pastCustomers <- measureOverTimeCusts[, .(nCusts = mean(nCustomers)), by = .("YEARMONTH", "Store_type")]
pastCustomers <- pastCustomers[Store_type %in% c("Trial", "Control")]

# Control store 95th percentile
pastCustomers_Controls95 <- copy(pastCustomers[Store_type == "Control",])
pastCustomers_Controls95[, nCusts := nCusts * (1 + stdDev * 2)]
pastCustomers_Controls95[, Store_type := "Control 95th % confidence interval"]

# Control store 5th percentile
pastCustomers_Controls5 <- copy(pastCustomers[Store_type == "Control",])
pastCustomers_Controls5[, nCusts := nCusts * (1 - stdDev * 2)]
pastCustomers_Controls5[, Store_type := "Control 5th % confidence interval"]

# Combine data for plotting
trialAssessment <- rbind(pastCustomers, pastCustomers_Controls95, pastCustomers_Controls5)

# Plot
ggplot(trialAssessment, aes(TransactionMonth, nCusts, color = Store_type)) +
  geom_rect(data = trialAssessment[YEARMONTH < 201905 & YEARMONTH > 201901, ],
            aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), ymin = 0, ymax = Inf, color = NULL), 
            show.legend = FALSE) +
  geom_line() +
  labs(x = "Month of operation", y = "Total number of customers", title = "Total number of customers by month")


#
measureOverTime <- data[, .(
totSales = sum(TOT_SALES),
nCustomers = uniqueN(LYLTY_CARD_NBR),
nTxnPerCust = uniqueN(TXN_ID) / uniqueN(LYLTY_CARD_NBR),
nChipsPerTxn = sum(PROD_QTY) / uniqueN(TXN_ID),
avgPricePerUnit = sum(TOT_SALES) / sum(PROD_QTY)
), by = c("STORE_NBR", "YEARMONTH")]

trial_store <- 86

corr_nSales <- calculateCorrelation(preTrialMeasures, quote(totSales), trial_store)
corr_nCustomers <- calculateCorrelation(preTrialMeasures, quote(nCustomers), trial_store)

magnitude_nSales <- calculateMagnitudeDistance(preTrialMeasures, quote(totSales), trial_store)
magnitude_nCustomers <- calculateMagnitudeDistance(preTrialMeasures, quote(nCustomers), trial_store)

corr_weight <- 0.5

score_nSales <- merge(corr_nSales, magnitude_nSales, by = c("Store1", "Store2"))[, 
                                                                                 scoreNSales := corr_measure * corr_weight + mag_measure * (1 - corr_weight)]

score_nCustomers <- merge(corr_nCustomers, magnitude_nCustomers, by = c("Store1", "Store2"))[, 
                                                                                             scoreNCust := corr_measure * corr_weight + mag_measure * (1 - corr_weight)]

score_Control <- merge(score_nSales, score_nCustomers, by = c("Store1", "Store2"))
score_Control[, finalControlScore := scoreNSales * 0.5 + scoreNCust * 0.5]

control_store <- score_Control[Store1 == trial_store,][order(-finalControlScore)][2, Store2]
control_store

measureOverTimeSales <- measureOverTime

pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                                                         ifelse(STORE_NBR == control_store, "Control", "Other stores"))][
                                                           , totSales := mean(totSales), by = c("YEARMONTH", "Store_type")
                                                         ][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
                                                         ][YEARMONTH < 201903 , ]

ggplot(pastSales, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_line(aes(linetype = Store_type)) +
  labs(x = "Month of operation", y = "Total sales", title = "Total sales by month")

measureOverTimeCusts <- measureOverTime

pastCustomers <- measureOverTimeCusts[, Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                                                             ifelse(STORE_NBR == control_store, "Control", "Other stores"))][
                                                               , numberCustomers := mean(nCustomers), by = c("YEARMONTH", "Store_type")
                                                             ][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
                                                             ][YEARMONTH < 201903 , ]

ggplot(pastCustomers, aes(TransactionMonth, numberCustomers, color = Store_type)) +
  geom_line() +
  labs(x = "Month of operation", y = "Total number of customers", title = "Total number of customers by month")


scalingFactorForControlSales <- preTrialMeasures[STORE_NBR == trial_store & YEARMONTH < 201902, sum(totSales)] / preTrialMeasures[STORE_NBR == control_store & YEARMONTH < 201902, sum(totSales)]

measureOverTimeSales <- measureOverTime
scaledControlSales <- measureOverTimeSales[STORE_NBR == control_store, ][, controlSales := totSales * scalingFactorForControlSales]

percentageDiff <- merge(scaledControlSales[, c("YEARMONTH", "controlSales")], measureOverTime[STORE_NBR == trial_store, c("totSales", "YEARMONTH")], by = "YEARMONTH")[, percentageDiff := abs(controlSales-totSales)/controlSales]

stdDev <- sd(percentageDiff[YEARMONTH < 201902, percentageDiff])
degreesOfFreedom <- 7

measureOverTimeSales <- measureOverTime
pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store, "Trial", ifelse(STORE_NBR == control_store, "Control", "Other stores"))][, totSales := mean(totSales), by = c("YEARMONTH", "Store_type")][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")][Store_type %in% c("Trial", "Control")]

pastSales_Controls95 <- pastSales[Store_type == "Control", ][, totSales := totSales * (1 + stdDev * 2)][, Store_type := "Control 95th % confidence interval"]

pastSales_Controls5 <- pastSales[Store_type == "Control", ][, totSales := totSales * (1 - stdDev * 2)][, Store_type := "Control 5th % confidence interval"]

trialAssessment <- rbind(pastSales, pastSales_Controls95, pastSales_Controls5)

ggplot(trialAssessment, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_rect(data = trialAssessment[YEARMONTH < 201905 & YEARMONTH > 201901, ], aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), ymin = 0, ymax = Inf, color = NULL), show.legend = FALSE) +
  geom_line(aes(linetype = Store_type)) +
  labs(x = "Month of operation", y = "Total sales", title = "Total sales by month")


scalingFactorForControlCust <- preTrialMeasures[STORE_NBR == trial_store & YEARMONTH < 201902, sum(nCustomers)]/preTrialMeasures[STORE_NBR == control_store & YEARMONTH < 201902, sum(nCustomers)]

measureOverTimeCusts <- measureOverTime
scaledControlCustomers <- measureOverTimeCusts[STORE_NBR == control_store, ][, controlCustomers := nCustomers * scalingFactorForControlCust][, Store_type := ifelse(STORE_NBR == trial_store, "Trial", ifelse(STORE_NBR == control_store, "Control", "Other stores"))]

percentageDiff <- merge(scaledControlCustomers[, c("YEARMONTH", "controlCustomers")], measureOverTime[STORE_NBR == trial_store, c("nCustomers", "YEARMONTH")], by = "YEARMONTH")[, percentageDiff := abs(controlCustomers-nCustomers)/controlCustomers]

stdDev <- sd(percentageDiff[YEARMONTH < 201902, percentageDiff])
degreesOfFreedom <- 7

pastCustomers <- measureOverTimeCusts[, nCusts := mean(nCustomers), by = c("YEARMONTH", "Store_type")][Store_type %in% c("Trial", "Control")]

pastCustomers_Controls95 <- pastCustomers[Store_type == "Control", ][, nCusts := nCusts * (1 + stdDev * 2)][, Store_type := "Control 95th % confidence interval"]

pastCustomers_Controls5 <- pastCustomers[Store_type == "Control", ][, nCusts := nCusts * (1 - stdDev * 2)][, Store_type := "Control 5th % confidence interval"]

trialAssessment <- rbind(pastCustomers, pastCustomers_Controls95, pastCustomers_Controls5)

ggplot(trialAssessment, aes(TransactionMonth, nCusts, color = Store_type)) +
  geom_rect(data = trialAssessment[YEARMONTH < 201905 & YEARMONTH > 201901, ], aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), ymin = 0, ymax = Inf, color = NULL), show.legend = FALSE) +
  geom_line() +
  labs(x = "Month of operation", y = "Total number of customers", title = "Total number of customers by month")

measureOverTime <- data[, .(
  totSales = sum(TOT_SALES),
  nCustomers = uniqueN(LYLTY_CARD_NBR),
  nTxnPerCust = uniqueN(TXN_ID)/uniqueN(LYLTY_CARD_NBR),
  nChipsPerTxn = sum(PROD_QTY)/uniqueN(TXN_ID),
  avgPricePerUnit = sum(TOT_SALES)/sum(PROD_QTY)
), by = c("STORE_NBR", "YEARMONTH")][order(STORE_NBR, YEARMONTH)]

trial_store <- 88
corr_nSales <- calculateCorrelation(preTrialMeasures, quote(totSales), trial_store)
corr_nCustomers <- calculateCorrelation(preTrialMeasures, quote(nCustomers), trial_store)
magnitude_nSales <- calculateMagnitudeDistance(preTrialMeasures, quote(totSales), trial_store)
magnitude_nCustomers <- calculateMagnitudeDistance(preTrialMeasures, quote(nCustomers), trial_store)
corr_weight <- 0.5
score_nSales <- merge(corr_nSales, magnitude_nSales, by = c("Store1", "Store2"))[, scoreNSales := corr_measure * corr_weight + mag_measure * (1 - corr_weight)]
score_nCustomers <- merge(corr_nCustomers, magnitude_nCustomers, by = c("Store1", "Store2"))[, scoreNCust := corr_measure * corr_weight + mag_measure * (1 - corr_weight)]
score_Control <- merge(score_nSales, score_nCustomers, by = c("Store1", "Store2"))
score_Control[, finalControlScore := scoreNSales * 0.5 + scoreNCust * 0.5]
control_store <- score_Control[Store1 == trial_store, ][order(-finalControlScore)][2, Store2]

measureOverTimeSales <- measureOverTime
pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store, "Trial", ifelse(STORE_NBR == control_store, "Control", "Other stores"))][, totSales := mean(totSales), by = c("YEARMONTH", "Store_type")][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")][YEARMONTH < 201903 , ]
ggplot(pastSales, aes(TransactionMonth, totSales, color = Store_type)) + geom_line(aes(linetype = Store_type)) + labs(x = "Month of operation", y = "Total sales", title = "Total sales by month")

measureOverTimeCusts <- measureOverTime
pastCustomers <- measureOverTimeCusts[, Store_type := ifelse(STORE_NBR == trial_store, "Trial", ifelse(STORE_NBR == control_store, "Control", "Other stores"))][, numberCustomers := mean(nCustomers), by = c("YEARMONTH", "Store_type")][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")][YEARMONTH < 201903 , ]
ggplot(pastCustomers, aes(TransactionMonth, numberCustomers, color = Store_type)) + geom_line() + labs(x = "Month of operation", y = "Total number of customers", title = "Total number of customers by month")

scalingFactorForControlSales <- preTrialMeasures[STORE_NBR == trial_store & YEARMONTH < 201902, sum(totSales)]/preTrialMeasures[STORE_NBR == control_store & YEARMONTH < 201902, sum(totSales)]
measureOverTimeSales <- measureOverTime
scaledControlSales <- measureOverTimeSales[STORE_NBR == control_store, ][ , controlSales := totSales * scalingFactorForControlSales]
percentageDiff <- merge(scaledControlSales[, c("YEARMONTH", "controlSales")], measureOverTime[STORE_NBR == trial_store, c("totSales", "YEARMONTH")], by = "YEARMONTH")[, percentageDiff := abs(controlSales-totSales)/controlSales]
stdDev <- sd(percentageDiff[YEARMONTH < 201902 , percentageDiff])
degreesOfFreedom <- 7
measureOverTimeSales <- measureOverTime
pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store, "Trial", ifelse(STORE_NBR == control_store, "Control", "Other stores"))][, totSales := mean(totSales), by = c("YEARMONTH", "Store_type")][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")][Store_type %in% c("Trial", "Control"), ]
pastSales_Controls95 <- pastSales[Store_type == "Control", ][, totSales := totSales * (1 + stdDev * 2)][, Store_type := "Control 95th % confidence interval"]
pastSales_Controls5 <- pastSales[Store_type == "Control", ][, totSales := totSales * (1 - stdDev * 2)][, Store_type := "Control 5th % confidence interval"]
trialAssessment <- rbind(pastSales, pastSales_Controls95, pastSales_Controls5)
ggplot(trialAssessment, aes(TransactionMonth, totSales, color = Store_type)) + geom_rect(data = trialAssessment[ YEARMONTH < 201905 & YEARMONTH > 201901 ,], aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), ymin = 0 , ymax = Inf, color = NULL), show.legend = FALSE) + geom_line() + labs(x = "Month of operation", y = "Total sales", title = "Total sales by month")

scalingFactorForControlCust <- preTrialMeasures[STORE_NBR == trial_store & YEARMONTH < 201902, sum(nCustomers)]/preTrialMeasures[STORE_NBR == control_store & YEARMONTH < 201902, sum(nCustomers)]
measureOverTimeCusts <- measureOverTime
scaledControlCustomers <- measureOverTimeCusts[STORE_NBR == control_store, ][ , controlCustomers := nCustomers * scalingFactorForControlCust ][, Store_type := ifelse(STORE_NBR == trial_store, "Trial", ifelse(STORE_NBR == control_store, "Control", "Other stores"))]

percentageDiff <- merge(scaledControlCustomers[, c("YEARMONTH", "controlCustomers")], measureOverTime[STORE_NBR == trial_store, c("nCustomers", "YEARMONTH")], by = "YEARMONTH")[, percentageDiff := abs(controlCustomers-nCustomers)/controlCustomers]

stdDev <- sd(percentageDiff[YEARMONTH < 201902 , percentageDiff])
degreesOfFreedom <- 7

pastCustomers <- measureOverTimeCusts[, nCusts := mean(nCustomers), by = c("YEARMONTH", "Store_type")][Store_type %in% c("Trial", "Control"), ]

pastCustomers_Controls95 <- pastCustomers[Store_type == "Control", ][, nCusts := nCusts * (1 + stdDev * 2)][, Store_type := "Control 95th % confidence interval"]

pastCustomers_Controls5 <- pastCustomers[Store_type == "Control", ][, nCusts := nCusts * (1 - stdDev * 2)][, Store_type := "Control 5th % confidence interval"]

trialAssessment <- rbind(pastCustomers, pastCustomers_Controls95, pastCustomers_Controls5)

ggplot(trialAssessment, aes(TransactionMonth, nCusts, color = Store_type)) +
  geom_rect(data = trialAssessment[ YEARMONTH < 201905 & YEARMONTH > 201901 ,], aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), ymin = 0, ymax = Inf, color = NULL), show.legend = FALSE) +
  geom_line() +
  labs(x = "Month of operation", y = "Total number of customers", title = "Total number of customers by month")




