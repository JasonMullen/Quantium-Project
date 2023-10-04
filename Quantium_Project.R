# Install the packages (only if not previously installed)
# install.packages("data.table")
# install.packages("ggmosaic")
# install.packages("readxl")

# Load the required libraries
library(data.table)
library(ggplot2)
library(ggmosaic)
library(readr)
library(readxl)  # Added this library
library(scales)  # Ensure the scales library is loaded

# Define the file paths
path1 <- "/Users/JumpMan/Downloads/Fall 2023 Work/TheForge_Projects/Quantuim Project/QVI_transaction_data.xlsx"
path2 <- "/Users/JumpMan/Downloads/Fall 2023 Work/TheForge_Projects/Quantuim Project/QVI_purchase_behaviour.csv"

# Read the files into R
transaction_data <- read_excel(path1)
purchase_behaviour <- read.csv(path2)

# Convert the 'transaction_data' to a data.table object
transaction_data <- as.data.table(transaction_data)

# Examine the structure of the transaction_data
str(transaction_data)
names(transaction_data)

# Convert data column to a date format
# Ensure that the date column name you are using is consistent with the one in your dataset
transaction_data$DATE <- as.Date(transaction_data$DATE, origin = "1899-12-30")

# Examine the PROD_NAME column and count the occurrences
# Ensure 'PROD_NAME' exists in your dataset; otherwise, adjust accordingly.
result <- transaction_data[, .N, by = PROD_NAME]
print(result)


#Examine the words in Prod_name to see if there are any incorrect entries
#Such as products that are not chips 

productWords <- data.table(unlist(strsplit(unique(transaction_data[, PROD_NAME]), " ")))
setnames(productWords, 'words')
#Removing digits
productWords <- productWords[grepl("\\d",  words) == FALSE,]
#Removing special characters 
productWords <- productWords[grepl("[:alpha:]", words)]
#Let's look at the most common words by counting the number of times a word appears 
#And sorting them by this frequency in order of highest to lowest frequency
productWords[, .N, words][order(N, decreasing = TRUE)]

#Summarise the data to check for nulls and possible outliers
summary(transaction_data)
#Filter the dataset to find the outlier
transaction_data[PROD_QTY == 200]

#Let's see if the customer has had other transactions
transaction_data[LYLTY_CARD_NBR == 226000, ]

#Filter out the customer based on the loyalty card number 
transaction_data <- transaction_data[LYLTY_CARD_NBR != 2260000]
#Re-examine transaction data
summary(transaction_data)

#Count the number of transactions by date
transaction_data[, .N, by = DATE]

#Create a sequence of dates and join this the count of transactions by date
allDates <- data.table(seq(as.Date("2018/07/01"), as.Date("2019/06/30"), by = "day"))
setnames(allDates, "DATE")
transaction_by_day <- merge(allDates, transaction_data[, .N, by = DATE], all.x = TRUE)
#Setting plot themes to format graphs 
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))

#plot transactions over time 
ggplot(transaction_by_day, aes(x= DATE, y = N)) + 
  geom_line() +
  labs(x ="Day", y = "Number of transactions", title  = "Transactions over time") +
  scale_x_date(breaks = date_breaks("1 month")) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#Filter to December and look at individual days 
ggplot(transaction_by_day[month(DATE) == 12, ], aes(x = DATE, y = N)) +
  geom_line() + 
  labs(x = "Day", y = "Number of transportations", title = " Transactions over time") + 
  scale_x_date(breaks = "1 day") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


#pack size
#We can work this out by taking the digits that are in PROD_NAME
transaction_data[, PACK_SIZE := parse_number(PROD_NAME)]

#Always check your output 
#Let's check if the pack sizes look sensible 
transaction_data[, .N, PACK_SIZE][order(PACK_SIZE)]

#### Let's check the output of the first few rows to see if we have indeed
#↪ picked out pack size.
transaction_data

#Let's plot a histogram of PACK_SIZE. since we know that it is a categorical 
#variable and not a continous variable even though it is numeric. 
hist(transaction_data[,  PACK_SIZE])

#Brands
transaction_data[, BRAND := toupper(substr(PROD_NAME, 1, regexpr(pattern = ' ', 

                                                                                                                                  PROD_NAME)-1))]
#Checking Brands
transaction_data[, .N, by = BRAND][order(-N)]

#Clean brand names
transaction_data[BRAND == "RED", BRAND := "RRD"]
transaction_data[BRAND == "SNBTS", BRAND := "SUNBITES"]
transaction_data[BRAND == "INFZNS", BRAND := "INFUZIONS"]
transaction_data[BRAND == "WW", BRAND := "WOOLWORTHS"]
transaction_data[BRAND == "SMITH", BRAND := "SMITHS"]
transaction_data[BRAND == "NCC", BRAND := "NATURAL"]
transaction_data[BRAND == "DORITO", BRAND := "DORITOS"]
transaction_data[BRAND == "GRAIN", BRAND := "GRNWVES"]

#Check again
transaction_data[, .N, by = BRAND][order(BRAND)]


#CUSTOMER DATASET
str(purchase_behaviour)
summary(purchase_behaviour)
#Examining the values of lifestage and premium customer
purchase_behaviour <- as.data.table(purchase_behaviour)
purchase_behaviour[, .N, by = LIFESTAGE][order(-N)]
purchase_behaviour[, .N, by = PREMIUM_CUSTOMER][order(-N)]
#Merge transaction data to customer data
data <- merge(transaction_data, purchase_behaviour, all.x = TRUE) 

#checking of customers were not matched by checking nulls 
data[is.null(LIFESTAGE), .N]
data[is.null(PREMIUM_CUSTOMER), .N]

#fwrite(data, paste0(filepath, "QVI_data.csv"))

#DATA ANALYSIS ON CUSTOMER SEGMENTS
#Who spends the most on chips (total sales), describing customers by lifestage and how premium their
#general purchasing behaviour is
#• How many customers are in each segment
#• How many chips are bought per customer by segment
#• What’s the average chip price by customer segment
dev.off()

#Total sales by LIFESTAGE and PREMIUM_CUSTOMER
sales <- data[, .(SALES = sum(TOT_SALES)), .(LIFESTAGE, PREMIUM_CUSTOMER)]

p <- ggplot(data = sales) + 
  geom_mosaic(aes(weight = SALES, x = product(PREMIUM_CUSTOMER, LIFESTAGE),
                  fill = PREMIUM_CUSTOMER)) +
  labs(x = "Lifestage", y = "Premium customer flag", title = "Proportion of sales" ) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
p + geom_text(data = ggplot_build(p)$data[[1]], 
                   aes(x = (xmin + xmax)/2, y = (ymin + ymax) / 2, 
                       label = as.character(paste(round(.wt/sum(.wt),3)*100, "%"))))

#Number of customers by LIFESTAGE and PREMIUM_CUSTOMER
customers <- data[,.(CUSTOMERS = uniqueN(LYLTY_CARD_NBR)), .(LIFESTAGE,
                                                PREMIUM_CUSTOMER)][order(-CUSTOMERS)]
p <- ggplot(data = customers) + 
  geom_mosaic(aes(weight = CUSTOMERS, x = product(PREMIUM_CUSTOMER, LIFESTAGE), fill = PREMIUM_CUSTOMER)) +
  labs(x = "Lifestage", y = "Premium customer flag", title = "Portion of Customers") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

p + geom_text(data = ggplot_build(p)$data[[1]], 
                   aes(x = (xmin + xmax)/2, y = (ymin + ymax)/2, 
                       label = as.character(paste(round(.wt/sum(.wt),3)*100, "%"))))

# Average number of units per customer by LIFESTAGE and PREMIUM_CUSTOMER
avg_units <- data[, .(AVG = sum(PROD_QTY)/uniqueN(LYLTY_CARD_NBR)), 
                  by = .(LIFESTAGE, PREMIUM_CUSTOMER)][order(-AVG)]

# Create plot
p <- ggplot(data = avg_units, aes(x = LIFESTAGE, y = AVG, fill = PREMIUM_CUSTOMER)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Lifestage", y = "Avg units per transaction", title = "Units per customer") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# To visualize the plot, you can simply print 'p'
print(p)

# Average price per unit by LIFESTAGE and PREMIUM_CUSTOMER
avg_price <- data[, .(AVG = sum(TOT_SALES)/sum(PROD_QTY)), 
                  by = .(LIFESTAGE, PREMIUM_CUSTOMER)][order(-AVG)]

# Create plot
p <- ggplot(data = avg_price, aes(x = LIFESTAGE, y = AVG, fill = PREMIUM_CUSTOMER)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Lifestage", y = "Avg price per unit", title = "Price per unit") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# To visualize the plot, you can simply print 'p'
print(p)

# Calculate price per unit for each transaction
data[, price := TOT_SALES/PROD_QTY]

# Perform t-test 
result <- t.test(
  data[LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES") & 
         PREMIUM_CUSTOMER == "Mainstream", price],
  
  data[LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES") & 
         PREMIUM_CUSTOMER != "Mainstream", price],
  
  alternative = "greater"
)

# To see the results of the t-test, print the result
print(result)

# Segmentation
segment1 <- data[LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER == "Mainstream", ]
other <- data[!(LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER == "Mainstream"), ]

# Total quantities for each segment
quantity_segment1 <- segment1[, sum(PROD_QTY)]
quantity_other <- other[, sum(PROD_QTY)]

# Calculate proportion of each brand's purchases within segment1
quantity_segment1_by_brand <- segment1[, .(targetSegment = sum(PROD_QTY)/quantity_segment1), by = BRAND]

# Calculate proportion of each brand's purchases within the other segment
quantity_other_by_brand <- other[, .(other = sum(PROD_QTY)/quantity_other), by = BRAND]

# Merge the two datasets on BRAND and calculate the brand affinity scores
brand_proportions <- merge(quantity_segment1_by_brand, quantity_other_by_brand, by = "BRAND")[, 
                                                                                              affinityToBrand := targetSegment/other]

# Order the data by affinity scores in descending order
brand_proportions_sorted <- brand_proportions[order(-affinityToBrand)]

print(brand_proportions_sorted)

# Assuming your data has a 'PACK_SIZE' column representing the product pack size

# Calculate proportion of each pack size's purchases within segment1
quantity_segment1_by_pack <- segment1[, .(targetSegment = sum(PROD_QTY)/quantity_segment1), by = PACK_SIZE]

# Calculate proportion of each pack size's purchases within the other segment
quantity_other_by_pack <- other[, .(other = sum(PROD_QTY)/quantity_other), by = PACK_SIZE]

# Merge the two datasets on PACK_SIZE and calculate the pack affinity scores
pack_proportions <- merge(quantity_segment1_by_pack, quantity_other_by_pack, by = "PACK_SIZE")[, 
                                                                                               affinityToPack := targetSegment/other]

# Order the data by affinity scores in descending order
pack_proportions_sorted <- pack_proportions[order(-affinityToPack)]

print(pack_proportions_sorted)

data[PACK_SIZE == 270, unique(PROD_NAME)]


# Sales have mainly been due to Budget - older families, Mainstream - young singles/couples, and Mainstream - retirees shoppers. 
# We found that the high spend in chips for mainstream young singles/couples and retirees is due to there being more of them than other buyers. 
# Mainstream, midage and young singles and couples are also more likely to pay more per packet of chips. 
# This is indicative of impulse buying behaviour.
# We’ve also found that Mainstream young singles and couples are 23% more likely to purchase Tyrrells chips 
# compared to the rest of the population. 
# The Category Manager may want to increase the category’s performance by off-locating some Tyrrells and 
# smaller packs of chips in discretionary space near segments where young singles and couples frequent more often 
# to increase visibility and impulse behaviour.
# Quantium can help the Category Manager with recommendations of where these segments are and further help them with measuring the impact of the changed placement. 
# We’ll work on measuring the impact of trials in the next task and putting all these together in the third task
