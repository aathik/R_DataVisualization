#################################################################################################
#Set the source file location
setwd("/Users/aathik/Documents/Uwin/ADT/Lab/Lab1")
getwd()

#Install Package
install.packages("dplyr", dependencies = TRUE)
install.packages("dbscan")

#Include Package
library(dplyr)
library("dbscan")

#1################################################################################################
# Q1. 
# Import csv dataset
vehicles <- read.csv("Vehicle.csv")

#Summary of the data inside the dataset
summary(vehicles)

#2################################################################################################
# Q2.
# Structure of the dataset
str(vehicles)

#Dimension of the dataset
dim(vehicles)

#3################################################################################################
# Q3.
#Column names of the dataset
colnames(vehicles)

#First 3 rows of the dataset
head(vehicles, 3)

#Last 6 rows of the dataset
tail(vehicles, 6)

#4###############################################################################################
# Q4.
#Average Kms_Driven for each type of car 
average_Kms <- vehicles %>% group_by(Car_Name) %>% summarise(mean_kms = mean(Kms_Driven)) %>% as.data.frame()
average_Kms

#5###############################################################################################
# Q5.
# Average Selling_Price of the cars in each year
average_SP <- vehicles %>% group_by(Year) %>% summarise(mean_sp = mean(Selling_Price)) %>% as.data.frame()
average_SP

#6###############################################################################################
# Q6.
# Unique combinations of Car_Name, Fuel_Type, Seller_Type, and Transmission in the dataset
unique_comb <- vehicles %>% select(Car_Name, Fuel_Type, Seller_Type, Transmission) %>% distinct()
unique_comb

#7###############################################################################################
# Q7.
# Different combinations of Car_Name, Fuel_Type, Seller_Type, and Transmission in the dataset.
comination <- vehicles %>% select(Car_Name, Fuel_Type, Seller_Type, Transmission) %>% as.data.frame()
comination

#How many times does it occur?
frequency_comb <- comination %>% group_by(Car_Name, Fuel_Type, Seller_Type, Transmission) %>% summarise(Frequency = n())
frequency_comb

#Ascending order
asc_order <- frequency_comb %>% arrange(Frequency) %>% as.data.frame()
asc_order

#Descending order
desc_order <- frequency_comb %>% arrange(desc(Frequency)) %>% as.data.frame()
desc_order

#8###############################################################################################
# Q8.
# Missing Values in Dataset
is.na(vehicles)

#9###############################################################################################
# Q9.
# Columns contain missing values and total missing values for each column
colSums(is.na(vehicles))

#10###############################################################################################
# Q10.
# Replace the missing values in the dataset with the most repeated value of that field
for(column in colnames(vehicles)){
  #print(col)
  max_rep_val <- names(which.max(table(vehicles[[column]])))
  #print(max_rep_val)
  vehicles[[column]][(is.na(vehicles[[column]]))] <- max_rep_val
}

# Check if the missing values were replaced successfully
if(any(colSums(is.na(vehicles))>0)){
  print("There are stil missing values in dataset")
}else {
  print("Replacement Successful")
}

#11###############################################################################################
# Q11.
# Duplicate rows
vehicles[duplicated(vehicles), ]

# Remove Duplicate rows
vehicles <- vehicles[!duplicated(vehicles), ]
vehicles[duplicated(vehicles), ]

#12###############################################################################################
# Q12.
# Replace the values of attributes
vehicles$Fuel_Type[vehicles$Fuel_Type == 'Petrol'] <- 0
vehicles$Fuel_Type[vehicles$Fuel_Type == 'Diesel'] <- 1
vehicles$Fuel_Type[vehicles$Fuel_Type == 'CNG'] <- 2
#head(vehicles)
print(unique(vehicles$Fuel_Type))

vehicles$Seller_Type[vehicles$Seller_Type == 'Dealer'] <- 0
vehicles$Seller_Type[vehicles$Seller_Type == 'Individual'] <- 1
#head(vehicles)
print(unique(vehicles$Seller_Type))

vehicles$Transmission[vehicles$Transmission == 'Manual'] <- 0
vehicles$Transmission[vehicles$Transmission == 'Automatic'] <- 1
#head(vehicles)
print(unique(vehicles$Transmission))

#13###############################################################################################
# Q13.
# Add a new field called ‘Age’, and input the values by using the field Year. Show the output 
year <- as.integer(format(Sys.Date(), '%Y'))
vehicles$Age <- year - as.integer(vehicles$Year)
head(vehicles)

#14###############################################################################################
# Q14.
# Create a new dataset by selecting only the columns “Car_name”, “Selling_Price”, “Present_Price”, and “Kms_Drive”.
# Show the output of the new dataset
vehicles_new <- vehicles[c("Car_Name", "Selling_Price", "Present_Price", "Kms_Driven")]
head(vehicles_new)

#15###############################################################################################
# Q15.
# Shuffle the rows of the Vehicle dataset randomly 
head(vehicles)
shuffled_vehicles <- vehicles[sample(nrow(vehicles)), ]
# Display shuffled_vehicles
head(shuffled_vehicles)

a#16###############################################################################################
# Q16.
# Create a scatter plot of the Selling_Price Vs Present_Price. Color code the points based on the Transmission
plot(vehicles$Present_Price, vehicles$Selling_Price, 
     col = ifelse(vehicles$Transmission == 0, "red", "blue"),
     main = "Scatter Plot: Selling_Price Vs Present_Price",
     xlab = "Selling_Price",
     ylab = "Present_Price",
     pch = 2)
legend("bottomright", legend = c("Manual", "Automatic"),
       col = c("red", "blue"), pch = 16, title = "Transmission")

#17###############################################################################################
# Q17.
#  Create a box plot of the Selling_Price Vs Transmission and Fuel_Type
boxplot(as.numeric(Selling_Price)~as.numeric(Transmission), data = vehicles,
        xlab = "Transmission",
        ylab = "Selling Price",
        main = "Box Plot: Selling_Price Vs Transmission")

#  Create a box plot of the Selling_Price Vs Transmission and Fuel_Type
boxplot(as.numeric(Selling_Price)~as.numeric(Fuel_Type), data = vehicles,
        xlab = "Transmission",
        ylab = "Fuel Type",
        main = "Box Plot: Selling_Price Vs Fuel_Type")

#18###############################################################################################
# Q18.
# Create a scatter plot of the Selling_Price Vs Kms_Driven, and use k-means clustering to cluster 
# the points into 4 clusters. Colour-code based on the cluster they belong to.
data_x <- vehicles[c("Selling_Price", "Kms_Driven")]
kmeans_result <- kmeans(data_x, 4)
plot(data_x$Kms_Driven, data_x$Selling_Price,
     col=kmeans_result$cluster,
     xlab = "Kms_Driven", ylab = "Selling_Price",
     main = "Scatter Plot: Selling Price Vs Kms_Driven (K-means Clustering)")
legend("topright", legend = paste("Cluster", 1:4),
       col = 1:4, pch = 16, title = "Cluster")

#19###############################################################################################
# Q19.
# Create a scatter plot of the Selling_Price Vs Present_Price, and use hierarchical clustering 
# to cluster the points into 3 clusters? Colour-code the points based on the cluster they belong to  
data_h <- vehicles[c("Selling_Price", "Present_Price")]
dist_matrix <-dist(data_h, method = 'euclidean')
hclust_result <- hclust(dist_matrix, method='average')
#hclust_result
plot(data_h$Present_Price, data_h$Selling_Price, 
     col = cutree(hclust_result, k = 3),
     pch = 16,
     xlab = "Present_Price", ylab = "Selling_Price",
     main = "Scatter Plot: Selling_Price vs Present_Price (Hierarchical CLustering)")
legend("bottomright", legend = paste("Cluster", 1:3),
       col = 1:3, pch = 16, title = "Cluster")

#20###############################################################################################
# Q20.
# Add a new field called ‘Age’, and calculate it using the field ‘Year’. Create a barplot.
year <- as.integer(format(Sys.Date(), '%Y'))
vehicles$Age <- year - as.integer(vehicles$Year)
head(vehicles)

barplot(table(vehicles$Age),
        main = "Barplot: Age",
        xlab = "Age", ylab = "Count",
        col = "blue")

barplot(table(vehicles$Year),
        main = "Barplot: Year",
        xlab = "Year", ylab = "Count",
        col = "red")

barplot(table(vehicles$Transmission),
        main = "Barplot: Transmission",
        xlab = "Transmission", ylab = "Count",
        col = "green")

barplot(table(vehicles$Seller_Type),
        main = "Barplot: Seller_Type",
        xlab = "Seller_Type", ylab = "Count",
        col = "yellow")

barplot(table(vehicles$Fuel_Type),
        main = "Barplot: Fuel_Type",
        xlab = "Fuel_Type", ylab = "Count",
        col = "brown")

barplot(table(vehicles$Owner),
        main = "Barplot: Owner",
        xlab = "Owner", ylab = "Count",
        col = "purple")

#21###############################################################################################
# Q21.
# Create a correlation plot of the whole dataset variables and explain the output. Do not forget to 
# convert some of the variable’s datatype if required and possible 
#head(vehicles)
#typeof(vehicles$Owner)
vehicles$Year <- as.numeric(vehicles$Year)
vehicles$Selling_Price <- as.numeric(vehicles$Selling_Price)
vehicles$Present_Price <- as.numeric(vehicles$Present_Price)
vehicles$Kms_Driven <- as.numeric(vehicles$Kms_Driven)
data <- vehicles[c("Year","Selling_Price", "Present_Price", "Kms_Driven")]
pairs(data)
plot(data, col = "blue", main = "Vehicles dataset")



#22###############################################################################################
# Q22.
# 
data_s <- vehicles[c("Selling_Price", "Kms_Driven")]
dbscan_result <- dbscan(data_s, eps = 200, minPts = 9)
dbscan_result
data_s$cluster <- dbscan_result$cluster
data_s$cluster
cluster_colors <- c("red", "blue", "green") 
plot(data_s$Kms_Driven, data_s$Selling_Price, 
     col = cluster_colors[data_s$cluster],
     pch = 16,
     xlab = "Kms_Driven", ylab = "Selling_Price",
     main = "Scatter Plot: Selling_Price vs Kms_Driven (DBSCAN)")
legend("topright", legend = paste("Cluster", 0:2),
       col = cluster_colors,
       pch = 16)


