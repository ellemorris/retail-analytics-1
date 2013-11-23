
data <- read.csv('/Users/Alex/Downloads/dunnhumby hack reduce product launch challenge/training set.csv')
#get each product type


#reshape our data into a "wide" format
#want 1 row per product
new_product <- unique(data$Product_Launch_Id)

#make initial dataframe
naive_data <- data.frame(Product = new_product)

#product categories
#note that transformation from a factor... otherwise screws up
class(data$Product_Category)
data$Product_Category <- as.character(data$Product_Category)
for (i in c(1:length(naive_data$Product))){
  naive_data$Product_Category[i] <- data$Product_Category[data$Product_Launch_Id == naive_data$Product[i] & data$Weeks_Since_Launch == 1]}

#transform everything else... 
#WARNINGS THIS TAKES A LONG TIME
for (k in c(4:18)){
  for (i in c(1:26)){
    x = (length(naive_data)+1)
    for (j in c(1:length(naive_data$Product))){
      naive_data[j,x] <- data[data$Product_Launch_Id == naive_data$Product[j] & data$Weeks_Since_Launch == i, k]
    }
    colnames(naive_data)[x]  <- paste(names(data)[k],(i), sep="")
  }
}
#save everything
save(naive_data, file="dh_reformatted.rData")







#old stuff below -----------------------
fit <- lm(Sales26 ~ Sales13 + Stores13 + Stores26 + DistCustCum13, data=naive_data[1:2000,])
linear_preds = predict(fit, newdata=naive_data[2001:2768,])
linear_preds[linear_preds < 0] = 0

fit2 <- randomForest(Sales26 ~ Sales13 + Stores13 + Stores26 + DistCustCum13, data=naive_data[1:2000,], ntree=500, mtry=3)
rf_preds <- predict(fit2, newdata=naive_data[2001:2768,])


linear_error <- sqrt(sum((log(linear_preds+1) - log(naive_data[2001:2768,]$Sales26+1))^2)/length(linear_preds))
rf_error <- sqrt(sum((log(rf_preds+1) - log(naive_data[2001:2768,]$Sales26+1))^2)/length(rf_preds))

for (i in c(1:length(segment_data$Product))){
segment_data[i,2:12] <- data[data$Product_Launch_Id == segment_data$Product[i] & data$Weeks_Since_Launch == 13,8:18]}
pca <- prcomp(segment_data[,2:12])

naive_data$segments1 <- pca$x[,1]
fit <- lm(Sales26 ~ Sales13 + Stores13 + Stores26 + DistCustCum13 + segments1 + segments2 + segments3 + segments4 + StoreRatio1326, data=naive_data[1:2000,])
linear_preds = predict(fit, newdata=naive_data[2001:2768,])
linear_preds[linear_preds < 0] = 0
linear_error <- sqrt(sum((log(linear_preds+1) - log(naive_data[2001:2768,]$Sales26+1))^2)/length(linear_preds))
print(linear_error)

fit2 <- randomForest(SalesPerStore26 ~ SalesPerStore13 + Sales13 + Stores13 + Stores26 + DistCustCum13 + segments1 + StoreRatio1326, data=naive_data[700:2768,], ntree=1000, mtry=5)
rf_preds <- predict(fit2, newdata=naive_data[1:699,])
rf_error <- sqrt(sum((log(rf_preds+1) - log(naive_data[1:699,]$SalesPerStore26+1))^2)/length(rf_preds))
print(rf_error)
