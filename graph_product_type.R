#load data
#change the directory
data <- read.csv('/Users/Alex/Downloads/dunnhumby hack reduce product launch challenge/training set.csv')

#aggregate unique product types
products_types <- unique(data$Product_Category)
master_list <- list()

#create list of lists
for (i in c(1:length(products_types))){
  type = unique(data$Product_Category)[i]
  #print(type)
  data_for_type <- data[data$Product_Category==type,]
  product_ids <- unique(data_for_type$Product_Launch_Id)
  products <- list()
  for (j in c(1:length(product_ids))){
    products[[j]] <- data_for_type$Units_that_sold_that_week[data_for_type$Product_Launch_Id == product_ids[j]]
  }
  master_list[[as.character(type[1])]] = products
}

#plotting function
plot_all <- function(product){
  plot(product[[1]], type="l")
  if (length(product) > 1){
    for (i in c(2:length(product))){
      lines(c(1:26), product[[i]])
    }
  } 
}

#use example
plot_all(master_list[["Dvds"]])
