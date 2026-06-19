
library(httr)
library(jsonlite)
library(stringr)
library(dplyr)


extract_specs <- function(name) {
  

  if (is.null(name) || name == "" || is.na(name)) {
    return(list(brand = NA, model = NA, ram = NA, storage = NA))
  }
  

  spec_match <- str_match(name, "(\\d+GB?)\\s*[/+]\\s*(\\d+(GB|TB))")
  
  if (!is.na(spec_match[1, 1])) {
    ram <- spec_match[1, 2]
    storage <- spec_match[1, 3]
    # Get the part before RAM/Storage as model
    model_part <- str_trim(substr(name, 1, regexpr(spec_match[1,1], name) - 1))
  } else {
    ram <- NA
    storage <- NA
    model_part <- name
  }
  
  # Split Brand and Model
  parts <- str_split(model_part, "\\s+", n = 2)[[1]]
  brand <- ifelse(length(parts) >= 1, parts[1], NA)
  model <- ifelse(length(parts) >= 2, parts[2], NA)
  
  return(list(brand = brand, model = model, ram = ram, storage = storage))
}


scrape_pickaboo_api <- function() {
  
  base_api_url <- "https://www.pickaboo.com/rest/V1/categorypageapi/smartphone"
  
  all_products <- data.frame(
    Brand = character(),
    model = character(),
    Ram = character(),
    storage = character(),
    price = numeric(),
    rating = numeric(),
    stringsAsFactors = FALSE
  )
  
  seen_ids <- c()
  
  headers <- add_headers(
    `User-Agent` = "Mozilla/5.0",
    Accept = "application/json"
  )
  
  # Loop through pages (adjust max pages as needed)
  for (page in 1:10) {
    cat("Scraping Page", page, "...\n")
    
    response <- GET(
      url = base_api_url,
      headers,
      query = list(
        prodLimit = 20,
        currentPage = page,
        featProdLimit = 6,
        web = 1
      )
    )
    
    
    
    data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    
    product_list <- data$cat_prods
    
   
    if (page == 1 && !is.null(data$featured_products)) {
      product_list <- bind_rows(product_list, data$featured_products)
    }
    
    if (length(product_list) == 0) next
    
  
    for (i in 1:nrow(product_list)) {
      p_id <- product_list$id[i]
      
      if (!is.na(p_id) && !(p_id %in% seen_ids)) {
        seen_ids <- c(seen_ids, p_id)
        
        name <- product_list$product_name[i]
        specs <- extract_specs(name)
        
        all_products <- rbind(all_products, data.frame(
          Brand = specs$brand,
          model = specs$model,
          Ram = specs$ram,
          storage = specs$storage,
          price = ifelse(is.null(product_list$product_price[i]), NA, product_list$product_price[i]),
          rating = ifelse(is.null(product_list$rating[i]), NA, product_list$rating[i]),
          stringsAsFactors = FALSE
        ))
      }
    }
    
    cat("Processed", nrow(product_list), "products on page", page, "\n\n")
    Sys.sleep(1)  
  }
  

  write.csv(all_products, "pickaboo_api_data.csv", row.names = FALSE)
  cat("Saved", nrow(all_products), "products to 'pickaboo_api_data.csv'\n")
  
 
  
  return(all_products)
}

products_df <- scrape_pickaboo_api()
--------------------------------------------------------------------------------
  #showing data set
print(products_df)
#########################
products_df <- products_df %>%
  mutate(
    storage = case_when(
      products_df$price >= 140000 & price <= 200000 ~ "256GB",
      products_df$price > 200000 ~ "512GB",
      TRUE ~ storage 
    )
  )
products_df <- products_df %>%
  mutate(
    Ram = case_when(
      products_df$price >= 140000 & price <= 200000 ~ "8GB",
      products_df$price > 200000 ~ "12GB",
      TRUE ~ Ram  
    )
  )
#################
#showing data set (EDA)
head(products_df) 
str(products_df)
summary(products_df)

#Checking for Missing Values 
colSums(is.na(products_df))

#Checking for Missing Values bar chart
missing_counts <- colSums(is.na(products_df))
barplot(missing_counts, 
        main = "Missing Values per Column", 
        xlab = "Columns", 
        ylab = "Count of Missing Values", 
        col = "tomato", 
        las = 2) 

#Handle missing model
products_df$model[is.na(products_df$model)] <- "Unknown"
colSums(is.na(products_df))

#Handle missing rating using mean
rating_mean <- mean(products_df$rating, na.rm = TRUE)
products_df$rating[is.na(products_df$rating)] <- rating_mean
products_df$rating <- round(products_df$rating, 1)
head(products_df)

# How many outliers per brand
library(dplyr)

df <- products_df %>%
  mutate(price = ifelse(price == 0, NA, price)) %>%
  filter(!is.na(price), !is.na(Brand))

# IQR method to detect outliers
Q1 <- quantile(df$price, 0.25)
Q3 <- quantile(df$price, 0.75)
IQR_value <- Q3 - Q1

lower <- Q1 - 1.5 * IQR_value
upper <- Q3 + 1.5 * IQR_value

# Filter outliers
df_outliers <- df %>%
  filter(price < lower | price > upper)

# Summarise by Brand – only Brand and max_price
outlier_summary <- df_outliers %>%
  group_by(Brand) %>%
  summarise(max_price = max(price), .groups = "drop")

 print(outlier_summary)


# which model phone out of stock 
out_of_stock <- products_df %>%
  filter(price == 0)

nrow(out_of_stock)

barplot(
  table(out_of_stock$model),
  las = 2,
  col = "red",
  main = "Out of Stock Smartphone Models",
  ylab = "Number of Products"
)

#total duplicate rows
sum(duplicated(products_df))

# total button phones and smartphones
library(ggplot2)
library(dplyr)
library(tidyr)

summary_df <- products_df %>%
  summarise(
    ButtonPhones = sum(is.na(Ram) | is.na(storage)),
    Smartphones = sum(!is.na(Ram) & !is.na(storage))
  ) %>%
  pivot_longer(everything(), names_to = "Type", values_to = "Count")


ggplot(summary_df, aes(x = Type, y = Count, fill = Type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("red", "skyblue")) +
  labs(title = "Button Phones vs Smartphones",
       x = "Phone Type",
       y = "Number of Phones") +
  theme_minimal()

#Price Distribution
library(dplyr)
library(ggplot2)

price_summary <- products_df %>%
  filter(!is.na(price)) %>%
  mutate(price_range = cut(price,
                           breaks = seq(0, 350000, by = 25000),
                           include.lowest = TRUE,
                           right = FALSE,
                           labels = paste0(seq(0, 325000, by = 25000), "-", seq(25000, 350000, by = 25000))
  )) %>%
  group_by(price_range) %>%
  summarise(Count = n(), .groups = "drop")

ggplot(price_summary, aes(x = price_range, y = Count)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  geom_text(aes(label = Count), vjust = -0.4, size = 4) +
  scale_y_continuous(breaks = seq(0, max(price_summary$Count) + 10, by = 5)) +
  labs(title = "Smartphone Count by Price Range",
       x = "Price Range (Tk)", y = "Count") +
  theme_minimal()



#price vs Ram
ggplot(products_df %>% filter(!is.na(Ram)), aes(x = Ram, y = price)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Price vs RAM", x = "RAM", y = "Price") +
  scale_y_continuous(breaks = seq(0, 350000, by = 50000)) + 
  theme_minimal()

#price vs storage
ggplot(products_df %>% filter(!is.na(storage)), aes(x = storage, y = price)) +
  geom_boxplot(fill = "lightpink") +
  labs(title = "Price vs Storage", x = "Storage", y = "Price") +
  scale_y_continuous(breaks = seq(0, 350000, by = 50000)) + 
  theme_minimal()

#Count of phones per brand
products_df %>%
  group_by(Brand) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = reorder(Brand, -Count), y = Count, fill = Brand)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.5) +
  labs(title = "Number of Phones per Brand", x = "Brand", y = "Count") +
  theme_minimal()

#Average price per brand
library(dplyr)
library(ggplot2)

products_df %>%
  group_by(Brand) %>%
  summarise(AveragePrice = mean(price, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = reorder(Brand, -AveragePrice), y = AveragePrice, fill = Brand)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0, 350000, by = 25000)) + 
  labs(title = "Average Price per Brand", x = "Brand", y = "Average Price") +
  theme_minimal()


#  Price distribution by Brand
ggplot(products_df %>% filter(!is.na(price), !is.na(Brand)),
       aes(x = reorder(Brand, price, FUN = median), y = price)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Smartphone Prices Across Brands",
       x = "Brand", y = "Price ") +
  scale_y_continuous(breaks = seq(0, 350000, by = 15000)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Rating Analysis with brand
products_df %>%
  filter(!is.na(rating)) %>%
  group_by(Brand) %>%
  summarise(AverageRating = mean(rating)) %>%
  ggplot(aes(x = reorder(Brand, -AverageRating), y = AverageRating, fill = Brand)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,5, by = .2)) + 
  labs(title = "Average Rating per Brand", x = "Brand", y = "Rating") +
  theme_minimal()

#Price vs Rating
ggplot(products_df %>% filter(!is.na(rating)), aes(x = price, y = rating, color = Brand)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_x_continuous(breaks = seq(0, 350000, by = 25000)) +
  labs(title = "Price vs Rating", x = "Price", y = "Rating") +
  theme_minimal()



#chi -square test
library(dplyr)
products_df <- products_df %>%
  mutate(
    Brand = as.factor(Brand),
    Sold_out = as.factor(ifelse(price == 0, "Yes", "No"))  
  )

tbl <- table(products_df$Brand, products_df$Sold_out)

chi_result <- chisq.test(tbl)
print(chi_result)

##


library(dplyr)
library(stringr)

cluster_df <- products_df %>%
  filter(!is.na(price), !is.na(Ram), !is.na(storage)) %>%
  mutate(
    Ram_num = as.numeric(str_remove(Ram, "GB")),
    Storage_num = as.numeric(str_remove(storage, "GB|TB")) *
      ifelse(str_detect(storage, "TB"), 1024, 1)
  ) %>%
  select(price, Ram_num, Storage_num)

# Scaling (mandatory for clustering)
cluster_scaled <- scale(cluster_df)

set.seed(123)

)

##
library(dplyr)

cluster_range <- cluster_df %>%
  group_by(KMeans_Cluster) %>%
  summarise(
    Price_Range =
      paste0(round(min(price)), " to ", round(max(price))),
    
    RAM_Range =
      paste0(round(min(Ram_num)), " to ", round(max(Ram_num))),
    
    Storage_Range =
      paste0(round(min(Storage_num)), " to ", round(max(Storage_num)))
  )

print(cluster_range)






