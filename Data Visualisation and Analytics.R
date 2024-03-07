## HEADER ####
## Who: <William Banda>
## What: Data Visualisation and Analysis (C7083)
## Last edited: 2024-02-08
####

## CONTENTS ####
## 00 Setup
## 01 Import Data
## 02 Data Tidying
## 03 Data Visualisation
## 04 Statistical Analysis and Plotting Graphs


## 00 Setup
# Get working directory
getwd() # Prints working directory in Console

## Set working directory
setwd("C:/Users/WilliamBanda1/Documents/C7083")

# Install relevant packages and libraries
install.packages("readr")
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)

# 01 Import data
# Import the Milk Cow Facts Data
Data <- readr::read_csv("C:/Users/WilliamBanda1/Documents/C7083/milkcow_facts.csv")

# Import the State_Milk_Production Data
Data2 <- readr::read_csv("C:/Users/WilliamBanda1/Documents/C7083/state_milk_production.csv")

# Import the Fluid_Milk_sales_Data
Data3 <- readr::read_csv("C:/Users/WilliamBanda1/Documents/C7083/fluid_milk_sales.csv")

# Import the Clean Cheese Data
Data4 <- readr::read_csv("C:/Users/WilliamBanda1/Documents/C7083/clean_cheese.csv")

# Import the Milk_Products_Facts Data
Data5 <- readr::read_csv("C:/Users/WilliamBanda1/Documents/C7083/milk_products_facts.csv")

# 02 Data Tidying
# Rename the variables in Clean Cheese Data
Data4 <- Data4 %>%
  rename(
    total_american_cheese = `Total American Chese`,
    total_italian_cheese = `Total Italian Cheese`,
    total_natural_cheese = `Total Natural Cheese`,
    total_processed_cheese_products = `Total Processed Cheese Products`
  )

## 03 Data Visualisation

# Visualise the milk cow facts data using a multi-panel bar plot
# firt select only the specified variables in the data set
Data_subset <- Data[, c("year", "avg_milk_cow_number", "milk_per_cow", "milk_production_lbs", "avg_price_milk")]

# Reshape the data from wide to long format
Data_long <- pivot_longer(Data_subset, cols = -year, names_to = "Variable", values_to = "Value")

# Create a multi panel bar plot using ggplot comparing each variable to year
ggplot(Data_long, aes(x = year, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Variable, scales = "free_y", labeller = labeller(Variable = c(
    year = "Year",
    avg_milk_cow_number = "Average Number of Milk Cows",
    milk_per_cow = "Average Milk Production/Cow (lbs)",
    milk_production_lbs = "Total Milk Production (lbs)",
    avg_price_milk = "Average Milk Price (Dollars/lb)"
  )), ncol = 2) +
  labs(title = "Comparison of Milk Cow Facts Over the Years",
       x = "Year",
       y = "Value") +
  scale_y_continuous(labels = scales::comma) +  # Format y-axis labels as comma-separated
  theme_minimal() +
  theme(legend.position = "none",  # Remove legend
        strip.text = element_text(size = 10),  # Customize facet titles
        strip.background = element_blank())  # Remove facet title background

# Now lets plot a heat map of the top ten producing milk states in the US
# Calculate the total milk production by state
milk_by_state <- aggregate(milk_produced ~ state, Data2, sum)

# Sort the states by milk production in ascending order and select the top 10
top_states <- head(arrange(milk_by_state, milk_produced), 10)


# Create the heatmap
ggplot(top_states, aes(x = 1, y = reorder(state, milk_produced), fill = milk_produced)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue", labels = scales::comma) +
  labs(title = "Top 10 Milk Producing States",
       x = NULL,
       y = "State",
       fill = "Milk Produced (lbs)") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


# Create an area chart of the Fluid Milk Sales Data 

# Assigning the dataframe 'Data3' to a variable named 'df'
df <- Data3

# Create the area chart
ggplot(df, aes(x = year, y = pounds, fill = milk_type)) +
  geom_area(stat = "identity") +  # Use "identity" for stacked area
  labs(title = "Fluid Milk Sales by Type per Year",
       x = "Year", y = "Pounds", fill = "Milk Type") +  # Restore y-axis label and rename legend title
  scale_fill_brewer(palette = "Set1") + # Use the same palette for fill and color
  theme_bw() +
  scale_y_continuous(labels = scales::comma_format(), name = "Pounds")  # Format y-axis with commas, set label to "Pounds"

# Lets Visualise the clean cheese data using a line plot to observe the trends
# Filter the dataset to include only the specified variables
Data4_filtered <- Data4 %>%
  select(Year, total_american_cheese, total_italian_cheese, 
         total_natural_cheese, total_processed_cheese_products)

# Melt the filtered data into long format
Data4_long <- pivot_longer(Data4_filtered, cols = -Year, names_to = "Variable", values_to = "Value")


# Define custom colors for each line
custom_colors <- c("total_american_cheese" = "blue", 
                   "total_italian_cheese" = "red", 
                   "total_natural_cheese" = "green", 
                   "total_processed_cheese_products" = "black")

# Create a line plot with custom colors
ggplot(Data4_long, aes(x = Year, y = Value, color = Variable)) +
  geom_line() +
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  labs(title = "Trend of Cheese Consumption Over Time",
       x = "Year",
       y = "Consumption (lbs/person)",
       color = "Type of Cheese") +
  theme_minimal()

 # Lets create a scatter plot using base R to compare consumption of 2 type of Cheese
  plot(Data4$total_american_cheese,                          # Specify data
       Data4$total_italian_cheese,                                # Specify data
       main = "American Cheese vs Italian Cheese Consumption", # Add title
       xlab = "Total American Cheese 
       (lbs/person)",                        # Add x-axis label
       ylab = "Total Italian Cheese 
       (lbs/person)")                              # Add y-axis label
  
  # Fit linear regression model
  lm_model <- lm(total_italian_cheese ~ total_american_cheese, data = Data4)
  
  # Add regression line
  abline(lm_model, col = "red")
  
# Create an interactive plot of Dairy Products Consumption Over Time
# Filter by specific years (optional)
Data5_filtered <- Data5 %>%
  filter(year >= 2010 & year <= 2020)  # Example filter

     # Create the line chart
plot_ly(Data5_filtered, x = ~year) %>%
  # Add product lines
  add_lines(x = ~year, y = ~butter, color = "Butter", name = "Butter") %>%
  add_lines(x = ~year, y = ~dry_whey, color = "Dry Whey", name = "Dry Whey") %>%
  add_lines(x = ~year, y = ~fluid_yogurt, color = "Fluid Yogurt", name = "Fluid Yogurt") %>%
  add_lines(x = ~year, y = ~frozen_ice_cream_regular, color = "Frozen Ice Cream (Regular)", name = "Frozen Ice Cream (Regular)") %>%
  # Add labels on lines
  add_text(x = ~year[10], y = ~butter[10], text = "Butter", showlegend = FALSE, textposition = "top center") %>%
  add_text(x = ~year[20], y = ~dry_whey[20], text = "Dry Whey", showlegend = FALSE, textposition = "top center") %>%
  add_text(x = ~year[30], y = ~fluid_yogurt[30], text = "Fluid Yogurt", showlegend = FALSE, textposition = "top center") %>%
  add_text(x = ~year[40], y = ~frozen_ice_cream_regular[40], text = "Frozen Ice Cream (Regular)", showlegend = FALSE, textposition = "top center") %>%
  # Customize plot labels and legend
  layout(title = "Dairy Products Consumption Over Time",
         yaxis = list(title = "Consumption (lbs/person)"),
         legend = list(title = list(text = "Product")))



