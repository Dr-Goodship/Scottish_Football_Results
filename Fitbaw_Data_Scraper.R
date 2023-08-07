library(rvest)
library(dplyr)

getwd()
setwd("/Users/paulgoodship/Documents/Data Projects/Football/Scotland/All Results/Github/")

# Set up empty lists
df <- list()
df1 <- list()
df2 <- list()

# URL of the page to scrape
url <- "https://www.fitbastats.com/index.php"

# Read the HTML content of the page
page <- read_html(url)

# Extract the relevant web links
links <- html_attr(html_nodes(page, "a[href*='/index.php']"), "href")

# Extract characters before "/index.php"
link_names <- sub("/index.php.*", "", links)

# Create a list of link names
link_list <- as.list(link_names)

# Loop through each link
for (i in seq_along(link_list)) {
  # Construct the URL for the current link
  url <- paste0("https://www.fitbastats.com/", link_list[[i]], "/team_results_list.php?page=")
  
  # Scrape data from HTML tables
  for (j in 1:200) {
    tryCatch({
      # Construct the complete URL for the current page
      page_url <- paste0(url, j)
      
      # Read the HTML content of the page
      page <- read_html(page_url)
      
      # Extract the HTML table from the page
      table <- html_table(page)
      
      # Filter data frames with 6 columns
      filtered_table <- Filter(function(df) ncol(df) == 6, table)
      
      # Check if the filtered table is not empty
      if (!is_empty(filtered_table)) {
        # Append the filtered table to the result list
        df[[length(df) + 1]] <- filtered_table
      }
    }, error = function(e) return(NULL))
  }
}

# Add link_list names as an extra column to each data frame in df
for (i in seq_along(df)) {
  if (!is_empty(df[[i]])) {
    df[[i]] <- lapply(df[[i]], function(x) mutate(x, link = link_list[[i]]))
  }
}


# Loop through each data frame in df and add the link column
for (i in seq_along(df)) {
  if (!is_empty(df[[i]])) {
    df[[i]]$link <- link_list[i]
  }
}


#filtered_df <- Filter(function(df) ncol(df) == 7, table)

# Initialize empty list
filtered_df <- list()

# Loop through each element in df
for (i in seq_along(df)) {
  for (j in seq_along(df[[i]])) {
    tryCatch({
      # Check if the data frame is not NULL and has exactly 7 columns
      if (!is.null(df[[i]][[j]]) && ncol(df[[i]][[j]]) == 7) {
        # Add the data frame to filtered_df
        filtered_df[[length(filtered_df) + 1]] <- df[[i]][[j]]
      }
    }, error = function(e) {
      # Ignore errors and continue to the next element
      NULL
    })
  }
}

# Combine all the data frames into a single data frame
finalDF <- do.call(rbind, filtered_df)


# Combine all the data frames into a single data frame
finalDF <- do.call(rbind, filtered_df)






# Combine all the data frames into a single data frame
finalDF <- do.call(rbind, filtered_df)




# Combine all the non-empty data frames into a single data frame
finalDF <- do.call(rbind, df)


# Combine all the non-empty data frames into a single data frame
finalDF <- bind_rows(df)

### create lookup file
# Create a data frame for the lookup file
team_lookup <- data.frame(
  team_code = link_names,
  team_name = c(
    "Scotland",
    "Scotland B",
    "Scotland U23",
    "Scotland U21",
    "Aberdeen FC",
    "Celtic FC",
    "Dundee FC",
    "Heart of Midlothian FC",
    "Hibernian FC",
    "Kilmarnock FC",
    "Livingston FC",
    "Motherwell FC",
    "Rangers FC",
    "Ross County FC",
    "St. Johnstone FC",
    "St. Mirren FC",
    "Airdrieonians FC",
    "Arbroath FC",
    "Ayr United FC",
    "Dundee United FC",
    "Dunfermline Athletic FC",
    "Inverness Caledonian Thistle FC",
    "Greenock Morton FC",
    "Partick Thistle FC",
    "Queen's Park FC",
    "Raith Rovers FC",
    "Alloa Athletic FC",
    "Annan Athletic FC",
    "Cove Rangers FC",
    "Edinburgh City FC",
    "Falkirk FC",
    "Hamilton Academical FC",
    "Kelty Hearts FC",
    "Montrose FC",
    "Queen of the South FC",
    "Stirling Albion FC",
    "Bonnyrigg Rose Athletic FC",
    "Clyde FC",
    "Dumbarton FC",
    "East Fife FC",
    "Elgin City FC",
    "Forfar Athletic FC",
    "Peterhead FC",
    "Spartans FC",
    "Stenhousemuir FC",
    "Stranraer FC",
    "Albion Rovers FC",
    "Berwick Rangers FC",
    "Brechin City FC",
    "Cowdenbeath FC",
    "East Stirlingshire FC",
    "Airdrieonians 1878 FC",
    "Clydebank FC",
    "Gretna FC",
    "King's Park FC",
    "Leith Athletic FC",
    "Port Glasgow Athletic FC",
    "St. Bernard's FC",
    "Third Lanark FC",
    "Vale of Leven FC"
  ),
  stringsAsFactors = FALSE
)

# Save the lookup file as a CSV
write.csv(team_lookup, "team_lookup.csv", row.names = FALSE)

# Read the team lookup file
#team_lookup <- read.csv("team_lookup.csv", stringsAsFactors = FALSE)

# Merge the team lookup with the finalDF data frame
mergedDF <- merge(finalDF, team_lookup, by.x = "link", by.y = "team_code", all.x = TRUE)

# Display the merged data frame
print(mergedDF)




