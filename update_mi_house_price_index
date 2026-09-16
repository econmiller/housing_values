# Install required packages if you don't have them
# install.packages(c("quantmod", "xts", "dplyr", "tidyr", "knitr"))

library(quantmod)
library(xts)
library(dplyr)
library(tidyr)
library(knitr)

# 1. Define the FRED series codes and their human-readable names
series_map <- c(
  "USSTHPI"         = "United States",
  "MISTHPI"         = "Michigan",
  "ATNHPIUS19804Q"  = "Detroit-Dearborn-Livonia, MI (MSAD)",
  "ATNHPIUS47644Q"  = "Warren-Troy-Farmington Hills, MI (MSAD)",
  "ATNHPIUS24340Q"  = "Grand Rapids-Kentwood, MI (MSA)",
  "ATNHPIUS11460Q"  = "Ann Arbor, MI (MSA)",
  "ATNHPIUS29620Q"  = "Lansing-East Lansing, MI (MSA)",
  "ATNHPIUS22420Q"  = "Flint, MI (MSA)",
  "ATNHPIUS28020Q"  = "Kalamazoo-Portage, MI (MSA)",
  "ATNHPIUS27100Q"  = "Jackson, MI (MSA)",
  "ATNHPIUS40980Q"  = "Saginaw, MI (MSA)",
  "ATNHPIUS12980Q"  = "Battle Creek, MI (MSA)"
)

# 2. Fetch data from FRED and calculate Year-over-Year percent change
fetch_yoy <- function(code) {
  # getSymbols pulls directly into the global environment or returns it
  xts_data <- getSymbols(code, src = "FRED", auto.assign = FALSE)
  
  # Calculate percent change from 4 quarters ago: ((Xt - Xt-4) / Xt-4) * 100
  yoy_change <- (xts_data - lag(xts_data, 4)) / lag(xts_data, 4) * 100
  return(yoy_change)
}

# Combine all series into a single list and merge
series_list <- lapply(names(series_map), fetch_yoy)
combined_xts <- do.call(merge, series_list)
colnames(combined_xts) <- unname(series_map)

# Convert to a data frame
df <- data.frame(Date = index(combined_xts), coredata(combined_xts))

# 3. Filter for the last 3 years of quarterly data
# Convert Date to standard Date object, arrange descending to get latest, then filter
df$Date <- as.Date(df$Date)
df <- df %>% arrange(desc(Date))

# Keep the most recent 12 quarters (3 years)
df_recent <- head(df, 12)

# Format the Date column nicely (e.g., "2025 Q4")
# Since FRED dates usually map to the end of the quarter:
get_quarter_str <- function(date_val) {
  y <- format(date_val, "%Y")
  q <- ceiling(as.numeric(format(date_val, "%m")) / 3)
  paste0(y, " Q", q)
}
df_recent$Quarter <- sapply(df_recent$Date, get_quarter_str)

# Reorder columns to put Quarter first and drop raw Date
df_table <- df_recent %>%
  select(Quarter, everything(), -Date) %>%
  mutate(across(where(is.numeric), ~ round(., 2))) # Round to 2 decimal places

# 4. Generate HTML Table markup
# You can customize the table classes to match your MSU/Bootstrap web styles if needed
html_table <- kable(df_table, format = "html", 
                    table.attr = "class='table table-striped table-bordered'",
                    col.names = c("Quarter", unname(series_map)))

# Save to an HTML snippet file that your website can pull or display
writeLines(html_table, "housing_table.html")
print("Housing table generated successfully!")
