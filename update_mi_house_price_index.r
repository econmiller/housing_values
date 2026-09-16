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

# 2. Robust function to fetch data via public FRED CSV URLs
fetch_fred_csv <- function(code, name) {
  url <- paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", code)
  df <- read.csv(url, stringsAsFactors = FALSE)
  
  colnames(df)[1] <- "DATE"
  colnames(df)[2] <- "val"
  
  df$val <- suppressWarnings(as.numeric(df$val))
  df$Date <- as.Date(df$DATE, format = "%Y-%m-%d")
  
  # Calculate Year-over-Year percent change (4 quarters prior)
  df <- df %>%
    filter(!is.na(Date)) %>%
    arrange(Date) %>%
    mutate(yoy = (val - lag(val, 4)) / lag(val, 4) * 100) %>%
    select(Date, yoy)
  
  colnames(df)[2] <- name
  return(df)
}

# Fetch all series and merge them together by Date
dfs <- mapply(fetch_fred_csv, names(series_map), series_map, SIMPLIFY = FALSE)
combined_df <- Reduce(function(x, y) full_join(x, y, by = "Date"), dfs)

# Filter for the last 3 years of quarterly data (12 quarters)
combined_df <- combined_df %>% 
  filter(!is.na(Date)) %>%
  arrange(desc(Date))

df_recent <- head(combined_df, 12)

# Helper function to format date into "YYYY Q#" format
get_quarter_str <- function(date_val) {
  y <- format(date_val, "%Y")
  q <- ceiling(as.numeric(format(date_val, "%m")) / 3)
  paste0(y, " Q", q)
}

df_recent$Quarter <- sapply(df_recent$Date, get_quarter_str)

# 3. Transpose the table: Regions as rows, Quarters as columns (Rounded to 1 decimal place)
df_for_pivot <- df_recent %>% select(-Date)

df_long <- df_for_pivot %>%
  pivot_longer(cols = -Quarter, names_to = "Region", values_to = "YOY")

df_transposed <- df_long %>%
  pivot_wider(names_from = Quarter, values_from = YOY) %>%
  mutate(across(where(is.numeric), ~ round(., 1)))

# Sort quarter columns chronologically
quarter_cols <- sort(setdiff(names(df_transposed), "Region"))
df_transposed <- df_transposed %>% select(Region, all_of(quarter_cols))

# 4. Generate Styled HTML Table using MSU Colors with Subtitle and Footer Notes
html_table <- paste0(
  "<div style='overflow-x:auto; font-family: Arial, sans-serif;'>\n",
  "<style>\n",
  "  .msu-housing-table {\n",
  "    width: 100%;\n",
  "    border-collapse: collapse;\n",
  "    font-size: 14px;\n",
  "    color: #333333;\n",
  "    margin-bottom: 8px;\n",
  "  }\n",
  "  .msu-housing-table th {\n",
  "    background-color: #18453b;\n",
  "    color: #ffffff;\n",
  "    text-align: left;\n",
  "    padding: 10px 12px;\n",
  "    border: 1px solid #18453b;\n",
  "  }\n",
  "  .msu-housing-table td {\n",
  "    padding: 9px 12px;\n",
  "    border: 1px solid #dcdcdc;\n",
  "  }\n",
  "  .msu-housing-table tr:nth-child(even) {\n",
  "    background-color: #f4f7f5;\n",
  "  }\n",
  "  .msu-housing-table tr:hover {\n",
  "    background-color: #e8ede9;\n",
  "  }\n",
  "  .table-subtitle {\n",
  "    font-size: 13px;\n",
  "    color: #555555;\n",
  "    margin-bottom: 10px;\n",
  "    font-weight: bold;\n",
  "  }\n",
  "  .table-footer {\n",
  "    font-size: 11px;\n",
  "    color: #666666;\n",
  "    font-style: italic;\n",
  "    margin-top: 5px;\n",
  "  }\n",
  "</style>\n",
  "<div class='table-subtitle'>All measures are in percent change, year-over-year</div>\n",
  kable(df_transposed, format = "html", table.attr = "class='msu-housing-table'", col.names = c("Region / Area", quarter_cols)),"\n",
  "<div class='table-footer'>Source: Realtor.com via FRED: St. Louis Federal Reserve</div>\n",
  "</div>"
)

# Save to HTML snippet file
writeLines(html_table, "housing_table.html")
print("Updated MSU styled housing table generated successfully!")
