library(data.table)
library(lubridate)

years <- 1985:2023
file_root <- "https://www.ndbc.noaa.gov/view_text_file.php?filename=44013h"
tail <- ".txt.gz&dir=data/historical/stdmet/"
buoy_data_list <- list()

for (year in years) {
  
  if (year == 2000) next  # Skip the year 2000 as per your requirement
  
  path <- paste0(file_root, year, tail)
  skip_lines <- if (year >= 1985 & year <= 2006) 1 else 2
  
  header <- scan(path, what = 'character', nlines = 1, quiet = TRUE)
  second_line <- scan(path, what = 'character', nlines = 1, skip = 1, quiet = TRUE)
  
  if (length(second_line) == length(header)) {
    buoy <- fread(path, header = FALSE, skip = skip_lines)
  } else {
    buoy <- fread(path, header = FALSE, skip = 1)
  }
  
  colnames(buoy) <- header
  
  
  if (all(c('YY', 'MM', 'DD', 'hh', 'mm') %in% colnames(buoy))) {
    buoy[, Date := make_datetime(YY, MM, DD, hh, mm)]
  }
  
  buoy_data_list[[as.character(year)]] <- buoy
}
all_buoy_data <- rbindlist(buoy_data_list, fill = TRUE)


all_buoy_data$YY <- as.character(all_buoy_data$YY)
all_buoy_data$YYYY <- as.character(all_buoy_data$YYYY)
all_buoy_data$`#YY` <- as.character(all_buoy_data$`#YY`)
all_buoy_data$year <- ifelse(!is.na(all_buoy_data$YYYY), all_buoy_data$YYYY,
                             ifelse(!is.na(all_buoy_data$`#YY`), all_buoy_data$`#YY`, all_buoy_data$YY))
all_buoy_data$year <- as.numeric(all_buoy_data$year)
all_buoy_data$year <- ifelse(all_buoy_data$year >= 85 & all_buoy_data$year <= 98, 
                             all_buoy_data$year + 1900, 
                             all_buoy_data$year)

all_buoy_data <- all_buoy_data %>% select(-YY, -YYYY, -`#YY`)
all_buoy_data <- all_buoy_data %>% relocate(year, .before = 1)
all_buoy_data$PRES <- ifelse(!is.na(all_buoy_data$PRES), all_buoy_data$PRES, all_buoy_data$BAR)
all_buoy_data$WDIR <- ifelse(!is.na(all_buoy_data$WDIR), all_buoy_data$WDIR, all_buoy_data$WD)
all_buoy_data <- all_buoy_data %>% select(-BAR, -WD)

library(data.table)
file_root <- "https://www.ndbc.noaa.gov/view_text_file.php?filename=44013h"
year <- "2000"
tail <- ".txt.gz&dir=data/historical/stdmet/"
path <- paste0(file_root, year, tail)
buoy_2000 <- fread(path, header = FALSE, skip = 1, fill = TRUE)
header <- scan(path, what = 'character', nlines = 1, quiet = TRUE)
if (length(header) != ncol(buoy_2000)) {
  header <- header[1:ncol(buoy_2000)]
}
setnames(buoy_2000, header)
if (!"TIDE" %in% colnames(buoy_2000)) {
  buoy_2000[, TIDE := 99] 
} else {
  buoy_2000[TIDE == "", TIDE := 99]
}
buoy_2000 <- buoy_2000 %>% rename(year = YYYY)
buoy_2000 <- buoy_2000 %>% 
  rename(PRES = BAR, WDIR = WD)
buoy_2000$mm <- NA
buoy_2000$year <- as.numeric(buoy_2000$year)


buoy_1985_2023 <- bind_rows(buoy_2000, all_buoy_data)
buoy_1985_2023 <- buoy_1985_2023 %>% arrange(year)
buoy_1985_2023$mm[is.na(buoy_1985_2023$mm)] <- 0

buoy_1985_2023$datetime <- make_datetime(
  year = buoy_1985_2023$year, 
  month = buoy_1985_2023$MM, 
  day = buoy_1985_2023$DD, 
  hour = buoy_1985_2023$hh, 
  min = buoy_1985_2023$mm
)