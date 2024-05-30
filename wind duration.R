### Calcuate wind event duration as a function
### change threshold_speed and rerun

### finding function
find_wind_events <- function(df, threshold_speed = 5) {
  wind_events <- list()
  current_event <- list(start_date = NULL, start_time = NULL, end_date = NULL, end_time = NULL, duration = 0)
  event_index <- 1
  
  for (i in 1:nrow(df)) {
    wind_speed <- df[i, "wind_speed"]
    
    if (wind_speed > threshold_speed) {
      if (is.null(current_event$start_date)) {
        current_event$start_date <- df[i, "sampledate"]
        current_event$start_time <- df[i, "hours"]
      }
      current_event$end_date <- df[i, "sampledate"]
      current_event$end_time <- df[i, "hours"]
      current_event$duration <- current_event$duration + 1
      df[i, "Wind_Event"] <- event_index
      df[i, "Event_Duration"] <- current_event$duration
    } else if (!is.null(current_event$start_date)) {
      wind_events[[event_index]] <- current_event
      current_event <- list(start_date = NULL, start_time = NULL, end_date = NULL, end_time = NULL, duration = 0)
      event_index <- event_index + 1
    }
  }
  
  if (!is.null(current_event$start_date)) {
    wind_events[[event_index]] <- current_event
  }
  
  return(wind_events)
}

### Bring in data
file_path <- "your file path"
df <- read.csv(file_path, header = TRUE)

### Create new columns
df$Wind_Event <- ''
df$Event_Duration <- ''

### output
wind_events <- find_wind_events(df)


# Embedding results into the input CSV file
event_index <- 1
for (event in wind_events) {
  start_date <- event$start_date
  end_date <- event$end_date
  start_time <- event$start_time
  end_time <- event$end_time
  duration <- event$duration
  df[df$sampledate == start_date & df$hour == start_time, "Wind_Event"] <- event_index
  df[df$sampledate == end_date & df$hour == end_time, "Event_Duration"] <- duration
  df[df$Wind_Event == event_index, "Event_Duration"] <- duration
  event_index <- event_index + 1
}

# For hours with wind speed less than 5m/s, leave the Wind_Event and Event_Duration columns empty
df[df$wind_speed <= 5, c("Wind_Event", "Event_Duration")] <- ''

# Saving the updated CSV file
write.csv(df, file = "file name", row.names = FALSE)


#### remove missing lines
df <- read.csv(file.choose(), header=TRUE)

weve <- df %>%
  drop_na(Event_Duration) %>%
  drop_na(Wind_Event)
### 

### save results
write.csv(weve_5m, "filename")
