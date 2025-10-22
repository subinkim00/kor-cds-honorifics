library(dplyr)
library(stringr)

################################################################################
# DF CONSTRUCTION
################################################################################
# define base directory
base_dir <- "~/Desktop/kor"

# define age groups
age_groups <- c("8mo", "13mo", "27mo")

# initialize list to store df's for each age group
age_group_data <- list()

# honorifics patterns as a regex
honorifics_pattern <- "(?<![\\p{L}@])(?:네|예|저는|제가|전|저희)(?=[^\\p{L}\\p{P}]|[:\\s,])|(?:요|죠|시오|니다|시다)(?=\\b|[^\\p{L}\\p{P}])|(?<=\\b)(네|예)(?=[[:punct:]\\s,])"

# loop through each age group
for (age in age_groups) {
  # define the path to coded folder
  coded_path <- file.path(base_dir, age, "coded")
  # get all .txt files in the coded folder
  txt_files <- list.files(coded_path, pattern = "\\.txt$", full.names = TRUE)
  # read each file into a df
  data_frames <- lapply(txt_files, function(file) {
    # extract participant ID from the filename (ex. coded_13_A0P04M.txt)
    participant_id <- str_extract(basename(file), "(?<=coded_)[^\\.]+")
    # read file into df
    df <- read.delim(file, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
    # assign column names
    colnames(df) <- c("line_number", "speaker", "code", "transcription")
    # line_number to numeric for consistency
    df$line_number <- as.numeric(df$line_number)
    # add time stamps
    df$time_stamp <- stringr::str_extract(df$transcription, "\\d+_\\d+")
    # clean up the transcription column to remove time stamps
    df$transcription <- gsub("\\x15.*", "", df$transcription)
    # add honorifics column
    df$honorifics <- ifelse(str_detect(df$transcription, honorifics_pattern), 1, 0)
    # add participant column
    df$participant <- participant_id
    return(df)
  })
  
  # combine all df's for age group
  age_group_data[[age]] <- bind_rows(data_frames)
}

# access df's by age group
data_8mo <- age_group_data[["8mo"]]
data_13mo <- age_group_data[["13mo"]]
data_27mo <- age_group_data[["27mo"]]

# big data frame with all age groups
combined_data <- bind_rows(age_group_data, .id = "age_group")

# extract start and end times from annotations (convert from milliseconds to seconds)
combined_data <- combined_data %>%
  mutate(
    start_time = as.numeric(sub("_.*", "", time_stamp)) / 1e3, # extract and convert start time
    end_time = as.numeric(sub(".*_", "", time_stamp)) / 1e3 # extract and convert end time
  )

# calculate and add duration in seconds to df
combined_data <- combined_data %>%
  mutate(duration = end_time - start_time)