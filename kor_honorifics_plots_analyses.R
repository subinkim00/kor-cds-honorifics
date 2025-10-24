library(dplyr)
library(ggplot2)
library(viridis)
library(scales) # percent formatting
library(broom)  # tidying model output
library(ggpubr) # adding stats easily to plot
library(ggrepel)
library(tidyr)

################################################################################
# REFER TO kor_honorifics_df_constr.R TO CONSTRUCT RELEVANT DF FROM TXT FILES
# df should contain following columns: participant, speaker, code, transcription,
# honorifics, age_group
################################################################################

############################## START_DESC_STATS ################################
# filter for MOT speaker and group by age_group and participant
utterances_per_minute_mot <- combined_data %>%
  filter(speaker == "MOT", code != "OS", code != "YY", code != "XX", code != "OO") %>%
  mutate(duration = ifelse(is.na(duration), 0, duration)) %>% # replace NA durations with 0
  group_by(age_group, participant) %>%
  summarize(
    total_utterances = n(), # total utterances for the participant
    total_duration_minutes = sum(duration, na.rm = TRUE) / 60, # total duration in minutes
    utterances_per_minute = ifelse(
      total_duration_minutes > 0,
      total_utterances / total_duration_minutes,
      NA
    ) # avoid division by zero
  ) %>%
  ungroup()

# calculate mean and standard deviation for each age_group
summary_stats_mot <- utterances_per_minute_mot %>%
  filter(!is.na(utterances_per_minute)) %>%
  group_by(age_group) %>% # Add this line
  summarize(
    mean_utterances_per_minute = mean(utterances_per_minute, na.rm = TRUE),
    sd_utterances_per_minute = sd(utterances_per_minute, na.rm = TRUE)
  )

# print results
print(utterances_per_minute_mot)
print(summary_stats_mot)

# group by age_group and participant for CHI
utterances_per_minute_chi <- combined_data %>%
  filter(speaker == "CHI", code != "XX", code != "YY", code != "OO") %>%
  mutate(duration = ifelse(is.na(duration), 0, duration)) %>%
  group_by(age_group, participant) %>%
  summarize(
    total_utterances = n(),
    total_duration_minutes = sum(duration, na.rm = TRUE) / 60,
    utterances_per_minute = ifelse(
      total_duration_minutes > 0,
      total_utterances / total_duration_minutes,
      NA
    )
  ) %>%
  ungroup()

# calculate mean and standard deviation for each age_group
summary_stats_chi_by_age <- utterances_per_minute_chi %>%
  filter(!is.na(utterances_per_minute)) %>% # exclude rows with NA values in rate
  group_by(age_group) %>%
  summarize(
    mean_utterances_per_minute = mean(utterances_per_minute, na.rm = TRUE),
    sd_utterances_per_minute = sd(utterances_per_minute, na.rm = TRUE),
    se_utterances_per_minute = sd(utterances_per_minute, na.rm = TRUE) / sqrt(n()) # calculate SE
  )

print(summary_stats_chi_by_age)

# calculate mean number of distinct speech act categories per age_group
# not counting "OS", "YY", "XX", "OO"
distinct_codes_per_participant <- combined_data %>%
  filter(!(code %in% c("OS", "YY", "XX", "OO"))) %>%
  group_by(age_group, speaker, participant) %>%
  summarize(
    n_distinct_codes = n_distinct(code)
  ) %>%
  ungroup()

summary_codes_mot <- distinct_codes_per_participant %>%
  filter(speaker == "MOT") %>%
  group_by(age_group) %>%
  summarize(
    mean_distinct_codes = mean(n_distinct_codes, na.rm = TRUE),
    sd_distinct_codes = sd(n_distinct_codes, na.rm = TRUE),
    se_distinct_codes = sd(n_distinct_codes, na.rm = TRUE) / sqrt(n())
  )

print(summary_codes_mot)
################################ END_DESC_STATS ################################

################################# START_GRAPHS #################################
#####################################
# PURE HONORIFICS COUNTS
#####################################
################################################################################
# PLOT 1: COMPARISON OF CHILD VS. EXP. VS. FAMILY-DIRECTED HONORIFICS USAGE
################################################################################

# calculate summary for CDS across all age groups

# filter for MOT's speech directed to the child
cds_data_all_ages <- combined_data %>%
  # exclude unintelligible speech, other-directed-speech, speaking-for-child
  filter(speaker == "MOT", !code %in% c("XX", "OS", "SF"))

# calculate the participant-level proportion of honorific utterances in CDS
cds_participant_data <- cds_data_all_ages %>%
  group_by(participant) %>%
  filter(n() > 0) %>% 
  summarise(
    honorific_proportion = sum(honorifics == 1, na.rm = TRUE) / n(), 
    .groups = 'drop' 
  )  

# calculate the mean and standard error across participants for CDS
if (nrow(cds_participant_data) > 1) {
  mean_prop_cds <- mean(cds_participant_data$honorific_proportion, na.rm = TRUE)
  sd_prop_cds <- sd(cds_participant_data$honorific_proportion, na.rm = TRUE)
  se_prop_cds <- sd_prop_cds / sqrt(nrow(cds_participant_data))
} else if (nrow(cds_participant_data) == 1) {
  mean_prop_cds <- cds_participant_data$honorific_proportion[1]
  sd_prop_cds <- NA 
  se_prop_cds <- NA 
} else {
  mean_prop_cds <- NA 
  sd_prop_cds <- NA
  se_prop_cds <- NA
}

# create summary data frame for CHI
summary_child <- data.frame(
  addressee = "Child",
  proportion = mean_prop_cds,
  sd = sd_prop_cds,
  se = se_prop_cds
)


# calculate summaries for experimenter- and family-directed speech

# filter for MOT's speech coded as OS or SF (other speech or speaking-for-child)
os_sf_data <- combined_data %>%
  filter(speaker == "MOT", code == "OS" | code == "SF") %>%
  mutate(
    addressee_type = ifelse(startsWith(transcription, "ads-fam "), "Family", "Experimenter") 
  )

# calculate the participant-level proportion for OS/SF addressees
os_sf_participant_data <- os_sf_data %>%
  group_by(participant, addressee_type) %>%
  filter(n() > 0) %>%
  summarise(
    honorific_proportion = sum(honorifics == 1, na.rm = TRUE) / n(), 
    .groups = 'drop' # Drop grouping
  )

# NOTE: removing participant 33_A2P12M specifically for the family addressee type
# (they were calling an aunt, unlike other mothers who called husbands or moms,
# skewing honorifics distribution)
participant_to_exclude <- "33_A2P12M"

os_sf_participant_data <- os_sf_participant_data %>%
  filter(!(participant == participant_to_exclude))

# calculate the mean and standard error for Experimenter
exp_data <- os_sf_participant_data %>% filter(addressee_type == "Experimenter")
if (nrow(exp_data) > 1) {
  mean_prop_exp <- mean(exp_data$honorific_proportion, na.rm = TRUE)
  sd_prop_exp <- sd(exp_data$honorific_proportion, na.rm = TRUE)
  se_prop_exp <- sd_prop_exp / sqrt(nrow(exp_data)) 
} else if (nrow(exp_data) == 1) {
  mean_prop_exp <- exp_data$honorific_proportion[1]
  sd_prop_exp <- NA
  se_prop_exp <- NA
} else {
  mean_prop_exp <- NA
  sd_prop_exp <- NA
  se_prop_exp <- NA
}

summary_experimenter <- data.frame(
  addressee = "Experimenter",
  proportion = mean_prop_exp,
  sd = sd_prop_exp,
  se = se_prop_exp
)

# calculate the mean and standard error for Family (using the filtered data)
fam_data <- os_sf_participant_data %>% filter(addressee_type == "Family") 

if (nrow(fam_data) > 1) {
  mean_prop_fam <- mean(fam_data$honorific_proportion, na.rm = TRUE)
  sd_prop_fam <- sd(fam_data$honorific_proportion, na.rm = TRUE)
  se_prop_fam <- sd_prop_fam / sqrt(nrow(fam_data))
} else if (nrow(fam_data) == 1) {
  mean_prop_fam <- fam_data$honorific_proportion[1]
  sd_prop_fam <- NA
  se_prop_fam <- NA
} else {
  mean_prop_fam <- NA
  sd_prop_fam <- NA
  se_prop_fam <- NA
}

summary_family <- data.frame(
  addressee = "Family",
  proportion = mean_prop_fam,
  sd = sd_prop_fam,
  se = se_prop_fam
)

# combine summaries
final_summary_df <- bind_rows(summary_child, summary_experimenter, summary_family) %>%
  filter(!is.na(proportion)) 

# create combined plot
addressee_levels <- c("Family", "Child", "Experimenter") 

final_summary_df <- final_summary_df %>%
  filter(addressee %in% addressee_levels)

ggplot(final_summary_df, aes(x = factor(addressee, levels = addressee_levels), y = proportion)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.7) +
  geom_errorbar(
    data = filter(final_summary_df, !is.na(se)), 
    aes(ymin = proportion - se, ymax = proportion + se), 
    width = 0.2, 
    color = "black"
  ) +
  geom_text(
    aes(label = scales::percent(proportion, accuracy = 0.1)), 
    color = "white", 
    size = 4.5,      
    position = position_stack(vjust = 0.5) 
  ) +
  labs(
    title = "Honorifics Usage by Mothers Towards Different Addressees",
    x = "Addressee",
    y = "Proportion of Honorific Utterances"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11)
  )

###############################
# SPEECH ACTS
###############################
################################################################################
# PLOT 2: Speech Act Codes for Honorific Utterances in CDS
################################################################################

# define the mapping from code to category (INCA-A)
category_map <- list(
  "Directives" = c("AC", "AD", "AL", "CL", "CS", "DR", "GI", "GR", "RD", "RP", "RQ", "SS", "WD"),
  "Speech Elicitations" = c("CX", "EA", "EI", "EC", "EX", "RT", "SC"),
  "Commitments" = c("FP", "PA", "PD", "PF", "SI", "TD"),
  "Declarations" = c("DC", "DP", "ND", "YD"),
  "Markings" = c("CM", "EM", "EN", "ES", "MK", "TO", "XA"),
  "Statements" = c("AP", "CN", "DW", "ST", "WS"),
  "Questions" = c("AQ", "AA", "AN", "EQ", "NA", "QA", "QN", "RA", "SA", "TA", "TQ", "YQ", "YA"),
  "Performances" = c("PR", "TX", "ON"), # ON is my own category for onomatopoeia
  "Evaluations" = c("AB", "CR", "DS", "ED", "ET", "PM"),
  "Demands for Clarification" = c("RR"),
  "Text Editing" = c("CT"),
  "Vocalizations" = c("YY", "OO"),
  "Speaking for Child" = c("SF") # my own category
)

# helper function to get category from code
get_category <- function(code_value) {
  for (category_name in names(category_map)) {
    if (code_value %in% category_map[[category_name]]) {
      return(category_name)
    }
  }
  # only categories not included in the map should be XX (unintelligible speech)
  # and OS (other-directed speech)
  if (code_value %in% c("XX", "OS")) { 
    return(NA) # explicitly return NA for unwanted codes
  } else {
    return("Other/Unknown") # catch unexpected codes
  }
}

# data prep

# filter data such that speaker=MOT and code != XX/OS
all_mot_data_categorized <- combined_data %>%
  filter(speaker == "MOT", !code %in% c("XX", "OS")) %>% 
  mutate(category = sapply(code, get_category)) %>%
  filter(!is.na(category) & category != "Other/Unknown") # remove NAs/Unknowns

# calculate per-participant counts and percentages for each category/age
participant_category_summary <- all_mot_data_categorized %>%
  group_by(participant, age_group, category) %>%
  summarise(
    total_utterances_in_cat = n(),
    honorific_utterances_in_cat = sum(honorifics == 1, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # calculate percentage for each participant, category, age_group
  # handle cases where a participant didn't use a category at all (total_utterances_in_cat = 0)
  mutate(
    honorific_percentage_in_cat = ifelse(total_utterances_in_cat > 0, 
                                         honorific_utterances_in_cat / total_utterances_in_cat, 
                                         NA_real_) # use NA if no utterances in that category for that participant/age
  )

# calculate mean, SD, N, and SEM of these percentages across participants for each category/age
# only include participants who actually used that category in that age group
# (honorific_percentage_in_cat is not NA)
mean_sem_summary <- participant_category_summary %>%
  filter(!is.na(honorific_percentage_in_cat)) %>% # exclude NA percentages
  group_by(age_group, category) %>%
  summarise(
    mean_honorific_percentage = mean(honorific_percentage_in_cat, na.rm = TRUE),
    sd_honorific_percentage = sd(honorific_percentage_in_cat, na.rm = TRUE),
    n_participants = n(), # num participants contributing to this mean
    .groups = 'drop'
  ) %>%
  mutate(
    sem_honorific_percentage = ifelse(n_participants > 1, 
                                      sd_honorific_percentage / sqrt(n_participants), 
                                      NA_real_) # SE is NA if only 1 participant
  )

# determine overall category order
# order by overall mean honorific percentage for legend/facet order
category_overall_mean_pct <- mean_sem_summary %>%
  group_by(category) %>%
  summarise(avg_mean_pct_across_ages = mean(mean_honorific_percentage, na.rm=TRUE), .groups='drop') %>%
  arrange(desc(avg_mean_pct_across_ages))

ordered_category_levels_lines <- category_overall_mean_pct$category

# apply factor ordering to the mean_sem_summary dataframe
mean_sem_summary_ordered <- mean_sem_summary %>%
  mutate(category = factor(category, levels = ordered_category_levels_lines)) %>%
  # ensure all age groups are present for each category, even if mean is NA (for complete lines)
  tidyr::complete(age_group, category) # fill in missing combinations with NA for means/sem

# create line graph faceted by category (within each speech act, what percentage
# of it was marked with honorifics, shown across age group)

# define order for age groups
age_levels <- c("8mo", "13mo", "27mo")

# plot based on 'mean_sem_summary_ordered'
ggplot(mean_sem_summary_ordered %>% filter(!is.na(mean_honorific_percentage) & (category == "Speaking for Child" | category == "Performances" | category == "Speech Elicitations" | category == "Test Editing")), 
       aes(x = factor(age_group, levels = age_levels), 
           y = mean_honorific_percentage, 
           group = 1)) + # group by 1 within each facet to connect points for that category
  geom_line(linewidth = 1, color = "steelblue") + # single color for line within facet
  geom_point(size = 2.5, color = "steelblue") +
  facet_wrap(~ category, scales = "free_y", ncol=3) + # facet by category, free y-axis
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     name = "Mean % of Utterances with Honorifics",
                     expand = expansion(mult = c(0.1, 0.15)), # more expansion for facets
                     limits = c(0,NA)) +
  labs(
    title = "Mean Percentage of Honorifics within Speech Act Categories Across Age",
    x = "Age Group",
    y = "Mean % of Utterances with Honorifics (within Category)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    strip.text = element_text(size = 10, face = "bold"), # facet titles
    axis.text.x = element_text(size=9),
    panel.grid.minor.y = element_blank(),
    panel.spacing = unit(1.5, "lines") # add spacing between facets
  )

################################################################################
# PLOT 3: 100% STACKED BAR CHART (DIST OF SPEECH ACT GIVEN HONORIFICS-MARKED)
################################################################################
# sum honorific utterances per category per age group (across participants)
honorifics_by_category_age <- participant_category_summary %>%
  group_by(age_group, category) %>%
  summarize(total_honorifics_in_cat_age = sum(honorific_utterances_in_cat, na.rm = TRUE),
            .groups = 'drop')

# calculate the total honorific utterances for each age group
total_honorifics_per_age <- honorifics_by_category_age %>%
  group_by(age_group) %>%
  summarize(overall_total_honorifics_for_age = sum(total_honorifics_in_cat_age, na.rm = TRUE),
            .groups = 'drop')

# join these and calculate the percentage
plot_data_for_grouped_bar <- honorifics_by_category_age %>%
  left_join(total_honorifics_per_age, by = "age_group") %>%
  mutate(
    # ensure this calculation results in a 0-100 scale percentage
    percentage_of_total_honorifics_in_age_group = ifelse(
      overall_total_honorifics_for_age > 0,
      (total_honorifics_in_cat_age / overall_total_honorifics_for_age) * 100,
      0
    ), # convert age_group to a factor with specified levels for ordering
    age_group = factor(age_group, levels = c("8mo", "13mo", "27mo")),
    # order categories for consistent plotting
    category = factor(category, levels = rev(c("Questions", "Directives", "Statements", "Performances",
                                               "Markings", "Commitments", "Evaluations", "Speech Elicitations",
                                               "Declarations", "Speaking for Child", "Text Editing",
                                               "Demands for Clarification", "Vocalizations")))
  )

# check if the data looks reasonable before plotting
if(any(plot_data_for_grouped_bar$percentage_of_total_honorifics_in_age_group > 100, na.rm = TRUE) ||
   any(plot_data_for_grouped_bar$percentage_of_total_honorifics_in_age_group < 0, na.rm = TRUE)) {
  warning("Calculated percentages are outside the 0-100 range. Please check data preparation.")
}

grouped_bar_plot <- ggplot(
  plot_data_for_grouped_bar,
  aes(
    x = category,
    y = percentage_of_total_honorifics_in_age_group / 100,
    fill = factor(age_group, levels = c("27mo", "13mo", "8mo"))
  )
) +
  geom_col(
    position = position_dodge(width = 0.9),
    colour = "black",
    width = 0.8
  ) +
  # manually assign colors for each age group
  scale_fill_manual(
    values = c(
      "27mo" = "black",
      "13mo" = "grey",
      "8mo"  = "white"
    )
  ) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_continuous(
    labels = number_format(accuracy = 0.1),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Distribution of honorific utterances across speech act categories",
    x = "Speech act category",
    y = "Proportion of total honorific utterances within age group",
    fill = "Age Group"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(hjust = 1, vjust = 1, size = 11),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    legend.position = "top"
  ) +
  coord_flip()
############################### END_GRAPHS #####################################

############################# START_STAT_TESTS #################################
################################################################
# PAIRED T-TEST: is cds honorifics > family-directed honorifics?
################################################################
# calculate the mean and SE for Experimenter
# filtering out participant we excluded OFDS speech for
cds_data <- cds_participant_data %>% filter(participant != "33_A2P12M")

# select only participant and honorific_proportion for merging
# and rename honorific_proportion to be specific to the addressee type
cds_proportions <- cds_data %>%
  select(participant, honorific_proportion) %>%
  rename(prop_cds = honorific_proportion)

fam_proportions <- fam_data %>%
  select(participant, honorific_proportion) %>%
  rename(prop_to_family = honorific_proportion)

# merge based on participant ID
paired_honorific_data <- merge(cds_proportions, fam_proportions, by = "participant")

# paired t-test: is cds honorifics > family-directed honorifics?
paired_ttest_result <- t.test(
  paired_honorific_data$prop_cds,
  paired_honorific_data$prop_to_family,
  paired = TRUE,
  alternative = "greater" # as opposed to "two.sided" or "less" if needed
)

################################################################
# AVERAGE RECORDING LENGTH
################################################################
# check if 'participant' column exists
if (!"participant" %in% colnames(combined_data)) {
  stop("'participant' column not found in combined_data. This column is needed to identify unique recording sessions.")
}
if (!"start_time" %in% colnames(combined_data) || !"end_time" %in% colnames(combined_data)) {
  stop("'start_time' and/or 'end_time' columns not found in combined_data.")
}

recording_lengths <- combined_data %>%
  group_by(participant) %>%
  summarize(
    session_min_start_time = min(start_time, na.rm = TRUE),
    session_max_end_time = max(end_time, na.rm = TRUE),
    .groups = 'drop' # drop grouping for next step
  ) %>%
  mutate(
    session_duration_seconds = session_max_end_time - session_min_start_time
  )

# handle potential Inf/-Inf if all start_time/end_time were NA for a participant
recording_lengths <- recording_lengths %>%
  filter(is.finite(session_duration_seconds))

# calculate the average recording length
if (nrow(recording_lengths) > 0) {
  average_recording_length_seconds <- mean(recording_lengths$session_duration_seconds, na.rm = TRUE)
  average_recording_length_minutes <- average_recording_length_seconds / 60
  
  print(paste("Average recording length across all sessions (seconds):", round(average_recording_length_seconds, 2)))
  print(paste("Average recording length across all sessions (minutes):", round(average_recording_length_minutes, 2)))
  
  # SD
  sd_recording_length_seconds <- sd(recording_lengths$session_duration_seconds, na.rm = TRUE)
  sd_recording_length_minutes <- sd_recording_length_seconds / 60
  print(paste("SD of recording length across all sessions (seconds):", round(sd_recording_length_seconds, 2)))
  print(paste("SD of recording length across all sessions (minutes):", round(sd_recording_length_minutes, 2)))
  
  # min/max
  min_recording_length_minutes <- min(recording_lengths$session_duration_seconds, na.rm = TRUE) / 60
  max_recording_length_minutes <- max(recording_lengths$session_duration_seconds, na.rm = TRUE) / 60
  print(paste("Min recording length (minutes):", round(min_recording_length_minutes, 2)))
  print(paste("Max recording length (minutes):", round(max_recording_length_minutes, 2)))
  print(paste("Number of unique recording sessions considered:", nrow(recording_lengths)))
  
} else {
  print("No valid recording lengths could be calculated. Check data and processing steps.")
}

################################################################
# INTERACTION DURATION BY ADDRESSEE
################################################################
# Filter for Mother's speech and valid addressee types
combined_data <- combined_data %>%
  mutate(
    duration = ifelse(is.na(duration), 0, duration), # ensure duration NAs are 0 for summing
    addressee_type = case_when(
      speaker == "MOT" & code == "OS" & str_starts(transcription, "ads-exp ") ~ "Experimenter",
      speaker == "MOT" & code == "OS" & str_starts(transcription, "ads-fam ") ~ "Family",
      speaker == "MOT" ~ "Child", # default for other MOT speech
      TRUE ~ NA_character_ # for non-MOT speech or unclassified
    )
  )

combined_data <- combined_data %>%
  mutate(
    transcription_cleaned = case_when(
      addressee_type == "Experimenter" ~ str_remove(transcription, "^ads-exp "),
      addressee_type == "Family" ~ str_remove(transcription, "^ads-fam "),
      TRUE ~ transcription
    )
  )

mot_speech_by_addressee <- combined_data %>%
  filter(speaker == "MOT", !is.na(addressee_type))

# check if we have data for each addressee type
print("Counts of MOT utterances by derived addressee_type:")
print(table(mot_speech_by_addressee$addressee_type))

# calculate total interaction duration per participant per addressee type
participant_interaction_durations <- mot_speech_by_addressee %>%
  group_by(participant, addressee_type) %>%
  summarize(total_interaction_duration_seconds = sum(duration, na.rm = TRUE), .groups = 'drop')

# calculate the mean of these total interaction durations for each addressee type
mean_durations_by_addressee_type <- participant_interaction_durations %>%
  group_by(addressee_type) %>%
  summarize(
    mean_total_duration_seconds = mean(total_interaction_duration_seconds, na.rm = TRUE),
    sd_total_duration_seconds = sd(total_interaction_duration_seconds, na.rm = TRUE),
    median_total_duration_seconds = median(total_interaction_duration_seconds, na.rm = TRUE),
    min_total_duration_seconds = min(total_interaction_duration_seconds, na.rm = TRUE),
    max_total_duration_seconds = max(total_interaction_duration_seconds, na.rm = TRUE),
    n_participants_in_category = n(), # mum participants who had speech in this addressee category
    .groups = 'drop'
  ) %>%
  mutate(
    mean_total_duration_minutes = mean_total_duration_seconds / 60,
    sd_total_duration_minutes = sd_total_duration_seconds / 60,
    median_total_duration_minutes = median_total_duration_seconds / 60,
    min_total_duration_minutes = min_total_duration_seconds / 60,
    max_total_duration_minutes = max_total_duration_seconds / 60
  )

print("Mean, SD, Median, Min, Max of (Total Interaction Duration per Participant) by Addressee Type:")
print(mean_durations_by_addressee_type)
participant_interaction_durations <- mot_speech_by_addressee %>%
  group_by(participant, addressee_type) %>%
  summarize(total_interaction_duration_seconds = sum(duration, na.rm = TRUE), .groups = 'drop')

# calculate the mean of these total interaction durations for each addressee type
mean_durations_by_addressee_type_per_participant_avg <- participant_interaction_durations %>%
  group_by(addressee_type) %>%
  summarize(
    mean_total_duration_seconds = mean(total_interaction_duration_seconds, na.rm = TRUE),
    sd_total_duration_seconds = sd(total_interaction_duration_seconds, na.rm = TRUE),
    n_participants_for_type = n(), # num participants who spoke to this addressee type
    .groups = 'drop'
  ) %>%
  mutate(
    mean_total_duration_minutes = mean_total_duration_seconds / 60,
    sd_total_duration_minutes = sd_total_duration_seconds / 60
  )

print("Method 1: Mean of (Total Interaction Duration per Participant) by Addressee Type:")
print(mean_durations_by_addressee_type_per_participant_avg)

################################################################
# AVERAGE HONORIFICS PROPORTIONS BY AGE GROUP
################################################################
participant_cds_proportions <- combined_data %>%
  filter(addressee_type == "Child") %>% # Filter for CDS
  group_by(age_group, participant) %>%
  summarize(
    total_cds_utterances_participant = n(),
    honorific_cds_utterances_participant = sum(honorifics, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    proportion_honorifics_cds_participant = ifelse(total_cds_utterances_participant > 0,
                                                   honorific_cds_utterances_participant / total_cds_utterances_participant,
                                                   NA)
  ) %>%
  filter(!is.na(proportion_honorifics_cds_participant)) # remove participants with no CDS utterances if any (should be none :p)

summary_cds_honorifics_by_age <- participant_cds_proportions %>%
  group_by(age_group) %>%
  summarize(
    mean_proportion_honorifics_cds = mean(proportion_honorifics_cds_participant, na.rm = TRUE),
    sd_proportion_honorifics_cds = sd(proportion_honorifics_cds_participant, na.rm = TRUE),
    median_proportion_honorifics_cds = median(proportion_honorifics_cds_participant, na.rm = TRUE),
    n_participants = n(), # num participants contributing to this age group's CDS stats
    se_proportion_honorifics_cds = sd_proportion_honorifics_cds / sqrt(n_participants),
    .groups = 'drop'
  )

print("Summary of Per-Participant Honorific Proportions in CDS by Age Group:")
print(summary_cds_honorifics_by_age)

################################################################
# PAIRED T-TEST: QUESTIONS VS. DIRECTIVES @ 27 MO
################################################################
# filter for 27mo and questions/directives
data_27mo_qd <- participant_category_summary %>%
  filter(age_group == "27mo" & category %in% c("Questions", "Directives"))

# pivot data wider
paired_data_27mo <- data_27mo_qd %>%
  select(participant, category, honorific_percentage_in_cat) %>%
  pivot_wider(
    names_from = category,
    values_from = honorific_percentage_in_cat
  )

# filter for participants with data for BOTH categories
# (remove rows where a participant might have "Questions" but not "Directives" at 27mo, or vice-versa)
paired_data_27mo_complete <- paired_data_27mo %>%
  filter(!is.na(Questions) & !is.na(Directives))

# perform the paired t-test
if (nrow(paired_data_27mo_complete) < 2) { # need at least 2 pairs for sd of differences
  print("Not enough paired observations for 27mo Questions vs Directives to run a t-test.")
} else if (nrow(paired_data_27mo_complete) < 5) { # arbitrary small N warning
  print(paste("Warning: Very few (", nrow(paired_data_27mo_complete), ") paired observations for 27mo Questions vs Directives."))
  ttest_result_27mo_qd <- t.test(
    paired_data_27mo_complete$Questions,
    paired_data_27mo_complete$Directives,
    paired = TRUE,
    alternative = "greater" # H1: Proportion in Questions > Proportion in Directives
  )
  print(ttest_result_27mo_qd)
} else {
  ttest_result_27mo_qd <- t.test(
    paired_data_27mo_complete$Questions,
    paired_data_27mo_complete$Directives,
    paired = TRUE,
    alternative = "greater" # H1
  )
  print(ttest_result_27mo_qd)
}

################################################################
# CHI-SQUARE: REFLECTION OF RAW FREQUENCY OF SPEECH ACTS?
################################################################
# sanity check: get unique age groups
age_groups_to_test <- unique(all_mot_data_categorized$age_group)

# initialize a list to store results for each age group
results_by_age <- list()

for (current_age in age_groups_to_test) {
  
  cat(paste("\n--- Processing Age Group:", current_age, "---\n"))
  
  # filter data for the current age group
  data_current_age <- all_mot_data_categorized %>%
    filter(age_group == current_age)
  
  # calculate observed honorific counts and total category counts for this age group
  category_age_summary <- data_current_age %>%
    group_by(category) %>% # only need to group by category now, as age is filtered
    summarise(
      observed_honorific_count = sum(honorifics == 1, na.rm = TRUE),
      total_category_count = n(), # total utterances in this category for this age
      .groups = 'drop'
    ) %>%
    filter(total_category_count > 0) # remove categories not used at this age
  
  # check if there's enough data to proceed for this age group
  if(nrow(category_age_summary) < 2 || sum(category_age_summary$observed_honorific_count) == 0) {
    cat(paste("Skipping age group", current_age, "- insufficient categories or no honorifics observed.\n"))
    results_by_age[[current_age]] <- list(test_result = NULL, summary_table = category_age_summary, message = "Skipped - insufficient data")
    next # skip to the next age group
  }
  
  # calculate overall proportion of ALL utterances that are honorific for THIS AGE GROUP
  total_honorifics_current_age = sum(category_age_summary$observed_honorific_count)
  total_utterances_current_age = sum(category_age_summary$total_category_count)
  
  if (total_utterances_current_age == 0) { # should be caught by nrow check above, but just being extra careful :p
    cat(paste("Skipping age group", current_age, "- no utterances observed.\n"))
    results_by_age[[current_age]] <- list(test_result = NULL, summary_table = category_age_summary, message = "Skipped - no utterances")
    next
  }
  overall_honorific_proportion_current_age = total_honorifics_current_age / total_utterances_current_age
  
  cat(paste("Overall proportion of honorifics for age", current_age, ":", scales::percent(overall_honorific_proportion_current_age), "\n"))
  
  # calculate EXPECTED honorific count for each category for THIS AGE GROUP
  category_age_summary <- category_age_summary %>%
    mutate(
      expected_honorific_count = total_category_count * overall_honorific_proportion_current_age,
      # proportions of total utterances FOR THIS AGE (for the 'p' in chisq.test)
      proportion_of_total_utterances_age = total_category_count / total_utterances_current_age
    )
  
  cat("Summary Table for Chi-squared Goodness-of-Fit Test (Age:", current_age, "):\n")
  print(as.data.frame(category_age_summary))
  
  # check for low expected counts
  cat("Expected Honorific Counts (Age:", current_age, "):\n")
  print(category_age_summary$expected_honorific_count)
  low_expected_counts <- any(category_age_summary$expected_honorific_count < 5)
  if(low_expected_counts){
    cat(paste("Warning: Some expected honorific counts are less than 5 for age group", current_age, ". Chi-squared approximation may be less accurate.\n"))
  }
  
  # perform the Chi-squared Goodness-of-Fit Test
  observed_counts_vector_age <- category_age_summary$observed_honorific_count
  expected_proportions_vector_age <- category_age_summary$proportion_of_total_utterances_age
  
  test_output <- NULL
  if (length(observed_counts_vector_age) == length(expected_proportions_vector_age) && length(observed_counts_vector_age) > 1) {
    # try with simulation if expected counts are low
    simulate_p <- if(low_expected_counts) TRUE else FALSE
    
    # use tryCatch to handle potential errors if chisq.test fails (e.g., all expected are 0)
    test_output <- tryCatch({
      chisq.test(
        x = observed_counts_vector_age,
        p = expected_proportions_vector_age,
        simulate.p.value = simulate_p,
        B = if(simulate_p) 2000 else 0 # B is only used if simulate.p.value is TRUE
      )
    }, error = function(e) {
      cat(paste("Error performing Chi-squared test for age group", current_age, ":", e$message, "\n"))
      return(paste("Error:", e$message))
    })
    
    cat(paste("\nChi-squared Test Result for Age Group:", current_age, "\n"))
    print(test_output)
    
  } else {
    cat(paste("Cannot perform test for age group", current_age, "- mismatch in vector lengths or insufficient categories.\n"))
    test_output <- "Skipped - data issue"
  }
  
  # store results
  results_by_age[[current_age]] <- list(
    test_result = test_output, 
    summary_table = category_age_summary,
    message = if(low_expected_counts) "Warning: Low expected counts" else "OK"
  )
}

# access results for each age group:
# print(results_by_age[["8mo"]]$test_result)
# print(results_by_age[["8mo"]]$summary_table)
