## MOT-CHI naturalistic dyadic interactions coded for speech acts (INCA-A)
each subdirectory 8mo, 13mo, and 27mo contain the following:
- original .cha files downloaded from Talkbank Ko corpus
- under the `coded` subdirectory within each age group directory, .txt files of speech-act-coded interactions
  - e.g. 47	MOT	CL	이름야 . 17485_18123
  - line_num_in_transcript  INCA-A_code  transcription  timecode_in_rec

## Scripts
### kor_honorifics_df_constr.R
- USE: iterates through base directory with `age_group\coded` subdirectories to create dataframe for plotting and analyses

### kor_honorifics_plots_analyses.R
- USE: given a `combined_data` dataframe in the environment (output from kor_honorifics_df_constr.R), run descriptive stats, stat tests, and create plots
- OUTPUT:
  - descriptive stats:
    - average and SD values for number of utterances per minute by age group, MOT and CHI
    - average recording length
    - interaction duration by addressee
    - average honorifics proportions by age group
  - graphs:
    - PLOT 1: COMPARISON OF CHILD VS. EXP. VS. FAMILY-DIRECTED HONORIFICS USAGE
      - bar graph of proportion of honorific utterances per speaker
    - PLOT 2: SPEECH ACT CODES FOR HONORIFIC UTTERANCES IN CDS
      - within each speech act, what percentage of it was marked with honorifics, shown across age group
      - line graph faceted by category
    - PLOT 3: 100% STACKED BAR CHART FOR DIST OF SPEECH ACTS
      - given all honorifics-marked utterances for an age group, what are their distributions across speech act categories?
  - stat tests:
    - PAIRED T-TEST: is cds honorifics > family-directed honorifics across participants?
    - PAIRED T-TEST: proportion of questions vs. directives at 27mo
    - CHI-SQUARE: does the distribution of honorifics utterances simply mirror the raw frequency of speech acts across all utterances?
