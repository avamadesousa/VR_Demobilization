# Prep data

#load both datafiles (sona + umiami panel)
sona_path <- file.path(project_root, "data", "VR_CA_Sona_Dec2025_UCSB_RAW.csv")
panel_path <- file.path(project_root, "data", "VR_Raw2_UMiami.csv")

# read every col as char
sona_raw <- read.csv(sona_path, colClasses = "character", na.strings = c("", "NA"),
                     check.names = TRUE, stringsAsFactors = FALSE)
panel_raw <- read.csv(panel_path, colClasses = "character", na.strings = c("", "NA"),
                      check.names = TRUE, stringsAsFactors = FALSE)


# save counts at each filtering step
sample_flow <- data.frame(source = character(), step = character(), n = integer())
record_count <- function(data, source_name, step_name) {
  sample_flow <<- rbind(sample_flow, data.frame(
    source = source_name, step = step_name, n = nrow(data)
  ))
}

sona <- sona_raw %>% filter(grepl("^R_", ResponseId))
record_count(sona, "UCSB SONA", "Participant records in export")
sona <- sona %>% filter(Progress == "100", consent == "1")
record_count(sona, "UCSB SONA", "Complete and consented")

# split multi-select race codes into whole tokens so different numbers aren't confused
has_code <- function(x, code) {
  vapply(strsplit(ifelse(is.na(x), "", x), ",", fixed = TRUE),
         function(tokens) as.character(code) %in% trimws(tokens), logical(1))
}
sona <- sona %>% mutate(
  asian_selected = has_code(race.ethnicity, 3),
  latine_selected = has_code(race.ethnicity, 4),
  black_selected = has_code(race.ethnicity, 2)
)
sona <- sona %>% filter(asian_selected | latine_selected | black_selected)
record_count(sona, "UCSB SONA", "At least one focal group selected")
sona <- sona %>% filter(asian_selected + latine_selected + black_selected == 1)
record_count(sona, "UCSB SONA", "Exactly one focal group selected")
sona <- sona %>% filter(Att_Check1 == "1", Att_Check2 == "1")
record_count(sona, "UCSB SONA", "Pass both attention checks")
sona <- sona %>% mutate(
  group = case_when(black_selected ~ "Black", latine_selected ~ "Hispanic/Latine",
                    asian_selected ~ "Asian"),
  recruitment = "UCSB SONA"
)

panel <- panel_raw %>% filter(grepl("^R_", ResponseId))
record_count(panel, "UM panel", "Participant records in export")
panel <- panel %>% filter(Progress == "100", consent == "Yes, I do consent.")
record_count(panel, "UM panel", "Complete and consented")
panel <- panel %>% mutate(
  black_selected = !is.na(race.ethnicity_2) & race.ethnicity_2 == "Black/African American",
  latine_selected = !is.na(race.ethnicity_4) & race.ethnicity_4 == "Hispanic/Latine",
  asian_selected = !is.na(race.ethnicity_3) & race.ethnicity_3 == "Asian or Pacific Islander"
)
panel <- panel %>% filter(black_selected | latine_selected | asian_selected)
record_count(panel, "UM panel", "At least one focal group selected")
panel <- panel %>% filter(
  Att_Check1 == "Witnessing or hearing about online discrimination",
  Att_Check2 == "TRUE"
)
record_count(panel, "UM panel", "Pass both attention checks")
panel <- panel %>% filter(asian_selected + latine_selected + black_selected == 1)
record_count(panel, "UM panel", "Exactly one focal group selected")
panel <- panel %>% mutate(
  group = case_when(black_selected ~ "Black", latine_selected ~ "Hispanic/Latine",
                    asian_selected ~ "Asian"),
  recruitment = "UM panel"
)
write.csv(sample_flow, file.path(output_dir, "sample_flow.csv"), row.names = FALSE)



# Specify every item used by these two analyses
# types asked for four targets. Only suffix _1 (past year) is used.
vr_events <- c("Joke", "Belong", "Slight", "Unfair", "Threat", "Actual",
               "Killed", "Inst", "PolThreat", "PolActual", "PolKill")
vr_items <- unlist(lapply(c("Friend", "Fam", "Stranger", "Online"), function(target) {
  names <- paste0("VR_", target, "_", vr_events, "_1")
  # Qualtrics abbreviated Stranger to Strang for the three police items.
  sub("VR_Stranger_Pol", "VR_Strang_Pol", names, fixed = TRUE)
}), use.names = FALSE)
past_items <- paste0("CA_Past_", 1:7)
future_items <- paste0("CA_Future_", 1:7)
identity_items <- paste0("GroupID_", 1:4)
all_items <- c(vr_items, past_items, future_items, identity_items)
stopifnot(length(vr_items) == 44, !anyDuplicated(all_items))

# Exact response labels, in scoring order
vr_labels <- c("Never", "Once", "A few times", "Often", "Very often", "Almost always")
past_labels <- c("Never", "Rarely", "Occasionally", "Sometimes", "Frequently",
                 "Very Often", "Extremely Often")
future_labels <- c("No, definitely not", "No, very unlikely", "No, somewhat unlikely",
                   "Neutral / Unsure", "Yes, somewhat likely", "Yes, very likely", "Yes, definitely")
identity_labels <- c("Strongly disagree", "Disagree", "Somewhat disagree",
                     "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree")

score_response <- function(x, labels) {
  cleaned <- tolower(trimws(x))
  cleaned[cleaned %in% c("", "na")] <- NA_character_
  label_scores <- match(cleaned, tolower(labels))
  numeric_scores <- suppressWarnings(as.numeric(cleaned))
  is_valid_number <- !is.na(numeric_scores) & numeric_scores %in% seq_along(labels)
  scores <- as.numeric(label_scores)
  scores[is_valid_number] <- numeric_scores[is_valid_number]
  unknown <- !is.na(cleaned) & is.na(scores)
  if (any(unknown)) stop("Unrecognized response: ", paste(unique(x[unknown]), collapse = "; "))
  scores
}

score_items <- function(data) {
  stopifnot(all(all_items %in% names(data)))
  # The malformed label is the sixth response option in the export.
  data$VR_Fam_PolKill_1[!is.na(data$VR_Fam_PolKill_1) &
                          trimws(data$VR_Fam_PolKill_1) == "Click to write Scale Point 6"] <- "6"
  for (item in vr_items) data[[item]] <- score_response(data[[item]], vr_labels)
  for (item in past_items) data[[item]] <- score_response(data[[item]], past_labels)
  for (item in future_items) data[[item]] <- score_response(data[[item]], future_labels)
  for (item in identity_items) data[[item]] <- score_response(data[[item]], identity_labels)
  data$GroupID_1 <- 8 - data$GroupID_1
  data$GroupID_3 <- 8 - data$GroupID_3
  data
}
sona <- score_items(sona)
panel <- score_items(panel)

# Numeric gender codes in SONA; text labels in the UM panel export.
# Retain all reported categories. Do not recode everyone other than women as
# men, or assign a nonresponse to a gender category.
gender_labels <- c("Male", "Female", "Non-binary", "Genderqueer", "Genderfluid",
                   "Intersex", "Transgender male", "Transgender female",
                   "Prefer not to say", "Other")
sona$gender <- factor(score_response(sona$gender, gender_labels),
                      levels = seq_along(gender_labels), labels = gender_labels)
panel$gender <- factor(score_response(panel$gender, gender_labels),
                       levels = seq_along(gender_labels), labels = gender_labels)

# Harmonize the other demographic fields for the descriptive audit.
# These are single-choice fields; frequency counts must sum to the group N.
demographic_labels <- list(
  edu = c("Middle school level or equivalent", "Highschool diploma/GED", "Some college",
          "Bachelor’s or equivalent", "Master’s or equivalent", "PhD or equivalent"),
  fam_income = c("$0-$9,999", "$10,000-$24,999", "$25,000-$49,999", "$50,000-$74,999",
                 "$75,000-$99,999", "$100,000-$149,999", "$150,000+", "Prefer not to answer"),
  Gen_Stat = c("As 1st generation", "As 2nd generation", "As 3rd generation",
               "More than 3rd generation", "Unknown", "Other (please indicate):"),
  US_Live = c("All my life", "Since I was a toddler (2 – 5 years)",
              "Since I was a child (5 – 12 years)", "As a teenager (13 – 19 years)",
              "As a young adult (20 – 30 years)", "As an adult (over 30 years)")
)

# harmonize typographic differences only
normalize_text <- function(x) {
  x <- trimws(x)
  x <- gsub("â€™", "'", x, fixed = TRUE)
  x <- gsub("â€“", "-", x, fixed = TRUE)
  x <- gsub("[’‘]", "'", x)
  x <- gsub("[–—]", "-", x)
  x <- gsub("\\s+", " ", x)
  x
}
harmonize_demographic <- function(x, labels) {
  x <- normalize_text(x)
  labels <- normalize_text(labels)
  index <- match(x, as.character(seq_along(labels)))
  x[!is.na(index)] <- labels[index[!is.na(index)]]
  x
}
for (variable in names(demographic_labels)) {
  sona[[variable]] <- harmonize_demographic(sona[[variable]], demographic_labels[[variable]])
  panel[[variable]] <- harmonize_demographic(panel[[variable]], demographic_labels[[variable]])
  observed <- unique(c(sona[[variable]], panel[[variable]]))
  unexpected <- setdiff(observed[!is.na(observed)], normalize_text(demographic_labels[[variable]]))
  if (length(unexpected) > 0) stop("Unmatched demographic labels for ", variable, ": ",
                                   paste(unexpected, collapse = "; "))
}

keep_columns <- c("ResponseId", "group", "recruitment", "age", "gender",
                  names(demographic_labels), "asian_selected", "black_selected",
                  "latine_selected", all_items)
study_data <- bind_rows(sona[, keep_columns], panel[, keep_columns])
study_data$age <- suppressWarnings(as.numeric(study_data$age))
study_data$group <- factor(study_data$group, levels = c("Black", "Hispanic/Latine", "Asian"))
study_data$recruitment <- factor(study_data$recruitment, levels = c("UM panel", "UCSB SONA"))
stopifnot(!anyNA(study_data$ResponseId), !anyDuplicated(study_data$ResponseId))
stopifnot(all(study_data$age[!is.na(study_data$age)] >= 18),
          all(study_data$age[!is.na(study_data$age)] <= 100))

# preserve  original available-item mean rule for the primary reanalysis.
scale_mean <- function(items, minimum_answered = 1) {
  n_answered <- rowSums(!is.na(items))
  score <- rowMeans(items, na.rm = TRUE)
  score[n_answered < minimum_answered] <- NA_real_
  score
}
scale_items <- list(lifetime_ca = past_items, future_ca = future_items,
                    past_year_vr = vr_items, identification = identity_items)
for (scale_name in names(scale_items)) {
  items <- study_data[, scale_items[[scale_name]], drop = FALSE]
  study_data[[scale_name]] <- scale_mean(items)
  study_data[[paste0(scale_name, "_answered")]] <- rowSums(!is.na(items))
}
stopifnot(identical(as.integer(table(study_data$group)), c(433L, 402L, 285L)))

# cleaned data export
analysis_data <- study_data %>% select(-ResponseId)
write.csv(analysis_data, file.path(output_dir, "complete_cleaned_data.csv"), row.names = FALSE)
