# Load packages
library(dplyr)
library(car)

# Read dataset
data <- read.csv("experiment_stimuli_dataframe (final).csv")

### preparation

# Filter for words only
words <- data %>% filter(Condition == "word")

# Convert Emotional_valence_cat to factor
words$Emotional_valence_cat <- factor(words$Emotional_valence_cat, levels = c("positive", "neutral", "negative"))

# List of numeric columns to clean
numeric_cols <- c("Length", "Lg10SUBTLEX_US", "Emotional_valence_cont")

# Convert columns to numeric, coercing "None" to NA
words <- words %>%
  mutate(across(all_of(numeric_cols), ~ as.numeric(as.character(.))))

# Remove rows with NA for each dependent variable before ANOVA
words_length <- words %>% filter(!is.na(Length))
words_freq   <- words %>% filter(!is.na(Lg10SUBTLEX_US))
words_val    <- words %>% filter(!is.na(Emotional_valence_cont))


### tests

# 1. Length
length_aov <- aov(Length ~ Emotional_valence_cat, data = words_length)
summary(length_aov)
leveneTest(Length ~ Emotional_valence_cat, data = words_length)
TukeyHSD(length_aov)

# 2. Frequency
freq_aov <- aov(Lg10SUBTLEX_US ~ Emotional_valence_cat, data = words_freq)
summary(freq_aov)
leveneTest(Lg10SUBTLEX_US ~ Emotional_valence_cat, data = words_freq)
TukeyHSD(freq_aov)

# 3. Continuous emotional valence
valence_aov <- aov(Emotional_valence_cont ~ Emotional_valence_cat, data = words_val)
summary(valence_aov)
leveneTest(Emotional_valence_cont ~ Emotional_valence_cat, data = words_val)
TukeyHSD(valence_aov)


# anonva tests
oneway.test(Lg10SUBTLEX_US ~ Emotional_valence_cat, data = words_freq, var.equal = FALSE)
oneway.test(Length ~ Emotional_valence_cat, data = words_freq, var.equal = FALSE)
oneway.test(Emotional_valence_cont ~ Emotional_valence_cat, data = words_freq, var.equal = FALSE)


# takeaway:
# no issues with frequency
# no issues with continual valence (to be expected, but still good to check)
# small issue with word length: negative vs neutral: p = 0.049 -> significant difference: negative words are slightly longer than neutral words, no other contrasts are significant

# additionally:
# Pairwise t-tests for Length
pairwise.t.test(words_length$Length, 
                words_length$Emotional_valence_cat, 
                p.adjust.method = "holm",   # controls for multiple comparisons
                pool.sd = TRUE)             # use pooled SD (standard t-test)






# both words and nonwords:


# Convert Condition to factor
length_data$Condition <- factor(length_data$Condition, levels = c("word", "nonword"))



# standard t-test length of words vs nonwords
t_test_length_equal <- t.test(Length ~ Condition, data = length_data, var.equal = TRUE)
t_test_length_equal

# statistically signifcant difference in length between words and nonwords: nonwords tend to be slightly longer than words


# todo: standard t test for frequency (only did anova test, but that was fine so we can assume ttest will be fine as well) - just for analytical symmetry






