# R scripts for the paper "Algorithmic Failure to Predict as a Humanities Methodology"
# by Jill Walker Rettberg
# 28 Feb 2022
# 
# Released under a CC-BY license. 
# 
# This version of the scripts fetches the datasets from Github. Before 
# publication of the commentary these links will be changed to 
# the official version of the dataset archived in Dataverse, where it is awaiting 
# review before publication.
# 
# Requires the packages tidyverse and class - install these packages if they're 
# not already installed.
# 
# install.packages(tidyverse)
# install.packages(class)

library(tidyverse)

# LOAD DATA ---------------------------------------------------------------

#Import characters file (../data/Characters.csv)
#define column types and factors
AllCharacters <- read_csv(
        "https://github.com/jilltxt/HumansRobotsAndMachineVision/raw/main/data/characters.csv",
        col_types = cols(
                CharacterID = col_integer(),
                Character = col_character(),
                Species = col_factor(levels = c(
                        "Animal", "Cyborg", "Fictional", 
                        "Human", "Machine", "Unknown")),
                Gender = col_factor(levels = c(
                        "Female","Male","Non-binary or Other", "Trans Woman",
                        "Unknown")),
                RaceOrEthnicity = col_factor(levels = c(
                        "Asian", "Black", "Person of Colour", "White", "Immigrant", "Indigenous",
                        "Complex", "Unknown")),
                Age = col_factor(levels = c(
                        "Child", "Young Adult", "Adult", "Elderly", 
                        "Unknown")),
                Sexuality = col_factor(levels = c(
                        "Homosexual", "Heterosexual", "Bi-sexual", "Other",
                        "Unknown")),
                IsGroup = col_logical(),
                IsCustomizable = col_logical()
        )
)

# 1. Define Characters as the subset of AllCharacters that are not group characters or 
# customizable characters.
# 2. Convert "Unknown" values to NA. 
# 3. Simplify RaceorEthnicity and Species to make analysis easier.
# 4. Select relevant columns.

Characters <- AllCharacters %>% 
        filter(IsCustomizable == FALSE) %>% 
        na_if("Unknown") %>% 
        select(Character, Species, Gender, Sexuality, 
               RaceOrEthnicity, Age) %>% 
        mutate(RaceOrEthnicity = recode(RaceOrEthnicity,  
                                        "Asian" = "Asian", 
                                        "Black" = "PoC", 
                                        "White" = "White", 
                                        "Person of Colour" = "PoC",
                                        "Indigenous" = "PoC",
                                        "Immigrant" = "PoC",
                                        "Complex"  = "PoC")) %>% 
        mutate(Species = recode(Species,
                                "Human" = "Human",
                                "Machine" = "Robot",
                                "Cyborg" = "Robot",
                                "Fictional" = "Fictional",
                                "Animal" = "Animal"))


# To figure out what these characters actually do with the machine vision we need 
# to load data about the Situations in which they interact with machine vision
# technologies in the creative works in our sample.
# 
# The following code imports data about the Situations from situations.csv, 
# sets the column types, and also tells R to skip the columns we’re not going 
# to need for this analysis.

Situations <- read_csv(
        "https://raw.githubusercontent.com/jilltxt/HumansRobotsAndMachineVision/main/data/situations.csv",
        col_types = cols(
                SituationID = col_integer(),
                SituationTitle = col_skip(),
                Genre = col_character(),
                Character = col_character(),
                Entity = col_character(),
                Technology = col_character(),
                Verb = col_character()
        )
)


# ADD ACTIVE/PASSIVE VARIABLE ---------------------------------------------


# Add column stating whether verb is active or passive.
# Call this column "target" since this is the target or outcome we want to build
# a model to predict from the other variables. This variable is TRUE for active 
# verbs (ending in -ing) and FALSE for passive verbs (ending in -ed)

Situations <- Situations %>% 
        mutate(target = (str_detect(Verb, "ing"))) %>% 
        filter(!is.na(Verb))

# Situations includes data about actions taken by entities (e.g. law
# enforcemnet, corporations) and by technologies. Those rows have misssing data
# (NA) in the Character variable as they're not referencing a character.
# For this analysis, we are only interested in actions taken by characters, so 
# will delete rows where the Character is NA.

Character_situations <- Situations %>% 
        select(SituationID, Genre, Character, Verb, target) %>% 
        filter(!is.na(Character))


# MERGE CHARACTERS AND SITUATIONS TO CREATE VERB TABLE --------------------

# Now we combine the two dataframes using the Character 
# column as the shared information.
Character_verbs <- merge(
        x = Character_situations, y = Characters, 
        by = "Character") %>% 
        select(Character, SituationID, Genre, Verb, Species, Gender, 
               RaceOrEthnicity, Age, Sexuality, target)


# TRANSFORM TO CONTINGENCY TABLE --------------------------------------------------

# Make a contingency table for the Character_verbs.
# If using existing data from a content analysis you might be able to start here.

Character_verbs_contingency <- Character_verbs %>% 
        select(Verb, Gender, Species, RaceOrEthnicity, Age, Sexuality) %>% 
        pivot_longer(cols= -Verb,
                     names_to = "variable", 
                     values_to = "value") %>% 
        drop_na() %>% 
        group_by(Verb, value) %>%
        summarise(n=n()) %>% 
        pivot_wider(names_from = "value", values_from = "n") %>% 
        mutate_all(~replace(., is.na(.), 0)) %>%  # convert NA to 0 since it's count 
        mutate(target = str_detect(Verb, "ing"), .after = Verb) # new col target


# VISUALISE DISTRIBUTION OF TRAITS IN DATASET -----------------------------

# Fig 1 - distribution of action proportional by trait --------------------

# Plot barcharts showing the proportion of active and passive verbs for each
# character trait. 
Character_verbs %>% select(Genre, Species, Gender, 
                           RaceOrEthnicity, Age, Sexuality, target) %>% 
        pivot_longer(!target, values_to = "value") %>%
        drop_na() %>% 
        ggplot(aes(x=factor(value), fill=factor(target))) +
        scale_fill_manual(name="Action type",
                values=c("steelblue", "orangered1"),
                labels=c("Passive", "Active")) +
        geom_bar(position="fill", alpha=.7)+
        theme( axis.line = element_line(colour = "darkblue", 
                                        size = 1, linetype = "solid")) +
        theme_minimal() +
        labs(title ="Characters' interactions with machine vision technologies",
             subtitle="(Proportional)",
             y = "", 
             x = "") +
        theme(axis.text.x = element_blank()) +
        coord_flip() +
        facet_wrap(~name, scales="free")


# Fig2 - Distribution by count --------------------------------------------

# Same but using count, not proportion, which shows that some categories have small numbers. 

Character_verbs %>% select(Genre, Species, Gender, 
                           RaceOrEthnicity, Age, Sexuality, target) %>% 
        pivot_longer(!target, values_to = "value") %>%
        drop_na() %>% 
        ggplot(aes(x=factor(value), fill=factor(target))) +
        scale_fill_manual(name="Action type",
                          values=c("steelblue", "orangered1"),
                          labels=c("Passive", "Active")) +
        geom_bar(position="fill", alpha=.7)+
        theme( axis.line = element_line(colour = "darkblue", 
                                        size = 1, linetype = "solid")) +
        theme_minimal() +
        labs(title ="Character interactions with machine vision technologies",
             subtitle="(Absolute numbers)",
             y = "", 
             x = "") +
        theme(axis.text.x = element_blank()) +
        coord_flip() +
        facet_wrap(~name, scales="free")


# NORMALISE VALUES --------------------------------------------------------


library(class)

# Define normalize function (from Lantz p. 80)
# This normalises the values so they are all between 0 and 1.

normalise <- function(x) {
        return ((x - min(x)) / (max(x) - min(x)))
}

# convert the Verb column to rownames, since the normalising needs all numeric data 
# variables - but rownames are fine and used as labels in the plot.
Character_verbs_contingency_rownames <- Character_verbs_contingency %>% 
        column_to_rownames(var = "Verb")

# Create a new version of the contingency table with normalised values. 
#Character_verbs_contingency_norm <- as.data.frame(lapply(Character_verbs_contingency_temp, normalise))


# SPLIT DATASET INTO TRAINING AND TEST SUBSET -----------------------------

# Setting the seed so s the "random" rows selected
# will be the same each time if we run the prediction multiple times. 
set.seed(2022)

# Split dataset into subsets for training and testing.
split <- sample(1:nrow(Character_verbs_contingency),
                as.integer(0.7*nrow(Character_verbs_contingency)), F)
train <- Character_verbs_contingency_rownames[split,]
test <- Character_verbs_contingency_rownames[-split,]

# Storing a list of the actual verbnames in the order they are in the test data
# so that later we can add them back in and see which verb was predicted active
# or passive.
verbnames <- rownames(test)

# Normalise the data in train and test (do this AFTER the split so I know what 
# the row numbers are and can add the test_verbs back in)
train <- as.data.frame(lapply(train, normalise))
test <- as.data.frame(lapply(test, normalise))


# RUN THE kNN ALGORITHM ---------------------------------------------------

# This is the actual machine learning.
# 
# format:
# prediction <- knn(training data, test data, class to try to predict, how many k's)
# 
# Remove first two columns (the verb and whether or not it is active) from both the 
# training and test subsets.

prediction <- knn(train = train[-c(1:2)], test = test[-c(1:2)], cl = train$target, k=1)


# ADD PREDICTIONS TO ORIGINAL DATA ----------------------------------------

# Now we have the predictions we can add them to the original contingency table and 
# the more detailed data about the characters and verbs to see the differences. 
#
# Add the predictions to the subset of the original data we used for the testing.
# This has 225 verbs.
# 
verbs <- cbind(test, prediction)
# Add the verbnames back in.
verbs$Verb <- verbnames

# Find false passives (i.e. verbs the prediction thought were passive (ending in -ed)
# but that were really active)

Verb_pred <- verbs %>% 
        mutate(Prediction_type = case_when(target == 1 & prediction == 0 ~ "False Passive",
                                           target == 0 & prediction == 1 ~"False Active",
                                           target == 0 & prediction == 0 ~ "Accurate",
                                           target == 1 & prediction == 1 ~ "Accurate",
                                           TRUE ~ "Other")) %>% 
        select(Verb, prediction, Prediction_type)

# Merge predictions with the full Character_verbs table.
Verb_pred1 <- merge(Verb_pred, Character_verbs, by = "Verb")

# But Character_verbs includes the training dataset, and we only have predictions
# for the 225 verbs in the test dataset. So we'll remove the verbs that don't 
# have predictions even though this reduces our dataset a lot. 

Character_verb_predictions <- Verb_pred1 %>% 
        drop_na(prediction) 

# Add frequency count to compare predictions to how often an action is taken.

pred <- Character_verb_predictions %>% 
        add_count(Verb, name = "Count") %>% 
        select(Verb, Count, Prediction_type, target) %>% 
        distinct() %>% 
        arrange(desc(Count))

summary(pred)
# shows that 75% of the actions are used 4 or less times. 

# VISUALISE TRAITS OF UNPREDICTABLE ACTIONS ------------------------------------

# fig3: false predictions proportional ------------------------------------

# Plot barcharts showing the proportion of mispredicted  verbs for each
# character trait. 
# 
Character_verb_predictions %>% 
        filter(Prediction_type != "Accurate") %>% 
        select(Genre, Species, Gender, 
               RaceOrEthnicity, Age, Sexuality, Prediction_type) %>% 
        pivot_longer(!Prediction_type, values_to = "value") %>%
        drop_na() %>% 
        ggplot(aes(x=factor(value), fill=factor(Prediction_type))) +
        scale_fill_manual(values=c("steelblue", "orangered1" )) +
        geom_bar(position="fill", alpha=.7)+
        theme(axis.line = element_line(colour = "darkblue", 
                                       size = 1, 
                                       linetype = "solid")) +
        theme_minimal() +
        labs(fill="Predicted action:", 
             title ="Traits of characters whose actions were falsely predicted",
             subtitle="(Proportional)",
             y = "", 
             x = "") +
        theme(axis.text.x = element_blank()) +
        coord_flip() +
        facet_wrap(~name, scales="free")


# fig4: False predictions count -------------------------------------------

Character_verb_predictions %>% 
        filter(Prediction_type != "Accurate") %>% 
        select(Genre, Species, Gender, 
               RaceOrEthnicity, Age, Sexuality, Prediction_type) %>% 
        pivot_longer(!Prediction_type, values_to = "value") %>%
        drop_na() %>% 
        ggplot(aes(x=factor(value), fill=factor(Prediction_type))) +
        scale_fill_manual(values=c("steelblue", "orangered1" )) +
        geom_bar(position="dodge", alpha=.7)+
        theme(axis.line = element_line(colour = "darkblue", 
                                       size = 1, 
                                       linetype = "solid")) +
        theme_minimal() +
        labs(fill="Predicted action:", 
             title ="Traits of characters whose actions were falsely predicted",
             subtitle = "(Absolute numbers)",
             y = "", 
             x = "") +
        theme(axis.text.x = element_blank()) +
        coord_flip() +
        facet_wrap(~name, scales="free")


# LIST CORRECTLY PREDICTED VERBS -----------------------------------------------

Character_verb_predictions %>%  
        filter(Prediction_type == "Accurate") %>% 
        group_by(Verb) %>% 
        summarise(Count=n()) %>% 
        arrange(desc(Count))


# LIST FALSE PASSIVES ------------------------------------------------------

Character_verb_predictions %>%  
        filter(Prediction_type == "False Passive") %>% 
        group_by(Verb) %>% 
        summarise(Count=n()) %>% 
        arrange(desc(Count))


# LIST FALSE ACTIVES ------------------------------------------------------

Character_verb_predictions %>%  
        filter(Prediction_type == "False Active") %>% 
        group_by(Verb) %>% 
        summarise(Count=n()) %>% 
        arrange(desc(Count))
