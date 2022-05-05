# Visualisations of specific sets of verbs
# 
### -------------------- SETUP: load situations.csv and characters.csv -------###

library(tidyverse)

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

# Setup Characters the way you want it - drop the mutate() lines if you want to
# keep all the categories for RaceOrEthnicity.

# 1. Define Characters as the subset of AllCharacters that are not group characters or 
# customizable characters.
# 2. Convert "Unknown" values to NA. 
# 3. Simplify RaceorEthnicity and Species to make analysis easier.
# 4. Select relevant columns.

Characters <- AllCharacters %>% 
        filter(IsCustomizable == FALSE) %>% 
#       na_if("Unknown") %>% 
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
# to need.

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

# Add column stating whether verb is active or passive.
# Call this column "target" since this is the target or outcome we want to build
# a model to predict from the other variables. This variable is TRUE for active 
# verbs (ending in -ing) and FALSE for passive verbs (ending in -ed)

Situations <- Situations %>% 
        filter(!is.na(Verb))

# Create a dataframe where each row is an occurance of a character, technology
# or entity using a specific verb in a specific situation.

Verbs <- merge(Situations, Characters, by = "Character", all = TRUE)

# Add a column specifying if the verb is passive or active (-ing or -ed) and
# arrange the columns in a meaningful order.
Verbs <- Verbs %>% 
        mutate(VerbType = case_when(
                str_detect(Verb, "ing$") ~ "Active",
                str_detect(Verb, "ed$") ~ "Passive")) %>%
        add_count(Verb, name = "Verb_freq") %>% 
        relocate(Verb, Genre, VerbType, Verb_freq, SituationID, Technology, Entity, 
                Character, Species, Gender, Age, RaceOrEthnicity, Sexuality)

### ---------------------- VISUALISATIONS ------------------------####

# Two possible layouts for the character traits for a verb.

# Character traits for two specific verbs
# laid out using facet_grid, which allows me to set the bars to the same width
# but spreads the graph out a lot.

Verbs %>% 
        filter(Genre == "Game") %>% 
        filter(Verb == "Killed" | Verb == "Killing") %>% 
        rename(Race = RaceOrEthnicity) %>% 
        select(-c(SituationID, Verb_freq, VerbType, Technology, Entity, Character, Genre)) %>% 
        pivot_longer(!Verb, values_to = "value") %>%
        drop_na() %>% 
        ggplot(aes(x=factor(value), fill=Verb)) +
        geom_bar(width = 0.5, position = "dodge") +
        geom_text(stat = "count", 
                  aes(label =..count..), 
                  position = position_dodge(0.7),
                  vjust = -0.4) +
        labs(fill="", 
             title ="Character traits for specific actions in machine vision situations",
             y = "", 
             x = "") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45,    # angle labels for legibility
                                         hjust = TRUE), 
              panel.margin = unit(1.5, "line")) +         # more space between facets
        facet_grid(.~factor(name, levels = c("Genre", "Species", "Gender", "Age", 
                                             "Race", "Sexuality")), 
                            scales="free_x", space = "free")

        
Verbs %>% 
        filter(Verb == "Hunted" | Verb == "Hunting") %>% 
        select(-c(SituationID, Verb_freq, VerbType, Technology, Entity, Character)) %>% 
        pivot_longer(!Verb, values_to = "value") %>%
        drop_na() %>% 
        ggplot(aes(x=factor(value), fill=factor(Verb))) +
        scale_fill_manual(values=c("steelblue", "orangered1" )) +
        geom_bar(position="fill", alpha=.7)+
        theme(axis.line = element_line(colour = "darkblue", 
                                       size = 1, 
                                       linetype = "solid")) +
        theme_minimal() +
        labs(fill="", 
             title ="Verb",
             y = "", 
             x = "") +
        theme(axis.text.x = element_blank()) +
        coord_flip() +
        facet_wrap(~name, scales="free")


# This generates stacked barchats showing how many times a specific
# entity or technology uses specific verbs. The example is set to two 
# verbs but more can be added by adding more of the OR Verb IS EQUAL TO 
# "whatever" clauses.
# 
# | Verb == "Something" 

Verbs %>% 
        select(Verb, Technology, Entity) %>% 
        # filter(Genre == "Art")  # use this line to only see a specific genre
        filter(Verb == "Killing" | Verb == "Killed")  %>% 
        pivot_longer(!Verb, values_to = "value") %>%
        drop_na() %>% 
        ggplot(aes(x=factor(value), fill=Verb)) +
        geom_bar() +
        geom_text(stat = "count", 
                  aes(label =..count..), 
                  position = position_stack(vjust = 0.5)) +
        labs(fill="", 
             title ="Specific actions by entities and technologies in machine vision situations",
             y = "", 
             x = "") +
        theme_minimal() +
        coord_flip() +
        facet_wrap(~name, scales="free_y")

