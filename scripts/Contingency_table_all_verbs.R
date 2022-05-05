# 23 Feb 2022

# This makes a contingency table of Character_verbs
# Each value (e.g. Female or Human) becomes a column name and the value
# is the count for each verb. 

suppressMessages(library(tidyverse))

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

# Make contingency table SitCounts with count of times each verb is used by 
# a Character, a Technology or an Entity. Then add a column VerbType stating 
# whether Verb is active or passive.

SitCounts <- Situations %>% 
        mutate(Tech = !is.na(Technology),
               Char = !is.na(Character),
               Ent = !is.na(Entity)) %>% 
        select(Verb, Tech, Char, Ent) %>%
        pivot_longer(-Verb, names_to = "var", values_to = "value") %>% 
        count(Verb, var, wt = value) %>% 
        pivot_wider(Verb, names_from = var, values_from = n) %>% 
        mutate(VerbType = case_when(
                        str_detect(Verb, "ing$") ~ "Active",
                        str_detect(Verb, "ed$") ~ "Passive"))

write_csv(SitCounts, "data/situation_counts.csv")


#Import characters file
#define column types and factors
Orig_Characters <- read_csv(
        "https://raw.githubusercontent.com/jilltxt/HumansRobotsAndMachineVision/main/data/characters.csv",
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
        

# Change some of the variables to simplify for analysis. 
# 
# Format here is:
# mutate(New_column_name = recode(Old_column_name, "New value" = "Old value"))
# 
# Could remove customizable characters with this line:
#         filter(IsCustomizable == FALSE) %>% 
# 
# Convert "Unknown" values to NA. 
# 
# Select relevant columns.

Characters <- Orig_Characters %>% 
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

# Merge these new columns back into Situations.

Verbs <- merge(Situations, Characters, by = "Character", all = TRUE)


#Make a contingency table for the Verbs
Tech_verbs_contingency <- Verbs %>% 
        filter(!is.na(Technology)) %>% 
        select(Verb, Genre, Technology) %>% 
        pivot_longer(cols= -Verb,
                     names_to = "variable", 
                     values_to = "value") %>% 
        group_by(Verb, value) %>%
        summarise(n=n()) %>% 
        pivot_wider(names_from = "value", values_from = "n") %>% 
        mutate_all(~replace(., is.na(.), 0)) %>%  # convert NA to 0 since it's count 
        mutate(target = str_detect(Verb, "ing"), .after = Verb) %>% # new col target
        relocate(Verb, target, Art, Game, Narrative) #put these cols first


Entity_verbs_contingency <- Verbs %>% 
        filter(!is.na(Entity)) %>% 
        select(Verb, Genre, Entity) %>% 
        pivot_longer(cols= -Verb,
                     names_to = "variable", 
                     values_to = "value") %>% 
        group_by(Verb, value) %>%
        summarise(n=n()) %>% 
        pivot_wider(names_from = "value", values_from = "n") %>% 
        mutate_all(~replace(., is.na(.), 0)) %>%  # convert NA to 0 since it's count 
        mutate(target = str_detect(Verb, "ing"), .after = Verb) %>%  # new col target
        relocate(Verb, target, Art, Game, Narrative)

Character_verbs_contingency <- Verbs %>% 
        filter(!is.na(Character)) %>% 
        select(Verb, Gender, Species, RaceOrEthnicity, Age, Sexuality) %>%
        pivot_longer(cols= -Verb,
                     names_to = "variable", 
                     values_to = "value") %>% 
        group_by(Verb, value) %>%
        summarise(n=n()) %>% 
        pivot_wider(names_from = "value", values_from = "n") %>% 
        mutate_all(~replace(., is.na(.), 0)) %>%  # convert NA to 0 since it's count 
        mutate(target = str_detect(Verb, "ing"), .after = Verb) %>%  # new col target 
        relocate()

write_csv(Tech_verbs_contingency, "data/tech_verbs_contingency.csv")
write_csv(Character_verbs_contingency, "data/character_verbs_contingency.csv")
write_csv(Entity_verbs_contingency, "data/entity_verbs_contingency.csv")

## --- ADDING WORK-LEVEL INFORMATION -- ##

creativeworks <- read_csv(
        "https://raw.githubusercontent.com/jilltxt/HumansRobotsAndMachineVision/main/data/creativeworks.csv")

works_contingency <- creativeworks %>% 
        select(WorkID, Genre, Country, Sentiment, Topic, 
               TechRef, TechUsed) %>% 
        distinct() %>% 
        pivot_longer(cols= -WorkID,
                     names_to = "variable", 
                     values_to = "value") %>% 
        group_by(WorkID, value) %>%
        summarise(n=n()) %>% 
        pivot_wider(names_from = "value", values_from = "n") %>% 
        mutate_all(~replace(., is.na(.), 0))  # convert NA to 0 since it's count 


## --- correlation matrix - but this isn't very useful --- ###

# The correlation matrix works on this contingency table.
# Needed to change all NAs to 0 for cor() to work - makes sense
# because NA means no incidences of this so should be zero.
cormat <- cor(Entity_verbs_contingency %>% keep(is.numeric))

cormat %>% 
        as.data.frame %>% 
        mutate(var2=rownames(.)) %>%
        pivot_longer(!var2, values_to = "value") %>%
        ggplot(aes(x=name,y=var2,fill=abs(value),label=round(value,2))) +
        geom_tile() + geom_label() + xlab("") + ylab("") +
        ggtitle("Correlation matrix of our predictors") +
        labs(fill="Correlation\n(absolute):") +
        theme(axis.text.x = element_text(angle=90))


# The next bit does something similar but making percentages.
# 
# Tutorial here: https://bookdown.org/paul/applied-data-visualization/categorical-variables-2.html

# This makes a table of all the verbs with percentages for each value in each 
# character trait. 

temp <- Character_verbs %>% 
        select(Verb, Gender, Species, RaceOrEthnicity, Age, Sexuality) %>% 
        pivot_longer(cols= -Verb,
                names_to = "variable", 
                values_to = "value") %>% 
        drop_na() %>% 
        group_by(Verb) %>%
        
        summarize(pct_female = mean(value == "Female", na.rm=TRUE),
                  pct.male = mean(value == "Male", na.rm=TRUE),
                  pct.trans = mean(value == "Trans Woman", na.rm=TRUE),
                  pct.human = mean(value == "Human", na.rm=TRUE),
                  pct.robot = mean(value == "Machine"|value=="Cyborg", na.rm=TRUE),
                  pct.animal = mean(value == "Animal", na.rm=TRUE),
                  pct.fictional = mean(value == "Fictional", na.rm=TRUE),
                  pct.asian = mean(value == "Asian", na.rm=TRUE),
                  pct.white = mean(value == "White", na.rm=TRUE),
                  pct.black = mean(value == "Black", na.rm=TRUE),
                  pct.PoC.other = mean(value == "Immigrant"|value == "Indigenous"|value == "Complex"|value == "Person of Colour", na.rm=TRUE),
                  pct.heterosexual = mean(value == "Heterosexual", na.rm=TRUE),
                  pct.homosexual = mean(value == "Homosexual", na.rm=TRUE),
                  pct.sexuality.other = mean(value == "Bi-sexual"|value == "Other", na.rm=TRUE),
                  pct.child = mean(value == "Child", na.rm=TRUE),
                  pct.young.adult = mean(value == "Young Adult", na.rm=TRUE),
                  pct.adult = mean(value == "Adult", na.rm=TRUE),
                  pct.elderly = mean(value == "Elderly", na.rm=TRUE)
                  ) 
        
write_csv(temp, "data/Character_verbs_percentages.csv")


