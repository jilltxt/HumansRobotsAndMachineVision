# Scripts to analyse the Machine Vision dataset. 
# Written by Jill Walker Rettberg
# 
# RQ: What characterises technologies, entities and characters that are tagged 
# as classifying or being classified, or with similar actions?
# 

# Set start verb ----------------------

# THIS IS WHERE YOU NEED TO ENTER INFORMATION
# Replace the verbs in this variable with the verbs  you're interested in. 
# These are your target actions. 
#
target_action <- c("Hacking", "Hacked", "Tricking", "Tricked", "Repurposing", "Repurposed", 
                   "Subverting", "Subverted", "Obfuscating", "Obfuscated", "Pranking", "Pranked")

# Define what you want the actions to be called in the plot.

name_of_target_actions <- "Hacking-related actions"
name_of_NON_target_actions <- "Not hacking-related"

plot_title <- "Which agents are hacking or hacked in interactions with machine vision?"


# Import situations -----------------------------------------------------------

library(tidyverse)

Situations <- read_csv("data/situations.csv",
                       col_types = cols(
                               SituationID = col_integer(),
                               Situation = col_character(), 
                               Genre = col_character(),
                               Character = col_character(),
                               Entity = col_character(),
                               Technology = col_character(),
                               Verb = col_character()
                       )
)


# Find synonyms for start actions -----------------------------------------

# find synonyms for target_action using the syn library - which basically just
# produces lists of synonyms (or antonyms using the ant() function)

library(syn)

# Run this to see if any of the words in target_action have synonyms in the syn
# package. Note that the syn package is based on the Moby thesaurus which is from
# 1996 and is often weak on verbs - it has none of the "Hacking" verbs in it, for 
# instance, but many "Flawed" verbs. 
# 
syns(target_action)

# This line of code redefines target_action to include the verbs typed in manually
# above and any synonyms of those verbs that are found. 
#
target_action <- c(target_action, unlist(lapply(target_action, syn)))

# convert to title case (first letter capitalised) to match the capitalisations
# in Situations$Verb. Subset only the synonyms that end in -ed or -ing..

target_action <- str_to_title(
        target_action[str_detect(target_action, "ing")|str_detect(target_action, "ed")])

# And finally delete any verbs that aren't in the dataset, to keep it tidy.
# 
target_action <- target_action[target_action %in% Situations$Verb]

               

# Create plot showing which agents do the target actions ------------------

# Define variable

Situations %>% 
        mutate(Target_action = case_when(Verb %in% target_action ~ name_of_target_actions,
                                  TRUE ~ name_of_NON_target_actions),
               Agent = case_when(!is.na(Entity) ~ "Entity",
                                 !is.na(Technology) ~ "Technology",
                                 !is.na(Character) ~ "Character")) %>% 
        filter(Target_action == name_of_target_actions) %>% 
        select(Genre, Verb, Entity, Technology, Agent) %>% 
        pivot_longer(!Verb, values_to = "value") %>%
        drop_na() %>% 
        ggplot(aes(x=value, fill=Verb)) +
        geom_bar(aes(y = ..count..))+
        theme(axis.line = element_line(colour = "darkblue", 
                                       size = 1, 
                                       linetype = "solid")) +
        theme_minimal() +
        labs(
                title = plot_title,
                subtitle="") +
        coord_flip() +
        facet_wrap(~name, scales="free") 


# Is there a difference between which agents do the target actions --------

Situations %>% 
        mutate(Target_action = case_when(Verb %in% target_action ~ name_of_target_actions,
                                         TRUE ~ name_of_NON_target_actions),
               Agent = case_when(!is.na(Entity) ~ "Entity",
                                 !is.na(Technology) ~ "Technology",
                                 !is.na(Character) ~ "Character")) %>% 
       #filter(Verb != "Drained") %>%  # remove # to manually remove any verbs that seem wrong
        select(Target_action, Agent, Genre) %>% 
        group_by(Agent, Target_action) %>% 
        add_count(Agent, name = "AgentFreq", sort = TRUE) %>% 
        add_count(Target_action, name = "TargetFreq", sort = TRUE) %>% 
        ggplot(aes(x = Target_action, fill = Agent)) +
        geom_bar(position="fill") +
        coord_flip() +
        facet_wrap(~Genre)
