# Script for Caring AIs.
# 
# Are AIs that are not portrayed as characters more likely to be 
# flawed?
# 

library(tidyverse)

# Import situations.
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

Situations %>% 
        filter(Verb == "Flawed")

# find synonyms for flawed using the syn library - which basically just
# produces lists of synonyms (or antonyms using the ant() function)

library(syn)

# Make a vector (list of words) consisting of synonyms to verbs I know are in our
# dataset that are similar to flawed. Add the actual words from the dataset so they're
# also included.
# 
flaws <- c("Flawed", "Mistaking", "Misidentifying", "Malfunctioning", 
           "Blinded", "Glitching", "Misclassifying", "Impaired",
           syn("flawed"), syn("mistaking"), syn("misidentifying"), 
           syn("failing"), syn("malfunctioning"), syn("blinded"), 
           syn("glitching"), syn("misclassifying"), syn("distorted"), 
           syn("damaged"), syn("mutilated"), syn("biased"))

# get synonyms for all THOSE synonyms. 
# just doing syn(flaws) won't work, because you have to do syn on ecah
# individual word (each value in the vector)
# lapply lets you feed a vector to a function and have the function run on
# each value consecutively. lapply produces a list, so we need to unlist it 
# to get a vector again.

more_flaws <- unlist(lapply(flaws, syn))

# convert to title case (first letter capitalised) to match the capitalisations
# in Situations$Verb. Subset only the synonyms that end in -ed or -ing. I 
# suppose that last thing isn't really necessary, but...

more_flaws <- str_to_title(
        more_flaws[str_detect(more_flaws, "ing")|str_detect(more_flaws, "ed")])

# Let's just get the "more flawed" verbs so we can weed out the outrageous ones. 
view(Situations %>% 
        mutate(Flawed = case_when(Verb %in% more_flaws ~ "1",
                                  TRUE ~ "0"),
               Agent = case_when(!is.na(Entity) ~ "Entity",
                                 !is.na(Technology) ~ "Technology",
                                 !is.na(Character) ~ "Character")) %>% 
        filter(Flawed == 1) %>% 
        select(Verb) %>% 
        add_count(Verb) %>%
        distinct())

# This gives 127 different verbs, far too many. The list includes verbs like 
# controlling, running and following. But there are also some relevant verbs. 
# Possibles: Overwhelmed, Wounded, Confused, Destroyed, Weakened, Deceived, Incapacitated,
# Impaired, Injured, Blocked. But really, I think the list shows that the first "flaws"
# is sufficient. I added a few extra verbs to the first set though.

flaws <- str_to_title(
        flaws[str_detect(flaws, "ing")|str_detect(flaws, "ed")])

# Add a column in Situations that is 1 if the Verb is in the flaws vector. 
Situations %>% 
        mutate(Flawed = case_when(Verb %in% flaws ~ "1",
                                  TRUE ~ "0"),
               Agent = case_when(!is.na(Entity) ~ "Entity",
                                 !is.na(Technology) ~ "Technology",
                                 !is.na(Character) ~ "Character")) %>% 
        filter(Flawed == 1) %>% 
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
             title ="Which agents fail in interactions with machine vision?",
             subtitle="") +
        theme(axis.text.x = element_blank()) +
        coord_flip() +
        facet_wrap(~name, scales="free_y") 
        
# But this is absolute count and it'd be more useful to see the proportions. 

Situations %>% 
        mutate(Flawed = case_when(Verb %in% flaws ~ "Flawed",
                                  TRUE ~ "Not flawed"),
               Agent = case_when(!is.na(Entity) ~ "Entity",
                                 !is.na(Technology) ~ "Technology",
                                 !is.na(Character) ~ "Character")) %>% 
        filter(Flawed == "Flawed" & Genre == "Narrative") %>%
        filter(Verb != "Drained" 
               & Verb !="Faked"
               & Verb != "Irritated"
               & Verb != "Arrested") %>%  # manually removed cos seems wrong
        select(Verb, Entity, Technology, Agent) %>% 
        group_by(Agent, Verb) %>% 
        add_count(Verb, name = "Freq", sort = TRUE) %>% 
        add_count(Verb, name = "Freq", sort = TRUE) %>% 
        ggplot(aes(x = Verb, fill = Agent)) +
        geom_bar(position="stack") +
        labs(title ="Flawed, damaged and failed - actions shown by agent",
             subtitle ="Narratives only") +
        coord_flip() 


# Useful scrap of code:
# pop this at the end of a ggplot thing to save it -> sit_plot
# then use the code
# ggplot_build(sit_plot)
# to both plot the graph and also show the numbers ggplot was using
# 

# Show whether technologies are more likely to be "flawed" 
# than characters are - and the answer is YES. Tech more likely than 
# characters, and entities less likely.
                
Situations %>% 
        mutate(Flawed = case_when(Verb %in% flaws ~ "Flawed",
                                  TRUE ~ "Not flawed"),
               Agent = case_when(!is.na(Entity) ~ "Entity",
                                 !is.na(Technology) ~ "Technology",
                                 !is.na(Character) ~ "Character")) %>% 
        filter(Genre == "Narrative") %>%
        filter(Verb != "Drained" 
               & Verb !="Faked"
               & Verb != "Irritated"
               & Verb != "Arrested"
               ) %>%  # manually removed cos seem wrong
        select(Flawed, Agent) %>% 
        group_by(Agent, Flawed) %>% 
        add_count(Agent, name = "AgentFreq", sort = TRUE) %>% 
        add_count(Flawed, name = "FlawFreq", sort = TRUE) %>% 
        ggplot(aes(x = Flawed, fill = Agent)) +
        geom_bar(position="fill") 


# Same visualisatoin if we remove "confused". Clearly characters are
# NOT more likley to be flawed. 
# --> 10.04.2022 - what was I thinking here? the two graphs seem
# pretty similar? characters DO seem more likely to be flawed? 
# 

Situations %>% 
        mutate(Flawed = case_when(Verb %in% flaws ~ "Flawed",
                                  TRUE ~ "Not flawed"),
               Agent = case_when(!is.na(Entity) ~ "Entity",
                                 !is.na(Technology) ~ "Technology",
                                 !is.na(Character) ~ "Character")) %>% 
        filter(Genre == "Narrative") %>%
        filter(Verb != "Drained" 
               & Verb !="Faked"
               & Verb != "Irritated"
               & Verb != "Arrested"
               & Verb != "Confused"
        ) %>%  # manually removed cos seem wrong
        select(Flawed, Agent) %>% 
        group_by(Agent, Flawed) %>% 
        add_count(Agent, name = "AgentFreq", sort = TRUE) %>% 
        add_count(Flawed, name = "FlawFreq", sort = TRUE) %>% 
        ggplot(aes(x = Flawed, fill = Agent)) +
        geom_bar(position="fill") 


# Messy testing -----------------------------------------------------------

# only 7 verbs in set - check whether flaws is working

test <- Situations %>% 
        mutate(Flawed = case_when(Verb %in% flaws ~ "1",
                                  TRUE ~ "0"),
               Agent = case_when(!is.na(Entity) ~ "Entity",
                                 !is.na(Technology) ~ "Technology",
                                 !is.na(Character) ~ "Character")) %>% 
        select(Verb, Flawed, Entity, Technology, Agent)

test <- Situations %>% 
        mutate(Flawed = case_when(Verb %in% flaws ~ "Flawed",
                                  TRUE ~ "Not flawed"),
               Agent = case_when(!is.na(Entity) ~ "Entity",
                                 !is.na(Technology) ~ "Technology",
                                 !is.na(Character) ~ "Character")) %>% 
        filter(Genre == "Narrative")

