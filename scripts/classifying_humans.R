# Scripts to analyse the Machine Vision dataset. 
# Written by Jill Walker Rettberg
# 
# RQ: What characterises technologies, entities and characters that are tagged 
# as classifying or being classified, or with similar actions?
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
        filter(Verb == "Classified")


# First go at generating list of synonyms for flawed ----------------------

# find synonyms for flawed using the syn library - which basically just
# produces lists of synonyms (or antonyms using the ant() function)

library(syn)

# Make a vector (list of words) consisting of synonyms to verbs I know are in our
# dataset that are similar to flawed. Add the actual words from the dataset so they're
# also included. syn("classified") fetches synonyms to the word so we can expand the list.
# 
classify <- c("Classified", "Classifying", "Identified", "Identifying", "Misidentified",  
           "Misclassifying", "Misgendering", "Misgendered",
           syn("classified"), syn("classifying"))



# convert to title case (first letter capitalised) to match the capitalisations
# in Situations$Verb. Subset only the synonyms that end in -ed or -ing. I 
# suppose that last thing isn't really necessary, but...

classify <- str_to_title(
        classify[str_detect(classify, "ing")|str_detect(classify, "ed")])

# Let's just get the "more flawed" verbs so we can weed out the outrageous ones. 
view(Situations %>% 
        mutate(Classify = case_when(Verb %in% classify ~ "1",
                                  TRUE ~ "0"),
               Agent = case_when(!is.na(Entity) ~ "Entity",
                                 !is.na(Technology) ~ "Technology",
                                 !is.na(Character) ~ "Character")) %>% 
        filter(Classify == 1) %>% 
        select(Verb) %>% 
        add_count(Verb) %>%
        distinct())

# This gives 12 different verbs. 

flaws <- str_to_title(
        flaws[str_detect(flaws, "ing")|str_detect(flaws, "ed")])

flawed_actions <- flaws[flaws %in% Situations$Verb]

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
        coord_flip() +
        facet_wrap(~name, scales="free") 
        
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


# Again but sorted by frequency -------------------------------------------

Flawed_situations <- Situations %>% 
        mutate(Flawed = case_when(Verb %in% flaws ~ "Flawed",
                                  TRUE ~ "Not flawed"),
               Agent = case_when(!is.na(Entity) ~ "Entity",
                                 !is.na(Technology) ~ "Technology",
                                 !is.na(Character) ~ "Character")) %>% 
        filter(Flawed == "Flawed") %>%
        filter(Verb != "Drained" 
               & Verb !="Faked"
               & Verb != "Irritated"
               & Verb != "Arrested") %>%  # manually removed cos seems wrong
        select(Verb, Entity, Technology, Agent, Genre)

# Set levels for Technology so we can sort them by frequency
# using fct_infreq() in ggplot().
levels(Flawed_situations$Technology) = factor(unique(Flawed_situations$Technology))

# Same for Verbs.
levels(Flawed_situations$Verb) = factor(unique(Flawed_situations$Verb))


# Plot flawed actions -----------------------------------------------------

# Now plot. 
# Use fct_rev() to reverse the order of fct_infreq so highest frequency verb
# is at the top if doing coord_flip(). 
# ggplot(aes(x = fct_rev(fct_infreq(Verb)), fill = Agent)) +
# 
# Narratives
flawed_narratives <- Flawed_situations %>%
        filter(Genre == "Narrative") %>% 
        group_by(Agent, Verb) %>% 
        ggplot(aes(x = fct_rev(fct_infreq(Verb)), fill = Agent)) +
        geom_bar(position="stack") +
        labs(title ="Flawed, damaged and failed - actions shown by agent",
             subtitle ="Narratives only",
             x = "Actions",
             y = "Number of occurrences") +
        theme(axis.text.x = element_text(angle=45, vjust=1, hjust = 1)) +
        coord_flip() 

#Games
flawed_games <- Flawed_situations %>%
        filter(Genre == "Game") %>% 
        group_by(Agent, Verb) %>% 
        ggplot(aes(x = fct_rev(fct_infreq(Verb)), fill = Agent)) +
        geom_bar(position="stack") +
        labs(title ="Flawed, damaged and failed - actions shown by agent",
             subtitle ="Games only",
             x = "Actions",
             y = "Number of occurrences") +
        theme(axis.text.x = element_text(angle=45, vjust=1, hjust = 1)) +
        coord_flip() 

# Art
flawed_art <- Flawed_situations %>%
        filter(Genre == "Art") %>% 
        group_by(Agent, Verb) %>% 
        add_count(Verb, name = "Freq", sort = TRUE) %>% 
        add_count(Verb, name = "Freq", sort = TRUE) %>% 
        ggplot(aes(x = fct_rev(fct_infreq(Verb)), fill = Agent)) +
        geom_bar(position="stack") +
        labs(title ="Flawed, damaged and failed - actions shown by agent",
             subtitle ="Art only",
             x = "Actions",
             y = "Number of occurrences") +
        theme(axis.text.x = element_text(angle=45, vjust=1, hjust = 1)) +
        coord_flip() 



# Scraps of code that could be useful -------------------------------------

# Useful scrap of code:
# pop this at the end of a ggplot thing to save it -> sit_plot
# then use the code
# ggplot_build(sit_plot)
# to both plot the graph and also show the numbers ggplot was using
# 


# Are characters, entities or technologies more likely to be flawe --------

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



#  Final set of verbs that are flawed --------------------------------------

# Start with a list of words I know are in the dataset that describe flaws.

library(syn)

flaws <- c("Flawed", "Mistaking", "Misidentifying", "Malfunctioning", 
           "Blinded", "Glitching", "Misclassifying", "Impaired", 
           "Mistaking", "Misidentifying", "Failing", 
           "Distorted", "Distorting")

# get synonyms for all THOSE synonyms. 
# just doing syn(flaws) won't work, because you have to do syn on ecah
# individual word (each value in the vector)
# lapply lets you feed a vector to a function and have the function run on
# each value consecutively. lapply produces a list, so we need to unlist it 
# to get a vector again.

flaws <- unlist(lapply(flaws, syn))
#This generates a list of 613 words. Not all are verbs. 
#
#Change all to title case, then remove those that don't end in -ed or -ing.
flaws <- str_to_title(
        flaws[str_detect(flaws, "ing")|str_detect(flaws, "ed")])

flaws <- flaws[! flaws %in% c("Drained", "Faked", "Irritated", "Arrested", "Confused")]

# fails list - this one is bigger, including bias -------------------------

fails <- c("Flawed", "Mistaking", "Misidentifying", "Malfunctioning", 
                   "Blinded", "Glitching", "Misclassifying", "Impaired", 
                   "Mistaking", "Misidentifying", "Failing", 
                   "Distorted", "Distorting", "Unsettled", "Tricked", "Hacked", 
           "Pranked", "Deceived", "Disoriented", "Disabled", "Mutilated", "Failed", 
           "Broken", "Exploited")

fails <- unlist(lapply(fails, syn))

fails <- str_to_title(
        fails[str_detect(fails, "ing")|str_detect(fails, "ed")])

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

# Sorting out the situations to only include narratives -------------------

# Add WorkID to Situations
# Import creativeworks.csv using machinevisionscripts.R

works <- CreativeWorks %>% 
        select(WorkID, Country, Sentiment, SituationID) %>% 
        distinct()

SitWorks <- merge(works, Situations)

# Filter: only keep narratives
Narr_SitWorks <- SitWorks %>% 
        filter(Genre=="Narrative")

# Import worksinfo.csv and convert to wide format using machinevisionscripts.R
# Select relevant columns

worksinfo <- worksinfo %>% 
        select(WorkID, Year, Country1, IsSciFi)

Narr_SitWorks <- merge(Narr_SitWorks, worksinfo)

# Import narrativegenres.csv to get subgenres.

narrativegenres <- read_csv("data/narrativegenres.csv") %>% 
        select(NarrativeGenre, WorkID)

Narr_SitWorks <- merge(Narr_SitWorks, narrativegenres)

# Now check how many are scifi.

Narr_SitWorks %>% 
        filter(IsSciFi == "Yes") %>% 
        select(WorkID, NarrativeGenre) %>% 
        distinct() %>% 
        group_by(fct_infreq(NarrativeGenre)) %>% 
        summarise(count = n())


# Visualising flaws with all the extra works info -------------------------

Flawed_situations <- Narr_SitWorks %>% 
        mutate(Flawed = case_when(Verb %in% flaws ~ "Flawed",
                                  TRUE ~ "Not flawed"),
               Agent = case_when(!is.na(Entity) ~ "Entity",
                                 !is.na(Technology) ~ "Technology",
                                 !is.na(Character) ~ "Character")) %>% 
        select(SituationID, Verb, Flawed, Entity, Technology, Agent, NarrativeGenre, Country, 
               Sentiment, IsSciFi)

# Get an overview over the distribution.

Flawed_situations %>% 
        select(SituationID, Flawed, Entity, Technology, Agent, NarrativeGenre, Country, 
               Sentiment, IsSciFi) %>% 
        pivot_longer(-c(SituationID, Flawed)) %>%
        drop_na() %>% 
        ggplot(aes(x=value, fill = Flawed)) + 
        geom_bar(alpha=.7) +
        theme_minimal() +
        coord_flip() +
        facet_wrap(~name, scales="free")

# Shows that the proportion of flawed actions is really low. 


# Proportion of flawed actions. -------------------------------------------

Flawed_situations %>%
        select(SituationID, Verb, Flawed) %>% 
        distinct() %>% 
        group_by(Flawed) %>% 
        summarise(count=n())

# Only 2% of verbs are "flawed"?
# 
#Generate list of flawed verbs and use count:
Flawed_situations %>% 
        select(SituationID, Verb, Flawed) %>% 
        distinct() %>% 
        filter(Flawed == "Flawed") %>% 
        group_by(fct_infreq(Verb)) %>%
        summarise(count=n())

Flawed_situations %>% 
        select(SituationID, Verb, Flawed) %>% 
        distinct() %>% 
        filter(Flawed == "Not flawed") %>% 
        group_by(fct_infreq(Verb)) %>%
        summarise(count=n())


# Which agents fail - narratives only and more work info ------------------

#Make sure to keep SituationID before distinct() or lose multiples of verbs.

Flawed_situations %>% 
        filter(Flawed == "Flawed") %>% 
        select(SituationID, NarrativeGenre, Verb, Entity, Technology, Agent) %>% 
        distinct() %>% 
        select(-SituationID) %>% 
        pivot_longer(!Verb, values_to = "value") %>%
        drop_na() %>% 
        ggplot(aes(x=fct_rev(fct_infreq(value)), fill=Verb)) +
        geom_bar(aes(y = ..count..))+
        theme(axis.line = element_line(colour = "darkblue", 
                                       size = 1, 
                                       linetype = "solid")) +
        theme_minimal() +
        labs( title ="Which agents fail in interactions with machine vision?",
              subtitle="Narratives only") +
        coord_flip() +
        facet_wrap(~name, scales="free") 

#Try again but with the technologies controlling? Does that work?
Flawed_situations %>% 
        filter(Flawed == "Flawed") %>% 
        select(SituationID, NarrativeGenre, Verb, Entity, Technology, Agent) %>% 
        distinct() %>% 
        select(-SituationID) %>% 
        pivot_longer(!Technology, values_to = "value") %>%
        drop_na() %>% 
        ggplot(aes(x=fct_rev(fct_infreq(value)), fill=Technology)) +
        geom_bar(aes(y = ..count..))+
        theme(axis.line = element_line(colour = "darkblue", 
                                       size = 1, 
                                       linetype = "solid")) +
        theme_minimal() +
        labs(
                title ="Which agents fail in interactions with machine vision?",
                subtitle="Narratives only") +
        coord_flip() +
        facet_wrap(~name, scales="free") 

# OK, the technologies are pretty evenly distributed in genres. Let's focus
# on just the verbs.
# 

Flawed_situations %>% 
        filter(Flawed == "Flawed") %>% 
        select(SituationID, Verb, Technology) %>% 
        distinct() %>% 
        pivot_longer(-c(Technology, SituationID), values_to = "value") %>%
        drop_na() %>% 
        ggplot(aes(x=fct_rev(fct_infreq(value)), fill=Technology)) +
        geom_bar(aes(y = ..count..))+
        theme(axis.line = element_line(colour = "darkblue", 
                                       size = 1, 
                                       linetype = "solid")) +
        theme_minimal() +
        labs(
                title ="Which agents fail in interactions with machine vision?",
                subtitle="Narratives only") +
        coord_flip() 

# No verb is used more than once by one technology... Nothing interesting to see here...

Flawed_situations %>% 
        select(Flawed, Technology, Agent) %>% 
        distinct() %>% 
        pivot_longer(!Flawed, values_to = "value") %>% 
        drop_na %>%
        ggplot(aes(x=fct_rev(fct_infreq(value)), fill=Flawed)) +
        geom_bar(aes(y = ..count..))+
        theme(axis.line = element_line(colour = "darkblue", 
                                       size = 1, 
                                       linetype = "solid")) +
        theme_minimal() +
        labs(
                title ="Which agents fail in interactions with machine vision?",
                subtitle="Narratives only") +
        coord_flip() 
        