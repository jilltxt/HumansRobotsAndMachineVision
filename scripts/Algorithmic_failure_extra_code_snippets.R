# Extra bits of code for the Algorithmic Failure paper.
# 
# These code snippets all assume you have already run the setup code in the 
# main script, and have loaded the tidyverse package.
# 
# library(tidyverse)
#
# This code filters out specific kinds of prediction and specific character traits.
# It is set for the Prediction_type to NOT be equal to Accurate, and 
# Age to be Child, which only returns one result from the test dataset.
# To find all accurate predictions, change the != (not equal to) to == (is equal
# to), or you can for instance change to (Prediction_type == "False passive") 
# to show all the false passive predictions. Similarly change the (Age == "Child")
# to change the traits you want to see.

Character_verb_predictions %>% 
        filter(Prediction_type != "Accurate") %>% 
        select(Verb, Species, Gender, 
               RaceOrEthnicity, Age, Sexuality, Prediction_type) %>% 
        filter(Age == "Child")

# This generates a list of verbs arranged by how often they are used by characters.
# Character_verbs is the overall dataset, not the test dataset. 

Verb_freq <- Character_verbs %>% 
        select(Verb) %>% 
        count(Verb) %>% 
        arrange(desc(n))

Character_verbs %>% 
        filter(Verb == "Hunting" | Verb == "Hunted") %>% 
        select(-c(target,SituationID, Character)) %>% 
        pivot_longer(!Verb, values_to = "value") %>%

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



# Add verb_freq¨ ----------------------------------------------------------

# Are accurate predictions  simply the most frequent verbs in the test dataset?  

# 

Character_verb_predictions %>%
        add_count(Verb) %>% 
        arrange(desc(n)) %>% 
        select(Verb, Prediction_type, n) %>% 
        distinct() 

# 9 most frequent verbs in test dataset are all accurate, but after that it gets 
# messier. Verb no 9 is used 26 times. 

# Remove the top 9 verbs by filtering to only keep n<26. 

Verb_pred_freq <- Character_verb_predictions %>%
        add_count(Verb) %>% 
        arrange(desc(n)) %>% 
        select(Verb, Prediction_type, n) %>% 
        distinct() %>%
        filter(n<26) 

ggpie(Verb_pred_freq, Prediction_type)



# Verb predictions by gender ----------------------------------------------

library(ggpie)

ggpie(Character_verb_predictions, Gender, Prediction_type, 
      nrow=2, border.color="white", border.width=1.5,
      label.color="black", label.size=3, offset=.7,
      title="Verb predictions by gender\n") +
        theme(legend.position=c(.8, .2))
